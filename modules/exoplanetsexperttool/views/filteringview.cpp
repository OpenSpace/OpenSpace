/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <modules/exoplanetsexperttool/views/filteringview.h>

#include <modules/exoplanetsexperttool/datahelper.h>
#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <implot.h>

namespace openspace::exoplanets {

FilteringView::FilteringView(const DataViewer& dataViewer,
    const DataSettings& dataSettings)
    : _dataViewer(dataViewer), _quickFilterGroups(dataSettings.quickFilterGroups)
{
    // Set all quick filters to false
    const size_t nQuickFilterGroups = _quickFilterGroups.size();
    _quickFilterFlags.reserve(nQuickFilterGroups);
    for (size_t groupId = 0; groupId < nQuickFilterGroups; ++groupId) {
        const DataSettings::QuickFilterGroup& group =
            _quickFilterGroups[groupId];

        std::vector<bool> flags;
        flags.assign(group.quickFilters.size(), false);
        _quickFilterFlags.push_back(flags);
    }

    // Find default column - first numeric.
    for (size_t i = 0; i < _dataViewer.columns().size(); ++i) {
        if (_dataViewer.isNumericColumn(i)) {
            _rowLimitColumnIndex = i; // TODO: Should be the row limit column
            break;
        }
    }
}

bool FilteringView::renderFilterSettings() {
    bool filterWasChanged = false;

    if (ImGui::Button("Reset internal")) {
        _columnFilters.clear();

        _quickFilterFlags.reserve(_quickFilterGroups.size());
        for (size_t i = 0; i < _quickFilterGroups.size(); ++i) {
            _quickFilterFlags[i].assign(_quickFilterFlags[i].size(), false);
        }

        filterWasChanged = true;
    }

    ImGui::SameLine();
    if (ImGui::Button("Reset row limit")) {
        _limitNumberOfRows = false;
        filterWasChanged = true;
    }

    ImGui::SameLine();
    if (ImGui::Button("Reset external")) {
        _useExternalSelection = false;
        _overrideInternalSelection = false;
        filterWasChanged = true;
    }

    filterWasChanged |= renderColumnFilterSettings();
    filterWasChanged |= renderRowLimitFilterSettings();
    filterWasChanged |= renderExternalFilterSettings();

    return filterWasChanged;
}

std::vector<size_t> FilteringView::applyFiltering(const std::vector<ExoplanetItem>& data) {
    std::vector<size_t> filteredData;
    filteredData.reserve(data.size());

    for (int i = 0; i < data.size(); i++) {
        const ExoplanetItem& item = data[i];

        bool filteredOut = false;

        // Pre-defined filters
        for (size_t groupId = 0; groupId < _quickFilterFlags.size(); ++groupId) {
            const DataSettings::QuickFilterGroup& group =
                _quickFilterGroups[groupId];

            bool hasAnyEnabled = false;
            for (bool isChecked : _quickFilterFlags[groupId]) {
                if (isChecked) {
                    hasAnyEnabled = true;
                    break;
                }
            }

            if (!hasAnyEnabled) {
                continue;
            }

            auto passesQuickFilter = [this](const DataSettings::QuickFilter& q,
                const ExoplanetItem& item)
                {
                    bool passedFilter = true;
                    // Sub-filters in a quick filter are applied with AND, always
                    for (const DataSettings::QuickFilter::Filter& filter : q.filters) {
                        bool isNumeric = _dataViewer.isNumericColumn(
                            _dataViewer.columnIndex(filter.column)
                        );
                        ColumnFilter cf = ColumnFilter(
                            filter.query,
                            isNumeric ? ColumnFilter::Type::Numeric : ColumnFilter::Type::Text
                        );
                        passedFilter &= cf.passFilter(
                            _dataViewer.columnValue(filter.column, item)
                        );
                    }
                    return passedFilter;
                };

            bool passedAnyGroupFilter = false; // only used for OR

            for (size_t qIndex = 0; qIndex < _quickFilterFlags[groupId].size(); ++qIndex) {
                const DataSettings::QuickFilter& q = group.quickFilters[qIndex];
                bool shouldMatchFilter = _quickFilterFlags[groupId][qIndex];

                switch (group.type) {
                case DataSettings::QuickFilterGroup::Type::And:
                    if (shouldMatchFilter) {
                        filteredOut |= !passesQuickFilter(q, item);
                    }
                    break;
                case DataSettings::QuickFilterGroup::Type::Or:
                    passedAnyGroupFilter |= shouldMatchFilter && passesQuickFilter(q, item);
                    break;
                default: break;
                }
            }

            if (group.type == DataSettings::QuickFilterGroup::Type::Or) {
                filteredOut |= !passedAnyGroupFilter;
            }
        }

        // Other filters
        for (const ColumnFilterEntry& f : _columnFilters) {
            if (!f.enabled) {
                continue;
            }

            filteredOut |= !f.filter.passFilter(
                _dataViewer.columnValue(_dataViewer.columns()[f.columnIndex], item)
            );
        }

        if (!filteredOut) {
            filteredData.push_back(i);
        }
    }
    filteredData.shrink_to_fit();

    static int nRowsAfterLimit = 0;
    ColumnKey rowLimitCol = _dataViewer.columns()[_rowLimitColumnIndex];

    if (_limitNumberOfRows && filteredData.size() > _nRows) {
        auto compare = [&rowLimitCol, &data, this](const size_t& lhs, const size_t& rhs) {
            // We are interested in the largest, so flip the order
            const ExoplanetItem& l = data[_useHighestValue ? rhs : lhs];
            const ExoplanetItem& r = data[_useHighestValue ? lhs : rhs];
            return _dataViewer.compareColumnValues(rowLimitCol, l, r);
        };

        std::sort(filteredData.begin(), filteredData.end(), compare);
        filteredData.erase(filteredData.begin() + _nRows, filteredData.end());
        nRowsAfterLimit = static_cast<int>(filteredData.size());
    }

    if (_limitNumberOfRows) {
        ImGui::TextColored(
            view::helper::toImVec4(view::colors::DescriptiveText),
            std::format("After row limit : {}", nRowsAfterLimit).c_str()
        );
    }

    return filteredData;
}

std::vector<size_t> FilteringView::applyExternalSelection(
                                                   const std::vector<int>& externalSelection,
                                                   const std::vector<size_t>& prefilteredData)
{
    if (!_useExternalSelection) {
        return prefilteredData;
    }

    std::vector<size_t> filteredData = prefilteredData;

    if (_overrideInternalSelection) {
        // Just use the external seleciton, out of the box
        filteredData.clear();
        filteredData.reserve(externalSelection.size());
        for (int i : externalSelection) {
            filteredData.push_back(static_cast<size_t>(i));
        }
    }
    else {
        // Do an intersection, i.e. check if the filtered out items are in the selection
        std::vector<size_t> newFilteredData;
        newFilteredData.reserve(filteredData.size());

        for (size_t index : filteredData) {
            bool isFound = std::find(
                externalSelection.begin(),
                externalSelection.end(),
                static_cast<int>(index)
            ) != externalSelection.end();
            if (isFound) {
                newFilteredData.push_back(index);
            }
        }
        newFilteredData.shrink_to_fit();
        filteredData = std::move(newFilteredData);
    }

    return filteredData;
}

bool FilteringView::renderColumnFilterSettings() {
    bool filterWasChanged = false;

    bool headerIsOpen = ImGui::CollapsingHeader(
        "Internal filters",
        ImGuiTreeNodeFlags_DefaultOpen
    );

    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Filter the data internally, within the OpenSpace application"
    );

    if (!headerIsOpen) {
        return filterWasChanged;
    }

    // Per-column filtering
    {
        static size_t filterColIndex = 0;

        ImGui::Separator();
        ImGui::Text("Filter on column");
        ImGui::SetNextItemWidth(120);
        if (ImGui::BeginCombo("##Column", _dataViewer.columnName(filterColIndex))) {
            for (size_t i = 0; i < _dataViewer.columns().size(); ++i) {
                if (ImGui::Selectable(_dataViewer.columnName(i), filterColIndex == i)) {
                    filterColIndex = i;
                }
            }
            ImGui::EndCombo();
        }

        ImGui::SameLine();

        static char queryString[128] = "";

        bool numeric = _dataViewer.isNumericColumn(filterColIndex);

        ImGui::SetNextItemWidth(numeric ? ImGui::GetContentRegionAvail().x * 0.3f : -150);
        bool inputEntered = ImGui::InputTextWithHint(
            "##Query",
            "has value",
            queryString,
            IM_ARRAYSIZE(queryString),
            ImGuiInputTextFlags_EnterReturnsTrue
        );

        // Short description
        ImGui::SameLine();
        ImGui::TextUnformatted(numeric ?
            ColumnFilter::NumericFilterDescriptionShort :
            ColumnFilter::TextFilterDescriptionShort
        );

        // Help marker
        ImGui::SameLine();
        view::helper::renderHelpMarker(numeric ?
            ColumnFilter::NumericFilterDescription :
            ColumnFilter::TextFilterDescription
        );

        if (ImGui::Button("Add filter") || inputEntered) {
            ColumnFilter filter = numeric ?
                ColumnFilter(queryString, ColumnFilter::Type::Numeric) :
                ColumnFilter(queryString, ColumnFilter::Type::Text);

            if (filter.isValid()) {
                _columnFilters.push_back({ filterColIndex , filter });
                strcpy(queryString, "");
                filterWasChanged = true;
            }
        }

        // Clear the text field
        ImGui::SameLine();
        if (ImGui::Button("Clear text field")) {
            strcpy(queryString, "");
        }
    }

    ImGui::Spacing();

    // List of column filters
    {
        const std::string filtersHeader = _columnFilters.empty() ?
            "Added filters" :
            std::format("Added filters ({})", _columnFilters.size());

        // The ### operator overrides the ID, ignoring the preceding label
        // => Won't rerender when label changes
        const std::string headerWithId = std::format("{}###FiltersHeader", filtersHeader);

        if (ImGui::CollapsingHeader(headerWithId.c_str(), ImGuiTreeNodeFlags_DefaultOpen)) {
            ImGui::Indent();

            if (_columnFilters.empty()) {
                ImGui::Text("No active filters");
            }

            int indexToErase = -1;
            constexpr const int nColumns = 5;

            const ImGuiTableFlags flags = ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_RowBg;

            if (ImGui::BeginTable("filtersTable", nColumns, flags)) {
                for (int i = 0; i < _columnFilters.size(); ++i) {
                    ColumnFilterEntry& f = _columnFilters[i];
                    const std::string queryString = f.filter.query();
                    ImGui::TableNextRow();

                    ImGui::PushID(std::format("FilterColEnabled-{}", i).c_str());
                    ImGui::TableNextColumn();
                    if (ImGui::Checkbox("##Enabled", &f.enabled)) {
                        filterWasChanged = true;
                    }
                    ImGui::PopID();

                    ImGui::TableNextColumn();
                    ImGui::Text(_dataViewer.columnName(f.columnIndex));

                    ImGui::TableNextColumn();
                    ImGui::Text("    ");

                    ImGui::TableNextColumn();
                    ImGui::Text(queryString.empty() ? "has value" : queryString.c_str());

                    ImGui::TableNextColumn();
                    ImGui::PushID(i);
                    if (ImGui::SmallButton("Delete")) {
                        indexToErase = i;
                    }
                    ImGui::PopID();
                }

                if (indexToErase != -1) {
                    _columnFilters.erase(_columnFilters.begin() + indexToErase);
                    filterWasChanged = true;
                }

                ImGui::EndTable();
            }
            ImGui::Unindent();
        }
    }

    ImGui::Separator();
    ImGui::Spacing();

    // Pre-defined quick filters
    {
        const size_t nGroups = _quickFilterGroups.size();
        for (size_t groupId = 0; groupId < nGroups; ++groupId) {
            const DataSettings::QuickFilterGroup& group = _quickFilterGroups[groupId];

            if (!group.title.empty()) {
                ImGui::Text(group.title.c_str());
            }

            for (size_t i = 0; i < group.quickFilters.size(); ++i) {
                const DataSettings::QuickFilter& filter = group.quickFilters[i];

                bool isSelected = _quickFilterFlags[groupId][i];

                ImGui::PushID(std::format("quickFilter_{}", i).c_str());
                filterWasChanged |= ImGui::Checkbox(filter.name.c_str(), &isSelected);
                ImGui::PopID();

                _quickFilterFlags[groupId][i] = isSelected;

                if (!filter.description.empty()) {
                    ImGui::SameLine();
                    view::helper::renderHelpMarker(filter.description.c_str());
                }

                if (group.showOnSameLine && (i != group.quickFilters.size() - 1)) {
                    ImGui::SameLine();
                }
            }
        }
    }

    ImGui::Spacing();

    return filterWasChanged;
}

bool FilteringView::renderRowLimitFilterSettings() {
    bool filterWasChanged = false;

    bool headerIsOpen = ImGui::CollapsingHeader("Row limit");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Limit the number of filtered rows (internal) based on which "
        "have the highest TSM/ESM value"
    );

    if (!headerIsOpen) {
        return filterWasChanged;
    }

    filterWasChanged |= ImGui::Checkbox("##RowLimit", &_limitNumberOfRows);
    ImGui::SameLine();
    ImGui::Text("Limit number of rows");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Enable to only show the top X resulting rows with highest or lowest value "
        "for the given column"
    );

    ImGui::Text("Show");
    ImGui::SameLine();
    ImGui::SetNextItemWidth(85);
    filterWasChanged |= ImGui::InputInt("##nRows", &_nRows);
    ImGui::SameLine();
    ImGui::Text("rows with");
    ImGui::SameLine();

    const char* highOrLowChoices[] = { "highest", "lowest" };
    static int highOrLowIndex = 0; // highest

    ImGui::SetNextItemWidth(80);
    bool highLowChanged = ImGui::Combo(
        "##HighOrLowCombo",
        &highOrLowIndex,
        highOrLowChoices,
        IM_ARRAYSIZE(highOrLowChoices)
    );
    if (highLowChanged) {
        filterWasChanged = true;
    };

    _useHighestValue = std::string(highOrLowChoices[highOrLowIndex]) == "highest";

    ImGui::SameLine();

    const char* columnName = _dataViewer.columnName(_rowLimitColumnIndex);
    ImGui::SetNextItemWidth(100);
    if (ImGui::BeginCombo("##RowLimitColumn", columnName)) {
        for (size_t i = 0; i < _dataViewer.columns().size(); ++i) {
            // Ignore non-numeric columns
            if (!_dataViewer.isNumericColumn(i)) {
                continue;
            }

            const char* name = _dataViewer.columnName(i);
            if (ImGui::Selectable(name, _rowLimitColumnIndex == i)) {
                _rowLimitColumnIndex = i;
                filterWasChanged = true;
            }
        }
        ImGui::EndCombo();
    }

    ImGui::Spacing();

    return filterWasChanged;
}

bool FilteringView::renderExternalFilterSettings() {
    bool filterWasChanged = false;

    bool headerIsOpen = ImGui::CollapsingHeader("External filters");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Control filtering/selection coming from the external webpage. \n \n"
        "Note that it is ignored by default. Set the 'Use selection from webpage' "
        "to true to apply the selection. "
    );

    if (!headerIsOpen) {
        return filterWasChanged;
    }

    // Filter from webpage
    if (ImGui::Checkbox("Use selection from website", &_useExternalSelection)) {
        filterWasChanged = true;
    }

    if (ImGui::Checkbox("Override internal selection", &_overrideInternalSelection)) {
        filterWasChanged = true;
    }
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "If set to true, only the selection from the webpage will be shown. "
        "Meaning that the above internal filtering will be ignored."
    );

    ImGui::Spacing();

    return filterWasChanged;
}

} // namespace openspace::exoplanets
