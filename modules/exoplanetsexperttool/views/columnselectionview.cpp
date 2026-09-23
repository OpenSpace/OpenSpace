/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/exoplanetsexperttool/views/columnselectionview.h>

#include <modules/exoplanetsexperttool/datahelper.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>

namespace openspace::exoplanets {

std::vector<ColumnKey> ColumnSelectionView::initializeColumnsFromData(
                                                  const std::vector<ExoplanetItem>& data,
                                                  const DataSettings& dataSettings)
{
    std::vector<ColumnKey> allColumnsOrdered;

    if (data.empty()) {
        return allColumnsOrdered;
    }

    const auto& allDataColumns = data.front().dataColumns;
    const auto& columnsWithSettings = dataSettings.columnInfo;

    allColumnsOrdered.reserve(allDataColumns.size());

    // The name column is required and should be handled separately, to always be the
    // first column
    bool hasNameColumn = !dataSettings.nameColumn().empty();
    _nameColumn = hasNameColumn ? dataSettings.nameColumn() : "name";

    allColumnsOrdered.push_back(_nameColumn);

    // The default column are the which info has been provided for, if they exist in
    // the dataset
    _namedColumns.clear();
    _namedColumns.reserve(columnsWithSettings.size());
    for (auto const& [key, _] : columnsWithSettings) {
        if (allDataColumns.contains(key) && key != _nameColumn) {
            _namedColumns.push_back(key);
        }
    }
    _namedColumns.shrink_to_fit();

    // Sort the defauls columns based on provided name instead of key
    std::sort(
        _namedColumns.begin(),
        _namedColumns.end(),
        [&dataSettings](const ColumnKey& lhs, const ColumnKey& rhs) {
            return data::caseInsensitiveLessThan(
                dataSettings.columnName(lhs),
                dataSettings.columnName(rhs)
            );
        }
    );

    allColumnsOrdered.insert(allColumnsOrdered.end(), _namedColumns.begin(), _namedColumns.end());
    _selectedNamedColumns.assign(allColumnsOrdered.size(), true);

    // Add other columns, if there are any. Assume all items have the same columns
    _otherColumns.clear();
    _otherColumns.reserve(allDataColumns.size());
    for (auto const& [key, _] : allDataColumns) {
        if (!columnsWithSettings.contains(key) && key != _nameColumn) {
            _otherColumns.push_back(key);
        }
    }
    _otherColumns.shrink_to_fit();
    allColumnsOrdered.insert(allColumnsOrdered.end(), _otherColumns.begin(), _otherColumns.end());
    _selectedOtherColumns.assign(_otherColumns.size(), false);
    _computedColumns.clear();
    _selectedComputedColumns.clear();

    allColumnsOrdered.shrink_to_fit();

    return allColumnsOrdered;
}

bool ColumnSelectionView::renderColumnSettingsView(const DataSettings& dataSettings) {
    int nSelected = 0;
    bool selectionChanged = false;

    ImGui::TextWrapped("Select which columns to show in the tables in the interface.");

    ImGui::Separator();

    // Required columns
    ImGui::BeginGroup();
    {
        ImGui::Text("Object name (required): ");
        ImGui::SameLine();

        view::helper::renderDescriptiveText(
            dataSettings.columnName(_nameColumn)
        );

        if (dataSettings.columnInfo.contains(_nameColumn)) {
            ImGui::SameLine();
            view::helper::renderDescriptiveText(
                std::format("({})", _nameColumn).c_str()
            );

            if (dataSettings.hasDescription(_nameColumn)) {
                ImGui::SameLine();
                view::helper::renderHelpMarker(dataSettings.description(_nameColumn).c_str());
            }
        }

        ImGui::EndGroup();
    }

    ImGui::Separator();
    ImGui::Spacing();

    const float groupsSpacing = 10.f;
    const float availableGroupsWidth = ImGui::GetContentRegionAvail().x - 2.f * groupsSpacing;
    const float group1Width = availableGroupsWidth * 0.35f;
    const float group2Width = availableGroupsWidth * 0.45f;
    const float group3Width = availableGroupsWidth * 0.20f;
    const float groupsHeight = ImGui::GetContentRegionAvail().y * 0.8f;

    // Named columns
    ImGui::BeginChild("NamedColumnsGroup", ImVec2(group1Width, groupsHeight), false);
    {
        ImGui::Text("Named columns:");

        ImGui::SameLine();
        view::helper::renderHelpMarker(
            "The columns specified with column information and a given name in "
            "the .json file with data settings."
        );

        ImGui::SameLine(0, 10);

        ImGui::PushID("clear_default");
        if (ImGui::Button("Clear")) {
            _selectedNamedColumns.assign(_namedColumns.size(), false);
        }
        ImGui::PopID();
        ImGui::SameLine();

        ImGui::PushID("select_all_default");
        if (ImGui::Button("Select all")) {
            _selectedNamedColumns.assign(_namedColumns.size(), true);
        }
        ImGui::PopID();

        ImGui::BeginChild(
            "NamedColumns",
            ImVec2(0.f, ImGui::GetContentRegionAvail().y),
            false
        );

        for (int i = 0; i < _namedColumns.size(); i++) {
            const ColumnKey& c = _namedColumns[i];

            bool isSelected = _selectedNamedColumns[i];
            if (ImGui::Checkbox(dataSettings.columnName(c), &isSelected)) {
                selectionChanged = true;
            }
            _selectedNamedColumns[i] = isSelected;

            nSelected += _selectedNamedColumns[i] ? 1 : 0;

            ImGui::SameLine();
            view::helper::renderDescriptiveText(std::format("({})", c).c_str());

            if (dataSettings.hasDescription(c)) {
                ImGui::SameLine();
                view::helper::renderHelpMarker(dataSettings.description(c).c_str());
            }
        }
        ImGui::EndChild();
    }
    ImGui::EndChild();

    ImGui::SameLine(0, groupsSpacing);

    // Other columns
    ImGui::BeginChild("OtherColumnsGroup", ImVec2(group2Width, groupsHeight), false);
    {
        ImGui::Text("Other columns:");

        ImGui::SameLine();
        view::helper::renderHelpMarker(
            "Any other columns that may exist in the dataset."
        );

        ImGui::SameLine(0, 10);

        ImGui::PushID("clear_other");
        if (ImGui::Button("Clear")) {
            _selectedOtherColumns.assign(_otherColumns.size(), false);
        }
        ImGui::PopID();

        ImGui::SameLine();
        static bool showOnlySelected = false;
        ImGui::Checkbox("Show only checked", &showOnlySelected);

        static ImGuiTextFilter filter;
        filter.Draw();

        ImGui::BeginChild(
            "OtherColumns",
            ImVec2(0.f, ImGui::GetContentRegionAvail().y),
            false
        );
        ImGui::Columns(2, nullptr, false);
        for (int i = 0; i < _otherColumns.size(); i++) {
            const ColumnKey& c = _otherColumns[i];

            if (!filter.PassFilter(c.c_str())) {
                continue;
            }

            bool isSelected = _selectedOtherColumns[i];

            if (showOnlySelected && !isSelected) {
                continue;
            }

            if (ImGui::Checkbox(dataSettings.columnName(c), &isSelected)) {
                selectionChanged = true;
            }
            _selectedOtherColumns[i] = isSelected;

            nSelected += _selectedOtherColumns[i] ? 1 : 0;

            ImGui::NextColumn();
        }
        ImGui::Columns(1);
        ImGui::EndChild();
    }
    ImGui::EndChild();

    ImGui::SameLine(0, groupsSpacing);

    // Computed columns
    ImGui::BeginChild("ComputedColumnsGroup", ImVec2(group3Width, groupsHeight), false);
    {
        ImGui::TextUnformatted("Computed:");
        ImGui::SameLine();
        ImGui::PushID("clear_computed");
        if (ImGui::Button("Clear")) {
            _selectedComputedColumns.assign(_computedColumns.size(), false);
            selectionChanged = true;
        }
        ImGui::PopID();

        if (_computedColumns.empty()) {
            ImGui::TextDisabled("No computed columns");
        }
        else {
            ImGui::BeginChild(
                "ComputedColumns",
                ImVec2(0.f, ImGui::GetTextLineHeightWithSpacing() * 6.f),
                false
            );
            for (size_t index = 0; index < _computedColumns.size(); ++index) {
                bool isSelected = _selectedComputedColumns[index];
                if (ImGui::Checkbox(_computedColumns[index].c_str(), &isSelected)) {
                    selectionChanged = true;
                }
                _selectedComputedColumns[index] = isSelected;
                nSelected += isSelected ? 1 : 0;
            }
            ImGui::EndChild();
        }
    }
    ImGui::EndChild();

    // Selected columns summary
    ImGui::Spacing();
    ImGui::Separator();
    ImGui::Spacing();

    if (ImGui::BeginChild("SelectedColumnsSummary")) {
        ImGui::BeginGroup();
        {
            ImGui::Text("Selected columns:");
            ImGui::SameLine(0.f, 20.f);
            view::helper::renderDescriptiveText(std::format("({})", nSelected).c_str());

            if (nSelected > IMGUI_TABLE_MAX_COLUMNS) {
                ImGui::TextColored(
                    view::helper::toImVec4(view::colors::Error),
                    std::format(
                        "Invalid number of columns! Cannot be more than {}",
                        IMGUI_TABLE_MAX_COLUMNS
                    ).c_str()
                );
            }

            if (nSelected == 0) {
                ImGui::TextColored(
                    view::helper::toImVec4(view::colors::Error),
                    "Select at least one column!"
                );
            }

            // Always include name column
            const bool hasNameColumn = !dataSettings.nameColumn().empty();
            const ColumnKey nameColumn = hasNameColumn ? dataSettings.nameColumn() : "name";

            std::string columnsText = dataSettings.columnName(nameColumn);
            if (columnsText.empty()) {
                columnsText = std::string(nameColumn);
            }

            // Add selected named columns
            for (int i = 0; i < static_cast<int>(_namedColumns.size()); ++i) {
                if (_selectedNamedColumns[i] && _namedColumns[i] != nameColumn) {
                    columnsText += ", " + std::string(dataSettings.columnName(_namedColumns[i]));
                }
            }

            // Add selected other columns
            for (int i = 0; i < static_cast<int>(_otherColumns.size()); ++i) {
                if (_selectedOtherColumns[i] && _otherColumns[i] != nameColumn) {
                    columnsText += ", " + std::string(dataSettings.columnName(_otherColumns[i]));
                }
            }

            for (int i = 0; i < static_cast<int>(_computedColumns.size()); ++i) {
                if (_selectedComputedColumns[i]) {
                    columnsText += ", " + _computedColumns[i];
                }
            }

            ImGui::PushTextWrapPos(0.f);
            ImGui::TextUnformatted(columnsText.c_str());
            ImGui::PopTextWrapPos();

            ImGui::EndGroup();
        }
    }
    ImGui::EndChild();

    return selectionChanged;
}

void ColumnSelectionView::updateComputedColumns(
    const std::map<ColumnKey, ComputedColumn>& columns)
{
    std::vector<bool> selections;
    selections.reserve(columns.size());
    for (const auto& [key, _] : columns) {
        auto previous = std::find(_computedColumns.begin(), _computedColumns.end(), key);
        if (previous == _computedColumns.end()) {
            selections.push_back(false);
        }
        else {
            const size_t index = std::distance(_computedColumns.begin(), previous);
            selections.push_back(_selectedComputedColumns[index]);
        }
    }

    _computedColumns.clear();
    _computedColumns.reserve(columns.size());
    for (const auto& [key, _] : columns) {
        _computedColumns.push_back(key);
    }
    _selectedComputedColumns = std::move(selections);
}

std::vector<ColumnKey> ColumnSelectionView::orderedSelectedColumns() const {
    std::vector<ColumnKey> selectedColumns;
    selectedColumns.reserve(IMGUI_TABLE_MAX_COLUMNS);
    // Always include name column
    selectedColumns.push_back(_nameColumn);
    for (int i = 0; i < _namedColumns.size(); i++) {
        if (_selectedNamedColumns[i]) {
            selectedColumns.push_back(_namedColumns[i]);
        }
    }
    for (int i = 0; i < _otherColumns.size(); i++) {
        if (_selectedOtherColumns[i]) {
            selectedColumns.push_back(_otherColumns[i]);
        }
    }
    for (int i = 0; i < _computedColumns.size(); i++) {
        if (_selectedComputedColumns[i]) {
            selectedColumns.push_back(_computedColumns[i]);
        }
    }
    selectedColumns.shrink_to_fit();
    return selectedColumns;
}

} // namespace openspace::exoplanets
