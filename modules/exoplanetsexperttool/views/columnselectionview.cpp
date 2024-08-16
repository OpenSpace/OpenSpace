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

#include <modules/exoplanetsexperttool/views/columnselectionview.h>

#include <modules/exoplanetsexperttool/datahelper.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>

namespace openspace::exoplanets {

std::vector<ColumnKey> ColumnSelectionView::initializeColumnsFromData(
                                                  const std::vector<ExoplanetItem>& data,
                                                  const DataSettings& dataSettings)
{
    std::vector<ColumnKey> result;

    if (data.empty()) {
        return result;
    }

    const auto& allDataColumns = data.front().dataColumns;
    const auto& columnsWithSettings = dataSettings.columnInfo;

    auto isNameColumn = [&dataSettings](const ColumnKey & key) {
        return key == dataSettings.dataMapping.name;
    };

    // The name column is required and should be handled separately, to always be the
    // first column
    bool hasNameColumn = !dataSettings.nameColumn().empty();
    result.push_back(hasNameColumn ? dataSettings.nameColumn() : "name");

    // The default column are the which info has been provided for, if they exist in
    // the dataset
    _namedColumns.reserve(columnsWithSettings.size());
    for (auto const& [key, _] : columnsWithSettings) {
        if (allDataColumns.contains(key) && !isNameColumn(key)) {
            _namedColumns.push_back(key);
        }
    }
    _namedColumns.shrink_to_fit();

    // Sort the defauls columns based on provided name instead of key
    std::sort(
        _namedColumns.begin(),
        _namedColumns.end(),
        [&dataSettings, this](const ColumnKey& lhs, const ColumnKey& rhs) {
            return data::caseInsensitiveLessThan(
                dataSettings.columnName(lhs),
                dataSettings.columnName(rhs)
            );
        }
    );

    result.insert(result.end(), _namedColumns.begin(), _namedColumns.end());
    _selectedNamedColumns.assign(result.size(), true);

    // Add other columns, if there are any. Assume all items have the same columns
    _otherColumns.reserve(allDataColumns.size());
    for (auto const& [key, _] : allDataColumns) {
        if (!columnsWithSettings.contains(key) && !isNameColumn(key)) {
            _otherColumns.push_back(key);
        }
    }
    _otherColumns.shrink_to_fit();
    _selectedOtherColumns.assign(_otherColumns.size(), false);

    // Fill up with other columns
    for (size_t i = 0; i < _otherColumns.size(); ++i) {
        if (!(result.size() < IMGUI_TABLE_MAX_COLUMNS)) {
            break;
        }
        const ColumnKey& key = _otherColumns[i];
        result.push_back(key);
        _selectedOtherColumns[i] = true;
    }

    return result;
}

void ColumnSelectionView::renderColumnSettingsView(std::vector<ColumnKey>& columnsToEdit,
                                                   const DataSettings& dataSettings)
{
    if (ImGui::Button("Set up columns...")) {
        ImGui::OpenPopup("Set columns");
    }

    // Always center this window when appearing
    ImVec2 center = ImGui::GetMainViewport()->GetCenter();
    ImGui::SetNextWindowPos(center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));

    ImGuiWindowFlags flags = ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_HorizontalScrollbar;

    if (!ImGui::BeginPopupModal("Set columns", NULL, flags)) {
        return;
    }

    constexpr const int MaxItemsPerColumn = 20;
    int nSelected = 0;
    bool canSelectMore = true;

    std::vector<bool> prevSelectedDefault = _selectedNamedColumns;
    std::vector<bool> prevSelectedOther = _selectedOtherColumns;

    auto resetSelection = [this, &prevSelectedDefault, &prevSelectedOther]() {
        _selectedNamedColumns = prevSelectedDefault;
        _selectedOtherColumns = prevSelectedOther;
    };

    auto applySelection = [&columnsToEdit, &dataSettings, this](size_t nSelected) {
        columnsToEdit.clear();
        columnsToEdit.reserve(nSelected + 1);

        bool hasNameColumn = !dataSettings.nameColumn().empty();
        columnsToEdit.push_back(hasNameColumn ? dataSettings.nameColumn() : "name");

        for (int i = 0; i < _namedColumns.size(); i++) {
            if (_selectedNamedColumns[i]) {
                columnsToEdit.push_back(_namedColumns[i]);
            }
        }

        for (int i = 0; i < _otherColumns.size(); i++) {
            if (_selectedOtherColumns[i]) {
                columnsToEdit.push_back(_otherColumns[i]);
            }
        }
    };

    ImGui::Text(
        "This view controls which data columns are exposed in the application. "
        "Only the selected columns will be exposed in the table view, can be "
        "used for color mapping, filtering, et cetera. "
    );

    ImGui::Separator();

    // Required columns
    ImGui::BeginGroup();
    {
        ImGui::Text("Required columns:");
        ImGui::SameLine();
        view::helper::renderHelpMarker(
            "The name column, specified in the data mapping part the .json file with "
            "data settings, is required."
        );
        ImGui::Spacing();

        ImGui::Indent(8.f);
        {
            ImGui::Text("Object name: ");
            ImGui::SameLine();

            bool hasNameColumn = !dataSettings.nameColumn().empty();
            const ColumnKey nameColumn = hasNameColumn ? dataSettings.nameColumn() : "name";

            view::helper::renderDescriptiveText(
                dataSettings.columnName(nameColumn)
            );

            if (dataSettings.columnInfo.contains(nameColumn)) {
                ImGui::SameLine();
                view::helper::renderDescriptiveText(
                    std::format("({})", nameColumn).c_str()
                );

                if (dataSettings.hasDescription(nameColumn)) {
                    ImGui::SameLine();
                    view::helper::renderHelpMarker(dataSettings.description(nameColumn).c_str());
                }
            }
        }
        ImGui::Unindent(8.f);

        ImGui::Separator();

        ImGui::EndGroup();
    }

    ImGui::Spacing();

    // Named columns
    ImGui::BeginGroup();
    {
        ImGui::Text("Named columns:");

        ImGui::SameLine();
        view::helper::renderHelpMarker(
            "This is the columns specified with column information and a given name in "
            "the .json file with data settings."
        );

        ImGui::SameLine();

        ImGui::PushID("clear_default");
        if (ImGui::Button("Clear selection")) {
            _selectedNamedColumns.assign(_namedColumns.size(), false);
        }
        ImGui::PopID();
        ImGui::SameLine();

        ImGui::PushID("select_all_default");
        if (ImGui::Button("Select all")) {
            _selectedNamedColumns.assign(_namedColumns.size(), true);
        }
        ImGui::PopID();

        ImGui::BeginGroup();
        for (int i = 0; i < _namedColumns.size(); i++) {
            if (i % MaxItemsPerColumn == 0) {
                ImGui::EndGroup();
                ImGui::SameLine();
                ImGui::BeginGroup();
            }

            if (nSelected > IMGUI_TABLE_MAX_COLUMNS) {
                canSelectMore = false;
            }

            const ColumnKey& c = _namedColumns[i];

            bool isSelected = _selectedNamedColumns[i];
            ImGui::Checkbox(dataSettings.columnName(c), &isSelected);
            _selectedNamedColumns[i] = isSelected;

            nSelected += _selectedNamedColumns[i] ? 1 : 0;

            ImGui::SameLine();
            view::helper::renderDescriptiveText(std::format("({})", c).c_str());

            if (dataSettings.hasDescription(c)) {
                ImGui::SameLine();
                view::helper::renderHelpMarker(dataSettings.description(c).c_str());
            }
        }
        ImGui::EndGroup();

        ImGui::EndGroup();
    }
    ImGui::SameLine();

    // Other columns
    ImGui::BeginGroup();
    {
        ImGui::Text("Other columns:");

        ImGui::SameLine();
        view::helper::renderHelpMarker(
            "This is any other columns that may exist in the dataset."
        );

        ImGui::SameLine();

        ImGui::PushID("clear_other");
        if (ImGui::Button("Clear selection")) {
            _selectedOtherColumns.assign(_otherColumns.size(), false);
        }
        ImGui::PopID();

        ImGui::BeginGroup();
        for (int i = 0; i < _otherColumns.size(); i++) {
            if (i % MaxItemsPerColumn == 0) {
                ImGui::EndGroup();
                ImGui::SameLine();
                ImGui::BeginGroup();
            }

            if (nSelected > IMGUI_TABLE_MAX_COLUMNS) {
                canSelectMore = false;
            }

            const ColumnKey& c = _otherColumns[i];

            bool isSelected = _selectedOtherColumns[i];
            ImGui::Checkbox(dataSettings.columnName(c), &isSelected);
            _selectedOtherColumns[i] = isSelected;

            nSelected += _selectedOtherColumns[i] ? 1 : 0;
        }
        ImGui::EndGroup();

        ImGui::EndGroup();
    }

    bool isInvalidColumnNr = nSelected > IMGUI_TABLE_MAX_COLUMNS || nSelected == 0;
    glm::vec4 textColor =
        isInvalidColumnNr ? view::colors::Error : view::colors::DescriptiveText;

    ImGui::TextColored(
        view::helper::toImVec4(textColor),
        std::format(
            "Selected: {} / {}", nSelected, IMGUI_TABLE_MAX_COLUMNS
        ).c_str()
    );

    // Ok / Cancel
    if (isInvalidColumnNr) {
        ImGui::PushStyleColor(
            ImGuiCol_Button,
            view::helper::toImVec4(view::colors::DisabledButton)
        );
        ImGui::PushStyleColor(
            ImGuiCol_ButtonHovered,
            view::helper::toImVec4(view::colors::DisabledButton)
        );
        ImGui::PushStyleColor(
            ImGuiCol_ButtonActive,
            view::helper::toImVec4(view::colors::DisabledButton)
        );
    }

    if (ImGui::Button("OK", ImVec2(120, 0)) && !isInvalidColumnNr) {
        applySelection(nSelected);
        ImGui::CloseCurrentPopup();
    }

    if (isInvalidColumnNr) {
        ImGui::PopStyleColor(3);
    }

    ImGui::SetItemDefaultFocus();
    ImGui::SameLine();
    if (ImGui::Button("Cancel", ImVec2(120, 0))) {
        resetSelection();
        ImGui::CloseCurrentPopup();
    }
    ImGui::EndPopup();
}

} // namespace openspace::exoplanets
