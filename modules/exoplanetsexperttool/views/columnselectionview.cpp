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

    allColumnsOrdered.shrink_to_fit();

    return allColumnsOrdered;
}

bool ColumnSelectionView::renderColumnSettingsView(const DataSettings& dataSettings) {
    static bool showSetColumnsWindow = false;
    if (ImGui::Button("Set up table columns...")) {
        showSetColumnsWindow = true;
    }

    if (!showSetColumnsWindow) {
        return false;
    }

    // Always center this window when appearing
    ImVec2 center = ImGui::GetMainViewport()->GetCenter();
    ImGui::SetNextWindowPos(center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));

    ImGuiWindowFlags flags = ImGuiWindowFlags_NoSavedSettings;
    if (!ImGui::Begin("Set table columns", &showSetColumnsWindow, flags)) {
        ImGui::End();
        return false;
    }

    int nSelected = 0;
    bool selectionChanged = false;

    auto resetSelection = [&selectionChanged, this]() {
        _selectedNamedColumns = _savedSelectedNamedColumns;
        _selectedOtherColumns = _savedSelectedOtherColumns;
        selectionChanged = false;
    };

    auto applySelection = [&selectionChanged, this]() {
        _savedSelectedNamedColumns = _selectedNamedColumns;
        _savedSelectedOtherColumns = _selectedOtherColumns;
        selectionChanged = true;
    };

    ImGui::TextWrapped(
        "Select which columns to show in the tables in the interface."
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

        ImGui::SameLine(0, 20);

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

        ImGui::BeginChild(
            "NamedColumns",
            ImVec2(
                ImGui::GetContentRegionAvail().x * 0.4f,
                ImGui::GetContentRegionAvail().y * 0.8f
            ),
            false
        );

        for (int i = 0; i < _namedColumns.size(); i++) {
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
        ImGui::EndChild();

        ImGui::EndGroup();
    }
    ImGui::SameLine(0, 10);

    // Other columns
    ImGui::BeginGroup();
    {
        ImGui::Text("Other columns:");

        ImGui::SameLine();
        view::helper::renderHelpMarker(
            "This is any other columns that may exist in the dataset."
        );

        ImGui::SameLine(0, 20);

        ImGui::PushID("clear_other");
        if (ImGui::Button("Clear selection")) {
            _selectedOtherColumns.assign(_otherColumns.size(), false);
        }
        ImGui::PopID();

        ImGui::SameLine(0, 20);
        static bool showOnlySelected = false;
        ImGui::Checkbox("Show only checked", &showOnlySelected);

        static ImGuiTextFilter filter;
        ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x * 0.4f);
        filter.Draw();

        ImGui::BeginChild(
            "OtherColumns",
            ImVec2(
                ImGui::GetContentRegionAvail().x * 0.9f,
                ImGui::GetContentRegionAvail().y * 0.8f
            ),
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

            ImGui::Checkbox(dataSettings.columnName(c), &isSelected);
            _selectedOtherColumns[i] = isSelected;

            nSelected += _selectedOtherColumns[i] ? 1 : 0;

            ImGui::NextColumn();
        }
        ImGui::Columns(1);
        ImGui::EndChild();

        ImGui::EndGroup();
    }

    // Selected columns summary
    ImGui::Spacing();
    ImGui::Separator();
    ImGui::Spacing();
    ImGui::BeginGroup();
    {
        ImGui::Text("Selected columns:");
        // Always include name column
        bool hasNameColumn = !dataSettings.nameColumn().empty();
        const ColumnKey nameColumn = hasNameColumn ? dataSettings.nameColumn() : "name";
        std::string columnsText = dataSettings.columnName(nameColumn);

        // Add selected named columns
        for (int i = 0; i < _namedColumns.size(); i++) {
            if (_selectedNamedColumns[i]) {
                columnsText += ", " + std::string(dataSettings.columnName(_namedColumns[i]));
            }
        }

        // Add selected other columns
        for (int i = 0; i < _otherColumns.size(); i++) {
            if (_selectedOtherColumns[i]) {
                columnsText += ", " + std::string(dataSettings.columnName(_otherColumns[i]));
            }
        }

        ImGui::TextWrapped("%s", columnsText.c_str());

        ImGui::EndGroup();
    }
    ImGui::Spacing();

    // Confirmaiton / Cancellation
    {
        // Push to bottom of window
        float buttonHeight = ImGui::GetFrameHeightWithSpacing() * 2; // Space for text + buttons
        float availableHeight = ImGui::GetContentRegionAvail().y;
        if (availableHeight > buttonHeight) {
            ImGui::Dummy(ImVec2(0.0f, availableHeight - buttonHeight));
        }

        ImGui::Separator();

        bool isInvalidColumnNr = nSelected > IMGUI_TABLE_MAX_COLUMNS || nSelected == 0;
        glm::vec4 textColor =
            isInvalidColumnNr ? view::colors::Error : view::colors::DescriptiveText;

        // Float to right
        float buttonWidth = 120.0f;
        float spacing = ImGui::GetStyle().ItemSpacing.x;
        float textWidth = ImGui::CalcTextSize(
            std::format("Selected: {} / {}", nSelected, IMGUI_TABLE_MAX_COLUMNS).c_str()
        ).x;
        float totalWidth = textWidth + spacing + buttonWidth + spacing + buttonWidth;

        ImGui::SetCursorPosX(ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x - totalWidth);

        ImGui::TextColored(
            view::helper::toImVec4(textColor),
            std::format(
                "Selected: {} / {}", nSelected, IMGUI_TABLE_MAX_COLUMNS
            ).c_str()
        );

        ImGui::SameLine();

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

        if (isInvalidColumnNr) {
            ImGui::BeginDisabled();
        }

        if (ImGui::Button("Save", ImVec2(120, 0))) {
            applySelection();
            showSetColumnsWindow = false;
            //ImGui::CloseCurrentPopup();
        }

        if (isInvalidColumnNr) {
            ImGui::PopStyleColor(3);
            ImGui::EndDisabled();
        }

        ImGui::SetItemDefaultFocus();
        ImGui::SameLine();
        if (ImGui::Button("Cancel", ImVec2(120, 0))) {
            resetSelection();
            showSetColumnsWindow = false;
            //ImGui::CloseCurrentPopup();
        }
    }
    ImGui::End();

    return selectionChanged;
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
    selectedColumns.shrink_to_fit();
    return selectedColumns;
}

} // namespace openspace::exoplanets
