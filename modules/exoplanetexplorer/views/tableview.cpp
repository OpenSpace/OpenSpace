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

#include <modules/exoplanetexplorer/views/tableview.h>

#include <modules/exoplanetexplorer/dataloader.h>
#include <modules/exoplanetexplorer/dataviewer.h>
#include <modules/exoplanetexplorer/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/logging/logmanager.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <stdexcept>


namespace {
    constexpr std::string_view _loggerCat = "Exoplanets Explorer Table";

    std::filesystem::path defaultPinnedItemsDirectory() {
        return absPath("${MODULE_EXOPLANETEXPLORER}/cache");
    }
}

namespace openspace::exoplanets {

TableView::TableView(DataViewer& dataViewer, const std::vector<ColumnKey>& columns)
    : _dataViewer(dataViewer), _columns(columns)
{}

void TableView::updateColumns(const std::vector<ColumnKey>& columns) {
    _columns = columns;
}

void TableView::render(bool* open, std::vector<size_t>& filteredDataRows) {
    ImGui::SetNextWindowSize(view::helper::DefaultWindowSize, ImGuiCond_FirstUseEver);

    if (!ImGui::Begin("Table", open, ImGuiWindowFlags_MenuBar)) {
        ImGui::End();
        return;
    }

    if (ImGui::BeginMenuBar()) {
        if (ImGui::BeginMenu("File")) {
            ImGui::BeginDisabled(_pinnedItems.empty());
            if (ImGui::MenuItem("Export pinned items...")) {
                _openExportPopup = true;
                _exportStatus.clear();
                _exportStatusIsError = false;
            }
            ImGui::EndDisabled();
            ImGui::EndMenu();
        }
        if (ImGui::BeginMenu("Settings")) {
            if (ImGui::MenuItem("Set table columns...")) {
                _dataViewer.openColumnSelectionView();
            }
            ImGui::EndMenu();
        }
        ImGui::EndMenuBar();
    }

    if (_openExportPopup) {
        ImGui::OpenPopup("Export pinned items");
        _openExportPopup = false;
    }
    renderExportPopup();

    if (!_exportStatus.empty()) {
        const ImVec4 statusColor = view::helper::toImVec4(
            _exportStatusIsError ? view::colors::Error : view::colors::Good
        );
        ImGui::TextColored(statusColor, "%s", _exportStatus.c_str());
    }

    // @TODO: Maybe do a more sophisticated comparison view
    bool showPinnedTable = ImGui::CollapsingHeader("Pinned items");
    ImGui::SameLine();
    view::helper::renderDescriptiveText(
        std::format("({})", _pinnedItems.size()).c_str()
    );
    if (showPinnedTable) {
        renderTable("pinned_items_table", _pinnedItems, true);
    }

    ImGui::Separator();
    view::helper::renderDescriptiveText(std::format(
        "Showing {} / {} planets",
        filteredDataRows.size(), _dataViewer.data().size()
    ).c_str());


    // Search table
    static char searchString[128] = "";
    ImGui::InputTextWithHint(
        "##Query",
        "Search for an item by name here...",
        searchString,
        IM_ARRAYSIZE(searchString)
    );
    ImGui::SameLine();
    if (ImGui::Button("Clear")) {
        strcpy(searchString, "");
    }

    ImGui::SameLine();
    const std::string pinButtonLabel = std::format(
        "Pin selected ({})",
        _selection.size()
    );
    const float pinButtonWidth =
        ImGui::CalcTextSize(pinButtonLabel.c_str()).x +
        2.f * ImGui::GetStyle().FramePadding.x;
    const float currentX = ImGui::GetCursorPosX();
    const float rightAlignedX = currentX + ImGui::GetContentRegionAvail().x - pinButtonWidth;
    if (rightAlignedX > currentX) {
        ImGui::SetCursorPosX(rightAlignedX);
    }
    ImGui::BeginDisabled(_selection.empty());
    if (ImGui::Button(pinButtonLabel.c_str())) {
        for (const size_t index : _selection) {
            const auto found = std::find(_pinnedItems.begin(), _pinnedItems.end(), index);
            if (found == _pinnedItems.end()) {
                _pinnedItems.push_back(index);
            }
        }
    }
    ImGui::EndDisabled();

    renderTable("full_exoplanets_table", filteredDataRows, false, searchString);

    ImGui::End();
}

void TableView::renderExportPopup() {
    const ImGuiWindowFlags flags = ImGuiWindowFlags_AlwaysAutoResize;
    if (!ImGui::BeginPopupModal("Export pinned items", nullptr, flags)) {
        return;
    }

    static char name[128] = "pinned_items.csv";
    static char directory[512] = "";

    ImGui::InputText("Filename (.csv)", name, IM_ARRAYSIZE(name));

    const std::string defaultDirectory = defaultPinnedItemsDirectory().string();
    ImGui::SetNextItemWidth(ImGui::CalcTextSize(defaultDirectory.c_str()).x * 1.3f);
    ImGui::InputTextWithHint(
        "Directory",
        defaultDirectory.c_str(),
        directory,
        IM_ARRAYSIZE(directory)
    );

    const bool hasName = name[0] != '\0';
    std::filesystem::path filePath =
        std::filesystem::path(directory[0] == '\0' ? defaultDirectory : directory) / name;
    if (filePath.extension() != ".csv") {
        filePath += ".csv";
    }
    filePath = absPath(filePath.string());

    std::error_code ec;
    const bool exists = hasName && std::filesystem::exists(filePath, ec);
    if (exists) {
        ImGui::TextColored(
            ImVec4(1.f, 0.7f, 0.3f, 1.f),
            "A file with this name exists and will be overwritten"
        );
    }
    if (!_exportStatus.empty() && _exportStatusIsError) {
        ImGui::TextColored(
            view::helper::toImVec4(view::colors::Error),
            "%s",
            _exportStatus.c_str()
        );
    }

    ImGui::Separator();
    if (ImGui::Button("Cancel") || ImGui::IsKeyPressed(ImGuiKey_Escape)) {
        ImGui::CloseCurrentPopup();
    }
    ImGui::SameLine();
    ImGui::BeginDisabled(!hasName);
    if (ImGui::Button(exists ? "Overwrite" : "Export") ||
        (hasName && ImGui::IsKeyPressed(ImGuiKey_Enter)))
    {
        try {
            savePinnedItemsToCsv(filePath);
            _exportStatus = std::format(
                "Exported {} pinned items to '{}'",
                _pinnedItems.size(),
                filePath.string()
            );
            _exportStatusIsError = false;
            name[0] = '\0';
            ImGui::CloseCurrentPopup();
        }
        catch (const std::exception& e) {
            _exportStatus = std::format("Failed to export pinned items: {}", e.what());
            _exportStatusIsError = true;
        }
    }
    ImGui::EndDisabled();
    ImGui::EndPopup();
}

void TableView::savePinnedItemsToCsv(const std::filesystem::path& path) const {
    std::error_code ec;
    std::filesystem::create_directories(path.parent_path(), ec);
    if (ec) {
        throw std::runtime_error(std::format(
            "Could not create directory '{}': {}",
            path.parent_path().string(),
            ec.message()
        ));
    }

    DataLoader::validateSaveTarget(path, _dataViewer.currentDataFile());
    DataLoader::saveData(
        path,
        _dataViewer.data(),
        _pinnedItems,
        _dataViewer.columns(),
        _dataViewer.computedColumns(),
        true
    );
}

void TableView::renderFirstTableColumn(const ExoplanetItem& item, size_t row) {
    const float RowHeight = ImGui::GetTextLineHeightWithSpacing(); // Inner height

    if (_dataViewer.systemViewer()->hasSystemBeenAdded(item.hostName) &&
        _dataViewer.systemViewer()->systemCanBeAdded(item.hostName))
    {
        // System was added, but scene graph node does not exist (not enough data to visualize)
        view::helper::renderHelpMarker(
            "There is not enough data to visualize this system"
        );
    }
    else if (_dataViewer.systemViewer()->systemCanBeAdded(item.hostName)) {
        ImGui::PushID(std::format("addbutton{}", row).c_str());
        if (ImGui::Button("+", ImVec2(20, RowHeight))) {
            _dataViewer.systemViewer()->addExoplanetSystem(item.hostName);
        }
        ImGui::PopID();
    }
    else {
        // Add a target button instead
        ImGui::PushID(std::format("targetbutton{}", row).c_str());

        // Check if is target item. The GUI name should be set from the planet nameö
        const SceneGraphNode* node = global::navigationHandler->anchorNode();
        bool isCurrentAnchor = node && node->guiName() == item.name;
        if (isCurrentAnchor) {
            ImGui::PushStyleColor(ImGuiCol_Button, ImColor(0, 153, 112).Value);
            ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImColor(0, 204, 150).Value);
        }
        else {
            // A slightly darker blue color
            ImGui::PushStyleColor(ImGuiCol_Button, ImColor(23, 43, 71).Value);
            ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImColor(71, 135, 223).Value);
        }

        if (ImGui::Button("->", ImVec2(20, RowHeight))) {
            _dataViewer.systemViewer()->addOrTargetPlanet(item);
        }
        ImGui::PopStyleColor(2);

        ImGui::PopID();
    }
}

void TableView::renderTable(const std::string& tableId, std::vector<size_t>& dataRows,
                            bool useFixedHeight, std::string_view search)
{
    static ImGuiTableFlags flags =
        ImGuiTableFlags_ScrollX | ImGuiTableFlags_ScrollY
        | ImGuiTableFlags_BordersV | ImGuiTableFlags_BordersOuter
        | ImGuiTableFlags_Reorderable | ImGuiTableFlags_Hideable
        | ImGuiTableFlags_Sortable | ImGuiTableFlags_Resizable
        | ImGuiTableFlags_RowBg;

    const int nColumns = static_cast<int>(_columns.size());

    const std::vector<ExoplanetItem>& data = _dataViewer.data();

    // Some size variables
    const float RowHeight = ImGui::GetTextLineHeightWithSpacing(); // Inner height
    const float TableHeight =
        (dataRows.size() + 1) * 1.2f * RowHeight + ImGui::GetStyle().ScrollbarSize;
    const ImVec2 TableSize = ImVec2(0.f, useFixedHeight ? TableHeight : 0.f);

    if (ImGui::BeginTable(tableId.c_str(), nColumns + 1, flags, TableSize)) {
        // Extra column with add button
        ImGuiTableColumnFlags firstColFlags = ImGuiTableColumnFlags_NoResize |
            ImGuiTableColumnFlags_WidthFixed | ImGuiTableColumnFlags_NoSort |
            ImGuiTableColumnFlags_NoHide;
        ImGui::TableSetupColumn("", firstColFlags, 0.f);

        // Columns
        for (int colIdx = 0; colIdx < _columns.size(); colIdx++) {
            ImGuiTableColumnFlags colFlags = ImGuiTableColumnFlags_PreferSortDescending;
            const ColumnKey c = _columns[colIdx];
            if (_dataViewer.isNameColumn(c)) {
                colFlags |= ImGuiTableColumnFlags_DefaultSort;
            }
            ImGui::TableSetupColumn(_dataViewer.columnName(c), colFlags, 0.f, colIdx);
        }

        // Make header and first column (name) always visible
        ImGui::TableSetupScrollFreeze(2, 1);

        // Instead of calling TableHeadersRow(), we set up custom headers with help markers
        //ImGui::TableHeadersRow();
        ImGui::TableNextRow(ImGuiTableRowFlags_Headers);

        ImGui::TableHeader("");

        for (int i = 0; i < _columns.size(); i++) {
            const ColumnKey& col = _columns[i];
            const char* name = _dataViewer.columnName(col);

            ImGui::TableSetColumnIndex(i + 1);
            ImGui::PushID(i);
            ImGui::TableHeader(name);

            if (_dataViewer.dataSettings().hasDescription(col)) {
                const float TEXT_WIDTH = ImGui::CalcTextSize(name).x;
                ImGui::SameLine(0.0f, TEXT_WIDTH + 2.f);
                view::helper::renderHelpMarker(
                    _dataViewer.dataSettings().description(col).c_str()
                );
            }

            ImGui::PopID();
        }

        // Sorting
        if (ImGuiTableSortSpecs* sortSpecs = ImGui::TableGetSortSpecs()) {
            const bool hasSortSpec = (sortSpecs->SpecsCount > 0 && sortSpecs->Specs != nullptr);
            if (hasSortSpec && (sortSpecs->SpecsDirty || _dataViewer.filterChanged())) {
                auto compare = [&sortSpecs, &data, this](const size_t& lhs, const size_t& rhs) {
                    ImGuiSortDirection sortDir = sortSpecs->Specs->SortDirection;
                    bool flip = (sortDir == ImGuiSortDirection_Descending);

                    const ExoplanetItem& l = flip ? data[rhs] : data[lhs];
                    const ExoplanetItem& r = flip ? data[lhs] : data[rhs];

                    int colIndex = static_cast<int>(sortSpecs->Specs->ColumnUserID);
                    ColumnKey key = _columns[colIndex];
                    return _dataViewer.compareColumnValues(key, l, r);
                };

                std::sort(dataRows.begin(), dataRows.end(), compare);
                sortSpecs->SpecsDirty = false;
            }
        }

        std::vector<size_t> displayedRows;
        if (search.empty()) {
            displayedRows = dataRows;
        }
        else {
            for (size_t r : dataRows) {
                bool passSearch = ColumnFilter(
                    std::string(search),
                    ColumnFilter::Type::Text
                ).passFilter(data[r].name);

                if (passSearch) {
                    displayedRows.push_back(r); // Go to next
                }
            }
        }

        // Rows
        ImGuiListClipper clipper;
        clipper.Begin(static_cast<int>(displayedRows.size()));
        while (clipper.Step()) {
            for (size_t row = clipper.DisplayStart; row < clipper.DisplayEnd; row++) {
                const size_t index = displayedRows[row];
                const ExoplanetItem& item = data[index];

                ImGuiSelectableFlags selectableFlags = ImGuiSelectableFlags_SpanAllColumns
                    | ImGuiSelectableFlags_AllowOverlap;

                auto found = std::find(_selection.begin(), _selection.end(), index);
                const bool itemIsSelected = found != _selection.end();

                ImGui::TableNextRow(ImGuiTableRowFlags_None, RowHeight);

                ImGui::TableNextColumn();
                renderFirstTableColumn(item, row);

                for (int i = 0; i < _columns.size(); i++) {
                    const ColumnKey& col = _columns[i];
                    ImGui::TableNextColumn();

                    if (_dataViewer.isNameColumn(col)) {
                        bool changed = ImGui::Selectable(
                            item.name.c_str(),
                            itemIsSelected,
                            selectableFlags
                        );

                        // Context menu
                        ImGui::PushID(std::format("context-{}", item.name).c_str());
                        if (ImGui::BeginPopupContextItem("item context menu")) {
                            ImGui::Text(item.name.c_str());

                            auto foundIndex = std::find(
                                _pinnedItems.begin(),
                                _pinnedItems.end(),
                                index
                            );
                            bool isPinned = foundIndex != _pinnedItems.end();

                            ImGui::SameLine();
                            ImGui::SetNextItemWidth(-10);
                            if (ImGui::Button(isPinned ? "Unpin" : "Pin")) {
                                if (isPinned) {
                                    _pinnedItems.erase(foundIndex);
                                }
                                else {
                                    _pinnedItems.push_back(index);
                                }
                            }

                            ImGui::Separator();

                            ImGui::TextLinkOpenURL(
                                item.referenceName.c_str(),
                                item.referenceUrl.c_str()
                            );

                            ImGui::Separator();
                            if (!_dataViewer.dataMapping().hostName.empty() &&
                                !item.hostName.empty())
                            {
                                _dataViewer.systemViewer()->renderSystemViewQuickControls(
                                    item.hostName
                                );
                            }
                            else if (ImGui::Button("Show object view")) {
                                _dataViewer.systemViewer()->showObjectView(index);
                                ImGui::CloseCurrentPopup();
                            }

                            ImGui::EndPopup();
                        }
                        ImGui::PopID();

                        // Check double click, left mouse button
                        if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(0)) {
                            LINFO(std::format("Double click: {}", item.name));
                            bool isPlanetSystem = !_dataViewer.dataMapping().hostName.empty() &&
                                !item.hostName.empty();

                            if (isPlanetSystem) {
                                _dataViewer.systemViewer()->addOrTargetPlanet(item);
                                _dataViewer.systemViewer()->showSystemView(item.hostName);
                            }
                            else {
                                _dataViewer.systemViewer()->showObjectView(index);
                            }
                        }

                        if (changed) {
                            if (ImGui::GetIO().KeyCtrl) {
                                if (itemIsSelected) {
                                    _selection.erase(found);
                                }
                                else {
                                    _selection.push_back(index);
                                }
                            }
                            else {
                                _selection.clear();
                                _selection.push_back(index);
                            }
                        }
                        continue;
                    }

                    _dataViewer.renderColumnValue(col, item);
                }
            }
        }
        ImGui::EndTable();
    }
}

} // namespace openspace::exoplanets
