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

#include <modules/exoplanetsexperttool/views/filteringview.h>

#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/util/datahelper.h>
#include <modules/exoplanetsexperttool/views/spatialselectionview.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/json.h>
#include <implot.h>
#include <algorithm>
#include <fstream>
#include <stdexcept>

namespace {
    constexpr int FilterFileVersion = 1;
    constexpr std::string_view SpatialFileSuffix = ".spatial.json";

    std::filesystem::path defaultFilterDirectory() {
        return absPath("${MODULE_EXOPLANETSEXPERTTOOL}/data/filters");
    }

    std::vector<std::filesystem::path> savedFilterFiles() {
        std::vector<std::filesystem::path> files;
        std::error_code ec;
        const std::filesystem::path dir = defaultFilterDirectory();
        if (!std::filesystem::is_directory(dir, ec)) {
            return files;
        }
        for (const auto& entry : std::filesystem::directory_iterator(dir, ec)) {
            const std::filesystem::path& p = entry.path();
            if (entry.is_regular_file(ec) && p.extension() == ".json" &&
                !p.filename().string().ends_with(SpatialFileSuffix))
            {
                files.push_back(p);
            }
        }
        std::sort(files.begin(), files.end());
        return files;
    }
} // namespace

namespace openspace::exoplanets {

FilteringView::FilteringView(DataViewer& dataViewer,
                             const DataSettings& dataSettings)
    : _dataViewer(dataViewer)
    , _quickFilterGroups(dataSettings.quickFilterGroups)
{
    if (!_dataViewer.columns().empty()) {
        _newFilterColumn = _dataViewer.columns().front();
    }

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
            _rowLimitColumn = _dataViewer.columns()[i];
            break;
        }
    }
}

bool FilteringView::isUsingRowFiltering() const {
    return _limitNumberOfRows;
}

bool FilteringView::isUsingExternalFiltering() const {
    return _useExternalSelection;
}

bool FilteringView::isUsingSpatialFiltering() const {
    return _useSpatialSelection;
}

bool FilteringView::useCurrentSpatialQueryAsFilter() {
    const bool changed = !_useSpatialSelection ||
        _appliedSavedSpatialSelection.has_value();
    _useSpatialSelection = true;
    _appliedSavedSpatialSelection.reset();
    return changed;
}

bool FilteringView::disableSpatialFiltering() {
    const bool changed = _useSpatialSelection;
    _useSpatialSelection = false;
    return changed;
}

int FilteringView::activeFilters() const {
    return _nActiveFilters;
}

std::string FilteringView::rowLimitDescription() const {
    return std::format(
        "{} rows with {} {}",
        _nRows, _useHighestValue ? "highest" : "lowest",
        _dataViewer.columnName(rowLimitColumn())
    );
}

const ColumnKey& FilteringView::rowLimitColumn() const {
    if (_dataViewer.hasColumn(_rowLimitColumn)) {
        return _rowLimitColumn;
    }

    for (const ColumnKey& column : _dataViewer.columns()) {
        if (_dataViewer.isNumericColumn(column)) {
            return column;
        }
    }
    return _dataViewer.columns().front();
}

std::string FilteringView::spatialFilterDescription(
                                      const SpatialSelectionView& spatialSelectionView) const
{
    const SavedSelection* applied = appliedSavedSelection(spatialSelectionView);
    return applied ? applied->name : "Current spatial query";
}

const SavedSelection* FilteringView::appliedSavedSelection(
                                      const SpatialSelectionView& spatialSelectionView) const
{
    if (!_appliedSavedSpatialSelection.has_value()) {
        return nullptr;
    }
    const std::vector<SavedSelection>& saved =
        spatialSelectionView.handler().savedSelections();
    const auto it = std::find_if(
        saved.begin(),
        saved.end(),
        [this](const SavedSelection& s) { return s.id == *_appliedSavedSpatialSelection; }
    );
    return it != saved.end() ? &*it : nullptr;
}

void FilteringView::renderAppliedColumnFilters() const {
    bool didRender = false;
    for (const ColumnFilterEntry& f : _columnFilters) {
        if (!f.enabled || !_dataViewer.hasColumn(f.column)) {
            continue;
        }
        const std::string& query = f.filter.query();
        ImGui::TextUnformatted(
            std::format("{}: {}", _dataViewer.columnName(f.column), query == "" ? "Has value" : query).c_str()
        );
        didRender = true;
    }
    for (size_t groupId = 0; groupId < _quickFilterFlags.size(); ++groupId) {
        const DataSettings::QuickFilterGroup& group =
            _quickFilterGroups[groupId];
        for (size_t qIndex = 0; qIndex < _quickFilterFlags[groupId].size(); ++qIndex) {
            const DataSettings::QuickFilter& q = group.quickFilters[qIndex];
            bool shouldMatchFilter = _quickFilterFlags[groupId][qIndex];
            if (shouldMatchFilter) {
                if (group.title != "") {
                    ImGui::TextUnformatted(
                        std::format("{}: {}", group.title, q.name).c_str()
                    );
                }
                else {
                    ImGui::TextUnformatted(q.name.c_str());
                }
                didRender = true;
            }
        }
    }

    if (!didRender) {
        ImGui::TextUnformatted("No active filters");
    }
}

bool FilteringView::render(bool* open, const SpatialSelectionView& spatialSelectionView) {
    ImGui::SetNextWindowSize(ImVec2(430, 450), ImGuiCond_FirstUseEver);
    if (!ImGui::Begin("Filters", open, ImGuiWindowFlags_MenuBar)) {
        ImGui::End();
        return false;
    }

    bool filterWasChanged = false;

    filterWasChanged |= renderMenuBar();
    filterWasChanged |= renderSaveLoadPopups(spatialSelectionView);

    if (!_statusMessage.empty()) {
        const ImVec4 color = _statusIsError ?
            ImVec4(1.f, 0.4f, 0.4f, 1.f) : ImVec4(0.6f, 0.9f, 0.6f, 1.f);
        if (ImGui::SmallButton("x##DismissStatus")) {
            _statusMessage.clear();
            _statusWarning.clear();
        }
        ImGui::SameLine();
        ImGui::PushStyleColor(ImGuiCol_Text, color);
        ImGui::TextWrapped("%s", _statusMessage.c_str());
        ImGui::PopStyleColor();
        if (!_statusWarning.empty()) {
            ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.f, 0.7f, 0.3f, 1.f));
            ImGui::TextWrapped("%s", _statusWarning.c_str());
            ImGui::PopStyleColor();
        }
        ImGui::Separator();
    }

    ImGui::Text("Reset:");

    ImGui::SameLine();
    if (ImGui::Button("Internal")) {
        resetInternalFilters();
        filterWasChanged = true;
    }

    ImGui::SameLine();
    if (ImGui::Button("Row limit##RessetRowLimit")) {
        _limitNumberOfRows = false;
        filterWasChanged = true;
    }

    ImGui::SameLine();
    if (ImGui::Button("External")) {
        _useExternalSelection = false;
        _overrideInternalSelection = false;
        filterWasChanged = true;
    }

    ImGui::SameLine();
    if (ImGui::Button("Spatial")) {
        _useSpatialSelection = false;
        _appliedSavedSpatialSelection.reset();
        filterWasChanged = true;
    }

    filterWasChanged |= renderColumnFilterSettings();
    filterWasChanged |= renderRowLimitFilterSettings();
    filterWasChanged |= renderSpatialFilterSettings(spatialSelectionView);
    filterWasChanged |= renderExternalFilterSettings();

    ImGui::End();

    return filterWasChanged;
}

void FilteringView::resetInternalFilters() {
    _columnFilters.clear();
    _editingFilterIndex = -1;
    for (std::vector<bool>& flags : _quickFilterFlags) {
        flags.assign(flags.size(), false);
    }
    updateActiveFilterCount();
}

void FilteringView::updateActiveFilterCount() {
    _nActiveFilters = 0;
    for (const ColumnFilterEntry& f : _columnFilters) {
        if (f.enabled && _dataViewer.hasColumn(f.column)) {
            ++_nActiveFilters;
        }
    }
    for (const std::vector<bool>& group : _quickFilterFlags) {
        _nActiveFilters += static_cast<int>(std::count(group.begin(), group.end(), true));
    }
}

void FilteringView::setStatus(std::string message, bool isError, std::string warning) {
    _statusMessage = std::move(message);
    _statusIsError = isError;
    _statusWarning = std::move(warning);
}

void FilteringView::saveToFile(const std::filesystem::path& path,
                               const SpatialSelectionView& spatialSelectionView)
{
    nlohmann::json columnFilters = nlohmann::json::array();
    for (const ColumnFilterEntry& f : _columnFilters) {
        columnFilters.push_back({
            { "column", f.column },
            { "query", f.filter.query() },
            { "enabled", f.enabled }
        });
    }

    nlohmann::json quickFilters = nlohmann::json::array();
    for (size_t groupId = 0; groupId < _quickFilterFlags.size(); ++groupId) {
        const DataSettings::QuickFilterGroup& group = _quickFilterGroups[groupId];
        for (size_t i = 0; i < _quickFilterFlags[groupId].size(); ++i) {
            if (_quickFilterFlags[groupId][i]) {
                quickFilters.push_back({
                    { "group", group.title },
                    { "name", group.quickFilters[i].name }
                });
            }
        }
    }

    nlohmann::json json = {
        { "version", FilterFileVersion },
        { "dataFile", _dataViewer.currentDataFile().filename().string() },
        { "columnFilters", columnFilters },
        { "quickFilters", quickFilters },
        { "rowLimit", {
            { "enabled", _limitNumberOfRows },
            { "nRows", _nRows },
            { "useHighest", _useHighestValue },
            { "column", _rowLimitColumn }
        }}
    };

    std::filesystem::create_directories(path.parent_path());

    std::string spatialWarning;
    if (_useSpatialSelection) {
        std::optional<SavedSelection> selection;
        if (_appliedSavedSpatialSelection.has_value()) {
            if (const SavedSelection* s = appliedSavedSelection(spatialSelectionView)) {
                selection = *s;
            }
        }
        else if (auto query = spatialSelectionView.currentSpatialQuery(); query) {
            selection = SavedSelection{
                .name = std::format("{} (spatial)", path.stem().string()),
                .summary = "Saved with filters",
                .query = std::move(*query)
            };
        }

        if (selection.has_value()) {
            const std::string spatialFile =
                path.stem().string() + std::string(SpatialFileSuffix);
            SpatialSelectionHandler::saveSelectionsToFile(
                path.parent_path() / spatialFile,
                { *selection }
            );
            json["spatialFile"] = spatialFile;
        }
        else {
            spatialWarning = " (no spatial query to save)";
        }
    }

    std::ofstream file(path, std::ofstream::trunc);
    if (!file.good()) {
        throw std::runtime_error(std::format("Could not write to '{}'", path.string()));
    }
    file << json.dump(2);

    setStatus(std::format("Saved filters to '{}'{}", path.string(), spatialWarning));
}

void FilteringView::loadFromFile(const std::filesystem::path& path, bool merge) {
    std::ifstream file(path);
    if (!file.good()) {
        throw std::runtime_error(std::format("Could not open '{}'", path.string()));
    }
    const nlohmann::json json = nlohmann::json::parse(file);
    if (json.value("version", 1) > FilterFileVersion) {
        throw std::runtime_error("Filter file is from a newer version");
    }

    // Parse and validate everything before touching the current filtering state
    std::string warnings;
    auto warn = [&warnings](const std::string& message) {
        warnings += (warnings.empty() ? "Warnings: " : "; ") + message;
    };

    std::vector<ColumnFilterEntry> loadedFilters;
    for (const nlohmann::json& entry : json.value("columnFilters", nlohmann::json::array())) {
        const ColumnKey column = entry.at("column").get<std::string>();
        const std::string query = entry.value("query", "");
        if (!_dataViewer.hasColumn(column)) {
            warn(std::format("column '{}' not found", column));
            continue;
        }
        // Type comes from the current dataset, not the file
        const ColumnFilter filter = ColumnFilter(
            query,
            _dataViewer.isNumericColumn(column) ?
                ColumnFilter::Type::Numeric : ColumnFilter::Type::Text
        );
        if (!filter.isValid()) {
            warn(std::format("invalid query '{}' for '{}'", query, column));
            continue;
        }
        loadedFilters.push_back({ column, filter, entry.value("enabled", true) });
    }

    std::vector<std::vector<bool>> quickFlags = _quickFilterFlags;
    if (!merge) {
        for (std::vector<bool>& flags : quickFlags) {
            flags.assign(flags.size(), false);
        }
    }
    for (const nlohmann::json& entry : json.value("quickFilters", nlohmann::json::array())) {
        const std::string groupTitle = entry.value("group", "");
        const std::string name = entry.at("name").get<std::string>();
        bool found = false;
        for (size_t groupId = 0; groupId < _quickFilterGroups.size() && !found; ++groupId) {
            const DataSettings::QuickFilterGroup& group = _quickFilterGroups[groupId];
            if (group.title != groupTitle) {
                continue;
            }
            for (size_t i = 0; i < group.quickFilters.size(); ++i) {
                if (group.quickFilters[i].name == name) {
                    quickFlags[groupId][i] = true;
                    found = true;
                    break;
                }
            }
        }
        if (!found) {
            warn(std::format("quick filter '{}' not found", name));
        }
    }

    const bool hasRowLimit = json.contains("rowLimit");
    bool rowLimitEnabled = false;
    int nRows = _nRows;
    bool useHighest = _useHighestValue;
    ColumnKey rowLimitCol = _rowLimitColumn;
    if (hasRowLimit) {
        const nlohmann::json& r = json.at("rowLimit");
        rowLimitEnabled = r.value("enabled", false);
        nRows = r.value("nRows", nRows);
        useHighest = r.value("useHighest", useHighest);
        rowLimitCol = r.value("column", rowLimitCol);
        if (rowLimitEnabled && !_dataViewer.hasColumn(rowLimitCol)) {
            warn(std::format("row limit column '{}' not found", rowLimitCol));
        }
    }

    // Done last since it adds saved selections; nothing below may throw
    std::optional<size_t> spatialId;
    if (json.contains("spatialFile")) {
        // Only a file name is accepted, so the companion file must sit next to the preset
        const std::filesystem::path spatialFile = path.parent_path() /
            std::filesystem::path(json.at("spatialFile").get<std::string>()).filename();
        const std::vector<size_t> ids =
            _dataViewer.spatialSelectionHandler().loadSelectionsFromFile(spatialFile);
        if (ids.empty()) {
            throw std::runtime_error("Spatial selection file contains no selections");
        }
        spatialId = ids.front();
    }

    if (!merge) {
        _columnFilters.clear();
    }
    for (ColumnFilterEntry& loaded : loadedFilters) {
        const bool isDuplicate = std::any_of(
            _columnFilters.begin(),
            _columnFilters.end(),
            [&loaded](const ColumnFilterEntry& f) {
                return f.column == loaded.column &&
                    f.filter.query() == loaded.filter.query();
            }
        );
        if (!isDuplicate) {
            _columnFilters.push_back(std::move(loaded));
        }
    }
    _quickFilterFlags = std::move(quickFlags);

    if (hasRowLimit && (!merge || rowLimitEnabled)) {
        _limitNumberOfRows = rowLimitEnabled;
        _nRows = nRows;
        _useHighestValue = useHighest;
        _rowLimitColumn = rowLimitCol;
    }

    if (spatialId.has_value()) {
        _useSpatialSelection = true;
        _appliedSavedSpatialSelection = spatialId;
    }
    else if (!merge) {
        _useSpatialSelection = false;
        _appliedSavedSpatialSelection.reset();
    }

    _editingFilterIndex = -1;
    updateActiveFilterCount();

    const std::string savedDataFile = json.value("dataFile", "");
    if (!savedDataFile.empty() &&
        savedDataFile != _dataViewer.currentDataFile().filename().string())
    {
        warn(std::format("saved for dataset '{}'", savedDataFile));
    }

    setStatus(
        std::format("{} '{}'", merge ? "Merged" : "Loaded", path.filename().string()),
        false,
        std::move(warnings)
    );
}

bool FilteringView::tryLoadFromFile(const std::filesystem::path& path, bool merge) {
    try {
        loadFromFile(path, merge);
        return true;
    }
    catch (const std::exception& e) {
        setStatus(std::format("Failed to load '{}': {}", path.string(), e.what()), true);
        return false;
    }
}

bool FilteringView::renderMenuBar() {
    if (!ImGui::BeginMenuBar()) {
        return false;
    }

    bool filterWasChanged = false;
    if (ImGui::BeginMenu("File")) {
        if (ImGui::MenuItem("Save filters...")) {
            _openSavePopup = true;
        }

        ImGui::Separator();

        auto renderFileList = [this, &filterWasChanged](bool merge) {
            const std::vector<std::filesystem::path> files = savedFilterFiles();
            if (files.empty()) {
                ImGui::MenuItem("No saved filters", nullptr, false, false);
            }
            for (const std::filesystem::path& f : files) {
                if (ImGui::MenuItem(f.stem().string().c_str())) {
                    filterWasChanged |= tryLoadFromFile(f, merge);
                }
            }
        };

        if (ImGui::BeginMenu("Load (replace)")) {
            renderFileList(false);
            ImGui::EndMenu();
        }
        if (ImGui::BeginMenu("Load (merge)")) {
            renderFileList(true);
            ImGui::EndMenu();
        }
        if (ImGui::MenuItem("Load from path...")) {
            _openLoadPathPopup = true;
        }
        ImGui::EndMenu();
    }

    ImGui::EndMenuBar();
    return filterWasChanged;
}

bool FilteringView::renderSaveLoadPopups(const SpatialSelectionView& spatialSelectionView)
{
    bool filterWasChanged = false;

    // Popups must be opened outside the menu bar's ID scope
    if (_openSavePopup) {
        ImGui::OpenPopup("Save filters");
        _openSavePopup = false;
    }
    if (_openLoadPathPopup) {
        ImGui::OpenPopup("Load filters");
        _openLoadPathPopup = false;
    }

    const ImGuiWindowFlags flags = ImGuiWindowFlags_AlwaysAutoResize;

    if (ImGui::BeginPopupModal("Save filters", nullptr, flags)) {
        static char name[128] = "";
        static char dir[256] = "";

        ImGui::InputText("Filename (.json)", name, IM_ARRAYSIZE(name));

        const std::string defaultDir = defaultFilterDirectory().string();
        ImGui::SetNextItemWidth(ImGui::CalcTextSize(defaultDir.c_str()).x * 1.3f);
        ImGui::InputTextWithHint("Directory", defaultDir.c_str(), dir, IM_ARRAYSIZE(dir));

        const bool hasName = name[0] != '\0';
        std::filesystem::path filePath =
            std::filesystem::path(dir[0] == '\0' ? defaultDir : dir) / name;
        if (filePath.extension() != ".json") {
            filePath += ".json";
        }
        std::error_code ec;
        const bool exists = hasName && std::filesystem::exists(filePath, ec);

        if (_useSpatialSelection) {
            view::helper::renderDescriptiveText(std::format(
                "The spatial filter is saved to '{}{}'",
                filePath.stem().string(), SpatialFileSuffix
            ).c_str());
        }
        if (exists) {
            ImGui::TextColored(
                ImVec4(1.f, 0.7f, 0.3f, 1.f),
                "A file with this name exists and will be overwritten"
            );
        }

        ImGui::Separator();

        if (ImGui::Button("Cancel") || ImGui::IsKeyPressed(ImGuiKey_Escape)) {
            ImGui::CloseCurrentPopup();
        }
        ImGui::SameLine();
        ImGui::BeginDisabled(!hasName);
        if (ImGui::Button(exists ? "Overwrite" : "Save") ||
            (hasName && ImGui::IsKeyPressed(ImGuiKey_Enter)))
        {
            try {
                saveToFile(filePath, spatialSelectionView);
                name[0] = '\0';
            }
            catch (const std::exception& e) {
                setStatus(std::format("Failed to save filters: {}", e.what()), true);
            }
            ImGui::CloseCurrentPopup();
        }
        ImGui::EndDisabled();
        ImGui::EndPopup();
    }

    if (ImGui::BeginPopupModal("Load filters", nullptr, flags)) {
        static char path[512] = "";
        static int mode = 0;

        ImGui::SetNextItemWidth(400.f);
        ImGui::InputTextWithHint("File path", "path/to/filters.json", path, IM_ARRAYSIZE(path));
        ImGui::RadioButton("Replace current filters", &mode, 0);
        ImGui::SameLine();
        ImGui::RadioButton("Merge with current filters", &mode, 1);

        ImGui::Separator();

        if (ImGui::Button("Cancel") || ImGui::IsKeyPressed(ImGuiKey_Escape)) {
            ImGui::CloseCurrentPopup();
        }
        ImGui::SameLine();
        const bool hasPath = path[0] != '\0';
        ImGui::BeginDisabled(!hasPath);
        if (ImGui::Button("Load") || (hasPath && ImGui::IsKeyPressed(ImGuiKey_Enter))) {
            filterWasChanged |= tryLoadFromFile(std::filesystem::path(path), mode == 1);
            ImGui::CloseCurrentPopup();
        }
        ImGui::EndDisabled();
        ImGui::EndPopup();
    }

    return filterWasChanged;
}

std::vector<size_t> FilteringView::applyFiltering(const std::vector<ExoplanetItem>& data,
                                               const std::vector<int>& externalSelection,
                                               const SpatialSelectionView& spatialSelectionView,
                                               std::vector<size_t>& rowsWithoutExternalSelection)
{
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
            if (!f.enabled || !_dataViewer.hasColumn(f.column)) {
                continue;
            }

            filteredOut |= !f.filter.passFilter(
                _dataViewer.columnValue(f.column, item)
            );
        }

        if (!filteredOut) {
            filteredData.push_back(i);
        }
    }
    filteredData.shrink_to_fit();

    applyRowLimit(data, filteredData);
    rowsWithoutExternalSelection = filteredData;
    applySpatialSelection(data, spatialSelectionView, rowsWithoutExternalSelection);

    applyExternalSelection(externalSelection, filteredData);
    applySpatialSelection(data, spatialSelectionView, filteredData);

    return filteredData;
}

void FilteringView::applyRowLimit(const std::vector<ExoplanetItem>& data,
                                  std::vector<size_t>& filteredData)
{
    if (!_limitNumberOfRows) {
        return;
    }

    const ColumnKey& rowLimitCol = rowLimitColumn();

    if (filteredData.size() > _nRows) {
        auto compare = [&rowLimitCol, &data, this](const size_t& lhs, const size_t& rhs) {
            // We are interested in the largest, so flip the order
            const ExoplanetItem& l = data[_useHighestValue ? rhs : lhs];
            const ExoplanetItem& r = data[_useHighestValue ? lhs : rhs];
            return _dataViewer.compareColumnValues(rowLimitCol, l, r);
        };

        std::sort(filteredData.begin(), filteredData.end(), compare);
        filteredData.erase(filteredData.begin() + _nRows, filteredData.end());
    }
}

void FilteringView::applyExternalSelection(const std::vector<int>& externalSelection,
                                           std::vector<size_t>& prefilteredData)
{
    if (!_useExternalSelection || externalSelection.empty()) {
        return;
    }

    if (_overrideInternalSelection) {
        // Just use the external selection, out of the box
        prefilteredData.clear();
        prefilteredData.reserve(externalSelection.size());
        for (int i : externalSelection) {
            prefilteredData.push_back(static_cast<size_t>(i));
        }
    }
    else {
        // Do an intersection, i.e. check if the filtered out items are in the selection
        std::vector<size_t> newFilteredData;
        newFilteredData.reserve(prefilteredData.size());

        for (size_t index : prefilteredData) {
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
        prefilteredData = std::move(newFilteredData);
    }
}

void FilteringView::applySpatialSelection(
                                      const std::vector<ExoplanetItem>& data,
                                      const SpatialSelectionView& spatialSelectionView,
                                      std::vector<size_t>& prefilteredData) const
{
    if (!_useSpatialSelection) {
        return;
    }

    std::optional<SpatialSelectionQuery> query;

    if (_appliedSavedSpatialSelection.has_value()) {
        if (const SavedSelection* s = appliedSavedSelection(spatialSelectionView)) {
            query = s->query;
        }
    }
    else {
        // Use current spatial query
        query = spatialSelectionView.currentSpatialQuery();
    }

    if (!query.has_value()) {
        return;
    }

    prefilteredData = spatialSelectionView.handler().select(
        data,
        prefilteredData,
        *query
    );
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
        ImGui::Separator();
        ImGui::Text("Filter on column");
        ImGui::SetNextItemWidth(120);
        if (ImGui::BeginCombo("##Column", _dataViewer.columnName(_newFilterColumn))) {
            static ImGuiTextFilter columnFilter;
            if (ImGui::IsWindowAppearing()) {
                ImGui::SetKeyboardFocusHere();
                columnFilter.Clear();
            }
            columnFilter.Draw("##Filter");

            for (size_t i = 0; i < _dataViewer.columns().size(); ++i) {
                const char* name = _dataViewer.columnName(i);

                if (!columnFilter.PassFilter(name)) {
                    continue;
                }

                const ColumnKey& column = _dataViewer.columns()[i];
                if (ImGui::Selectable(name, _newFilterColumn == column)) {
                    _newFilterColumn = column;
                }

                _dataViewer.renderColumnDescriptionTooltip(i);
            }
            ImGui::EndCombo();
        }

        ImGui::SameLine();

        static char queryString[128] = "";

        bool numeric = _dataViewer.isNumericColumn(_newFilterColumn);

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
                _columnFilters.push_back({ _newFilterColumn, filter });
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
            constexpr const int nColumns = 6;

            const ImGuiTableFlags flags = ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_RowBg;

            static char editingColumnText[128] = "";

            if (ImGui::BeginTable("filtersTable", nColumns, flags)) {
                for (int i = 0; i < _columnFilters.size(); ++i) {
                    ColumnFilterEntry& f = _columnFilters[i];
                    if (!_dataViewer.hasColumn(f.column)) {
                        continue;
                    }
                    const std::string queryString = f.filter.query();
                    bool isEditingColumn = (_editingFilterIndex == i);
                    ImGui::TableNextRow();

                    ImGui::PushID(std::format("FilterColEnabled-{}", i).c_str());
                    ImGui::TableNextColumn();
                    if (ImGui::Checkbox("##Enabled", &f.enabled)) {
                        filterWasChanged = true;
                    }
                    ImGui::PopID();

                    ImGui::TableNextColumn();
                    ImGui::Text(_dataViewer.columnName(f.column));
                    _dataViewer.renderColumnDescriptionTooltip(
                        _dataViewer.columnIndex(f.column)
                    );

                    ImGui::TableNextColumn();
                    ImGui::Text("    ");

                    ImGui::TableNextColumn();
                    if (isEditingColumn) {
                        ImGui::SetNextItemWidth(100);
                        bool wasChanged = ImGui::InputText(
                            "##EditQuery",
                            editingColumnText,
                            IM_ARRAYSIZE(editingColumnText),
                            ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_AutoSelectAll
                        );
                        if (wasChanged) {
                            ColumnFilter filter = f.filter.isNumeric() ?
                                ColumnFilter(editingColumnText, ColumnFilter::Type::Numeric) :
                                ColumnFilter(editingColumnText, ColumnFilter::Type::Text);

                            if (filter.isValid()) {
                                _editingFilterIndex = -1;
                                f.filter = filter;
                                filterWasChanged = true;
                            }
                        }
                    }
                    else {
                        ImGui::Text(queryString.empty() ? "has value" : queryString.c_str());
                    }

                    ImGui::TableNextColumn();
                    ImGui::PushID(std::format("EditFilter_{}", i).c_str());
                    if (!isEditingColumn) {
                        if (ImGui::SmallButton("Edit")) {
                            _editingFilterIndex = i;
                            strcpy(editingColumnText, queryString.data());
                        }
                    }
                    else {
                        if (ImGui::SmallButton("Abort Edit")) {
                            _editingFilterIndex = -1;
                        }
                    }
                    ImGui::PopID();


                    ImGui::TableNextColumn();
                    ImGui::PushID(std::format("DeleteFilter_{}", i).c_str());
                    if (ImGui::SmallButton("Delete")) {
                        indexToErase = i;
                    }
                    ImGui::PopID();
                }

                if (indexToErase != -1) {
                    _columnFilters.erase(_columnFilters.begin() + indexToErase);
                    _editingFilterIndex = -1;
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

                bool shouldAddSameLine = group.itemsPerLine != -1 && ((i + 1) % group.itemsPerLine != 0);
                bool isLastInGroup = (i == group.quickFilters.size() - 1);
                if (shouldAddSameLine && !isLastInGroup) {
                    ImGui::SameLine();
                }
            }
        }
    }

    ImGui::Spacing();

    if (filterWasChanged) {
        updateActiveFilterCount();
    }

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
    int highOrLowIndex = _useHighestValue ? 0 : 1;

    ImGui::SetNextItemWidth(80);
    bool highLowChanged = ImGui::Combo(
        "##HighOrLowCombo",
        &highOrLowIndex,
        highOrLowChoices,
        IM_ARRAYSIZE(highOrLowChoices)
    );
    if (highLowChanged) {
        _useHighestValue = highOrLowIndex == 0;
        filterWasChanged = true;
    }

    ImGui::SameLine();

    const char* columnName = _dataViewer.columnName(rowLimitColumn());
    ImGui::SetNextItemWidth(100);
    if (ImGui::BeginCombo("##RowLimitColumn", columnName)) {
        static ImGuiTextFilter columnFilter;
        if (ImGui::IsWindowAppearing()) {
            ImGui::SetKeyboardFocusHere();
            columnFilter.Clear();
        }

        columnFilter.Draw("##Filter");
        for (size_t i = 0; i < _dataViewer.columns().size(); ++i) {
            // Ignore non-numeric columns
            if (!_dataViewer.isNumericColumn(i)) {
                continue;
            }

            const char* name = _dataViewer.columnName(i);
            if (!columnFilter.PassFilter(name)) {
                continue;
            }

            const ColumnKey& column = _dataViewer.columns()[i];
            if (ImGui::Selectable(name, _rowLimitColumn == column)) {
                _rowLimitColumn = column;
                filterWasChanged = true;
            }
        }
        ImGui::EndCombo();
    }

    ImGui::Spacing();

    return filterWasChanged;
}

bool FilteringView::renderSpatialFilterSettings(
                                      const SpatialSelectionView& spatialSelectionView)
{
    bool filterWasChanged = false;

    const bool headerIsOpen = ImGui::CollapsingHeader("Spatial filters");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Filter the current results using the live spatial query or one saved spatial "
        "query. The query is evaluated against this dataset and intersects the other "
        "filters."
    );

    if (!headerIsOpen) {
        return filterWasChanged;
    }

    filterWasChanged |= ImGui::Checkbox(
        "Use spatial selection",
        &_useSpatialSelection
    );

    if (!_useSpatialSelection) {
        ImGui::BeginDisabled();
    }

    const std::vector<SavedSelection>& saved =
        spatialSelectionView.handler().savedSelections();

    const std::string sourceDescription = spatialFilterDescription(spatialSelectionView);

    ImGui::SetNextItemWidth(-1.f);
    if (ImGui::BeginCombo("##SpatialFilterSource", sourceDescription.c_str())) {
        const bool currentIsSelected =
            appliedSavedSelection(spatialSelectionView) == nullptr;
        if (ImGui::Selectable("Current spatial query", currentIsSelected)) {
            _appliedSavedSpatialSelection.reset();
            filterWasChanged = true;
        }

        for (const SavedSelection& selection : saved) {
            const bool isSelected = _appliedSavedSpatialSelection == selection.id;
            if (ImGui::Selectable(selection.name.c_str(), isSelected)) {
                _appliedSavedSpatialSelection = selection.id;
                filterWasChanged = true;
            }
        }
        ImGui::EndCombo();
    }

    if (!_useSpatialSelection) {
        ImGui::EndDisabled();
    }

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

    ImGui::Text("External Selection: ");
    size_t nItems = _dataViewer.externalSelectionSize();
    if (nItems > 0) {
        ImGui::SameLine();
        view::helper::renderDescriptiveText(
            std::format("{} items", nItems).c_str()
        );

        ImGui::SameLine(ImGui::GetWindowContentRegionMax().x - 100);
        if (ImGui::Button("Delete", ImVec2(100, 0))) {
            _dataViewer.clearExternalSelection();
            filterWasChanged = true;
        }
    }

    ImGui::Text("Last updated: ");
    ImGui::SameLine();
    view::helper::renderDescriptiveText(
        _dataViewer.lastExternalSelectionTimestamp().c_str()
    );

    ImGui::Spacing();

    return filterWasChanged;
}

} // namespace openspace::exoplanets
