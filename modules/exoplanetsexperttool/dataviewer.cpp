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

#include <modules/exoplanetsexperttool/dataviewer.h>

#include <modules/exoplanetsexperttool/columnfilter.h>
#include <modules/exoplanetsexperttool/datahelper.h>
#include <modules/exoplanetsexperttool/dataloader.h>
#include <modules/exoplanetsexperttool/exoplanetsexperttoolmodule.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <fstream>
#include <iostream>
#include <string_view>

#include <chrono>

#ifdef WIN32
#include <Windows.h>
#else // WIN32
#include <iomanip>
#include <sstream>
#include <sys/time.h>
#endif // WIN32


#include <implot.h>

#define SHOW_IMGUI_HELPERS

namespace {
    constexpr std::string_view _loggerCat = "ExoplanetsDataViewer";

    constexpr std::string_view WebpagePath = "${MODULE_EXOPLANETSEXPERTTOOL}/webpage/index.html";

    constexpr std::string_view AboutTheTool =
        "This is a research tool under development and we are currently \n"
        "looking for feedback from users. This feedback will be included \n"
        "in our scientific publication covering the tool. \n"
        "\n"
        "Thank you for taking the time to trying it out, and please do not \n"
        "hesitate to reach out with any questions, input or feedback";

    constexpr char GetInTouchLink[] =
        "https://data.openspaceproject.com/release/ExoplanetExplorer/misc/get_in_touch";

    bool hasTag(const openspace::SceneGraphNode* node, std::string_view tag) {
        if (!node) {
            return false;
        }
        const std::vector<std::string>& tags = node->tags();

        return std::find(tags.begin(), tags.end(), tag) != std::end(tags);
    };

    // Stolen from Log.cpp. // TODO: move to some util file in the module
    std::string timeString() {
#ifdef WIN32
        SYSTEMTIME t = {};
        GetLocalTime(&t);

        return std::format(
            "{:0>2}:{:0>2}:{:0>2}.{:0<3}", t.wHour, t.wMinute, t.wSecond, t.wMilliseconds
        );
#else
        struct timeval t;
        gettimeofday(&t, nullptr);
        tm* m = gmtime(&t.tv_sec);

        return std::format(
            "{:0>2}:{:0>2}:{:0>2}.{:0<3}", m->tm_hour, m->tm_min, m->tm_sec, t.tv_usec / 1000
        );
#endif
    }

    constexpr const openspace::Property::PropertyInfo ExternalSelectionInfo = {
        "ExternalSelection",
        "External Selection from Webpage",
        "Contains the indices of the rows in the data file that should be included, "
        "based on the filtering on the external webpage.",
        openspace::Property::Visibility::Hidden
    };
}

namespace openspace::exoplanets {

DataViewer::DataViewer(std::string identifier, std::string guiName)
    : PropertyOwner({ std::move(identifier), std::move(guiName) })
    , _externalSelection(ExternalSelectionInfo)
    , _glyphHandler(*this)
{
    _externalSelection.setReadOnly(true);
    addProperty(_externalSelection);

    _externalSelection.onChange([this]() {
        if (_externalSelection.value().empty()) {
            // Selection was cleared. Clear timestamp
            _lastExternalSelectionTimeStamp = "";
        }
        else {
            LINFO("Updated selection from webpage");
            _lastExternalSelectionTimeStamp = timeString();
        }
        _externalSelectionChanged = true;
    });

    // Interaction callbacks. OBS! A bit ugly to handle this separately from ImGui io....
    global::callback::keyboard->emplace_back(
        [&](Key key, KeyModifier, KeyAction action, bool) -> bool {
            bool isCtrl = key == Key::LeftControl;
            if (isCtrl && action == KeyAction::Press) {
                _holdingCtrl = true;
            }
            else if (isCtrl && action == KeyAction::Release) {
                _holdingCtrl = false;
            }
            // Do not capture
            return false;
        }
    );
}

void DataViewer::initializeData() {
    LDEBUG("Initializing dataset from files specified in module");

    // Load things related to the dataset. We need to do this on initialize rather than
    // construction since we need the module to exist first (to access its settings)
    initializeData(DataLoader::loadDataSettingsFromJson());
}

bool DataViewer::loadCsvFile(std::filesystem::path path) {
    DataSettings settings = _dataSettings;
    settings.dataFile = std::move(path);
    return initializeData(std::move(settings));
}

bool DataViewer::initializeData(DataSettings settings) {
    LINFO("Loading data from file: " + settings.dataFile.string());

    std::vector<ExoplanetItem> data;
    try {
        data = DataLoader::loadData(settings);
    }
    catch (const std::exception& e) {
        LERROR(std::format("Failed to load data: {}", e.what()));
        return false;
    }

    if (data.empty()) {
        LERROR("No data was loaded!");
        return false;
    }

    _dataSettings = std::move(settings);
    _data = std::move(data);
    _computedColumns.clear();
    _hostIdToPlanetsMap.clear();
    _meanColumnValues.clear();
    _selection.clear();
    _externalSelection = {};

    // Initialize filtered data index list and map of host star to planet indices
    _filteredData.clear();
    _filteredData.reserve(_data.size());
    for (size_t i = 0; i < _data.size(); i++) {
        _filteredData.push_back(i);

        if (!_dataSettings.dataMapping.hostName.empty()) {
            _hostIdToPlanetsMap[makeIdentifier(_data[i].hostName)].push_back(i);
        }
    }

    _columns = _columnSelectionView.initializeColumnsFromData(_data, _dataSettings);

    // The other views use the loaded data, so call this afterwards
    _colorMappingView = std::make_unique<ColorMappingView>(*this, _dataSettings);
    _filteringView = std::make_unique<FilteringView>(*this, _dataSettings);
    _systemViewer = std::make_unique<SystemViewer>(*this);
    _tableView = std::make_unique<TableView>(
        *this,
        _columnSelectionView.orderedSelectedColumns()
    );
    _computeColumnsView = std::make_unique<ComputeColumnsView>(*this, _dataSettings);

    _currentlyTargettedSystem = std::nullopt;

    LDEBUG("Finished initializing based on dataset");

    // Compute mean values
    for (size_t i = 0; i < _columns.size(); i++) {
        if (!isNumericColumn(i)) {
            continue;
        }
        computeMeanForColumn(_columns[i]);
    }

    _colormapWasChanged = true;
    _filterChanged = true;
    _selectionChanged = true;
    return true;
}

void DataViewer::initializeGL() {
    _glyphHandler.initializeRenderables();
    initializeCallbacks();

    _colorMappingView->initializeGL();
}

std::filesystem::path DataViewer::currentDataFile() const {
    return _dataSettings.dataFile;
}

std::filesystem::path DataViewer::computedColumnHistoryFile() const {
    auto module = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    const std::filesystem::path settingsFile = absPath(module->dataConfigFile());
    return settingsFile.parent_path() / "computed_columns_history.json";
}

std::variant<const char*, float> DataViewer::columnValue(const ColumnKey& key,
                                                         const ExoplanetItem& item) const
{
    if (auto it = _computedColumns.find(key); it != _computedColumns.end()) {
        return it->second.values.at(static_cast<size_t>(item.id));
    }

    const std::variant<std::string, float>& value = item.dataColumns.at(key);

    if (std::holds_alternative<std::string>(value)) {
        return std::get<std::string>(value).c_str();
    }
    return std::get<float>(value);
}

std::variant<const char*, float> DataViewer::columnValue(const ColumnKey& key,
                                                         size_t rowIndex) const
{
    if (auto it = _computedColumns.find(key); it != _computedColumns.end()) {
        return it->second.values.at(rowIndex);
    }
    return columnValue(key, _data.at(rowIndex));
}

bool DataViewer::isNumericColumn(size_t index) const {
    return  isNumericColumn(_columns[index]);
}

bool DataViewer::isNumericColumn(const ColumnKey& key) const {
    if (_computedColumns.contains(key)) {
        return true;
    }
    ghoul_assert(_data.size() > 0, "Data size cannot be zero");
    // Test type using the first data point
    std::variant<const char*, float> aValue = columnValue(key, _data.front());
    return std::holds_alternative<float>(aValue);
}

bool DataViewer::hasColumn(const ColumnKey& key) const {
    return std::find(_columns.begin(), _columns.end(), key) != _columns.end();
}

size_t DataViewer::columnIndex(const ColumnKey& key) const {
    for (size_t i = 0; i < _columns.size(); ++i) {
        if (_columns[i] == key) {
            return i;
        }
    }
    LWARNING(std::format(
        "Tried to get index of non-selected column: '{}'", key
    ));
    return 0;
}

const char* DataViewer::columnName(const ColumnKey& key) const {
    if (_computedColumns.contains(key)) {
        return key.c_str();
    }
    return _dataSettings.columnName(key);
}

const char* DataViewer::columnName(size_t columnIndex) const {
    // TODO: validate index
    return columnName(_columns[columnIndex]);
}

bool DataViewer::isNameColumn(const ColumnKey& key) const {
    return key == _dataSettings.nameColumn();
}

std::optional<float> DataViewer::meanValue(const ColumnKey& key) const {
    if (_meanColumnValues.contains(key)) {
        return _meanColumnValues.at(key);
    }
    return std::nullopt;
}

bool DataViewer::hasColumnDescription(const ColumnKey& key) const {
    return _dataSettings.hasDescription(key);
}

const char* DataViewer::columnDescription(const ColumnKey& key) const {
    ghoul_assert(hasColumnDescription(key), "Must have a description");
    return _dataSettings.description(key).c_str();
}

const std::vector<ExoplanetItem>& DataViewer::data() const {
    return _data;
}

const std::vector<size_t>& DataViewer::currentFiltering() const {
    return _filteredData;
}

const std::vector<ColumnKey>& DataViewer::columns() const {
    return _columns;
}

const std::map<ColumnKey, ComputedColumn>& DataViewer::computedColumns() const {
    return _computedColumns;
}

bool DataViewer::addComputedColumn(ColumnKey key, std::string expression,
                                   std::vector<float> values)
{
    if (key.empty() || values.size() != _data.size() ||
        _computedColumns.contains(key) ||
        std::find(_columns.begin(), _columns.end(), key) != _columns.end())
    {
        return false;
    }

    _columns.push_back(std::move(key));
    _computedColumns.emplace(
        _columns.back(),
        ComputedColumn{ std::move(expression), std::move(values) }
    );
    computeMeanForColumn(_columns.back());
    _columnSelectionView.updateComputedColumns(_computedColumns);
    _tableView->updateColumns(_columnSelectionView.orderedSelectedColumns());
    _filterChanged = true;
    return true;
}

bool DataViewer::removeComputedColumn(const ColumnKey& key) {
    if (!_computedColumns.erase(key)) {
        return false;
    }

    std::erase(_columns, key);
    _meanColumnValues.erase(key);
    _columnSelectionView.updateComputedColumns(_computedColumns);
    _tableView->updateColumns(_columnSelectionView.orderedSelectedColumns());
    _colormapWasChanged = true;
    _filterChanged = true;
    return true;
}

const DataSettings::DataMapping& DataViewer::dataMapping() const {
    return _dataSettings.dataMapping;
}

const DataSettings& DataViewer::dataSettings() const {
    return _dataSettings;
}

bool DataViewer::filterChanged() const {
    return _filterChanged;
}

ColorMappingView* DataViewer::colorMappingView() {
    return _colorMappingView.get();
}

SystemViewer* DataViewer::systemViewer() {
    return _systemViewer.get();
}

TableView* DataViewer::tableView() {
    return _tableView.get();
}

std::vector<size_t> DataViewer::planetsForHost(const std::string& hostIdentifier) const {
    if (!_hostIdToPlanetsMap.contains(hostIdentifier)) {
        return {};
    }
    return _hostIdToPlanetsMap.at(hostIdentifier);
}

size_t DataViewer::externalSelectionSize() const {
    return _externalSelection.value().size();
}

const std::string& DataViewer::lastExternalSelectionTimestamp() const {
    return _lastExternalSelectionTimeStamp;
}

void DataViewer::clearExternalSelection() {
    // TODO: This should be done though Lua to be synced across all nodes
    _externalSelection = {};
    LINFO("Cleared external selection");
};

void DataViewer::setSelection(const std::vector<size_t>& indices) {
    _selection = indices;
    _selectionChanged = true;
}

bool DataViewer::compareColumnValues(const ColumnKey& key, const ExoplanetItem& left,
                                     const ExoplanetItem& right) const
{
    std::variant<const char*, float> leftValue = columnValue(key, left);
    std::variant<const char*, float> rightValue = columnValue(key, right);

    // TODO: make sure they are the same type

    if (std::holds_alternative<const char*>(leftValue) &&
        std::holds_alternative<const char*>(rightValue))
    {
        return !data::caseInsensitiveLessThan(
            std::get<const char*>(leftValue),
            std::get<const char*>(rightValue)
        );
    }
    else if (std::holds_alternative<float>(leftValue) &&
        std::holds_alternative<float>(rightValue))
    {
        return data::compareValuesWithNan(std::get<float>(leftValue), std::get<float>(rightValue));
    }
    else {
        LERROR("Trying to compare mismatching column types");
        return false;
    }
}

void DataViewer::renderStartupInfo() {
    // Always center this window when appearing
    ImVec2 center = ImGui::GetMainViewport()->GetCenter();
    ImGui::SetNextWindowPos(center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));

    ImGuiWindowFlags flags = ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_HorizontalScrollbar;

    ImGui::OpenPopup("We need your help!");
    if (ImGui::BeginPopupModal("We need your help!", NULL, flags)) {
        ImGui::Text("Welcome to the Exoplanet Explorer");
        ImGui::Spacing();
        ImGui::Text(AboutTheTool.data());
        ImGui::Spacing();

        if (ImGui::Button("Get in touch!")) {
            system(std::format("start {}", GetInTouchLink).c_str());
        }
        ImGui::SameLine();
        ImGui::TextDisabled("(opens a webpage in your browser)");
        ImGui::Spacing();
        ImGui::Spacing();

        ImGui::Separator();

        // Ok
        if (ImGui::Button("Continue", ImVec2(120, 0)) ||
            ImGui::IsKeyPressed(ImGuiKey_Enter))
        {
            ImGui::CloseCurrentPopup();
            _shouldOpenInfoWindow = false;
        }
        ImGui::SetItemDefaultFocus();

        ImGui::EndPopup();
    }
}

void DataViewer::initializeCallbacks() {
    Property* anchorProperty =
        global::navigationHandler->orbitalNavigator().property("Anchor");

    if (!anchorProperty) {
        return;
    }

    anchorProperty->onChange([this]() {
        const SceneGraphNode* node =
            global::navigationHandler->orbitalNavigator().anchorNode();

        std::optional<std::string> system = std::nullopt;

        if (hasTag(node, "exoplanet_system")) {
            // Target is an expolanet system => show top menu
            system = node->identifier();
        }
        else if (hasTag(node, "exoplanet_planet")) {
            // The system is the parent
            system = node->parent()->identifier();
        }

        // The system variable contains the identifier of the currently targetted system.
        // To get more matches with the host star names, undo the whitespace to
        // underscore transformation in the identifier
        if (system.has_value()) {
            std::replace((*system).begin(), (*system).end(), '_', ' ');
        }

        _currentlyTargettedSystem = system;
    });
}

void DataViewer::render() {
    static bool showHelpers = false;

    auto mod = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    if (mod->showInfoWindowAtStartup() && _shouldOpenInfoWindow) {
        renderStartupInfo();
        return;
    }

    // Tooltip for hovered planets. Only do the (potentially expensive, since it may
    // trigger GPU picking) hover computation when the mouse is not currently captured
    // by an ImGui widget/window. This avoids doing unnecessary picking work every
    // frame, which was especially noticeable (and costly) while holding CTRL, since
    // that is when the picking-based hovering is actually enabled in the renderables.
    ImGuiIO& mainIo = ImGui::GetIO();
    int hoveredPlanet = mainIo.WantCaptureMouse ? -1 : _glyphHandler.getHoveredPlanetIndex();
    renderPlanetTooltip(hoveredPlanet);
    handleDoubleClickHoveredPlanet(hoveredPlanet);

    // Add vertical spacing before the menus, to account for main OpenSpace UI
    float offset = 20.f; //px
    ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0.f, offset));

    if (ImGui::BeginMainMenuBar()) {
        ImGui::PopStyleVar(); // We don't want the padding to affect the other menus below, so pop here

        ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 0.8f * offset);
        ImGui::TextDisabled("ExoplanetExplorer:");

        renderFileMenu();

        if (ImGui::BeginMenu("Views")) {
            ImGui::MenuItem("Table", NULL, &_showTable);
            ImGui::MenuItem("Filters", NULL, &_showFilterSettingsWindow);
            ImGui::MenuItem("Color mapping", NULL, &_showColormapWindow);
            ImGui::MenuItem("Compute data columns", NULL, &_showComputeColumnsWindow);
            if (mod->showInfoWindowAtStartup()) {
                ImGui::Separator();
                ImGui::MenuItem("Start-up info", NULL, &_shouldOpenInfoWindow);
            }

#ifdef SHOW_IMGUI_HELPERS
            ImGui::MenuItem("ImGui Helpers", NULL, &showHelpers);
#endif
            ImGui::EndMenu();
        }

        renderSettingsMenu();

        if (ImGui::BeginMenu("Navigation")) {
            if (ImGui::Button("Refocus on Earth")) {
                refocusView();
            }
            ImGui::SameLine();
            view::helper::renderHelpMarker(
                "Reset the camera to focus on Earth. Useful for example when you have "
                "focused on another planet system, or just moved the camera around."
            );
            if (ImGui::Button("Zoom to overview")) {
                flyToOverview();
            }
            ImGui::SameLine();
            view::helper::renderHelpMarker(
                "Fly to an overview of the exoplanets. This means viewing the planets "
                "from the ouside in, from a position far out in our galaxy"
            );
            if (ImGui::Button("Zoom to inside view")) {
                flyToInsideView();
            }
            ImGui::SameLine();
            view::helper::renderHelpMarker(
                "Fly to a view close to our solar system. The planets will be placed "
                "on their position on the night sky"
            );

            ImGui::Text("Tips for manual navigation");
            ImGui::SameLine();
            view::helper::renderHelpMarker(
                "Hold CTRL while rotating to change where the camera is focusing. "
                "Reset using the \"Refocus on Earth\" button. \n"
                "\n"
                "You can also rotate the view using the middle mouse button. Give it a try!"
            );

            ImGui::EndMenu();
        }

        if (ImGui::BeginMenu("Webpage")) {
            const std::string path = absPath(WebpagePath).string();

            ImGui::Text(
                "Open an interactive webpage for further interaction and \n"
                "visualization of the dataset. \n \n"
                "The webpage can be used to control the selection, as well \n"
                "as visualizing the current planet selection. Click a button \n"
                "to open in your selected browser."
            );

            if (ImGui::Button("Open in Chrome (Windows)")) {
                system(std::format("start chrome.exe {}", path).c_str());
            }
            if (ImGui::Button("Open in Firefox (Windows)")) {
                system(std::format("start firefox {}", path).c_str());
            }

            ImGui::EndMenu();
        }

        if (_currentlyTargettedSystem.has_value()) {
            std::string system = (*_currentlyTargettedSystem);
            if (ImGui::BeginMenu(std::format("System: {}", system.c_str()).c_str())) {
                _systemViewer->showSystemView(system);
                ImGui::EndMenu();
            }
        }

        ImGui::SameLine(0.f, 30.f);
        ImGui::TextDisabled(std::format(
            "Showing {} / {} planets", _filteredData.size(), _data.size()
        ).c_str());

        ImGui::SameLine(0.f, 30.f);

        // Filters info
        {
            std::string label = std::format(
                "Filters: {}", _filteringView->activeFilters()
            );
            ImVec2 text_size = ImGui::CalcTextSize(label.c_str());
            if (ImGui::Selectable(label.c_str(), false, ImGuiSelectableFlags_None, text_size)) {
                _showFilterSettingsWindow = true;
            }

            if (ImGui::IsItemHovered()) {
                ImGui::BeginTooltip();
                view::helper::renderDescriptiveText(("Click to open filters view"));
                ImGui::Separator();
                _filteringView->renderAppliedColumnFilters();
                ImGui::EndTooltip();
            }

            if (_filteringView->isUsingRowFiltering()) {
                ImGui::SameLine();
                ImGui::TextColored(ImVec4(0.f, 1.f, 1.f, 1.f), "+ row limit");
                if (ImGui::IsItemHovered()) {
                    ImGui::SetTooltip(_filteringView->rowLimitDescription().c_str());
                }
            }

            if (_externalSelection.value().size() > 0 && _filteringView->isUsingExternalFiltering()) {
                ImGui::SameLine();
                ImGui::TextColored(ImVec4(1.f, 0.3f, 1.f, 1.f), "+ external"); // TODO: add timestamp here?
                if (ImGui::IsItemHovered()) {
                    ImGui::BeginTooltip();
                    ImGui::TextUnformatted(std::format(
                        "External selection: {} planets", _externalSelection.value().size()
                    ).c_str());
                    view::helper::renderDescriptiveText(std::format(
                        "Last updated {}", _lastExternalSelectionTimeStamp
                    ).c_str());

                    ImGui::EndTooltip();
                }
            }
        }

        // Coloring (only show first column)
        {
            ImGui::SameLine(0, 30);

            float totalWidth = 0.f;

            const std::vector<ColorMappingView::ColorMappedVariable>& cmappedVariables =
                _colorMappingView->colorMapperVariables();

            const ColorMappingView::ColorMappedVariable& firstCmap = cmappedVariables.front();

            const char* column = columnName(firstCmap.column);
            totalWidth += ImGui::CalcTextSize(column).x;

            std::string min = std::format("{:.2f}", firstCmap.colorScaleMin);
            std::string max = std::format("{:.2f}", firstCmap.colorScaleMax);

            totalWidth += ImGui::CalcTextSize(min.c_str()).x;
            totalWidth += ImGui::CalcTextSize(max.c_str()).x;
            totalWidth += 2.f * ImGui::GetStyle().ItemSpacing.x;

            int cmap = ImPlot::GetColormapIndex(
                _colorMappingView->colormapFromIndex(firstCmap.colormapIndex)
            );
            float startY = ImGui::GetCursorPosY();               // already correctly offset for the bar
            float buttonHeight = ImGui::GetFrameHeight();
            ImVec2 buttonPadding = ImGui::GetStyle().FramePadding;
            totalWidth += buttonPadding.x;

            const char* label = "Color:";
            totalWidth += ImGui::CalcTextSize(label).x;

            // Some extra padding to the right;
            totalWidth += ImGui::GetStyle().ItemSpacing.x * 2.f;

            std::string logScale = firstCmap.useLogScale ? " (log)" : "";
            if (!logScale.empty()) {
                totalWidth += ImGui::CalcTextSize(logScale.c_str()).x;
                totalWidth += ImGui::GetStyle().ItemSpacing.x;
            }

            std::string cmapCount;
            if (cmappedVariables.size() > 1) {
                cmapCount = std::format(" 1/{}", cmappedVariables.size());
                totalWidth += ImGui::CalcTextSize(cmapCount.c_str()).x;
                totalWidth += ImGui::GetStyle().ItemSpacing.x;
            }

            // Right-align icon + selectable in current content region
            const float currentX = ImGui::GetCursorPosX();
            const float rightAlignedX = currentX + ImGui::GetContentRegionAvail().x - totalWidth;
            if (rightAlignedX > currentX) {
                ImGui::SetCursorPosX(rightAlignedX);
            }

            ImVec2 text_size = ImGui::CalcTextSize(label);
            if (ImGui::Selectable(label, false, ImGuiSelectableFlags_None, text_size)) {
                _showColormapWindow = true;
            }
            if (ImGui::IsItemHovered()) {
                ImGui::BeginTooltip();
                view::helper::renderDescriptiveText(("Click to open color mapping view"));
                ImGui::EndTooltip();
            }

            ImGui::SameLine();
            ImGui::TextUnformatted(min.c_str());
            ImGui::SameLine();

            ImGui::SetCursorPosY(startY + 0.5f * buttonHeight + 2.f * buttonPadding.y);
            if (ImPlot::ColormapButton(column, ImVec2(0, buttonHeight), cmap)) {
                _showColormapOverviewWindow = !_showColormapOverviewWindow;
            }

            ImGui::SameLine();
            ImGui::TextUnformatted(max.c_str());

            if (!logScale.empty()) {
                ImGui::SameLine();
                view::helper::renderDescriptiveText(logScale.c_str());

                if (ImGui::IsItemHovered()) {
                    ImGui::SetTooltip("Using logarithmic scale");
                }
            }

            if (!cmapCount.empty()) {
                ImGui::SameLine();
                view::helper::renderDescriptiveText(cmapCount.c_str());

                if (ImGui::IsItemHovered()) {
                    ImGui::SetTooltip(std::format(
                        "First of {} color mappings", cmappedVariables.size()
                    ).c_str());
                }
            }
        }

        ImGui::EndMainMenuBar();
    }

    _filterChanged = false;

    // Windows


    if (_showFilterSettingsWindow) {
        _filterChanged = _filteringView->render(&_showFilterSettingsWindow);
    }

    _filterChanged = _filterChanged || _externalSelectionChanged;

    // Update the filtered data right away
    if (_filterChanged) {
        _filteredData = _filteringView->applyFiltering(
            _data,
            _externalSelection.value()
        );

        updateFilteredRowsProperty();

        // Clear selection
        _selection.clear();
        _selectionChanged = true;
    }

    _externalSelectionChanged = false;

    if (_showColormapWindow) {
        _colormapWasChanged = _colorMappingView->render(&_showColormapWindow);
    }

    if (_showColormapOverviewWindow) {
        renderColormapOverviewWindow(&_showColormapOverviewWindow);
    }

    if (_showTable) {
        _tableView->render(&_showTable, _filteredData);
    }

    if (_showComputeColumnsWindow) {
        _computeColumnsView->render(&_showComputeColumnsWindow);
    }

    _systemViewer->renderAllSystemViews();

#ifdef SHOW_IMGUI_HELPERS
    if (showHelpers) {
        ImGui::Begin("Style Editor");
        ImGui::ShowStyleEditor();
        ImGui::End();

        ImGui::ShowDemoWindow();
        ImGui::ShowMetricsWindow();
        ImPlot::ShowDemoWindow();
    }
#endif

    // Update linked views, if needed

    if (_filterChanged || _colormapWasChanged) {
        updateGlyphRenderData();
        _colormapWasChanged = false;
        _filterChanged = false;
    }

    if (_selectionChanged) {
        _glyphHandler.updateSelectionInRenderable(_selection);
        _selectionChanged = false;
    }
}

void DataViewer::renderColormapOverviewWindow(bool* open) {
    const ImGuiViewport* viewport = ImGui::GetMainViewport();
    const float posPadding = 10.f;
    ImGui::SetNextWindowPos(
        ImVec2(viewport->WorkPos.x + viewport->WorkSize.x - posPadding, viewport->WorkPos.y + posPadding),
        ImGuiCond_Always,
        ImVec2(1.f, 0.f)
    );

    if (ImGui::Begin("Colors", open, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoCollapse)) {
        _colorMappingView->renderActiveColormapOverview();
        ImGui::Spacing();
        if (ImGui::Button("Edit")) {
            _showColormapWindow = true;
        }
    }
    ImGui::End();
}

void DataViewer::renderColumnDescriptionTooltip(size_t index) const {
    const ColumnKey& key = _columns[index];
    if (hasColumnDescription(key)) {
        ImGui::SameLine();
        view::helper::renderHelpMarker(columnDescription(key));
    }
}

void DataViewer::renderColumnValue(const ColumnKey& key, const ExoplanetItem& item) const {
    std::optional<const char*> format;

    if (_dataSettings.columnInfo.contains(key)) {
        const DataSettings::ColumnInfo& colInfo =
            _dataSettings.columnInfo.at(key);

        if (!colInfo.format.empty()) {
            format = colInfo.format.c_str();
        }
    }

    std::variant<const char*, float> value = columnValue(key, item);

    if (std::holds_alternative<float>(value)) {
        float v = std::get<float>(value);
        if (std::isnan(v)) {
            ImGui::TextUnformatted("");
        }
        else {
            ImGui::Text(format.value_or("%.2f"), v);
        }
    }
    else if (std::holds_alternative<const char*>(value)) {
        ImGui::Text("%s", std::get<const char*>(value));
    }
}

void DataViewer::renderPlanetTooltip(int index) const {
    if (index < 0) {
        return; // no planet hovered
    }

    // Show tooltip window on mouse position
    ImVec2 pos = ImGui::GetIO().MousePos;
    ImGui::SetNextWindowPos(pos, ImGuiCond_Appearing, ImVec2(-0.01f, 1.f));
    ImGui::SetNextWindowBgAlpha(0.35f); // Transparent background
    ImGuiWindowFlags flags = ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoDecoration |
        ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_NoFocusOnAppearing;

    const ExoplanetItem& item = _data[index];

    if (ImGui::Begin("##planetToolTip", NULL, flags)) {
        ImGui::Text(item.name.c_str());

        ImGui::Separator();

        // Render the colormapped values
        using Cmap = ColorMappingView::ColorMappedVariable;
        for (const Cmap& cmap : _colorMappingView->colorMapperVariables()) {
            const ColumnKey& key = cmap.column;
            std::variant<const char*, float> value = columnValue(key, item);

            const float lineHeight = ImGui::GetTextLineHeight();
            const float buttonSize = 10.f;
            ImGui::ColorButton(
                "##NoValuecolor",
                view::helper::toImVec4(_colorMappingView->colorFromColormap(item, cmap)),
                ImGuiColorEditFlags_NoInputs | ImGuiColorEditFlags_NoTooltip,
                ImVec2(buttonSize, lineHeight)
            );

            ImGui::SameLine();

            ImGui::Text("%s:", columnName(key));
            ImGui::SameLine();
            if (std::holds_alternative<float>(value)) {
                ImGui::Text("%.2f", std::get<float>(value));
            }
            else {
                ImGui::Text("%s", std::get<const char*>(value));
            }
        }
    }
    ImGui::End();
}

void DataViewer::handleDoubleClickHoveredPlanet(int index) {
    // Do nothing if user is not holding CTRL. Note that this is a little ugly, since the
    // index is only set if CTRL is hold and this is handled in the renderable...
    // Also note that we don't want to handle these clicks if ImGui is caring about the mouse input!
    ImGuiIO& io = ImGui::GetIO();
    if (!_holdingCtrl || io.WantCaptureMouse) {
        return;
    }

    if (index < 0) {
        // No planet hovered. Clear selection if double click
        if (ImGui::IsMouseDoubleClicked(0) && _selection.size() > 0) {
            _selection.clear();
            _selectionChanged = true;
        }

        return;
    }

    const ExoplanetItem& item = _data[index];

    if (ImGui::IsMouseDoubleClicked(0)) {
        _systemViewer->showSystemView(item.hostName);

        // Select planet, if not already selected
        auto found = std::find(_selection.begin(), _selection.end(), index);
        const bool itemIsSelected = found != _selection.end();

        if (!itemIsSelected) {
            _selection.push_back(index);
        }
        else {
            _selection.erase(found);
        }
        _selectionChanged = true;
    }
}

void DataViewer::updateFilteredRowsProperty(std::optional<std::vector<size_t>> customIndices) {
    auto mod = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    Property* filteredRowsProperty = mod->property("FilteredDataRows");
    if (filteredRowsProperty) {
        std::vector<std::string> indices;

        if (customIndices.has_value()) {
            std::transform(
                customIndices.value().begin(), customIndices.value().end(), std::back_inserter(indices),
                [](size_t i) { return std::to_string(static_cast<int>(i)); }
            );
        }
        else {
            indices.reserve(_filteredData.size());
            std::transform(
                _filteredData.begin(), _filteredData.end(), std::back_inserter(indices),
                [](size_t i) { return std::to_string(static_cast<int>(i)); }
            );
        }

        const std::string script = std::format(
            "openspace.setPropertyValueSingle('{}', {{{}}})",
            filteredRowsProperty->uri(),
            ghoul::join(indices, ",")
        );

        global::scriptEngine->queueScript({
            .code = script,
            .addToLog = ScriptEngine::Script::ShouldBeLogged::No
        });
    }
}

void DataViewer::renderFileMenu() {
    static bool showOpenCsvModal = false;
    static bool showSaveCsvModal = false;
    static char csvPath[1024] = "";
    static std::string openCsvError;

    // Menu
    if (ImGui::BeginMenu("File")) {
        if (ImGui::MenuItem("New", NULL, false, false)) {
            // TODO: Create a settings.json file
        }
        if (ImGui::MenuItem("Open CSV")) {
            showOpenCsvModal = true;
            openCsvError.clear();
        }
        ImGui::MenuItem("Save CSV", NULL, &showSaveCsvModal);
        ImGui::SameLine();
        view::helper::renderHelpMarker("Save the current filtered data items as a CSV file");
        if (ImGui::MenuItem("Save filters", NULL, false, false)) {
            // TODO: Menu to save the current set of filters.
            // To a settings .json file? Or a completely new file format?
        }

        ImGui::Separator();
        ImGui::Text("Current data file:");
        ImGui::Text(currentDataFile().filename().string().c_str());
        if (ImGui::IsItemHovered()) {
            ImGui::SetTooltip("%s", currentDataFile().string().c_str());
        }
        ImGui::EndMenu();
    }

    // Modals for view
    ImVec2 center = ImGui::GetMainViewport()->GetCenter();
    ImGui::SetNextWindowPos(center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));
    ImGuiWindowFlags flags = ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_HorizontalScrollbar;

    if (showOpenCsvModal) {
        ImGui::OpenPopup("Open CSV");
        if (ImGui::BeginPopupModal("Open CSV", NULL, flags)) {
            ImGui::TextUnformatted(
                "Load a CSV using the column mappings from the current dataset."
            );
            ImGui::SetNextItemWidth(520.f);
            ImGui::InputText("CSV path", csvPath, IM_ARRAYSIZE(csvPath));

            if (!openCsvError.empty()) {
                ImGui::TextColored(
                    view::helper::toImVec4(view::colors::Error),
                    "%s",
                    openCsvError.c_str()
                );
            }

            if (ImGui::Button("Cancel") || ImGui::IsKeyPressed(ImGuiKey_Escape)) {
                ImGui::CloseCurrentPopup();
                showOpenCsvModal = false;
                openCsvError.clear();
            }
            ImGui::SameLine();
            if (ImGui::Button("Open") || ImGui::IsKeyPressed(ImGuiKey_Enter)) {
                const std::filesystem::path path = csvPath;
                if (loadCsvFile(path)) {
                    csvPath[0] = '\0';
                    ImGui::CloseCurrentPopup();
                    showOpenCsvModal = false;
                    openCsvError.clear();
                }
                else {
                    openCsvError = "Could not load the CSV file";
                }
            }
            ImGui::SetItemDefaultFocus();
            ImGui::EndPopup();
        }
    }

    if (showSaveCsvModal) {
        ImGui::OpenPopup("Save CSV");
        if (ImGui::BeginPopupModal("Save CSV", NULL, flags)) {
            ImGui::Text("Save the currently selected filtering to a CSV file.");

            static char name[128] = "";
            ImGui::InputText(
                "Filename (.csv)",
                name,
                IM_ARRAYSIZE(name)
            );

            const char* defaultDir = absPath(
                "${MODULE_EXOPLANETSEXPERTTOOL}/data"
            ).string().c_str();

            const float dirTextWidth = ImGui::CalcTextSize(defaultDir).x;
            ImGui::SetNextItemWidth(dirTextWidth * 1.3f);

            static char dir[256] = "";
            ImGui::InputTextWithHint(
                "Directory",
                defaultDir,
                dir,
                IM_ARRAYSIZE(dir)
            );

            static bool incudeAllColumns = true;
            ImGui::Checkbox("Include all columns", &incudeAllColumns);
            ImGui::SameLine();
            view::helper::renderHelpMarker(
                "If checked, include all data column in the original data file. "
                "Otherwise, only include the selected columns."
            );

            static bool savePosition = true;
            ImGui::Checkbox("Save computed XYZ position", &savePosition);
            ImGui::SameLine();
            view::helper::renderHelpMarker(
                "If checked, the computed x, y and z position of the system will also be "
                "saved, even if it was not included in the dataset from the beginning."
            );

            static bool useParsec = true;
            ImGui::Checkbox("Use Parsec for position", &useParsec);
            ImGui::SameLine();
            view::helper::renderHelpMarker("Otherwise, the position is saved in meters.");

            // TODO: Also add option to save filtering separately

            view::helper::renderDescriptiveText(std::format(
                "Will save {} rows with {} columns",
                _filteredData.size(),
                incudeAllColumns ? _data.front().dataColumns.size() :_columns.size()
            ).c_str());

            ImGui::Separator();

            // Cancel
            if (ImGui::Button("Cancel") ||
                ImGui::IsKeyPressed(ImGuiKey_Escape))
            {
                ImGui::CloseCurrentPopup();
                showSaveCsvModal = false;
            }
            ImGui::SameLine();
            // Save
            if (ImGui::Button("Save") ||
                ImGui::IsKeyPressed(ImGuiKey_Enter))
            {
                std::filesystem::path fileName = std::filesystem::path(name);
                std::filesystem::path directory = std::filesystem::path(dir);
                if (directory.empty()) {
                    directory = std::filesystem::path(defaultDir);
                }

                if (!fileName.has_extension()) {
                    fileName.replace_extension("csv");
                }
                std::filesystem::path path = directory / fileName;
                DataLoader::saveData(
                    path,
                    _data,
                    _filteredData,
                    !incudeAllColumns ? _columns : std::vector<ColumnKey>(),
                    savePosition,
                    useParsec
                );

                ImGui::CloseCurrentPopup();
                showSaveCsvModal = false;
            }
            ImGui::SetItemDefaultFocus();

            ImGui::EndPopup();
        }
    }
}

void DataViewer::renderSettingsMenu() {
    // OBS! These should match the default settings for the SGNs
    static bool showKepler = true;
    static bool showMilkyWayLine = true;

    static bool showColumnSelectionView = false;

    // Column selection is rendered in a separate window, so do this before the menu
    if (showColumnSelectionView) {
        // Always center this window when appearing
        ImVec2 center = ImGui::GetMainViewport()->GetCenter();
        ImGui::SetNextWindowPos(center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));

        ImGuiWindowFlags flags = ImGuiWindowFlags_NoSavedSettings;
        ImGui::SetNextWindowSize(ImVec2(800, 600), ImGuiCond_FirstUseEver);
        if (ImGui::Begin("Set table columns", &showColumnSelectionView, flags)) {
            if (_columnSelectionView.renderColumnSettingsView(_dataSettings)) {
                _tableView->updateColumns(_columnSelectionView.orderedSelectedColumns());
            }
            ImGui::End();
        }
    }

    if (!ImGui::BeginMenu("Settings")) {
        return;
    }

    if (ImGui::Button("Set up table columns...")) {
        showColumnSelectionView = true;
    }

    ImGui::Separator();

    if (ImGui::Checkbox("Show Kepler FOV cue", &showKepler)) {
        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('{}', {})",
            "Scene.KeplerPrism.Renderable.Enabled",
            showKepler
        ));
    }

    if (ImGui::Checkbox("Show line to Milky Way center", &showMilkyWayLine)) {
        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('{}', {})",
            "Scene.MilkyWayEarthLine.Renderable.Enabled",
            showMilkyWayLine
        ));
    }

    ImGui::Separator();

    const char* items[] = { "Rings", "Inclination", "Star", "Inclination and Star" };
    static int item_current = 0;
    ImGui::SetNextItemWidth(120);
    if (ImGui::Combo("Glyph mode", &item_current, items, IM_ARRAYSIZE(items))) {
        _glyphHandler.setGlyphMode(static_cast<GlyphHandler::GlyphMode>(item_current));
    };

    _glyphHandler.renderModeSpecificSettings();

    ImGui::Separator();

    // Font size
    ImGuiIO& io = ImGui::GetIO();
    float dragWidth = 60.f * io.FontGlobalScale;
    {
        constexpr float MIN_GUI_SCALE = 0.3f;
        constexpr float MAX_GUI_SCALE = 2.0f;
        ImGui::SetNextItemWidth(dragWidth);
        ImGui::DragFloat(
            "GUI font scale", &io.FontGlobalScale, 0.005f,
            MIN_GUI_SCALE, MAX_GUI_SCALE, "%.2f", ImGuiSliderFlags_AlwaysClamp
        );
    }

    {
        constexpr float MIN_GLYPH_SCALE = 0.3f;
        constexpr float MAX_GLYPH_SCALE = 2.0f;
        static float glyphSizeScale = GlyphHandler::DefaultGlyphScale;
        ImGui::SetNextItemWidth(dragWidth);
        bool changed = ImGui::DragFloat(
            "Glyph scale", &glyphSizeScale, 0.005f,
            MIN_GLYPH_SCALE, MAX_GLYPH_SCALE, "%.2f", ImGuiSliderFlags_AlwaysClamp
        );

        if (changed) {
            _glyphHandler.setGlyphScale(glyphSizeScale);
        }
    }

    ImGui::EndMenu();
}

void DataViewer::updateGlyphRenderData() {
    LDEBUG("Updating glyph render data");

    // For now, only write the filtered data. Later on we might want to render the
    // filtered out points somehow and then we should write out the full dataset

    using GlyphRenderData = ExoplanetsExpertToolModule::GlyphRenderData;

    std::vector<GlyphRenderData::Item> data;
    data.reserve(_filteredData.size());

    for (size_t index : _filteredData) {
        const ExoplanetItem& item = _data[index];
        if (!item.position.has_value()) {
            // Skip items without position, since they can't be rendered
            continue;
        }

        GlyphRenderData::Item renderItem;
        renderItem.index = index;
        renderItem.position = *item.position;
        renderItem.component = static_cast<size_t>(item.indexInSystem + 1);

        size_t nVariables = _colorMappingView->colorMapperVariables().size();

        for (int i = 0; i < nVariables; ++i) {
            const ImVec4 color = view::helper::toImVec4(
                _colorMappingView->colorFromColormap(
                    item,
                    _colorMappingView->colorMapperVariables()[i]
                )
            );
            renderItem.colors.push_back(
                _colorMappingView->colorFromColormap(
                    item,
                    _colorMappingView->colorMapperVariables()[i]
                )
            );
        }

        const std::variant<std::string, float>& inclination =
            item.dataColumns.at("pl_orbincl"); // TODO: Do not hardcode

        if (std::holds_alternative<float>(inclination)) {
            renderItem.inclination = std::get<float>(inclination);
        }
        else {
            renderItem.inclination = std::numeric_limits<float>::quiet_NaN();
        }

        data.push_back(renderItem);
    }
    data.shrink_to_fit();

    auto mod = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    mod->updateGlyphRenderData(std::move(data));
}

void DataViewer::refocusView() const {
    global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth');"
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');"
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    );
}

void DataViewer::flyToOverview() const {
    // Create a linear path to Earth
    global::scriptEngine->queueScript(
        "openspace.pathnavigation.createPath({"
            "TargetType = 'Node', "
            "Target = 'Earth', "
            "Height = 5e+19, " // distance is what matters
            "Duration = 4, "
            "PathType = 'Linear'"
        "});"
    );
}

void DataViewer::flyToInsideView() const {
    // Create a linear path to Earth
    global::scriptEngine->queueScript(
        "openspace.pathnavigation.createPath({"
            "TargetType = 'Node', "
            "Target = 'Earth', "
            "Height = 5e+13, " // distance is what matters
            "Duration = 4, "
            "PathType = 'Linear'"
        "});"
    );
}

void DataViewer::computeMeanForColumn(const ColumnKey& key) {
    int count = 0;
    float sum = 0.f;
    for (size_t rowIndex = 0; rowIndex < _data.size(); ++rowIndex) {
        const std::variant<const char*, float> value = columnValue(key, rowIndex);
        if (!std::holds_alternative<float>(value)) {
            LERROR(std::format(
                "Trying to compute mean value for non-numeric column: {}. Skipping. "
                "OBS! This is a sign that the column is wrongly classified as numeric",
                key
            ));
            return;
        }
        const float v = std::get<float>(value);
        if (!std::isnan(v)) {
            sum += v;
            count++;
        }
    }

    _meanColumnValues[key] = sum / static_cast<float>(count);
}

} // namespace openspace::exoplanets
