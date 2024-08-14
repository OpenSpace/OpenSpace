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

#include <modules/exoplanetsexperttool/dataviewer.h>

#include <modules/exoplanets/exoplanetshelper.h>
#include <modules/exoplanetsexperttool/columnfilter.h>
#include <modules/exoplanetsexperttool/datahelper.h>
#include <modules/exoplanetsexperttool/exoplanetsexperttoolmodule.h>
#include <modules/exoplanetsexperttool/rendering/renderableexoplanetglyphcloud.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <algorithm>
#include <fstream>
#include <iostream>

#include <chrono>

#ifdef WIN32
#include <Windows.h>
#else // WIN32
#include <iomanip>
#include <sstream>
#include <sys/time.h>
#endif // WIN32


#include <implot.h>

//#define SHOW_IMGUI_HELPERS

namespace {
    constexpr char _loggerCat[] = "ExoplanetsDataViewer";

    constexpr char RenderDataFile[] = "${TEMPORARY}/pointrenderdata.dat";
    constexpr char LabelsFile[] = "${TEMPORARY}/exosystems.label";

    constexpr char WebpagePath[] = "${MODULE_EXOPLANETSEXPERTTOOL}/webpage/index.html";

    constexpr char AboutTheTool[] =
        "This is a research tool under development and we are currently \n"
        "looking for feedback from users. This feedback will be included \n"
        "in our scientific publication covering the tool. \n"
        "\n"
        "Thank you for taking the time to trying it out, and please do not \n"
        "hesitate to reach out with any questions, input or feedback";

    constexpr char GetInTouchLink[] =
        "https://data.openspaceproject.com/release/ExoplanetExplorer/misc/get_in_touch";

    void queueScriptSynced(const std::string& script) {
        using namespace openspace;
        global::scriptEngine->queueScript(
            script,
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }

    void setRenderableEnabled(std::string_view id, bool value) {
        using namespace openspace;
        queueScriptSynced(std::format(
            "openspace.setPropertyValueSingle('{}', {});",
            std::format("Scene.{}.Renderable.Enabled", id),
            value ? "true" : "false"
        ));
    };

    void setDefaultValueOrbitVisuals(bool shouldShowIfDefault) {
        const std::string appearanceUri = "{defaultvalues_shape}.Renderable";
        if (shouldShowIfDefault) {
            // Draw using points
            queueScriptSynced(std::format(
                "openspace.setPropertyValue('{0}.Appearance.Rendering', 1.0);"
                "openspace.setPropertyValue('{0}.Appearance.PointSize', 64.0);"
                "openspace.setPropertyValue('{0}.Appearance.EnableFade', false);"
                "openspace.setPropertyValue('{0}.Resolution', 100);",
                appearanceUri
            ));
        }
        else {
            // Draw using lines
            queueScriptSynced(std::format(
                "openspace.setPropertyValue('{0}.Appearance.Rendering', 0.0);"
                "openspace.setPropertyValue('{0}.Appearance.EnableFade', true);",
                appearanceUri
            ));
        }
    };

    // Set increased reach factors of all exoplanet renderables, to trigger fading out of
    // glyph cloud
    void setIncreasedReachfactors() {
        queueScriptSynced(
            "openspace.setPropertyValue('{exoplanet}.ApproachFactor', 15000000.0)"
            "openspace.setPropertyValue('{exoplanet_system}.ApproachFactor', 15000000.0)"
        );
    }

    bool hasTag(const openspace::SceneGraphNode* node, std::string_view tag) {
        if (!node) {
            return false;
        }
        const std::vector<std::string>& tags = node->tags();

        return std::find(tags.begin(), tags.end(), tag) != std::end(tags);
    };

    // This should match the implementation in the exoplanet module
    std::string planetIdentifier(const openspace::exoplanets::ExoplanetItem& p) {
        using namespace openspace::exoplanets;
        return openspace::makeIdentifier(p.name);
    }

    const ImVec2 DefaultWindowSize = ImVec2(350, 350);
    constexpr const float DefaultGlyphSize = 22.f;

    // @TODO this could be a templated helper function for lists. Used a lot
    std::string formatIndicesList(const std::vector<size_t>& indices) {
        std::string result;
        for (size_t i : indices) {
            result += std::to_string(i) + ',';
        }
        if (!result.empty()) {
            result.pop_back();
        }
        return result;
    }

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

    // Format string for system window name
    std::string systemWindowName(std::string_view host) {
        return std::format("System: {}", host);
    }

    constexpr const openspace::properties::Property::PropertyInfo ExternalSelectionInfo =
    {
        "ExternalSelection",
        "External Selection from Webpage",
        "Contains the indices of the rows in the data file that should be included, "
        "based on the filtering on the external webpage.",
        openspace::properties::Property::Visibility::Hidden
    };
}

namespace openspace::exoplanets {

DataViewer::DataViewer(std::string identifier, std::string guiName)
    : properties::PropertyOwner({ std::move(identifier), std::move(guiName) })
    , _externalSelection(ExternalSelectionInfo)
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

    _dataSettings = DataLoader::loadDataSettingsFromJson();

    // Load the dataset
    _data = DataLoader::loadData(_dataSettings);

    // Initialize filtered data index list and map of host star to planet indices
    _filteredData.reserve(_data.size());
    for (size_t i = 0; i < _data.size(); i++) {
        _filteredData.push_back(i);
        _hostIdToPlanetsMap[makeIdentifier(_data[i].hostName)].push_back(i);
    }

    _columns = _columnSelectionView.initializeColumnsFromData(_data, _dataSettings);

    // The other views use the loaded data, so call this afterwards
    _colorMappingView = std::make_unique<ColorMappingView>(*this, _dataSettings);
    _filteringView = std::make_unique<FilteringView>(*this, _dataSettings);

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

void DataViewer::initializeGL() {
    initializeRenderables();
    initializeCallbacks();

    _colorMappingView->initializeGL();
}

std::variant<const char*, float> DataViewer::columnValue(const ColumnKey& key,
    const ExoplanetItem& item) const
{
    const std::variant<std::string, float>& value = item.dataColumns.at(key);

    if (std::holds_alternative<std::string>(value)) {
        return std::get<std::string>(value).c_str();
    }
    return std::get<float>(value);
}

bool DataViewer::isNumericColumn(size_t index) const {
    ghoul_assert(_data.size() > 0, "Data size cannot be zero");
    // Test type using the first data point
    std::variant<const char*, float> aValue = columnValue(_columns[index], _data.front());
    return std::holds_alternative<float>(aValue);
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
    return _dataSettings.columnName(key);
}

const char* DataViewer::columnName(size_t columnIndex) const {
    // TODO: validate index
    return _dataSettings.columnName(_columns[columnIndex]);
}

const ColumnKey& DataViewer::nameColumn() const {
    return _dataSettings.dataMapping.name;
}

bool DataViewer::isNameColumn(const ColumnKey& key) const {
    return key == nameColumn();
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

const std::vector<size_t>& DataViewer::planetsForHost(const std::string& hostIdentifier) const {
    ghoul_assert(_hostIdToPlanetsMap.contains(hostIdentifier), "Map must contain the host");
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
        ImGui::Text(AboutTheTool);
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
        ImGuiIO& io = ImGui::GetIO();
        if (ImGui::Button("Continue", ImVec2(120, 0)) ||
            ImGui::IsKeyPressed(io.KeyMap[ImGuiKey_Enter]))
        {
            ImGui::CloseCurrentPopup();
            _shouldOpenInfoWindow = false;
        }
        ImGui::SetItemDefaultFocus();

        ImGui::EndPopup();
    }
}

void DataViewer::initializeRenderables() {
    using namespace std::string_literals;

    writeRenderDataToFile();

    std::filesystem::path dataFilePath = absPath(RenderDataFile);
    std::filesystem::path labelsFilePath = absPath(LabelsFile);

    if (!std::filesystem::is_regular_file(dataFilePath)) {
        LWARNING("Count not find data file for points rendering");
        return;
    }

    if (!std::filesystem::is_regular_file(labelsFilePath)) {
        LWARNING("Count not find file for labels rendering");
        return;
    }

    ghoul::Dictionary gui;
    gui.setValue("Name", "All Exoplanets"s);
    gui.setValue("Path", "/ExoplanetExplorer"s);

    ghoul::Dictionary renderable;
    renderable.setValue("Type", "RenderableExoplanetGlyphCloud"s);
    renderable.setValue("Size", 100.0);
    renderable.setValue("BillboardMinMaxSize", glm::dvec2(DefaultGlyphSize));
    renderable.setValue("UseFixedWidth", false);
    renderable.setValue("RenderBinMode", "PreDeferredTransparent"s);
    renderable.setValue("DataFile", dataFilePath.string());
    renderable.setValue("HighlightColor", glm::dvec3(
        view::colors::DefaultSelected
    ));

    ghoul::Dictionary labels;
    labels.setValue("File", labelsFilePath.string());
    labels.setValue("Size", 15);
    labels.setValue("MinMaxSize", glm::ivec2(4, 12));
    labels.setValue("Unit", "pc"s);
    renderable.setValue("Labels", labels);

    ghoul::Dictionary node;
    node.setValue("Identifier", std::string(ExoplanetsExpertToolModule::GlyphCloudIdentifier));
    node.setValue("Renderable", renderable);
    node.setValue("GUI", gui);

    queueScriptSynced(
        std::format("openspace.addSceneGraphNode({})", ghoul::formatLua(node))
    );
}

void DataViewer::initializeCallbacks() {
    properties::Property* anchorProperty =
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
        else if (hasTag(node, "exoplanet")) {
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
    static bool showTable = true;
    static bool showFilterSettingsWindow = false;
    static bool showColormapWindow = false;
    static bool showHelpers = false;

    auto mod = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    if (mod->showInfoWindowAtStartup() && _shouldOpenInfoWindow) {
        renderStartupInfo();
        return;
    }

    // Tooltip for hovered planets
    int hoveredPlanet = getHoveredPlanetIndex();
    renderPlanetTooltip(hoveredPlanet);
    handleDoubleClickHoveredPlanet(hoveredPlanet);

    if (ImGui::BeginMainMenuBar()) {
        if (ImGui::BeginMenu("Windows")) {
            ImGui::MenuItem("Table", NULL, &showTable);
            ImGui::MenuItem("Filters", NULL, &showFilterSettingsWindow);
            ImGui::MenuItem("Color mapping", NULL, &showColormapWindow);
            if (mod->showInfoWindowAtStartup()) {
                ImGui::Separator();
                ImGui::MenuItem("Start-up info", NULL, &_shouldOpenInfoWindow);
            }

#ifdef SHOW_IMGUI_HELPERS
            ImGui::MenuItem("ImGui Helpers", NULL, &showHelpers);
#endif
            ImGui::EndMenu();
        }


        if (ImGui::BeginMenu("Settings")) {
            renderSettingsMenuContent();
            ImGui::EndMenu();
        }

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
                bool isAlreadyOpen = std::find(
                    _shownPlanetSystemWindows.begin(),
                    _shownPlanetSystemWindows.end(),
                    system
                ) != _shownPlanetSystemWindows.end();

                if (!isAlreadyOpen) {
                    _shownPlanetSystemWindows.push_back(system);
                }
                ImGui::EndMenu();
            }
        }

        ImGui::EndMainMenuBar();
    }

    // Windows
    if (showFilterSettingsWindow) {
        renderFilterSettingsWindow(&showFilterSettingsWindow);
    }

    if (showColormapWindow) {
        renderColormapWindow(&showColormapWindow);
    }

    if (showTable) {
        renderTableWindow(&showTable);
    }

    // Render any detail windows that the user has requested
    if (!_shownPlanetSystemWindows.empty()) {
        std::list<std::string> hostsToRemove;
        for (const std::string& host : _shownPlanetSystemWindows) {
            bool isOpen = true;

            ImGui::SetNextWindowSize(ImVec2(500.f, 0.f), ImGuiCond_Appearing);
            if (ImGui::Begin(systemWindowName(host).c_str(), &isOpen)) {
                renderSystemViewContent(host);
            }
            ImGui::End();

            if (!isOpen) {
                // was closed => remove from list
                hostsToRemove.push_back(host);
            }
        }

        for (const std::string& host : hostsToRemove) {
            _shownPlanetSystemWindows.remove(host);
        }
    }

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
        writeRenderDataToFile();
        _colormapWasChanged = false;
        _filterChanged = false;
    }
}

void DataViewer::renderColormapWindow(bool* open) {
    ImGui::SetNextWindowSize(ImVec2(350, 450), ImGuiCond_FirstUseEver);
    if (!ImGui::Begin("Color mapping", open)) {
        ImGui::End();
        return;
    }

    _colormapWasChanged = _colorMappingView->renderViewContent();
}

void DataViewer::renderTableWindow(bool *open) {
    ImGui::SetNextWindowSize(DefaultWindowSize, ImGuiCond_FirstUseEver);

    if (!ImGui::Begin("Exoplanet Explorer Table", open)) {
        ImGui::End();
        return;
    }

    // @TODO: Maybe do a more sophisticated comparison view
    bool showPinnedTable = ImGui::CollapsingHeader("Pinned planets");
    ImGui::SameLine();
    view::helper::renderDescriptiveText(
        std::format("({})", _pinnedPlanets.size()).c_str()
    );
    if (showPinnedTable) {
        renderTable("pinned_exoplanets_table", _pinnedPlanets, true);
    }

    ImGui::Separator();
    view::helper::renderDescriptiveText(std::format(
        "Showing {} exoplanets out of a total {} ",
        _filteredData.size(), _data.size()
    ).c_str());

    // Search table
    static char searchString[128] = "";
   ImGui::InputTextWithHint(
        "##Query",
        "Search for a planet here...",
        searchString,
        IM_ARRAYSIZE(searchString)
    );
    ImGui::SameLine();
    if (ImGui::Button("Clear")) {
        strcpy(searchString, "");
    }

    renderTable("full_exoplanets_table", _filteredData, false, searchString);

    ImGui::End();
}

void DataViewer::renderTable(const std::string& tableId,
                             std::vector<size_t>& planetRows, bool useFixedHeight,
                             std::string_view search)
{
    static ImGuiTableFlags flags =
        ImGuiTableFlags_ScrollX | ImGuiTableFlags_ScrollY
        | ImGuiTableFlags_BordersV | ImGuiTableFlags_BordersOuter
        | ImGuiTableFlags_Reorderable | ImGuiTableFlags_Hideable
        | ImGuiTableFlags_Sortable | ImGuiTableFlags_Resizable
        | ImGuiTableFlags_RowBg;

    const int nColumns = static_cast<int>(_columns.size());

    // Some size variables
    const float RowHeight = ImGui::GetTextLineHeightWithSpacing(); // Inner height
    const float TableHeight =
        (planetRows.size() + 1) * 1.2f * RowHeight + ImGui::GetStyle().ScrollbarSize;
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
            if (isNameColumn(c)) {
                colFlags |= ImGuiTableColumnFlags_DefaultSort;
            }
            ImGui::TableSetupColumn(columnName(c), colFlags, 0.f, colIdx);
        }

        // Make header and first column (name) always visible
        ImGui::TableSetupScrollFreeze(2, 1);

        // Instead of calling TableHeadersRow(), we set up custom headers with help markers
        //ImGui::TableHeadersRow();
        ImGui::TableNextRow(ImGuiTableRowFlags_Headers);

        ImGui::TableHeader("");

        for (int colIdx = 0; colIdx < _columns.size(); colIdx++) {
            const ColumnKey c = _columns[colIdx];
            ImGui::TableSetColumnIndex(colIdx + 1);
            ImGui::PushID(colIdx);
            ImGui::TableHeader(columnName(c));

            if (_dataSettings.hasDescription(c)) {
                const float TEXT_WIDTH = ImGui::CalcTextSize(columnName(c)).x;
                ImGui::SameLine(0.0f, TEXT_WIDTH + 2.f);
                view::helper::renderHelpMarker(_dataSettings.description(c).c_str());
            }

            ImGui::PopID();
        }

        // Sorting
        if (ImGuiTableSortSpecs* sortSpecs = ImGui::TableGetSortSpecs()) {
            if (sortSpecs->SpecsDirty || _filterChanged) {
                auto compare = [&sortSpecs, this](const size_t& lhs,
                                                  const size_t& rhs) -> bool
                {
                    ImGuiSortDirection sortDir = sortSpecs->Specs->SortDirection;
                    bool flip = (sortDir == ImGuiSortDirection_Descending);

                    const ExoplanetItem& l = flip ? _data[rhs] : _data[lhs];
                    const ExoplanetItem& r = flip ? _data[lhs] : _data[rhs];

                    int colIndex = static_cast<int>(sortSpecs->Specs->ColumnUserID);
                    ColumnKey key = _columns[colIndex];

                    return compareColumnValues(key, l, r);
                };

                std::sort(planetRows.begin(), planetRows.end(), compare);
                sortSpecs->SpecsDirty = false;
            }
        }

        std::vector<size_t> displayedRows;
        if (search.empty()) {
            displayedRows = planetRows;
        }
        else {
            for (size_t r : planetRows) {
                bool passSearch = ColumnFilter(
                    std::string(search),
                    ColumnFilter::Type::Text
                ).passFilter(_data[r].name);

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
                const ExoplanetItem& item = _data[index];

                ImGuiSelectableFlags selectableFlags = ImGuiSelectableFlags_SpanAllColumns
                    | ImGuiSelectableFlags_AllowItemOverlap;

                auto found = std::find(_selection.begin(), _selection.end(), index);
                const bool itemIsSelected = found != _selection.end();

                ImGui::TableNextRow(ImGuiTableRowFlags_None, RowHeight);

                ImGui::TableNextColumn();
                if (systemCanBeAdded(item.hostName)) {
                    ImGui::PushID(std::format("addbutton{}", row).c_str());
                    if (ImGui::Button("+", ImVec2(20, RowHeight))) {
                        addExoplanetSystem(item.hostName);
                    }
                    ImGui::PopID();
                }
                else {
                    // Add a target button instead
                    ImGui::PushID(std::format("targetbutton{}", row).c_str());

                    // Check if is target item. The GUI name should be set from the planet name
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
                        addOrTargetPlanet(item);
                    }
                    ImGui::PopStyleColor(2);

                    ImGui::PopID();
                }

                for (int colIdx = 0; colIdx < _columns.size(); colIdx++) {
                    const ColumnKey col = _columns[colIdx];
                    ImGui::TableNextColumn();

                    if (isNameColumn(col)) {
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
                                _pinnedPlanets.begin(),
                                _pinnedPlanets.end(),
                                index
                            );
                            bool isPinned = foundIndex != _pinnedPlanets.end();

                            ImGui::SameLine();
                            ImGui::SetNextItemWidth(-10);
                            if (ImGui::Button(isPinned ? "Unpin" : "Pin")) {
                                if (isPinned) {
                                    _pinnedPlanets.erase(foundIndex);
                                }
                                else {
                                    _pinnedPlanets.push_back(index);
                                }
                            }

                            ImGui::Separator();

                            ImGui::Text(item.referenceName.c_str());
                            ImGui::SameLine();

                            if (ImGui::Button("Link (Chrome)")) {
                                system(std::format("start chrome.exe {}", item.referenceUrl).c_str());
                            }
                            ImGui::SameLine();
                            if (ImGui::Button("Link (Firefox)")) {
                                system(std::format("start firefox {}", item.referenceUrl).c_str());
                            }

                            ImGui::Separator();

                            bool isAlreadyOpen = std::find(
                                _shownPlanetSystemWindows.begin(),
                                _shownPlanetSystemWindows.end(),
                                item.hostName
                            ) != _shownPlanetSystemWindows.end();

                            if (!isAlreadyOpen) {
                                ImGui::PushID(std::format("ShowShystemView-{}", item.name).c_str());
                                if (ImGui::Button("Show system view")) {
                                    _shownPlanetSystemWindows.push_back(item.hostName);
                                    ImGui::CloseCurrentPopup();
                                }
                                ImGui::PopID();
                            }
                            else {
                                ImGui::TextDisabled("A system view is already opened for this system");
                            }

                            bool systemIsAdded = !systemCanBeAdded(item.hostName);
                            if (systemIsAdded) {
                                if (ImGui::Button("Zoom to star")) {
                                    flyToStar(makeIdentifier(item.hostName));
                                }
                            }
                            else {
                                if (ImGui::Button("+ Add system")) {
                                    addExoplanetSystem(item.hostName);
                                }
                            }

                            ImGui::EndPopup();
                        }
                        ImGui::PopID();

                        // Check double click, left mouse button
                        if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(0)) {
                            LINFO(std::format("Double click: {}", item.name));
                            addOrTargetPlanet(item);

                            // Also open the system view for that system
                            bool isAlreadyOpen = std::find(
                                _shownPlanetSystemWindows.begin(),
                                _shownPlanetSystemWindows.end(),
                                item.hostName
                            ) != _shownPlanetSystemWindows.end();

                            if (!isAlreadyOpen) {
                                _shownPlanetSystemWindows.push_back(item.hostName);
                            }
                            else {
                                // Bring window to front
                                ImGui::SetWindowFocus(systemWindowName(item.hostName).c_str());
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

                            _selectionChanged = true;
                        }
                        continue;
                    }

                    renderColumnValue(colIdx, item);
                }
            }
        }
        ImGui::EndTable();

        if (_selectionChanged) {
            updateSelectionInRenderable();
            _selectionChanged = false;
        }
    }
}

void DataViewer::renderFilterSettingsWindow(bool* open) {
    // Reset some state changed variables
    _filterChanged = false;
    _externalSelectionChanged = false;

    ImGui::SetNextWindowSize(ImVec2(430, 450), ImGuiCond_FirstUseEver);
    if (!ImGui::Begin("Filters", open)) {
        ImGui::End();
        return;
    }

    _filterChanged = _filteringView->renderFilterSettings();

    // Update the filtered data
    if (_filterChanged) {
        _filteredData = _filteringView->applyFiltering(
            _data,
            _externalSelection.value()
        );
    }

    ImGui::Separator();

    view::helper::renderDescriptiveText(std::format(
        "Number items after filtering: {} / {}",
        _filteredData.size(), _data.size()
    ).c_str());

    ImGui::End(); // Filter settings window

    updateFilteredRowsProperty();

    // Clear selection
    if (_filterChanged) {
        _selection.clear();
        updateSelectionInRenderable();
    }
}

int DataViewer::getHoveredPlanetIndex() const {
    std::string sgnId = std::string(ExoplanetsExpertToolModule::GlyphCloudIdentifier);
    SceneGraphNode* n = sceneGraphNode(sgnId);
    if (!n) {
        return -1;
    }

   RenderableExoplanetGlyphCloud* cloud =
       dynamic_cast<RenderableExoplanetGlyphCloud*>(n->renderable());
   if (!cloud) {
       return -1;
   }

   properties::Property* p = cloud->property("CurrentlyHoveredIndex");
   properties::IntProperty* index = dynamic_cast<properties::IntProperty*>(p);
   return index ? *index : -1;
}

void DataViewer::renderPlanetTooltip(int index) const {
    if (index < 0) {
        return; // no planet hovered
    }

    // Show tooltip iwndow on mouse position
    ImVec2 pos = ImGui::GetIO().MousePos;
    ImGui::SetNextWindowPos(pos, ImGuiCond_Appearing, ImVec2(-0.01f, 1.f));
    ImGui::SetNextWindowBgAlpha(0.35f); // Transparent background
    ImGuiWindowFlags flags = ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoDecoration |
        ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_NoFocusOnAppearing;

    const ExoplanetItem& item = _data[index];

    if (ImGui::Begin("##planetToolTip", NULL, flags)) {
        ImGui::Text(item.name.c_str());
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
        // Open system window
        bool isAlreadyOpen = std::find(
            _shownPlanetSystemWindows.begin(),
            _shownPlanetSystemWindows.end(),
            item.hostName
        ) != _shownPlanetSystemWindows.end();

        if (!isAlreadyOpen) {
            _shownPlanetSystemWindows.push_back(item.hostName);
        }
        else {
            // Bring window to front
            ImGui::SetWindowFocus(systemWindowName(item.hostName).c_str());
        }

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
    properties::Property* filteredRowsProperty = mod->property("FilteredDataRows");
    if (filteredRowsProperty) {
        std::vector<int> indices;

        if (customIndices.has_value()) {
            std::transform(
                customIndices.value().begin(), customIndices.value().end(), std::back_inserter(indices),
                [](size_t i) { return static_cast<int>(i); }
            );
        }
        else {
            indices.reserve(_filteredData.size());
            std::transform(
                _filteredData.begin(), _filteredData.end(), std::back_inserter(indices),
                [](size_t i) { return static_cast<int>(i); }
            );
        }

        // TODO: should set this over Lua script API instead
        filteredRowsProperty->set(indices);
    }
}

void DataViewer::renderSettingsMenuContent() {
    // OBS! These should match the default settings for the SGNs
    static bool useFixedWidth = false;
    static bool showKepler = true;
    static bool showMilkyWayLine = true;

    // This function also renders the buttons that opens the modal
    _columnSelectionView.renderColumnSettingsView(_columns, _dataSettings);

    if (ImGui::Checkbox("Use fixed ring width", &useFixedWidth)) {
        queueScriptSynced(std::format(
            "openspace.setPropertyValueSingle('{}', {})",
            std::format(
                "Scene.{}.Renderable.UseFixedWidth",
                ExoplanetsExpertToolModule::GlyphCloudIdentifier
            ),
            useFixedWidth
        ));
    }

    if (ImGui::Checkbox("Show Kepler FOV cue", &showKepler)) {
        queueScriptSynced(std::format(
            "openspace.setPropertyValueSingle('{}', {})",
            "Scene.KeplerPrism.Renderable.Enabled",
            showKepler
        ));
    }

    if (ImGui::Checkbox("Show line to Milky Way center", &showMilkyWayLine)) {
        queueScriptSynced(std::format(
            "openspace.setPropertyValueSingle('{}', {})",
            "Scene.MilkyWayEarthLine.Renderable.Enabled",
            showMilkyWayLine
        ));
    }

    ImGui::Separator();

    // Font size
    ImGuiIO& io = ImGui::GetIO();
    float dragWidth = 60.f * io.FontGlobalScale;
    {
        const float MIN_GUI_SCALE = 0.3f;
        const float MAX_GUI_SCALE = 2.0f;
        ImGui::SetNextItemWidth(dragWidth);
        ImGui::DragFloat(
            "GUI font scale", &io.FontGlobalScale, 0.005f,
            MIN_GUI_SCALE, MAX_GUI_SCALE, "%.2f", ImGuiSliderFlags_AlwaysClamp
        );
    }

    {
        const float MIN_GLYPH_SCALE = 0.3f;
        const float MAX_GLYPH_SCALE = 2.0f;
        static float glyphSizeScale = 1.0;
        ImGui::SetNextItemWidth(dragWidth);
        bool changed = ImGui::DragFloat(
            "Glyph scale", &glyphSizeScale, 0.005f,
            MIN_GLYPH_SCALE, MAX_GLYPH_SCALE, "%.2f", ImGuiSliderFlags_AlwaysClamp
        );

        if (changed) {
            queueScriptSynced(std::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.BillboardMinMaxSize', {})",
                ExoplanetsExpertToolModule::GlyphCloudIdentifier,
                ghoul::to_string(glm::dvec2(DefaultGlyphSize * glyphSizeScale))
            ));
        }
    }
}

void DataViewer::renderSystemViewContent(const std::string& host) {
    const std::string hostIdentifier = makeIdentifier(host);
    bool systemIsAdded = !systemCanBeAdded(host);

    std::vector<size_t>& planetIndices = _hostIdToPlanetsMap[makeIdentifier(host)];

    static bool changeDefaultValueOrbitAppearance = false;

    ImGui::BeginGroup();
    {
        if (!systemIsAdded) {
            if (ImGui::Button("Add system")) {
                addExoplanetSystem(host);
                setDefaultValueOrbitVisuals(changeDefaultValueOrbitAppearance);
            }
        }
        else {
            // Button to focus Star
            if (ImGui::Button("Focus star")) {
                // Ugly: Always set reach factors when targetting object;
                // we can't do it until the system is added to the scene
                setIncreasedReachfactors();

                queueScriptSynced(std::format(
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '{}');"
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');"
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);",
                    hostIdentifier
                ));
            }

            ImGui::SameLine();
            if (ImGui::Button("Zoom to star")) {
                flyToStar(hostIdentifier);
            }

            // Buttons to enable/disable helper renderables
            const std::string sizeRingId = hostIdentifier + "_1AU_Circle";
            const Renderable* sizeRing = renderable(sizeRingId);
            if (sizeRing) {
                bool enabled = sizeRing->isEnabled();
                if (ImGui::Checkbox("Show 1 AU Ring ", &enabled)) {
                    setRenderableEnabled(sizeRingId, enabled);
                }
                ImGui::SameLine();
                view::helper::renderHelpMarker(
                    "Show a ring with a radius of 1 AU around the star of the system"
                );
            }

            const std::string inclinationPlaneId = hostIdentifier + "_EdgeOnInclinationPlane";
            const Renderable* inclinationPlane = renderable(inclinationPlaneId);
            if (inclinationPlane) {
                bool enabled = inclinationPlane->isEnabled();
                if (ImGui::Checkbox("Show 90 degree inclination plane", &enabled)) {
                    setRenderableEnabled(inclinationPlaneId, enabled);
                }
                ImGui::SameLine();
                view::helper::renderHelpMarker(
                    "Show a grid plane that represents 90 degree inclinaiton, "
                    "i.e. orbits in this plane are visible \"edge-on\" from Earth"
                );
            }

            const std::string arrowId = hostIdentifier + "_EarthDirectionArrow";
            const Renderable* arrow = renderable(arrowId);
            if (arrow) {
                bool enabled = arrow->isEnabled();
                if (ImGui::Checkbox("Show direction to Earth", &enabled)) {
                    setRenderableEnabled(arrowId, enabled);
                }
                ImGui::SameLine();
                view::helper::renderHelpMarker(
                    "Show an arrow pointing in the direction from the host star "
                    "to Earth"
                );
            }

            const std::string habitableZoneId = hostIdentifier + "_HZ_Disc";
            const Renderable* habitableZone = renderable(habitableZoneId);
            if (habitableZone) {
                bool enabled = habitableZone->isEnabled();
                if (ImGui::Checkbox("Show habitable zone", &enabled)) {
                    setRenderableEnabled(habitableZoneId, enabled);
                }
            }

            if (planetIndices.size() > 0) {
                // Assume that if first one we find is enabled/disabled, all are
                std::string planetDiscId;
                for (size_t i : planetIndices) {
                    const std::string discId = planetIdentifier(_data[i]) + "_Disc";
                    if (renderable(discId)) {
                        planetDiscId = discId;
                        break;
                    }
                }

                const Renderable* planetOrbitDisc = renderable(planetDiscId);
                if (planetOrbitDisc) {
                    bool enabled = planetOrbitDisc->isEnabled();

                    if (ImGui::Checkbox("Show orbit uncertainty", &enabled)) {
                        for (size_t i : planetIndices) {
                            const ExoplanetItem& p = _data[i];
                            const std::string discId = planetIdentifier(p) + "_Disc";
                            if (renderable(discId)) {
                                setRenderableEnabled(discId, enabled);
                            }
                        }
                    }
                    ImGui::SameLine();
                    view::helper::renderHelpMarker(
                        "Show/hide the disc overlayed on planet orbits that visualizes the "
                        "uncertainty of the orbit's semi-major axis"
                    );
                }
            }

            if (ImGui::Checkbox(
                    "Point orbit for default values",
                    &changeDefaultValueOrbitAppearance
                ))
            {
                setDefaultValueOrbitVisuals(changeDefaultValueOrbitAppearance);
            }
            ImGui::SameLine();
            view::helper::renderHelpMarker(
                "Orbits whose shape/inclination is set using default values will be "
                "rendered as points instead of lines. This setting is applied globally "
                "across all rendered systems"
            );
        }

        ImGui::EndGroup();
    }

    ImGui::SameLine();
    ImGui::BeginGroup();
    {
        if (systemIsAdded) {
            static bool colorOrbits = false;
            bool colorOptionChanged = ImGui::Checkbox("Color planet orbits", &colorOrbits);

            auto colorTrail = [](const ExoplanetItem& p, const glm::vec3& color) {
                const std::string planetTrailId = planetIdentifier(p) + "_Trail";
                const std::string planetDiscId = planetIdentifier(p) + "_Disc";

                if (renderable(planetTrailId)) {
                    std::string propertyId = std::format(
                        "Scene.{}.Renderable.Appearance.Color", planetTrailId
                    );
                    queueScriptSynced(std::format(
                        "openspace.setPropertyValueSingle('{}', {});",
                        propertyId, ghoul::to_string(color)
                    ));
                }

                if (renderable(planetDiscId)) {
                    std::string propertyId = std::format(
                        "Scene.{}.Renderable.MultiplyColor", planetDiscId
                    );
                    queueScriptSynced(std::format(
                        "openspace.setPropertyValueSingle('{}', {});",
                        propertyId, ghoul::to_string(color)
                    ));
                }
            };

            auto setTrailThicknessAndFade = [](const ExoplanetItem& p, float width, float fade) {
                const std::string id = planetIdentifier(p) + "_Trail";
                if (!renderable(id)) {
                    return;
                }
                std::string appearance =
                    std::format("Scene.{}.Renderable.Appearance", id);

                queueScriptSynced(std::format(
                    "openspace.setPropertyValueSingle('{0}.LineWidth', {1});"
                    "openspace.setPropertyValueSingle('{0}.Fade', {2});",
                    appearance, width, fade
                ));
            };

            auto resetTrailWidth = [&setTrailThicknessAndFade](const ExoplanetItem& p) {
                setTrailThicknessAndFade(p, 10.f, 1.f);
            };

            if (colorOrbits) {
                static ColorMappingView::ColorMappedVariable variable;
                bool colorEditChanged = _colorMappingView->renderColormapEdit(variable, makeIdentifier(host));
                if (colorOptionChanged || colorEditChanged) {
                    for (size_t planetIndex : planetIndices) {
                        const ExoplanetItem& p = _data[planetIndex];
                        if (colorOptionChanged) {
                            // First time we change color
                            setTrailThicknessAndFade(p, 20.f, 30.f);
                        }

                        glm::vec3 color = glm::vec3(_colorMappingView->colorFromColormap(p, variable));
                        colorTrail(p, color);
                    }
                }
            }
            else if (colorOptionChanged) {
                // Reset rendering
                for (size_t planetIndex : planetIndices) {
                    const ExoplanetItem& p = _data[planetIndex];
                    colorTrail(p, glm::vec3(1.f, 1.f, 1.f));
                    resetTrailWidth(p);
                }
            }

        }
        ImGui::EndGroup();
    }

    ImGui::Text("Planets:");

    // OBS! Push an overrided id to make the column settings sync across multiple
    // windows. This is not possible just using the same id in the BeginTable call,
    // since the id is connected to the ImGuiwindow instance
    ImGui::PushOverrideID(ImHashStr("systemTable"));
    renderTable("systemTable", planetIndices, true);
    ImGui::PopID();

    // Quickly set external selection to webpage
    if (ImGui::Button("Send planets to external webpage")) {
        updateFilteredRowsProperty(planetIndices);
    }
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Send just the planets in this planet system to the ExoplanetExplorer analysis "
        "webpage. Note that this overrides any other filtering. To bring back the filter "
        "selection, update the filtering in any way or press the next button."
    );
    ImGui::SameLine();
    if (ImGui::Button("Reset webpage to filtered")) {
        updateFilteredRowsProperty();
    }
}

void DataViewer::renderColumnValue(int columnIndex, const ExoplanetItem& item) {
    std::optional<const char*> format;

    if (_dataSettings.columnInfo.contains(_columns[columnIndex])) {
        const DataSettings::ColumnInfo& colInfo =
            _dataSettings.columnInfo.at(_columns[columnIndex]);

        if (!colInfo.format.empty()) {
            format = colInfo.format.c_str();
        }
    }

    std::variant<const char*, float> value = columnValue(_columns[columnIndex], item);

    if (std::holds_alternative<float>(value)) {
        float v = std::get<float>(value);
        if (std::isnan(v)) {
            ImGui::TextUnformatted("");
        }
        else {
            ImGui::Text(format.value_or("%.4f"), v);
        }
    }
    else if (std::holds_alternative<const char*>(value)) {
        ImGui::Text("%s", std::get<const char*>(value));
    }
}

void DataViewer::writeRenderDataToFile() {
    std::ofstream file(absPath(RenderDataFile), std::ios::binary);
    if (!file) {
        LERROR(std::format("Cannot open file '{}' for writing", RenderDataFile));
        return;
    }

    std::ofstream labelfile(absPath(LabelsFile));
    if (!labelfile) {
        LERROR(std::format("Cannot open file '{}' for writing", LabelsFile));
    }
    labelfile << "textcolor 1" << std::endl;

    LDEBUG("Writing render data to file");

    std::vector<size_t> indicesWithPositions;
    indicesWithPositions.reserve(_filteredData.size());

    // For now, only write the filtered data. Later on we might want to render the
    // filtered out points somehow and then we should write out the full dataset

    for (size_t index : _filteredData) {
        const ExoplanetItem& item = _data[index];
        if (item.position.has_value()) {
            indicesWithPositions.push_back(index);
        }
    }
    indicesWithPositions.shrink_to_fit();

    std::vector<std::string_view> hosts;
    hosts.reserve(_filteredData.size());

    // Write number of points
    size_t nPoints = indicesWithPositions.size();
    file.write(reinterpret_cast<const char*>(&nPoints), sizeof(size_t));

    for (size_t index : indicesWithPositions) {
        const ExoplanetItem& item = _data[index];

        file.write(reinterpret_cast<const char*>(&index), sizeof(size_t));

        size_t nVariables = _colorMappingView->colorMapperVariables().size();
        file.write(reinterpret_cast<const char*>(&nVariables), sizeof(size_t));

        const glm::dvec3 position = *item.position;
        file.write(reinterpret_cast<const char*>(&position.x), sizeof(double));
        file.write(reinterpret_cast<const char*>(&position.y), sizeof(double));
        file.write(reinterpret_cast<const char*>(&position.z), sizeof(double));

        for (int i = 0; i < nVariables; ++i) {
            const ImVec4 color = view::helper::toImVec4(
                _colorMappingView->colorFromColormap(
                    item,
                    _colorMappingView->colorMapperVariables()[i]
                )
            );
            file.write(reinterpret_cast<const char*>(&color.x), sizeof(float));
            file.write(reinterpret_cast<const char*>(&color.y), sizeof(float));
            file.write(reinterpret_cast<const char*>(&color.z), sizeof(float));
            file.write(reinterpret_cast<const char*>(&color.w), sizeof(float));
        }

        // Get a number for the planet's index in system
        file.write(reinterpret_cast<const char*>(&item.indexInSystem), sizeof(int));

        // Write label to file
        bool isAdded = std::find(hosts.begin(), hosts.end(), item.hostName) != hosts.end();
        if (!isAdded) {
            labelfile << std::format(
                "{} {} {} text {}",
                position.x, position.y, position.z, item.hostName
            );
            labelfile << std::endl;
            hosts.push_back(item.hostName);
        }
    }

    file.close();
}

void DataViewer::updateSelectionInRenderable() {
    const std::string indices = formatIndicesList(_selection);
    const std::string uri = std::format(
        "Scene.{}.Renderable.Selection",
        ExoplanetsExpertToolModule::GlyphCloudIdentifier
    );

    queueScriptSynced(
        "openspace.setPropertyValueSingle('" + uri + "', { " + indices + " })"
    );
}

void DataViewer::addOrTargetPlanet(const ExoplanetItem& item) {
    const std::string identifier = makeIdentifier(item.hostName);

    if (systemCanBeAdded(item.hostName)) {
        LINFO("Adding system. Click again to target");
        addExoplanetSystem(item.hostName);
    }
    else {
        // Ugly: Always set reach factors when targetting object;
        // we can't do it until the system is added to the scene
        setIncreasedReachfactors();

        queueScriptSynced(
            "openspace.setPropertyValueSingle("
                "'NavigationHandler.OrbitalNavigator.Anchor',"
                "'" + planetIdentifier(item) + "'"
            ");"
            "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');"
            "openspace.setPropertyValueSingle("
                "'NavigationHandler.OrbitalNavigator.RetargetAnchor', "
                "nil"
            ");"
        );
    }
}

bool DataViewer::systemCanBeAdded(const std::string& host) const {
    const std::string identifier = makeIdentifier(host);

    // Check if it does not already exist
    return sceneGraphNode(identifier) == nullptr;

    // TODO: also check against exoplanet list
}

void DataViewer::addExoplanetSystem(const std::string& host) const {
    queueScriptSynced("openspace.exoplanets.addExoplanetSystem('" + host + "')");
}

void DataViewer::refocusView() const {
    queueScriptSynced(
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth');"
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');"
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    );
}

void DataViewer::flyToOverview() const {
    // Create a linear path to Earth
    queueScriptSynced(
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
    queueScriptSynced(
        "openspace.pathnavigation.createPath({"
            "TargetType = 'Node', "
            "Target = 'Earth', "
            "Height = 5e+13, " // distance is what matters
            "Duration = 4, "
            "PathType = 'Linear'"
        "});"
    );
}

void DataViewer::flyToStar(std::string_view hostIdentifier) const {
    // Ugly: Always set reach factors when targetting object;
    // we can't do it until the system is added to the scene
    setIncreasedReachfactors();

    queueScriptSynced(
        "openspace.setPropertyValueSingle("
            "'NavigationHandler.OrbitalNavigator.Anchor',"
            "'" + std::string(hostIdentifier) +
        "')"
        "openspace.pathnavigation.zoomToDistanceRelative(100.0, 5.0);"
    );
}

} // namespace openspace::exoplanets
