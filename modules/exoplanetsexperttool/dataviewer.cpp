/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <modules/exoplanetsexperttool/exoplanetsexperttoolmodule.h>
#include <modules/exoplanetsexperttool/rendering/renderableexoplanetglyphcloud.h>
#include <modules/exoplanetsexperttool/rendering/renderablepointdata.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
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
        "https://weber.itn.liu.se/~emmbr26/ExoplanetExplorer/get_in_touch";

    bool caseInsensitiveLessThan(const char* lhs, const char* rhs) {
        int res = _stricmp(lhs, rhs);
        return res < 0;
    }

    bool compareValues(double lhs, double rhs) {
        if (std::isnan(lhs)) {
            // also includes rhs is nan, in which case the order does not matter
            return true;
        }

        // rhs is nan, but not lhs
        if (std::isnan(rhs)) {
            return false;
        }
        return lhs < rhs;
    }

    void setRenderableEnabled(std::string_view id, bool value) {
        openspace::global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('{}', {});",
                fmt::format("Scene.{}.Renderable.Enabled", id),
                value ? "true" : "false"
            ),
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    };

    // Set increased reach factors of all exoplanet renderables, to trigger fading out of
    // glyph cloud
    void setIncreasedReachfactors() {
        openspace::global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{exoplanet}.ApproachFactor', 15000000.0)"
            "openspace.setPropertyValue('{exoplanet_system}.ApproachFactor', 15000000.0)",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
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
        return createIdentifier(p.hostName) + "_" + p.component;
    }

    constexpr const glm::vec3 DefaultSelectedColor = { 0.2f, 0.8f, 1.f };
    constexpr const glm::vec4 DescriptiveTextColor = { 0.6f, 0.6f, 0.6f, 1.f };
    constexpr const glm::vec4 ErrorColor = { 1.f, 0.2f, 0.2f, 1.f };

    constexpr const glm::vec4 DisabledButtonColor = { 0.3f, 0.3f, 0.3f, 0.7f };

    const ImVec2 DefaultWindowSize = ImVec2(350, 350);

    constexpr const float DefaultGlyphSize = 22.f;

    // @TODO This can be implemented as a constructor in imconfig.h to enable conversion
    ImVec4 toImVec4(const glm::vec4& v) {
        return ImVec4(v.x, v.y, v.z, v.w);
    }

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

        return fmt::format(
            "{:0>2}:{:0>2}:{:0>2}.{:0<3}", t.wHour, t.wMinute, t.wSecond, t.wMilliseconds
        );
#else
        struct timeval t;
        gettimeofday(&t, nullptr);
        tm* m = gmtime(&t.tv_sec);

        return fmt::format(
            "{:0>2}:{:0>2}:{:0>2}.{:0<3}", m->tm_hour, m->tm_min, m->tm_sec, t.tv_usec / 1000
        );
#endif
    }

    // Format stirng for system window name
    std::string systemWindowName(std::string_view host) {
        return fmt::format("System: {}", host);
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

    _data = _dataLoader.loadData();

    _filteredData.reserve(_data.size());
    for (size_t i = 0; i < _data.size(); i++) {
        _filteredData.push_back(i);
        _hostIdToPlanetsMap[createIdentifier(_data[i].hostName)].push_back(i);
    }
    _filteredDataWithoutExternalSelection = _filteredData;

    _defaultColumns = {
        { "Name", ColumnID::Name },
        { "Host", ColumnID::Host },
        { "Year", ColumnID::DiscoveryYear, "%.0f", "Discovery year"},
        { "Planets", ColumnID::NPlanets, "%.0f" },
        { "Stars ", ColumnID::NStars, "%.0f" },
        { "ESM", ColumnID::ESM, "%.2f" },
        { "TSM", ColumnID::TSM, "%.2f" },
        {
            "Radius",
            ColumnID::PlanetRadius,
            "%.2f",
            "Planet radius in Earth radii"
        },
        {
            "Teq",
            ColumnID::PlanetTemperature,
            "%.0f",
            "Planet equilibrium temperature (K)"
        },
        { "Mass", ColumnID::PlanetMass, "%.2f", "in Earth masses" },
        { "Mass Err (%)", ColumnID::PlanetMassError, "%.2f", "Mass uncertainty"},
        { "Surface gravity", ColumnID::SurfaceGravity, "%.2f", "(m/s^2)"},
        // Orbits
        { "Semi-major axis", ColumnID::SemiMajorAxis, "%.2f", "(AU)"},
        { "Eccentricity", ColumnID::Eccentricity, "%.2f" },
        { "Orbit period", ColumnID::Period, "%.2f" },
        { "Inclination", ColumnID::Inclination, "%.2f" },
        // Star
        {
            "Star Teff",
            ColumnID::StarTemperature,
            "%.0f",
            "Star effective temperature (K)"
        },
        { "Star radius ", ColumnID::StarRadius, "%.2f", "(Solar)"},
        { "Star age", ColumnID::StarAge, "%.2f", "(Gyr)"},
        { "MagJ", ColumnID::MagnitudeJ, "%.2f" },
        { "MagK", ColumnID::MagnitudeK, "%.2f" },
        { "Distance ", ColumnID::Distance, "%.2f", "(pc)"},
        { "Ra", ColumnID::Ra, "%.2f" },
        { "Dec", ColumnID::Dec, "%.2f" },
        { "Metallicity (dex)", ColumnID::Metallicity, "%.2f" },
        { "Metallicity ratio", ColumnID::MetallicityRatio },
        // Discovery
        { "Discovery method", ColumnID::DiscoveryMethod },
        { "Telescope", ColumnID::DiscoveryTelescope },
        { "Instrument", ColumnID::DiscoveryInstrument },
        // Detected molecules
        {
            "Detected",
            ColumnID::MoleculesDetection,
            std::nullopt,
            "Detected molecules from IAC ExoAtmospheres dataset"
        },
        {
            "Upper limit",
            ColumnID::MoleculesUpperLimit,
            std::nullopt,
            "Upper limit detection of molecules from IAC ExoAtmospheres dataset"
        },
        {
            "No detection",
            ColumnID::MoleculesNoDetection,
            std::nullopt,
            "No detection molecules from IAC ExoAtmospheres dataset"
        },
        {
            "Water detection",
            ColumnID::WaterDetection,
            "%.0f",
            "Flag for whether water has been detected in the planet atmosphere "
            "(1 = yes, -1 = no, 0 = maybe/upper limit)"
        }
    };

    _columns = _defaultColumns;
    _selectedDefaultColumns.assign(_defaultColumns.size(), true);
    // Add other oclumns, if there are any. Assume all data items have the same columns
    if (_data.size() > 0 && _data.front().otherColumns.size() > 0) {
        for (auto col : _data.front().otherColumns) {
            Column c = { col.first, ColumnID::Other };
            _otherColumns.push_back(c);
            bool isSelected = false;
            _selectedOtherColumns.push_back(isSelected);
        }
    }

    // Must match names in implot and customly added ones
    _colormaps = {
        "Viridis",
        "Plasma",
        "Hot",
        "Cool",
        //"Autumn", // custom
        //"Spring", // custom
        //"Summer", // custom
        //"Winter", // custom
        "Jet",
        "Spectral",
        "RdBu",
        "BrBG",
        "PiYG",
        "Twilight",
        "Deep",
        "Dark",
        "Paired",
    };

    // TODO: make sure that settings are preserved between sessions?
    _variableSelection.push_back(ColorMappedVariable());
}

void DataViewer::initializeGL() {
    initializeRenderables();
    initializeCallbacks();

    //  TODO: These do not work when using multiple windows :(
    // Probably has to do with contexts

    //// Intilize custom color maps (generated from matplotlib)
    //const ImVec4 autumn[] = {
    //    ImVec4(1.f, 0.f,         0.f, 1.f),
    //    ImVec4(1.f, 0.14117647f, 0.f, 1.f),
    //    ImVec4(1.f, 0.28627451f, 0.f, 1.f),
    //    ImVec4(1.f, 0.42745098f, 0.f, 1.f),
    //    ImVec4(1.f, 0.57254902f, 0.f, 1.f),
    //    ImVec4(1.f, 0.71372549f, 0.f, 1.f),
    //    ImVec4(1.f, 0.85882353f, 0.f, 1.f),
    //    ImVec4(1.f, 1.f,         0.f, 1.f)
    //};

    //const ImVec4 spring[] = {
    //    ImVec4(1.f, 0.f,         1.f,         1.f),
    //    ImVec4(1.f, 0.14117647f, 0.85882353f, 1.f),
    //    ImVec4(1.f, 0.28627451f, 0.71372549f, 1.f),
    //    ImVec4(1.f, 0.42745098f, 0.57254902f, 1.f),
    //    ImVec4(1.f, 0.57254902f, 0.42745098f, 1.f),
    //    ImVec4(1.f, 0.71372549f, 0.28627451f, 1.f),
    //    ImVec4(1.f, 0.85882353f, 0.14117647f, 1.f),
    //    ImVec4(1.f, 1.f,         0.f,         1.f)
    //};

    //const ImVec4 summer[] = {
    //    ImVec4(0.f,         0.5f,        0.4f, 1.f),
    //    ImVec4(0.14117647f, 0.57058824f, 0.4f, 1.f),
    //    ImVec4(0.28627451f, 0.64313725f, 0.4f, 1.f),
    //    ImVec4(0.42745098f, 0.71372549f, 0.4f, 1.f),
    //    ImVec4(0.57254902f, 0.78627451f, 0.4f, 1.f),
    //    ImVec4(0.71372549f, 0.85686275f, 0.4f, 1.f),
    //    ImVec4(0.85882353f, 0.92941176f, 0.4f, 1.f),
    //    ImVec4(1.f,         1.f,         0.4f, 1.f)
    //};

    //const ImVec4 winter[] = {
    //    ImVec4(0.f, 0.f,         1.f,         1.f),
    //    ImVec4(0.f, 0.14117647f, 0.92941176f, 1.f),
    //    ImVec4(0.f, 0.28627451f, 0.85686275f, 1.f),
    //    ImVec4(0.f, 0.42745098f, 0.78627451f, 1.f),
    //    ImVec4(0.f, 0.57254902f, 0.71372549f, 1.f),
    //    ImVec4(0.f, 0.71372549f, 0.64313725f, 1.f),
    //    ImVec4(0.f, 0.85882353f, 0.57058824f, 1.f),
    //    ImVec4(0.f, 1.f,         0.5f,        1.f)
    //};

    //ImPlot::AddColormap("Autumn", autumn, 8, false);
    //ImPlot::AddColormap("Spring", spring, 8, false);
    //ImPlot::AddColormap("Summer", summer, 8, false);
    //ImPlot::AddColormap("Winter", winter, 8, false);
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
            system(fmt::format("start {}", GetInTouchLink).c_str());
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

    if (_useGlyphRendering) {
        renderable.setValue("Type", "RenderableExoplanetGlyphCloud"s);
        renderable.setValue("Size", 100.0);
        renderable.setValue("BillboardMinMaxSize", glm::dvec2(DefaultGlyphSize));
        renderable.setValue("UseFixedWidth", false);
        renderable.setValue("RenderBinMode", "PreDeferredTransparent"s);
    }
    else {
        renderable.setValue("Type", "RenderablePointData"s);
        renderable.setValue("Size", 10.0);
    }

    renderable.setValue("DataFile", dataFilePath.string());
    renderable.setValue("HighlightColor", glm::dvec3(DefaultSelectedColor));

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

    openspace::global::scriptEngine->queueScript(
        fmt::format("openspace.addSceneGraphNode({})", ghoul::formatLua(node)),
        scripting::ScriptEngine::RemoteScripting::Yes
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
    static bool showScatterPlotWindow = false;
    static bool showHelpers = false;

    auto mod = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    if (mod->showInfoWindowAtStartup() && _shouldOpenInfoWindow) {
        renderStartupInfo();
        return;
    }

    // Tooltip for hovered planets
    int hoveredPlanet = getHoveredPlanetIndex();
    renderPlanetTooltip(hoveredPlanet);

    if (ImGui::BeginMainMenuBar()) {
        if (ImGui::BeginMenu("Windows")) {
            ImGui::MenuItem("Table", NULL, &showTable);
            ImGui::MenuItem("Filters", NULL, &showFilterSettingsWindow);
            ImGui::MenuItem("Color mapping", NULL, &showColormapWindow);
            ImGui::MenuItem("Scatter plot", NULL, &showScatterPlotWindow);
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
            if (ImGui::Button("Zoom to overview")) {
                flyToOverview();
            }
            if (ImGui::Button("Zoom to inside view")) {
                flyToInsideView();
            }
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
                system(fmt::format("start chrome.exe {}", path).c_str());
            }
            if (ImGui::Button("Open in Firefox (Windows)")) {
                system(fmt::format("start firefox {}", path).c_str());
            }

            ImGui::EndMenu();
        }

        if (_currentlyTargettedSystem.has_value()) {
            std::string system = (*_currentlyTargettedSystem);
            if (ImGui::BeginMenu(fmt::format("System: {}", system.c_str()).c_str())) {
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
    _filterChanged = false;
    if (showFilterSettingsWindow) {
        renderFilterSettingsWindow(&showFilterSettingsWindow);
    }

    _colormapWasChanged = false;
    if (showColormapWindow) {
        renderColormapWindow(&showColormapWindow);
    }

    if (showScatterPlotWindow) {
        renderScatterPlotWindow(&showScatterPlotWindow);
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
    }
}

void DataViewer::renderHelpMarker(const char* text) {
    ImGui::TextDisabled("(?)");
    if (ImGui::IsItemHovered()) {
        ImGui::BeginTooltip();
        ImGui::PushTextWrapPos(ImGui::GetFontSize() * 35.0f);
        ImGui::TextUnformatted(text);
        ImGui::PopTextWrapPos();
        ImGui::EndTooltip();
    }
}

bool DataViewer::renderColormapEdit(ColorMappedVariable& variable,
                                    std::string_view relevantSystem)
{
    constexpr const int InputWidth = 120;
    bool wasChanged = false;

    ImGui::BeginGroup();
    {
        ImGui::SetNextItemWidth(InputWidth);
        if (ImGui::BeginCombo("Column", _columns[variable.columnIndex].name.c_str())) {
            for (int i = 0; i < _columns.size(); ++i) {
                // Ignore non-numeric columns
                if (!isNumericColumn(i)) {
                    continue;
                }

                const char* name = _columns[i].name.c_str();
                if (ImGui::Selectable(name, variable.columnIndex == i)) {
                    variable.columnIndex = i;
                    wasChanged = true;
                }
            }
            ImGui::EndCombo();
        }

        ImGui::SetNextItemWidth(InputWidth);
        if (ImGui::BeginCombo("Colormap", _colormaps[variable.colormapIndex])) {
            for (int i = 0; i < _colormaps.size(); ++i) {
                const char* name = _colormaps[i];
                ImPlot::ColormapIcon(ImPlot::GetColormapIndex(name));
                ImGui::SameLine();
                if (ImGui::Selectable(name, variable.colormapIndex == i)) {
                    variable.colormapIndex = i;
                    wasChanged = true;
                }
            }
            ImGui::EndCombo();
        }

        const int colormapColumn = variable.columnIndex;

        // Min/max values for color range
        ImGui::SetNextItemWidth(InputWidth);
        if (ImGui::DragFloatRange2("Min / Max", &variable.colorScaleMin, &variable.colorScaleMax, 1.f)) {
            wasChanged = true;
        }

        bool updateMinMax = false;

        std::vector<size_t> relevantIndices;

        if (!relevantSystem.empty() && ImGui::SmallButton("Set from planets in system")) {
            relevantIndices =
                _hostIdToPlanetsMap[createIdentifier(std::string(relevantSystem))];
            updateMinMax = true;
        }
        else if (ImGui::SmallButton("Set from current table data")) {
            relevantIndices = _filteredData;
            updateMinMax = true;
        }
        else if (ImGui::SmallButton("Set from full data")) {
            std::vector<size_t> v(_data.size()); // same number of indices as data
            std::iota(std::begin(v), std::end(v), 0);
            relevantIndices = std::move(v);
            updateMinMax = true;
        }

        if (updateMinMax && !relevantIndices.empty()) {
            float newMin = std::numeric_limits<float>::max();
            float newMax = std::numeric_limits<float>::lowest();

            for (size_t i : relevantIndices) {
                const ExoplanetItem& item = _data[i];
                auto value = valueFromColumn(colormapColumn, item);
                if (!std::holds_alternative<float>(value)) {
                    // Shouldn't be possible to try to use non numbers
                    throw;
                }

                float val = std::get<float>(value);
                if (std::isnan(val)) {
                    continue;
                }
                newMax = std::max(val, newMax);
                newMin = std::min(val, newMin);
            }

            variable.colorScaleMin = newMin;
            variable.colorScaleMax = newMax;
            wasChanged = true;
        };

        ImGui::EndGroup();
    }

    constexpr const int ColorScaleHeight = 140;

    // Render visuals for colormap
    ImGui::SameLine();
    ImPlot::PushColormap(_colormaps[variable.colormapIndex]);
    ImPlot::ColormapScale(
        "##ColorScale",
        variable.colorScaleMin,
        variable.colorScaleMax,
        ImVec2(0, ColorScaleHeight)
    );
    ImPlot::PopColormap();

    return wasChanged;
}

void DataViewer::renderColormapWindow(bool* open) {
    ImGui::SetNextWindowSize(ImVec2(350, 450), ImGuiCond_FirstUseEver);
    if (!ImGui::Begin("Color mapping", open)) {
        ImGui::End();
        return;
    }

    // Start variable group
    ImGui::BeginGroup();

    ImGui::BeginGroup();

    // Colormap for each selected variable
    if (ImGui::Button("+ Add variable")) {
        if (_variableSelection.size() < 8) {
            _variableSelection.push_back(ColorMappedVariable());
            _colormapWasChanged = true;
        }
    };
    ImGui::SameLine();

    // NaNColor
    ImGuiColorEditFlags nanColorFlags = ImGuiColorEditFlags_NoInputs |
        ImGuiColorEditFlags_NoLabel | ImGuiColorEditFlags_AlphaPreview | ImGuiColorEditFlags_AlphaBar;
    static ImVec4 c = toImVec4(_nanPointColor);
    if (ImGui::ColorEdit4("NanColor", (float*)&c, nanColorFlags)) {
        _nanPointColor = { c.x, c.y, c.z, c.w };
        _colormapWasChanged = true;
    }
    ImGui::SameLine();
    ImGui::Text("No value color");

    ImGui::EndGroup();

    ImGui::Spacing();

    ImGui::BeginGroup();

    // Note the reverse ordering
    for (int index = static_cast<int>(_variableSelection.size()) - 1; index >= 0; --index) {
        ColorMappedVariable& variable = _variableSelection[index];

        ImGui::PushID(fmt::format("##variable{}", index).c_str());

        ImGui::Text(fmt::format("{}.", index + 1).c_str());
        ImGui::SameLine();

        // Entire variable group
        _colormapWasChanged |= renderColormapEdit(variable);

        ImGui::PopID();
        ImGui::SameLine();

        ImGui::PushID(fmt::format("##remove{}", index).c_str());
        if (_variableSelection.size() > 1 && ImGui::Button("x")) {
            _variableSelection.erase(_variableSelection.begin() + index);
            _colormapWasChanged = true;
        }
        ImGui::PopID();

        // Some spacing before the next group
        ImGui::Spacing();
    }

    ImGui::EndGroup(); // all variable groups

    // Circle plot to show which parameters map to which part of a glyph
    ImGui::SameLine();
    {
        int nVariables = static_cast<int>(_variableSelection.size());
        std::vector<float> data(nVariables, 1.f / static_cast<float>(nVariables));

        // First build array with real strings. Note that this has to stay alive for
        // the netire lifetime of the char * array
        std::vector<std::string> labelStrings;
        labelStrings.reserve(nVariables);
        for (int i = 0; i < nVariables; ++i) {
            std::string label = _columns[_variableSelection[i].columnIndex].name;
            label = label.substr(0, 10); // limit length
            labelStrings.push_back(fmt::format(" {}. {}", i+1, label));
        }

        // Then build array with const char * from that array
        std::vector<const char*> labels;
        labels.reserve(nVariables);
        for (int i = 0; i < nVariables; ++i) {
            labels.push_back(labelStrings[i].data());
        }

        // Reverse vector to get the order its actually rendered
        std::reverse(labels.begin(), labels.end());

        constexpr const int ColorScaleHeight = 140;
        ImVec2 plotSize = ImVec2(1.5 * ColorScaleHeight, ColorScaleHeight);
        ImPlot::SetNextPlotLimits(0, 1.5, 0, 1, ImGuiCond_Always);
        if (ImPlot::BeginPlot("##Pie", NULL, NULL, plotSize, ImPlotFlags_Equal | ImPlotFlags_NoMousePos, ImPlotAxisFlags_NoDecorations, ImPlotAxisFlags_NoDecorations)) {
            ImPlot::PlotPieChart(labels.data(), data.data(), nVariables, 1.1, 0.5, 0.3, true, NULL);
            ImPlot::EndPlot();
        }
    }

    ImGui::EndGroup(); // variables + plot group

    ImGui::End();
}

void DataViewer::renderScatterPlotWindow(bool* open) {
    ImGui::SetNextWindowSize(ImVec2(450, 400), ImGuiCond_FirstUseEver);
    if (!ImGui::Begin("Scatter plots", open)) {
        ImGui::End();
        return;
    }

    std::vector<float> ra, dec;
    ra.reserve(_filteredData.size());
    dec.reserve(_filteredData.size());

    for (size_t i : _filteredData) {
        const ExoplanetItem& item = _data[i];
        if (item.ra.hasValue() && item.dec.hasValue()) {
            ra.push_back(item.ra.value);
            dec.push_back(item.dec.value);
        }
    }

    std::vector<float> ra_selected, dec_selected;
    ra_selected.reserve(_selection.size());
    dec_selected.reserve(_selection.size());

    for (size_t i : _selection) {
        const ExoplanetItem& item = _data[i];
        if (item.ra.hasValue() && item.dec.hasValue()) {
            ra_selected.push_back(item.ra.value);
            dec_selected.push_back(item.dec.value);
        }
    }

    // Ra dec plot
    ImGui::BeginGroup();

    ImVec4 selectedColor =
    { DefaultSelectedColor.x, DefaultSelectedColor.y, DefaultSelectedColor.z, 1.f };

    ImGui::Spacing();

    const ColorMappedVariable& first = _variableSelection.front();

    // Scatterplot
    static const ImVec2 plotSize = { 400, 300 };
    ImPlotFlags plotFlags = ImPlotFlags_NoLegend;
    ImPlotAxisFlags axisFlags = ImPlotAxisFlags_None;

    static float pointSize = 1.5f;
    ImPlot::PushColormap(_colormaps[first.colormapIndex]);
    ImPlot::SetNextPlotLimits(0.0, 360.0, -90.0, 90.0, ImGuiCond_Always);
    if (ImPlot::BeginPlot("Star Coordinate", "Ra", "Dec", plotSize, plotFlags, axisFlags)) {
        ImPlot::PushStyleVar(ImPlotStyleVar_MarkerSize, pointSize);

        for (size_t i : _filteredData) {
            const ExoplanetItem& item = _data[i];

            if (!item.ra.hasValue() || !item.dec.hasValue()) {
                continue;
            }

            const ImVec4 pointColor = toImVec4(colorFromColormap(
                item,
                _variableSelection.front())
            ); // from first map

            const ImPlotPoint point = { item.ra.value, item.dec.value };
            const char* label = "Data " + i;
            ImPlot::PushStyleColor(ImPlotCol_MarkerFill, pointColor);
            ImPlot::PushStyleColor(ImPlotCol_MarkerOutline, pointColor);
            ImPlot::PlotScatter(label, &point.x, &point.y, 1);
            ImPlot::PopStyleColor();
            ImPlot::PopStyleColor();
        }
        ImPlot::PopStyleVar();

        ImPlot::PushStyleVar(ImPlotStyleVar_MarkerSize, 3.f * pointSize);
        ImPlot::PushStyleColor(ImPlotCol_MarkerFill, selectedColor);
        ImPlot::PushStyleColor(ImPlotCol_MarkerOutline, selectedColor);
        ImPlot::PlotScatter(
            "Selected",
            ra_selected.data(),
            dec_selected.data(),
            static_cast<int>(ra_selected.size())
        );
        ImPlot::PopStyleColor();
        ImPlot::PopStyleColor();
        ImPlot::PopStyleVar();
        ImPlot::EndPlot();

        ImGui::SameLine();
        ImPlot::ColormapScale(
            "##ColorScale",
            first.colorScaleMin,
            first.colorScaleMax,
            ImVec2(60, plotSize.y)
        );

        ImGui::SetNextItemWidth(70);
        ImGui::DragFloat("Point size", &pointSize, 0.1f, 0.f, 5.f);
        ImPlot::PopColormap();
    }
    ImGui::EndGroup(); // ra dec plot group

    ImGui::End();
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
    ImGui::TextColored(
        toImVec4(DescriptiveTextColor),
        fmt::format("({})", _pinnedPlanets.size()).c_str()
    );
    if (showPinnedTable) {
        renderTable("pinned_exoplanets_table", _pinnedPlanets, true);
    }

    ImGui::Separator();
    ImGui::TextColored(
        toImVec4(DescriptiveTextColor),
        fmt::format(
            "Showing {} exoplanets out of a total {} ",
            _filteredData.size(), _data.size()
        ).c_str()
    );

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

    bool selectionChanged = false;

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
            const Column c = _columns[colIdx];
            if (c.id == ColumnID::Name) {
                colFlags |= ImGuiTableColumnFlags_DefaultSort;
            }
            ImGui::TableSetupColumn(c.name.c_str(), colFlags, 0.f, colIdx);
        }

        // Make header and first column (name) always visible
        ImGui::TableSetupScrollFreeze(2, 1);

        // Instead of calling TableHeadersRow(), we set up custom headers with help markers
        //ImGui::TableHeadersRow();
        ImGui::TableNextRow(ImGuiTableRowFlags_Headers);

        ImGui::TableHeader("");

        for (int colIdx = 0; colIdx < _columns.size(); colIdx++) {
            const Column c = _columns[colIdx];
            ImGui::TableSetColumnIndex(colIdx + 1);
            ImGui::PushID(colIdx);
            ImGui::TableHeader(c.name.c_str());

            if (colIdx >= 0) {
                if (c.description.has_value()) {
                    const float TEXT_WIDTH = ImGui::CalcTextSize(c.name.c_str()).x;
                    ImGui::SameLine(0.0f, TEXT_WIDTH + 2.f);
                    renderHelpMarker(*c.description);
                }
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

                    int col = static_cast<int>(sortSpecs->Specs->ColumnUserID);

                    return compareColumnValues(col, l, r);
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
                ).passFilter(_data[r].planetName);

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
                    ImGui::PushID(fmt::format("addbutton{}", row).c_str());
                    if (ImGui::Button("+", ImVec2(20, RowHeight))) {
                        addExoplanetSystem(item.hostName);
                    }
                    ImGui::PopID();
                }
                else {
                    // Add a target button instead
                    ImGui::PushID(fmt::format("targetbutton{}", row).c_str());

                    // Check if is target item. The GUI name should be set from the planet name
                    const SceneGraphNode* node = global::navigationHandler->anchorNode();
                    bool isCurrentAnchor = node && node->guiName() == item.planetName;
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
                    const Column col = _columns[colIdx];
                    ImGui::TableNextColumn();

                    if (col.id == ColumnID::Name) {
                        bool changed = ImGui::Selectable(
                            item.planetName.c_str(),
                            itemIsSelected,
                            selectableFlags
                        );

                        // Context menu
                        ImGui::PushID(fmt::format("context-{}", item.planetName).c_str());
                        if (ImGui::BeginPopupContextItem("item context menu")) {
                            ImGui::Text(item.planetName.c_str());

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
                            ImGui::PushID(fmt::format("Planetreflink-{}", item.planetName).c_str());

                            if (ImGui::Button("Link")) {
                                system(fmt::format("start {}", item.referenceUrl).c_str());
                            }
                            ImGui::PopID();

                            ImGui::Separator();

                            ImGui::PushID(fmt::format("ShowShystemView-{}", item.planetName).c_str());

                            bool isAlreadyOpen = std::find(
                                _shownPlanetSystemWindows.begin(),
                                _shownPlanetSystemWindows.end(),
                                item.hostName
                            ) != _shownPlanetSystemWindows.end();

                            if (!isAlreadyOpen) {
                                if (ImGui::Button("Show system view")) {
                                    _shownPlanetSystemWindows.push_back(item.hostName);
                                    ImGui::CloseCurrentPopup();
                                }
                            }
                            else {
                                ImGui::TextDisabled("A system view is already opened for this system");
                            }

                            ImGui::EndPopup();
                            ImGui::PopID();
                        }
                        ImGui::PopID();

                        // Check double click, left mouse button
                        if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(0)) {
                            LINFO(fmt::format("Double click: {}", item.planetName));
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

                            selectionChanged = true;
                        }
                        continue;
                    }

                    renderColumnValue(colIdx, col.format, item);
                }
            }
        }
        ImGui::EndTable();

        if (selectionChanged) {
            updateSelectionInRenderable();
        }
    }
}

void DataViewer::renderFilterSettingsWindow(bool* open) {
    ImGui::SetNextWindowSize(ImVec2(430, 450), ImGuiCond_FirstUseEver);
    if (!ImGui::Begin("Filters", open)) {
        ImGui::End();
        return;
    }

    // Some pre-defined filters
    static bool hideNanTsm = false;
    static bool hideNanEsm = false;
    static bool showOnlyMultiPlanetSystems = false;
    static bool showOnlyHasPosition = false;

    // Planet bins
    static bool showTerrestrial = false;
    static bool showSmallSubNeptunes = false;
    static bool showLargeSubNeptunes = false;
    static bool showSubJovians = false;
    static bool showLargerPlanets = false;

    // Discovery methods
    static bool showTransit = true;
    static bool showRadialVelocity = true;
    static bool showOther = true;

    // Row limit
    static int nRows = 100;
    static bool limitNumberOfRows = false;
    static int nItemsWithoutRowLimit = static_cast<int>(_filteredData.size());

    if (ImGui::Button("Reset internal")) {
        // Reset to default values

        _appliedFilters.clear(); // Column filters

        hideNanTsm = false;
        hideNanEsm = false;
        showOnlyMultiPlanetSystems = false;
        showOnlyHasPosition = false;

        // Planet bins
        showTerrestrial = false;
        showSmallSubNeptunes = false;
        showLargeSubNeptunes = false;
        showSubJovians = false;
        showLargerPlanets = false;

        // Discovery methods
        showTransit = true;
        showRadialVelocity = true;
        showOther = true;

        _filterChanged = true;
    }

    ImGui::SameLine();
    if (ImGui::Button("Reset row limit")) {
        limitNumberOfRows = false;
        _filterChanged = true;
    }

    ImGui::SameLine();
    if (ImGui::Button("Reset external")) {
        _externalSelection = {};
        _filterChanged = true;
    }

    // Internal filters group
    bool showInternalFiltersSection =
        ImGui::CollapsingHeader("Internal filters", ImGuiTreeNodeFlags_DefaultOpen);
    ImGui::SameLine();
    renderHelpMarker("Filter the data internally, within the OpenSpace application");

    if (showInternalFiltersSection) {
        // Per-column filtering
        {
            static int filterColIndex = 0;

            ImGui::Separator();
            ImGui::Text("Filter on column");
            ImGui::SetNextItemWidth(120);
            if (ImGui::BeginCombo("##Column", _columns[filterColIndex].name.c_str())) {
                for (int i = 0; i < _columns.size(); ++i) {
                    if (ImGui::Selectable(_columns[i].name.c_str(), filterColIndex == i)) {
                        filterColIndex = i;
                    }
                }
                ImGui::EndCombo();
            }

            ImGui::SameLine();

            static char queryString[128] = "";

            bool numeric = isNumericColumn(filterColIndex);

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
            renderHelpMarker(numeric ?
                ColumnFilter::NumericFilterDescription :
                ColumnFilter::TextFilterDescription
            );

            if (ImGui::Button("Add filter") || inputEntered) {
                ColumnFilter filter = numeric ?
                    ColumnFilter(queryString, ColumnFilter::Type::Numeric) :
                    ColumnFilter(queryString, ColumnFilter::Type::Text);

                if (filter.isValid()) {
                    _appliedFilters.push_back({ filterColIndex , filter });
                    strcpy(queryString, "");
                    _filterChanged = true;
                }
            }

            // Clear the text field
            ImGui::SameLine();
            if (ImGui::Button("Clear text field")) {
                strcpy(queryString, "");
            }

        }

        ImGui::Spacing();

        // Render list of column filters
        {
            const std::string filtersHeader = _appliedFilters.empty() ?
                "Added filters" :
                fmt::format("Added filters ({})", _appliedFilters.size());

            // The ### operator overrides the ID, ignoring the preceding label
            // => Won't rerender when label changes
            const std::string headerWithId = fmt::format("{}###FiltersHeader", filtersHeader);

            if (ImGui::CollapsingHeader(headerWithId.c_str(), ImGuiTreeNodeFlags_DefaultOpen)) {
                ImGui::Indent();

                if (_appliedFilters.empty()) {
                    ImGui::Text("No active filters");
                }

                int indexToErase = -1;
                constexpr const int nColumns = 5;

                const ImGuiTableFlags flags = ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_RowBg;

                if (ImGui::BeginTable("filtersTable", nColumns, flags)) {
                    for (int i = 0; i < _appliedFilters.size(); ++i) {
                        ColumnFilterEntry &f = _appliedFilters[i];
                        const std::string queryString = f.filter.query();
                        ImGui::TableNextRow();

                        ImGui::PushID(fmt::format("FilterColEnabled-{}", i).c_str());
                        ImGui::TableNextColumn();
                        if (ImGui::Checkbox("##Enabled", &f.enabled)) {
                            _filterChanged = true;
                        }
                        ImGui::PopID();

                        ImGui::TableNextColumn();
                        ImGui::Text(_columns[f.columnIndex].name.c_str());

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
                        _appliedFilters.erase(_appliedFilters.begin() + indexToErase);
                        _filterChanged = true;
                    }

                    ImGui::EndTable();
                }
                ImGui::Unindent();
            }
        }

        ImGui::Separator();
        ImGui::Spacing();

        // Pre-defined filters
        {
            _filterChanged |= ImGui::Checkbox("Hide null TSM", &hideNanTsm);
            ImGui::SameLine();
            _filterChanged |= ImGui::Checkbox("Hide null ESM", &hideNanEsm);

            _filterChanged |= ImGui::Checkbox("Only multi-planet", &showOnlyMultiPlanetSystems);
            ImGui::SameLine();
            _filterChanged |= ImGui::Checkbox("Must have 3D positional data", &showOnlyHasPosition);
            ImGui::SameLine();
            renderHelpMarker(
                "Only include data points that will show up in OpenSpace's 3D rendered view"
            );

            ImGui::Text("Planet bin");
            _filterChanged |= ImGui::Checkbox("Terrestrial", &showTerrestrial);
            ImGui::SameLine();
            renderHelpMarker("Rp < 1.5  (Earth radii)");

            _filterChanged |= ImGui::Checkbox("Small Sub-Neptune", &showSmallSubNeptunes);
            ImGui::SameLine();
            renderHelpMarker("1.5 < Rp < 2.75  (Earth radii)");

            ImGui::SameLine();
            _filterChanged |= ImGui::Checkbox("Large Sub-Neptune", &showLargeSubNeptunes);
            ImGui::SameLine();
            renderHelpMarker("2.75 < Rp < 4.0  (Earth radii)");

            _filterChanged |= ImGui::Checkbox("Sub-Jovian", &showSubJovians);
            ImGui::SameLine();
            renderHelpMarker("4.0 < Rp < 10)  (Earth radii)");

            ImGui::SameLine();
            _filterChanged |= ImGui::Checkbox("Larger", &showLargerPlanets);
            ImGui::SameLine();
            renderHelpMarker("Rp > 10  (Earth radii)");

            ImGui::Text("Discovery method");
            _filterChanged |= ImGui::Checkbox("Transit", &showTransit);
            ImGui::SameLine();
            _filterChanged |= ImGui::Checkbox("Radial Velocity", &showRadialVelocity);
            ImGui::SameLine();
            _filterChanged |= ImGui::Checkbox("Other", &showOther);
        }
    }

    ImGui::Spacing();
    ImGui::Separator();

    // Row limit group
    bool showRowLimitSection = ImGui::CollapsingHeader("Row limit");
    ImGui::SameLine();
    renderHelpMarker(
        "Limit the number of filtered rows (internal) based on which "
        "have the highest TSM/ESM value"
    );

    static bool overrideInternalSelection = false;
    bool rowLimitFilterChanged = false;
    ColumnID rowLimitCol = ColumnID::TSM;
    if (showRowLimitSection) {
        // Number of rows with max TSM/ESM filter
        {
            ImGui::Text("Limit number of rows");
            ImGui::SameLine();
            renderHelpMarker(
                "Enable to only show the top X resulting rows with max ESM/TSM value"
            );

            rowLimitFilterChanged |= ImGui::Checkbox("##RowLimit", &limitNumberOfRows);
            ImGui::SameLine();
            ImGui::Text("Show first");
            ImGui::SameLine();
            ImGui::SetNextItemWidth(100);
            rowLimitFilterChanged |= ImGui::InputInt("##nRows", &nRows);
            ImGui::SameLine();
            ImGui::Text(" rows with highest ");
            ImGui::SameLine();

            const char* metricChoices[] = { "TSM", "ESM" };
            static int currentMetricChoiceIndex = 0;
            ImGui::SetNextItemWidth(70);
            rowLimitFilterChanged |= ImGui::Combo(
                "##ESMorTSMcombo",
                &currentMetricChoiceIndex,
                metricChoices,
                IM_ARRAYSIZE(metricChoices)
            );
            _filterChanged |= rowLimitFilterChanged;

            const char* metricChoice = metricChoices[currentMetricChoiceIndex];
            rowLimitCol = (metricChoice == "TSM") ? ColumnID::TSM : ColumnID::ESM;
        }
    }

    ImGui::Spacing();
    ImGui::Separator();

    // External filter
    bool showExternalFiltersSection = ImGui::CollapsingHeader("External filters");
    ImGui::SameLine();
    renderHelpMarker(
        "Control filtering/selection coming from the external webpage. \n \n"
        "Note that it is ignored by default. Set the 'Use selection from webpage' "
        "to true to apply the selection. "
    );

    bool externalSelectionSettingsChanged = false;
    if (showExternalFiltersSection) {
        // Filter from webpage
        {
            if (ImGui::Checkbox("Use selection from website", &_useExternalSelection)) {
                externalSelectionSettingsChanged = true;
            }
            if (!_externalSelection.value().empty()) {
                ImGui::SameLine(ImGui::GetWindowContentRegionMax().x - 100);
                if (ImGui::Button("Delete", ImVec2(100, 0))) {
                    LINFO("Deleted external selection");
                    _externalSelection = {};
                    externalSelectionSettingsChanged = true;
                }
            }

            ImGui::Text("Selection: ");
            if (!_externalSelection.value().empty()) {
                ImGui::SameLine();
                ImGui::TextColored(
                    ImColor(200, 200, 200),
                    fmt::format("{} items", _externalSelection.value().size()).c_str()
                );
            }

            ImGui::Text("Last updated: ");
            ImGui::SameLine();
            ImGui::TextColored(ImColor(200, 200, 200), _lastExternalSelectionTimeStamp.c_str());

            if (ImGui::Checkbox("Override internal selection", &overrideInternalSelection)) {
                externalSelectionSettingsChanged = true;
            }
            ImGui::SameLine();
            renderHelpMarker(
                "If set to true, only the selection from the webpage will be shown. "
                "Meaning that the above internal filtering will be ignored."
            );
        }

        _filterChanged |= externalSelectionSettingsChanged;

        // Also check if a new selection was sent from the webpage
        _filterChanged |= (_useExternalSelection && _externalSelectionChanged);
    }

    ImGui::Separator();

    // Filter the data
    // Update the filtered data
    if (_filterChanged) {
        _filteredData.clear();
        _filteredData.reserve(_data.size());

        for (int i = 0; i < _data.size(); i++) {
            const ExoplanetItem& d = _data[i];

            // Pre-defined filters
            bool filteredOut = hideNanTsm && std::isnan(d.tsm);
            filteredOut |= hideNanEsm && std::isnan(d.esm);
            filteredOut |= showOnlyMultiPlanetSystems && !d.multiSystemFlag;
            filteredOut |= showOnlyHasPosition && !d.position.has_value();

            bool hasBinFilter = showTerrestrial || showSmallSubNeptunes ||
                                showLargeSubNeptunes || showSubJovians ||
                                showLargerPlanets;

            if (hasBinFilter) {
                bool matchesBinFilter = false;
                if (d.radius.hasValue()) {
                    float r = d.radius.value;
                    // TODO: make it possible to set these values
                    matchesBinFilter |= showTerrestrial && (r <= 1.5);
                    matchesBinFilter |= showSmallSubNeptunes && (r > 1.5 && r <= 2.75);
                    matchesBinFilter |= showLargeSubNeptunes && (r > 2.75 && r <= 4.0);
                    matchesBinFilter |= showSubJovians && (r > 4.0 && r <= 10.0);
                    matchesBinFilter |= showLargerPlanets && (r > 10.0);
                }
                filteredOut |= !matchesBinFilter;
            }

            // Shortcut filter for discovery method
            bool passDiscoveryMethod = false;

            bool isTransit = ColumnFilter(
                "transit",
                ColumnFilter::Type::Text
            ).passFilter(d.discoveryMethod);

            bool isRV = ColumnFilter(
                "radial velocity",
                ColumnFilter::Type::Text
            ).passFilter(d.discoveryMethod);

            if (showOther && !(isTransit || isRV)) {
                passDiscoveryMethod = true;
            }
            else {
                passDiscoveryMethod |= (showTransit && isTransit);
                passDiscoveryMethod |= (showRadialVelocity && isRV);
            }
            filteredOut |= !passDiscoveryMethod;

            // Other filters
            for (const ColumnFilterEntry& f : _appliedFilters) {
                if (!f.enabled) {
                    continue;
                }

                std::variant<const char*, float> value =
                    valueFromColumn(f.columnIndex, d);

                if (std::holds_alternative<float>(value)) {
                    float val = std::get<float>(value);
                    filteredOut |= !f.filter.passFilter(val);
                }
                else { // text
                    const char* val = std::get<const char*>(value);
                    filteredOut |= !f.filter.passFilter(val);
                }
            }

            if (!filteredOut) {
                _filteredData.push_back(i);
            }
        }
        _filteredData.shrink_to_fit();

        nItemsWithoutRowLimit = static_cast<int>(_filteredData.size());
    }

    // Show how many values the filter corresponds to, without the limited rows
    ImGui::TextColored(
        toImVec4(DescriptiveTextColor),
        fmt::format(
            "Current internal filter corresponds to {} exoplanets and \n"
            "has {} active column filters \n", nItemsWithoutRowLimit, _appliedFilters.size()
        ).c_str()
    );

    // Limit the number of rows by first sorting based on the chosen metric
    static int nRowsAfterLimit = 0;
    if (limitNumberOfRows && _filteredData.size() > nRows) {
        auto compare = [&rowLimitCol, this](const size_t& lhs, const size_t& rhs) {
            // We are interested in the largest, so flip the order
            const ExoplanetItem& l = _data[rhs];
            const ExoplanetItem& r = _data[lhs];
            return compareColumnValues(columnIndexFromId(rowLimitCol), l, r);
        };

        std::sort(_filteredData.begin(), _filteredData.end(), compare);
        _filteredData.erase(_filteredData.begin() + nRows, _filteredData.end());
        nRowsAfterLimit = static_cast<int>(_filteredData.size());
    }

    if (limitNumberOfRows) {
        ImGui::TextColored(
            toImVec4(DescriptiveTextColor),
            fmt::format("After row limit : {}", nRowsAfterLimit).c_str()
        );
    }

    // OBS! This is a little nasty. Should be some better way to do it
    bool shouldUpdateBasedOnExternalSelection = false;
    shouldUpdateBasedOnExternalSelection |= externalSelectionSettingsChanged;
    shouldUpdateBasedOnExternalSelection |= _externalSelectionChanged;
    shouldUpdateBasedOnExternalSelection |= (_filterChanged && _useExternalSelection);
    shouldUpdateBasedOnExternalSelection &= (_useExternalSelection && !_externalSelection.value().empty());

    if (shouldUpdateBasedOnExternalSelection) {
        if (overrideInternalSelection) {
            // Just use the external seleciton, out of the box
            std::vector<size_t> newFilteredData;
            newFilteredData.reserve(_externalSelection.value().size());

            for (int i : _externalSelection.value()) {
                newFilteredData.push_back(static_cast<size_t>(i));
            }

            _filterChanged = true; // Update filter changed flag, to always trigger resorting
            _filteredData = std::move(newFilteredData);
        }
        else {
            // Do an intersection, i.e. check if the filtered out items are in the selection
            std::vector<size_t> newFilteredData;
            newFilteredData.reserve(_filteredData.size());
            std::vector<int> searchList = _externalSelection.value();

            for (size_t index : _filteredData) {
                bool isFound = std::find(
                    searchList.begin(),
                    searchList.end(),
                    static_cast<int>(index)
                ) != searchList.end();
                if (isFound) {
                    newFilteredData.push_back(index);
                }
            }
            newFilteredData.shrink_to_fit();
            _filteredData = std::move(newFilteredData);
        }
    }

    if (_useExternalSelection && !_externalSelection.value().empty()) {
        ImGui::TextColored(
            toImVec4(DescriptiveTextColor),
            fmt::format("After applying external filtering: {}", _filteredData.size()).c_str()
        );
    }

    ImGui::End(); // Filter settings window

    updateFilteredRowsProperty();

    // Handle selection
    if (_filterChanged) {
        _selection.clear();
        updateSelectionInRenderable();
    }

    // Reset some state changed variables
    _externalSelectionChanged = false;
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

    if (ImGui::Begin("##planetToolTip", NULL, flags)) {
        ImGui::Text(_data[index].planetName.c_str());
    }

    // TODO: Also handle mouseclick
}

void DataViewer::updateFilteredRowsProperty() {
    auto mod = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    properties::Property* filteredRowsProperty = mod->property("FilteredDataRows");
    if (filteredRowsProperty) {
        std::vector<int> indices;
        indices.reserve(_filteredData.size());
        std::transform(
            _filteredData.begin(), _filteredData.end(), std::back_inserter(indices),
            [](size_t i) { return static_cast<int>(i); }
        );

        // TODO: should set this over Lua script API instead
        filteredRowsProperty->set(indices);
    }
}

void DataViewer::renderSettingsMenuContent() {
    // OBS! These should match the default settings for the SGNs
    static bool useFixedWidth = false;
    static bool showKepler = true;
    static bool showMilkyWayLine = true;

    renderColumnSettingsModal();

    if (ImGui::Checkbox("Use fixed ring width", &useFixedWidth)) {
        openspace::global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('{}', {})",
                fmt::format("Scene.{}.Renderable.UseFixedWidth",
                    ExoplanetsExpertToolModule::GlyphCloudIdentifier
                ),
                useFixedWidth
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    if (ImGui::Checkbox("Show Kepler FOV cue", &showKepler)) {
        openspace::global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('{}', {})",
                "Scene.KeplerPrism.Renderable.Enabled",
                showKepler
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    if (ImGui::Checkbox("Show line to Milky Way center", &showMilkyWayLine)) {
        openspace::global::scriptEngine->queueScript(
            fmt::format(
                "openspace.setPropertyValueSingle('{}', {})",
                "Scene.MilkyWayEarthLine.Renderable.Enabled",
                showMilkyWayLine
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
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
            openspace::global::scriptEngine->queueScript(
                fmt::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.BillboardMinMaxSize', {})",
                    ExoplanetsExpertToolModule::GlyphCloudIdentifier,
                    glm::dvec2(DefaultGlyphSize * glyphSizeScale
                )),
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }
}

void DataViewer::renderColumnSettingsModal() {
    if (ImGui::Button("Set up columns...")) {
        ImGui::OpenPopup("Set columns");
    }

    constexpr const int nPerColumn = 20;

    std::deque<bool> prevSelectedDefault = _selectedDefaultColumns;
    std::deque<bool> prevSelectedOther = _selectedOtherColumns;
    auto resetSelection = [this, &prevSelectedDefault, &prevSelectedOther]() {
        _selectedDefaultColumns = prevSelectedDefault;
        _selectedOtherColumns = prevSelectedOther;
    };

    // Always center this window when appearing
    ImVec2 center = ImGui::GetMainViewport()->GetCenter();
    ImGui::SetNextWindowPos(center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));

    int nSelected = 0;
    bool canSelectMore = true;
    ImGuiWindowFlags flags = ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_HorizontalScrollbar;

    if (ImGui::BeginPopupModal("Set columns", NULL, flags)) {
        ImGui::Text(
            "Select up to 64 columns to show in the tool. OBS! For now, only numerical "
            "values in the \"other columns\" will be read correctly."
        );
        ImGui::Separator();

        // Default columns
        ImGui::BeginGroup();
        {
            ImGui::Text("Default columns:");
            ImGui::SameLine();

            ImGui::PushID("clear_default");
            if (ImGui::Button("Clear selection")) {
                _selectedDefaultColumns.assign(_defaultColumns.size(), false);
            }
            ImGui::PopID();

            ImGui::BeginGroup();
            for (int i = 0; i < _defaultColumns.size(); i++) {
                if (i % nPerColumn == 0) {
                    ImGui::EndGroup();
                    ImGui::SameLine();
                    ImGui::BeginGroup();
                }

                if (nSelected > IMGUI_TABLE_MAX_COLUMNS) {
                    canSelectMore = false;
                }

                const Column& c = _defaultColumns[i];

                if (c.id == ColumnID::Name) {
                    // Name is required
                    ImGui::Checkbox(c.name.c_str(), &_selectedDefaultColumns[i]);
                    ImGui::SameLine();
                    ImGui::TextDisabled(" (required)");
                    _selectedDefaultColumns[i] = true;
                }
                else {
                    ImGui::Checkbox(c.name.c_str(), &_selectedDefaultColumns[i]);
                }

                nSelected += _selectedDefaultColumns[i] ? 1 : 0;
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

            ImGui::PushID("clear_other");
            if (ImGui::Button("Clear selection")) {
                _selectedOtherColumns.assign(_otherColumns.size(), false);
            }
            ImGui::PopID();

            ImGui::BeginGroup();
            for (int i = 0; i < _otherColumns.size(); i++) {
                if (i % nPerColumn == 0) {
                    ImGui::EndGroup();
                    ImGui::SameLine();
                    ImGui::BeginGroup();
                }

                if (nSelected > IMGUI_TABLE_MAX_COLUMNS) {
                    canSelectMore = false;
                }

                const Column& c = _otherColumns[i];
                ImGui::Checkbox(c.name.c_str(), &_selectedOtherColumns[i]);
                nSelected += _selectedOtherColumns[i] ? 1 : 0;
            }
            ImGui::EndGroup();

            ImGui::EndGroup();
        }

        bool isTooManyColumns = nSelected > IMGUI_TABLE_MAX_COLUMNS;
        glm::vec4 textColor = isTooManyColumns ? ErrorColor : DescriptiveTextColor;

        ImGui::TextColored(
            toImVec4(textColor),
            fmt::format(
                "Selected: {} / {}", nSelected, IMGUI_TABLE_MAX_COLUMNS
            ).c_str()
        );

        // Ok / Cancel
        if (isTooManyColumns) {
            ImGui::PushStyleColor(ImGuiCol_Button, toImVec4(DisabledButtonColor));
            ImGui::PushStyleColor(ImGuiCol_ButtonHovered, toImVec4(DisabledButtonColor));
            ImGui::PushStyleColor(ImGuiCol_ButtonActive, toImVec4(DisabledButtonColor));
        }

        if (ImGui::Button("OK", ImVec2(120, 0)) && !isTooManyColumns) {
            setUpSelectedColumns(nSelected);
            ImGui::CloseCurrentPopup();
        }

        if (isTooManyColumns) {
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
}

void DataViewer::setUpSelectedColumns(int nSelected) {
    ghoul_assert(
        (_selectedDefaultColumns.size() == _defaultColumns.size()) &&
        (_selectedOtherColumns.size() == _otherColumns.size()),
        "Number of columns must match!"
    );
    _columns.clear();
    _columns.reserve(nSelected);

    for (int i = 0; i < _defaultColumns.size(); i++) {
        if (_selectedDefaultColumns[i]) {
            _columns.push_back(_defaultColumns[i]);
        }
    }

    for (int i = 0; i < _otherColumns.size(); i++) {
        if (_selectedOtherColumns[i]) {
            _columns.push_back(_otherColumns[i]);
        }
    }
}

void DataViewer::renderSystemViewContent(const std::string& host) {
    const std::string hostIdentifier = createIdentifier(host);
    bool systemIsAdded = !systemCanBeAdded(host);

    std::vector<size_t>& planetIndexes = _hostIdToPlanetsMap[createIdentifier(host)];

    ImGui::BeginGroup();
    {
        if (!systemIsAdded) {
            if (ImGui::Button("Add system")) {
                addExoplanetSystem(host);
            }
        }
        else {
            // Button to focus Star
            if (ImGui::Button("Focus star")) {
                // Ugly: Always set reach factors when targetting object;
                // we can't do it until the system is added to the scene
                setIncreasedReachfactors();

                openspace::global::scriptEngine->queueScript(
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '" + hostIdentifier + "');"
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');"
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);",
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            }

            ImGui::SameLine();
            if (ImGui::Button("Zoom to star")) {
                // Ugly: Always set reach factors when targetting object;
                // we can't do it until the system is added to the scene
                setIncreasedReachfactors();

                openspace::global::scriptEngine->queueScript(
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '" + hostIdentifier + "');"
                    "openspace.pathnavigation.zoomToDistanceRelative(100.0, 5.0);",
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
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
                renderHelpMarker(
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
                renderHelpMarker(
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
                renderHelpMarker(
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

            if (planetIndexes.size() > 0) {
                // Assume that if first one we find is enabled/disabled, all are
                std::string planetDiscId;
                for (size_t i : planetIndexes) {
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
                        for (size_t i : planetIndexes) {
                            const ExoplanetItem& p = _data[i];
                            const std::string discId = planetIdentifier(p) + "_Disc";
                            if (renderable(discId)) {
                                setRenderableEnabled(discId, enabled);
                            }
                        }
                    }
                    ImGui::SameLine();
                    renderHelpMarker(
                        "Show/hide the disc overlayed on planet orbits that visualizes the "
                        "uncertainty of the orbit's semi-major axis"
                    );
                }
            }



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
                    std::string propertyId = fmt::format(
                        "Scene.{}.Renderable.Appearance.Color", planetTrailId
                    );
                    openspace::global::scriptEngine->queueScript(
                        fmt::format(
                            "openspace.setPropertyValueSingle('{}', {});",
                            propertyId, ghoul::to_string(color)
                        ),
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                }

                if (renderable(planetDiscId)) {
                    std::string propertyId = fmt::format(
                        "Scene.{}.Renderable.MultiplyColor", planetDiscId
                    );
                    openspace::global::scriptEngine->queueScript(
                        fmt::format(
                            "openspace.setPropertyValueSingle('{}', {});",
                            propertyId, ghoul::to_string(color)
                        ),
                        scripting::ScriptEngine::RemoteScripting::Yes
                    );
                }
            };

            auto setTrailThicknessAndFade = [](const ExoplanetItem& p, float width, float fade) {
                const std::string id = planetIdentifier(p) + "_Trail";
                if (!renderable(id)) {
                    return;
                }
                std::string appearance =
                    fmt::format("Scene.{}.Renderable.Appearance", id);

                openspace::global::scriptEngine->queueScript(
                    fmt::format(
                        "openspace.setPropertyValueSingle('{0}.LineWidth', {1});"
                        "openspace.setPropertyValueSingle('{0}.Fade', {2});",
                        appearance, width, fade
                    ),
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            };

            auto resetTrailWidth = [&setTrailThicknessAndFade](const ExoplanetItem& p) {
                setTrailThicknessAndFade(p, 10.f, 1.f);
            };

            if (colorOrbits) {
                static ColorMappedVariable variable;
                bool colorEditChanged = renderColormapEdit(variable, host);
                if (colorOptionChanged || colorEditChanged) {
                    for (size_t planetIndex : planetIndexes) {
                        const ExoplanetItem& p = _data[planetIndex];
                        if (colorOptionChanged) {
                            // First time we change color
                            setTrailThicknessAndFade(p, 20.f, 30.f);
                        }

                        glm::vec3 color = glm::vec3(colorFromColormap(p, variable));
                        colorTrail(p, color);
                    }
                }
            }
            else if (colorOptionChanged) {
                // Reset rendering
                for (size_t planetIndex : planetIndexes) {
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
    renderTable("systemTable", planetIndexes, true);
    ImGui::PopID();
}

void DataViewer::renderColumnValue(int columnIndex, std::optional<const char*> format,
                                   const ExoplanetItem& item)
{
    std::variant<const char*, float> value = valueFromColumn(columnIndex, item);

    if (std::holds_alternative<float>(value)) {
        float v = std::get<float>(value);
        if (std::isnan(v)) {
            ImGui::TextUnformatted("");
        }
        else {
            ImGui::Text(format.value_or("%f"), v);
        }
    }
    else if (std::holds_alternative<const char*>(value)) {
        ImGui::Text("%s", std::get<const char*>(value));
    }
}

int DataViewer::columnIndexFromId(ColumnID id) const {
    ghoul_assert(id != ColumnID::Other, "Can only check specific columns");

    for (int i = 0; i < _columns.size(); i++) {
        if (_columns[i].id == id) {
            return i;
        }
    }
    throw("Could not find column"); // not found
}


bool DataViewer::compareColumnValues(int columnIndex, const ExoplanetItem& left,
                                     const ExoplanetItem& right) const
{
    std::variant<const char*, float> leftValue = valueFromColumn(columnIndex, left);
    std::variant<const char*, float> rightValue = valueFromColumn(columnIndex, right);

    // TODO: make sure they are the same type

    if (std::holds_alternative<const char*>(leftValue) && std::holds_alternative<const char*>(rightValue)) {
        return !caseInsensitiveLessThan(
            std::get<const char*>(leftValue),
            std::get<const char*>(rightValue)
        );
    }
    else if (std::holds_alternative<float>(leftValue) && std::holds_alternative<float>(rightValue)) {
        return compareValues(std::get<float>(leftValue), std::get<float>(rightValue));
    }
    else {
        LERROR("Trying to compare mismatching column types");
        return false;
    }
}

std::variant<const char*, float> DataViewer::valueFromColumn(int columnIndex,
                                                         const ExoplanetItem& item) const
{
    ColumnID column = _columns[columnIndex].id;
    switch (column) {
        case ColumnID::Name:
            return item.planetName.c_str();
        case ColumnID::Host:
            return item.hostName.c_str();
        case ColumnID::DiscoveryYear:
            return static_cast<float>(item.discoveryYear);
        case ColumnID::NPlanets:
            return static_cast<float>(item.nPlanets);
        case ColumnID::NStars:
            return static_cast<float>(item.nStars);
        case ColumnID::ESM:
            return item.esm;
        case ColumnID::TSM:
            return item.tsm;
        case ColumnID::PlanetRadius:
            return item.radius.value;
        case ColumnID::PlanetTemperature:
            return item.eqilibriumTemp.value;
        case ColumnID::PlanetMass:
            return item.mass.value;
        case ColumnID::PlanetMassError:
            return item.mass.relativeErrorRange(); // TODO: make it possible to change whicha and how errors are being shown
        case ColumnID::SurfaceGravity:
            return item.surfaceGravity.value;
        // Orbits
        case ColumnID::SemiMajorAxis:
            return item.semiMajorAxis.value;
        case ColumnID::Eccentricity:
            return item.eccentricity.value;
        case ColumnID::Period:
            return item.period.value;
        case ColumnID::Inclination:
            return item.inclination.value;
        // Star
        case ColumnID::StarTemperature:
            return item.starEffectiveTemp.value;
        case ColumnID::StarRadius:
            return item.starRadius.value;
        case ColumnID::StarAge:
            return item.starAge.value;
        case ColumnID::MagnitudeJ:
            return item.magnitudeJ.value;
        case ColumnID::MagnitudeK:
            return item.magnitudeK.value;
        case ColumnID::Distance:
            return item.distance.value;
        case ColumnID::Ra:
            return item.ra.value;
        case ColumnID::Dec:
            return item.dec.value;
        case ColumnID::Metallicity:
            return item.starMetallicity.value;
        case ColumnID::MetallicityRatio:
            return item.starMetallicityRatio.c_str();
        // Discovery
        case ColumnID::DiscoveryMethod:
            return item.discoveryMethod.c_str();
        case ColumnID::DiscoveryTelescope:
            return item.discoveryTelescope.c_str();
        case ColumnID::DiscoveryInstrument:
            return item.discoveryInstrument.c_str();
        // Detected molecules
        case ColumnID::MoleculesDetection:
            return item.moleculesDetection.c_str();
        case ColumnID::MoleculesUpperLimit:
            return item.moleculesUpperLimit.c_str();
        case ColumnID::MoleculesNoDetection:
            return item.moleculesNoDetection.c_str();
        case ColumnID::WaterDetection:
            return item.waterDetection;
        case ColumnID::Other: {
            std::string key = _columns[columnIndex].name;
            std::variant<std::string, float> value = item.otherColumns.at(key);

            if (std::holds_alternative<std::string>(value)) {
                return std::get<std::string>(value).c_str();
            }
            else {
                return std::get<float>(value);
            }
        }
        default:
            throw ghoul::MissingCaseException();
    }
}

bool DataViewer::isNumericColumn(int index) const {
    ghoul_assert(_data.size() > 0, "Data size cannot be zero");
    // Test type using the first data point
    std::variant<const char*, float> aValue = valueFromColumn(index, _data.front());
    return std::holds_alternative<float>(aValue);
}

glm::vec4 DataViewer::colorFromColormap(const ExoplanetItem& item,
                                        const ColorMappedVariable& variable)
{
    const int colormapColumn = variable.columnIndex;

    std::variant<const char*, float> value = valueFromColumn(colormapColumn, item);
    float fValue = 0.0;
    if (std::holds_alternative<float>(value)) {
        fValue = std::get<float>(value);
    }
    else {
        // text column => cannot be mapped to colormap
        // OBS! This should not happen
        return _nanPointColor;
    }

    glm::vec4 pointColor;
    if (std::isnan(fValue)) {
        pointColor = _nanPointColor;
    }
    else {
        // TODO: handle min > max
        ImPlot::PushColormap(_colormaps[variable.colormapIndex]);

        float min = variable.colorScaleMin;
        float max = variable.colorScaleMax;
        float minMaxDiff = std::abs(max - min);
        float t = minMaxDiff > std::numeric_limits<float>::epsilon() ?
                 (fValue - min) / minMaxDiff : 0.f;

        t = std::clamp(t, 0.f, 1.f);
        ImVec4 c = ImPlot::SampleColormap(t);
        ImPlot::PopColormap();
        pointColor = { c.x, c.y, c.z, c.w };
    }
    return pointColor;
}

void DataViewer::writeRenderDataToFile() {
    std::ofstream file(absPath(RenderDataFile), std::ios::binary);
    if (!file) {
        LERROR(fmt::format("Cannot open file '{}' for writing", RenderDataFile));
        return;
    }

    std::ofstream labelfile(absPath(LabelsFile));
    if (!labelfile) {
        LERROR(fmt::format("Cannot open file '{}' for writing", LabelsFile));
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

    // TODO: use size_t instead of unsigned int

    // Write number of points
    unsigned int nPoints = static_cast<unsigned int>(indicesWithPositions.size());
    file.write(reinterpret_cast<const char*>(&nPoints), sizeof(unsigned int));

    for (size_t index : indicesWithPositions) {
        const ExoplanetItem& item = _data[index];

        file.write(reinterpret_cast<const char*>(&index), sizeof(size_t));

        size_t nVariables = _variableSelection.size();
        file.write(reinterpret_cast<const char*>(&nVariables), sizeof(size_t));

        const glm::dvec3 position = *item.position;
        file.write(reinterpret_cast<const char*>(&position.x), sizeof(double));
        file.write(reinterpret_cast<const char*>(&position.y), sizeof(double));
        file.write(reinterpret_cast<const char*>(&position.z), sizeof(double));

        if (!_useGlyphRendering) {
            nVariables = 1; // If not glyph, just use first variable
        }

        for (int i = 0; i < nVariables; ++i) {
            const ImVec4 color = toImVec4(colorFromColormap(item, _variableSelection[i]));
            file.write(reinterpret_cast<const char*>(&color.x), sizeof(float));
            file.write(reinterpret_cast<const char*>(&color.y), sizeof(float));
            file.write(reinterpret_cast<const char*>(&color.z), sizeof(float));
            file.write(reinterpret_cast<const char*>(&color.w), sizeof(float));
        }

        // Other data used for rendering
        if (_useGlyphRendering) {
            // Get a number for the planetary component
            int component = item.component - 'a';
            file.write(reinterpret_cast<const char*>(&component), sizeof(int));
        }

        // Write label to file
        bool isAdded = std::find(hosts.begin(), hosts.end(), item.hostName) != hosts.end();
        if (!isAdded) {
            labelfile << fmt::format(
                "{} {} {} text {}",
                position.x, position.y, position.z, item.hostName
            );
            labelfile << std::endl;
            hosts.push_back(item.hostName);
        }
    }

}

void DataViewer::updateSelectionInRenderable() {
    const std::string indices = formatIndicesList(_selection);
    const std::string uri = fmt::format(
        "Scene.{}.Renderable.Selection",
        ExoplanetsExpertToolModule::GlyphCloudIdentifier
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle('" + uri + "', { " + indices + " })",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DataViewer::addOrTargetPlanet(const ExoplanetItem& item) {
    const std::string identifier = createIdentifier(item.hostName);

    if (systemCanBeAdded(item.hostName)) {
        LINFO("Adding system. Click again to target");
        addExoplanetSystem(item.hostName);
    }
    else {
        // Ugly: Always set reach factors when targetting object;
        // we can't do it until the system is added to the scene
        setIncreasedReachfactors();

        openspace::global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle("
                "'NavigationHandler.OrbitalNavigator.Anchor',"
                "'" + planetIdentifier(item) + "'"
            ");"
            "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');"
            "openspace.setPropertyValueSingle("
                "'NavigationHandler.OrbitalNavigator.RetargetAnchor', "
                "nil"
            ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

bool DataViewer::systemCanBeAdded(const std::string& host) const {
    const std::string identifier = createIdentifier(host);

    // Check if it does not already exist
    return sceneGraphNode(identifier) == nullptr;

    // TODO: also check against exoplanet list
}


void DataViewer::addExoplanetSystem(const std::string& host) const {
    openspace::global::scriptEngine->queueScript(
        "openspace.exoplanets.addExoplanetSystem('" + host + "')",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle('Modules.CefWebGui.Reload', nil)",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DataViewer::refocusView() const {
    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth');"
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');"
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DataViewer::flyToOverview() const {
    // Create a linear path to Earth
    openspace::global::scriptEngine->queueScript(
        "openspace.pathnavigation.createPath({"
            "TargetType = 'Node', "
            "Target = 'Earth', "
            "Height = 5e+19, " // distance is what matters
            "Duration = 4, "
            "PathType = 'Linear'"
        "});",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DataViewer::flyToInsideView() const {
    // Create a linear path to Earth
    openspace::global::scriptEngine->queueScript(
        "openspace.pathnavigation.createPath({"
            "TargetType = 'Node', "
            "Target = 'Earth', "
            "Height = 1e+17, " // distance is what matters
            "Duration = 4, "
            "PathType = 'Linear'"
        "});",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

} // namespace openspace::exoplanets
