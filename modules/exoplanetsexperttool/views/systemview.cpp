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

#include <modules/exoplanetsexperttool/views/systemview.h>

#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/views/colormappingview.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <modules/imgui/include/imgui_include.h>

namespace {
    void setRenderableEnabled(std::string_view id, bool value) {
        using namespace openspace;
        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('{}', {});",
            std::format("Scene.{}.Renderable.Enabled", id),
            value ? "true" : "false"
        ));
    };

    // This should match the implementation in the exoplanet module
    std::string planetIdentifier(const openspace::exoplanets::ExoplanetItem& p) {
        using namespace openspace::exoplanets;
        return openspace::makeIdentifier(p.name);
    }

    // Format string for system window name
    std::string systemWindowName(std::string_view host) {
        return std::format("System: {}", host);
    }

    void setDefaultValueOrbitVisuals(bool shouldShowIfDefault) {
        using namespace openspace;
        const std::string appearanceUri = "{defaultvalues_shape}.Renderable"; // @TODO (2026-02-12) This tag no longer exists
        if (shouldShowIfDefault) {
            // Draw using points
            global::scriptEngine->queueScript(std::format(
                "openspace.setPropertyValue('{0}.Appearance.Rendering', 1.0);"
                "openspace.setPropertyValue('{0}.Appearance.PointSize', 64.0);"
                "openspace.setPropertyValue('{0}.Appearance.EnableFade', false);"
                "openspace.setPropertyValue('{0}.Resolution', 100);",
                appearanceUri
            ));
        }
        else {
            // Draw using lines
            global::scriptEngine->queueScript(std::format(
                "openspace.setPropertyValue('{0}.Appearance.Rendering', 0.0);"
                "openspace.setPropertyValue('{0}.Appearance.EnableFade', true);",
                appearanceUri
            ));
        }
    };

    // Set increased reach factors of all exoplanet renderables, to trigger fading out of
    // glyph cloud
    void setIncreasedReachfactors() {
        using namespace openspace;
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{exoplanet}.ApproachFactor', 15000000.0)"
            "openspace.setPropertyValue('{exoplanet_system}.ApproachFactor', 15000000.0)"
        );
    }

    void colorTrail(const openspace::exoplanets::ExoplanetItem& p,
                    const glm::vec3& color)
    {
        using namespace openspace;

        const std::string planetTrailId = planetIdentifier(p) + "_Trail";
        const std::string planetDiscId = planetIdentifier(p) + "_Disc";

        if (openspace::renderable(planetTrailId)) {
            std::string propertyId = std::format(
                "Scene.{}.Renderable.Appearance.Color", planetTrailId
            );
            global::scriptEngine->queueScript(std::format(
                "openspace.setPropertyValueSingle('{}', {});",
                propertyId, ghoul::to_string(color)
            ));
        }

        if (openspace::renderable(planetDiscId)) {
            std::string propertyId = std::format(
                "Scene.{}.Renderable.MultiplyColor", planetDiscId
            );
            global::scriptEngine->queueScript(std::format(
                "openspace.setPropertyValueSingle('{}', {});",
                propertyId, ghoul::to_string(color)
            ));
        }
    };

    void setTrailThicknessAndFade(const openspace::exoplanets::ExoplanetItem& p,
                                  float width, float fade)
    {
        using namespace openspace;

        const std::string id = planetIdentifier(p) + "_Trail";
        if (!openspace::renderable(id)) {
            return;
        }
        std::string appearance =
            std::format("Scene.{}.Renderable.Appearance", id);

        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('{0}.LineWidth', {1});"
            "openspace.setPropertyValueSingle('{0}.Fade', {2});",
            appearance, width, fade
        ));
    };

    void resetTrailWidth(const openspace::exoplanets::ExoplanetItem& p) {
        setTrailThicknessAndFade(p, 10.f, 1.f);
    };
}

namespace openspace::exoplanets {

SystemViewer::SystemViewer(DataViewer& dataViewer)
    : _dataViewer(dataViewer)
{}

void SystemViewer::renderAllSystemViews() {
    if (_shownPlanetSystemWindows.empty()) {
        return;
    }

    std::list<std::string> hostsToRemove;
    for (const std::string& host : _shownPlanetSystemWindows) {
        bool isOpen = true;

        ImGui::SetNextWindowSize(ImVec2(0.f, 0.f), ImGuiCond_Appearing);
        if (ImGui::Begin(systemWindowName(host).c_str(), &isOpen)) {
            renderSystemViewContent(host);
            ImGui::End();
        }

        if (!isOpen) {
            // was closed => remove from list
            hostsToRemove.push_back(host);
        }
    }

    for (const std::string& host : hostsToRemove) {
        _shownPlanetSystemWindows.remove(host);
    }
}

void SystemViewer::renderSystemViewQuickControls(const std::string& host) {
    if (host.empty()) {
        return;
    }

    bool isAlreadyOpen = std::find(
        _shownPlanetSystemWindows.begin(),
        _shownPlanetSystemWindows.end(),
        host
    ) != _shownPlanetSystemWindows.end();

    if (!isAlreadyOpen) {
        //ImGui::PushID(std::format("ShowSystemView-{}", item.name).c_str());
        if (ImGui::Button("Show system view")) {
            _shownPlanetSystemWindows.push_back(host);
            ImGui::CloseCurrentPopup();
        }
        //ImGui::PopID();
    }
    else {
        ImGui::TextDisabled("A system view is already opened for this system");
    }

    bool systemIsAdded = !systemCanBeAdded(host);
    if (systemIsAdded) {
        if (ImGui::Button("Zoom to star")) {
            flyToStar(makeIdentifier(host));
        }
    }
    else {
        if (ImGui::Button("+ Add system")) {
            addExoplanetSystem(host);
        }
    }
}

const std::list<std::string>& SystemViewer::showSystemViews() const {
    return _shownPlanetSystemWindows;
}

void SystemViewer::showSystemView(const std::string& host) {
    // Also open the system view for that system
    bool isAlreadyOpen = std::find(
        _shownPlanetSystemWindows.begin(),
        _shownPlanetSystemWindows.end(),
        host
    ) != _shownPlanetSystemWindows.end();

    if (!isAlreadyOpen) {
        _shownPlanetSystemWindows.push_back(host);
    }
    else {
        // Bring window to front
        ImGui::SetWindowFocus(systemWindowName(host).c_str());
    }
}

bool SystemViewer::systemCanBeAdded(const std::string& host) const {
    const std::string identifier = makeIdentifier(host);

    // Check if it does not already exist
    return sceneGraphNode(identifier) == nullptr;

    // TODO: also check against exoplanet list
}

void SystemViewer::addExoplanetSystem(const std::string& host) const {
    global::scriptEngine->queueScript("openspace.exoplanets.addExoplanetSystem('" + host + "')");
}

void SystemViewer::addOrTargetPlanet(const ExoplanetItem& item) const {
    const std::string identifier = makeIdentifier(item.hostName);

    if (systemCanBeAdded(item.hostName)) {
        LINFOC("Exoplanet System", "Adding system. Click again to target");
        addExoplanetSystem(item.hostName);
    }
    else {
        // Ugly: Always set reach factors when targetting object;
        // we can't do it until the system is added to the scene
        setIncreasedReachfactors();

        global::scriptEngine->queueScript(
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

void SystemViewer::flyToStar(std::string_view hostIdentifier) const {
    // Ugly: Always set reach factors when targetting object;
    // we can't do it until the system is added to the scene
    setIncreasedReachfactors();

    global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle("
        "'NavigationHandler.OrbitalNavigator.Anchor',"
        "'" + std::string(hostIdentifier) +
        "')"
        "openspace.navigation.zoomToDistanceRelative(100.0, 5.0);"
    );
}

void SystemViewer::renderSystemViewContent(const std::string& host) {
    const std::string hostIdentifier = makeIdentifier(host);
    bool systemIsAdded = !systemCanBeAdded(host);

    std::vector<size_t> planetIndices =
        _dataViewer.planetsForHost(makeIdentifier(host));

    ImGui::Text(std::format("{} system, {} planets", host, planetIndices.size()).c_str());
    ImGui::SameLine();

    if (!systemIsAdded) {
        if (ImGui::Button("Add system")) {
            addExoplanetSystem(host);
            setDefaultValueOrbitVisuals(_highlightDefaultOrbits);
        }
    }
    else {
        // Button to focus Star
        if (ImGui::Button("Focus star")) {
            // Ugly: Always set reach factors when targetting object;
            // we can't do it until the system is added to the scene
            setIncreasedReachfactors();

            global::scriptEngine->queueScript(std::format(
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
    }

    ImGui::Separator();

    if (ImGui::BeginTabBar("SystemViewTabs")) {
        // Initial tab shows an overview of the exoplanet system
        if (ImGui::BeginTabItem("Overview")) {
            renderOverviewTabContent(host, planetIndices);
            ImGui::EndTabItem();
        }

        // Tab: Buttons to enable/disable helper renderables or otherwise control the visuals
        if (ImGui::BeginTabItem("Visuals")) {
            if (systemIsAdded) {
                renderVisualsTabContent(host, planetIndices);
            }
            else {
                ImGui::Text("Start by adding the system...");
            }
            ImGui::EndTabItem();
        }

        // Tab: Show the table
        if (ImGui::BeginTabItem("Data (Table)")) {
            ImGui::Text("Here is the full table data for all planets in this system.");

            // OBS! Push an overrided id to make the column settings sync across multiple
            // windows. This is not possible just using the same id in the BeginTable call,
            // since the id is connected to the ImGuiwindow instance
            ImGui::PushOverrideID(ImHashStr("systemTable"));
            _dataViewer.renderTable("systemTable", planetIndices, true);
            ImGui::PopID();


            // Quickly set external selection to webpage
            if (ImGui::Button("Send planets to external webpage")) {
                _dataViewer.updateFilteredRowsProperty(planetIndices);
            }
            ImGui::SameLine();
            view::helper::renderHelpMarker(
                "Send just the planets in this planet system to the ExoplanetExplorer analysis "
                "webpage. Note that this overrides any other filtering. To bring back the filter "
                "selection, update the filtering in any way or press the next button."
            );
            ImGui::SameLine();
            if (ImGui::Button("Reset webpage to filtered")) {
                _dataViewer.updateFilteredRowsProperty();
            }

            ImGui::EndTabItem();
        }

        ImGui::EndTabBar();
    }
}

void SystemViewer::renderOverviewTabContent(const std::string& host,
                                            const std::vector<size_t>& planetIndices)
{
    // TODO: For now, assume that the table columns come from the Exoplanet archive.
    // Later, the names for the columns should not be hardcoded.

    if (planetIndices.empty()) {
        ImGui::Text("No information.");
        return;
    }

    const ExoplanetItem& first = _dataViewer.data()[planetIndices.front()];
    size_t nPlanets = planetIndices.size();

    ImGuiIO& io = ImGui::GetIO();
    float wScale = io.FontGlobalScale;
    float hScale = wScale * (wScale < 1.f ? 1.f : 0.9f);

    // General information about the system
    ImGui::BeginChild(
        std::format("overview_left{}", host).c_str(),
        ImVec2(300 * wScale, 150 * hScale),
        true
    );
    {
        const float indent = 150.f * wScale;
        view::helper::renderDescriptiveText("Distance to Earth: ");
        ImGui::SameLine(indent);
        _dataViewer.renderColumnValue(_dataViewer.dataMapping().positionDistance, first);
        ImGui::SameLine();
        view::helper::renderDescriptiveText("(Parsec)");

        view::helper::renderDescriptiveText("In habitable zone: ");
        ImGui::SameLine(indent);
        ImGui::Text("x"); // TODO: add detals of how many

        ImGui::Text("");

        ImGui::Text("TODO: Other general interesting");
        ImGui::Text("info about the system");
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild(
        std::format("overview_right{}", host).c_str(),
        ImVec2(190 * wScale, 150 * hScale),
        true
    );
    {
        const float indent = 100.f * wScale;
        // Star information
        view::helper::renderDescriptiveText("Stars: ");
        ImGui::SameLine(indent);
        _dataViewer.renderColumnValue("sy_snum", first);

        ImGui::Text("");

        view::helper::renderDescriptiveText("Teff: ");
        ImGui::SameLine(indent);
        _dataViewer.renderColumnValue("st_teff", first);
        ImGui::SameLine();
        view::helper::renderDescriptiveText("(K)");

        view::helper::renderDescriptiveText("Metallicity: ");
        ImGui::SameLine(indent);
        _dataViewer.renderColumnValue("st_met", first);
        ImGui::SameLine();
        _dataViewer.renderColumnValue("st_metratio", first);

        view::helper::renderDescriptiveText("Radius: ");
        ImGui::SameLine(indent);
        _dataViewer.renderColumnValue("st_rad", first);
        ImGui::SameLine();
        view::helper::renderDescriptiveText("(Solar)");

        view::helper::renderDescriptiveText("Age: ");
        ImGui::SameLine(indent);
        _dataViewer.renderColumnValue("st_age", first);
        ImGui::SameLine();
        view::helper::renderDescriptiveText("(Gyr)");
    }
    ImGui::EndChild();

    // Frames showing information about each planet
    ImGui::Text("Planets: ");

    const float indent = 80.f * wScale;
    const float avgExtraIndent = 20.f * wScale;

    auto columnIndent = [&](size_t columnIndex) {
        return static_cast<float>((columnIndex + 1)) * indent;
    };

    const float avgColumnIndent = columnIndent(nPlanets) + avgExtraIndent;

    ImGui::Text("");

    for (size_t i = 0; i < nPlanets; ++i) {
        size_t index = planetIndices[i];
        const ExoplanetItem& p = _dataViewer.data()[index];

        ImGui::SameLine(columnIndent(i));

        // Get the name, but remove the star name from the beginning
        const std::variant<std::string, float>& value =
            p.dataColumns.at(_dataViewer.dataMapping().name);

        if (std::holds_alternative<float>(value)) {
            // This should not happen
            ImGui::Text(std::format("{}", i).c_str());
        }
        else {
            std::string fullName = std::get<std::string>(value);

            std::string::size_type it = fullName.find(host);

            if (it != std::string::npos) {
                fullName.erase(it, host.length());
            }

            ImGui::Text(std::format("{}", fullName).c_str());
        }
    }

    ImGui::SameLine(avgColumnIndent);
    view::helper::renderDescriptiveText("Average");

    // TODO: Include average for certain groups?  Like planets of similar size, for example
    // TODO: Include which "quick filters" that the planet matches...?

    ImGui::Separator();

    // TODO: Avoid hardcoded column names
    std::vector<std::pair<std::string, ColumnKey>> columns = {
        {"ESM", "ESM"},
        {"TSM", "TSM"},
        {"Radius", "pl_rade"},
        {"Mass", "pl_bmasse"},
        {"Incl.", "pl_orbincl"},
        {"Orbit", "pl_orbsmax"},
        {"Period", "pl_orbper"},
        {"Ecc", "pl_orbeccen"},
        {"Method", "discoverymethod"}
    };

    for (const std::pair<std::string, ColumnKey>& pair : columns) {
        view::helper::renderDescriptiveText(std::format("{}: ", pair.first).c_str());
        const ColumnKey& colKey = pair.second;
        for (size_t i = 0; i < nPlanets; ++i) {
            size_t index = planetIndices[i];
            const ExoplanetItem& p = _dataViewer.data()[index];

            ImGui::SameLine(columnIndent(i));
            ImGui::PushItemWidth(static_cast<float>(indent));
            _dataViewer.renderColumnValue(colKey, p);
        }

        if (_dataViewer.meanValue(colKey).has_value()) {
            ImGui::SameLine(avgColumnIndent);
            view::helper::renderDescriptiveText(std::format("{:.2f}", *_dataViewer.meanValue(colKey)).c_str());
        }
    }
    // TODO: Highlight values that are very different from average

    ImGui::Separator();

    ImGui::Text("");

    for (size_t i = 0; i < nPlanets; ++i) {
        size_t index = planetIndices[i];
        const ExoplanetItem& p = _dataViewer.data()[index];

        const SceneGraphNode* node = global::navigationHandler->anchorNode();
        bool isCurrentAnchor = node && node->guiName() == p.name;
        if (isCurrentAnchor) {
            ImGui::PushStyleColor(ImGuiCol_Button, ImColor(0, 153, 112).Value);
            ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImColor(0, 204, 150).Value);
        }

        ImGui::SameLine(columnIndent(i));

        ImGui::PushID(std::format("target_button{} ", index).c_str());
        if (ImGui::Button("Target")) {
            addOrTargetPlanet(p);
        }
        ImGui::PopID();

        if (isCurrentAnchor) {
            ImGui::PopStyleColor(2);
        }
    }
}

void SystemViewer::renderVisualsTabContent(const std::string& host,
                                           const std::vector<size_t>& planetIndices)
{
    const std::string hostIdentifier = makeIdentifier(host);

    ImGui::BeginGroup();
    {
        ImGui::Text("Local:");
        ImGui::SameLine();
        view::helper::renderHelpMarker("Applied to just this individual system");

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

        if (!planetIndices.empty()) {
            // Assume that if first one we find is enabled/disabled, all are
            std::string planetDiscId;
            for (size_t i : planetIndices) {
                const ExoplanetItem& p = _dataViewer.data()[i];
                const std::string discId = planetIdentifier(p) + "_Disc";
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
                        const ExoplanetItem& p = _dataViewer.data()[i];
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
    }
    ImGui::EndGroup();

    ImGui::SameLine();
    ImGui::BeginGroup();
    {
        ImGui::Text("Global:");
        ImGui::SameLine();
        view::helper::renderHelpMarker("Applied globally, to all rendered exoplanet systems");

        if (ImGui::Checkbox("Point orbit for default values", &_highlightDefaultOrbits)) {
            setDefaultValueOrbitVisuals(_highlightDefaultOrbits);
        }
        ImGui::SameLine();
        view::helper::renderHelpMarker(
            "Orbits whose shape/inclination is set using default values will be "
            "rendered as points instead of lines."
        );

        bool colorOptionChanged = ImGui::Checkbox("Color planet orbits", &_colorOrbits);

        static bool colorVariableInitialized = false;
        static ColorMappingView::ColorMappedVariable orbitColorVariable;

        if (!colorVariableInitialized) {
            orbitColorVariable = {
                .columnIndex = _dataViewer.colorMappingView()->firstNumericColumn()
            };
            colorVariableInitialized = true;
        }

        if (_colorOrbits) {
            bool colorEditChanged = _dataViewer.colorMappingView()->renderColormapEdit(
                orbitColorVariable,
                hostIdentifier
            );

            if (colorOptionChanged || colorEditChanged) {
                for (size_t planetIndex : planetIndices) {
                    const ExoplanetItem& p = _dataViewer.data()[planetIndex];
                    if (colorOptionChanged) {
                        // First time we change color
                        setTrailThicknessAndFade(p, 20.f, 30.f);
                    }

                    glm::vec3 color = glm::vec3(
                        _dataViewer.colorMappingView()->colorFromColormap(p, orbitColorVariable)
                    );
                    colorTrail(p, color);
                }
            }
        }
        else if (colorOptionChanged) {
            // Reset rendering
            for (size_t planetIndex : planetIndices) {
                const ExoplanetItem& p = _dataViewer.data()[planetIndex];
                colorTrail(p, glm::vec3(1.f, 1.f, 1.f));
                resetTrailWidth(p);
            }
        }
    }
    ImGui::EndGroup();
}


} // namespace openspace::exoplanets
