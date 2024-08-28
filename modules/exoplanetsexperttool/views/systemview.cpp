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
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <modules/imgui/include/imgui_include.h>

namespace {
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

    void colorTrail(const openspace::exoplanets::ExoplanetItem& p,
                    const glm::vec3& color)
    {
        const std::string planetTrailId = planetIdentifier(p) + "_Trail";
        const std::string planetDiscId = planetIdentifier(p) + "_Disc";

        if (openspace::renderable(planetTrailId)) {
            std::string propertyId = std::format(
                "Scene.{}.Renderable.Appearance.Color", planetTrailId
            );
            queueScriptSynced(std::format(
                "openspace.setPropertyValueSingle('{}', {});",
                propertyId, ghoul::to_string(color)
            ));
        }

        if (openspace::renderable(planetDiscId)) {
            std::string propertyId = std::format(
                "Scene.{}.Renderable.MultiplyColor", planetDiscId
            );
            queueScriptSynced(std::format(
                "openspace.setPropertyValueSingle('{}', {});",
                propertyId, ghoul::to_string(color)
            ));
        }
    };

    void setTrailThicknessAndFade(const openspace::exoplanets::ExoplanetItem& p,
                                  float width, float fade)
    {
        const std::string id = planetIdentifier(p) + "_Trail";
        if (!openspace::renderable(id)) {
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
    queueScriptSynced("openspace.exoplanets.addExoplanetSystem('" + host + "')");
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

void SystemViewer::flyToStar(std::string_view hostIdentifier) const {
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

void SystemViewer::renderSystemViewContent(const std::string& host) {
    const std::string hostIdentifier = makeIdentifier(host);
    bool systemIsAdded = !systemCanBeAdded(host);

    std::vector<size_t> planetIndices =
        _dataViewer.planetsForHost(makeIdentifier(host));

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

            if (colorOrbits) {
                static ColorMappingView::ColorMappedVariable variable;

                bool colorEditChanged = _dataViewer.colorMappingView()->renderColormapEdit(
                    variable,
                    makeIdentifier(host)
                );

                if (colorOptionChanged || colorEditChanged) {
                    for (size_t planetIndex : planetIndices) {
                        const ExoplanetItem& p = _dataViewer.data()[planetIndex];
                        if (colorOptionChanged) {
                            // First time we change color
                            setTrailThicknessAndFade(p, 20.f, 30.f);
                        }

                        glm::vec3 color = glm::vec3(
                            _dataViewer.colorMappingView()->colorFromColormap(p, variable)
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

    ImGui::Text("Planets:");

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
}

} // namespace openspace::exoplanets
