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

#include <modules/exoplanetsexperttool/views/systemview.h>

#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/views/colormappingview.h>
#include <modules/exoplanetsexperttool/views/tableview.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <openspace/engine/globals.h>
#include <openspace/logging/logmanager.h>
#include <openspace/misc/stringhelper.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>

#include <modules/imgui/include/imgui_include.h>

namespace {
    using namespace openspace;

    void setRenderableEnabled(std::string_view id, bool value) {
        using namespace openspace;
        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('{}', {});",
            std::format("Scene.{}.Renderable.Enabled", id),
            value ? "true" : "false"
        ));
    };

    // This should match the implementation in the exoplanet module
    std::string planetIdentifier(const exoplanets::ExoplanetItem& p) {
        return makeIdentifier(p.name);
    }

    // Format string for system window name
    std::string systemWindowName(std::string_view host) {
        return std::format("System: {}", host);
    }

    // Set increased reach factors of all exoplanet renderables, to trigger fading out of
    // glyph cloud
    void setIncreasedReachfactors() {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('{exoplanet_planet}.ApproachFactor', 15000000.0)"
            "openspace.setPropertyValue('{exoplanet_system}.ApproachFactor', 15000000.0)"
        );
    }

    void colorTrail(const exoplanets::ExoplanetItem& p,
                    const glm::vec3& color)
    {
        const std::string planetTrailId = planetIdentifier(p) + "_Trail";
        const std::string planetDiscId = planetIdentifier(p) + "_Disc";

        if (openspace::renderable(planetTrailId)) {
            std::string propertyId = std::format(
                "Scene.{}.Renderable.Appearance.Color", planetTrailId
            );
            global::scriptEngine->queueScript(std::format(
                "openspace.setPropertyValueSingle('{}', {});",
                propertyId, to_string(color)
            ));
        }

        if (renderable(planetDiscId)) {
            std::string propertyId = std::format(
                "Scene.{}.Renderable.MultiplyColor", planetDiscId
            );
            global::scriptEngine->queueScript(std::format(
                "openspace.setPropertyValueSingle('{}', {});",
                propertyId, to_string(color)
            ));
        }
    };

    void setTrailThicknessAndFade(const exoplanets::ExoplanetItem& p,
                                  float width, float fade)
    {
        const std::string id = planetIdentifier(p) + "_Trail";
        if (!renderable(id)) {
            return;
        }
        std::string appearance =
            std::format("Scene.{}.Renderable.Appearance", id);

        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('{0}.LineWidth', {1});"
            "openspace.setPropertyValueSingle('{0}.LineFadeAmount', {2});",
            appearance, width, fade
        ));
    };

    void resetTrailWidth(const exoplanets::ExoplanetItem& p) {
        setTrailThicknessAndFade(p, 10.f, 1.f);
    };

    // Render the initials of each word of the text, with the full text as a tooltip
    void renderAbbreviated(const std::string& text) {
        std::string abbreviation;
        for (const std::string& word : tokenizeString(text, ' ')) {
            if (!word.empty()) {
                abbreviation += word[0];
            }
        }
        ImGui::Text("%s", abbreviation.c_str());
        ImGui::SetItemTooltip("%s", text.c_str());
    }
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

    const bool systemMissing = hasSystemBeenAdded(host) && systemCanBeAdded(host);
    if (systemMissing) {
        view::helper::renderHelpMarker(
            "There is not enough data to visualize this system"
        );
    }
    else {
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

bool SystemViewer::hasSystemBeenAdded(const std::string& host) const {
    return std::find(_addedHostStars.begin(), _addedHostStars.end(), host) != _addedHostStars.end();
}

void SystemViewer::addExoplanetSystem(const std::string& host) {
    if (std::find(_addedHostStars.begin(), _addedHostStars.end(), host) == _addedHostStars.end()) {
        _addedHostStars.push_back(host);
    }

    std::string dataFile = _dataViewer.currentDataFile().string();
    // Replace backslashes with forward slashes for Lua script compatibility
    std::replace(dataFile.begin(), dataFile.end(), '\\', '/');

    global::scriptEngine->queueScript(std::format(
        "openspace.exoplanets.loadExoplanetsFromCsv('{}', \"{}\")",
        dataFile, host
    ));
}

void SystemViewer::removeExoplanetSystem(const std::string& host) {
    global::scriptEngine->queueScript(std::format(
        "openspace.exoplanets.removeExoplanetSystem(\"{}\")",
        host
    ));
    std::erase(_addedHostStars, host);
}

void SystemViewer::removeAllExoplanetSystems() {
    for (const std::string& host : _addedHostStars) {
        global::scriptEngine->queueScript(std::format(
            "openspace.exoplanets.removeExoplanetSystem(\"{}\")",
            host
        ));
    }
    _addedHostStars.clear();
}

const std::vector<std::string>& SystemViewer::addedHostStars() const {
    return _addedHostStars;
}

void SystemViewer::addOrTargetPlanet(const ExoplanetItem& item) {
    const std::string identifier = makeIdentifier(item.hostName);

    if (systemCanBeAdded(item.hostName)) {
        LINFOC("Exoplanet System", "Adding system. Click again to target");
        addExoplanetSystem(item.hostName);
    }
    else {
        // Ugly: Always set reach factors when targetting object;
        // we can't do it until the system is added to the scene
        setIncreasedReachfactors();

        global::scriptEngine->queueScript(std::format(
            "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '{}');"
            "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');"
            , planetIdentifier(item)
        ));

        if (!ImGui::GetIO().KeyShift) {
            global::scriptEngine->queueScript(
                "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
            );
        }
    }
}

void SystemViewer::flyToStar(std::string_view hostIdentifier) const {
    if (sceneGraphNode(hostIdentifier) == nullptr) {
        return;
    }

    // Ugly: Always set reach factors when targetting object;
    // we can't do it until the system is added to the scene
    setIncreasedReachfactors();

    global::scriptEngine->queueScript(std::format(
        "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '{}')"
        "openspace.navigation.zoomToDistanceRelative(100.0, 5.0);",
        hostIdentifier
    ));
}

void SystemViewer::renderSystemViewContent(const std::string& host) {
    const std::string hostIdentifier = makeIdentifier(host);
    bool systemIsAdded = !systemCanBeAdded(host);
    const bool systemMissing = hasSystemBeenAdded(host) && systemCanBeAdded(host);

    std::vector<size_t> planetIndices =
        _dataViewer.planetsForHost(makeIdentifier(host));

    ImGui::Text(std::format("{} system, {} planets", host, planetIndices.size()).c_str());
    ImGui::SameLine();

    if (systemMissing) {
        view::helper::renderHelpMarker(
            "There is not enough data to visualize this system"
        );
    }
    else if (!systemIsAdded) {
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

            global::scriptEngine->queueScript(std::format(
                "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', '{}');"
                "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');",
                hostIdentifier
            ));

            if (!ImGui::GetIO().KeyShift) {
                global::scriptEngine->queueScript(
                    "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
                );
            }
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
            _dataViewer.tableView()->renderTable("systemTable", planetIndices, true);
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
    if (planetIndices.empty()) {
        ImGui::Text("No information.");
        return;
    }

    const ExoplanetItem& first = _dataViewer.data()[planetIndices.front()];
    size_t nPlanets = planetIndices.size();

    const DataSettings::SystemViewColumns& viewColumns =
        _dataViewer.dataSettings().systemView;
    const ColumnKey& ratioKey = _dataViewer.dataMapping().metallicityRatio;
    const ColumnKey& methodKey = _dataViewer.dataMapping().discoveryMethod;

    const ImGuiStyle& style = ImGui::GetStyle();
    const float padding = 2.f * style.ItemSpacing.x;

    auto columnLabel = [this](const ColumnKey& key) {
        return std::format("{}: ", _dataViewer.columnName(key));
    };

    // The width required to fit the widest of the given column labels
    auto labelWidth = [&](const std::vector<ColumnKey>& keys) {
        float width = 0.f;
        for (const ColumnKey& key : keys) {
            if (_dataViewer.hasColumn(key)) {
                width = std::max(width, ImGui::CalcTextSize(columnLabel(key).c_str()).x);
            }
        }
        return width + padding;
    };

    // Render a "Name: value" row for one column of the given item. An optional second
    // column may be rendered on the same line, directly after the value
    auto renderColumnRow = [&](const ColumnKey& key, const ExoplanetItem& item,
                               float indent, const ColumnKey& inlineKey = ColumnKey())
    {
        view::helper::renderDescriptiveText(columnLabel(key).c_str());
        ImGui::SameLine(indent);
        _dataViewer.renderColumnValue(key, item);

        if (!inlineKey.empty()) {
            ImGui::SameLine();
            _dataViewer.renderColumnValue(inlineKey, item);
        }

        if (_dataViewer.hasColumnDescription(key)) {
            ImGui::SameLine();
            view::helper::renderHelpMarker(_dataViewer.columnDescription(key));
        }
    };

    std::vector<ColumnKey> systemRows;
    systemRows.reserve(viewColumns.systemColumns.size() + 1);
    if (_dataViewer.hasColumn(_dataViewer.dataMapping().positionDistance)) {
        systemRows.push_back(_dataViewer.dataMapping().positionDistance);
    }
    for (const ColumnKey& key : viewColumns.systemColumns) {
        if (_dataViewer.hasColumn(key)) {
            systemRows.push_back(key);
        }
    }

    // The column of each row, and the column to render on the same line, if any
    std::vector<std::pair<ColumnKey, ColumnKey>> starRows;
    starRows.reserve(viewColumns.starColumns.size());
    for (const ColumnKey& key : viewColumns.starColumns) {
        if (!_dataViewer.hasColumn(key)) {
            continue;
        }
        // The metallicity ratio is shown on the same line as the column before it
        if (key == ratioKey && !starRows.empty()) {
            starRows.back().second = ratioKey;
            continue;
        }
        starRows.emplace_back(key, ColumnKey());
    }

    // + 1 for the "Main star" title
    const size_t nRows = std::max(systemRows.size(), starRows.size() + 1);
    const float boxHeight = static_cast<float>(nRows) * ImGui::GetTextLineHeightWithSpacing()
        + 2.f * style.WindowPadding.y;

    // General information about the system
    ImGui::BeginChild(
        std::format("overview_left{}", host).c_str(),
        ImVec2(ImGui::GetContentRegionAvail().x * 0.5f, boxHeight),
        true
    );
    {
        const float indent = labelWidth(systemRows);
        for (const ColumnKey& key : systemRows) {
            renderColumnRow(key, first, indent);
        }
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild(
        std::format("overview_right{}", host).c_str(),
        ImVec2(0, boxHeight),
        true
    );
    {
        ImGui::Text("Main star");

        const float indent = labelWidth(viewColumns.starColumns);
        for (const std::pair<ColumnKey, ColumnKey>& row : starRows) {
            renderColumnRow(row.first, first, indent, row.second);
        }
    }
    ImGui::EndChild();

    // Frames showing information about each planet
    std::vector<ColumnKey> planetRows;
    planetRows.reserve(viewColumns.planetColumns.size() + 1);
    for (const ColumnKey& key : viewColumns.planetColumns) {
        if (_dataViewer.hasColumn(key)) {
            planetRows.push_back(key);
        }
    }
    const bool hasMethodColumn = _dataViewer.hasColumn(methodKey);
    if (hasMethodColumn) {
        planetRows.push_back(methodKey);
    }

    // The planet names, with the host star name removed from the beginning
    std::vector<std::string> planetNames;
    planetNames.reserve(nPlanets);
    for (size_t index : planetIndices) {
        const ExoplanetItem& p = _dataViewer.data()[index];
        const std::variant<std::string, float>& value =
            p.dataColumns.at(_dataViewer.dataMapping().name);

        if (std::holds_alternative<float>(value)) {
            // This should not happen
            planetNames.push_back(std::format("{}", planetNames.size()));
            continue;
        }

        std::string name = std::get<std::string>(value);
        std::string::size_type it = name.find(host);
        if (it != std::string::npos) {
            name.erase(it, host.length());
        }
        planetNames.push_back(name);
    }

    const float labelIndent = std::max(
        labelWidth(planetRows),
        ImGui::CalcTextSize("Planets: ").x + padding
    );

    // Wide enough to fit the planet names, the "Target" buttons and typical values
    float columnWidth = std::max(
        ImGui::CalcTextSize("-1000.00").x,
        ImGui::CalcTextSize("Target").x + 2.f * style.FramePadding.x
    );
    for (const std::string& name : planetNames) {
        columnWidth = std::max(columnWidth, ImGui::CalcTextSize(name.c_str()).x);
    }
    columnWidth += padding;

    auto columnIndent = [&](size_t columnIndex) {
        return labelIndent + static_cast<float>(columnIndex) * columnWidth;
    };

    const float avgColumnIndent = columnIndent(nPlanets) + padding;

    ImGui::Text("Planets: ");
    for (size_t i = 0; i < nPlanets; ++i) {
        ImGui::SameLine(columnIndent(i));
        ImGui::Text("%s", planetNames[i].c_str());
    }

    ImGui::SameLine(avgColumnIndent);
    view::helper::renderDescriptiveText("Average");

    // TODO: Include average for certain groups?  Like planets of similar size, for example
    // TODO: Include which "quick filters" that the planet matches...?

    ImGui::Separator();

    for (const ColumnKey& colKey : planetRows) {
        const bool isMethodColumn = hasMethodColumn && (colKey == methodKey);

        if (isMethodColumn) {
            ImGui::Separator();
        }

        view::helper::renderDescriptiveText(columnLabel(colKey).c_str());
        if (_dataViewer.hasColumnDescription(colKey)) {
            ImGui::SetItemTooltip("%s", _dataViewer.columnDescription(colKey));
        }

        for (size_t i = 0; i < nPlanets; ++i) {
            size_t index = planetIndices[i];
            const ExoplanetItem& p = _dataViewer.data()[index];

            ImGui::SameLine(columnIndent(i));

            if (isMethodColumn) {
                auto it = p.dataColumns.find(colKey);
                if (it != p.dataColumns.end() &&
                    std::holds_alternative<std::string>(it->second))
                {
                    renderAbbreviated(std::get<std::string>(it->second));
                }
                else {
                    ImGui::Text("N/A");
                }
            }
            else {
                _dataViewer.renderColumnValue(colKey, p);
            }
        }

        if (!isMethodColumn && _dataViewer.meanValue(colKey).has_value()) {
            ImGui::SameLine(avgColumnIndent);
            view::helper::renderDescriptiveText(
                std::format("{:.2f}", *_dataViewer.meanValue(colKey)).c_str()
            );
        }
    }
    // TODO: Highlight values that are very different from average

    const bool systemMissing = hasSystemBeenAdded(host) && systemCanBeAdded(host);
    if (!systemMissing) {
        ImGui::Separator();

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
}

void SystemViewer::renderVisualsTabContent(const std::string& host,
                                           const std::vector<size_t>& planetIndices)
{
    const std::string hostIdentifier = makeIdentifier(host);

    ImGui::BeginGroup();
    {
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
            if (ImGui::Checkbox("Show 90-degree inclination plane", &enabled)) {
                setRenderableEnabled(inclinationPlaneId, enabled);
            }
            ImGui::SameLine();
            view::helper::renderHelpMarker(
                "Show a grid plane that represents 90 degree inclination, "
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

        const std::string rotationAxisId = hostIdentifier + "_RotationAxis";
        const Renderable* rotationAxis = renderable(rotationAxisId);
        if (rotationAxis) {
            bool enabled = rotationAxis->isEnabled();
            if (ImGui::Checkbox("Show star rotation axis", &enabled)) {
                setRenderableEnabled(rotationAxisId, enabled);
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
        bool colorOptionChanged = ImGui::Checkbox("Color planet orbits", &_shouldColorOrbits);

        static bool colorVariableInitialized = false;
        static ColorMappingView::ColorMappedVariable orbitColorVariable;

        if (!colorVariableInitialized) {
            orbitColorVariable = {
                .column = _dataViewer.colorMappingView()->firstNumericColumn()
            };
            colorVariableInitialized = true;
        }

        if (_shouldColorOrbits) {
            bool colorEditChanged = _dataViewer.colorMappingView()->renderColormapEdit(
                orbitColorVariable,
                hostIdentifier
            );

            if (colorOptionChanged || colorEditChanged) {
                for (size_t planetIndex : planetIndices) {
                    const ExoplanetItem& p = _dataViewer.data()[planetIndex];
                    if (colorOptionChanged) {
                        // First time we change color
                        setTrailThicknessAndFade(p, 20.f, 0.f);
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
