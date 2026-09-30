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

#include <modules/exoplanetsexperttool/views/spatialselectionview.h>

#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <implot.h>
#include <algorithm>
#include <format>

namespace openspace::exoplanets {

SpatialSelectionView::SpatialSelectionView(DataViewer& dataViewer,
                                           const DataSettings& dataSettings)
    : _dataViewer(dataViewer)
    , _dataSettings(dataSettings)
{
    // Default Sky Map rectangle over a meaningful patch
    _skyMapRect.raMin = 280.0;
    _skyMapRect.raMax = 310.0;
    _skyMapRect.decMin = 35.0;
    _skyMapRect.decMax = 55.0;
}

InteractionMode SpatialSelectionView::interactionMode() const {
    return _mode;
}

void SpatialSelectionView::setInteractionMode(InteractionMode mode) {
    _mode = mode;
}

bool SpatialSelectionView::isSelectionMode() const {
    return _mode == InteractionMode::Selection;
}

SpatialSelectionHandler& SpatialSelectionView::handler() {
    return _handler;
}

const SpatialSelectionHandler& SpatialSelectionView::handler() const {
    return _handler;
}

void SpatialSelectionView::updateSkyMapCache() const {
    _cachedRa.clear();
    _cachedDec.clear();

    const std::vector<ExoplanetItem>& data = _dataViewer.data();
    const std::vector<size_t>& filtered = _dataViewer.currentFiltering();
    const DataSettings::DataMapping& mapping = _dataViewer.dataMapping();

    if (mapping.positionRa.empty() || mapping.positionDec.empty()) {
        _skyMapCacheDirty = false;
        return;
    }

    _cachedRa.reserve(filtered.size());
    _cachedDec.reserve(filtered.size());

    for (size_t idx : filtered) {
        if (idx >= data.size()) {
            continue;
        }
        const ExoplanetItem& item = data[idx];
        const bool hasRaDec = item.dataColumns.contains(mapping.positionRa) &&
            item.dataColumns.contains(mapping.positionDec);
        if (hasRaDec) {
            const std::variant<std::string, float>& raVal =
                item.dataColumns.at(mapping.positionRa);
            const std::variant<std::string, float>& decVal =
                item.dataColumns.at(mapping.positionDec);
            if (std::holds_alternative<float>(raVal) &&
                std::holds_alternative<float>(decVal))
            {
                _cachedRa.push_back(std::get<float>(raVal));
                _cachedDec.push_back(std::get<float>(decVal));
            }
        }
    }

    _skyMapCacheDirty = false;
}

std::vector<size_t> SpatialSelectionView::computeCurrentSpatialSelection() const {
    const std::vector<ExoplanetItem>& data = _dataViewer.data();
    const std::vector<size_t>& filtered = _dataViewer.currentFiltering();

    switch (_currentMethod) {
        case SpatialSelectionMethod::Shape3D:
            if (_shape3DType == Shape3DType::Sphere) {
                return _handler.selectSphere(data, filtered, _sphereVolume);
            }
            else {
                return _handler.selectBox(data, filtered, _boxVolume);
            }
        case SpatialSelectionMethod::SkyMap:
            return _handler.selectSkyRect(
                data,
                filtered,
                _skyMapRect,
                _dataViewer.dataMapping()
            );
        // case SpatialSelectionMethod::Density:
        //     return _handler.selectDensity(data, filtered, _densitySettings);
        default:
            return {};
    }
}

void SpatialSelectionView::applyCurrentSelection() {
    std::vector<size_t> sel = computeCurrentSpatialSelection();
    _dataViewer.setSelection(sel);
}

void SpatialSelectionView::applyCombinedSavedSelections() {
    std::vector<size_t> combined = _handler.combinedSavedSelections();
    _dataViewer.setSelection(combined);
}

void SpatialSelectionView::render(bool* open) {
    if (!open || !*open) {
        return;
    }

    ImGui::SetNextWindowSize(ImVec2(520, 640), ImGuiCond_FirstUseEver);
    if (!ImGui::Begin("Spatial Selection", open)) {
        ImGui::End();
        return;
    }

    renderModeSelector();
    ImGui::Separator();

    renderMethodTabs();
    ImGui::Separator();

    renderCurrentSelectionActions();
    ImGui::Separator();

    renderSavedSelectionsManager();

    ImGui::End();
}

void SpatialSelectionView::renderModeSelector() {
    ImGui::TextUnformatted("Mouse Interaction Mode:");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Navigation Mode: Standard OpenSpace mouse navigation (rotate, zoom, pan).\n"
        "Selection Mode: Mouse clicks and drags are dedicated to selection tools."
    );

    int modeIdx = static_cast<int>(_mode);
    if (ImGui::RadioButton("Navigation Mode", &modeIdx, 0)) {
        _mode = InteractionMode::Navigation;
    }
    ImGui::SameLine();
    if (ImGui::RadioButton("Selection Mode", &modeIdx, 1)) {
        _mode = InteractionMode::Selection;
    }

    if (_mode == InteractionMode::Selection) {
        ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.4f, 0.9f, 0.4f, 1.0f));
        ImGui::TextUnformatted("● Selection Mode Active (Camera navigation paused)");
        ImGui::PopStyleColor();
    }
}

void SpatialSelectionView::renderMethodTabs() {
    if (ImGui::BeginTabBar("SpatialSelectionTabBar")) {
        if (ImGui::BeginTabItem("3D Shape")) {
            _currentMethod = SpatialSelectionMethod::Shape3D;
            renderShape3DTab();
            ImGui::EndTabItem();
        }
        if (ImGui::BeginTabItem("2D Sky Map")) {
            _currentMethod = SpatialSelectionMethod::SkyMap;
            renderSkyMapTab();
            ImGui::EndTabItem();
        }
        // if (ImGui::BeginTabItem("Density (Future)")) {
        //     _currentMethod = SpatialSelectionMethod::Density;
        //     renderDensityTab();
        //     ImGui::EndTabItem();
        // }
        ImGui::EndTabBar();
    }
}

void SpatialSelectionView::renderShape3DTab() {
    bool changed = false;

    ImGui::TextUnformatted("Shape Type:");
    ImGui::SameLine();
    int shapeTypeIdx = static_cast<int>(_shape3DType);
    if (ImGui::RadioButton("Sphere", &shapeTypeIdx, 0)) {
        _shape3DType = Shape3DType::Sphere;
        changed = true;
    }
    ImGui::SameLine();
    if (ImGui::RadioButton("Box (Cuboid)", &shapeTypeIdx, 1)) {
        _shape3DType = Shape3DType::Box;
        changed = true;
    }

    ImGui::Spacing();

    glm::dvec3& centerVec = (_shape3DType == Shape3DType::Sphere)
        ? _sphereVolume.center
        : _boxVolume.center;

    float center[3] = {
        static_cast<float>(centerVec.x),
        static_cast<float>(centerVec.y),
        static_cast<float>(centerVec.z)
    };
    if (ImGui::DragFloat3("Center (pc)", center, 1.0f, -100000.0f, 100000.0f, "%.1f")) {
        centerVec = glm::dvec3(center[0], center[1], center[2]);
        _sphereVolume.center = centerVec;
        _boxVolume.center = centerVec;
        changed = true;
    }

    ImGui::Spacing();
    ImGui::TextUnformatted("Center Presets:");
    if (ImGui::Button("Earth / Solar System (0,0,0)")) {
        _sphereVolume.center = glm::dvec3(0.0);
        _boxVolume.center = glm::dvec3(0.0);
        changed = true;
    }
    ImGui::SameLine();
    if (ImGui::Button("Camera Position")) {
        if (global::navigationHandler) {
            const glm::dvec3 camPos = global::navigationHandler->camera()->position();
            // Convert from meters to parsecs (1 pc approx 3.08567758e16 meters)
            constexpr double MetersPerParsec = 3.08567758149137e16;
            _sphereVolume.center = camPos / MetersPerParsec;
            _boxVolume.center = _sphereVolume.center;
            changed = true;
        }
    }

    const std::vector<size_t>& sel = _dataViewer.selection();
    const bool hasSingleSelection = sel.size() == 1;
    const std::vector<ExoplanetItem>& allData = _dataViewer.data();
    const bool hasValidPos = hasSingleSelection && (sel[0] < allData.size()) &&
        allData[sel[0]].position.has_value();

    std::string planetButtonLabel = "Selected Planet";
    if (hasSingleSelection && (sel[0] < allData.size()) && !allData[sel[0]].name.empty())
    {
        planetButtonLabel = std::format("Selected Planet ({})", allData[sel[0]].name);
    }

    ImGui::SameLine();
    if (!hasValidPos) {
        ImGui::BeginDisabled();
    }
    if (ImGui::Button(planetButtonLabel.c_str())) {
        if (hasValidPos) {
            _sphereVolume.center = *allData[sel[0]].position;
            _boxVolume.center = _sphereVolume.center;
            changed = true;
        }
    }
    if (!hasValidPos) {
        ImGui::EndDisabled();
        if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled)) {
            if (sel.empty()) {
                ImGui::SetTooltip("No planet is currently selected");
            }
            else if (sel.size() > 1) {
                ImGui::SetTooltip(
                    "Multiple planets are selected (requires exactly one)"
                );
            }
            else {
                ImGui::SetTooltip("The selected planet has no 3D position data");
            }
        }
    }

    ImGui::Spacing();
    ImGui::Separator();

    if (_shape3DType == Shape3DType::Sphere) {
        float radius = static_cast<float>(_sphereVolume.radius);
        if (ImGui::DragFloat("Radius (pc)", &radius, 1.0f, 0.1f, 100000.0f, "%.1f pc")) {
            _sphereVolume.radius = std::max(0.1, static_cast<double>(radius));
            changed = true;
        }
    }
    else {
        float dims[3] = {
            static_cast<float>(_boxVolume.dimensions.x),
            static_cast<float>(_boxVolume.dimensions.y),
            static_cast<float>(_boxVolume.dimensions.z)
        };
        if (ImGui::DragFloat3("Dimensions (pc)", dims, 1.0f, 0.1f, 100000.0f, "%.1f")) {
            _boxVolume.dimensions = glm::dvec3(
                std::max(0.1f, dims[0]),
                std::max(0.1f, dims[1]),
                std::max(0.1f, dims[2])
            );
            changed = true;
        }
    }

    if (changed && _liveUpdate) {
        applyCurrentSelection();
    }
}

void SpatialSelectionView::renderSkyMapTab() {
    if (_skyMapCacheDirty || _dataViewer.filterChanged()) {
        updateSkyMapCache();
    }

    bool changed = false;

    ImGui::TextUnformatted("Night Sky Map (Earth ICRS: RA & Dec)");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Click and drag the box inside the plot to change the celestial selection area.\n"
        "RA: [0°, 360°], Dec: [-90°, +90°]"
    );

    const ImVec2 plotSize = ImVec2(-1.0f, 240.0f);
    constexpr ImPlotFlags plotFlags = ImPlotFlags_None;

    if (ImPlot::BeginPlot("##SkyMapPlot", plotSize, plotFlags)) {
        ImPlot::SetupAxes("Right Ascension (deg)", "Declination (deg)");
        ImPlot::SetupAxesLimits(0.0, 360.0, -90.0, 90.0, ImGuiCond_FirstUseEver);

        if (!_cachedRa.empty()) {
            ImPlot::PlotScatter(
                "Planets",
                _cachedRa.data(),
                _cachedDec.data(),
                static_cast<int>(_cachedRa.size())
            );
        }

        // Draggable Selection Rectangle on the Sky Map
        double x1 = _skyMapRect.raMin;
        double y1 = _skyMapRect.decMin;
        double x2 = _skyMapRect.raMax;
        double y2 = _skyMapRect.decMax;

        const ImVec4 rectColor = ImVec4(1.0f, 0.75f, 0.0f, 0.85f);
        if (ImPlot::DragRect(0, &x1, &y1, &x2, &y2, rectColor)) {
            _skyMapRect.raMin = std::clamp(std::min(x1, x2), 0.0, 360.0);
            _skyMapRect.raMax = std::clamp(std::max(x1, x2), 0.0, 360.0);
            _skyMapRect.decMin = std::clamp(std::min(y1, y2), -90.0, 90.0);
            _skyMapRect.decMax = std::clamp(std::max(y1, y2), -90.0, 90.0);
            changed = true;
        }

        ImPlot::EndPlot();
    }

    // Direct numeric bounds controls
    float raRange[2] = {
        static_cast<float>(_skyMapRect.raMin),
        static_cast<float>(_skyMapRect.raMax)
    };
    if (ImGui::DragFloat2("RA Range (deg)", raRange, 0.5f, 0.0f, 360.0f, "%.1f°")) {
        _skyMapRect.raMin = std::clamp(static_cast<double>(raRange[0]), 0.0, 360.0);
        _skyMapRect.raMax = std::clamp(static_cast<double>(raRange[1]), 0.0, 360.0);
        changed = true;
    }

    float decRange[2] = {
        static_cast<float>(_skyMapRect.decMin),
        static_cast<float>(_skyMapRect.decMax)
    };
    if (ImGui::DragFloat2("Dec Range (deg)", decRange, 0.5f, -90.0f, 90.0f, "%.1f°")) {
        _skyMapRect.decMin = std::clamp(static_cast<double>(decRange[0]), -90.0, 90.0);
        _skyMapRect.decMax = std::clamp(static_cast<double>(decRange[1]), -90.0, 90.0);
        changed = true;
    }

    if (ImGui::Checkbox("Filter by Distance", &_skyMapRect.useDistanceFilter)) {
        changed = true;
    }
    if (_skyMapRect.useDistanceFilter) {
        float distRange[2] = {
            static_cast<float>(_skyMapRect.distMin),
            static_cast<float>(_skyMapRect.distMax)
        };
        if (ImGui::DragFloat2("Distance Range (pc)", distRange, 5.0f, 0.0f, 100000.0f,
            "%.0f pc"))
        {
            _skyMapRect.distMin = std::max(0.0, static_cast<double>(distRange[0]));
            _skyMapRect.distMax = std::max(0.0, static_cast<double>(distRange[1]));
            changed = true;
        }
    }

    if (changed && _liveUpdate) {
        applyCurrentSelection();
    }
}

void SpatialSelectionView::renderDensityTab() {
    // Density selection is disabled for now
    ImGui::TextWrapped(
        "Density selection is currently disabled."
    );

    //ImGui::TextWrapped(
    //    "Density selection selects points located in denser clusters in 3D space. "
    //    "Configuring neighborhood parameters allows selecting high-density clusters."
    //);
    //ImGui::Spacing();

    //bool changed = false;
    //float r = static_cast<float>(_densitySettings.searchRadius);
    //if (ImGui::DragFloat("Search Radius (pc)", &r, 0.5f, 0.1f, 1000.0f, "%.1f pc")) {
    //    _densitySettings.searchRadius = std::max(0.1, static_cast<double>(r));
    //    changed = true;
    //}

    //if (ImGui::SliderInt("Min Neighbors", &_densitySettings.minNeighbors, 1, 100)) {
    //    changed = true;
    //}

    //if (changed && _liveUpdate) {
    //    applyCurrentSelection();
    //}
}

void SpatialSelectionView::renderCurrentSelectionActions() {
    std::vector<size_t> currentSel = computeCurrentSpatialSelection();
    const size_t totalCandidates = _dataViewer.currentFiltering().size();
    const double pct = totalCandidates > 0
        ? (100.0 * static_cast<double>(currentSel.size()) /
           static_cast<double>(totalCandidates))
        : 0.0;

    ImGui::Text(
        "Current spatial query: %zu / %zu items (%.1f%%)",
        currentSel.size(),
        totalCandidates,
        pct
    );

    ImGui::Checkbox("Live Update", &_liveUpdate);
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "When enabled, moving spatial sliders or dragging on the sky map immediately "
        "updates the active selection."
    );

    if (ImGui::Button("Apply to Active Selection")) {
        applyCurrentSelection();
    }
    ImGui::SameLine();
    if (ImGui::Button("Clear Selection")) {
        _dataViewer.setSelection({});
    }

    ImGui::Spacing();
    ImGui::PushItemWidth(180);
    ImGui::InputTextWithHint(
        "##SaveName",
        "Selection name...",
        _saveNameBuffer,
        IM_ARRAYSIZE(_saveNameBuffer)
    );
    ImGui::PopItemWidth();
    ImGui::SameLine();

    if (ImGui::Button("Save Current Selection")) {
        std::string summary;
        switch (_currentMethod) {
            case SpatialSelectionMethod::Shape3D:
                if (_shape3DType == Shape3DType::Sphere) {
                    summary = std::format(
                        "Sphere (r={:.0f}pc @ {:.0f},{:.0f},{:.0f})",
                        _sphereVolume.radius,
                        _sphereVolume.center.x,
                        _sphereVolume.center.y,
                        _sphereVolume.center.z
                    );
                }
                else {
                    summary = std::format(
                        "Box ({:.0f}x{:.0f}x{:.0f}pc @ {:.0f},{:.0f},{:.0f})",
                        _boxVolume.dimensions.x,
                        _boxVolume.dimensions.y,
                        _boxVolume.dimensions.z,
                        _boxVolume.center.x,
                        _boxVolume.center.y,
                        _boxVolume.center.z
                    );
                }
                break;
            case SpatialSelectionMethod::SkyMap:
                summary = std::format(
                    "SkyMap (RA:[{:.0f}°,{:.0f}°], Dec:[{:.0f}°,{:.0f}°])",
                    _skyMapRect.raMin,
                    _skyMapRect.raMax,
                    _skyMapRect.decMin,
                    _skyMapRect.decMax
                );
                break;
            case SpatialSelectionMethod::Density:
                summary = std::format(
                    "Density (r={:.0f}pc, min={})",
                    _densitySettings.searchRadius,
                    _densitySettings.minNeighbors
                );
                break;
        }

        std::string name = _saveNameBuffer;
        _handler.addSavedSelection(name, summary, currentSel);
        _saveNameBuffer[0] = '\0'; // reset buffer
    }
}

void SpatialSelectionView::renderSavedSelectionsManager() {
    ImGui::TextUnformatted("Saved Selections:");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Save multiple selections, enable/disable individual ones, and apply their "
        "combination at once."
    );

    std::vector<SavedSelection>& saved = _handler.savedSelections();
    if (saved.empty()) {
        ImGui::TextDisabled("No saved selections yet.");
        return;
    }

    if (ImGui::Button("Apply Enabled Selections")) {
        applyCombinedSavedSelections();
    }
    ImGui::SameLine();
    if (ImGui::Button("Clear All Saved")) {
        _handler.clearSavedSelections();
        return;
    }

    ImGui::BeginChild("SavedSelectionsList", ImVec2(0, 150), true);
    size_t toRemove = static_cast<size_t>(-1);

    for (size_t i = 0; i < saved.size(); ++i) {
        ImGui::PushID(static_cast<int>(i));

        ImGui::Checkbox("##enabled", &saved[i].isEnabled);
        ImGui::SameLine();

        ImGui::Text("%s (%zu items)", saved[i].name.c_str(), saved[i].indices.size());
        if (!saved[i].summary.empty()) {
            ImGui::SameLine();
            ImGui::TextDisabled("[%s]", saved[i].summary.c_str());
        }

        ImGui::SameLine(ImGui::GetWindowWidth() - 75);
        if (ImGui::SmallButton("Load")) {
            _dataViewer.setSelection(saved[i].indices);
        }
        ImGui::SameLine();
        if (ImGui::SmallButton("X")) {
            toRemove = i;
        }

        ImGui::PopID();
    }

    if (toRemove != static_cast<size_t>(-1)) {
        _handler.removeSavedSelection(toRemove);
    }

    ImGui::EndChild();
}

} // namespace openspace::exoplanets
