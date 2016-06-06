/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/onscreengui/include/guiperformancecomponent.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/performance/performancelayout.h>
#include <openspace/performance/performancemanager.h>
#include <openspace/rendering/renderengine.h>

#include <ghoul/misc/sharedmemory.h>

#include <imgui.h>
#include <cppformat/format.h>

#include <algorithm>
#include <numeric>

namespace {
    const std::string _loggerCat = "GuiPerformanceComponent";
}

namespace openspace {
namespace gui {

void GuiPerformanceComponent::initialize() {
    _minMaxValues[0] = 100.f;
    _minMaxValues[1] = 250.f;
    _sortingSelection = -1;
}

void GuiPerformanceComponent::deinitialize() {
    delete _performanceMemory;
    _performanceMemory = nullptr;
}

void GuiPerformanceComponent::render() {
    using namespace performance;

    ImGui::Begin("Performance", &_isEnabled);
    if (OsEng.renderEngine().doesPerformanceMeasurements() &&
            ghoul::SharedMemory::exists(PerformanceManager::PerformanceMeasurementSharedData))
    {
        ImGui::SliderFloat2("Min values, max Value", _minMaxValues, 0.f, 10000.f);
        _minMaxValues[1] = fmaxf(_minMaxValues[0], _minMaxValues[1]);
        
        if (ImGui::CollapsingHeader("SceneGraph")) {
            // The indices correspond to the index into the average array further below
            ImGui::Text("Sorting");
            ImGui::RadioButton("No Sorting", &_sortingSelection, -1);
            ImGui::RadioButton("UpdateEphemeris", &_sortingSelection, 0);
            ImGui::RadioButton("UpdateRender", &_sortingSelection, 1);
            ImGui::RadioButton("RenderTime", &_sortingSelection, 2);

            if (!_performanceMemory)
                _performanceMemory = new ghoul::SharedMemory(PerformanceManager::PerformanceMeasurementSharedData);

            void* ptr = _performanceMemory->memory();

            PerformanceLayout* layout = reinterpret_cast<PerformanceLayout*>(ptr);

            std::vector<size_t> indices(layout->nScaleGraphEntries);
            std::iota(indices.begin(), indices.end(), 0);

            // Ordering:
            // updateEphemeris
            // UpdateRender
            // RenderTime
            std::vector<std::array<float, 3>> averages(layout->nScaleGraphEntries, { 0.f, 0.f, 0.f });

            for (int i = 0; i < layout->nScaleGraphEntries; ++i) {
                const PerformanceLayout::SceneGraphPerformanceLayout& entry = layout->sceneGraphEntries[i];

                int v[3] = { 0, 0, 0 };

                for (int j = 0; j < PerformanceLayout::NumberValues; ++j) {
                    averages[i][0] += entry.updateEphemeris[j];
                    if (entry.updateEphemeris[j] != 0.f)
                        ++v[0];
                    averages[i][1] += entry.updateRenderable[j];
                    if (entry.updateRenderable[j] != 0.f)
                        ++v[1];
                    averages[i][2] += entry.renderTime[j];
                    if (entry.renderTime[j] != 0.f)
                        ++v[2];
                }

                if (v[0] != 0)
                    averages[i][0] /= static_cast<float>(v[0]);
                if (v[1] != 0)
                    averages[i][1] /= static_cast<float>(v[1]);
                if (v[2] != 0)
                    averages[i][2] /= static_cast<float>(v[2]);
            }

            if (_sortingSelection != -1) {
                int sortIndex = _sortingSelection;

                std::sort(
                    indices.begin(),
                    indices.end(),
                    [sortIndex, &averages](size_t a, size_t b) {
                        return averages[a][sortIndex] > averages[b][sortIndex];
                    }
                );

            }

            for (int i = 0; i < layout->nScaleGraphEntries; ++i) {
                const PerformanceLayout::SceneGraphPerformanceLayout& entry = layout->sceneGraphEntries[indices[i]];

                if (ImGui::CollapsingHeader(entry.name)) {
                    std::string updateEphemerisTime = std::to_string(entry.updateEphemeris[entry.currentUpdateEphemeris - 1]) + "us";
                    ;
                    ImGui::PlotLines(
                        fmt::format("UpdateEphemeris\nAverage: {}us", averages[i][0]).c_str(),
                        &entry.updateEphemeris[0],
                        PerformanceLayout::NumberValues,
                        0,
                        updateEphemerisTime.c_str(),
                        _minMaxValues[0],
                        _minMaxValues[1],
                        ImVec2(0, 40)
                    );

                    std::string updateRenderableTime = std::to_string(entry.updateRenderable[entry.currentUpdateRenderable - 1]) + "us";
                    ImGui::PlotLines(
                        fmt::format("UpdateRender\nAverage: {}us", averages[i][1]).c_str(),
                        &entry.updateRenderable[0],
                        PerformanceLayout::NumberValues,
                        0,
                        updateRenderableTime.c_str(),
                        _minMaxValues[0],
                        _minMaxValues[1],
                        ImVec2(0, 40)
                    );

                    std::string renderTime = std::to_string(entry.renderTime[entry.currentRenderTime - 1]) + "us";
                    ImGui::PlotLines(
                        fmt::format("RenderTime\nAverage: {}us", averages[i][2]).c_str(),
                        &entry.renderTime[0],
                        PerformanceLayout::NumberValues,
                        0,
                        renderTime.c_str(),
                        _minMaxValues[0],
                        _minMaxValues[1],
                        ImVec2(0, 40)
                    );
                }
            }
        }
        
        if (ImGui::CollapsingHeader("Functions")) {
            using namespace performance;
            
            if (!_performanceMemory)
                _performanceMemory = new ghoul::SharedMemory(PerformanceManager::PerformanceMeasurementSharedData);
            
            void* ptr = _performanceMemory->memory();
            
            PerformanceLayout* layout = reinterpret_cast<PerformanceLayout*>(ptr);

            for (int i = 0; i < layout->nFunctionEntries; ++i) {
                const PerformanceLayout::FunctionPerformanceLayout& entry = layout->functionEntries[i];
                
                float avg = 0.f;
                int count = 0;
                for (int j = 0; j < PerformanceLayout::NumberValues; ++j) {
                    avg += layout->functionEntries[i].time[j];
                    if (layout->functionEntries[i].time[j] != 0.f)
                        ++count;
                }
                avg /= count;
                
                const PerformanceLayout::FunctionPerformanceLayout& f = layout->functionEntries[i];
                
                std::string renderTime = std::to_string(entry.time[entry.currentTime - 1]) + "us";
                ImGui::PlotLines(
                    fmt::format("{}\nAverage: {}us", entry.name, avg).c_str(),
                    &entry.time[0],
                    PerformanceLayout::NumberValues,
                    0,
                    renderTime.c_str(),
                    _minMaxValues[0],
                    _minMaxValues[1],
                    ImVec2(0, 40)
                );
            }
            
        }
    }
    else {
        ImGui::TextWrapped("Performance monitoring is disabled. Enable with "
                           "'openspace.setPerformanceMeasurement(true)'");
    }

    ImGui::End();
}

} // namespace gui
} // namespace openspace
