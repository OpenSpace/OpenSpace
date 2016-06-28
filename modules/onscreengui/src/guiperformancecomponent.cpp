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
#include <fmt/format.h>

#include <algorithm>
#include <numeric>

namespace {
    const std::string _loggerCat = "GuiPerformanceComponent";

    enum class Sorting {
        NoSorting = -1,
        UpdateEphemeris = 0,
        UpdateRender = 1,
        Render = 2,
        Total = 3
    };
}

namespace openspace {
namespace gui {

void GuiPerformanceComponent::render() {
    using ghoul::SharedMemory;
    using namespace performance;

    ImGui::Begin("Performance", &_isEnabled);
    if (OsEng.renderEngine().doesPerformanceMeasurements()) {
        ghoul_assert(
            SharedMemory::exists(PerformanceManager::PerformanceMeasurementSharedData),
            "Shared Memory block was not allocated"
        );

        if (!_performanceMemory) {
            _performanceMemory = std::make_unique<ghoul::SharedMemory>(
                PerformanceManager::PerformanceMeasurementSharedData
            );
        }
        PerformanceLayout* layout = reinterpret_cast<PerformanceLayout*>(
            _performanceMemory->memory()
        );

        ImGui::Checkbox("SceneGraph", &_sceneGraphIsEnabled);
        ImGui::Checkbox("Functions", &_functionsIsEnabled);
        
        ImGui::Spacing();
        
        if (ImGui::Button("Reset measurements")) {
            OsEng.renderEngine().performanceManager()->resetPerformanceMeasurements();
        }
        
        if (_sceneGraphIsEnabled) {
            ImGui::Begin("SceneGraph", &_sceneGraphIsEnabled);
        
            // The indices correspond to the index into the average array further below
            ImGui::Text("Sorting");
            ImGui::RadioButton(
                "No Sorting",
                &_sortingSelection,
                static_cast<int>(Sorting::NoSorting)
            );
            ImGui::RadioButton(
                "UpdateEphemeris",
                &_sortingSelection,
                static_cast<int>(Sorting::UpdateEphemeris)
            );
            ImGui::RadioButton(
                "UpdateRender",
                &_sortingSelection,
                static_cast<int>(Sorting::UpdateRender)
            );
            ImGui::RadioButton(
                "RenderTime",
                &_sortingSelection,
                static_cast<int>(Sorting::Render)
            );
            ImGui::RadioButton(
                "TotalTime",
                &_sortingSelection,
                static_cast<int>(Sorting::Total)
            );

            // Later, we will sort this indices list instead of the real values for
            // performance reasons
            std::vector<size_t> indices(layout->nScaleGraphEntries);
            std::iota(indices.begin(), indices.end(), 0);

            // Ordering:
            // updateEphemeris
            // UpdateRender
            // RenderTime
            std::vector<std::array<float, 3>> averages(
                layout->nScaleGraphEntries,
                { 0.f, 0.f, 0.f }
            );
            
            std::vector<std::array<std::pair<float, float>, 3>> minMax(
                layout->nScaleGraphEntries
            );
            
            for (int i = 0; i < layout->nScaleGraphEntries; ++i) {
                const PerformanceLayout::SceneGraphPerformanceLayout& entry =
                    layout->sceneGraphEntries[i];

                int nValues[3] = { 0, 0, 0 };
                
                // Compute the averages and count the number of values so we don't divide
                // by 0 later
                for (int j = 0; j < PerformanceLayout::NumberValues; ++j) {
                    averages[i][0] += entry.updateEphemeris[j];
                    if (entry.updateEphemeris[j] != 0.f)
                        ++(nValues[0]);
                    averages[i][1] += entry.updateRenderable[j];
                    if (entry.updateRenderable[j] != 0.f)
                        ++(nValues[1]);
                    averages[i][2] += entry.renderTime[j];
                    if (entry.renderTime[j] != 0.f)
                        ++(nValues[2]);
                }

                if (nValues[0] != 0)
                    averages[i][0] /= static_cast<float>(nValues[0]);
                if (nValues[1] != 0)
                    averages[i][1] /= static_cast<float>(nValues[1]);
                if (nValues[2] != 0)
                    averages[i][2] /= static_cast<float>(nValues[2]);
                
                // Get the minimum/maximum values for each of the components so that we
                // can scale the plot by these numbers
                auto minmaxEphemeris = std::minmax_element(
                    std::begin(entry.updateEphemeris),
                    std::end(entry.updateEphemeris)
                );
                minMax[i][0] = std::make_pair(
                    *(minmaxEphemeris.first),
                    *(minmaxEphemeris.second)
                );
                
                auto minmaxUpdateRenderable = std::minmax_element(
                    std::begin(entry.updateRenderable),
                    std::end(entry.updateRenderable)
                );
                minMax[i][1] = std::make_pair(
                    *(minmaxUpdateRenderable.first),
                    *(minmaxUpdateRenderable.second)
                );

                auto minmaxRendering = std::minmax_element(
                    std::begin(entry.renderTime),
                    std::end(entry.renderTime)
                );
                minMax[i][2] = std::make_pair(
                    *(minmaxRendering.first),
                    *(minmaxRendering.second)
                );
                

            }
            
            // If we don't want to sort, we will leave the indices list alone, thus
            // leaving them in the regular ordering
            Sorting selection = Sorting(_sortingSelection);
            if (selection != Sorting::NoSorting) {
                std::function<bool(size_t, size_t)> sortFunc;

                if (selection == Sorting::Total) {
                    // If we do want to sort totally, we need to sum all the averages and
                    // use that as the criterion
                    sortFunc = [&averages](size_t a, size_t b) {
                        float sumA = std::accumulate(
                            std::begin(averages[a]),
                            std::end(averages[a]),
                            0.f
                        );

                        float sumB = std::accumulate(
                            std::begin(averages[b]),
                            std::end(averages[b]),
                            0.f
                        );

                        return sumA > sumB;
                    };
                }
                else {
                    // otherwise we use the sorting index
                    int sel = _sortingSelection;
                    sortFunc = [sel, &averages](size_t a, size_t b) {
                        return averages[a][sel] > averages[b][sel];
                    };
                }

                std::sort(
                    indices.begin(),
                    indices.end(),
                    sortFunc
                );
            }

            for (int i = 0; i < layout->nScaleGraphEntries; ++i) {
                // We are using the indices list as an additional level of indirection
                // into the respective values so that the list will be sorted by whatever
                // criterion we selected previously

                const PerformanceLayout::SceneGraphPerformanceLayout& entry =
                    layout->sceneGraphEntries[indices[i]];

                if (ImGui::CollapsingHeader(entry.name)) {

                    std::string updateEphemerisTime = std::to_string(
                        entry.updateEphemeris[PerformanceLayout::NumberValues - 1]
                    ) + "us";

                    ImGui::PlotLines(
                        fmt::format(
                            "UpdateEphemeris\nAverage: {}us",
                            averages[indices[i]][0]
                        ).c_str(),
                        &entry.updateEphemeris[0],
                        PerformanceLayout::NumberValues,
                        0,
                        updateEphemerisTime.c_str(),
                        minMax[indices[i]][0].first,
                        minMax[indices[i]][0].second,
                        ImVec2(0, 40)
                    );

                    std::string updateRenderableTime = std::to_string(
                        entry.updateRenderable[PerformanceLayout::NumberValues - 1]
                    ) + "us";

                    ImGui::PlotLines(
                        fmt::format(
                            "UpdateRender\nAverage: {}us",
                            averages[indices[i]][1]
                        ).c_str(),
                        &entry.updateRenderable[0],
                        PerformanceLayout::NumberValues,
                        0,
                        updateRenderableTime.c_str(),
                        minMax[indices[i]][1].first,
                        minMax[indices[i]][1].second,
                        ImVec2(0, 40)
                    );

                    std::string renderTime = std::to_string(
                        entry.renderTime[PerformanceLayout::NumberValues - 1]
                    ) + "us";

                    ImGui::PlotLines(
                        fmt::format(
                            "RenderTime\nAverage: {}us",
                            averages[indices[i]][2]
                        ).c_str(),
                        &entry.renderTime[0],
                        PerformanceLayout::NumberValues,
                        0,
                        renderTime.c_str(),
                        minMax[indices[i]][2].first,
                        minMax[indices[i]][2].second,
                        ImVec2(0, 40)
                    );
                }
            }
            ImGui::End();
        }
        
        if (_functionsIsEnabled) {
            ImGui::Begin("Functions", &_functionsIsEnabled);
            using namespace performance;
            
            for (int i = 0; i < layout->nFunctionEntries; ++i) {
                const PerformanceLayout::FunctionPerformanceLayout& entry =
                    layout->functionEntries[i];
                
                float avg = 0.f;
                int count = 0;
                for (int j = 0; j < PerformanceLayout::NumberValues; ++j) {
                    avg += layout->functionEntries[i].time[j];
                    if (layout->functionEntries[i].time[j] != 0.f)
                        ++count;
                }
                avg /= count;
                
                auto minmax = std::minmax_element(
                    std::begin(layout->functionEntries[i].time),
                    std::end(layout->functionEntries[i].time)
                );
                
                const PerformanceLayout::FunctionPerformanceLayout& f =
                    layout->functionEntries[i];
                
                std::string renderTime = std::to_string(
                    entry.time[PerformanceLayout::NumberValues - 1]
                ) + "us";
                ImGui::PlotLines(
                    fmt::format("{}\nAverage: {}us", entry.name, avg).c_str(),
                    &entry.time[0],
                    PerformanceLayout::NumberValues,
                    0,
                    renderTime.c_str(),
                    *(minmax.first),
                    *(minmax.second),
                    ImVec2(0, 40)
                );
            }
            ImGui::End();
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
