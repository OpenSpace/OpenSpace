/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/imgui/include/guiperformancecomponent.h>

#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/performance/performancelayout.h>
#include <openspace/performance/performancemanager.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/misc/sharedmemory.h>
#include <array>
#include <numeric>

namespace {
    enum Sorting {
        NoSorting = -1,
        UpdateTranslation = 0,
        UpdateRotation = 1,
        UpdateScaling = 2,
        UpdateRender = 3,
        Render = 4,
        Total = 5
    };

    constexpr openspace::properties::Property::PropertyInfo SortingSelectionInfo = {
        "SortingSelection",
        "Sorting",
        "This value determines the sorting order of the performance measurements."
    };

    constexpr openspace::properties::Property::PropertyInfo SceneGraphEnabledInfo = {
        "ShowSceneGraph",
        "Show Scene Graph Measurements",
        "If this value is enabled, the window showing the measurements for the scene "
        "graph values is visible."
    };

    constexpr openspace::properties::Property::PropertyInfo FunctionsEnabledInfo = {
        "ShowFunctions",
        "Show Function Measurements",
        "If this value is enabled, the window showing the measurements for the "
        "individual functions is visible."
    };

    constexpr openspace::properties::Property::PropertyInfo OutputLogsInfo = {
        "OutputLogs",
        "Output Logs",
        "" // @TODO Missing documentation
    };

} // namespace

namespace openspace::gui {

GuiPerformanceComponent::GuiPerformanceComponent()
    : GuiComponent("PerformanceComponent", "Performance Component")
    , _sortingSelection(SortingSelectionInfo, -1, -1, 6)
    , _sceneGraphIsEnabled(SceneGraphEnabledInfo, false)
    , _functionsIsEnabled(FunctionsEnabledInfo, false)
    , _outputLogs(OutputLogsInfo, false)
{
    addProperty(_sortingSelection);

    addProperty(_sceneGraphIsEnabled);
    addProperty(_functionsIsEnabled);
    addProperty(_outputLogs);
}

GuiPerformanceComponent::~GuiPerformanceComponent() {} // NOLINT

void GuiPerformanceComponent::render() {
    if (!global::performanceManager.isEnabled()) {
        return;
    }

    using ghoul::SharedMemory;
    using namespace performance;

    ImGui::SetNextWindowCollapsed(_isCollapsed);
    bool v = _isEnabled;
    ImGui::Begin("Performance", &v);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    PerformanceLayout* layout = global::performanceManager.performanceData();

    v = _sceneGraphIsEnabled;
    ImGui::Checkbox("SceneGraph", &v);
    _sceneGraphIsEnabled = v;
    v = _functionsIsEnabled;
    ImGui::Checkbox("Functions", &v);
    _functionsIsEnabled = v;
    v = _outputLogs;
    ImGui::Checkbox("Output Logs", &v);
    global::performanceManager.setLogging(v);
    // Need to catch if it's unsuccessful
    v = global::performanceManager.loggingEnabled();
    _outputLogs = v;

    ImGui::Spacing();

    if (ImGui::Button("Reset measurements")) {
        global::performanceManager.resetPerformanceMeasurements();
    }

    if (_sceneGraphIsEnabled) {
        bool sge = _sceneGraphIsEnabled;
        ImGui::Begin("SceneGraph", &sge);
        _sceneGraphIsEnabled = sge;

        // The indices correspond to the index into the average array further below
        ImGui::Text("Sorting");
        int sorting = _sortingSelection;
        ImGui::RadioButton("No Sorting",        &sorting, Sorting::NoSorting);
        ImGui::RadioButton("UpdateTranslation", &sorting, Sorting::UpdateTranslation);
        ImGui::RadioButton("UpdateRotation",    &sorting, Sorting::UpdateRotation);
        ImGui::RadioButton("UpdateScaling",     &sorting, Sorting::UpdateScaling);
        ImGui::RadioButton("UpdateRender",      &sorting, Sorting::UpdateRender);
        ImGui::RadioButton("RenderTime",        &sorting, Sorting::Render);
        ImGui::RadioButton("TotalTime",         &sorting, Sorting::Total);
        _sortingSelection = sorting;

        // Later, we will sort this indices list instead of the real values for
        // performance reasons
        std::vector<size_t> indices(layout->nScaleGraphEntries);
        std::iota(indices.begin(), indices.end(), 0);

        // Ordering:
        // updateTranslation
        // updateRotation
        // updateScaling
        // UpdateRender
        // RenderTime
        std::vector<std::array<float, 5>> averages(
            layout->nScaleGraphEntries,
            { 0.f, 0.f, 0.f, 0.f, 0.f }
        );

        std::vector<std::array<std::pair<float, float>, 5>> minMax(
            layout->nScaleGraphEntries
        );

        for (int i = 0; i < layout->nScaleGraphEntries; ++i) {
            const PerformanceLayout::SceneGraphPerformanceLayout& entry =
                layout->sceneGraphEntries[i];

            int nValues[5] = { 0, 0, 0, 0, 0 };

            // Compute the averages and count the number of values so we don't divide
            // by 0 later
            for (int j = 0; j < PerformanceLayout::NumberValues; ++j) {
                averages[i][0] += entry.updateTranslation[j];
                if (entry.updateTranslation[j] != 0.f) {
                    ++(nValues[0]);
                }
                averages[i][1] += entry.updateRotation[j];
                if (entry.updateRotation[j] != 0.f) {
                    ++(nValues[1]);
                }
                averages[i][2] += entry.updateScaling[j];
                if (entry.updateScaling[j] != 0.f) {
                    ++(nValues[2]);
                }
                averages[i][3] += entry.updateRenderable[j];
                if (entry.updateRenderable[j] != 0.f) {
                    ++(nValues[3]);
                }
                averages[i][4] += entry.renderTime[j];
                if (entry.renderTime[j] != 0.f) {
                    ++(nValues[4]);
                }
            }

            if (nValues[0] != 0) {
                averages[i][0] /= static_cast<float>(nValues[0]);
            }
            if (nValues[1] != 0) {
                averages[i][1] /= static_cast<float>(nValues[1]);
            }
            if (nValues[2] != 0) {
                averages[i][2] /= static_cast<float>(nValues[2]);
            }
            if (nValues[3] != 0) {
                averages[i][3] /= static_cast<float>(nValues[3]);
            }
            if (nValues[4] != 0) {
                averages[i][4] /= static_cast<float>(nValues[4]);
            }

            // Get the minimum/maximum values for each of the components so that we
            // can scale the plot by these numbers
            auto minmaxTranslation = std::minmax_element(
                std::begin(entry.updateTranslation),
                std::end(entry.updateTranslation)
            );
            minMax[i][0] = std::make_pair(
                *(minmaxTranslation.first),
                *(minmaxTranslation.second)
            );

            auto minmaxRotation = std::minmax_element(
                std::begin(entry.updateRotation),
                std::end(entry.updateRotation)
            );
            minMax[i][1] = std::make_pair(
                *(minmaxRotation.first),
                *(minmaxRotation.second)
            );

            auto minmaxScaling = std::minmax_element(
                std::begin(entry.updateScaling),
                std::end(entry.updateScaling)
            );
            minMax[i][2] = std::make_pair(
                *(minmaxScaling.first),
                *(minmaxScaling.second)
            );

            auto minmaxUpdateRenderable = std::minmax_element(
                std::begin(entry.updateRenderable),
                std::end(entry.updateRenderable)
            );
            minMax[i][3] = std::make_pair(
                *(minmaxUpdateRenderable.first),
                *(minmaxUpdateRenderable.second)
            );

            auto minmaxRendering = std::minmax_element(
                std::begin(entry.renderTime),
                std::end(entry.renderTime)
            );
            minMax[i][4] = std::make_pair(
                *(minmaxRendering.first),
                *(minmaxRendering.second)
            );
        }

        // If we don't want to sort, we will leave the indices list alone, thus
        // leaving them in the regular ordering
        Sorting selection = Sorting(sorting);
        if (selection != Sorting::NoSorting) {
            std::function<bool(size_t, size_t)> sortFunc;

            if (selection == Sorting::Total) {
                // If we do want to sort totally, we need to sum all the averages and
                // use that as the criterion
                sortFunc = [&averages](size_t a, size_t b) {
                    const float sumA = std::accumulate(
                        std::begin(averages[a]),
                        std::end(averages[a]),
                        0.f
                    );

                    const float sumB = std::accumulate(
                        std::begin(averages[b]),
                        std::end(averages[b]),
                        0.f
                    );

                    return sumA > sumB;
                };
            }
            else {
                // otherwise we use the sorting index
                sortFunc = [sel = _sortingSelection, &averages](size_t a, size_t b) {
                    return averages[a][sel] > averages[b][sel];
                };
            }

            std::sort(indices.begin(), indices.end(), sortFunc);
        }
        else {
            // NoSorting -> So we sort by names instead
            std::sort(
                indices.begin(),
                indices.end(),
                [layout](size_t a, size_t b) {
                    return std::string(layout->sceneGraphEntries[a].name) <
                           std::string(layout->sceneGraphEntries[b].name);
                }
            );
        }

        for (int i = 0; i < layout->nScaleGraphEntries; ++i) {
            // We are using the indices list as an additional level of indirection
            // into the respective values so that the list will be sorted by whatever
            // criterion we selected previously

            const PerformanceLayout::SceneGraphPerformanceLayout& entry =
                layout->sceneGraphEntries[indices[i]];

            if (ImGui::CollapsingHeader(entry.name)) {
                const std::string& updateTranslationTime = std::to_string(
                    entry.updateTranslation[PerformanceLayout::NumberValues - 1]
                ) + "us";

                ImGui::PlotLines(
                    fmt::format(
                        "UpdateTranslation\nAverage: {}us",
                        averages[indices[i]][0]
                    ).c_str(),
                    &entry.updateTranslation[0],
                    PerformanceLayout::NumberValues,
                    0,
                    updateTranslationTime.c_str(),
                    minMax[indices[i]][0].first,
                    minMax[indices[i]][0].second,
                    ImVec2(0, 40)
                );

                const std::string& updateRotationTime = std::to_string(
                    entry.updateRotation[PerformanceLayout::NumberValues - 1]
                ) + "us";

                ImGui::PlotLines(
                    fmt::format(
                        "UpdateRotation\nAverage: {}us",
                        averages[indices[i]][1]
                    ).c_str(),
                    &entry.updateRotation[0],
                    PerformanceLayout::NumberValues,
                    0,
                    updateRotationTime.c_str(),
                    minMax[indices[i]][1].first,
                    minMax[indices[i]][1].second,
                    ImVec2(0, 40)
                );

                const std::string& updateScalingTime = std::to_string(
                    entry.updateScaling[PerformanceLayout::NumberValues - 1]
                ) + "us";

                ImGui::PlotLines(
                    fmt::format(
                        "UpdateScaling\nAverage: {}us",
                        averages[indices[i]][2]
                    ).c_str(),
                    &entry.updateScaling[0],
                    PerformanceLayout::NumberValues,
                    0,
                    updateScalingTime.c_str(),
                    minMax[indices[i]][2].first,
                    minMax[indices[i]][2].second,
                    ImVec2(0, 40)
                );

                const std::string& updateRenderableTime = std::to_string(
                    entry.updateRenderable[PerformanceLayout::NumberValues - 1]
                ) + "us";

                ImGui::PlotLines(
                    fmt::format(
                        "UpdateRender\nAverage: {}us",
                        averages[indices[i]][3]
                    ).c_str(),
                    &entry.updateRenderable[0],
                    PerformanceLayout::NumberValues,
                    0,
                    updateRenderableTime.c_str(),
                    minMax[indices[i]][3].first,
                    minMax[indices[i]][3].second,
                    ImVec2(0, 40)
                );

                const std::string& renderTime = std::to_string(
                    entry.renderTime[PerformanceLayout::NumberValues - 1]
                ) + "us";

                ImGui::PlotLines(
                    fmt::format(
                        "RenderTime\nAverage: {}us",
                        averages[indices[i]][4]
                    ).c_str(),
                    &entry.renderTime[0],
                    PerformanceLayout::NumberValues,
                    0,
                    renderTime.c_str(),
                    minMax[indices[i]][4].first,
                    minMax[indices[i]][4].second,
                    ImVec2(0, 40)
                );
            }
        }
        ImGui::End();
    }

    if (_functionsIsEnabled) {
        bool fe = _functionsIsEnabled;
        ImGui::Begin("Functions", &fe);
        _functionsIsEnabled = fe;

        for (int i = 0; i < layout->nFunctionEntries; ++i) {
            using namespace performance;

            const PerformanceLayout::FunctionPerformanceLayout& entry =
                layout->functionEntries[i];

            float avg = 0.f;
            int count = 0;
            for (int j = 0; j < PerformanceLayout::NumberValues; ++j) {
                avg += layout->functionEntries[i].time[j];
                if (layout->functionEntries[i].time[j] != 0.f) {
                    ++count;
                }
            }
            if (count > 0) {
                avg /= count;
            }

            auto minmax = std::minmax_element(
                std::begin(layout->functionEntries[i].time),
                std::end(layout->functionEntries[i].time)
            );

            const std::string& renderTime = std::to_string(
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

    ImGui::End();
}

} // namespace openspace::gui
