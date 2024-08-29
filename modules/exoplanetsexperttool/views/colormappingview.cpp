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

#include <modules/exoplanetsexperttool/views/colormappingview.h>

#include <modules/exoplanetsexperttool/datahelper.h>
#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <implot.h>

namespace openspace::exoplanets {

ColorMappingView::ColorMappingView(DataViewer& dataViewer,
                                   const DataSettings& dataSettings)
    : _dataViewer(dataViewer)
{
    // Must match names in implot and customly added ones
    _colormaps = {
        "Viridis",
        "Plasma",
        "Hot",
        "Cool",
        "Autumn", // custom
        "Spring", // custom
        "Summer", // custom
        "Winter", // custom
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

    for (size_t i = 0; i < _dataViewer.columns().size(); ++i) {
        if (_dataViewer.isNumericColumn(i)) {
            _firstNumericColumnIndex = i;
            break;
        }
    }

    ColorMappedVariable newVariable = { .columnIndex = _firstNumericColumnIndex };
    if (dataSettings.defaultColormapping.has_value()) {
        DataSettings::CmapInfo info = *dataSettings.defaultColormapping;
        newVariable = {
            .columnIndex = _dataViewer.columnIndex(info.column),
            .colorScaleMin = info.min,
            .colorScaleMax = info.max
        };
    }
    _variableSelection.push_back(newVariable);

    // TODO: make sure that settings are preserved between sessions?
}

void ColorMappingView::initializeGL() {
    //  TODO: These do not work when using multiple windows :(
    // Probably has to do with contexts

    // Intilize custom color maps (generated from matplotlib).
    // Note that this requires an OpenGL context

    const ImVec4 autumn[] = {
        ImVec4(1.f, 0.f,         0.f, 1.f),
        ImVec4(1.f, 0.14117647f, 0.f, 1.f),
        ImVec4(1.f, 0.28627451f, 0.f, 1.f),
        ImVec4(1.f, 0.42745098f, 0.f, 1.f),
        ImVec4(1.f, 0.57254902f, 0.f, 1.f),
        ImVec4(1.f, 0.71372549f, 0.f, 1.f),
        ImVec4(1.f, 0.85882353f, 0.f, 1.f),
        ImVec4(1.f, 1.f,         0.f, 1.f)
    };

    const ImVec4 spring[] = {
        ImVec4(1.f, 0.f,         1.f,         1.f),
        ImVec4(1.f, 0.14117647f, 0.85882353f, 1.f),
        ImVec4(1.f, 0.28627451f, 0.71372549f, 1.f),
        ImVec4(1.f, 0.42745098f, 0.57254902f, 1.f),
        ImVec4(1.f, 0.57254902f, 0.42745098f, 1.f),
        ImVec4(1.f, 0.71372549f, 0.28627451f, 1.f),
        ImVec4(1.f, 0.85882353f, 0.14117647f, 1.f),
        ImVec4(1.f, 1.f,         0.f,         1.f)
    };

    const ImVec4 summer[] = {
        ImVec4(0.f,         0.5f,        0.4f, 1.f),
        ImVec4(0.14117647f, 0.57058824f, 0.4f, 1.f),
        ImVec4(0.28627451f, 0.64313725f, 0.4f, 1.f),
        ImVec4(0.42745098f, 0.71372549f, 0.4f, 1.f),
        ImVec4(0.57254902f, 0.78627451f, 0.4f, 1.f),
        ImVec4(0.71372549f, 0.85686275f, 0.4f, 1.f),
        ImVec4(0.85882353f, 0.92941176f, 0.4f, 1.f),
        ImVec4(1.f,         1.f,         0.4f, 1.f)
    };

    const ImVec4 winter[] = {
        ImVec4(0.f, 0.f,         1.f,         1.f),
        ImVec4(0.f, 0.14117647f, 0.92941176f, 1.f),
        ImVec4(0.f, 0.28627451f, 0.85686275f, 1.f),
        ImVec4(0.f, 0.42745098f, 0.78627451f, 1.f),
        ImVec4(0.f, 0.57254902f, 0.71372549f, 1.f),
        ImVec4(0.f, 0.71372549f, 0.64313725f, 1.f),
        ImVec4(0.f, 0.85882353f, 0.57058824f, 1.f),
        ImVec4(0.f, 1.f,         0.5f,        1.f)
    };

    ImPlot::AddColormap("Autumn", autumn, 8, false);
    ImPlot::AddColormap("Spring", spring, 8, false);
    ImPlot::AddColormap("Summer", summer, 8, false);
    ImPlot::AddColormap("Winter", winter, 8, false);
}

const std::vector<ColorMappingView::ColorMappedVariable>&
ColorMappingView::colorMapperVariables() const
{
    return _variableSelection;
}

size_t ColorMappingView::firstNumericColumn() const {
    return _firstNumericColumnIndex;
}

bool ColorMappingView::renderViewContent() {
    bool cmapWasChanged = false;

    // Start variable group
    ImGui::BeginGroup();

    ImGui::BeginGroup();

    // Colormap for each selected variable
    if (ImGui::Button("+ Add variable")) {
        if (_variableSelection.size() < 8) {
            ColorMappedVariable newVariable = { .columnIndex = _firstNumericColumnIndex };
            _variableSelection.push_back(newVariable);
            cmapWasChanged = true;
        }
    };
    ImGui::SameLine();

    // NaNColor
    ImGuiColorEditFlags nanColorFlags = ImGuiColorEditFlags_NoInputs |
        ImGuiColorEditFlags_NoLabel | ImGuiColorEditFlags_AlphaPreview |
        ImGuiColorEditFlags_AlphaBar;
    static ImVec4 c = view::helper::toImVec4(_nanPointColor);
    if (ImGui::ColorEdit4("NanColor", (float*)&c, nanColorFlags)) {
        _nanPointColor = { c.x, c.y, c.z, c.w };
        cmapWasChanged = true;
    }
    ImGui::SameLine();
    ImGui::Text("No value color");

    ImGui::EndGroup();

    ImGui::Spacing();

    ImGui::BeginGroup();

    // Note the reverse ordering
    for (int index = static_cast<int>(_variableSelection.size()) - 1; index >= 0; --index) {
        ColorMappedVariable& variable = _variableSelection[index];

        ImGui::PushID(std::format("##variable{}", index).c_str());

        ImGui::Text(std::format("{}.", index + 1).c_str());
        ImGui::SameLine();

        // Entire variable group
        cmapWasChanged |= renderColormapEdit(variable);

        ImGui::PopID();
        ImGui::SameLine();

        ImGui::PushID(std::format("##remove{}", index).c_str());
        if (_variableSelection.size() > 1 && ImGui::Button("x")) {
            _variableSelection.erase(_variableSelection.begin() + index);
            cmapWasChanged = true;
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
        // the entire lifetime of the char * array
        std::vector<std::string> labelStrings;
        labelStrings.reserve(nVariables);
        for (int i = 0; i < nVariables; ++i) {
            std::string label = _dataViewer.columnName(
                _dataViewer.columns()[_variableSelection[i].columnIndex]
            );
            label = label.substr(0, 10); // limit length
            labelStrings.push_back(std::format(" {}. {}", i + 1, label));
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

        if (ImPlot::BeginPlot(
                "##Pie",
                NULL, NULL,
                plotSize,
                ImPlotFlags_Equal | ImPlotFlags_NoMousePos,
                ImPlotAxisFlags_NoDecorations,
                ImPlotAxisFlags_NoDecorations
            ))
        {
            ImPlot::PlotPieChart(labels.data(), data.data(), nVariables, 1.1, 0.5, 0.3, true, NULL);
            ImPlot::EndPlot();
        }
    }

    ImGui::EndGroup(); // variables + plot group

    ImGui::End();

    return cmapWasChanged;
}

bool ColorMappingView::renderColormapEdit(ColorMappedVariable& variable,
                                          std::string_view relevantSystem)
{
    constexpr const int InputWidth = 120;
    bool wasChanged = false;

    ImGui::BeginGroup();
    {
        ImGui::SetNextItemWidth(InputWidth);
        if (ImGui::BeginCombo(
                "Column",
                _dataViewer.columnName(variable.columnIndex)
            ))
        {
            for (int i = 0; i < _dataViewer.columns().size(); ++i) {
                // Ignore non-numeric columns
                if (!_dataViewer.isNumericColumn(i)) {
                    continue;
                }

                const char* name = _dataViewer.columnName(i);
                if (ImGui::Selectable(name, variable.columnIndex == i)) {
                    variable.columnIndex = i;
                    wasChanged = true;
                }

                _dataViewer.renderColumnDescriptionTooltip(i);
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

        size_t colormapColumn = variable.columnIndex;

        // Min/max values for color range
        ImGui::SetNextItemWidth(InputWidth);
        if (ImGui::DragFloatRange2("Min / Max", &variable.colorScaleMin, &variable.colorScaleMax, 1.f)) {
            wasChanged = true;
        }

        bool updateMinMax = false;

        std::vector<size_t> relevantIndices;

        const std::vector<ExoplanetItem>& data = _dataViewer.data();

        if (!relevantSystem.empty() && ImGui::SmallButton("Set from planets in system")) {
            relevantIndices = _dataViewer.planetsForHost(std::string(relevantSystem));
            updateMinMax = true;
        }
        else if (ImGui::SmallButton("Set from current table data")) {
            relevantIndices = _dataViewer.currentFiltering();
            updateMinMax = true;
        }
        else if (ImGui::SmallButton("Set from full data")) {
            std::vector<size_t> v(data.size()); // same number of indices as data
            std::iota(std::begin(v), std::end(v), 0);
            relevantIndices = std::move(v);
            updateMinMax = true;
        }

        if (updateMinMax && !relevantIndices.empty()) {
            float newMin = std::numeric_limits<float>::max();
            float newMax = std::numeric_limits<float>::lowest();

            for (size_t i : relevantIndices) {
                const ExoplanetItem& item = data[i];
                auto value = _dataViewer.columnValue(_dataViewer.columns()[colormapColumn], item);
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

        // Render an opacity slider
        ImGui::SetNextItemWidth(InputWidth);
        if (ImGui::SliderFloat("Opacity", &variable.opacity, 0.f, 1.f)) {
            wasChanged = true;
        }
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

glm::vec4 ColorMappingView::colorFromColormap(const ExoplanetItem& item,
                                              const ColorMappedVariable& variable)
{
    const ColumnKey& colormapColumn = _dataViewer.columns()[variable.columnIndex];

    std::variant<const char*, float> value = _dataViewer.columnValue(colormapColumn, item);
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
    // Apply opacity
    pointColor.a *= variable.opacity;

    return pointColor;
}

} // namespace openspace::exoplanets
