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

#include <modules/exoplanetsexperttool/views/colormappingview.h>

#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/rendering/renderableexoplanetglyphcloud.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <implot.h>
#include <cmath>
#include <cstdio>
#include <string>
#include <vector>

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
        if (_variableSelection.size() < RenderableExoplanetGlyphCloud::MaxNumberColors) {
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

    //// Circle plot to show which parameters map to which part of a glyph
    //ImGui::SameLine();
    //{
    //    int nVariables = static_cast<int>(_variableSelection.size());
    //    std::vector<float> data(nVariables, 1.f / static_cast<float>(nVariables));

    //    // First build array with real strings. Note that this has to stay alive for
    //    // the entire lifetime of the char * array
    //    std::vector<std::string> labelStrings;
    //    labelStrings.reserve(nVariables);
    //    for (int i = 0; i < nVariables; ++i) {
    //        std::string label = _dataViewer.columnName(
    //            _dataViewer.columns()[_variableSelection[i].columnIndex]
    //        );
    //        label = label.substr(0, 10); // limit length
    //        labelStrings.push_back(std::format(" {}. {}", i + 1, label));
    //    }

    //    // Then build array with const char * from that array
    //    std::vector<const char*> labels;
    //    labels.reserve(nVariables);
    //    for (int i = 0; i < nVariables; ++i) {
    //        labels.push_back(labelStrings[i].data());
    //    }

    //    // Reverse vector to get the order its actually rendered
    //    std::reverse(labels.begin(), labels.end());

    //    constexpr const int ColorScaleHeight = 140;
    //    ImVec2 plotSize = ImVec2(1.5f * ColorScaleHeight, static_cast<float>(ColorScaleHeight));
    //    ImPlot::SetNextAxesLimits(0, 1.5, 0, 1, ImGuiCond_Always);

    //    if (ImPlot::BeginPlot(
    //            "##Pie",
    //            plotSize,
    //            ImPlotFlags_Equal | ImPlotFlags_NoMouseText
    //        ))
    //    {
    //        ImPlot::SetupAxes(
    //            nullptr,
    //            nullptr,
    //            ImPlotAxisFlags_NoDecorations,
    //            ImPlotAxisFlags_NoDecorations
    //        );

    //        // TODO: revive
    //        //ImPlot::PlotPieChart(
    //        //    labels.data(),
    //        //    data.data(),
    //        //    nVariables,
    //        //    1.1,
    //        //    0.5,
    //        //    0.3,
    //        //    "%.1f",
    //        //    90.0,
    //        //    true
    //        //);

    //        ImPlot::EndPlot();
    //    }
    //}

    ImGui::EndGroup(); // variables + plot group

    ImGui::End();

    return cmapWasChanged;
}

// Create a logarithmic color scale (ImPlot has no built-in one), with decade labels
void LogColormapScale(const char* label, double scaleMin, double scaleMax,
                      float barWidthPx, float height)
{
    if (scaleMin <= 0.0 || scaleMax <= 0.0 || scaleMin >= scaleMax) {
        return;
    }

    const double logMin = std::log10(scaleMin);
    const double logMax = std::log10(scaleMax);

    auto buildTicks = [](
        double minV,
        double maxV,
        std::initializer_list<double> multipliers,
        std::vector<double>& values,
        std::vector<std::string>* labels = nullptr
    ) {
        values.clear();
        if (labels) {
            labels->clear();
        }

        const int decadeStart = static_cast<int>(std::floor(std::log10(minV)));
        const int decadeEnd = static_cast<int>(std::ceil(std::log10(maxV)));

        for (int d = decadeStart; d <= decadeEnd; ++d) {
            const double decade = std::pow(10.0, d);
            for (double m : multipliers) {
                const double v = decade * m;
                if (v < minV || v > maxV) {
                    continue;
                }

                values.push_back(v);

                if (labels) {
                    char buf[32];
                    std::snprintf(buf, sizeof(buf), "%g", v);
                    labels->push_back(buf);
                }
            }
        }
    };

    std::vector<double> majorValues;
    std::vector<std::string> majorLabels;
    buildTicks(scaleMin, scaleMax, { 1.0 }, majorValues, &majorLabels);

    std::vector<double> minorValues;
    buildTicks(scaleMin, scaleMax, { 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 }, minorValues);

    std::vector<const char*> majorLabelPtrs;
    majorLabelPtrs.reserve(majorLabels.size());
    for (const std::string& s : majorLabels) {
        majorLabelPtrs.push_back(s.c_str());
    }

    float maxLabelWidth = 0.0f;
    for (const std::string& s : majorLabels) {
        maxLabelWidth = std::max(maxLabelWidth, ImGui::CalcTextSize(s.c_str()).x);
    }

    const ImPlotStyle& style = ImPlot::GetStyle();
    const float axisReserve = style.MajorTickLen.y
        + style.LabelPadding.x * 2.0f
        + maxLabelWidth
        + style.PlotPadding.x;
    const float totalWidth = barWidthPx + axisReserve;

    ImGui::PushID(label);
    ImPlot::PushStyleColor(ImPlotCol_AxisTick, ImVec4(1.f, 1.f, 1.f, 1.f));

    if (ImPlot::BeginPlot("##LogColorbar", ImVec2(totalWidth, height),
        ImPlotFlags_CanvasOnly | ImPlotFlags_NoInputs))
    {
        ImPlot::SetupAxis(ImAxis_X1, nullptr, ImPlotAxisFlags_NoDecorations);
        ImPlot::SetupAxisLimits(ImAxis_X1, 0.0, 1.0, ImPlotCond_Always);

        ImPlot::SetupAxis(ImAxis_Y1, nullptr, ImPlotAxisFlags_Opposite);
        ImPlot::SetupAxisScale(ImAxis_Y1, ImPlotScale_Log10);
        ImPlot::SetupAxisLimits(ImAxis_Y1, scaleMin, scaleMax, ImPlotCond_Always);

        if (!majorValues.empty()) {
            ImPlot::SetupAxisTicks(
                ImAxis_Y1,
                majorValues.data(),
                static_cast<int>(majorValues.size()),
                majorLabelPtrs.data(),
                false
            );
        }

        const ImVec2 plotSize = ImPlot::GetPlotSize();
        const float barFrac = (plotSize.x > 0.0f) ? std::min(1.0f, barWidthPx / plotSize.x) : 1.0f;

        ImDrawList* plotDl = ImPlot::GetPlotDrawList();
        constexpr int Steps = 256;
        for (int i = 0; i < Steps; ++i) {
            const float t0 = static_cast<float>(i) / Steps;
            const float t1 = static_cast<float>(i + 1) / Steps;

            const double v0 = std::pow(10.0, logMin + t0 * (logMax - logMin));
            const double v1 = std::pow(10.0, logMin + t1 * (logMax - logMin));

            const ImVec2 p0 = ImPlot::PlotToPixels(ImPlotPoint(0.0, v0));
            const ImVec2 p1 = ImPlot::PlotToPixels(ImPlotPoint(barFrac, v1));
            const ImVec4 c = ImPlot::SampleColormap(0.5f * (t0 + t1));

            plotDl->AddRectFilled(p0, p1, ImGui::ColorConvertFloat4ToU32(c));
        }

        ImDrawList* windowDl = ImGui::GetWindowDrawList();
        // Dimmer and shorter than major ticks
        const ImU32 minorColor = ImGui::GetColorU32(ImGuiCol_Text, 0.6f);
        const float minorTickLen = style.MajorTickLen.y * 0.3f;

        for (double v : minorValues) {
            const ImVec2 p = ImPlot::PlotToPixels(ImPlotPoint(1.0, v));
            windowDl->AddLine(p, ImVec2(p.x - minorTickLen, p.y), minorColor, 1.0f);
        }

        ImPlot::EndPlot();
    }

    ImPlot::PopStyleColor();
    ImGui::PopID();
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

        // Min/max values for color range
        ImGui::SetNextItemWidth(InputWidth);
        if (ImGui::DragFloatRange2("Min / Max", &variable.colorScaleMin, &variable.colorScaleMax, 1.f)) {
            wasChanged = true;
        }

        // Logarithmic scaling toggle
        if (ImGui::Checkbox("Log scale", &variable.useLogScale)) {
            wasChanged = true;
        }

        const ImVec4 WarningColor = ImVec4(1.f, 0.6f, 0.2f, 1.f);
        if (variable.useLogScale && variable.colorScaleMin <= 0.f) {
            ImGui::TextColored(WarningColor, "Min must be > 0 for log scale");
        }
        if (variable.useLogScale && variable.colorScaleMax <= 0.f) {
            ImGui::TextColored(WarningColor, "Max must be > 0 for log scale");
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

            size_t colormapColumn = variable.columnIndex;

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

    // Render visuals for colormap
    ImGui::SameLine();
    ImPlot::PushColormap(_colormaps[variable.colormapIndex]);

    constexpr const int ColorScaleHeight = 180;
    const bool canUseLogScale = variable.colorScaleMin > 0.f && variable.colorScaleMax > 0.f;

    if (variable.useLogScale && canUseLogScale) {
        // Use log-space bounds for the visual scale (ImPlot::ColormapScale has no log flag)
        LogColormapScale(
            "##ColorScale",
            variable.colorScaleMin,
            variable.colorScaleMax,
            15,
            ColorScaleHeight
        );
    }
    else {
        ImPlot::ColormapScale(
            "##ColorScale",
            variable.colorScaleMin,
            variable.colorScaleMax,
            ImVec2(0, ColorScaleHeight)
        );
    }

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
    else if (variable.useLogScale && fValue <= 0.f) {
        // Log scale is undefined for non-positive values; treat as NaN
        pointColor = _nanPointColor;
    }
    else {
        // TODO: handle min > max
        ImPlot::PushColormap(_colormaps[variable.colormapIndex]);

        float min = variable.colorScaleMin;
        float max = variable.colorScaleMax;
        float t = 0.f;

        if (variable.useLogScale && min > 0.f && max > 0.f) {
            float logMin = std::log10(min);
            float logMax = std::log10(max);
            float logVal = std::log10(fValue);
            float logDiff = std::abs(logMax - logMin);
            t = logDiff > std::numeric_limits<float>::epsilon() ?
                (logVal - logMin) / logDiff : 0.f;
        }
        else {
            float minMaxDiff = std::abs(max - min);
            t = minMaxDiff > std::numeric_limits<float>::epsilon() ?
                (fValue - min) / minMaxDiff : 0.f;
        }

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
