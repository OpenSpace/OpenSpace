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
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <numeric>
#include <string>
#include <vector>

namespace {
    // Curated high-contrast qualitative palette of 20 colors (Tableau20-style)
    constexpr ImVec4 Qualitative20Colors[] = {
        ImVec4(0.121569f, 0.466667f, 0.705882f, 1.f), // Blue
        ImVec4(1.000000f, 0.498039f, 0.054902f, 1.f), // Orange
        ImVec4(0.172549f, 0.627451f, 0.172549f, 1.f), // Green
        ImVec4(0.839216f, 0.152941f, 0.156863f, 1.f), // Red
        ImVec4(0.580392f, 0.403922f, 0.741176f, 1.f), // Purple
        ImVec4(0.549020f, 0.337255f, 0.294118f, 1.f), // Brown
        ImVec4(0.890196f, 0.466667f, 0.760784f, 1.f), // Pink
        ImVec4(0.498039f, 0.498039f, 0.498039f, 1.f), // Gray
        ImVec4(0.737255f, 0.741176f, 0.133333f, 1.f), // Olive
        ImVec4(0.090196f, 0.745098f, 0.811765f, 1.f), // Cyan
        ImVec4(0.682353f, 0.780392f, 0.909804f, 1.f), // Light Blue
        ImVec4(1.000000f, 0.733333f, 0.470588f, 1.f), // Light Orange
        ImVec4(0.596078f, 0.874510f, 0.541176f, 1.f), // Light Green
        ImVec4(1.000000f, 0.596078f, 0.588235f, 1.f), // Light Red
        ImVec4(0.772549f, 0.690196f, 0.835294f, 1.f), // Light Purple
        ImVec4(0.768627f, 0.611765f, 0.580392f, 1.f), // Light Brown
        ImVec4(0.968627f, 0.713725f, 0.823529f, 1.f), // Light Pink
        ImVec4(0.780392f, 0.780392f, 0.780392f, 1.f), // Light Gray
        ImVec4(0.858824f, 0.858824f, 0.552941f, 1.f), // Light Olive
        ImVec4(0.619608f, 0.854902f, 0.898039f, 1.f)  // Light Cyan
    };
    constexpr int NumQualitative20Colors = static_cast<int>(sizeof(Qualitative20Colors) / sizeof(Qualitative20Colors[0]));

    void LogColormapScale(const char* label, double scaleMin, double scaleMax,
                          float barWidthPx, float height)
    {
        if (scaleMin <= 0.0 || scaleMax <= 0.0 || scaleMin == scaleMax) {
            return;
        }

        const double logMin = std::log10(scaleMin);
        const double logMax = std::log10(scaleMax);

        const double tickMin = std::min(scaleMin, scaleMax);
        const double tickMax = std::max(scaleMin, scaleMax);

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
        buildTicks(tickMin, tickMax, { 1.0 }, majorValues, &majorLabels);

        std::vector<double> minorValues;
        buildTicks(tickMin, tickMax, { 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 }, minorValues);

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

                const ImVec2 rMin(std::min(p0.x, p1.x), std::min(p0.y, p1.y));
                const ImVec2 rMax(std::max(p0.x, p1.x), std::max(p0.y, p1.y));
                plotDl->AddRectFilled(rMin, rMax, ImGui::ColorConvertFloat4ToU32(c));
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
} // namespace

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

    _categoricalPalettes = {
        "Deep (10)",
        "Dark / Set1 (9)",
        "Paired (12)",
        "Pastel (9)",
        "Distinct 20"
    };

    for (size_t i = 0; i < _dataViewer.columns().size(); ++i) {
        if (_dataViewer.isNumericColumn(i)) {
            _firstNumericColumn = _dataViewer.columns()[i];
            break;
        }
    }

    ColorMappedVariable newVariable = { .column = _firstNumericColumn };
    if (dataSettings.defaultColormapping.has_value()) {
        DataSettings::CmapInfo info = *dataSettings.defaultColormapping;
        newVariable = {
            .column = info.column,
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
ColorMappingView::colorMapperVariables()
{
    for (ColorMappedVariable& variable : _variableSelection) {
        if (!_dataViewer.hasColumn(variable.column)) {
            variable.column = _firstNumericColumn;
        }
    }
    return _variableSelection;
}

const ColumnKey& ColorMappingView::firstNumericColumn() const {
    return _firstNumericColumn;
}

bool ColorMappingView::render(bool* open) {
    if (!ImGui::Begin("Color mapping", open, ImGuiWindowFlags_AlwaysAutoResize)) {
        ImGui::End();
        return false;
    }

    bool cmapWasChanged = false;

    // Start variable group
    ImGui::BeginGroup();

    {
        ImGui::BeginGroup();

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
    }

    ImGui::Spacing();

    {
        ImGui::BeginGroup();

        for (size_t index = 0; index < _variableSelection.size(); ++index) {
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

        // Add button
        {
            bool isMaxColors =
                _variableSelection.size() == RenderableExoplanetGlyphCloud::MaxNumberColors;

            if (isMaxColors) {
                ImGui::BeginDisabled();
            }

            // Colormap for each selected variable
            if (ImGui::Button("+ Add variable")) {
                ColorMappedVariable newVariable = { .column = _firstNumericColumn };
                _variableSelection.push_back(newVariable);
                cmapWasChanged = true;
            };

            if (isMaxColors) {
                ImGui::EndDisabled();
                ImGui::SameLine();
                ImGui::Text("Max colors reached");
            }
        }

        ImGui::EndGroup(); // all variable groups
    }

    // Circle plot to show which parameters map to which part of a glyph
    const int nVariables = static_cast<int>(_variableSelection.size());
    if (nVariables > 1) {
        ImGui::SameLine();
        if (nVariables > 0) {
            std::vector<float> data(nVariables, 1.f / static_cast<float>(nVariables));

            // ImPlot draws pie slices in the opposite winding order of the label
            // sequence we want, so build labels directly in reverse order
            std::vector<std::string> labelStrings;
            labelStrings.reserve(nVariables);
            for (int i = nVariables - 1; i >= 0; --i) {
                std::string label = _dataViewer.columnName(
                    _variableSelection[i].column
                );
                label = label.substr(0, 10); // limit length
                labelStrings.push_back(std::format(" {}. {}", i + 1, label));
            }

            std::vector<const char*> pieLabels;
            pieLabels.reserve(nVariables);
            for (const std::string& s : labelStrings) {
                pieLabels.push_back(s.c_str());
            }

            // The label is included in the speicfied plot size
            // Reserve extra vertical space for labels above the pie
            const float extraTopSpace = nVariables *
                (ImGui::GetFrameHeight() - ImGui::GetStyle().FramePadding.y);

            ImPlot::PushStyleVar(ImPlotStyleVar_LegendPadding, ImVec2(0.f, 3.f));

            const ImVec2 size = ImVec2(120.f, 120.f + extraTopSpace);
            constexpr ImPlotFlags flags = ImPlotFlags_Equal | ImPlotFlags_NoMouseText;
            if (ImPlot::BeginPlot("##Pie", size, flags)) {
                ImPlot::SetupAxes(
                    nullptr, nullptr,
                    ImPlotAxisFlags_NoDecorations,
                    ImPlotAxisFlags_NoDecorations
                );

                ImPlot::SetupLegend(
                    ImPlotLocation_North,
                    ImPlotLegendFlags_Outside | ImPlotLegendFlags_Reverse
                );

                ImPlot::SetupAxesLimits(0.0, 1.0, 0.0, 1.0, ImGuiCond_Always);

                constexpr double pieCenterY = 0.5;
                constexpr double pieRadius = 0.4;

                ImPlot::PlotPieChart(
                    pieLabels.data(), data.data(), nVariables,
                    0.5, pieCenterY, pieRadius,
                    "", 90.0
                );

                ImPlot::EndPlot();
            }

            ImPlot::PopStyleVar();
        }
    }

    ImGui::EndGroup(); // variables + plot group

    ImGui::End();

    return cmapWasChanged;
}

void ColorMappingView::renderActiveColormapOverview() {

    ImGui::ColorButton("##NoValuecolor", view::helper::toImVec4(_nanPointColor), ImGuiColorEditFlags_NoInputs);
    ImGui::SameLine();
    ImGui::TextUnformatted("No value");

    ImGui::Spacing();

    const int nVariables = static_cast<int>(_variableSelection.size());
    for (int i = 0; i < nVariables; ++i) {
        const ColorMappedVariable& variable = _variableSelection[i];
        const std::string columnName = _dataViewer.columnName(variable.column);

        ImGui::Text("%s", columnName.c_str());

        const bool isNumeric = _dataViewer.isNumericColumn(variable.column);

        if (isNumeric) {
            ImPlot::PushColormap(_colormaps[variable.colormapIndex]);
            const std::string label = std::format(
                "##ColormapScale{}",
                i
            );

            const float barWidthPx = 20.f;
            const float height = 150.f;
            if (variable.useLogScale) {
                LogColormapScale(
                    label.c_str(),
                    variable.colorScaleMin,
                    variable.colorScaleMax,
                    barWidthPx,
                    height
                );
            }
            else {
                ImPlot::ColormapScale(
                    label.c_str(),
                    variable.colorScaleMin,
                    variable.colorScaleMax,
                    ImVec2(0, height)
                );
            }

            ImPlot::PopColormap();
        }
        else {
            // Render small discrete categorical legend swatches
            const float swatchSize = 14.f;
            constexpr size_t MaxNameLength = 16;
            int catId = 0;
            for (const auto& [name, info] : variable.categories) {
                ImGui::PushID(std::format("##OverviewCat{}_{}", i, catId++).c_str());
                ImVec4 c = view::helper::toImVec4(info.color);
                c.w *= variable.opacity;
                ImGui::ColorButton("##CatSwatch", c, ImGuiColorEditFlags_NoInputs | ImGuiColorEditFlags_NoTooltip, ImVec2(swatchSize, swatchSize));
                ImGui::SameLine();
                view::helper::renderTruncatedTextWithTooltip(name, MaxNameLength);
                ImGui::PopID();
            }
        }
    }
}

bool ColorMappingView::renderColormapEdit(ColorMappedVariable& variable,
                                          std::string_view relevantSystem)
{
    constexpr const int InputWidth = 140;
    bool wasChanged = false;

    if (!_dataViewer.hasColumn(variable.column)) {
        variable.column = _firstNumericColumn;
        wasChanged = true;
    }

    const bool isNumeric = _dataViewer.isNumericColumn(variable.column);

    if (!isNumeric && variable.categories.empty()) {
        updateCategoriesForVariable(variable);
    }

    ImGui::BeginGroup();
    {
        ImGui::SetNextItemWidth(InputWidth);
        if (ImGui::BeginCombo(
                "Column",
                _dataViewer.columnName(variable.column)
            ))
        {
            static ImGuiTextFilter columnFilter;
            if (ImGui::IsWindowAppearing()) {
                ImGui::SetKeyboardFocusHere();
                columnFilter.Clear();
            }
            columnFilter.Draw("##Filter");

            for (int i = 0; i < _dataViewer.columns().size(); ++i) {
                const char* name = _dataViewer.columnName(i);
                if (!columnFilter.PassFilter(name)) {
                    continue;
                }

                const ColumnKey& column = _dataViewer.columns()[i];
                if (ImGui::Selectable(name, variable.column == column)) {
                    variable.column = column;
                    if (!_dataViewer.isNumericColumn(column)) {
                        updateCategoriesForVariable(variable);
                    }
                    wasChanged = true;
                }

                _dataViewer.renderColumnDescriptionTooltip(i);
            }
            ImGui::EndCombo();
        }

        if (isNumeric) {
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
                    auto value = _dataViewer.columnValue(variable.column, item);
                    if (!std::holds_alternative<float>(value)) {
                        continue;
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

            // Logarithmic scaling toggle
            if (ImGui::Checkbox("Log scale", &variable.useLogScale)) {
                wasChanged = true;
            }

            const ImVec4 WarningColor = view::helper::toImVec4(view::colors::Warning);
            if (variable.useLogScale && variable.colorScaleMin <= 0.f) {
                ImGui::TextColored(WarningColor, "Min must be > 0 for log scale");
            }
            if (variable.useLogScale && variable.colorScaleMax <= 0.f) {
                ImGui::TextColored(WarningColor, "Max must be > 0 for log scale");
            }
        }
        else {
            // Categorical Palette dropdown
            ImGui::SetNextItemWidth(InputWidth);
            if (ImGui::BeginCombo("Palette", _categoricalPalettes[variable.categoricalPaletteIndex])) {
                for (int i = 0; i < _categoricalPalettes.size(); ++i) {
                    const char* name = _categoricalPalettes[i];
                    if (ImGui::Selectable(name, variable.categoricalPaletteIndex == i)) {
                        variable.categoricalPaletteIndex = i;
                        resetCategoryColorsToPalette(variable);
                        wasChanged = true;
                    }
                }
                ImGui::EndCombo();
            }

            if (ImGui::SmallButton("Reset palette")) {
                resetCategoryColorsToPalette(variable);
                wasChanged = true;
            }
            ImGui::SameLine();
            if (ImGui::SmallButton("Rescan data")) {
                updateCategoriesForVariable(variable);
                wasChanged = true;
            }
        }

        // Render an opacity slider
        ImGui::SetNextItemWidth(InputWidth);
        if (ImGui::SliderFloat("Opacity", &variable.opacity, 0.f, 1.f)) {
            wasChanged = true;
        }
        ImGui::EndGroup();
    }

    // Render visuals for colormap
    ImGui::SameLine();

    if (isNumeric) {
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
    }
    else {
        // Render scrollable category color list with custom color pickers
        ImGui::BeginGroup();
        ImGui::Text("Categories (%zu):", variable.categories.size());

        const float childWidth = 200.f;
        const float childHeight = 160.f;
        if (ImGui::BeginChild("##CategoryList", ImVec2(childWidth, childHeight), true)) {
            ImGuiColorEditFlags colorFlags = ImGuiColorEditFlags_NoInputs |
                ImGuiColorEditFlags_NoLabel | ImGuiColorEditFlags_AlphaPreview;

            constexpr size_t MaxNameLength = 18;
            int catId = 0;
            for (auto& [name, info] : variable.categories) {
                ImGui::PushID(catId++);

                ImVec4 c = view::helper::toImVec4(info.color);
                if (ImGui::ColorEdit4("##Color", (float*)&c, colorFlags)) {
                    info.color = glm::vec4(c.x, c.y, c.z, c.w);
                    wasChanged = true;
                }
                ImGui::SameLine();

                const std::string suffix = std::format(" ({})", info.count);
                view::helper::renderTruncatedTextWithTooltip(name, MaxNameLength, suffix);

                ImGui::PopID();
            }
        }
        ImGui::EndChild();
        ImGui::EndGroup();
    }

    return wasChanged;
}

glm::vec4 ColorMappingView::colorFromColormap(const ExoplanetItem& item,
                                              const ColorMappedVariable& variable)
{
    const ColumnKey& colormapColumn = _dataViewer.hasColumn(variable.column) ?
        variable.column : _firstNumericColumn;

    const bool isNumeric = _dataViewer.isNumericColumn(colormapColumn);

    glm::vec4 pointColor = _nanPointColor;

    if (isNumeric) {
        std::variant<const char*, float> value = _dataViewer.columnValue(colormapColumn, item);
        float fValue = 0.0f;
        if (std::holds_alternative<float>(value)) {
            fValue = std::get<float>(value);
        }
        else {
            return _nanPointColor;
        }

        if (std::isnan(fValue)) {
            pointColor = _nanPointColor;
        }
        else if (variable.useLogScale && fValue <= 0.f) {
            // Log scale is undefined for non-positive values; treat as NaN
            pointColor = _nanPointColor;
        }
        else {
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
    }
    else {
        // String / Categorical column
        std::variant<const char*, float> value = _dataViewer.columnValue(colormapColumn, item);
        if (std::holds_alternative<const char*>(value)) {
            const char* str = std::get<const char*>(value);
            if (str && str[0] != '\0') {
                std::string catKey(str);
                auto it = variable.categories.find(catKey);
                if (it != variable.categories.end()) {
                    pointColor = it->second.color;
                }
                else {
                    // Category not in cache yet: assign deterministically based on hash
                    size_t hashVal = std::hash<std::string>{}(catKey);
                    if (variable.categoricalPaletteIndex == 4) {
                        ImVec4 c = Qualitative20Colors[hashVal % NumQualitative20Colors];
                        pointColor = glm::vec4(c.x, c.y, c.z, c.w);
                    }
                    else {
                        ImPlotColormap cmap = ImPlotColormap_Deep;
                        if (variable.categoricalPaletteIndex == 1) {
                            cmap = ImPlotColormap_Dark;
                        }
                        else if (variable.categoricalPaletteIndex == 2) {
                            cmap = ImPlotColormap_Paired;
                        }
                        else if (variable.categoricalPaletteIndex == 3) {
                            cmap = ImPlotColormap_Pastel;
                        }
                        ImVec4 c = ImPlot::GetColormapColor(static_cast<int>(hashVal), cmap);
                        pointColor = glm::vec4(c.x, c.y, c.z, c.w);
                    }
                }
            }
            else {
                // Empty string treated as missing/NaN
                pointColor = _nanPointColor;
            }
        }
        else {
            pointColor = _nanPointColor;
        }
    }

    // Apply opacity
    pointColor.a *= variable.opacity;

    return pointColor;
}

const char* ColorMappingView::colormapFromIndex(size_t index) const {
    return _colormaps[index];
}

const char* ColorMappingView::categoricalPaletteFromIndex(size_t index) const {
    if (index < _categoricalPalettes.size()) {
        return _categoricalPalettes[index];
    }
    return _categoricalPalettes.front();
}

void ColorMappingView::updateCategoriesForVariable(ColorMappedVariable& variable) {
    // Scan all dataset rows to extract unique non-empty string values and counts
    std::map<std::string, size_t> categoryCounts;
    const std::vector<ExoplanetItem>& allData = _dataViewer.data();
    for (const ExoplanetItem& item : allData) {
        std::variant<const char*, float> val = _dataViewer.columnValue(variable.column, item);
        if (std::holds_alternative<const char*>(val)) {
            const char* str = std::get<const char*>(val);
            if (str && str[0] != '\0') {
                categoryCounts[std::string(str)]++;
            }
        }
    }

    // Preserve existing custom colors if category already existed, otherwise assign palette color
    std::map<std::string, CategoryInfo> newCategories;
    size_t catIndex = 0;
    for (const auto& [name, count] : categoryCounts) {
        if (auto it = variable.categories.find(name); it != variable.categories.end()) {
            newCategories[name] = CategoryInfo{
                .color = it->second.color,
                .count = count
            };
        }
        else {
            // Assign default color from active qualitative palette
            glm::vec4 color(1.f, 1.f, 1.f, 1.f);
            if (variable.categoricalPaletteIndex == 4) { // Distinct 20
                ImVec4 c = Qualitative20Colors[catIndex % NumQualitative20Colors];
                color = glm::vec4(c.x, c.y, c.z, c.w);
            }
            else {
                // Map to built-in ImPlot qualitative colormaps:
                // 0: Deep (ImPlotColormap_Deep)
                // 1: Dark (ImPlotColormap_Dark)
                // 2: Paired (ImPlotColormap_Paired)
                // 3: Pastel (ImPlotColormap_Pastel)
                ImPlotColormap cmap = ImPlotColormap_Deep;
                if (variable.categoricalPaletteIndex == 1) {
                    cmap = ImPlotColormap_Dark;
                }
                else if (variable.categoricalPaletteIndex == 2) {
                    cmap = ImPlotColormap_Paired;
                }
                else if (variable.categoricalPaletteIndex == 3) {
                    cmap = ImPlotColormap_Pastel;
                }
                ImVec4 c = ImPlot::GetColormapColor(static_cast<int>(catIndex), cmap);
                color = glm::vec4(c.x, c.y, c.z, c.w);
            }

            newCategories[name] = CategoryInfo{
                .color = color,
                .count = count
            };
        }
        catIndex++;
    }

    variable.categories = std::move(newCategories);
}

void ColorMappingView::resetCategoryColorsToPalette(ColorMappedVariable& variable) {
    size_t catIndex = 0;
    for (auto& [name, info] : variable.categories) {
        if (variable.categoricalPaletteIndex == 4) { // Distinct 20
            ImVec4 c = Qualitative20Colors[catIndex % NumQualitative20Colors];
            info.color = glm::vec4(c.x, c.y, c.z, c.w);
        }
        else {
            ImPlotColormap cmap = ImPlotColormap_Deep;
            if (variable.categoricalPaletteIndex == 1) {
                cmap = ImPlotColormap_Dark;
            }
            else if (variable.categoricalPaletteIndex == 2) {
                cmap = ImPlotColormap_Paired;
            }
            else if (variable.categoricalPaletteIndex == 3) {
                cmap = ImPlotColormap_Pastel;
            }
            ImVec4 c = ImPlot::GetColormapColor(static_cast<int>(catIndex), cmap);
            info.color = glm::vec4(c.x, c.y, c.z, c.w);
        }
        catIndex++;
    }
}

} // namespace openspace::exoplanets
