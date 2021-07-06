/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/exoplanetsexperttool/dataviewer.h>

#include <modules/exoplanetsexperttool/rendering/renderablepointdata.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <algorithm>
#include <fstream>
#include <iostream>

#include <implot.h>

namespace {
    constexpr const char _loggerCat[] = "ExoplanetsDataViewer";

    constexpr const char RenderDataFileName[] = "${TEMPORARY}/pointrenderdata.dat";

    bool caseInsensitiveLessThan(const char* lhs, const char* rhs) {
        int res = _stricmp(lhs, rhs);
        return res < 0;
    }

    bool compareValues(double lhs, double rhs) {
        if (std::isnan(lhs)) {
            // also includes rhs is nan, in which case the order does not matter
            return true;
        }

        // rhs is nan, but not lhs
        if (std::isnan(rhs)) {
            return false;
        }
        return lhs < rhs;
    }

    constexpr const glm::vec3 DefaultSelectedColor = { 0.2f, 0.8f, 1.f };
    constexpr const glm::vec4 NanPointColor = { 0.3f, 0.3f, 0.3f, 1.f };

    constexpr const float DefaultColorScaleMinValue = 0.f;
    constexpr const float DefaultColorScaleMaxValue = 1000.f;

    // @TODO This can be implemented as a constructor in imconfig.h to enable conversion
    ImVec4 toImVec4(const glm::vec4& v) {
        return ImVec4(v.x, v.y, v.z, v.w);
    }

    // @TODO this could be a temoplated helper function for lists. Used a lot
    std::string formatIndicesList(const std::vector<size_t>& indices) {
        std::string result;
        for (size_t i : indices) {
            result += std::to_string(i) + ',';
        }
        if (!result.empty()) {
            result.pop_back();
        }
        return result;
    }
}

namespace openspace::exoplanets::gui {

DataViewer::DataViewer(std::string identifier, std::string guiName)
    : properties::PropertyOwner({ std::move(identifier), std::move(guiName) })
    , _pointsIdentifier("ExoplanetDataPoints")
    , _colorScaleMin(DefaultColorScaleMinValue)
    , _colorScaleMax(DefaultColorScaleMaxValue)
{
    _data = _dataLoader.loadData();

    _filteredData.reserve(_data.size());
    for (size_t i = 0; i < _data.size(); i++) {
        _filteredData.push_back(i);
    }

    _columns = {
        { "Name", ColumnID::Name },
        { "Host", ColumnID::Host },
        { "Year of discovery", ColumnID::DiscoveryYear, "%.0f" },
        { "Planets", ColumnID::NPlanets, "%.0f" },
        { "Stars ", ColumnID::NStars, "%.0f" },
        { "ESM", ColumnID::ESM, "%.2f" },
        { "TSM", ColumnID::TSM, "%.2f" },
        { "Planet radius (Earth radii)", ColumnID::PlanetRadius, "%.2f" },
        { "Planet equilibrium temp. (K)", ColumnID::PlanetTemperature, "%.0f" },
        { "Mass", ColumnID::PlanetMass, "%.2f" },
        { "Surface Gravity (m/s^2)", ColumnID::SurfaceGravity, "%.2f" },
        // Orbits
        { "Semi-major axis (AU)", ColumnID::SemiMajorAxis, "%.2f" },
        { "Eccentricity", ColumnID::Eccentricity, "%.2f" },
        { "Orbit period", ColumnID::Period, "%.2f" },
        { "Inclination", ColumnID::Inclination, "%.2f" },
        // Star
        { "Star effective temp. (K)", ColumnID::StarTemperature, "%.0f" },
        { "Star radius (Solar)", ColumnID::StarRadius, "%.2f" },
        { "MagJ", ColumnID::MagnitudeJ, "%.2f" },
        { "MagK", ColumnID::MagnitudeK, "%.2f" },
        { "Distance (pc)", ColumnID::Distance, "%.2f" }
    };

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

    // TODO: make sure that settings are preserved between sessions?
    _columnForColormap = 5; // ESM
    _currentColormapIndex = 0;
}

void DataViewer::initializeGL() {
    initializeRenderables();

    // Intilize custom color maps (generated from matplotlib)
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

void DataViewer::initializeRenderables() {
    using namespace std::string_literals;

    writeRenderDataToFile();

    if (!std::filesystem::is_regular_file(absPath(RenderDataFileName))) {
        LWARNING("Count not find data file for points rendering");
        return;
    }

    ghoul::Dictionary gui;
    gui.setValue("Name", "All Exoplanets"s);
    gui.setValue("Path", "/ExoplanetsTool"s);

    std::filesystem::path dataFilePath = absPath(RenderDataFileName);

    ghoul::Dictionary renderable;
    renderable.setValue("Type", "RenderablePointData"s);
    renderable.setValue("DataFile", dataFilePath.string());
    renderable.setValue("HighlightColor", glm::dvec3(DefaultSelectedColor));
    renderable.setValue("Size", 10.0);

    ghoul::Dictionary node;
    node.setValue("Identifier", _pointsIdentifier);
    node.setValue("Renderable", renderable);
    node.setValue("GUI", gui);

    openspace::global::scriptEngine->queueScript(
        fmt::format("openspace.addSceneGraphNode({})", ghoul::formatLua(node)),
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void DataViewer::render() {
    renderTable();
    ImGui::Spacing();
    renderScatterPlotAndColormap();
}

void DataViewer::renderScatterPlotAndColormap() {
    _colormapWasChanged = false;

    static const ImVec2 size = { 400, 300 };
    auto plotFlags = ImPlotFlags_NoLegend;
    auto axisFlags = ImPlotAxisFlags_None;

    // TODO: make static varaibles and only update data if filter and/or selection changed

    std::vector<float> ra, dec;
    ra.reserve(_filteredData.size());
    dec.reserve(_filteredData.size());

    for (size_t i : _filteredData) {
        const ExoplanetItem& item = _data[i];
        if (item.ra.hasValue() && item.ra.hasValue()) {
            ra.push_back(item.ra.value);
            dec.push_back(item.dec.value);
        }
    }

    std::vector<float> ra_selected, dec_selected;
    ra_selected.reserve(_selection.size());
    dec_selected.reserve(_selection.size());

    for (size_t i : _selection) {
        const ExoplanetItem& item = _data[i];
        if (item.ra.hasValue() && item.ra.hasValue()) {
            ra_selected.push_back(item.ra.value);
            dec_selected.push_back(item.dec.value);
        }
    }

    // Colormap
    ImGui::Text("Colormap Settings");
    ImGui::SetNextItemWidth(100);
    if (ImGui::BeginCombo("Column", _columns[_columnForColormap].name)) {
        for (int i = 0; i < _columns.size(); ++i) {
            // Ignore non-numeric columns
            if (!isNumericColumn(_columns[i].id)) {
                continue;
            }

            const char* name = _columns[i].name;
            if (ImGui::Selectable(name, _columnForColormap == i)) {
                _columnForColormap = i;
                _colormapWasChanged = true;
            }
        }
        ImGui::EndCombo();
    }

    ImGui::SameLine();
    ImGui::SetNextItemWidth(200);
    if (ImGui::BeginCombo("Colormap", _colormaps[_currentColormapIndex])) {
        for (int i = 0; i < _colormaps.size(); ++i) {
            const char* name = _colormaps[i];
            ImPlot::ColormapIcon(ImPlot::GetColormapIndex(name));
            ImGui::SameLine();
            if (ImGui::Selectable(name, _currentColormapIndex == i)) {
                _currentColormapIndex = i;
                _colormapWasChanged = true;
            }
        }
        ImGui::EndCombo();
    }

    const ColumnID colormapColumn = _columns[_columnForColormap].id;

    // Min/max values for color range
    ImGui::SetNextItemWidth(200);
    if (ImGui::DragFloatRange2("Min / Max", &_colorScaleMin, &_colorScaleMax, 1.f)) {
        _colormapWasChanged = true;
    }

    ImGui::SameLine();
    if (ImGui::Button("Set from current table data")) {
        float newMin = std::numeric_limits<float>::max();
        float newMax = std::numeric_limits<float>::lowest();

        for (size_t i : _filteredData) {
            const ExoplanetItem& item = _data[i];
            auto value = valueFromColumn(colormapColumn, item);
            if (std::holds_alternative<float>(value)) {
                float val = std::get<float>(value);
                if (std::isnan(val)) {
                    continue;
                }
                if (val > newMax) {
                    newMax = static_cast<float>(val);
                }
                if (val < newMin) {
                    newMin = static_cast<float>(val);
                }
            }
            else {
                // Shouldn't be possible to try to use non numbers
                throw;
            }
        }

        _colorScaleMin = newMin;
        _colorScaleMax = newMax;
        _colormapWasChanged = true;
    };

    ImVec4 selectedColor =
        { DefaultSelectedColor.x, DefaultSelectedColor.y, DefaultSelectedColor.z, 1.f };

    // Scatterplot
    static float pointSize = 1.5f;
    ImPlot::PushColormap(_colormaps[_currentColormapIndex]);
    ImPlot::SetNextPlotLimits(0.0, 360.0, -90.0, 90.0, ImGuiCond_Always);
    if (ImPlot::BeginPlot("Star Coordinate", "Ra", "Dec", size, plotFlags, axisFlags)) {
        ImPlot::PushStyleVar(ImPlotStyleVar_MarkerSize, pointSize);

        for (size_t i : _filteredData) {
            const ExoplanetItem& item = _data[i];

            if (!item.ra.hasValue() || !item.dec.hasValue()) {
                continue;
            }

            const ImVec4 pointColor = toImVec4(colorFromColormap(item));
            const ImPlotPoint point = { item.ra.value, item.dec.value };
            const char* label = "Data " + i;
            ImPlot::PushStyleColor(ImPlotCol_MarkerFill, pointColor);
            ImPlot::PushStyleColor(ImPlotCol_MarkerOutline, pointColor);
            ImPlot::PlotScatter(label, &point.x, &point.y, 1);
            ImPlot::PopStyleColor();
            ImPlot::PopStyleColor();
        }
        ImPlot::PopStyleVar();

        ImPlot::PushStyleVar(ImPlotStyleVar_MarkerSize, 3.f * pointSize);
        ImPlot::PushStyleColor(ImPlotCol_MarkerFill, selectedColor);
        ImPlot::PushStyleColor(ImPlotCol_MarkerOutline, selectedColor);
        ImPlot::PlotScatter(
            "Selected",
            ra_selected.data(),
            dec_selected.data(),
            static_cast<int>(ra_selected.size())
        );
        ImPlot::PopStyleColor();
        ImPlot::PopStyleColor();
        ImPlot::PopStyleVar();
        ImPlot::EndPlot();

        ImGui::SameLine();
        ImPlot::ColormapScale(
            "##ColorScale",
            _colorScaleMin,
            _colorScaleMax,
            ImVec2(60, size.y)
        );

        ImGui::SetNextItemWidth(70);
        ImGui::DragFloat("Point size", &pointSize, 0.1f, 0.f, 5.f);

        ImPlot::PopColormap();
    }
}

void DataViewer::renderTable() {
    static const ImVec2 size = { 0, 400 };

    static ImGuiTableFlags flags =
        ImGuiTableFlags_ScrollX | ImGuiTableFlags_ScrollY
        | ImGuiTableFlags_BordersV | ImGuiTableFlags_BordersOuter
        | ImGuiTableFlags_Reorderable | ImGuiTableFlags_Hideable
        | ImGuiTableFlags_Sortable | ImGuiTableFlags_Resizable
        | ImGuiTableFlags_RowBg;

    const ImGuiTableColumnFlags hide = ImGuiTableColumnFlags_DefaultHide;

    const int nColumns = static_cast<int>(_columns.size());

    bool selectionChanged = false;
    bool filterChanged = renderFilterSettings();

    ImGui::Separator();
    ImGui::TextColored(
        ImVec4(0.6f, 0.6f, 0.6f, 1.f),
        fmt::format(
            "Showing {} / {} matching exoplanets", _filteredData.size(), _data.size()
        ).c_str()
    );

    if (ImGui::BeginTable("exoplanets_table", nColumns, flags, size)) {
        // Header
        for (auto c : _columns) {
            ImGuiTableColumnFlags colFlags = ImGuiTableColumnFlags_PreferSortDescending;
            if (c.id == ColumnID::Name) {
                colFlags |= ImGuiTableColumnFlags_DefaultSort;
            }
            ImGui::TableSetupColumn(c.name, colFlags, 0.f, c.id);
        }
        ImGui::TableSetupScrollFreeze(0, 1); // Make header always visible
        ImGui::TableHeadersRow();

        // Sorting
        if (ImGuiTableSortSpecs* sortSpecs = ImGui::TableGetSortSpecs()) {
            if (sortSpecs->SpecsDirty || filterChanged) {
                auto compare = [&sortSpecs, this](const size_t& lhs,
                                                  const size_t& rhs) -> bool
                {
                    ImGuiSortDirection sortDir = sortSpecs->Specs->SortDirection;
                    bool flip = (sortDir == ImGuiSortDirection_Descending);

                    const ExoplanetItem& l = flip ? _data[rhs] : _data[lhs];
                    const ExoplanetItem& r = flip ? _data[lhs] : _data[rhs];

                    ColumnID col = static_cast<ColumnID>(sortSpecs->Specs->ColumnUserID);

                    return compareColumnValues(col, l, r);
                };

                std::sort(_filteredData.begin(), _filteredData.end(), compare);
                sortSpecs->SpecsDirty = false;
            }
        }

        // Rows
        for (size_t row = 0; row < _filteredData.size(); row++) {
            if (row > 1000) {
                // TODO: show a hint about the number of rendered rows somewhere in the UI
                break; // cap the maximum number of rows we render
            }

            const size_t index = _filteredData[row];
            const ExoplanetItem& item = _data[index];

            ImGuiSelectableFlags selectableFlags = ImGuiSelectableFlags_SpanAllColumns
                | ImGuiSelectableFlags_AllowItemOverlap;

            auto found = std::find(_selection.begin(), _selection.end(), index);
            const bool itemIsSelected = found != _selection.end();

            for (const Column col : _columns) {
                ImGui::TableNextColumn();

                if (col.id == ColumnID::Name) {
                    bool changed = ImGui::Selectable(
                        item.planetName.c_str(),
                        itemIsSelected,
                        selectableFlags
                    );

                    if (changed) {
                        if (ImGui::GetIO().KeyCtrl) {
                            if (itemIsSelected) {
                                _selection.erase(found);
                            }
                            else {
                                _selection.push_back(index);
                            }
                        }
                        else {
                            _selection.clear();
                            _selection.push_back(index);
                        }

                        selectionChanged = true;
                    }
                    continue;
                }

                renderColumnValue(col.id, col.format, item);
            }
        }
        ImGui::EndTable();

        if (filterChanged || _colormapWasChanged) {
            writeRenderDataToFile();
        }

        if (selectionChanged) {
            updateSelectionInRenderable();
        }
    }
}

bool DataViewer::renderFilterSettings() {
    static bool hideNanTsm = false;
    static bool hideNanEsm = false;
    static bool showOnlyMultiPlanetSystems = false;
    static bool showOnlyHasPosition = false;

    bool filterChanged = false;

    // Filtering
    filterChanged |= ImGui::Checkbox("Hide null TSM", &hideNanTsm);
    ImGui::SameLine();
    filterChanged |= ImGui::Checkbox("Hide null ESM", &hideNanEsm);
    ImGui::SameLine();
    filterChanged |= ImGui::Checkbox("Only multi-planet", &showOnlyMultiPlanetSystems);
    ImGui::SameLine();
    filterChanged |= ImGui::Checkbox("Must have positional data (3D)", &showOnlyHasPosition);

    // Per-column filtering
    static int filterColIndex = 0;
    ImGui::Separator();
    ImGui::Text("Filter on column");
    ImGui::SetNextItemWidth(100);
    if (ImGui::BeginCombo("##Column", _columns[filterColIndex].name)) {
        for (int i = 0; i < _columns.size(); ++i) {
            if (ImGui::Selectable(_columns[i].name, filterColIndex == i)) {
                filterColIndex = i;
            }
        }
        ImGui::EndCombo();
    }

    ImGui::SameLine();

    static char queryString[128] = "";

    ImGui::SetNextItemWidth(200);
    bool inputEntered = ImGui::InputText(
        "##Query",
        queryString,
        IM_ARRAYSIZE(queryString),
        ImGuiInputTextFlags_EnterReturnsTrue
    );

    bool numeric = isNumericColumn(_columns[filterColIndex].id);

    // Short description
    ImGui::SameLine();
    ImGui::TextUnformatted(numeric ?
        ColumnFilter::NumericFilterDescriptionShort :
        ColumnFilter::TextFilterDescriptionShort
    );

    // Help marker
    ImGui::SameLine();
    ImGui::TextDisabled("(?)");
    if (ImGui::IsItemHovered()) {
        ImGui::BeginTooltip();
        ImGui::PushTextWrapPos(ImGui::GetFontSize() * 35.0f);
        ImGui::TextUnformatted(numeric ?
            ColumnFilter::NumericFilterDescription :
            ColumnFilter::TextFilterDescription
        );
        ImGui::PopTextWrapPos();
        ImGui::EndTooltip();
    }

    // Clear the text field
    ImGui::SameLine();
    if (ImGui::Button("Clear")) {
        strcpy(queryString, "");
    }

    if (ImGui::Button("Add filter") || inputEntered) {
        ColumnFilter filter = numeric ?
            ColumnFilter(queryString, ColumnFilter::Type::Numeric) :
            ColumnFilter(queryString, ColumnFilter::Type::Text);

        if (filter.isValid()) {
            _appliedFilters.push_back({ filterColIndex , filter });
            strcpy(queryString, "");
            filterChanged = true;
        }
    }

    const std::string filtersHeader = _appliedFilters.empty() ?
        "Added filters" :
        fmt::format("Added filters ({})", _appliedFilters.size());

    // The ### operator overrides the ID, ignoring the preceding label
    // => Won't rerender when label changes
    const std::string headerWithId = fmt::format("{}###FiltersHeader", filtersHeader);

    if (ImGui::CollapsingHeader(headerWithId.c_str())) {
        ImGui::Indent();

        if (_appliedFilters.empty()) {
            ImGui::Text("No active filters");
        }

        int indexToErase = -1;
        constexpr const int nColumns = 4;
        if (ImGui::BeginTable("filtersTable", nColumns, ImGuiTableFlags_SizingFixedFit)) {
            for (int i = 0; i < _appliedFilters.size(); ++i) {
                ColumnFilterEntry f = _appliedFilters[i];
                ImGui::TableNextRow();
                ImGui::TableNextColumn();
                ImGui::Text(_columns[f.columnIndex].name);
                ImGui::TableNextColumn();
                ImGui::Text("    ");
                ImGui::TableNextColumn();
                ImGui::Text(f.filter.query().c_str());
                ImGui::TableNextColumn();

                ImGui::PushID(i);
                if (ImGui::SmallButton("Delete")) {
                    indexToErase = i;
                }
                ImGui::PopID();
            }

            if (indexToErase != -1) {
                _appliedFilters.erase(_appliedFilters.begin() + indexToErase);
                filterChanged = true;
            }

            ImGui::EndTable();
        }
        ImGui::Unindent();
    }

    bool selectionChanged = false;

    if (filterChanged) {
        _filteredData.clear();
        _filteredData.reserve(_data.size());

        for (int i = 0; i < _data.size(); i++) {
            const ExoplanetItem& d = _data[i];

            bool filteredOut = hideNanTsm && std::isnan(d.tsm);
            filteredOut |= hideNanEsm && std::isnan(d.esm);
            filteredOut |= showOnlyMultiPlanetSystems && !d.multiSystemFlag;
            filteredOut |= showOnlyHasPosition && !d.position.has_value();

            for (const ColumnFilterEntry& f : _appliedFilters) {
                std::variant<const char*, float> value =
                    valueFromColumn(_columns[f.columnIndex].id, d);

                if (std::holds_alternative<float>(value)) {
                    float val = std::get<float>(value);
                    filteredOut |= !f.filter.passFilter(val);
                }
                else { // text
                    const char* val = std::get<const char*>(value);
                    filteredOut |= !f.filter.passFilter(val);
                }
            }

            if (!filteredOut) {
                _filteredData.push_back(i);
            }

            // If a filteredOut item is selected, remove it from selection
            if (filteredOut) {
                auto found = std::find(_selection.begin(), _selection.end(), i);
                const bool itemIsSelected = found != _selection.end();

                if (itemIsSelected) {
                    _selection.erase(found);
                    selectionChanged = true;
                }
            }
        }
        _filteredData.shrink_to_fit();

        if (selectionChanged) {
            updateSelectionInRenderable();
        }
    }

    return filterChanged;
}

void DataViewer::renderColumnValue(ColumnID column, const char* format,
                                   const ExoplanetItem& item)
{
    std::variant<const char*, float> value = valueFromColumn(column, item);

    if (std::holds_alternative<float>(value)) {
        float v = std::get<float>(value);
        if (std::isnan(v)) {
            ImGui::TextUnformatted("");
        }
        else {
            ImGui::Text(format, std::get<float>(value));
        }
    }
    else if (std::holds_alternative<const char*>(value)) {
        ImGui::Text(format, std::get<const char*>(value));
    }
}

bool DataViewer::compareColumnValues(ColumnID column, const ExoplanetItem& left,
                                     const ExoplanetItem& right) const
{
    std::variant<const char*, float> leftValue = valueFromColumn(column, left);
    std::variant<const char*, float> rightValue = valueFromColumn(column, right);

    // TODO: make sure they are the same type

    if (std::holds_alternative<const char*>(leftValue)) {
        return !caseInsensitiveLessThan(
            std::get<const char*>(leftValue),
            std::get<const char*>(rightValue)
        );
    }
    else if (std::holds_alternative<float>(leftValue)) {
        return compareValues(std::get<float>(leftValue), std::get<float>(rightValue));
    }
    else {
        LERROR("Trying to compare undefined column types");
        return false;
    }
}

std::variant<const char*, float> DataViewer::valueFromColumn(ColumnID column,
                                                         const ExoplanetItem& item) const
{
    switch (column) {
        case ColumnID::Name:
            return item.planetName.c_str();
        case ColumnID::Host:
            return item.hostName.c_str();
        case ColumnID::DiscoveryYear:
            return static_cast<float>(item.discoveryYear);
        case ColumnID::NPlanets:
            return static_cast<float>(item.nPlanets);
        case ColumnID::NStars:
            return static_cast<float>(item.nStars);
        case ColumnID::ESM:
            return item.esm;
        case ColumnID::TSM:
            return item.tsm;
        case ColumnID::PlanetRadius:
            return item.radius.value;
        case ColumnID::PlanetTemperature:
            return item.eqilibriumTemp.value;
        case ColumnID::PlanetMass:
            return item.mass.value;
        case ColumnID::SurfaceGravity:
            return item.surfaceGravity.value;
        // Orbits
        case ColumnID::SemiMajorAxis:
            return item.semiMajorAxis.value;
        case ColumnID::Eccentricity:
            return item.eccentricity.value;
        case ColumnID::Period:
            return item.period.value;
        case ColumnID::Inclination:
            return item.inclination.value;
        // Star
        case ColumnID::StarTemperature:
            return item.starEffectiveTemp.value;
        case ColumnID::StarRadius:
            return item.starRadius.value;
        case ColumnID::MagnitudeJ:
            return item.magnitudeJ.value;
        case ColumnID::MagnitudeK:
            return item.magnitudeK.value;
        case ColumnID::Distance:
            return item.distance.value;
        default:
            throw ghoul::MissingCaseException();
    }
}

bool DataViewer::isNumericColumn(ColumnID id) const {
    ghoul_assert(_data.size() > 0, "Data size cannot be zero");
    // Test type using the first data point
    std::variant<const char*, float> aValue = valueFromColumn(id, _data.front());
    return std::holds_alternative<float>(aValue);
}

glm::vec4 DataViewer::colorFromColormap(const ExoplanetItem& item) {
    const ColumnID colormapColumn = _columns[_columnForColormap].id;

    std::variant<const char*, float> value = valueFromColumn(colormapColumn, item);
    float fValue = 0.0;
    if (std::holds_alternative<float>(value)) {
        fValue = std::get<float>(value);
    }
    else {
        // text column => cannot be mapped to colormap
        // OBS! This should not happen
        return NanPointColor;
    }

    glm::vec4 pointColor;
    if (std::isnan(fValue)) {
        pointColor = NanPointColor;
    }
    else {
        // TODO: handle min > max
        ImPlot::PushColormap(_colormaps[_currentColormapIndex]);

        float minMaxDiff = std::abs(_colorScaleMax - _colorScaleMin);
        float t = minMaxDiff > std::numeric_limits<float>::epsilon() ?
                 (fValue - _colorScaleMin) / minMaxDiff : 0.f;

        t = std::clamp(t, 0.f, 1.f);
        ImVec4 c = ImPlot::SampleColormap(t);
        ImPlot::PopColormap();
        pointColor = { c.x, c.y, c.z, c.w };
    }
    return pointColor;
}

void DataViewer::writeRenderDataToFile() {
    std::ofstream file(absPath(RenderDataFileName), std::ios::binary);
    if (!file) {
        LERROR(fmt::format("Cannot open file '{}' for writing", RenderDataFileName));
        return;
    }

    LDEBUG("Writing render data to file");

    // For now, only write the filtered data. Later on we might want to render the
    // filtered out points somehow and then we should write out the full dataset
    std::vector<size_t> indicesWithPositions;
    indicesWithPositions.reserve(_filteredData.size());
    for (size_t index : _filteredData) {
        const ExoplanetItem& item = _data[index];
        if (item.position.has_value()) {
            indicesWithPositions.push_back(index);
        }
    }
    indicesWithPositions.shrink_to_fit();

    // Write number of points
    unsigned int nPoints = static_cast<unsigned int>(indicesWithPositions.size());
    file.write(reinterpret_cast<const char*>(&nPoints), sizeof(unsigned int));

    for (size_t index : indicesWithPositions) {
        const ExoplanetItem& item = _data[index];

        file.write(reinterpret_cast<const char*>(&index), sizeof(size_t));

        const glm::dvec3 position = *item.position;
        file.write(reinterpret_cast<const char*>(&position.x), sizeof(double));
        file.write(reinterpret_cast<const char*>(&position.y), sizeof(double));
        file.write(reinterpret_cast<const char*>(&position.z), sizeof(double));

        const ImVec4 color = toImVec4(colorFromColormap(item));
        file.write(reinterpret_cast<const char*>(&color.x), sizeof(float));
        file.write(reinterpret_cast<const char*>(&color.y), sizeof(float));
        file.write(reinterpret_cast<const char*>(&color.z), sizeof(float));
        file.write(reinterpret_cast<const char*>(&color.w), sizeof(float));
    }
}

void DataViewer::updateSelectionInRenderable() {
    const std::string indices = formatIndicesList(_selection);
    const std::string uri =
        fmt::format("Scene.{}.Renderable.Selection", _pointsIdentifier);

    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle('" + uri + "', { " + indices + " })",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

} // namespace openspace::gui
