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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATAVIEWER___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATAVIEWER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/exoplanetsexperttool/columnfilter.h>
#include <modules/exoplanetsexperttool/dataloader.h>
#include <modules/exoplanetsexperttool/datastructures.h>
#include <openspace/properties/optionproperty.h>
#include <variant>
#include <vector>

namespace openspace::exoplanets::gui {

enum ColumnID {
    Name,
    Host,
    DiscoveryYear,
    NPlanets,
    NStars,
    ESM,
    TSM,
    PlanetRadius,
    PlanetTemperature,
    PlanetMass,
    SurfaceGravity,
    SemiMajorAxis,
    Eccentricity,
    Period,
    Inclination,
    StarTemperature,
    StarRadius,
    MagnitudeJ,
    MagnitudeK,
    Distance
};

struct Column {
    const char* name;
    ColumnID id;
    const char* format = "%s";
};

class DataViewer : public properties::PropertyOwner {
public:
    DataViewer(std::string identifier, std::string guiName = "");

    void initializeGL();
    void render();

private:
    void initializeRenderables();
    void renderScatterPlotAndColormap();
    void renderTable();

    // Render filter settings and return true if filtering changed
    bool renderFilterSettings();

    void renderColumnValue(ColumnID column, const char* format,
        const ExoplanetItem& item);

    std::variant<const char*, float> valueFromColumn(ColumnID column,
        const ExoplanetItem& item) const;

    bool compareColumnValues(ColumnID column, const ExoplanetItem& left,
        const ExoplanetItem& right) const ;

    std::string formatIndicesList(const std::vector<size_t>& dataIndices);

    // Check if a column is numeric. If it isn't, then it is text based
    bool isNumericColumn(ColumnID id) const;

    glm::vec4 colorFromColormap(const ExoplanetItem& item);

    // Write the information about the rendered points to a file
    void writeRenderDataToFile();

    DataLoader _dataLoader;
    std::vector<ExoplanetItem> _data;
    std::vector<size_t> _filteredData;  // The indices of the items which will be rendered
    std::vector<size_t> _selection;     // Indices of selected data points

    std::string _pointsIdentifier;

    std::vector<Column> _columns;
    std::vector<const char*> _colormaps;
    int _currentColormapIndex;
    int _columnForColormap; // index
    float _colorScaleMin;
    float _colorScaleMax;
    bool _colormapWasChanged = true;

    struct ColumnFilterEntry {
        int columnIndex;
        ColumnFilter filter;
    };
    std::vector<ColumnFilterEntry> _appliedFilters;
};

} // namespace openspace::exoplanets::gui

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATAVIEWER___H__
