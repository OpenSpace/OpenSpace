/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <openspace/properties/list/intlistproperty.h>
#include <openspace/properties/optionproperty.h>
#include <ghoul/glm.h>
#include <deque>
#include <optional>
#include <variant>
#include <vector>

namespace openspace::exoplanets {

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
    PlanetMassError,
    SurfaceGravity,
    SemiMajorAxis,
    Eccentricity,
    Period,
    Inclination,
    StarTemperature,
    StarRadius,
    StarAge,
    MagnitudeJ,
    MagnitudeK,
    Distance,
    Ra,
    Dec,
    Metallicity,
    MetallicityRatio,
    DiscoveryMethod,
    DiscoveryTelescope,
    DiscoveryInstrument,
    MoleculesDetection,
    MoleculesUpperLimit,
    MoleculesNoDetection,
    WaterDetection,
    Other
};

struct Column {
    std::string name;
    ColumnID id;
    std::optional<const char*> format = std::nullopt;
    std::optional<const char*> description = std::nullopt;
};

class DataViewer : public properties::PropertyOwner {
public:
    DataViewer(std::string identifier, std::string guiName = "");

    void initializeGL();
    void render();

private:
    struct ColorMappedVariable {
        int colormapIndex = 0;
        int columnIndex = 5; // Default column is ESM. OBS! Fragile!
        float colorScaleMin = 0.f;
        float colorScaleMax = 100.f;
    };

    void renderStartupInfo();
    bool _shouldOpenInfoWindow = true;

    void initializeRenderables();
    void initializeCallbacks();
    void renderHelpMarker(const char* text);

    void renderTable(const std::string& tableId, std::vector<size_t>& planetRows,
        bool useFixedHeight, std::string_view search = "");

    void renderTableWindow(bool* open);
    void renderScatterPlotWindow(bool* open);

    // Returns true if value was changed. If relevantSystem given,
    // also show a button to color based on planets in that system
    bool renderColormapEdit(ColorMappedVariable& variable,
        std::string_view relevantSystem = "");

    void renderColormapWindow(bool* open);
    void renderFilterSettingsWindow(bool* open);

    // Updates the property in the module so that it matches the filtered
    // rows in the UI
    void updateFilteredRowsProperty();

    void renderSettingsMenuContent();

    void renderColumnSettingsModal();
    void setUpSelectedColumns(int nSelected);

    void renderSystemViewContent(const std::string& host);

    void renderColumnValue(int columnIndex, std::optional<const char*> format,
        const ExoplanetItem& item);

    int columnIndexFromId(ColumnID id) const;

    std::variant<const char*, float> valueFromColumn(int columnIndex,
        const ExoplanetItem& item) const;

    // Compare the values of two Exoplanets items, given a specific column.
    // The comparison made is (left < right)
    bool compareColumnValues(int columnIndex, const ExoplanetItem& left,
        const ExoplanetItem& right) const ;

    // Check if a column is numeric. If it isn't, then it is text based
    bool isNumericColumn(int index) const;

    glm::vec4 colorFromColormap(const ExoplanetItem& item,
        const ColorMappedVariable& variable);

    // Write the information about the rendered points to a file
    void writeRenderDataToFile();

    void updateSelectionInRenderable();

    void addOrTargetPlanet(const ExoplanetItem& item);

    bool systemCanBeAdded(const std::string& host) const;
    void addExoplanetSystem(const std::string& host) const;
    void refocusView() const;
    void flyToOverview() const;
    void flyToInsideView() const;

    DataLoader _dataLoader;
    std::vector<ExoplanetItem> _data;
    std::vector<size_t> _filteredData;  // The indices of the items which will be rendered
    std::vector<size_t> _selection;     // Indices of selected data points

    std::vector<size_t> _pinnedPlanets;

    std::map<std::string, std::vector<size_t>> _hostIdToPlanetsMap;

    std::vector<Column> _defaultColumns;
    std::vector<Column> _otherColumns;

    std::deque<bool> _selectedDefaultColumns;
    std::deque<bool> _selectedOtherColumns;

    std::vector<Column> _columns;

    std::vector<const char*> _colormaps;
    bool _colormapWasChanged = true;

    std::vector<ColorMappedVariable> _variableSelection;

    struct ColumnFilterEntry {
        int columnIndex;
        ColumnFilter filter;
        bool enabled = true;
    };
    std::vector<ColumnFilterEntry> _appliedFilters;

    // Filter selection from webpage
    properties::IntListProperty _externalSelection;
    std::vector<size_t> _filteredDataWithoutExternalSelection;
    std::string _lastExternalSelectionTimeStamp;
    bool _useExternalSelection = false;
    bool _externalSelectionChanged = false;

    bool _filterChanged = false;

    // Some settings
    glm::vec4 _nanPointColor = { 0.3f, 0.3f, 0.3f, 1.f };

    bool _useGlyphRendering = true;

    std::list<std::string> _shownPlanetSystemWindows;

    std::optional<std::string> _currentlyTargettedSystem = std::nullopt;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATAVIEWER___H__
