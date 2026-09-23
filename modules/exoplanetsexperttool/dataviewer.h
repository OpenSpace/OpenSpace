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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATAVIEWER___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATAVIEWER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/exoplanetsexperttool/datastructures.h>
#include <modules/exoplanetsexperttool/glyphhandler.h>
#include <modules/exoplanetsexperttool/views/colormappingview.h>
#include <modules/exoplanetsexperttool/views/columnselectionview.h>
#include <modules/exoplanetsexperttool/views/computecolumnsview.h>
#include <modules/exoplanetsexperttool/views/filteringview.h>
#include <modules/exoplanetsexperttool/views/systemview.h>
#include <modules/exoplanetsexperttool/views/tableview.h>
#include <openspace/properties/list/intlistproperty.h>
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <unordered_map>
#include <variant>
#include <vector>

namespace openspace::exoplanets {

class DataViewer : public PropertyOwner {
public:
    DataViewer(std::string identifier, std::string guiName = "");

    void initializeData();
    bool loadCsvFile(std::filesystem::path path);

    void initializeGL();

    // Accessors and functions that are needed for the other views

    std::filesystem::path currentDataFile() const;
    std::filesystem::path computedColumnHistoryFile() const;

    bool isNumericColumn(size_t index) const;
    bool isNumericColumn(const ColumnKey& key) const;
    bool hasColumn(const ColumnKey& key) const;

    std::variant<const char*, float> columnValue(const ColumnKey& key,
        const ExoplanetItem& item) const;
    std::variant<const char*, float> columnValue(const ColumnKey& key,
        size_t rowIndex) const;
    size_t columnIndex(const ColumnKey& key) const;
    const char* columnName(const ColumnKey& key) const;
    const char* columnName(size_t columnIndex) const;
    bool isNameColumn(const ColumnKey& key) const;

    std::optional<float> meanValue(const ColumnKey& key) const;

    bool hasColumnDescription(const ColumnKey& key) const;
    const char* columnDescription(const ColumnKey& key) const;

    const std::vector<ExoplanetItem>& data() const;
    const std::vector<size_t>& currentFiltering() const;
    const std::vector<ColumnKey>& columns() const;
    const std::map<ColumnKey, ComputedColumn>& computedColumns() const;
    bool addComputedColumn(ColumnKey key, std::string expression,
        std::vector<float> values);
    bool removeComputedColumn(const ColumnKey& key);
    const DataSettings::DataMapping& dataMapping() const;
    const DataSettings& dataSettings() const;
    bool filterChanged() const;

    ColorMappingView* colorMappingView();
    SystemViewer* systemViewer();
    TableView* tableView();

    std::vector<size_t> planetsForHost(const std::string& hostStar) const;

    size_t externalSelectionSize() const;
    const std::string& lastExternalSelectionTimestamp() const;
    void clearExternalSelection();

    // Set the selected planets to be highlighted in the rendering
    void setSelection(const std::vector<size_t>& indices);

    // Compare the values of two Exoplanets items, given a specific column.
    // The comparison made is (left < right)
    bool compareColumnValues(const ColumnKey& key, const ExoplanetItem& left,
        const ExoplanetItem& right) const;

    // Updates the property in the module so that it matches the filtered rows in the UI,
    // unless a specific list of indices is specified. This property is used by the
    // external webpage to decide what planets to show.
    void updateFilteredRowsProperty(
        std::optional<std::vector<size_t>> customIndices = std::nullopt);

    void render();

    // Render column description on the same line as the previous imgui item,
    // if a description exists.
    void renderColumnDescriptionTooltip(size_t index) const;

    void renderColumnValue(const ColumnKey& key, const ExoplanetItem& item) const;

    // Write the information about the rendered points to a file
    void updateGlyphRenderData();

private:
    bool initializeData(DataSettings settings);

    void renderStartupInfo();
    bool _shouldOpenInfoWindow = true;

    void initializeCallbacks();

    void renderColormapOverviewWindow(bool* open);

    void renderPlanetTooltip(int index) const;
    void handleDoubleClickHoveredPlanet(int index);

    void renderFileMenu();
    void renderSettingsMenu();

    void refocusView() const;
    void flyToOverview() const;
    void flyToInsideView() const;

    void computeMeanForColumn(const ColumnKey& key);

    DataSettings _dataSettings;
    ColumnSelectionView _columnSelectionView;
    std::unique_ptr<ColorMappingView> _colorMappingView;
    std::unique_ptr<FilteringView> _filteringView;
    std::unique_ptr<SystemViewer> _systemViewer;
    std::unique_ptr<TableView> _tableView;
    std::unique_ptr<ComputeColumnsView> _computeColumnsView;

    GlyphHandler _glyphHandler;

    // Bools to toggle the views above
    bool _showTable = true;
    bool _showFilterSettingsWindow = false;
    bool _showColormapWindow = false;
    bool _showComputeColumnsWindow = false;

    bool _showColormapOverviewWindow = false;

    std::vector<ExoplanetItem> _data;
    std::vector<size_t> _filteredData;  // The indices of the items which will be rendered
    std::vector<size_t> _selection;     // Indices of selected data points

    std::unordered_map<std::string, std::vector<size_t>> _hostIdToPlanetsMap;

    std::unordered_map<std::string, float> _meanColumnValues; // For only numerical columns

    std::vector<ColumnKey> _columns; // All loaded columns
    std::map<ColumnKey, ComputedColumn> _computedColumns;

    bool _colormapWasChanged = true;
    bool _filterChanged = false;
    bool _selectionChanged = false;

    bool _externalSelectionChanged = false;
    std::string _lastExternalSelectionTimeStamp;
    IntListProperty _externalSelection;

    // Keep track of whether ctrl is held, to prevent undesired interaction
    // when interacting with glyphs
    bool _holdingCtrl = false;

    std::optional<std::string> _currentlyTargettedSystem = std::nullopt;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATAVIEWER___H__
