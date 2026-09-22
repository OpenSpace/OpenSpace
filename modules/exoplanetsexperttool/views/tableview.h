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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___TABLEVIEW___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___TABLEVIEW___H__

#include <modules/exoplanetsexperttool/datastructures.h>

namespace openspace::exoplanets {

class DataViewer;

class TableView {
public:
    TableView(DataViewer& dataViewer, const std::vector<ColumnKey>& columns);

    void updateColumns(const std::vector<ColumnKey>& columns);

    void render(bool* open, std::vector<size_t>& filteredDataRows);

    // Render the table with the given data rows, optionally filtering by a search string
    void renderTable(const std::string& tableId, std::vector<size_t>& dataRows,
        bool useFixedHeight, std::string_view search = "");

private:
    // Render the first column in the table, which is used for navigation
    void renderFirstTableColumn(const ExoplanetItem& item, size_t row);

    DataViewer& _dataViewer;

    std::vector<size_t> _selection;     // Indices of selected data points
    std::vector<size_t> _pinnedItems;

    // ImGui table only supports a limited number of columns, so we need to keep track of
    // the columns we want to display
    std::vector<ColumnKey> _columns;

};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___TABLEVIEW___H__
