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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___FILTERINGVIEW___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___FILTERINGVIEW___H__

#include <modules/exoplanetsexperttool/columnfilter.h>
#include <modules/exoplanetsexperttool/datastructures.h>

namespace openspace::exoplanets {

class DataViewer;

class FilteringView {
public:
    FilteringView(DataViewer& dataViewer, const DataSettings& dataSettings);

    bool isUsingRowFiltering() const;
    bool isUsingExternalFiltering() const;

    // Return true if filtering was changed
    bool renderFilterSettings();

    // Return the rows matching the current filtering
    std::vector<size_t> applyFiltering(const std::vector<ExoplanetItem>& data,
        const std::vector<int>& externalSelection);

private:
    bool renderColumnFilterSettings();
    bool renderRowLimitFilterSettings();
    bool renderExternalFilterSettings();

    void applyRowLimit(const std::vector<ExoplanetItem>& data,
        std::vector<size_t>& prefilteredData);
    void applyExternalSelection(const std::vector<int>& externalSelection,
        std::vector<size_t>& prefilteredData);

    struct ColumnFilterEntry {
        size_t columnIndex;
        ColumnFilter filter;
        bool enabled = true;
    };
    std::vector<ColumnFilterEntry> _columnFilters;
    std::vector<std::vector<bool>> _quickFilterFlags;

    bool _limitNumberOfRows = false;
    int _nRows = 100;
    bool _useHighestValue = true;
    size_t _rowLimitColumnIndex = 0;

    // Filter selection from webpage
    bool _useExternalSelection = false;
    bool _overrideInternalSelection = false;

    DataViewer& _dataViewer;
    const std::vector<DataSettings::QuickFilterGroup>& _quickFilterGroups;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___FILTERINGVIEW___H__
