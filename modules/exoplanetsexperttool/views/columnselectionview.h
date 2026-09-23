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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLUMNSELECTIONVIEW___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLUMNSELECTIONVIEW___H__

#include <modules/exoplanetsexperttool/datastructures.h>
#include <string>
#include <vector>

namespace openspace::exoplanets {

class ColumnSelectionView {
public:
    ColumnSelectionView() = default;

    /**
     * Returns the columns in order of:
     *   1. Name column
     *   2. Named columns from the \p dataSettings
     *   3. Any other columns in the dataset
     */
    std::vector<ColumnKey> initializeColumnsFromData(
        const std::vector<ExoplanetItem>& data, const DataSettings& dataSettings);

    bool renderColumnSettingsView(const DataSettings& dataSettings);

    void updateComputedColumns(const std::map<ColumnKey, ComputedColumn>& columns);

    std::vector<ColumnKey> orderedSelectedColumns() const;

private:
    std::vector<ColumnKey> _namedColumns;
    std::vector<ColumnKey> _otherColumns;
    std::vector<ColumnKey> _computedColumns;

    // The name column is always selected
    ColumnKey _nameColumn;

    // Column selection for table views
    std::vector<bool> _selectedNamedColumns;
    std::vector<bool> _selectedOtherColumns;
    std::vector<bool> _selectedComputedColumns;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___COLUMNSELECTIONVIEW___H__
