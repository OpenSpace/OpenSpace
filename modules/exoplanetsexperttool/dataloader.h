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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATALOADER___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATALOADER___H__

#include <modules/exoplanetsexperttool/datastructures.h>
#include <string>
#include <vector>

namespace openspace::exoplanets {

class DataLoader {
public:
    DataLoader();

    static DataSettings loadDataSettingsFromJson();
    static DataSettings loadDataSettingsFromJson(const std::filesystem::path& path);
    static std::vector<std::string> csvColumnNames(const std::filesystem::path& path);
    static std::vector<ExoplanetItem> loadData(const DataSettings& settings);

    /**
     * Validate a CSV destination without opening it. Throws if it identifies the
     * protected dataset, is not a regular file, or cannot be inspected.
     *
     * \param targetPath the destination to inspect
     * \param protectedPath the module-configured CSV that must not be overwritten
     * \return Whether the destination exists and requires overwrite confirmation
     */
    static bool validateSaveTarget(const std::filesystem::path& targetPath,
        const std::filesystem::path& protectedPath);

    /**
     * Save data to a CSV file.
     *
     * \param targetPath the filepath for where to save the dataset
     * \param allItems the list of data items in the loaded dataset
     * \param indices the indices which to save. If empty, save all
     * \param columns the list of names for the columns to save in the dataset.
     *                If empty, save all
     * \param computedColumns the computed columns that can be included in the dataset
     * \param includeComputedColumns decides if computed columns should also be saved
     * \param includeXyzPosition decides if the computed XYZ position should also be
     *                           saved
     * \param useParsec if true, use Parsec for the saved position. Otherwise, use meters
     */
    static void saveData(const std::filesystem::path& targetPath,
        const std::vector<ExoplanetItem>& allItems,
        const std::vector<size_t>& indices = {},
        const std::vector<ColumnKey>& columns = {},
        const std::map<ColumnKey, ComputedColumn>& computedColumns = {},
        bool includeComputedColumns = false,
        bool includeXyzPosition = false, bool useParsec = false);
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATALOADER___H__
