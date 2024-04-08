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

#ifndef __OPENSPACE_MODULE_GAIA___READFILEJOB___H__
#define __OPENSPACE_MODULE_GAIA___READFILEJOB___H__

#include <openspace/util/concurrentjobmanager.h>

#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <filesystem>

namespace openspace::gaia {

struct ReadFileJob : public Job<std::vector<std::vector<float>>> {
    /**
     * Constructs a Job that will read a single FITS file in a concurrent thread and
     * divide the star data into 8 octants depending on position.
     *
     * \param filePath The file from which to load the data
     * \param allColumns Define which columns that will be read, it should correspond to
     *        the pre-defined order in the job. If additional columns are defined they
     *        will be read but slow down the process. Proper conversions of positions and
     *        velocities will take place and all values will be checked for NaNs. If
     *        \p firstRow is < 1 then reading will begin at first row in table. If
     *        \p lastRow < firstRow then entire table will be read
     * \param firstRow The index of the first row to be read
     * \param lastRow The index of the last row to be read
     * \param nDefaultCols Defines how many columns that will be read per star
     * \param nValuesPerStar Defines how many values that will be stored per star
     * \param fitsReader The reader that should be used in this job
     */
    ReadFileJob(std::filesystem::path filePath, std::vector<std::string> allColumns,
        int firstRow, int lastRow, size_t nDefaultCols, int nValuesPerStar,
        std::shared_ptr<FitsFileReader> fitsReader);

    ~ReadFileJob() override = default;

    void execute() override;

    std::vector<std::vector<float>> product() override;

private:
    std::filesystem::path _inFilePath;
    int _firstRow;
    int _lastRow;
    size_t _nDefaultCols;
    int _nValuesPerStar;
    std::vector<std::string> _allColumns;

    std::shared_ptr<FitsFileReader> _fitsFileReader;
    std::vector<std::vector<float>> _octants;
};

} // namespace openspace::gaiamission

#endif // __OPENSPACE_MODULE_GAIA___READFILEJOB___H__
