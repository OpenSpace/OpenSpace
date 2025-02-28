/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_GAIA___READFITSTASK___H__
#define __OPENSPACE_MODULE_GAIA___READFITSTASK___H__

#include <openspace/util/task.h>

#include <openspace/util/threadpool.h>
#include <openspace/util/concurrentjobmanager.h>
#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <filesystem>

namespace openspace {

namespace documentation { struct Documentation; }

class ReadFitsTask : public Task {
public:
    explicit ReadFitsTask(const ghoul::Dictionary& dictionary);
    ~ReadFitsTask() override = default;

    std::string description() override;
    void perform(const Task::ProgressCallback& onProgress) override;
    static documentation::Documentation Documentation();

private:
    const size_t MAX_SIZE_BEFORE_WRITE = 48000000; // ~183MB -> 2M stars with 24 values
    //const size_t MAX_SIZE_BEFORE_WRITE = 9000000; // ~34MB -> 0,5 stars with 18 values

    /**
     * Reads a single FITS file and stores ordered star data in one binary file.
     */
    void readSingleFitsFile(const Task::ProgressCallback& progressCallback);

    /**
     * Reads all FITS files in a folder with multiple threads and stores ordered star
     * data into 8 binary files.
     */
    void readAllFitsFilesFromFolder(const Task::ProgressCallback& progressCallback);

    /**
     * Writes data to an octant file.
     *
     * \param data The data that should be be written to file
     * \param index The index of the octant that should be written
     * \param isFirstWrite Defines if this is the first write to specified octant, if so
     *        the file is created, otherwise the accumulated data is appended to the end
     *        of the file
     * \param nValuesPerStar The number of values that should be stored per star
     */
    int writeOctantToFile(const std::vector<float>& data, int index,
        std::vector<bool>& isFirstWrite, int nValuesPerStar);

    std::filesystem::path _inFileOrFolderPath;
    std::filesystem::path _outFileOrFolderPath;
    bool _singleFileProcess = false;
    size_t _threadsToUse = 1;
    int _firstRow = 0;
    int _lastRow = 0;
    std::vector<std::string> _allColumnNames;
    std::vector<std::string> _filterColumnNames;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAIA___READFITSTASK___H__
