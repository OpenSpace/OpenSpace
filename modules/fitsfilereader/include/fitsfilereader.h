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

#ifndef __OPENSPACE_MODULE_FITSFILEREADER___FITSFILEREADER___H__
#define __OPENSPACE_MODULE_FITSFILEREADER___FITSFILEREADER___H__

#include <filesystem>
#include <string>
#include <memory>
#include <mutex>
#include <unordered_map>
#include <valarray>
#include <vector>

namespace CCfits {
    class FITS;
    class PHDU;
    class ExtHDU;
} // namespace CCfits

namespace ghoul::opengl { class Texture; }

namespace openspace {

template<typename T>
struct ImageData {
    std::valarray<T> contents;
    long int width;
    long int height;
};

template<typename T>
struct TableData {
    std::unordered_map<std::string, std::vector<T>> contents;
    int readRows;
    long int optimalRowsize;
    std::string name;
};

class FitsFileReader {
public:
    explicit FitsFileReader(bool verboseMode);
    ~FitsFileReader();

    template<typename T>
    std::shared_ptr<ImageData<T>> readImage(const std::filesystem::path& path);

    template<typename T>
    std::shared_ptr<std::unordered_map<std::string, T>> readHeader(
        std::vector<std::string>& keywords);
    template<typename T>
    std::shared_ptr<T> readHeaderValue(const std::string key);

    /**
     * Read specified table columns from fits file. If `readAll` is set to true the entire
     * table will be read before the selected columns, which makes the function take a lot
     * longer if it's a big file. If no HDU index is given the current Extension HDU will
     * be read from.
     */
    template<typename T>
    std::shared_ptr<TableData<T>> readTable(const std::filesystem::path& path,
        const std::vector<std::string>& columnNames, int startRow = 1, int endRow = 10,
        int hduIdx = 1, bool readAll = false);

    /**
     * Reads a single FITS file with pre-defined columns (defined for Viennas TGAS-file).
     * Returns a vector with all read stars with `nValuesPerStar`. If additional columns
     * are given by `filterColumnNames`, they will be read but it will slow doen the
     * reading tremendously.
     */
    std::vector<float> readFitsFile(std::filesystem::path filePath, int& nValuesPerStar,
        int firstRow, int lastRow, std::vector<std::string> filterColumnNames,
        int multiplier = 1);

    /**
     * Reads a single SPECK file and returns a vector with `nRenderValues` per star. Reads
     * data in pre-defined order based on AMNH's star data files.
     */
    std::vector<float> readSpeckFile(const std::filesystem::path& filePath,
        int& nRenderValues);

private:
    std::unique_ptr<CCfits::FITS> _infile;
    bool _verboseMode;

    bool isPrimaryHDU();
    template<typename T>
    std::shared_ptr<ImageData<T>> readImageInternal(CCfits::PHDU& image);

    template<typename T>
    std::shared_ptr<ImageData<T>> readImageInternal(CCfits::ExtHDU& image);

    mutable std::mutex _mutex;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FITSFILEREADER___FITSFILEREADER___H__
