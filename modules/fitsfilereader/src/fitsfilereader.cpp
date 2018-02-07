/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <ghoul/logging/logmanager.h>
#include <CCfits>
#include <algorithm>
#include <cstring>

using namespace CCfits;

namespace {
    const char* _loggerCat = "FitsFileReader";
}

namespace openspace {

FitsFileReader::FitsFileReader(bool verboseMode) {
    _verboseMode = verboseMode;
    FITS::setVerboseMode(_verboseMode);
}

FitsFileReader::~FitsFileReader() {
    _infile->destroy();
    _infile = nullptr;
}

bool FitsFileReader::isPrimaryHDU() {
    return _infile->extension().size() == 0;
}

template <typename T>
std::shared_ptr<ImageData<T>> FitsFileReader::readImage(const std::string& path) {
    try {
        _infile = std::make_unique<FITS>(path, Read, true);
        // Primary HDU Object
        if (isPrimaryHDU()) {
            const std::shared_ptr<ImageData<T>> im
                  = readImageInternal<T>(_infile->pHDU());
            return std::move(im);
        }
        // Extension HDU Object
        const std::shared_ptr<ImageData<T>> im
              = readImageInternal<T>(_infile->currentExtension());
        return std::move(im);
    } catch (FitsException& e){
        LERROR("Could not read FITS image from table");
    }

    return nullptr;
}

template <typename T>
std::shared_ptr<std::unordered_map<std::string, T>>
      FitsFileReader::readHeader(std::vector<std::string>& keywords)
{
    try {
        HDU* image = isPrimaryHDU() ? static_cast<HDU*>(&_infile->pHDU()) :
                                      static_cast<HDU*>(&_infile->currentExtension());

        std::vector<T> values;
        image->readKeys(keywords, values);

        if (values.size() != keywords.size()) {
            LERROR("Number of keywords does not match number of values");
        }

        std::unordered_map<std::string, T> result;
        std::transform(keywords.begin(), keywords.end(), values.begin(),
                       std::inserter(result, result.end()),
                       [](std::string key, T value) {
            return std::make_pair(key, value);
        });
        return std::make_shared<std::unordered_map<std::string, T>>(std::move(result));
    } catch (FitsException& e) {
        LERROR("Could not read FITS header");
    }
    return nullptr;
}

template <typename T>
std::shared_ptr<T> FitsFileReader::readHeaderValue(const std::string key) {
    try {
        HDU* image = isPrimaryHDU() ? static_cast<HDU*>(&_infile->pHDU()) :
                                      static_cast<HDU*>(&_infile->currentExtension());

        T value;
        image->readKey(key, value);
        return std::make_unique<T>(value);
    } catch (FitsException& e) {
        LERROR("Could not read FITS key");
    }
    return nullptr;
}

// Read specified table columns from fits file. 
// If readAll variable is set to true the entire table will be read before the selected columns,
// which makes the function take a lot longer if it's a big file. 
// If no HDU index is given the currentExtension will be read from. 
template<typename T>
std::shared_ptr<TableData<T>> FitsFileReader::readTable(std::string& path,
    std::vector<std::string>& columnNames, int startRow, int endRow, int hduIdx, bool readAll) {
    try {

        _infile = std::make_unique<FITS>(path, Read, readAll);

        // Make sure FITS file is not a Primary HDU Object (aka an image).
        if (!isPrimaryHDU()) {

            ExtHDU& table = _infile->extension(hduIdx);
            int numCols = columnNames.size();
            std::unordered_map<string, std::vector<T>> contents;

            for (int i = 0; i < numCols; ++i) {
                std::vector<T> columnData;
                LINFO("Read column: " + columnNames[i]);
                table.column(columnNames[i]).read(columnData, startRow, endRow);
                contents[columnNames[i]] = columnData;
            }

            // Create TableData object of table contents.
            TableData<T> loadedTable = {
                std::move(contents), table.getRowsize(), table.name()
            };

            return std::make_shared<TableData<T>>(loadedTable);
        }
    }
    catch (FitsException& e) {
        LERROR("Could not read FITS table from file. Make sure it's not an image file.");
    }

    return nullptr;
}


// This is pretty annoying, the read method is not derived from the HDU class
// in CCfits - need to explicitly cast to the sub classes to access read
template<typename T>
const std::shared_ptr<ImageData<T>> FitsFileReader::readImageInternal(ExtHDU& image) {
   try {
        std::valarray<T> contents;
        image.read(contents);
        ImageData<T> im = {std::move(contents), image.axis(0), image.axis(1)};
        return std::make_shared<ImageData<T>>(im);
    } catch (FitsException& e){
        LERROR("Could not read FITS image EXTHDU");
    }
    return nullptr;
}

template<typename T>
const std::shared_ptr<ImageData<T>> FitsFileReader::readImageInternal(PHDU& image) {
    try {
        std::valarray<T> contents;
        image.read(contents);
        ImageData<T> im = {std::move(contents), image.axis(0), image.axis(1)};
        return std::make_shared<ImageData<T>>(im);
    } catch (FitsException& e){
        LERROR("Could not read FITS image PHDU");
    }
    return nullptr;
}

// Type deductions
template std::shared_ptr<ImageData<unsigned long>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<long>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<unsigned int>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<int>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<double>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<float>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<unsigned char>> FitsFileReader::readImage(const std::string& path);

template std::shared_ptr<std::unordered_map<std::string, float>> FitsFileReader::readHeader(std::vector<std::string>& keywords);
template std::shared_ptr<std::unordered_map<std::string, std::string>> FitsFileReader::readHeader(std::vector<std::string>& keywords);

template std::shared_ptr<float> FitsFileReader::readHeaderValue(const std::string key);
template std::shared_ptr<int> FitsFileReader::readHeaderValue(const std::string key);
template std::shared_ptr<std::string> FitsFileReader::readHeaderValue(const std::string key);

template std::shared_ptr<TableData<float>> FitsFileReader::readTable(std::string& path, 
    std::vector<std::string>& columnNames,
    int startRow, int endRow, int hduIdx, bool readAll);
} // namespace openspace
