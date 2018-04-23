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

#ifndef __OPENSPACE_MODULE_FITSFILEREADER___FITSFILEREADER___H__
#define __OPENSPACE_MODULE_FITSFILEREADER___FITSFILEREADER___H__

#include <string>
#include <memory>
#include <vector>
#include <unordered_map>
#include <CCfits>

namespace CCfits { class FITS; class PHDU; class ExtHDU; }
namespace ghoul { namespace opengl{ class Texture; }}

namespace openspace {

template<typename T>
struct ImageData {
    std::valarray<T> contents;
    long int width;
    long int height;
};

template<typename T>
struct TableData {
    std::unordered_map<string, std::vector<T>> contents;
    int readRows;
    long int optimalRowsize;
    string name;
};

class FitsFileReader {
public:
    FitsFileReader(bool verboseMode);
    ~FitsFileReader();
    
    template<typename T>
    std::shared_ptr<ImageData<T>> readImage(const std::string& path);
    template<typename T>
    std::shared_ptr<std::unordered_map<std::string, T>> readHeader(std::vector<std::string>& keywords);
    template<typename T>
    std::shared_ptr<T> readHeaderValue(const std::string key);

    template<typename T>
    std::shared_ptr<TableData<T>> readTable(std::string& path, const std::vector<std::string>& columnNames,
        int startRow = 1, int endRow = 10, int hduIdx = 1, bool readAll = false);

private:
    std::unique_ptr<CCfits::FITS> _infile;
    bool _verboseMode;

    bool isPrimaryHDU();
    template<typename T>
    const std::shared_ptr<ImageData<T>> readImageInternal(CCfits::PHDU& image);
    template<typename T>
    const std::shared_ptr<ImageData<T>> readImageInternal(CCfits::ExtHDU& image);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FITSFILEREADER___FITSFILEREADER___H__
