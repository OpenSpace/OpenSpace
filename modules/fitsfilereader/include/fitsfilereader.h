/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_KAMELEON___FITSFILEREADER___H__
#define __OPENSPACE_MODULE_KAMELEON___FITSFILEREADER___H__

#include <string>
#include <memory>
#include <vector>
#include <unordered_map>

namespace CCfits { class PHDU; class ExtHDU; }
namespace ghoul { namespace opengl{ class Texture; }}

namespace openspace {

class FitsFileReader {
public:
    static void open(const std::string& path);
    static void close();

    template<typename T>
    static std::valarray<T> readImage();
    // Fits will throw error if keyword does not exist in header
    template<typename T>
    static const std::unordered_map<std::string, T> readHeader(std::vector<std::string>& keywords);
    template<typename T>
    static const T readHeaderValue(const std::string key);

    //TODO(mnoven): Don't assume that client has read image content already
    static const std::pair<int, int>& getImageSize();

    static std::unique_ptr<ghoul::opengl::Texture> loadTextureFromMemory(const std::string& buffer);
    static std::unique_ptr<ghoul::opengl::Texture> loadTexture();

private:
	// Only for debugging
	static void dump(CCfits::PHDU& image);
    static const bool isPrimaryHDU();
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_KAMELEON___FITSFILEREADER___H__
