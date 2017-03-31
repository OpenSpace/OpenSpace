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

namespace CCfits { class PHDU; }
namespace ghoul { namespace opengl{ class Texture; }}

namespace openspace {

class FitsFileReader {
public:
    static std::unique_ptr<ghoul::opengl::Texture> loadTexture(const std::string& path);
    static std::unique_ptr<ghoul::opengl::Texture> loadTextureFromMemory(const std::string& buffer);

    static std::valarray<float> readImage(const std::string& path);
    // Fits will throw error if keyword does not exist in header
    // TODO(mnoven): Make map template and remove float
    static std::unordered_map<std::string, float> readHeader(const std::string& path, std::vector<std::string>& keywords);

private:
	// Only for debugging
	static void dump(CCfits::PHDU& image);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_KAMELEON___FITSFILEREADER___H__
