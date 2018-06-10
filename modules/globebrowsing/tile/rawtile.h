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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RAWTILE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RAWTILE___H__

#include <modules/globebrowsing/tile/tileindex.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <memory>

namespace openspace::globebrowsing {

struct TileMetaData;
class TileTextureInitData;

struct RawTile {
    enum class ReadError {
        None = 0, // = CE_None
        Debug,    // = CE_Debug
        Warning,  // = CE_Warning
        Failure,  // = CE_Failure
        Fatal     // = CE_Fatal
    };

    char* imageData = nullptr;
    std::shared_ptr<TileMetaData> tileMetaData = nullptr;
    std::shared_ptr<TileTextureInitData> textureInitData = nullptr;
    TileIndex tileIndex = { 0, 0, 0 };
    ReadError error = ReadError::None;
    GLuint pbo = 0;

    static RawTile createDefault(const TileTextureInitData& initData);
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RAWTILE___H__
