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

#include <modules/globebrowsing/tile/rawtile.h>

#include <modules/globebrowsing/tile/tilemetadata.h>
#include <modules/globebrowsing/tile/tiletextureinitdata.h>

namespace openspace {
namespace globebrowsing {

RawTile::RawTile()
    : imageData(nullptr)
    , tileMetaData(nullptr)
    , textureInitData(nullptr)
    , tileIndex(0, 0, 0)
    , error(ReadError::None)
    , pbo(0)
{}

RawTile RawTile::createDefault(const TileTextureInitData& initData) {
    RawTile defaultRes;
    defaultRes.textureInitData = std::make_shared<TileTextureInitData>(initData);
    defaultRes.imageData = new char[initData.totalNumBytes()];
    std::fill_n(static_cast<char*>(defaultRes.imageData), initData.totalNumBytes(), 0);
    return defaultRes;

}

} // namespace globebrowsing
} // namespace openspace
