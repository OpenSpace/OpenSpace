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

namespace {
    const char* _loggerCat = "RawTile";
}

namespace openspace {
namespace globebrowsing {

RawTile::RawTile()
    : imageData(nullptr)
    , dimensions(0, 0, 0)
    , tileMetaData(nullptr)
    , tileIndex(0, 0, 0)
    , error(ReadError::None)
    , nBytesImageData(0)
{}
        
RawTile RawTile::createDefaultRes() {
    RawTile defaultRes;
    int w = 8;
    int h = 8;
    defaultRes.dimensions = glm::uvec3(w, h, 1);
    defaultRes.nBytesImageData = w * h * 1 * 3 * 4; // assume max 3 channels, max 4 bytes per pixel
    defaultRes.imageData = new char[defaultRes.nBytesImageData];
    std::fill_n((char*)defaultRes.imageData, defaultRes.nBytesImageData, 0);
    return std::move(defaultRes);
}

void RawTile::serializeMetaData(std::ostream& os) {
    os << dimensions.x << " " << dimensions.y << " " << dimensions.z << std::endl;
    os << tileIndex.x << " " << tileIndex.y << " " << tileIndex.level << std::endl;
    os << static_cast<int>(error) << std::endl;

    // preprocess data
    os << (tileMetaData != nullptr) << std::endl;
    if (tileMetaData != nullptr) {    
        tileMetaData->serialize(os);
    }
        
    os << nBytesImageData << std::endl;
}

RawTile RawTile::deserializeMetaData(std::istream& is) {
    RawTile res;
    is >> res.dimensions.x >> res.dimensions.y >> res.dimensions.z;
    is >> res.tileIndex.x >> res.tileIndex.y >> res.tileIndex.level;
    int err; is >> err; res.error = static_cast<ReadError>(err);
        
    res.tileMetaData = nullptr;
    bool hastileMetaData; 
    is >> hastileMetaData;
    if (hastileMetaData) {
        TileMetaData tileMetaData = TileMetaData::deserialize(is);
        res.tileMetaData = std::make_shared<TileMetaData>(tileMetaData);
    }
        
    is >> res.nBytesImageData;

    char binaryDataSeparator;
    is >> binaryDataSeparator; // not used
        
//    char* buffer = new char[res.nBytesImageData]();
    return std::move(res);
}

} // namespace globebrowsing
} // namespace openspace
