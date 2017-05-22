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

#include <modules/globebrowsing/tile/tiletextureinitdata.h>
#include <modules/globebrowsing/tile/rawtiledatareader/tiledatatype.h>

namespace openspace {
namespace globebrowsing {

const glm::ivec2 TileTextureInitData::tilePixelStartOffset = glm::ivec2(-2);
const glm::ivec2 TileTextureInitData::tilePixelSizeDifference = glm::ivec2(4);

TileTextureInitData::TileTextureInitData(size_t width, size_t height, GLuint glType,
    Format textureFormat, ShouldAllocateDataOnCPU shouldAllocateDataOnCPU)
    : _glType(glType)
    , _ghoulTextureFormat(textureFormat)
    , _shouldAllocateDataOnCPU(shouldAllocateDataOnCPU)
{
    _dimensionsWithoutPadding = glm::ivec3(width, height, 1);
    _dimensionsWithPadding = glm::ivec3(
        width + tilePixelSizeDifference.x, height + tilePixelSizeDifference.y, 1);
    _nRasters = tiledatatype::numberOfRasters(_ghoulTextureFormat);
    _bytesPerDatum = tiledatatype::numberOfBytes(glType);
    _bytesPerPixel = _nRasters * _bytesPerDatum;
    _bytesPerLine = _bytesPerPixel * _dimensionsWithPadding.x;
    _totalNumBytes = _bytesPerLine * _dimensionsWithPadding.y;
    _glTextureFormat = tiledatatype::glTextureFormat(_glType,
        _ghoulTextureFormat);
    calculateHashKey();
};

TileTextureInitData::TileTextureInitData(const TileTextureInitData& original)
    : TileTextureInitData(
        original.dimensionsWithoutPadding().x,
        original.dimensionsWithoutPadding().y,
        original.glType(),
        original.ghoulTextureFormat(),
        original.shouldAllocateDataOnCPU() ? ShouldAllocateDataOnCPU::Yes : ShouldAllocateDataOnCPU::No)
{ };

glm::ivec3 TileTextureInitData::dimensionsWithPadding() const {
    return _dimensionsWithPadding;
}

glm::ivec3 TileTextureInitData::dimensionsWithoutPadding() const {
    return _dimensionsWithoutPadding;
}

size_t TileTextureInitData::nRasters() const {
    return _nRasters;
}

size_t TileTextureInitData::bytesPerDatum() const {
    return _bytesPerDatum;
}

size_t TileTextureInitData::bytesPerPixel() const {
    return _bytesPerPixel;
}

size_t TileTextureInitData::bytesPerLine() const {
    return _bytesPerLine;
}

size_t TileTextureInitData::totalNumBytes() const {
    return _totalNumBytes;
}

GLuint TileTextureInitData::glType() const {
    return _glType;
}

TileTextureInitData::Format TileTextureInitData::ghoulTextureFormat() const {
    return _ghoulTextureFormat;
}

GLint TileTextureInitData::glTextureFormat() const {
    return _glTextureFormat;
}

bool TileTextureInitData::shouldAllocateDataOnCPU() const {
    return _shouldAllocateDataOnCPU;
}

TileTextureInitData::HashKey TileTextureInitData::hashKey() const {
    return _hashKey;
}

void TileTextureInitData::calculateHashKey() {
    ghoul_assert(_dimensionsWithoutPadding.x > 0, "Incorrect dimension");
    ghoul_assert(_dimensionsWithoutPadding.y > 0, "Incorrect dimension");
    ghoul_assert(_dimensionsWithoutPadding.x <= 1024, "Incorrect dimension");
    ghoul_assert(_dimensionsWithoutPadding.y <= 1024, "Incorrect dimension");
    ghoul_assert(_dimensionsWithoutPadding.z == 1, "Incorrect dimension");
    unsigned int format = getUniqueIdFromTextureFormat(_ghoulTextureFormat);
    ghoul_assert(format < 256, "Incorrect format");

    _hashKey = 0LL;

    _hashKey |= _dimensionsWithoutPadding.x;
    _hashKey |= _dimensionsWithoutPadding.y << 10;
    _hashKey |= _glType << (10 + 16);
    _hashKey |= format << (10 + 16 + 4);
};

unsigned int TileTextureInitData::getUniqueIdFromTextureFormat(
    Format textureFormat) const
{
    using Format = Format;
    switch (textureFormat) {
        case Format::Red:
            return 0;
        case Format::RG:
            return 1;
        case Format::RGB:
            return 2;
        case Format::BGR:
            return 3;
        case Format::RGBA:
            return 4;
        case Format::BGRA:
            return 5;
        case Format::DepthComponent:
            return 6;
        default:
            ghoul_assert(false, "Unknown texture format");
    }
}

} // namespace globebrowsing
} // namespace openspace
