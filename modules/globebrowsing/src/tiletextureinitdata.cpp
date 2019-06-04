/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/globebrowsing/src/tiletextureinitdata.h>

namespace {

const glm::ivec2 TilePixelStartOffset = glm::ivec2(-2);
const glm::ivec2 TilePixelSizeDifference = glm::ivec2(4);

size_t numberOfRasters(ghoul::opengl::Texture::Format format) {
    switch (format) {
        case ghoul::opengl::Texture::Format::Red:
            return 1;
        case ghoul::opengl::Texture::Format::RG:
            return 2;
        case ghoul::opengl::Texture::Format::RGB:
        case ghoul::opengl::Texture::Format::BGR:
            return 3;
        case ghoul::opengl::Texture::Format::RGBA:
        case ghoul::opengl::Texture::Format::BGRA:
            return 4;
        default: {
            ghoul_assert(false, "Unknown format");
            throw ghoul::MissingCaseException();
        }
    }
}

size_t numberOfBytes(GLenum glType) {
    switch (glType) {
        case GL_UNSIGNED_BYTE:  return sizeof(GLubyte);
        case GL_BYTE:           return sizeof(GLbyte);
        case GL_UNSIGNED_SHORT: return sizeof(GLushort);
        case GL_SHORT:          return sizeof(GLshort);
        case GL_UNSIGNED_INT:   return sizeof(GLuint);
        case GL_INT:            return sizeof(GLint);
        case GL_HALF_FLOAT:     return sizeof(GLhalf);
        case GL_FLOAT:          return sizeof(GLfloat);
        case GL_DOUBLE:         return sizeof(GLdouble);
        default:
            ghoul_assert(false, "Unknown data type");
            throw ghoul::MissingCaseException();
    }
}

unsigned int uniqueIdForTextureFormat(ghoul::opengl::Texture::Format textureFormat) {
    switch (textureFormat) {
        case ghoul::opengl::Texture::Format::Red:            return 0;
        case ghoul::opengl::Texture::Format::RG:             return 1;
        case ghoul::opengl::Texture::Format::RGB:            return 2;
        case ghoul::opengl::Texture::Format::BGR:            return 3;
        case ghoul::opengl::Texture::Format::RGBA:           return 4;
        case ghoul::opengl::Texture::Format::BGRA:           return 5;
        case ghoul::opengl::Texture::Format::DepthComponent: return 6;
        default:                                      throw ghoul::MissingCaseException();
    }
}

openspace::globebrowsing::TileTextureInitData::HashKey calculateHashKey(
                                                             const glm::ivec3& dimensions,
                                             const ghoul::opengl::Texture::Format& format,
                                                                     const GLenum& glType)
{
    ghoul_assert(dimensions.x > 0, "Incorrect dimension");
    ghoul_assert(dimensions.y > 0, "Incorrect dimension");
    ghoul_assert(dimensions.x <= 1024, "Incorrect dimension");
    ghoul_assert(dimensions.y <= 1024, "Incorrect dimension");
    ghoul_assert(dimensions.z == 1, "Incorrect dimension");
    unsigned int formatId = uniqueIdForTextureFormat(format);
    ghoul_assert(formatId < 256, "Incorrect format");

    openspace::globebrowsing::TileTextureInitData::HashKey res = 0ULL;

    res |= dimensions.x;
    res |= dimensions.y << 10;
    res |= static_cast<std::underlying_type_t<GLenum>>(glType) << (10 + 16);
    res |= formatId << (10 + 16 + 4);

    return res;
}

} // namespace

namespace openspace::globebrowsing {

TileTextureInitData tileTextureInitData(layergroupid::GroupID id, bool shouldPadTiles,
    size_t preferredTileSize)
{
    switch (id) {
        case layergroupid::GroupID::HeightLayers: {
            const size_t tileSize = preferredTileSize ? preferredTileSize : 64;
            return TileTextureInitData(
                tileSize,
                tileSize,
                GL_FLOAT,
                ghoul::opengl::Texture::Format::Red,
                TileTextureInitData::PadTiles(shouldPadTiles),
                TileTextureInitData::ShouldAllocateDataOnCPU::Yes
            );
        }
        case layergroupid::GroupID::ColorLayers: {
            const size_t tileSize = preferredTileSize ? preferredTileSize : 512;
            return TileTextureInitData(
                tileSize,
                tileSize,
                GL_UNSIGNED_BYTE,
                ghoul::opengl::Texture::Format::BGRA,
                TileTextureInitData::PadTiles(shouldPadTiles)
            );
        }
        case layergroupid::GroupID::Overlays: {
            const size_t tileSize = preferredTileSize ? preferredTileSize : 512;
            return TileTextureInitData(
                tileSize,
                tileSize,
                GL_UNSIGNED_BYTE,
                ghoul::opengl::Texture::Format::BGRA,
                TileTextureInitData::PadTiles(shouldPadTiles)
            );
        }
        case layergroupid::GroupID::NightLayers: {
            const size_t tileSize = preferredTileSize ? preferredTileSize : 512;
            return TileTextureInitData(
                tileSize,
                tileSize,
                GL_UNSIGNED_BYTE,
                ghoul::opengl::Texture::Format::BGRA,
                TileTextureInitData::PadTiles(shouldPadTiles)
            );
        }
        case layergroupid::GroupID::WaterMasks: {
            const size_t tileSize = preferredTileSize ? preferredTileSize : 512;
            return TileTextureInitData(
                tileSize,
                tileSize,
                GL_UNSIGNED_BYTE,
                ghoul::opengl::Texture::Format::BGRA,
                TileTextureInitData::PadTiles(shouldPadTiles)
            );
        }
        default: {
            throw ghoul::MissingCaseException();
        }
    }
}

TileTextureInitData::TileTextureInitData(size_t width, size_t height, GLenum type,
                                         ghoul::opengl::Texture::Format textureFormat,
                                         PadTiles pad, ShouldAllocateDataOnCPU allocCpu)
    : dimensions(width, height, 1)
    , tilePixelStartOffset(pad ? TilePixelStartOffset : glm::ivec2(0))
    , tilePixelSizeDifference(pad ? TilePixelSizeDifference : glm::ivec2(0))
    , glType(type)
    , ghoulTextureFormat(textureFormat)
    , nRasters(numberOfRasters(ghoulTextureFormat))
    , bytesPerDatum(numberOfBytes(glType))
    , bytesPerPixel(nRasters * bytesPerDatum)
    , bytesPerLine(bytesPerPixel * width)
    , totalNumBytes(bytesPerLine * height)
    , shouldAllocateDataOnCPU(allocCpu)
    , padTiles(pad)
    , hashKey(calculateHashKey(dimensions, ghoulTextureFormat, glType))
{}

TileTextureInitData TileTextureInitData::operator=(const TileTextureInitData& rhs) {
    if (this == &rhs) {
        return TileTextureInitData(*this);
    }

    return TileTextureInitData(rhs);
}

TileTextureInitData TileTextureInitData::operator=(TileTextureInitData&& rhs) {
    if (this == &rhs) {
        return TileTextureInitData(*this);
    }

    return TileTextureInitData(rhs);
}

} // namespace openspace::globebrowsing
