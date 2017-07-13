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

#include <modules/globebrowsing/tile/rawtiledatareader/simplerawtiledatareader.h>

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/geometry/angle.h>
#include <modules/globebrowsing/tile/rawtiledatareader/tiledatatype.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>

#include <ghoul/io/texture/texturereader.h>

namespace {
    const std::string _loggerCat = "SimpleRawTileDataReader";
}

namespace openspace {
namespace globebrowsing {

SimpleRawTileDataReader::SimpleRawTileDataReader(const std::string& filePath,
        const TileTextureInitData& initData,
        RawTileDataReader::PerformPreprocessing preprocess)
    : RawTileDataReader(initData, preprocess)
{
    _datasetFilePath = filePath;
    initialize();
}

void SimpleRawTileDataReader::reset() {
    initialize();
}

int SimpleRawTileDataReader::maxChunkLevel() const {
    return 5;
}

float SimpleRawTileDataReader::noDataValueAsFloat() const {
    return 0.0f;
}

int SimpleRawTileDataReader::rasterXSize() const {
    return _dataTexture->dimensions().x;
}

int SimpleRawTileDataReader::rasterYSize() const {
    return _dataTexture->dimensions().y;
}

int SimpleRawTileDataReader::dataSourceNumRasters() const {
    return _dataTexture->numberOfChannels();
}

float SimpleRawTileDataReader::depthOffset() const {
    return 0.0f;
}

float SimpleRawTileDataReader::depthScale() const {
    return 1.0f;
}

void SimpleRawTileDataReader::initialize() {
    _dataTexture = ghoul::io::TextureReader::ref().loadTexture(_datasetFilePath);
    if (_dataTexture == nullptr) {
        throw ghoul::RuntimeError(
            "Unable to read dataset: " + _datasetFilePath +
            ".\nCompiling OpenSpace with GDAL will allow for better support for different"
            "formats."
        );
    }
    _depthTransform = {depthScale(), depthOffset()};
}

RawTile::ReadError SimpleRawTileDataReader::rasterRead(
    int rasterBand, const IODescription& io, char* dataDestination) const
{
    ghoul_assert(static_cast<unsigned int>(io.read.fullRegion.numPixels.x) == _dataTexture->dimensions().x,
        "IODescription does not match data texture.");
    ghoul_assert(static_cast<unsigned int>(io.read.fullRegion.numPixels.y) == _dataTexture->dimensions().y,
        "IODescription does not match data texture.");

    // Modify to match OpenGL texture layout:
    IODescription modifiedIO = io;
    modifiedIO.read.region.start.y =
        modifiedIO.read.fullRegion.numPixels.y -
        modifiedIO.read.region.numPixels.y -
        modifiedIO.read.region.start.y;
  
    char* writeDataStart =
        dataDestination +
        _initData.bytesPerLine() * modifiedIO.write.region.start.y +
        _initData.bytesPerPixel() * modifiedIO.write.region.start.x;

    for (int y = 0; y < modifiedIO.write.region.numPixels.y; ++y) {
        for (int x = 0; x < modifiedIO.write.region.numPixels.x; ++x) {
            char* pixelWriteDestination =
                writeDataStart +
                y * _initData.bytesPerLine() +
                x * _initData.bytesPerPixel();

            int xInSource =
                modifiedIO.read.region.start.x +
                static_cast<float>(x) / modifiedIO.write.region.numPixels.x *
                modifiedIO.read.region.numPixels.x;
            int yInSource =
                modifiedIO.read.region.start.y +
                static_cast<float>(y) / modifiedIO.write.region.numPixels.y *
                modifiedIO.read.region.numPixels.y;

            glm::vec4 sourceTexel = _dataTexture->texelAsFloat(xInSource, yInSource);

            // Different type reinterpreting depending on the type of the target texture
            // the _initData.glType() does not necessarily have to be the same type as
            // the type of the source texture. Therefore the value is cast to float first.
            switch (_initData.glType()) {
                case GL_UNSIGNED_BYTE: {
                    unsigned char value = static_cast<unsigned char>(
                        sourceTexel[rasterBand - 1] * 255
                    );
                    char* bytes = reinterpret_cast<char*>(&value);
                    *pixelWriteDestination = *bytes;
                    break;
                }
                case GL_BYTE: {
                    char value = static_cast<char>(
                        sourceTexel[rasterBand - 1] * 255
                    );
                    char* bytes = reinterpret_cast<char*>(&value);
                    *pixelWriteDestination = *bytes;
                    break;
                }
                case GL_UNSIGNED_SHORT: {
                    unsigned short value = static_cast<unsigned short>(
                        sourceTexel[rasterBand - 1] * 65535
                    );
                    char* bytes = reinterpret_cast<char*>(&value);
                    *pixelWriteDestination = *bytes;
                    break;
                }
                case GL_SHORT: {
                    short value = static_cast<short>(
                        sourceTexel[rasterBand - 1] * 65535
                    );
                    char* bytes = reinterpret_cast<char*>(&value);
                    *pixelWriteDestination = *bytes;
                    break;
                }
                case GL_UNSIGNED_INT: {
                    unsigned int value = static_cast<unsigned int>(
                        sourceTexel[rasterBand - 1] * 4294967295
                    );
                    char* bytes = reinterpret_cast<char*>(&value);
                    *pixelWriteDestination = *bytes;
                    break;
                }
                case GL_INT: {
                    int value = static_cast<int>(
                        sourceTexel[rasterBand - 1] * 4294967295
                    );
                    char* bytes = reinterpret_cast<char*>(&value);
                    *pixelWriteDestination = *bytes;
                    break;
                }
                case GL_FLOAT: {
                    GLfloat value = sourceTexel[rasterBand - 1];
                    char* bytes = reinterpret_cast<char*>(&value);
                    *pixelWriteDestination = *bytes;
                    break;
                }
                default: {
                    ghoul_assert(false, "Unknown texture type");
                    return RawTile::ReadError::Failure;
                }
            }
        }
    }
    return RawTile::ReadError::None;
}


} // namespace globebrowsing
} // namespace openspace
