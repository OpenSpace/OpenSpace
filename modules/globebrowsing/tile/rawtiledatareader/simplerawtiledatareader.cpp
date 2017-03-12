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

namespace {
    const std::string _loggerCat = "SimpleRawTileDataReader";
}

namespace openspace {
namespace globebrowsing {

SimpleRawTileDataReader::SimpleRawTileDataReader(
    const std::string& filePath, const Configuration& config)
    : RawTileDataReader(config)
{
    using namespace ghoul::filesystem;
    std::string correctedPath = FileSystem::ref().pathByAppendingComponent(
        _initData.initDirectory, filePath);
    throw ghoul::RuntimeError(
        "SimpleRawTileDataReader is not yet implemented! Failed to load dataset:\n" +
        filePath + ". \nCompile OpenSpace with GDAL.");

    _initData = { "",  filePath, config.minimumTilePixelSize, config.dataType };
    ensureInitialized();
}

SimpleRawTileDataReader::~SimpleRawTileDataReader() {
    
}

void SimpleRawTileDataReader::reset() {
    _cached._maxLevel = -1;
    initialize();
}

int SimpleRawTileDataReader::maxChunkLevel() {
    return _cached._maxLevel;
}

float SimpleRawTileDataReader::noDataValueAsFloat() const {
    return 0.0f;
}

int SimpleRawTileDataReader::rasterXSize() const {
    return 0;
}

int SimpleRawTileDataReader::rasterYSize() const {
    return 0;
}

float SimpleRawTileDataReader::depthOffset() const {
    return 0.0f;
}

float SimpleRawTileDataReader::depthScale() const {
    return 1.0f;
}

IODescription SimpleRawTileDataReader::getIODescription(const TileIndex& tileIndex) const {
    IODescription io;

    return io;
}

void SimpleRawTileDataReader::initialize() {

}

char* SimpleRawTileDataReader::readImageData(
    IODescription& io, RawTile::ReadError& worstError) const {
    
}

RawTile::ReadError SimpleRawTileDataReader::rasterRead(
    int rasterBand, const IODescription& io, char* dataDestination) const
{
    RawTile::ReadError error = RawTile::ReadError::Fatal;
    return error;
}


} // namespace globebrowsing
} // namespace openspace
