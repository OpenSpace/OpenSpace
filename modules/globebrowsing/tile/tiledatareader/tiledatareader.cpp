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

#include <modules/globebrowsing/tile/tiledatareader.h>

#include <modules/globebrowsing/geometry/angle.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/tile/pixelregion.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tiledatatype.h>
#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tilemetadata.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>

#include <algorithm>
#include <float.h>
#include <limits>
#include <sstream>

namespace {
    const char* _loggerCat = "TileDataReader";
}

namespace openspace {
namespace globebrowsing {

std::ostream& operator<<(std::ostream& os, const PixelRegion& pr) {
    return os << pr.start.x << ", " << pr.start.y <<
        " with size " << pr.numPixels.x << ", " << pr.numPixels.y;
}

TileDataReader::TileDataReader(const Configuration& config)
    : _config(config)
{}

std::shared_ptr<RawTile> TileDataReader::defaultTileData() {
    ensureInitialized();
    PixelRegion pixelRegion = {
        PixelRegion::PixelCoordinate(0, 0),
        PixelRegion::PixelRange(16, 16)
    };
    std::shared_ptr<RawTile> rawTile = std::make_shared<RawTile>();
    rawTile->tileIndex = { 0, 0, 0 };
    rawTile->dimensions = glm::uvec3(pixelRegion.numPixels, 1);
    rawTile->nBytesImageData =
        rawTile->dimensions.x * rawTile->dimensions.y * _dataLayout.bytesPerPixel;
    rawTile->imageData = new char[rawTile->nBytesImageData];
    for (size_t i = 0; i < rawTile->nBytesImageData; ++i) {
        rawTile->imageData[i] = 0;
    }
    rawTile->error = RawTile::ReadError::None;
        
    if (_config.doPreProcessing) {
        rawTile->tileMetaData = getTileMetaData(rawTile, pixelRegion);
    }

    return rawTile;
}

std::array<double, 6> TileDataset::getGeoTransform() const {
    std::array<double, 6> padfTransform;

    // Global coverage of the whole latlon space
    GeodeticPatch globalCoverage(Geodetic2(0,0), Geodetic2(M_PI / 2, M_PI));
    padfTransform[1] = Angle<double>::fromRadians(
        globalCoverage.size().lon).asDegrees() / rasterXSize();
    padfTransform[5] = -Angle<double>::fromRadians(
        globalCoverage.size().lat).asDegrees() / rasterYSize();
    padfTransform[0] = Angle<double>::fromRadians(
        globalCoverage.getCorner(Quad::NORTH_WEST).lon).asDegrees();
    padfTransform[3] = Angle<double>::fromRadians(
        globalCoverage.getCorner(Quad::NORTH_WEST).lat).asDegrees();
    padfTransform[2] = 0;
    padfTransform[4] = 0;

    return padfTransform;
}

PixelRegion::PixelCoordinate TileDataReader::geodeticToPixel(const Geodetic2& geo) const {
    std::array<double, 6> padfTransform = getGeoTransform();
        
    double Y = Angle<double>::fromRadians(geo.lat).asDegrees();
    double X = Angle<double>::fromRadians(geo.lon).asDegrees();

    // convert from pixel and line to geodetic coordinates
    // Xp = padfTransform[0] + P*padfTransform[1] + L*padfTransform[2];
    // Yp = padfTransform[3] + P*padfTransform[4] + L*padfTransform[5];

    // <=>
    double* a = &(padfTransform[0]);
    double* b = &(padfTransform[3]);

    // Xp = a[0] + P*a[1] + L*a[2];
    // Yp = b[0] + P*b[1] + L*b[2];

    // <=>
    double divisor = (a[2] * b[1] - a[1] * b[2]);
    ghoul_assert(divisor != 0.0, "Division by zero!");
    //ghoul_assert(a[2] != 0.0, "a2 must not be zero!");
    double P = (a[0] * b[2] - a[2] * b[0] + a[2] * Y - b[2] * X) / divisor;
    double L = (-a[0] * b[1] + a[1] * b[0] - a[1] * Y + b[1] * X) / divisor;
    // ref: https://www.wolframalpha.com/input/?i=X+%3D+a0+%2B+a1P+%2B+a2L,+Y+%3D+b0+%2B+b1P+%2B+b2L,+solve+for+P+and+L

    double Xp = a[0] + P*a[1] + L*a[2];
    double Yp = b[0] + P*b[1] + L*b[2];

    ghoul_assert(abs(X - Xp) < 1e-10, "inverse should yield X as before");
    ghoul_assert(abs(Y - Yp) < 1e-10, "inverse should yield Y as before");

    return PixelRegion::PixelCoordinate(glm::round(P), glm::round(L));
}

Geodetic2 TileDataReader::pixelToGeodetic(const PixelRegion::PixelCoordinate& p) const {
    std::array<double, 6> padfTransform = getGeoTransform();
    Geodetic2 geodetic;
    // Should be using radians and not degrees?
    geodetic.lon = padfTransform[0] + p.x * padfTransform[1] + p.y * padfTransform[2];
    geodetic.lat = padfTransform[3] + p.x * padfTransform[4] + p.y * padfTransform[5];
    return geodetic;
}

} // namespace globebrowsing
} // namespace openspace
