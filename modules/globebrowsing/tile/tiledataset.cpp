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

#include <modules/globebrowsing/tile/tiledataset.h>

#include <limits>

#include <ogr_featurestyle.h>
#include <ogr_spatialref.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/misc/assert.h>

#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>


#include <modules/globebrowsing/geometry/angle.h>

#include <float.h>
#include <sstream>
#include <algorithm>

#include <gdal_priv.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>

#include <memory>
#include <set>
#include <queue>
#include <iostream>
#include <unordered_map>

#include <ghoul/filesystem/file.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/misc/threadpool.h>

#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tiledatatype.h>
#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/pixelregion.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/tilemetadata.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>

namespace {
    const std::string _loggerCat = "TileDataset";
}

namespace openspace {
namespace globebrowsing {

std::ostream& operator<<(std::ostream& os, const PixelRegion& pr) {
    return os << pr.start.x << ", " << pr.start.y << " with size " << pr.numPixels.x << ", " << pr.numPixels.y;
}

TileDataLayout::TileDataLayout() {}

TileDataLayout::TileDataLayout(GDALDataset* dataSet, GLuint preferredGlType) {
    // Assume all raster bands have the same data type
    gdalType =preferredGlType != 0 ?
        tiledatatype::getGdalDataType(preferredGlType) :
        dataSet->GetRasterBand(1)->GetRasterDataType();

    glType = tiledatatype::getOpenGLDataType(gdalType);
    numRasters = dataSet->GetRasterCount();
    bytesPerDatum = tiledatatype::numberOfBytes(gdalType);
    bytesPerPixel = bytesPerDatum * numRasters;
    textureFormat = tiledatatype::getTextureFormat(numRasters, gdalType);
}
  
TileDataset::IODescription TileDataset::IODescription::cut(PixelRegion::Side side, int pos) {
    PixelRegion readPreCut = read.region;
    PixelRegion writePreCut = write.region;

    glm::dvec2 ratio;
    ratio.x = write.region.numPixels.x / (double) read.region.numPixels.x;
    ratio.y = write.region.numPixels.y / (double) read.region.numPixels.y;

    double ratioRatio = ratio.x / ratio.y;

    //ghoul_assert(glm::abs(ratioRatio - 1.0) < 0.01, "Different read/write aspect ratio!");

    IODescription whatCameOff = *this;
    whatCameOff.read.region = read.region.globalCut(side, pos);

    PixelRegion::PixelRange cutSize = whatCameOff.read.region.numPixels;
    PixelRegion::PixelRange localWriteCutSize = ratio * glm::dvec2(cutSize);
        
    if (cutSize.x == 0 || cutSize.y == 0) {
        ghoul_assert(
            read.region.equals(readPreCut),
            "Read region should not have been modified"
        );
        ghoul_assert(
            write.region.equals(writePreCut),
            "Write region should not have been modified"
        );
    }

    int localWriteCutPos = (side == PixelRegion::Side::LEFT || side == PixelRegion::Side::RIGHT)
        ? localWriteCutSize.x : localWriteCutSize.y;
    whatCameOff.write.region = write.region.localCut(side, localWriteCutPos);

    return whatCameOff;
}

const glm::ivec2 TileDataset::tilePixelStartOffset = glm::ivec2(-2);
const glm::ivec2 TileDataset::tilePixelSizeDifference = glm::ivec2(4);

const PixelRegion TileDataset::padding = PixelRegion(
    tilePixelStartOffset,
    tilePixelSizeDifference
);
    
bool TileDataset::GdalHasBeenInitialized = false;

TileDataset::TileDataset(const std::string& gdalDatasetDesc, const Configuration& config)
    : _config(config)
    , hasBeenInitialized(false)
{
    _initData = { "",  gdalDatasetDesc, config.minimumTilePixelSize, config.dataType };
    ensureInitialized();
    _initData.initDirectory = CPLGetCurrentDir();
}

void TileDataset::reset() {
    _cached._maxLevel = -1;
    if (_dataset != nullptr) {
        GDALClose((GDALDatasetH)_dataset);
    }
        
    initialize();
}

float TileDataset::noDataValueAsFloat() {
    float noDataValue;
    if (_dataset && _dataset->GetRasterBand(1)) {
        noDataValue = _dataset->GetRasterBand(1)->GetNoDataValue();;
    }
    else {
        noDataValue = std::numeric_limits<float>::min();
    }
    return noDataValue;
}

void TileDataset::ensureInitialized() {
    if (!hasBeenInitialized) {
        initialize();
        hasBeenInitialized = true;
    }

}

void TileDataset::initialize() {
    gdalEnsureInitialized();

    _dataset = gdalDataset(_initData.gdalDatasetDesc);

    //Do any other initialization needed for the TileDataset
    _dataLayout = TileDataLayout(_dataset, _initData.dataType);
    _depthTransform = calculateTileDepthTransform();
    _cached._tileLevelDifference = calculateTileLevelDifference(_initData.minimumPixelSize);
}

void TileDataset::gdalEnsureInitialized() {
    if (!GdalHasBeenInitialized) {
        GDALAllRegister();
        CPLSetConfigOption(
            "GDAL_DATA",
            absPath("${MODULE_GLOBEBROWSING}/gdal_data").c_str()
        );
        setGdalProxyConfiguration();
        GdalHasBeenInitialized = true;
    }
}

void TileDataset::setGdalProxyConfiguration() {
    ghoul::Dictionary proxySettings;
    bool proxyEnabled = OsEng.configurationManager().getValue(
        ConfigurationManager::KeyHttpProxy, proxySettings
    );
    if (proxyEnabled) {
        std::string proxyAddress, proxyPort, proxyUser, proxyPassword, proxyAuth;

        bool success = proxySettings.getValue(
            ConfigurationManager::PartHttpProxyAddress,
            proxyAddress
        );
        success &= proxySettings.getValue(
            ConfigurationManager::PartHttpProxyPort,
            proxyPort
        );
        proxySettings.getValue(
            ConfigurationManager::PartHttpProxyAuthentication,
            proxyAuth
        );

        std::string proxyAuthString = "BASIC";
        if (proxyAuth == "basic" || proxyAuth == "") {
            proxyAuthString = "BASIC";
        } else if (proxyAuth == "ntlm") {
            proxyAuthString = "NTLM";
        } else if (proxyAuth == "digest") {
            proxyAuthString = "DIGEST";
        } else if (proxyAuth == "any") {
            proxyAuthString = "ANY";
        } else {
            success = false;
        }

        bool userAndPassword = proxySettings.getValue(
            ConfigurationManager::PartHttpProxyUser,
            proxyUser
        );
        userAndPassword &= proxySettings.getValue(
            ConfigurationManager::PartHttpProxyPassword,
            proxyPassword
        );

        if (success) {
            std::string proxy = proxyAddress + ":" + proxyPort;
            CPLSetConfigOption("GDAL_HTTP_PROXY", proxy.c_str());
            LDEBUG("Using proxy server " << proxy);
            if (userAndPassword) {
                std::string proxyUserPwd = proxyUser + ":" + proxyPassword;
                CPLSetConfigOption("GDAL_HTTP_PROXYUSERPWD", proxyUserPwd.c_str());
                CPLSetConfigOption("GDAL_HTTP_PROXYAUTH", proxyAuthString.c_str());
                LDEBUG("Using authentication method: " << proxyAuthString);
            }
        } else {
            LERROR("Invalid proxy settings for GDAL");
        }
    } else {
        LDEBUG("Setting up GDAL without proxy server");
    }
}

GDALDataset* TileDataset::gdalDataset(const std::string& gdalDatasetDesc) {
    GDALDataset* dataset = (GDALDataset *)GDALOpen(gdalDatasetDesc.c_str(), GA_ReadOnly);
    if (!dataset) {
        using namespace ghoul::filesystem;
        std::string correctedPath = FileSystem::ref().pathByAppendingComponent(
            _initData.initDirectory, gdalDatasetDesc
        );
        dataset = (GDALDataset *)GDALOpen(correctedPath.c_str(), GA_ReadOnly);
        if (!dataset) {
            throw ghoul::RuntimeError("Failed to load dataset:\n" + gdalDatasetDesc);
        }
    }

    // Commenting away the following for now since it is not supported for older
    // versions of GDAL. Only used for debug info.
    /*
    const std::string originalDriverName = dataset->GetDriverName();
        
    if (originalDriverName != "WMS") {
        LDEBUG("  " << originalDriverName);
        LDEBUG("  " << dataset->GetGCPProjection());
        LDEBUG("  " << dataset->GetProjectionRef());

        GDALDriver* driver = dataset->GetDriver();
        char** metadata = driver->GetMetadata();
        for (int i = 0; metadata[i] != nullptr; i++) {
            LDEBUG("  " << metadata[i]);
        }

        const char* in_memory = "";
        //GDALDataset* vrtDataset = driver->CreateCopy(in_memory, dataset, false, nullptr, nullptr, nullptr);
    }
    */

    return dataset;
}

TileDataset::~TileDataset() {
    delete _dataset;
}

std::shared_ptr<RawTile> TileDataset::readTileData(TileIndex tileIndex) {
    ensureInitialized();
    IODescription io = getIODescription(tileIndex);
    CPLErr worstError = CPLErr::CE_None;

    // Build the RawTile from the data we querred
    std::shared_ptr<RawTile> rawTile = std::make_shared<RawTile>();
    rawTile->imageData = readImageData(io, worstError);
    rawTile->error = worstError;
    rawTile->tileIndex = tileIndex;
    rawTile->dimensions = glm::uvec3(io.write.region.numPixels, 1);
    rawTile->nBytesImageData = io.write.totalNumBytes;
        
    if (_config.doPreProcessing) {
        rawTile->tileMetaData = getTileMetaData(rawTile, io.write.region);
        rawTile->error = std::max(rawTile->error, postProcessErrorCheck(rawTile, io));
    }

    return rawTile;
}

std::shared_ptr<RawTile> TileDataset::defaultTileData() {
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
    rawTile->error = CPLErr::CE_None;
        
    if (_config.doPreProcessing) {
        rawTile->tileMetaData = getTileMetaData(rawTile, pixelRegion);
        //rawTile->error = std::max(rawTile->error, postProcessErrorCheck(rawTile, io));
    }

    return rawTile;
}

int TileDataset::maxChunkLevel() {
    ensureInitialized();
    if (_cached._maxLevel < 0) {
        int numOverviews = _dataset->GetRasterBand(1)->GetOverviewCount();
        _cached._maxLevel = -_cached._tileLevelDifference;
        if (numOverviews > 0) {
            _cached._maxLevel += numOverviews - 1;
        }
    }
    return _cached._maxLevel;
}

TileDepthTransform TileDataset::getDepthTransform() {
    ensureInitialized();
    return _depthTransform;
}

const TileDataLayout& TileDataset::getDataLayout() {
    ensureInitialized();
    return _dataLayout;
}

int TileDataset::calculateTileLevelDifference(int minimumPixelSize) {
    GDALRasterBand* firstBand = _dataset->GetRasterBand(1);
    GDALRasterBand* maxOverview;
    int numOverviews = firstBand->GetOverviewCount();
    int sizeLevel0;
    if (numOverviews <= 0) { // No overviews. Use first band.
        maxOverview = firstBand;
    }
    else { // Pick the highest overview.
        maxOverview = firstBand->GetOverview(numOverviews - 1);
    }
    sizeLevel0 = maxOverview->GetXSize();
    double diff = log2(minimumPixelSize) - log2(sizeLevel0);
    return diff;
}

TileDepthTransform TileDataset::calculateTileDepthTransform() {
    GDALRasterBand* firstBand = _dataset->GetRasterBand(1);
        
    bool isFloat =
        (_dataLayout.gdalType == GDT_Float32 || _dataLayout.gdalType == GDT_Float64);
    double maximumValue =
        isFloat ? 1.0 : tiledatatype::getMaximumValue(_dataLayout.gdalType);

    TileDepthTransform transform;
    transform.depthOffset = firstBand->GetOffset();
    transform.depthScale = firstBand->GetScale() * maximumValue;
    return transform;
}

bool TileDataset::gdalHasOverviews() const {
    return _dataset->GetRasterBand(1)->GetOverviewCount() > 0;
}

int TileDataset::gdalOverview(const PixelRegion::PixelRange& regionSizeOverviewZero) const {
    GDALRasterBand* firstBand = _dataset->GetRasterBand(1);

    int minNumPixels0 = glm::min(regionSizeOverviewZero.x, regionSizeOverviewZero.y);

    int overviews = firstBand->GetOverviewCount();
    GDALRasterBand* maxOverview =
        overviews ? firstBand->GetOverview(overviews - 1) : firstBand;
        
    int sizeLevel0 = maxOverview->GetXSize();
    // The dataset itself may not have overviews but even if it does not, an overview
    // for the data region can be calculated and possibly be used to sample greater
    // Regions of the original dataset.
    int ov = std::log2(minNumPixels0) - std::log2(sizeLevel0 + 1) -
        _cached._tileLevelDifference;
    ov = glm::clamp(ov, 0, overviews - 1);
        
    return ov;
}

int TileDataset::gdalOverview(const TileIndex& tileIndex) const {
    int overviews = _dataset->GetRasterBand(1)->GetOverviewCount();
    int ov = overviews - (tileIndex.level + _cached._tileLevelDifference + 1);
    return glm::clamp(ov, 0, overviews - 1);
}

int TileDataset::gdalVirtualOverview(const TileIndex& tileIndex) const {
    int overviews = _dataset->GetRasterBand(1)->GetOverviewCount();
    int ov = overviews - (tileIndex.level + _cached._tileLevelDifference + 1);
    return ov;
}

PixelRegion TileDataset::gdalPixelRegion(GDALRasterBand* rasterBand) const {
    PixelRegion gdalRegion;
    gdalRegion.start.x = 0;
    gdalRegion.start.y = 0;
    gdalRegion.numPixels.x = rasterBand->GetXSize();
    gdalRegion.numPixels.y = rasterBand->GetYSize();
    return gdalRegion;
}

PixelRegion TileDataset::gdalPixelRegion(const GeodeticPatch& geodeticPatch) const {
    Geodetic2 nwCorner = geodeticPatch.getCorner(Quad::NORTH_WEST);
    Geodetic2 swCorner = geodeticPatch.getCorner(Quad::SOUTH_EAST);
    PixelRegion::PixelCoordinate pixelStart = geodeticToPixel(nwCorner);
    PixelRegion::PixelCoordinate pixelEnd = geodeticToPixel(swCorner);
    PixelRegion gdalRegion(pixelStart, pixelEnd - pixelStart);
    return gdalRegion;
}

GDALRasterBand* TileDataset::gdalRasterBand(int overview, int raster) const {
    GDALRasterBand* rasterBand = _dataset->GetRasterBand(raster);
    int numberOfOverviews = rasterBand->GetOverviewCount();
    rasterBand = gdalHasOverviews() ? rasterBand->GetOverview(overview) : rasterBand;
    ghoul_assert(rasterBand != nullptr, "Rasterband is null");
    return rasterBand;
}

std::array<double, 6> TileDataset::getGeoTransform() const {
    std::array<double, 6> padfTransform;
    CPLErr err = _dataset->GetGeoTransform(&padfTransform[0]);
    if (err == CE_Failure) {
        GeodeticPatch globalCoverage(Geodetic2(0,0), Geodetic2(M_PI / 2, M_PI));
        padfTransform[1] = Angle<double>::fromRadians(
            globalCoverage.size().lon).asDegrees() / _dataset->GetRasterXSize();
        padfTransform[5] = -Angle<double>::fromRadians(
            globalCoverage.size().lat).asDegrees() / _dataset->GetRasterYSize();
        padfTransform[0] = Angle<double>::fromRadians(
            globalCoverage.getCorner(Quad::NORTH_WEST).lon).asDegrees();
        padfTransform[3] = Angle<double>::fromRadians(
            globalCoverage.getCorner(Quad::NORTH_WEST).lat).asDegrees();
        padfTransform[2] = 0;
        padfTransform[4] = 0;
    }
    return padfTransform;
}

PixelRegion::PixelCoordinate TileDataset::geodeticToPixel(const Geodetic2& geo) const {
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

Geodetic2 TileDataset::pixelToGeodetic(const PixelRegion::PixelCoordinate& p) const {
    std::array<double, 6> padfTransform = getGeoTransform();
    Geodetic2 geodetic;
    // Should be using radians and not degrees?
    geodetic.lon = padfTransform[0] + p.x * padfTransform[1] + p.y * padfTransform[2];
    geodetic.lat = padfTransform[3] + p.x * padfTransform[4] + p.y * padfTransform[5];
    return geodetic;
}

TileDataset::IODescription TileDataset::getIODescription(const TileIndex& tileIndex) const {
    IODescription io;
    io.read.region = gdalPixelRegion(tileIndex);

    if (gdalHasOverviews()) {
        int overview = gdalOverview(tileIndex);
        io.read.overview = overview;
        io.read.region.downscalePow2(overview + 1);
        io.write.region = io.read.region;
        io.read.region.pad(padding);
    }
    else {
        io.read.overview = 0;
        io.write.region = io.read.region;
        int virtualOverview = gdalVirtualOverview(tileIndex);
        io.write.region.downscalePow2(virtualOverview + 1);
        PixelRegion scaledPadding = padding;

        scaledPadding.upscalePow2(std::max(virtualOverview + 1, 0));
        io.read.region.pad(scaledPadding);
    }

    // For correct sampling in height dataset, we need to pad the texture tile
    io.write.region.pad(padding);
    PixelRegion::PixelRange preRound = io.write.region.numPixels;
    io.write.region.roundDownToQuadratic();
    io.write.region.roundUpNumPixelToNearestMultipleOf(2);
    if (preRound != io.write.region.numPixels) {
        LDEBUG(tileIndex << " | " << preRound.x << ", " << preRound.y << " --> " << io.write.region.numPixels.x << ", " << io.write.region.numPixels.y);
    }

    io.write.region.start = PixelRegion::PixelCoordinate(0, 0); // write region starts in origin
    io.write.bytesPerLine = _dataLayout.bytesPerPixel * io.write.region.numPixels.x;
    io.write.totalNumBytes = io.write.bytesPerLine * io.write.region.numPixels.y;

    return io;
}

char* TileDataset::readImageData(IODescription& io, CPLErr& worstError) const {
    // allocate memory for the image
    char* imageData = new char[io.write.totalNumBytes];

    // Read the data (each rasterband is a separate channel)
    for (size_t i = 0; i < _dataLayout.numRasters; i++) {
        GDALRasterBand* rasterBand = gdalRasterBand(io.read.overview, i + 1);

        // The final destination pointer is offsetted by one datum byte size
        // for every raster (or data channel, i.e. R in RGB)
        char* dataDestination = imageData + (i * _dataLayout.bytesPerDatum);

        CPLErr err = repeatedRasterIO(rasterBand, io, dataDestination);

        // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
        worstError = std::max(worstError, err);
    }

    // GDAL reads pixel lines top to bottom, we want the opposit
    return imageData;
}

CPLErr TileDataset::repeatedRasterIO(GDALRasterBand* rasterBand, const IODescription& fullIO, char* dataDestination, int depth) const {
    std::string spaces = "                      ";
    std::string indentation = spaces.substr(0, 2 * depth);

    CPLErr worstError = CPLErr::CE_None;

    // NOTE: 
    // Ascii graphics illustrates the implementation details of this method, for one  
    // specific case. Even though the illustrated case is specific, readers can 
    // hopefully find it useful to get the general idea.

    // Make a copy of the full IO desription as we will have to modify it
    IODescription io = fullIO;
    PixelRegion gdalRegion = gdalPixelRegion(rasterBand);


    // Example: 
    // We have an io description that defines a WRITE and a READ region.
    // In this case the READ region extends outside of the defined gdal region,
    // meaning we will have to do wrapping


    // io.write.region             io.read.region
    //    |                         |
    //    V                         V
    // +-------+                +-------+ 
    // |       |                |       |--------+ 
    // |       |                |       |        |
    // |       |                |       |        |
    // +-------+                +-------+        |
    //                            |              | <-- gdalRegion  
    //                            |              |
    //                            +--------------+


    //LDEBUG(indentation << "-");
    //LDEBUG(indentation << "repeated read: " << io.read.region);
    //LDEBUG(indentation << "repeated write: " << io.write.region);

    bool didCutOff = false;

    if (!io.read.region.isInside(gdalRegion)) {
        //  Loop through each side: left, top, right, bottom
        for (int i = 0; i < 4; ++i) {

            // Example: 
            // We are currently considering the left side of the pixel region
            PixelRegion::Side side = (PixelRegion::Side) i;
            IODescription cutoff = io.cut(side, gdalRegion.edge(side));

            // Example: 
            // We cut off the left part that was outside the gdal region, and we now 
            // have an additional io description for the cut off region. 
            // Note that the cut-method used above takes care of the corresponding 
            // WRITE region for us.

            // cutoff.write.region    cutoff.read.region
            //  |  io.write.region     |  io.read.region
            //  |   |                  |   |
            //  V   V                  V   V
            // +-+-----+               +-+-----+ 
            // | |     |               | |     |--------+
            // | |     |               | |     |        |
            // | |     |               | |     |        |
            // +-+-----+               +-+-----+        |
            //                           |              | <-- gdalRegion  
            //                           |              |
            //                           +--------------+

            if (cutoff.read.region.area() > 0) {
                didCutOff = true;

                // Wrap by repeating
                PixelRegion::Side oppositeSide = (PixelRegion::Side) ((i + 2) % 4);

                cutoff.read.region.align(oppositeSide, gdalRegion.edge(oppositeSide));

                // Example:
                // The cut off region is wrapped to the opposite side of the region,
                // i.e. "repeated". Note that we don't want WRITE region to change, 
                // we're only wrapping the READ region.

                // cutoff.write.region   io.read.region cutoff.read.region
                //  |  io.write.region        |          |
                //  |   |                     V          V
                //  V   V                  +-----+      +-+
                // +-+-----+               |     |------| |
                // | |     |               |     |      | | 
                // | |     |               |     |      | |
                // | |     |               +-----+      +-+
                // +-+-----+               |              | <-- gdalRegion  
                //                         |              |
                //                         +--------------+

                // Example:
                // The cutoff region has been repeated along one of its sides, but 
                // as we can see in this example, it still has a top part outside the
                // defined gdal region. This is handled through recursion.
                CPLErr err = repeatedRasterIO(rasterBand, cutoff, dataDestination, depth + 1);

                worstError = std::max(worstError, err);
            }
        }
    }
    else if (worstError > CPLErr::CE_None) {
        LDEBUG(indentation << "Error reading padding: " << worstError);
    }
        
    CPLErr err = rasterIO(rasterBand, io, dataDestination);
    worstError = std::max(worstError, err);

    // The return error from a repeated rasterIO is ONLY based on the main region,
    // which in the usual case will cover the main area of the patch anyway

    return err;
}

CPLErr TileDataset::rasterIO(GDALRasterBand* rasterBand, const IODescription& io,
                             char* dataDestination) const
{
    PixelRegion gdalRegion = gdalPixelRegion(rasterBand);

    ghoul_assert(io.read.region.isInside(gdalRegion), "write region of bounds!");
    ghoul_assert(
        io.write.region.start.x >= 0 && io.write.region.start.y >= 0,
        "Invalid write region"
    );

    PixelRegion::PixelCoordinate end = io.write.region.end();
    size_t largestIndex =
        (end.y - 1) * io.write.bytesPerLine + (end.x - 1) * _dataLayout.bytesPerPixel;
    ghoul_assert(largestIndex <= io.write.totalNumBytes, "Invalid write region");

    char* dataDest = dataDestination;

    // OBS! GDAL reads pixels top to bottom, but we want our pixels bottom to top.
    // Therefore, we increment the destination pointer to the last line on in the 
    // buffer, and the we specify in the rasterIO call that we want negative line 
    // spacing. Doing this compensates the flipped Y axis
    dataDest += (io.write.totalNumBytes - io.write.bytesPerLine);

    // handle requested write region
    dataDest -= io.write.region.start.y * io.write.bytesPerLine; // note -= since flipped y axis
    dataDest += io.write.region.start.x * _dataLayout.bytesPerPixel;
        
    return rasterBand->RasterIO(
        GF_Read,
        io.read.region.start.x,             // Begin read x
        io.read.region.start.y,             // Begin read y
        io.read.region.numPixels.x,         // width to read x
        io.read.region.numPixels.y,         // width to read y
        dataDest,                           // Where to put data
        io.write.region.numPixels.x,        // width to write x in destination
        io.write.region.numPixels.y,        // width to write y in destination
        _dataLayout.gdalType,               // Type
        _dataLayout.bytesPerPixel,          // Pixel spacing
        -io.write.bytesPerLine              // Line spacing
    );
}

std::shared_ptr<TileMetaData> TileDataset::getTileMetaData(
                                                         std::shared_ptr<RawTile> rawTile,
                                                         const PixelRegion& region) const
{
    size_t bytesPerLine = _dataLayout.bytesPerPixel * region.numPixels.x;
    size_t totalNumBytes = bytesPerLine * region.numPixels.y;

    TileMetaData* preprocessData = new TileMetaData();
    preprocessData->maxValues.resize(_dataLayout.numRasters);
    preprocessData->minValues.resize(_dataLayout.numRasters);
    preprocessData->hasMissingData.resize(_dataLayout.numRasters);
        
    std::vector<float> noDataValues;
    noDataValues.resize(_dataLayout.numRasters);

    for (size_t c = 0; c < _dataLayout.numRasters; c++) {
        preprocessData->maxValues[c] = -FLT_MAX;
        preprocessData->minValues[c] = FLT_MAX;
        preprocessData->hasMissingData[c] = false;
        noDataValues[c] = _dataset->GetRasterBand(1)->GetNoDataValue();
    }

    for (size_t y = 0; y < region.numPixels.y; y++) {
        size_t yi = (region.numPixels.y - 1 - y) * bytesPerLine;
        size_t i = 0;
        for (size_t x = 0; x < region.numPixels.x; x++) {
            for (size_t c = 0; c < _dataLayout.numRasters; c++) {
                float noDataValue = _dataset->GetRasterBand(c + 1)->GetNoDataValue();
                float val = tiledatatype::interpretFloat(
                    _dataLayout.gdalType,
                    &(rawTile->imageData[yi + i])
                );
                if (val != noDataValue) {
                    preprocessData->maxValues[c] = std::max(
                        val,
                        preprocessData->maxValues[c]
                    );
                    preprocessData->minValues[c] = std::min(
                        val,
                        preprocessData->minValues[c]
                    );
                }
                else {
                    preprocessData->hasMissingData[c] = true;
                }
                i += _dataLayout.bytesPerDatum;
            }
        }
    }

    for (size_t c = 0; c < _dataLayout.numRasters; c++) {
        if (preprocessData->maxValues[c] > 8800.0f) {
            //LDEBUG("Bad preprocess data: " << preprocessData->maxValues[c] << " at " << region.tileIndex);
        }
    }

    return std::shared_ptr<TileMetaData>(preprocessData);
}

CPLErr TileDataset::postProcessErrorCheck(std::shared_ptr<const RawTile> rawTile,
                                          const IODescription& io) const
{
    int success;

    double missingDataValue = gdalRasterBand(io.read.overview)->GetNoDataValue(&success);
    if (!success) {
        // missing data value for TERRAIN.wms. Should be specified in XML
        missingDataValue = 32767; 
    }

    bool hasMissingData = false;
        
    for (size_t c = 0; c < _dataLayout.numRasters; c++) {
        hasMissingData |= rawTile->tileMetaData->maxValues[c] == missingDataValue;
    }
        
    bool onHighLevel = rawTile->tileIndex.level > 6;
    if (hasMissingData && onHighLevel) {
        return CE_Fatal;
    }
    // ugly test for heightmap overlay
    if (_dataLayout.textureFormat.ghoulFormat == ghoul::opengl::Texture::Format::RG) {
        // check the alpha
        if (rawTile->tileMetaData->maxValues[1] == 0.0
            && rawTile->tileMetaData->minValues[1] == 0.0)
        {
            //return CE_Warning;
        }
    }
    return CE_None;
}

} // namespace globebrowsing
} // namespace openspace
