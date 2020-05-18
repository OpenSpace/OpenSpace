/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/globebrowsing/src/rawtiledatareader.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/geodeticpatch.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>

#ifdef _MSC_VER
#pragma warning (push)
 // CPL throws warning about missing DLL interface
#pragma warning (disable : 4251)
#endif // _MSC_VER

#include <ogr_featurestyle.h>
#include <ogr_spatialref.h>
#include <cpl_virtualmem.h>

#include <gdal_priv.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#include <algorithm>
#include <fstream>

namespace openspace::globebrowsing {

namespace {

// These are some locations in memory taken from ESRI's No Data Available tile so that we
// can spotcheck these tiles and not present them
// The pair is <byte index, expected value>
struct MemoryLocation {
    int offset;
    std::byte value;
};

// The memory locations are grouped to be mostly cache-aligned
constexpr std::array<MemoryLocation, 42> NoDataAvailableData = {
    MemoryLocation{ 296380, std::byte(205) },
    MemoryLocation{ 296381, std::byte(205) },
    MemoryLocation{ 296382, std::byte(205) },
    MemoryLocation{ 296383, std::byte(255) },
    MemoryLocation{ 296384, std::byte(224) },
    MemoryLocation{ 296385, std::byte(224) },
    MemoryLocation{ 296386, std::byte(224) },
    MemoryLocation{ 296387, std::byte(255) },
    MemoryLocation{ 296388, std::byte(244) },
    MemoryLocation{ 296389, std::byte(244) },
    MemoryLocation{ 296390, std::byte(244) },
    MemoryLocation{ 296391, std::byte(255) },

    MemoryLocation{ 269840, std::byte(209) },
    MemoryLocation{ 269841, std::byte(209) },
    MemoryLocation{ 269842, std::byte(209) },
    MemoryLocation{ 269844, std::byte(203) },
    MemoryLocation{ 269845, std::byte(203) },
    MemoryLocation{ 269846, std::byte(203) },
    MemoryLocation{ 269852, std::byte(221) },
    MemoryLocation{ 269853, std::byte(221) },
    MemoryLocation{ 269854, std::byte(221) },
    MemoryLocation{ 269856, std::byte(225) },
    MemoryLocation{ 269857, std::byte(225) },
    MemoryLocation{ 269858, std::byte(225) },
    MemoryLocation{ 269860, std::byte(218) },
    MemoryLocation{ 269861, std::byte(218) },

    MemoryLocation{ 240349, std::byte(203) },
    MemoryLocation{ 240350, std::byte(203) },
    MemoryLocation{ 240352, std::byte(205) },
    MemoryLocation{ 240353, std::byte(204) },
    MemoryLocation{ 240354, std::byte(205) },

    MemoryLocation{ 0, std::byte(204) },
    MemoryLocation{ 7, std::byte(255) },
    MemoryLocation{ 520, std::byte(204) },
    MemoryLocation{ 880, std::byte(204) },
    MemoryLocation{ 883, std::byte(255) },
    MemoryLocation{ 91686, std::byte(204) },
    MemoryLocation{ 372486, std::byte(204) },
    MemoryLocation{ 670483, std::byte(255) },
    MemoryLocation{ 231684, std::byte(202) },
    MemoryLocation{ 232092, std::byte(202) },
    MemoryLocation{ 235921, std::byte(203) },
};

enum class Side {
    Left = 0,
    Top,
    Right,
    Bottom
};

float interpretFloat(GLenum glType, const std::byte* src) {
    switch (glType) {
        case GL_UNSIGNED_BYTE:
            return static_cast<float>(*reinterpret_cast<const GLubyte*>(src));
        case GL_UNSIGNED_SHORT:
            return static_cast<float>(*reinterpret_cast<const GLushort*>(src));
        case GL_SHORT:
            return static_cast<float>(*reinterpret_cast<const GLshort*>(src));
        case GL_UNSIGNED_INT:
            return static_cast<float>(*reinterpret_cast<const GLuint*>(src));
        case GL_INT:
            return static_cast<float>(*reinterpret_cast<const GLint*>(src));
        case GL_HALF_FLOAT:
            return static_cast<float>(*reinterpret_cast<const GLhalf*>(src));
        case GL_FLOAT:
            return static_cast<float>(*reinterpret_cast<const GLfloat*>(src));
        case GL_DOUBLE:
            return static_cast<float>(*reinterpret_cast<const GLdouble*>(src));
        default:
            ghoul_assert(false, "Unknown data type");
            throw ghoul::MissingCaseException();
    }
}

GDALDataType toGDALDataType(GLenum glType) {
    switch (glType) {
        case GL_UNSIGNED_BYTE:
            return GDT_Byte;
        case GL_UNSIGNED_SHORT:
            return GDT_UInt16;
        case GL_SHORT:
            return GDT_Int16;
        case GL_UNSIGNED_INT:
            return GDT_UInt32;
        case GL_INT:
            return GDT_Int32;
        case GL_FLOAT:
            return GDT_Float32;
        case GL_DOUBLE:
            return GDT_Float64;
        default:
            LERRORC("GDALRawTileDataReader", fmt::format(
                "OpenGL data type unknown to GDAL: {}", static_cast<int>(glType)
            ));
            throw ghoul::MissingCaseException();
    }
}

/**
 * Use as a helper function when determining the maximum tile level. This function
 * returns the negated number of overviews requred to downscale the highest overview
 * dataset so that it fits within minimumPixelSize pixels in the x-dimension.
 */
int calculateTileLevelDifference(GDALDataset* dataset, int minimumPixelSize) {
    GDALRasterBand* firstBand = dataset->GetRasterBand(1);
    GDALRasterBand* maxOverview;
    int numOverviews = firstBand->GetOverviewCount();
    if (numOverviews <= 0) { // No overviews. Use first band.
        maxOverview = firstBand;
    }
    else { // Pick the highest overview.
        maxOverview = firstBand->GetOverview(numOverviews - 1);
    }
    const int sizeLevel0 = maxOverview->GetXSize();
    const double diff = log2(minimumPixelSize) - log2(sizeLevel0);
    return static_cast<int>(diff);
}

/**
 * Aligns one the sides of the pixel regino to the specified position. This does
 * not change the number of pixels within the region.
 *
 * Example: Side = left and pos = 16:
 *                 start.x = 16 and keep the size the same
 */
void alignPixelRegion(PixelRegion& pr, Side side, int pos) {
    switch (side) {
        case Side::Left:
            pr.start.x = pos;
            break;
        case Side::Top:
            pr.start.y = pos;
            break;
        case Side::Right:
            pr.start.x = pos - pr.numPixels.x;
            break;
        case Side::Bottom:
            pr.start.y = pos - pr.numPixels.y;
            break;
    }
}

PixelRegion globalCut(PixelRegion& pr, Side side, int p) {
    const bool lineIntersect = [pr, side, p]() {
        switch (side) {
            case Side::Left:
            case Side::Right:
                return pr.start.x <= p && p <= (pr.start.x + pr.numPixels.x);
            case Side::Top:
            case Side::Bottom:
                return pr.start.y <= p && p <= (pr.start.y + pr.numPixels.y);
            default:
                throw ghoul::MissingCaseException();
        }
    }();

    if (!lineIntersect) {
        return PixelRegion();
    }

    auto setSide = [](PixelRegion& pr, Side side, int pos) {
        switch (side) {
            case Side::Left:
                pr.numPixels.x += (pr.start.x - pos);
                pr.start.x = pos;
                break;
            case Side::Top:
                pr.numPixels.y += (pr.start.y - pos);
                pr.start.y = pos;
                break;
            case Side::Right:
                pr.numPixels.x = pos - pr.start.x;
                break;
            case Side::Bottom:
                pr.numPixels.y = pos - pr.start.y;
                break;
        }
    };

    PixelRegion cutOff(pr);
    int cutSize = 0;
    switch (side) {
        case Side::Left:
            setSide(pr, Side::Left, p);
            setSide(cutOff, Side::Right, p - cutSize);
            break;
        case Side::Top:
            setSide(pr, Side::Top, p);
            setSide(cutOff, Side::Bottom, p - cutSize);
            break;
        case Side::Right:
            setSide(pr, Side::Right, p);
            setSide(cutOff, Side::Left, p + cutSize);
            break;
        case Side::Bottom:
            setSide(pr, Side::Bottom, p);
            setSide(cutOff, Side::Top, p + cutSize);
            break;
    }
    return cutOff;
}

int edge(const PixelRegion& pr, Side side) {
    switch (side) {
        case Side::Left:   return pr.start.x;
        case Side::Top:    return pr.start.y;
        case Side::Right:  return pr.start.x + pr.numPixels.x;
        case Side::Bottom: return pr.start.y + pr.numPixels.y;
        default:           throw ghoul::MissingCaseException();
    }
}

PixelRegion localCut(PixelRegion& pr, Side side, int localPos) {
    if (localPos < 1) {
        return PixelRegion();
    }
    else {
        const int edgeDirectionSign = (side < Side::Right) ? -1 : 1;
        return globalCut(pr, side, edge(pr, side) - edgeDirectionSign * localPos);
    }
}

bool isInside(const PixelRegion& lhs, const PixelRegion& rhs) {
    glm::ivec2 e = lhs.start + lhs.numPixels;
    glm::ivec2 re = rhs.start + rhs.numPixels;
    return rhs.start.x <= lhs.start.x && e.x <= re.x &&
           rhs.start.y <= lhs.start.y && e.y <= re.y;
}

IODescription cutIODescription(IODescription& io, Side side, int pos) {
    glm::dvec2 ratio = {
        io.write.region.numPixels.x / static_cast<double>(io.read.region.numPixels.x),
        io.write.region.numPixels.y / static_cast<double>(io.read.region.numPixels.y)
    };

    IODescription whatCameOff = io;
    whatCameOff.read.region = globalCut(io.read.region, side, pos);

    glm::ivec2 cutSize = whatCameOff.read.region.numPixels;
    glm::ivec2 localWriteCutSize = ratio * glm::dvec2(cutSize);

    int localWriteCutPos =
        (side == Side::Left || side == Side::Right) ?
        localWriteCutSize.x :
        localWriteCutSize.y;

    whatCameOff.write.region = localCut(io.write.region, side, localWriteCutPos);

    return whatCameOff;
}

/**
 * Returns the geo transform from raster space to projection coordinates as defined
 * by GDAL.
 */
std::array<double, 6> geoTransform(int rasterX, int rasterY) {
    GeodeticPatch cov(
        Geodetic2{ 0.0, 0.0 },
        Geodetic2{ glm::half_pi<double>(), glm::pi<double>() }
    );
    return {
        glm::degrees(cov.corner(Quad::NORTH_WEST).lon),
        glm::degrees(cov.size().lon) / rasterX,
        0.0,
        glm::degrees(cov.corner(Quad::NORTH_WEST).lat),
        0.0,
        glm::degrees(-cov.size().lat) / rasterY
    };
}

/**
 * Get the pixel corresponding to a specific position on the globe defined by the
 * Geodetic2 coordinate \p geo. If the dataset has overviews the function returns the
 * pixel at the lowest overview (highest resolution).
 *
 * \param geo The position on the globe to convert to pixel space.
 * \return a pixel coordinate in the dataset.
 */
glm::ivec2 geodeticToPixel(const Geodetic2& geo,
                           const std::array<double, 6>& transform)
{
    const std::array<double, 6>& t = transform;

    const double Y = glm::degrees(geo.lat);
    const double X = glm::degrees(geo.lon);

    const double divisor = t[2] * t[4] - t[1] * t[5];
    ghoul_assert(divisor != 0.0, "Division by zero!");

    const double P = (t[0] * t[5] - t[2] * t[3] + t[2] * Y - t[5] * X) / divisor;
    const double L = (-t[0] * t[4] + t[1] * t[3] - t[1] * Y + t[4] * X) / divisor;
    // ref: https://www.wolframalpha.com/input/?i=X+%3D+a0+%2B+a1P+%2B+a2L,
    //      +Y+%3D+b0+%2B+b1P+%2B+b2L,+solve+for+P+and+L

    [[maybe_unused]] const double Xp = t[0] + P * t[1] + L * t[2];
    [[maybe_unused]] const double Yp = t[3] + P * t[4] + L * t[5];
    ghoul_assert(std::abs(X - Xp) < 1e-10, "inverse should yield X as before");
    ghoul_assert(std::abs(Y - Yp) < 1e-10, "inverse should yield Y as before");

    return glm::ivec2(glm::round(P), glm::round(L));
}

/**
 * Get a pixel region corresponding to the given GeodeticPatch. If the dataset has
 * overviews the function returns the pixel region at the lowest overview (highest
 * resolution).
 *
 * \param \p geodeticPatch is a patch covering an area in geodetic coordinates
 * \return A PixelRegion covering the given geodetic patch at highest resolution.
 */
PixelRegion highestResPixelRegion(const GeodeticPatch& geodeticPatch,
                                  const std::array<double, 6>& transform)
{
    const Geodetic2 nwCorner = geodeticPatch.corner(Quad::NORTH_WEST);
    const Geodetic2 swCorner = geodeticPatch.corner(Quad::SOUTH_EAST);
    const glm::ivec2 pixelStart = geodeticToPixel(nwCorner, transform);
    const glm::ivec2 pixelEnd = geodeticToPixel(swCorner, transform);
    PixelRegion region;
    region.start = pixelStart;
    region.numPixels = pixelEnd - pixelStart;
    return region;
}

RawTile::ReadError postProcessErrorCheck(const RawTile& rawTile,
                                         [[ maybe_unused ]] size_t nRasters,
                                         float noDataValue)
{
    // This check was implicit before and just made explicit here
    ghoul_assert(
        nRasters == rawTile.tileMetaData.maxValues.size(),
        "Wrong numbers of max values"
    );

    const bool hasMissingData = std::any_of(
        rawTile.tileMetaData.maxValues.begin(),
        rawTile.tileMetaData.maxValues.end(),
        [noDataValue](float v) { return v == noDataValue; }
    );

    const bool onHighLevel = rawTile.tileIndex.level > 6;
    if (hasMissingData && onHighLevel) {
        return RawTile::ReadError::Fatal;
    }
    return RawTile::ReadError::None;
}

} // namespace


RawTileDataReader::RawTileDataReader(std::string filePath,
                                     TileTextureInitData initData,
                                     PerformPreprocessing preprocess)
    : _datasetFilePath(std::move(filePath))
    , _initData(std::move(initData))
    , _preprocess(preprocess)
{
    initialize();
}

RawTileDataReader::~RawTileDataReader() {
    std::lock_guard lockGuard(_datasetLock);
    if (_dataset) {
        GDALClose(_dataset);
        _dataset = nullptr;
    }
}

void RawTileDataReader::initialize() {
    if (_datasetFilePath.empty()) {
        throw ghoul::RuntimeError("File path must not be empty");
    }

    GlobeBrowsingModule& module = *global::moduleEngine.module<GlobeBrowsingModule>();

    std::string content = _datasetFilePath;
    if (module.isWMSCachingEnabled()) {
        std::string c;
        if (FileSys.fileExists(_datasetFilePath)) {
            // Only replace the 'content' if the dataset is an XML file and we want to do
            // caching
            std::ifstream t(_datasetFilePath);
            c.append(
                (std::istreambuf_iterator<char>(t)),
                std::istreambuf_iterator<char>()
            );
        }
        else {
            //GDAL input case for configuration string (e.g. temporal data)
            c = _datasetFilePath;
        }

        if (c.size() > 10 && c.substr(0, 10) == "<GDAL_WMS>") {
            // We know that _datasetFilePath is an XML file, so now we add a Cache line
            // into it iff there isn't already one in the XML and if the configuration
            // says we should

            // 1. Parse XML
            // 2. Inject Cache tag if it isn't already there
            // 3. Serialize XML to pass into GDAL

            LDEBUGC(_datasetFilePath, "Inserting caching tag");

            bool shouldSerializeXml = false;

            CPLXMLNode* root = CPLParseXMLString(c.c_str());
            CPLXMLNode* cache = CPLSearchXMLNode(root, "Cache");
            if (!cache) {
                // If there already is a cache, we don't want to modify it
                cache = CPLCreateXMLNode(root, CXT_Element, "Cache");

                CPLCreateXMLElementAndValue(
                    cache,
                    "Path",
                    absPath(module.wmsCacheLocation()).c_str()
                );
                CPLCreateXMLElementAndValue(cache, "Depth", "4");
                CPLCreateXMLElementAndValue(cache, "Expires", "315576000"); // 10 years
                CPLCreateXMLElementAndValue(
                    cache,
                    "MaxSize",
                    std::to_string(module.wmsCacheSize()).c_str()
                );

                // The serialization only needs to be one if the cache didn't exist
                // already
                shouldSerializeXml = true;
            }

            if (module.isInOfflineMode()) {
                CPLXMLNode* offlineMode = CPLSearchXMLNode(root, "OfflineMode");
                if (!offlineMode) {
                    CPLCreateXMLElementAndValue(root, "OfflineMode", "true");
                    shouldSerializeXml = true;
                }
            }


            if (shouldSerializeXml) {
                content = std::string(CPLSerializeXMLTree(root));
                //CPLSerializeXMLTreeToFile(root, (_datasetFilePath + ".xml").c_str());
            }
        }
    }

    _dataset = static_cast<GDALDataset*>(GDALOpen(content.c_str(), GA_ReadOnly));
    if (!_dataset) {
        throw ghoul::RuntimeError("Failed to load dataset: " + _datasetFilePath);
    }

    // Assume all raster bands have the same data type
    _rasterCount = _dataset->GetRasterCount();

    // calculateTileDepthTransform
    unsigned long long maximumValue = [t = _initData.glType]() {
        switch (t) {
            case GL_UNSIGNED_BYTE:  return 1ULL << 8ULL;
            case GL_UNSIGNED_SHORT: return 1ULL << 16ULL;
            case GL_SHORT:          return 1ULL << 15ULL;
            case GL_UNSIGNED_INT:   return 1ULL << 32ULL;
            case GL_INT:            return 1ULL << 31ULL;
            case GL_HALF_FLOAT:     return 1ULL;
            case GL_FLOAT:          return 1ULL;
            case GL_DOUBLE:         return 1ULL;
            default:
                ghoul_assert(false, "Unknown data type");
                throw ghoul::MissingCaseException();
        }
    }();


    _depthTransform.scale = static_cast<float>(
        _dataset->GetRasterBand(1)->GetScale() * maximumValue
    );
    _depthTransform.offset = static_cast<float>(
        _dataset->GetRasterBand(1)->GetOffset()
    );
    _rasterXSize = _dataset->GetRasterXSize();
    _rasterYSize = _dataset->GetRasterYSize();
    _noDataValue = static_cast<float>(_dataset->GetRasterBand(1)->GetNoDataValue());
    _dataType = toGDALDataType(_initData.glType);

    CPLErr error = _dataset->GetGeoTransform(_padfTransform.data());
    if (error == CE_Failure) {
        _padfTransform = geoTransform(_rasterXSize, _rasterYSize);
    }

    double tileLevelDifference = calculateTileLevelDifference(
        _dataset, _initData.dimensions.x
    );

    const int numOverviews = _dataset->GetRasterBand(1)->GetOverviewCount();
    _maxChunkLevel = static_cast<int>(-tileLevelDifference);
    if (numOverviews > 0) {
        _maxChunkLevel += numOverviews - 1;
    }
    _maxChunkLevel = std::max(_maxChunkLevel, 2);
}

void RawTileDataReader::reset() {
    std::lock_guard lockGuard(_datasetLock);
    _maxChunkLevel = -1;
    if (_dataset) {
        GDALClose(_dataset);
        _dataset = nullptr;
    }
    initialize();
}

RawTile::ReadError RawTileDataReader::rasterRead(int rasterBand,
                                                 const IODescription& io,
                                                 char* dataDestination) const
{
    ghoul_assert(isInside(io.read.region, io.read.fullRegion), "write region of bounds!");
    ghoul_assert(
        io.write.region.start.x >= 0 && io.write.region.start.y >= 0,
        "Invalid write region"
    );

    const glm::ivec2 end = io.write.region.start + io.write.region.numPixels;
    [[maybe_unused]] const size_t largestIndex =
        (end.y - 1) * io.write.bytesPerLine + (end.x - 1) * _initData.bytesPerPixel;
    ghoul_assert(largestIndex <= io.write.totalNumBytes, "Invalid write region");

    char* dataDest = dataDestination;

    // GDAL reads pixels top to bottom, but we want our pixels bottom to top.
    // Therefore, we increment the destination pointer to the last line on in the
    // buffer, and the we specify in the rasterIO call that we want negative line
    // spacing. Doing this compensates the flipped Y axis
    dataDest += (io.write.totalNumBytes - io.write.bytesPerLine);

    // handle requested write region. Note -= since flipped y axis
    dataDest -= io.write.region.start.y * io.write.bytesPerLine;
    dataDest += io.write.region.start.x * _initData.bytesPerPixel;

    GDALRasterBand* gdalRasterBand = _dataset->GetRasterBand(rasterBand);
    CPLErr readError = CE_Failure;
    readError = gdalRasterBand->RasterIO(
        GF_Read,
        io.read.region.start.x,         // Begin read x
        io.read.region.start.y,         // Begin read y
        io.read.region.numPixels.x,     // width to read x
        io.read.region.numPixels.y,     // width to read y
        dataDest,                       // Where to put data
        io.write.region.numPixels.x,    // width to write x in destination
        io.write.region.numPixels.y,    // width to write y in destination
        _dataType,                      // Type
        static_cast<int>(_initData.bytesPerPixel), // Pixel spacing
        -static_cast<int>(io.write.bytesPerLine)     // Line spacing
    );

    // Convert error to RawTile::ReadError
    switch (readError) {
        case CE_None:    return RawTile::ReadError::None;
        case CE_Debug:   return RawTile::ReadError::Debug;
        case CE_Warning: return RawTile::ReadError::Warning;
        case CE_Failure: return RawTile::ReadError::Failure;
        case CE_Fatal:   return RawTile::ReadError::Fatal;
        default:         return RawTile::ReadError::Failure;
    }
}

RawTile RawTileDataReader::readTileData(TileIndex tileIndex) const {
    size_t numBytes = _initData.totalNumBytes;

    RawTile rawTile;
    rawTile.imageData = std::unique_ptr<std::byte[]>(new std::byte[numBytes]);
    memset(rawTile.imageData.get(), 0xFF, numBytes);

    IODescription io = ioDescription(tileIndex);
    RawTile::ReadError worstError = RawTile::ReadError::None;
    readImageData(io, worstError, reinterpret_cast<char*>(rawTile.imageData.get()));

    for (const MemoryLocation& ml : NoDataAvailableData) {
        std::byte* ptr = rawTile.imageData.get();
        if (ml.offset >= numBytes || ptr[ml.offset] != ml.value) {
            // Bail out as early as possible
            break;
        }

        // If we got here, we have (most likely) a No data yet available tile
        worstError = RawTile::ReadError::Failure;
    }

    rawTile.error = worstError;
    rawTile.tileIndex = std::move(tileIndex);
    rawTile.textureInitData = _initData;

    if (_preprocess) {
        rawTile.tileMetaData = tileMetaData(rawTile, io.write.region);
        rawTile.error = std::max(
            rawTile.error,
            postProcessErrorCheck(rawTile, _initData.nRasters, noDataValueAsFloat())
        );
    }

    return rawTile;
}

void RawTileDataReader::readImageData(IODescription& io, RawTile::ReadError& worstError,
                                      char* imageDataDest) const
{
    // Only read the minimum number of rasters
    int nRastersToRead = std::min(_rasterCount, static_cast<int>(_initData.nRasters));

    switch (_initData.ghoulTextureFormat) {
        case ghoul::opengl::Texture::Format::Red: {
            char* dest = imageDataDest;
            const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
            worstError = std::max(worstError, err);
            break;
        }
        case ghoul::opengl::Texture::Format::RG:
        case ghoul::opengl::Texture::Format::RGB:
        case ghoul::opengl::Texture::Format::RGBA: {
            if (nRastersToRead == 1) { // Grayscale
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            else if (nRastersToRead == 2) { // Grayscale + alpha
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
                // Last read is the alpha channel
                char* dest = imageDataDest + (3 * _initData.bytesPerDatum);
                const RawTile::ReadError err = repeatedRasterRead(2, io, dest);
                worstError = std::max(worstError, err);
            }
            else { // Three or more rasters
                for (int i = 0; i < nRastersToRead; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = repeatedRasterRead(i + 1, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            break;
        }
        case ghoul::opengl::Texture::Format::BGR:
        case ghoul::opengl::Texture::Format::BGRA: {
            if (nRastersToRead == 1) { // Grayscale
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            else if (nRastersToRead == 2) { // Grayscale + alpha
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
                // Last read is the alpha channel
                char* dest = imageDataDest + (3 * _initData.bytesPerDatum);
                const RawTile::ReadError err = repeatedRasterRead(2, io, dest);
                worstError = std::max(worstError, err);
            }
            else { // Three or more rasters
                for (int i = 0; i < 3 && i < nRastersToRead; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = repeatedRasterRead(3 - i, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            if (nRastersToRead > 3) { // Alpha channel exists
                // Last read is the alpha channel
                char* dest = imageDataDest + (3 * _initData.bytesPerDatum);
                const RawTile::ReadError err = repeatedRasterRead(4, io, dest);
                worstError = std::max(worstError, err);
            }
            break;
        }
        default: {
            ghoul_assert(false, "Texture format not supported for tiles");
            break;
        }
    }
}

IODescription RawTileDataReader::ioDescription(const TileIndex& tileIndex) const {
    IODescription io;
    io.read.region = highestResPixelRegion(tileIndex, _padfTransform);

    // write region starts in origin
    io.write.region.start = glm::ivec2(0);
    io.write.region.numPixels = _initData.dimensions;

    io.read.overview = 0;
    io.read.fullRegion.start = { 0, 0 };
    io.read.fullRegion.numPixels = { _rasterXSize, _rasterYSize };
    // For correct sampling in dataset, we need to pad the texture tile

    PixelRegion scaledPadding;
    scaledPadding.start = _initData.tilePixelStartOffset;
    scaledPadding.numPixels = _initData.tilePixelSizeDifference;

    const double scale = static_cast<double>(io.read.region.numPixels.x) /
                         static_cast<double>(io.write.region.numPixels.x);
    scaledPadding.numPixels *= scale;
    scaledPadding.start *= scale;

    io.read.region.start += scaledPadding.start;
    io.read.region.numPixels += scaledPadding.numPixels;

    io.write.bytesPerLine = _initData.bytesPerLine;
    io.write.totalNumBytes = _initData.totalNumBytes;

    ghoul_assert(
        io.write.region.numPixels.x == io.write.region.numPixels.y,
        "Write region must be square"
    );
    ghoul_assert(
        io.write.region.numPixels.x == _initData.dimensions.x,
        "Write region must match tile it writes to."
    );

    return io;
}

const TileDepthTransform& RawTileDataReader::depthTransform() const {
    return _depthTransform;
}

glm::ivec2 RawTileDataReader::fullPixelSize() const {
    return geodeticToPixel(Geodetic2{ 90.0, 180.0 }, _padfTransform);
}

RawTile::ReadError RawTileDataReader::repeatedRasterRead(int rasterBand,
                                                         const IODescription& fullIO,
                                                         char* dataDestination,
                                                         int depth) const
{

    // NOTE:
    // Ascii graphics illustrates the implementation details of this method, for one
    // specific case. Even though the illustrated case is specific, readers can
    // hopefully find it useful to get the general idea.

    // Make a copy of the full IO desription as we will have to modify it
    IODescription io = fullIO;

    // Example:
    // We have an io description that defines a WRITE and a READ region.
    // In this case the READ region extends outside of the defined io.read.fullRegion,
    // meaning we will have to perform wrapping

    // io.write.region             io.read.region
    //    |                         |
    //    V                         V
    // +-------+                +-------+
    // |       |                |       |--------+
    // |       |                |       |        |
    // |       |                |       |        |
    // +-------+                +-------+        |
    //                            |              | <-- io.read.fullRegion
    //                            |              |
    //                            +--------------+

    RawTile::ReadError worstError = RawTile::ReadError::None;
    if (!isInside(io.read.region, io.read.fullRegion)) {
        //  Loop through each side: left, top, right, bottom
        for (int i = 0; i < 4; ++i) {
            // Example:
            // We are currently considering the left side of the pixel region
            const Side side = static_cast<Side>(i);
            IODescription cutoff = cutIODescription(
                io,
                side,
                edge(io.read.fullRegion, side)
            );

            // Example:
            // We cut off the left part that was outside the io.read.fullRegion, and we
            // now have an additional io description for the cut off region.
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
            //                           |              | <-- io.read.fullRegion
            //                           |              |
            //                           +--------------+

            const int area = cutoff.read.region.numPixels.x *
                             cutoff.read.region.numPixels.y;
            if (area > 0) {
                // Wrap by repeating
                Side oppositeSide = static_cast<Side>((i + 2) % 4);

                alignPixelRegion(
                    cutoff.read.region,
                    oppositeSide,
                    edge(io.read.fullRegion, oppositeSide)
                );

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
                // +-+-----+               |              | <-- io.read.fullRegion
                //                         |              |
                //                         +--------------+

                // Example:
                // The cutoff region has been repeated along one of its sides, but
                // as we can see in this example, it still has a top part outside the
                // defined gdal region. This is handled through recursion.
                const RawTile::ReadError err = repeatedRasterRead(
                    rasterBand,
                    cutoff,
                    dataDestination,
                    depth + 1
                );

                worstError = std::max(worstError, err);
            }
        }
    }

    const RawTile::ReadError err = rasterRead(rasterBand, io, dataDestination);

    // The return error from a repeated rasterRead is ONLY based on the main region,
    // which in the usual case will cover the main area of the patch anyway
    return err;
}

TileMetaData RawTileDataReader::tileMetaData(RawTile& rawTile,
                                             const PixelRegion& region) const
{
    const size_t bytesPerLine = _initData.bytesPerPixel * region.numPixels.x;

    TileMetaData preprocessData;
    preprocessData.maxValues.resize(_initData.nRasters);
    preprocessData.minValues.resize(_initData.nRasters);
    preprocessData.hasMissingData.resize(_initData.nRasters);

    std::vector<float> noDataValues(_initData.nRasters);
    for (size_t raster = 0; raster < _initData.nRasters; ++raster) {
        preprocessData.maxValues[raster] = -FLT_MAX;
        preprocessData.minValues[raster] = FLT_MAX;
        preprocessData.hasMissingData[raster] = false;
        noDataValues[raster] = noDataValueAsFloat();
    }

    bool allIsMissing = true;
    for (int y = 0; y < region.numPixels.y; ++y) {
        const size_t yi = (region.numPixels.y - 1 - y) * bytesPerLine;
        size_t i = 0;
        for (int x = 0; x < region.numPixels.x; ++x) {
            for (size_t raster = 0; raster < _initData.nRasters; ++raster) {
                const float noDataValue = noDataValueAsFloat();
                const float val = interpretFloat(
                    _initData.glType,
                    &(rawTile.imageData.get()[yi + i])
                );
                if (val != noDataValue && val == val) {
                    preprocessData.maxValues[raster] = std::max(
                        val,
                        preprocessData.maxValues[raster]
                    );
                    preprocessData.minValues[raster] = std::min(
                        val,
                        preprocessData.minValues[raster]
                    );
                    allIsMissing = false;
                }
                else {
                    preprocessData.hasMissingData[raster] = true;
                    float& floatToRewrite = reinterpret_cast<float&>(
                        rawTile.imageData[yi + i]
                    );
                    floatToRewrite = -std::numeric_limits<float>::max();
                }
                i += _initData.bytesPerDatum;
            }
        }
    }

    if (allIsMissing) {
        rawTile.error = RawTile::ReadError::Failure;
    }

    return preprocessData;
}

int RawTileDataReader::maxChunkLevel() const {
    return _maxChunkLevel;
}

float RawTileDataReader::noDataValueAsFloat() const {
    return _noDataValue;
}

} // namespace openspace::globebrowsing
