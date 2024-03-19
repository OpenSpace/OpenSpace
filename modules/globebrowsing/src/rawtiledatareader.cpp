/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/misc/profiling.h>

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
#include <filesystem>
#include <system_error>

namespace openspace::globebrowsing {

namespace {
    constexpr std::string_view _loggerCat = "RawTileDataReader";

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
            LERRORC(
                "GDALRawTileDataReader",
                fmt::format(
                    "OpenGL data type unknown to GDAL: {}", static_cast<int>(glType)
                )
            );
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
    GDALRasterBand* maxOverview = nullptr;
    const int numOverviews = firstBand->GetOverviewCount();
    if (numOverviews <= 0) { // No overviews. Use first band.
        maxOverview = firstBand;
    }
    else { // Pick the highest overview.
        maxOverview = firstBand->GetOverview(numOverviews - 1);
    }
    const int sizeLevel0 = maxOverview->GetXSize();
    const double diff = log2(minimumPixelSize) - log2(sizeLevel0);
    const double intdiff = diff >= 0 ? ceil(diff) : floor(diff);
    return static_cast<int>(intdiff);
}

bool isInside(const PixelRegion& lhs, const PixelRegion& rhs) {
    const glm::ivec2 e = lhs.start + lhs.numPixels;
    const glm::ivec2 re = rhs.start + rhs.numPixels;
    return rhs.start.x <= lhs.start.x && e.x <= re.x &&
           rhs.start.y <= lhs.start.y && e.y <= re.y;
}

/**
 * Returns the geo transform from raster space to projection coordinates as defined
 * by GDAL.
 */
std::array<double, 6> geoTransform(int rasterX, int rasterY) {
    const GeodeticPatch cov(
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
    ghoul_assert(divisor != 0.0, "Division by zero");

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
    ghoul_assert(nRasters == rawTile.tileMetaData.nValues, "Wrong numbers of max values");

    const bool hasMissingData = std::any_of(
        rawTile.tileMetaData.maxValues.begin(),
        rawTile.tileMetaData.maxValues.begin() + rawTile.tileMetaData.nValues,
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
                                     TileCacheProperties cacheProperties,
                                     PerformPreprocessing preprocess)
    : _datasetFilePath(std::move(filePath))
    , _initData(std::move(initData))
    , _cacheProperties(std::move(cacheProperties))
    , _preprocess(preprocess)
{
    ZoneScoped;

    initialize();
}

RawTileDataReader::~RawTileDataReader() {
    const std::lock_guard lockGuard(_datasetLock);
    if (_dataset) {
        GDALClose(_dataset);
        _dataset = nullptr;
    }
}

std::optional<std::string> RawTileDataReader::mrfCache() {
    // We don't support these formats as they will typically lack
    // crucial imformation such as GeoTags. It also makes little sense to
    // cache them as they are already local files.
    // If it is crucial to cache a dataset of this type, convert it to geotiff.
    constexpr std::array<std::string_view, 11> Unsupported = {
        "jpeg", "jpg",
        "png",
        "bmp",
        "psd",
        "tga",
        "gif",
        "hdr",
        "pic",
        "ppm", "pgm"
    };

    for (std::string_view fmt : Unsupported) {
        if (_datasetFilePath.ends_with(fmt)) {
            LWARNING(fmt::format(
                "Unsupported file format for MRF caching: '{}', Dataset: '{}'",
                fmt, _datasetFilePath
            ));
            return std::nullopt;
        }
    }

    const GlobeBrowsingModule& mod = *global::moduleEngine->module<GlobeBrowsingModule>();

    const std::string datasetIdentifier =
        std::to_string(std::hash<std::string>{}(_datasetFilePath));
    const std::string path = fmt::format("{}/{}/{}/",
        mod.mrfCacheLocation(), _cacheProperties.path, datasetIdentifier);
    const std::string root = absPath(path).string();
    std::string mrf = root + datasetIdentifier + ".mrf";

    if (!std::filesystem::exists(mrf)) {
        std::error_code ec;
        if (!std::filesystem::create_directories(root, ec)) {
            // Already existing directories causes a 'failure' but no error
            if (ec) {
                LWARNING(fmt::format(
                    "Failed to create directories for cache at: '{}'. "
                    "Error Code: '{}', message: {}",
                    root, std::to_string(ec.value()), ec.message()
                ));
                return std::nullopt;
            }
        }

        GDALDriver* driver = GetGDALDriverManager()->GetDriverByName("MRF");
        if (driver != nullptr) {
            GDALDataset* src = static_cast<GDALDataset*>(
                GDALOpen(_datasetFilePath.c_str(), GA_ReadOnly)
            );
            if (!src) {
                LWARNING(fmt::format(
                    "Failed to load dataset '{}'. GDAL error: {}",
                    _datasetFilePath, CPLGetLastErrorMsg()
                ));
                return std::nullopt;
            }

            defer { GDALClose(src); };

            char** createOpts = nullptr;
            createOpts = CSLSetNameValue(
                createOpts,
                "CACHEDSOURCE",
                _datasetFilePath.c_str()
            );
            createOpts = CSLSetNameValue(createOpts, "NOCOPY", "true");
            createOpts = CSLSetNameValue(createOpts, "uniform_scale", "2");
            createOpts = CSLSetNameValue(
                createOpts,
                "compress",
                _cacheProperties.compression.c_str()
            );
            createOpts = CSLSetNameValue(
                createOpts,
                "quality",
                std::to_string(_cacheProperties.quality).c_str()
            );
            createOpts = CSLSetNameValue(
                createOpts,
                "blocksize",
                std::to_string(_cacheProperties.blockSize).c_str()
            );

            GDALDataset* dst = static_cast<GDALDataset*>(
                driver->CreateCopy(mrf.c_str(), src, false, createOpts, nullptr, nullptr)
            );
            if (!dst) {
                LWARNING(fmt::format(
                    "Failed to create MRF Caching dataset dataset '{}'. GDAL error: {}",
                    mrf, CPLGetLastErrorMsg()
                ));
                return std::nullopt;
            }
            GDALClose(dst);

            return mrf;
        }
        else {
            LWARNING("Failed to create MRF driver");
            return std::nullopt;
        }
    }
    else {
        return mrf;
    }
}

void RawTileDataReader::initialize() {
    ZoneScoped;

    if (_datasetFilePath.empty()) {
        throw ghoul::RuntimeError("File path must not be empty");
    }
    std::string content = _datasetFilePath;

    if (_cacheProperties.enabled) {
        ZoneScopedN("MRF Caching");

        std::optional<std::string> cache = mrfCache();
        if (cache.has_value()) {
            content = cache.value();
        }
    }

    {
        ZoneScopedN("GDALOpen");
        _dataset = static_cast<GDALDataset*>(GDALOpen(content.c_str(), GA_ReadOnly));
        if (!_dataset) {
            throw ghoul::RuntimeError(fmt::format(
                "Failed to load dataset '{}'. GDAL error: {}",
                _datasetFilePath, CPLGetLastErrorMsg()
            ));
        }
    }

    // Assume all raster bands have the same data type
    _rasterCount = _dataset->GetRasterCount();

    // calculateTileDepthTransform
    const unsigned long long maximumValue = [](GLenum t) {
        switch (t) {
            case GL_UNSIGNED_BYTE:  return 1ULL << 8ULL;
            case GL_UNSIGNED_SHORT: return 1ULL << 16ULL;
            case GL_SHORT:          return 1ULL << 15ULL;
            case GL_UNSIGNED_INT:   return 1ULL << 32ULL;
            case GL_INT:            return 1ULL << 31ULL;
            case GL_HALF_FLOAT:
            case GL_FLOAT:
            case GL_DOUBLE:         return 1ULL;
            default:                throw ghoul::MissingCaseException();
        }
    }(_initData.glType);


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

    const CPLErr error = _dataset->GetGeoTransform(_padfTransform.data());
    if (error == CE_Failure) {
        _padfTransform = geoTransform(_rasterXSize, _rasterYSize);
    }

    const double tileLevelDifference = calculateTileLevelDifference(
        _dataset,
        _initData.dimensions.x
    );

    const int numOverviews = _dataset->GetRasterBand(1)->GetOverviewCount();
    _maxChunkLevel = static_cast<int>(-tileLevelDifference);
    if (numOverviews > 0) {
        _maxChunkLevel += numOverviews;
    }
    _maxChunkLevel = std::max(_maxChunkLevel, 2);
}

void RawTileDataReader::reset() {
    const std::lock_guard lockGuard(_datasetLock);
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
    ghoul_assert(isInside(io.read.region, io.read.fullRegion), "write region of bounds");
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
    const size_t numBytes = _initData.totalNumBytes;

    RawTile rawTile;
    rawTile.imageData = std::unique_ptr<std::byte[]>(new std::byte[numBytes]);
    memset(rawTile.imageData.get(), 0xFF, numBytes);

    IODescription io = ioDescription(tileIndex);
    RawTile::ReadError worstError = RawTile::ReadError::None;
    readImageData(io, worstError, reinterpret_cast<char*>(rawTile.imageData.get()));

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
    const int nReadRasters = std::min(_rasterCount, static_cast<int>(_initData.nRasters));

    switch (_initData.ghoulTextureFormat) {
        case ghoul::opengl::Texture::Format::Red: {
            char* dest = imageDataDest;
            const RawTile::ReadError err = rasterRead(1, io, dest);
            worstError = std::max(worstError, err);
            break;
        }
        case ghoul::opengl::Texture::Format::RG:
        case ghoul::opengl::Texture::Format::RGB:
        case ghoul::opengl::Texture::Format::RGBA: {
            if (nReadRasters == 1) { // Grayscale
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = rasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            else if (nReadRasters == 2) { // Grayscale + alpha
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = rasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
                // Last read is the alpha channel
                char* dest = imageDataDest + (3 * _initData.bytesPerDatum);
                const RawTile::ReadError err = rasterRead(2, io, dest);
                worstError = std::max(worstError, err);
            }
            else { // Three or more rasters
                for (int i = 0; i < nReadRasters; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = rasterRead(i + 1, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            break;
        }
        case ghoul::opengl::Texture::Format::BGR:
        case ghoul::opengl::Texture::Format::BGRA: {
            if (nReadRasters == 1) { // Grayscale
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = rasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            else if (nReadRasters == 2) { // Grayscale + alpha
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = rasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
                // Last read is the alpha channel
                char* dest = imageDataDest + (3 * _initData.bytesPerDatum);
                const RawTile::ReadError err = rasterRead(2, io, dest);
                worstError = std::max(worstError, err);
            }
            else { // Three or more rasters
                for (int i = 0; i < 3 && i < nReadRasters; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum);
                    const RawTile::ReadError err = rasterRead(3 - i, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            if (nReadRasters > 3) { // Alpha channel exists
                // Last read is the alpha channel
                char* dest = imageDataDest + (3 * _initData.bytesPerDatum);
                const RawTile::ReadError err = rasterRead(4, io, dest);
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
    io.read.fullRegion.start = glm::ivec2(0, 0);
    io.read.fullRegion.numPixels = glm::ivec2(_rasterXSize, _rasterYSize);

    io.write.bytesPerLine = _initData.bytesPerLine;
    io.write.totalNumBytes = _initData.totalNumBytes;

    ghoul_assert(
        io.write.region.numPixels.x == io.write.region.numPixels.y,
        "Write region must be square"
    );
    ghoul_assert(
        io.write.region.numPixels.x == _initData.dimensions.x,
        "Write region must match tile it writes to"
    );

    return io;
}

const TileDepthTransform& RawTileDataReader::depthTransform() const {
    return _depthTransform;
}

glm::ivec2 RawTileDataReader::fullPixelSize() const {
    return geodeticToPixel(Geodetic2{ 90.0, 180.0 }, _padfTransform);
}

TileMetaData RawTileDataReader::tileMetaData(RawTile& rawTile,
                                             const PixelRegion& region) const
{
    const size_t bytesPerLine = _initData.bytesPerPixel * region.numPixels.x;

    TileMetaData ppData;
    ghoul_assert(_initData.nRasters <= 4, "Unexpected number of rasters");
    ppData.nValues = static_cast<uint8_t>(_initData.nRasters);

    std::fill(ppData.maxValues.begin(), ppData.maxValues.end(), -FLT_MAX);
    std::fill(ppData.minValues.begin(), ppData.minValues.end(), FLT_MAX);
    std::fill(ppData.hasMissingData.begin(), ppData.hasMissingData.end(), false);

    bool allIsMissing = true;
    for (int y = 0; y < region.numPixels.y; ++y) {
        const size_t yi =
            (static_cast<unsigned long long>(region.numPixels.y) - 1 - y) * bytesPerLine;
        size_t i = 0;
        for (int x = 0; x < region.numPixels.x; ++x) {
            for (size_t raster = 0; raster < _initData.nRasters; ++raster) {
                const float noDataValue = noDataValueAsFloat();
                const float val = interpretFloat(
                    _initData.glType,
                    &(rawTile.imageData.get()[yi + i])
                );
                if (val != noDataValue && val == val) {
                    ppData.maxValues[raster] = std::max(
                        val,
                        ppData.maxValues[raster]
                    );
                    ppData.minValues[raster] = std::min(
                        val,
                        ppData.minValues[raster]
                    );
                    allIsMissing = false;
                }
                else {
                    ppData.hasMissingData[raster] = true;
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

    return ppData;
}

int RawTileDataReader::maxChunkLevel() const {
    return _maxChunkLevel;
}

float RawTileDataReader::noDataValueAsFloat() const {
    return _noDataValue;
}

} // namespace openspace::globebrowsing
