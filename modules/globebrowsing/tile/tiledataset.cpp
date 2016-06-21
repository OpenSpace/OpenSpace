  /*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#include <ogr_featurestyle.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/misc/assert.h>

#include <modules/globebrowsing/tile/tiledataset.h>
#include <modules/globebrowsing/tile/tileprovider.h>

#include <modules/globebrowsing/geometry/angle.h>

#include <float.h>

#include <sstream>
#include <algorithm>





namespace {
    const std::string _loggerCat = "TileDataset";
}



namespace openspace {
    void TilePreprocessData::serialize(std::ostream& os) {
        os << maxValues.size() << std::endl;
        for (float f : maxValues) {
            os << f << " ";
        }
        os << std::endl;
        for (float f : minValues) {
            os << f << " ";
        }
        os << std::endl;
    }


    TilePreprocessData TilePreprocessData::deserialize(std::istream& is) {
        TilePreprocessData res;
        int n; is >> n;
        res.maxValues.resize(n);
        for (int i = 0; i < n; i++) {
            is >> res.maxValues[i];
        }
        res.minValues.resize(n);
        for (int i = 0; i < n; i++) {
            is >> res.minValues[i];
        }

        return std::move(res);
    }

    TileIOResult::TileIOResult()
        : imageData(nullptr)
        , dimensions(0, 0, 0)
        , preprocessData(nullptr)
        , chunkIndex(0, 0, 0)
        , error(CE_None)
        , nBytesImageData(0)
    {

    }


        
    TileIOResult TileIOResult::createDefaultRes() {
        TileIOResult defaultRes;
        int w = 8;
        int h = 8;
        defaultRes.dimensions = glm::uvec3(w, h, 1);
        defaultRes.nBytesImageData = w * h * 1 * 3 * 4; // assume max 3 channels, max 4 bytes per pixel
        defaultRes.imageData = new char[defaultRes.nBytesImageData];
        std::fill_n((char*)defaultRes.imageData, defaultRes.nBytesImageData, 0);
        return std::move(defaultRes);
    }



    void TileIOResult::serializeMetaData(std::ostream& os) {
        os << dimensions.x << " " << dimensions.y << " " << dimensions.z << std::endl;
        os << chunkIndex.x << " " << chunkIndex.y << " " << chunkIndex.level << std::endl;
        os << error << std::endl;

        // preprocess data
        os << (preprocessData != nullptr) << std::endl;
        if (preprocessData != nullptr) {    
            preprocessData->serialize(os);
        }
        
        os << nBytesImageData << std::endl;
    }


    TileIOResult TileIOResult::deserializeMetaData(std::istream& is) {
        TileIOResult res;
        is >> res.dimensions.x >> res.dimensions.y >> res.dimensions.z;
        is >> res.chunkIndex.x >> res.chunkIndex.y >> res.chunkIndex.level;
        int err; is >> err; res.error = (CPLErr) err;
        
        res.preprocessData = nullptr;
        bool hasPreprocessData; 
        is >> hasPreprocessData;
        if (hasPreprocessData) {
            TilePreprocessData preprocessData = TilePreprocessData::deserialize(is);
            res.preprocessData = std::make_shared<TilePreprocessData>(preprocessData);
        }
        
        is >> res.nBytesImageData;

        char binaryDataSeparator;
        is >> binaryDataSeparator; // not used
        
        char* buffer = new char[res.nBytesImageData]();
        return std::move(res);
    }



    // INIT THIS TO FALSE AFTER REMOVED FROM TILEPROVIDER
    bool TileDataset::GdalHasBeenInitialized = false; 

    TileDataset::TileDataset(const std::string& gdalDatasetDesc, int minimumPixelSize, 
        bool doPreprocessing, GLuint dataType)
        : _minimumPixelSize(minimumPixelSize)
        , _doPreprocessing(doPreprocessing)
        , _maxLevel(-1)
    {
        if (!GdalHasBeenInitialized) {
            GDALAllRegister();
            CPLSetConfigOption("GDAL_DATA", absPath("${MODULE_GLOBEBROWSING}/gdal_data").c_str());

            GdalHasBeenInitialized = true;
        }

        _dataset = (GDALDataset *)GDALOpen(gdalDatasetDesc.c_str(), GA_ReadOnly);
        if (!_dataset) {
            throw ghoul::RuntimeError("Failed to load dataset:\n" + gdalDatasetDesc);
        }
        _dataLayout = DataLayout(_dataset, dataType);

        _depthTransform = calculateTileDepthTransform();
        _tileLevelDifference = calculateTileLevelDifference(_dataset, minimumPixelSize);
        _maxLevel = calculateMaxLevel(_tileLevelDifference);
    }


    TileDataset::~TileDataset() {
        delete _dataset;
    }

    int TileDataset::calculateTileLevelDifference(GDALDataset* dataset, int minimumPixelSize) {
        GDALRasterBand* firstBand = dataset->GetRasterBand(1);
        int numOverviews = firstBand->GetOverviewCount();
        int sizeLevel0 = firstBand->GetOverview(numOverviews - 1)->GetXSize();
        return log2(minimumPixelSize) - log2(sizeLevel0);
    }

    int TileDataset::calculateMaxLevel(int calculateMaxLevel) {
        int numOverviews = _dataset->GetRasterBand(1)->GetOverviewCount();
        _maxLevel = numOverviews - 1 - _tileLevelDifference;
        return _maxLevel;
    }

    TileDepthTransform TileDataset::calculateTileDepthTransform() {
        GDALRasterBand* firstBand = _dataset->GetRasterBand(1);
        // Floating point types does not have a fix maximum or minimum value and
        // can not be normalized when sampling a texture. Hence no rescaling is needed.
        double maximumValue = (_dataLayout.gdalType == GDT_Float32 || _dataLayout.gdalType == GDT_Float64) ?
            1.0 : getMaximumValue(_dataLayout.gdalType);
        
        TileDepthTransform transform;
        transform.depthOffset = firstBand->GetOffset();
        transform.depthScale = firstBand->GetScale() * maximumValue;
        return transform;
    }

    int TileDataset::getMaximumLevel() const {
        return _maxLevel;
    }

    TileDepthTransform TileDataset::getDepthTransform() const {
        return _depthTransform;
    }

    std::shared_ptr<TileIOResult> TileDataset::readTileData(ChunkIndex chunkIndex)
    {
        GdalDataRegion region(_dataset, chunkIndex, _tileLevelDifference);
        size_t bytesPerLine = _dataLayout.bytesPerPixel * region.numPixels.x;
        size_t totalNumBytes = bytesPerLine * region.numPixels.y;
        char* imageData = new char[totalNumBytes];

        CPLErr worstError = CPLErr::CE_None;

        // Read the data (each rasterband is a separate channel)
        for (size_t i = 0; i < _dataLayout.numRasters; i++) {
            GDALRasterBand* rasterBand = _dataset->GetRasterBand(i + 1)->GetOverview(region.overview);
            
            char* dataDestination = imageData + (i * _dataLayout.bytesPerDatum);
            
            CPLErr err = rasterBand->RasterIO(
                GF_Read,
                region.pixelStart.x,           // Begin read x
                region.pixelStart.y,           // Begin read y
                region.numPixels.x,            // width to read x
                region.numPixels.y,            // width to read y
                dataDestination,               // Where to put data
                region.numPixels.x,            // width to write x in destination
                region.numPixels.y,            // width to write y in destination
                _dataLayout.gdalType,		   // Type
                _dataLayout.bytesPerPixel,	   // Pixel spacing
                bytesPerLine);      // Line spacing

            // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
            worstError = std::max(worstError, err);
        }

        std::shared_ptr<TileIOResult> result(new TileIOResult);
        result->chunkIndex = chunkIndex;
        result->imageData = getImageDataFlippedY(region, _dataLayout, imageData);
        result->dimensions = glm::uvec3(region.numPixels, 1);
        result->nBytesImageData = _dataLayout.bytesPerPixel * region.numPixels.x * region.numPixels.y;
        if (_doPreprocessing) {
            result->preprocessData = preprocess(imageData, region, _dataLayout);
        }
        result->error = worstError;

        delete[] imageData;
        return result;
    }

    char* TileDataset::getImageDataFlippedY(const GdalDataRegion& region,
        const DataLayout& dataLayout, const char* imageData) 
    {
        size_t bytesPerLine = dataLayout.bytesPerPixel * region.numPixels.x;
        size_t totalNumBytes = bytesPerLine * region.numPixels.y;

        // GDAL reads image data top to bottom. We want the opposite.
        char* imageDataYflipped = new char[totalNumBytes];
        for (size_t y = 0; y < region.numPixels.y; y++) {
            size_t yi_flipped = y * bytesPerLine;
            size_t yi = (region.numPixels.y - 1 - y) * bytesPerLine;
            size_t i = 0;
            for (size_t x = 0; x < region.numPixels.x; x++) {
                for (size_t c = 0; c < dataLayout.numRasters; c++) {
                    for (size_t b = 0; b < dataLayout.bytesPerDatum; b++) {
                        imageDataYflipped[yi_flipped + i] = imageData[yi + i];
                        i++;
                    }
                }
            }
        }

        return imageDataYflipped;
    }


    const TileDataset::DataLayout& TileDataset::getDataLayout() const {
        return _dataLayout;
    }


    std::shared_ptr<TilePreprocessData> TileDataset::preprocess(const char* imageData,
        const GdalDataRegion& region, const DataLayout& dataLayout)
    {
        size_t bytesPerLine = dataLayout.bytesPerPixel * region.numPixels.x;
        size_t totalNumBytes = bytesPerLine * region.numPixels.y;

        TilePreprocessData* preprocessData = new TilePreprocessData();
        preprocessData->maxValues.resize(dataLayout.numRasters);
        preprocessData->minValues.resize(dataLayout.numRasters);

        for (size_t c = 0; c < dataLayout.numRasters; c++) {
            preprocessData->maxValues[c] = -FLT_MAX;
            preprocessData->minValues[c] = FLT_MAX;
        }

        ValueReader valueReader = getValueReader(dataLayout.gdalType);
        for (size_t y = 0; y < region.numPixels.y; y++) {
            size_t yi_flipped = y * bytesPerLine;
            size_t yi = (region.numPixels.y - 1 - y) * bytesPerLine;
            size_t i = 0;
            for (size_t x = 0; x < region.numPixels.x; x++) {
                for (size_t c = 0; c < dataLayout.numRasters; c++) {

                    float val = readFloat(dataLayout.gdalType, &(imageData[yi + i]));
                    preprocessData->maxValues[c] = std::max(val, preprocessData->maxValues[c]);
                    preprocessData->minValues[c] = std::min(val, preprocessData->minValues[c]);

                    i += dataLayout.bytesPerDatum;
                }
            }
        }

        return std::shared_ptr<TilePreprocessData>(preprocessData);
    }


    TileDataset::ValueReader TileDataset::getValueReader(GDALDataType gdalType) {
        switch (gdalType) {
        case GDT_Byte:      return [](const char* src) { return static_cast<float>(*reinterpret_cast<const GLubyte*>(src)); };
        case GDT_UInt16:    return [](const char* src) { return static_cast<float>(*reinterpret_cast<const GLushort*>(src)); };
        case GDT_Int16:     return [](const char* src) { return static_cast<float>(*reinterpret_cast<const GLshort*>(src)); };
        case GDT_UInt32:    return [](const char* src) { return static_cast<float>(*reinterpret_cast<const GLuint*>(src)); };
        case GDT_Int32:     return [](const char* src) { return static_cast<float>(*reinterpret_cast<const GLint*>(src)); };
        case GDT_Float32:   return [](const char* src) { return static_cast<float>(*reinterpret_cast<const GLfloat*>(src)); };
        case GDT_Float64:   return [](const char* src) { return static_cast<float>(*reinterpret_cast<const GLdouble*>(src)); };
        default:
            LERROR("Unknown data type");
            ghoul_assert(false, "Unknown data type");
            return nullptr;
        }
    }

     float TileDataset::readFloat(GDALDataType gdalType, const char* src) {
        switch (gdalType) {
        case GDT_Byte:      return static_cast<float>(*reinterpret_cast<const GLubyte*>(src));
        case GDT_UInt16:    return static_cast<float>(*reinterpret_cast<const GLushort*>(src));
        case GDT_Int16:     return static_cast<float>(*reinterpret_cast<const GLshort*>(src));
        case GDT_UInt32:    return static_cast<float>(*reinterpret_cast<const GLuint*>(src));
        case GDT_Int32:     return static_cast<float>(*reinterpret_cast<const GLint*>(src));
        case GDT_Float32:   return static_cast<float>(*reinterpret_cast<const GLfloat*>(src));
        case GDT_Float64:   return static_cast<float>(*reinterpret_cast<const GLdouble*>(src));
        default:
            LERROR("Unknown data type");
            ghoul_assert(false, "Unknown data type");
            return -1.0;
        }
    }



    size_t TileDataset::numberOfBytes(GDALDataType gdalType) {
        switch (gdalType) {
            case GDT_Byte: return sizeof(GLubyte);
            case GDT_UInt16: return sizeof(GLushort);
            case GDT_Int16: return sizeof(GLshort);
            case GDT_UInt32: return sizeof(GLuint);
            case GDT_Int32: return sizeof(GLint);
            case GDT_Float32: return sizeof(GLfloat);
            case GDT_Float64: return sizeof(GLdouble);
            default:  
                LERROR("Unknown data type");
                ghoul_assert(false, "Unknown data type");
                return -1; 
        }
    }

    size_t TileDataset::getMaximumValue(GDALDataType gdalType) {
        switch (gdalType) {
            case GDT_Byte: return 2 << 7;
            case GDT_UInt16: return 2 << 15;
            case GDT_Int16: return 2 << 14;
            case GDT_UInt32: return 2 << 31;
            case GDT_Int32: return 2 << 30;
            default:
                LERROR("Unknown data type"); 
                return -1;
        }
    }


    
    glm::uvec2 TileDataset::geodeticToPixel(GDALDataset* dataSet, const Geodetic2& geo) {
        double padfTransform[6];
        CPLErr err = dataSet->GetGeoTransform(padfTransform);

        ghoul_assert(err != CE_Failure, "Failed to get transform");

        Scalar Y = Angle<Scalar>::fromRadians(geo.lat).asDegrees();
        Scalar X = Angle<Scalar>::fromRadians(geo.lon).asDegrees();
        
        // convert from pixel and line to geodetic coordinates
        // Xp = padfTransform[0] + P*padfTransform[1] + L*padfTransform[2];
        // Yp = padfTransform[3] + P*padfTransform[4] + L*padfTransform[5];

        // <=>
        double* a = &(padfTransform[0]);
        double* b = &(padfTransform[3]);

        // Xp = a[0] + P*a[1] + L*a[2];
        // Yp = b[0] + P*b[1] + L*b[2];
        
        // <=>
        double divisor = (a[2]*b[1] - a[1]*b[2]);
        ghoul_assert(divisor != 0.0, "Division by zero!");
        //ghoul_assert(a[2] != 0.0, "a2 must not be zero!");
        double P = (a[0]*b[2] - a[2]*b[0] + a[2]*Y - b[2]*X) / divisor;
        double L = (-a[0]*b[1] + a[1]*b[0] - a[1]*Y + b[1]*X) / divisor;
        // ref: https://www.wolframalpha.com/input/?i=X+%3D+a0+%2B+a1P+%2B+a2L,+Y+%3D+b0+%2B+b1P+%2B+b2L,+solve+for+P+and+L

        double Xp = a[0] + P*a[1] + L*a[2];
        double Yp = b[0] + P*b[1] + L*b[2];

        ghoul_assert(abs(X - Xp) < 1e-10, "inverse should yield X as before");
        ghoul_assert(abs(Y - Yp) < 1e-10, "inverse should yield Y as before");
        
        return glm::uvec2(glm::round(P), glm::round(L));
    }


    TextureFormat TileDataset::getTextureFormat(
        int rasterCount, GDALDataType gdalType)
    {
        TextureFormat format;

        switch (rasterCount) {
        case 1: // Red
            format.ghoulFormat = Texture::Format::Red;
            switch (gdalType) {
            case GDT_Byte:      format.glFormat = GL_R8; break;
            case GDT_UInt16:    format.glFormat = GL_R16; break;
            case GDT_Int16:     format.glFormat = GL_R16; break;
            case GDT_UInt32:    format.glFormat = GL_R32UI; break;
            case GDT_Int32:     format.glFormat = GL_R32I; break;
            case GDT_Float32:   format.glFormat = GL_R32F; break;
            //case GDT_Float64:   format.glFormat = GL_RED; break; // No representation of 64 bit float?  
            default: LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        case 2:
            format.ghoulFormat = Texture::Format::RG;
            switch (gdalType) {
            case GDT_Byte: format.glFormat = GL_RG8; break;
            case GDT_UInt16: format.glFormat = GL_RG16; break;
            case GDT_Int16: format.glFormat = GL_RG16; break;
            case GDT_UInt32: format.glFormat = GL_RG32UI; break;
            case GDT_Int32: format.glFormat = GL_RG32I; break;
            case GDT_Float32: format.glFormat = GL_RG32F; break;    
            case GDT_Float64: format.glFormat = GL_RED; break; // No representation of 64 bit float?
            default: LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        case 3:
            format.ghoulFormat = Texture::Format::RGB;
            switch (gdalType) {
            case GDT_Byte: format.glFormat = GL_RGB8; break;
            case GDT_UInt16: format.glFormat = GL_RGB16; break;
            case GDT_Int16: format.glFormat = GL_RGB16; break;
            case GDT_UInt32: format.glFormat = GL_RGB32UI; break;
            case GDT_Int32: format.glFormat = GL_RGB32I; break;
            case GDT_Float32: format.glFormat = GL_RGB32F; break;    
            // case GDT_Float64: format.glFormat = GL_RED; break;// No representation of 64 bit float? 
            default: LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        case 4:
            format.ghoulFormat = Texture::Format::RGBA;
            switch (gdalType) {
            case GDT_Byte: format.glFormat = GL_RGBA8; break;
            case GDT_UInt16: format.glFormat = GL_RGBA16; break;
            case GDT_Int16: format.glFormat = GL_RGBA16; break;
            case GDT_UInt32: format.glFormat = GL_RGBA32UI; break;
            case GDT_Int32: format.glFormat = GL_RGBA32I; break;
            case GDT_Float32: format.glFormat = GL_RGBA32F; break;
            case GDT_Float64: format.glFormat = GL_RED; break; // No representation of 64 bit float?
            default: LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        default:
            LERROR("Unknown number of channels for OpenGL texture: " << rasterCount);
            break;
        }
        return format;
    }






    GLuint TileDataset::getOpenGLDataType(GDALDataType gdalType) {
        switch (gdalType) {
        case GDT_Byte: return GL_UNSIGNED_BYTE; 
        case GDT_UInt16: return GL_UNSIGNED_SHORT;
        case GDT_Int16: return GL_SHORT;
        case GDT_UInt32: return GL_UNSIGNED_INT;
        case GDT_Int32: return GL_INT;
        case GDT_Float32: return GL_FLOAT;
        case GDT_Float64: return GL_DOUBLE; 
        default:
            LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            return GL_UNSIGNED_BYTE;
        }
    }

    GDALDataType TileDataset::getGdalDataType(GLuint glType) {
        switch (glType) {
        case GL_UNSIGNED_BYTE: return GDT_Byte;
        case GL_UNSIGNED_SHORT: return GDT_UInt16;
        case GL_SHORT: return GDT_Int16;
        case GL_UNSIGNED_INT: return GDT_UInt32;
        case GL_INT: return GDT_Int32;
        case GL_FLOAT: return GDT_Float32;
        case GL_DOUBLE: return GDT_Float64;
        default:
            LERROR("OpenGL data type unknown to GDAL: " << glType);
            return GDT_Unknown;
        }
    }


    TileDataset::GdalDataRegion::GdalDataRegion(GDALDataset * dataSet,
        const ChunkIndex& chunkIndex, int tileLevelDifference)
        : chunkIndex(chunkIndex)
    {

        GDALRasterBand* firstBand = dataSet->GetRasterBand(1);

        // Assume all raster bands have the same data type

        // Level = overviewCount - overview (default, levels may be overridden)
        int numOverviews = firstBand->GetOverviewCount();


        // Generate a patch from the chunkIndex, extract the bounds which
        // are used to calculated where in the GDAL data set to read data. 
        // pixelStart0 and pixelEnd0 defines the interval in the pixel space 
        // at overview 0
        GeodeticPatch patch = GeodeticPatch(chunkIndex);

        glm::uvec2 pixelStart0 = geodeticToPixel(dataSet, patch.getCorner(Quad::NORTH_WEST));
        glm::uvec2 pixelEnd0 = geodeticToPixel(dataSet, patch.getCorner(Quad::SOUTH_EAST));
        glm::uvec2 numPixels0 = pixelEnd0 - pixelStart0;

        // Calculate a suitable overview to choose from the GDAL dataset
        int minNumPixels0 = glm::min(numPixels0.x, numPixels0.y);
        int sizeLevel0 = firstBand->GetOverview(numOverviews - 1)->GetXSize();
        int ov = std::log2(minNumPixels0) - std::log2(sizeLevel0 + 1) - tileLevelDifference;
        ov = glm::clamp(ov, 0, numOverviews - 1);

        // Convert the interval [pixelStart0, pixelEnd0] to pixel space at 
        // the calculated suitable overview, ov. using a >> b = a / 2^b
        int toShift = ov + 1;

        // Set member variables
        overview = ov;

        pixelStart = glm::uvec2(pixelStart0.x >> toShift, pixelStart0.y >> toShift);
        pixelEnd = glm::uvec2(pixelEnd0.x >> toShift, pixelEnd0.y >> toShift);
        numPixels = pixelEnd - pixelStart;
    }

    TileDataset::DataLayout::DataLayout() {

    }

    TileDataset::DataLayout::DataLayout(GDALDataset* dataSet, GLuint _glType) {
        // Assume all raster bands have the same data type
        gdalType = _glType != 0 ? getGdalDataType(glType) : dataSet->GetRasterBand(1)->GetRasterDataType();
        glType = getOpenGLDataType(gdalType);
        numRasters = dataSet->GetRasterCount();
        bytesPerDatum = numberOfBytes(gdalType);
        bytesPerPixel = bytesPerDatum * numRasters;
        textureFormat = getTextureFormat(numRasters, gdalType);
    }

}  // namespace openspace
