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


#include <modules/globebrowsing/other/gdaldataconverter.h>
#include <modules/globebrowsing/other/tileprovider.h>
#include <modules/globebrowsing/geodetics/angle.h>


namespace openspace {

    template<class T>
    GdalDataConverter<T>::GdalDataConverter()
    {

    }

    template<class T>
    GdalDataConverter<T>::~GdalDataConverter()
    {

    }


    template<class T>
    std::shared_ptr<UninitializedTextureTile> GdalDataConverter<T>::getUninitializedTextureTile(
        GDALDataset* dataSet,
        ChunkIndex chunkIndex,
        int tileLevelDifference)
    {
        int nRasters = dataSet->GetRasterCount();

        ghoul_assert(nRasters > 0, "Bad dataset. Contains no rasterband.");
        ghoul_assert(chunkIndex.level > 0, "Level of chunk index must be bigger than 0.");

        GDALRasterBand* firstBand = dataSet->GetRasterBand(1);

        // Assume all raster bands have the same data type
        GDALDataType gdalType = firstBand->GetRasterDataType();

        // Level = overviewCount - overview (default, levels may be overridden)
        int numOverviews = firstBand->GetOverviewCount();


        // Generate a patch from the chunkIndex, extract the bounds which
        // are used to calculated where in the GDAL data set to read data. 
        // pixelStart0 and pixelEnd0 defines the interval in the pixel space 
        // at overview 0
        GeodeticPatch patch = GeodeticPatch(chunkIndex);
        glm::uvec2 pixelStart0 = geodeticToPixel(dataSet, patch.northWestCorner());
        glm::uvec2 pixelEnd0 = geodeticToPixel(dataSet, patch.southEastCorner());
        glm::uvec2 numPixels0 = pixelEnd0 - pixelStart0;

        // Calculate a suitable overview to choose from the GDAL dataset
        int minNumPixels0 = glm::min(numPixels0.x, numPixels0.y);
        int sizeLevel0 = firstBand->GetOverview(numOverviews - 1)->GetXSize();
        int ov = log2(minNumPixels0) - log2(sizeLevel0 + 1) - tileLevelDifference;
        ov = glm::clamp(ov, 0, numOverviews - 1);

        // Convert the interval [pixelStart0, pixelEnd0] to pixel space at 
        // the calculated suitable overview, ov. using a >> b = a / 2^b
        int toShift = ov + 1;
        glm::uvec2 pixelStart(pixelStart0.x >> toShift, pixelStart0.y >> toShift);
        glm::uvec2 pixelEnd(pixelEnd0.x >> toShift, pixelEnd0.y >> toShift);
        glm::uvec2 numPixels = pixelEnd - pixelStart;

        // When GDAL reads rasterbands of small size, the image data gets screwed up.
        // This is not a solution to the problem, but makes it look a little better
        if (numPixels.x < 32 || numPixels.y < 32) {
            numPixels = glm::uvec2(32, 32);
        }

        // GDAL reads image data top to bottom
        T* imageData = new T[numPixels.x * numPixels.y * nRasters];

        // Read the data (each rasterband is a separate channel)
        for (size_t i = 0; i < nRasters; i++) {
            GDALRasterBand* rasterBand = dataSet->GetRasterBand(i + 1)->GetOverview(ov);

            int xSize = rasterBand->GetXSize();
            int ySize = rasterBand->GetYSize();

            CPLErr err = rasterBand->RasterIO(
                GF_Read,
                pixelStart.x,           // Begin read x
                pixelStart.y,           // Begin read y
                numPixels.x,            // width to read x
                numPixels.y,            // width to read y
                imageData + i,          // Where to put data
                numPixels.x,            // width to write x in destination
                numPixels.y,            // width to write y in destination
                gdalType,		        // Type
                sizeof(T) * nRasters,	// Pixel spacing
                0);                     // Line spacing

            if (err != CE_None) {
              ;//LERROR("There was a IO error (" << err << ") for: " << dataSet->GetDescription());
            }
        }
        // GDAL reads image data top to bottom. We want the opposite.
        T* imageDataYflipped = new T[numPixels.x * numPixels.y * nRasters];
        for (size_t y = 0; y < numPixels.y; y++) {
            for (size_t x = 0; x < numPixels.x * nRasters; x++) {
                imageDataYflipped[x + y * numPixels.x * nRasters] =
                    imageData[x + (numPixels.y - 1 - y) * numPixels.x * nRasters];
            }
        }

        delete[] imageData;
        
        glm::uvec3 dims(numPixels.x, numPixels.y, 1);
        UninitializedTextureTile::TextureFormat textureFormat =
            getTextureFormat(nRasters, gdalType);
        GLuint glType = getGlDataTypeFromGdalDataType(gdalType);
        UninitializedTextureTile* uninitedTexPtr = new UninitializedTextureTile(
            imageDataYflipped,
            dims,
            textureFormat,
            glType,
            chunkIndex);
        std::shared_ptr<UninitializedTextureTile> uninitedTex =
            std::shared_ptr<UninitializedTextureTile>(uninitedTexPtr);
        return uninitedTex;
    }

    template<class T>
    glm::uvec2 GdalDataConverter<T>::geodeticToPixel(GDALDataset* dataSet, const Geodetic2& geo) {
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

    template<class T>
    UninitializedTextureTile::TextureFormat GdalDataConverter<T>::getTextureFormat(
        int rasterCount,
        GDALDataType gdalType)
    {
        UninitializedTextureTile::TextureFormat format;

        switch (rasterCount)
        {
        case 1: // Red
            format.ghoulFormat = Texture::Format::Red;
            switch (gdalType)
            {
            case GDT_Byte:
                format.glFormat = GL_R8;
                break;
            case GDT_UInt16:
                format.glFormat = GL_R16;
                break;
            case GDT_Int16:
                format.glFormat = GL_R16;
                break;
            case GDT_UInt32:
                format.glFormat = GL_R32UI;
                break;
            case GDT_Int32:
                format.glFormat = GL_R32I;
                break;
            case GDT_Float32:
                format.glFormat = GL_R32F;
                break;
                /*
                case GDT_Float64:
                format.glFormat = GL_RED; // No representation of 64 bit float?
                break;
                */
            default:
                ;//LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        case 2:
            format.ghoulFormat = Texture::Format::RG;
            switch (gdalType)
            {
            case GDT_Byte:
                format.glFormat = GL_RG8;
                break;
            case GDT_UInt16:
                format.glFormat = GL_RG16;
                break;
            case GDT_Int16:
                format.glFormat = GL_RG16;
                break;
            case GDT_UInt32:
                format.glFormat = GL_RG32UI;
                break;
            case GDT_Int32:
                format.glFormat = GL_RG32I;
                break;
            case GDT_Float32:
                format.glFormat = GL_RG32F;
                break;
                /*
                case GDT_Float64:
                format.glFormat = GL_RED; // No representation of 64 bit float?
                break;
                */
            default:
                ;//LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        case 3:
            format.ghoulFormat = Texture::Format::RGB;
            switch (gdalType)
            {
            case GDT_Byte:
                format.glFormat = GL_RGB8;
                break;
            case GDT_UInt16:
                format.glFormat = GL_RGB16;
                break;
            case GDT_Int16:
                format.glFormat = GL_RGB16;
                break;
            case GDT_UInt32:
                format.glFormat = GL_RGB32UI;
                break;
            case GDT_Int32:
                format.glFormat = GL_RGB32I;
                break;
            case GDT_Float32:
                format.glFormat = GL_RGB32F;
                break;
                /*
                case GDT_Float64:
                format.glFormat = GL_RED; // No representation of 64 bit float?
                break;
                */
            default:
                ;//LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        case 4:
            format.ghoulFormat = Texture::Format::RGBA;
            switch (gdalType)
            {
            case GDT_Byte:
                format.glFormat = GL_RGBA8;
                break;
            case GDT_UInt16:
                format.glFormat = GL_RGBA16;
                break;
            case GDT_Int16:
                format.glFormat = GL_RGBA16;
                break;
            case GDT_UInt32:
                format.glFormat = GL_RGBA32UI;
                break;
            case GDT_Int32:
                format.glFormat = GL_RGBA32I;
                break;
            case GDT_Float32:
                format.glFormat = GL_RGBA32F;
                break;
                /*
                case GDT_Float64:
                format.glFormat = GL_RED; // No representation of 64 bit float?
                break;
                */
            default:
                ;//LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        default:
            ;//LERROR("Unknown number of channels for OpenGL texture: " << rasterCount);
            break;
        }
        return format;
    }

    template<class T>
    GLuint GdalDataConverter<T>::getGlDataTypeFromGdalDataType(GDALDataType gdalType)
    {
        switch (gdalType)
        {
        case GDT_Byte:
            return GL_UNSIGNED_BYTE;
            break;
        case GDT_UInt16:
            return GL_UNSIGNED_SHORT;
            break;
        case GDT_Int16:
            return GL_SHORT;
            break;
        case GDT_UInt32:
            return GL_UNSIGNED_INT;
            break;
        case GDT_Int32:
            return GL_INT;
            break;
        case GDT_Float32:
            return GL_FLOAT;
            break;
        case GDT_Float64:
            return GL_DOUBLE;
            break;
        default:
            //LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            return GL_UNSIGNED_BYTE;
        }
    }

}  // namespace openspace
