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

namespace {
    const std::string _loggerCat = "GdalDataConverter";
}

namespace openspace {

    GdalDataConverter::GdalDataConverter()
    {

    }

    GdalDataConverter::~GdalDataConverter()
    {

    }

    std::shared_ptr<Texture> GdalDataConverter::convertToOpenGLTexture(
        GDALDataset* dataSet,
        const GeodeticTileIndex& tileIndex,
        int GLType)
    {
        int nRasters = dataSet->GetRasterCount();

        ghoul_assert(nRasters > 0, "Bad dataset. Contains no rasterband.");

        GDALRasterBand* firstBand = dataSet->GetRasterBand(1);

        // Level = overviewCount - overview
        int overviewCount = firstBand->GetOverviewCount();
        int overview = overviewCount - tileIndex.level - 1;

        // The output texture will have this size
        int xSizelevel0 = firstBand->GetOverview(overviewCount - 1)->GetXSize();
        int ySizelevel0 = firstBand->GetOverview(overviewCount - 1)->GetYSize();




        // The data that the texture should read
        GLubyte* imageData = new GLubyte[xSizelevel0 * ySizelevel0 * nRasters];

        // Read the data (each rasterband is a separate channel)
        for (size_t i = 0; i < nRasters; i++)
        {
            GDALRasterBand* rasterBand = dataSet->GetRasterBand(i + 1)->GetOverview(overview);
            
            int xBeginRead = tileIndex.x * pow(2, tileIndex.level) *  xSizelevel0;
            int yBeginRead = tileIndex.y * pow(2, tileIndex.level) *  ySizelevel0;
            rasterBand->RasterIO(
                GF_Read,
                xBeginRead,					// Begin read x
                yBeginRead,					// Begin read y
                xSizelevel0,				// width to read x
                ySizelevel0,				// width to read y
                imageData + i,				// Where to put data
                xSizelevel0,				// width to read x in destination
                ySizelevel0,				// width to read y in destination
                GDT_Byte,					// Type
                sizeof(GLubyte) * nRasters,	// Pixel spacing
                0);							// Line spacing
        }

        GdalDataConverter::TextureFormat textrureFormat =
            getTextureFormatFromRasterCount(nRasters);


        Texture* tex = new Texture(
            static_cast<void*>(imageData),
            glm::uvec3(xSizelevel0, ySizelevel0, 1),
            textrureFormat.ghoulFormat,
            textrureFormat.glFormat,
            GL_UNSIGNED_BYTE,
            Texture::FilterMode::Linear,
            Texture::WrappingMode::Repeat);

        // The texture should take ownership of the data
        std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(tex);

        
        // Do not free imageData since the texture now has ownership of it
        return texture;
    }



    glm::uvec2 GdalDataConverter::geodeticToPixel(GDALDataset* dataSet, const Geodetic2& geo) {
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

        return glm::uvec2(P, L);
    }



    GdalDataConverter::TextureFormat GdalDataConverter::getTextureFormatFromRasterCount(
        int rasterCount)
    {
        TextureFormat format;

        switch (rasterCount)
        {
        case 1:
            format.ghoulFormat = Texture::Format::Red;
            format.glFormat = GL_RED;
            break;
        case 2:
            format.ghoulFormat = Texture::Format::RG;
            format.glFormat = GL_RG;
            break;
        case 3:
            format.ghoulFormat = Texture::Format::RGB;
            format.glFormat = GL_RGB;
            break;
        case 4:
            format.ghoulFormat = Texture::Format::RGBA;
            format.glFormat = GL_RGBA;
            break;
        default:

            break;
        }
        return format;
    }

}  // namespace openspace
