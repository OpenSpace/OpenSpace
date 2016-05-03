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

#ifndef __GDALDATACONVERTER_H__
#define __GDALDATACONVERTER_H__

//#include <modules/globebrowsing/other/tileprovider.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

#include <modules/globebrowsing/geodetics/geodetic2.h>

#include "gdal_priv.h"

#include <memory>


namespace openspace {
    using namespace ghoul::opengl;

    struct UninitializedTextureTile {

        struct TextureFormat
        {
            Texture::Format ghoulFormat;
            GLuint glFormat;
        };

        UninitializedTextureTile(
            void* data,
            glm::uvec3 dims,
            TextureFormat format,
            GLuint glType,
            const GeodeticTileIndex& ti)
            : imageData(data)
            , dimensions(dims)
            , texFormat(format)
            , glType(glType)
            , tileIndex(ti)
        {

        }

        void* imageData;
        glm::uvec3 dimensions;
        TextureFormat texFormat;
        GLuint glType;
        const GeodeticTileIndex tileIndex;
    };

    template<class T>
    class GdalDataConverter
    {
    public:

        GdalDataConverter();
        ~GdalDataConverter();

        std::shared_ptr<UninitializedTextureTile> getUninitializedTextureTile(
            GDALDataset * dataSet,
            const GeodeticTileIndex & tileIndex);

        UninitializedTextureTile::TextureFormat getTextureFormatFromRasterCount(
            int rasterCount);
        GLuint getGlDataTypeFromGdalDataType(GDALDataType gdalType);

        glm::uvec2 geodeticToPixel(GDALDataset* dataSet, const Geodetic2& geo);
    };

} // namespace openspace

#include <modules/globebrowsing/other/gdaldataconverter.inl>

#endif  // __GDALDATACONVERTER_H__