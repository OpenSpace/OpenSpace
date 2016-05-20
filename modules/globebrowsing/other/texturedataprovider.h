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

#ifndef __TEXTURE_DATA_PROVIDER_H__
#define __TEXTURE_DATA_PROVIDER_H__

//#include <modules/globebrowsing/other/tileprovider.h>

#include <ghoul/opengl/texture.h>

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/other/threadpool.h>

#include "gdal_priv.h"

#include <memory>
#include <set>
#include <queue>



namespace openspace {
    using namespace ghoul::opengl;

    struct RawTileData {

        struct TextureFormat {
            Texture::Format ghoulFormat;
            GLuint glFormat;
        };



        RawTileData(void* data, glm::uvec3 dims, TextureFormat format,
            GLuint glType, const ChunkIndex& chunkIndex)
            : imageData(data)
            , dimensions(dims)
            , texFormat(format)
            , glType(glType)
            , chunkIndex(chunkIndex)
        {

        }

        void* imageData;
        glm::uvec3 dimensions;
        TextureFormat texFormat;
        GLuint glType;
        const ChunkIndex chunkIndex;
    };







    class TextureDataProvider {
    public:

        TextureDataProvider();
        ~TextureDataProvider();


        std::shared_ptr<RawTileData> getTextureData(
            GDALDataset * dataSet, ChunkIndex chunkIndex, int tileLevelDifference);


        void asyncRequest(GDALDataset * dataSet, ChunkIndex chunkIndex, int tileLevelDifference);
        void updateAsyncRequests();
        bool hasTextureTileData() const;
        std::shared_ptr<RawTileData> nextTextureTile();

    private:






        //////////////////////////////////////////////////////////////////////////////////
        //                          HELPER STRUCTS                                      //
        //////////////////////////////////////////////////////////////////////////////////




        struct GdalDataRegion {

            GdalDataRegion(GDALDataset* dataSet, const ChunkIndex& chunkIndex,
                int tileLevelDifference);

            const ChunkIndex chunkIndex;

            glm::uvec2 pixelStart;
            glm::uvec2 pixelEnd;
            glm::uvec2 numPixels;

            size_t numRasters;

            int overview;

        };

        struct DataLayout {
            DataLayout(GDALDataset* dataSet, const GdalDataRegion& region);

            GDALDataType gdalType;
            size_t bytesPerDatum;
            size_t bytesPerPixel;
            size_t bytesPerLine;
            size_t totalNumBytes;

        };


        struct GdalAsyncRequest {
            GDALDataset* dataSet;
            GDALAsyncReader* asyncReader;
            const GdalDataRegion region;
            const DataLayout dataLayout;
            const char* imageData;
        };

        struct GdalAsyncRequestCompare {
            bool operator() (const GdalAsyncRequest& lhs, const GdalAsyncRequest& rhs) const {
                return lhs.region.chunkIndex.hashKey() < rhs.region.chunkIndex.hashKey();
            }
        };



        //////////////////////////////////////////////////////////////////////////////////
        //                             HELPER FUNCTIONS                                 //
        //////////////////////////////////////////////////////////////////////////////////


        

        static glm::uvec2 geodeticToPixel(GDALDataset* dataSet, const Geodetic2& geo);

        static GLuint getOpenGLDataType(GDALDataType gdalType);

        static RawTileData::TextureFormat getTextureFormat(int rasterCount, GDALDataType gdalType);

        static size_t numberOfBytes(GDALDataType gdalType);

        static std::shared_ptr<RawTileData> createRawTileData(const GdalDataRegion& region,
            const DataLayout& dataLayout, const char* imageData);


        //////////////////////////////////////////////////////////////////////////////////
        //                              MEMBER VARIABLES                                //
        //////////////////////////////////////////////////////////////////////////////////


        std::queue<std::shared_ptr<RawTileData>> loadedTextureTiles;
        std::set<GdalAsyncRequest, GdalAsyncRequestCompare> asyncRequests;

    };



} // namespace openspace





#endif  // __TEXTURE_DATA_PROVIDER_H__