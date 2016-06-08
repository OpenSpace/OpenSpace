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

#ifndef __TILE_DATASET_H__
#define __TILE_DATASET_H__

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

    struct TilePreprocessData {
        std::vector<float> maxValues;
        std::vector<float> minValues;
    };

    struct TextureFormat {
        Texture::Format ghoulFormat;
        GLuint glFormat;
    };

    struct TileIOResult {
        void* imageData;
        glm::uvec3 dimensions;
        std::shared_ptr<TilePreprocessData> preprocessData;
        ChunkIndex chunkIndex;
        CPLErr error;
    };



    struct TileDepthTransform {
        float depthScale;
        float depthOffset;
    };



    class TileDataset {
    public:

        
        /**
        * Opens a GDALDataset in readonly mode and calculates meta data required for 
        * reading tile using a ChunkIndex.
        *
        * \param gdalDatasetDesc  - A path to a specific file or raw XML describing the dataset 
        * \param minimumPixelSize - minimum number of pixels per side per tile requested 
        * \param datatype         - datatype for storing pixel data in requested tile
        */
        TileDataset(const std::string& gdalDatasetDesc, int minimumPixelSize, 
            bool doPreprocessing, GLuint dataType = 0);

        ~TileDataset();

        struct DataLayout {
            DataLayout();
            DataLayout(GDALDataset* dataSet, GLuint glType);

            GDALDataType gdalType;
            GLuint glType;


            size_t bytesPerDatum;
            size_t numRasters;
            size_t bytesPerPixel;

            TextureFormat textureFormat;
        };


       
        std::shared_ptr<TileIOResult> readTileData(ChunkIndex chunkIndex);

        int getMaximumLevel() const;

        TileDepthTransform getDepthTransform() const;

        const DataLayout& getDataLayout() const;


    private:

        struct GdalDataRegion {

            GdalDataRegion(GDALDataset* dataSet, const ChunkIndex& chunkIndex,
                int tileLevelDifference);

            const ChunkIndex chunkIndex;

            glm::uvec2 pixelStart;
            glm::uvec2 pixelEnd;
            glm::uvec2 numPixels;

            int overview;

        };





        //////////////////////////////////////////////////////////////////////////////////
        //                             HELPER FUNCTIONS                                 //
        //////////////////////////////////////////////////////////////////////////////////

        TileDepthTransform calculateTileDepthTransform();


        static int calculateTileLevelDifference(GDALDataset* dataset, int minimumPixelSize);

        static glm::uvec2 geodeticToPixel(GDALDataset* dataSet, const Geodetic2& geo);

        static GLuint getOpenGLDataType(GDALDataType gdalType);

        static GDALDataType getGdalDataType(GLuint glType);

        static TextureFormat getTextureFormat(int rasterCount, GDALDataType gdalType);

        static size_t numberOfBytes(GDALDataType gdalType);

        std::shared_ptr<TilePreprocessData> preprocess(const char* imageData,
            const GdalDataRegion& region, const DataLayout& dataLayout);

        typedef std::function<float(const char*)> ValueReader;
        static ValueReader getValueReader(GDALDataType gdalType);

        static float TileDataset::readFloat(GDALDataType gdalType, const char* src);


        static size_t getMaximumValue(GDALDataType gdalType);

        static char* getImageDataFlippedY(const GdalDataRegion& region,
            const DataLayout& dataLayout, const char* imageData);


        //////////////////////////////////////////////////////////////////////////////////
        //                              MEMBER VARIABLES                                //
        //////////////////////////////////////////////////////////////////////////////////

        static bool GdalHasBeenInitialized;

        int _minimumPixelSize;
        double _tileLevelDifference;

        TileDepthTransform _depthTransform;

        GDALDataset* _dataset;
        DataLayout _dataLayout;

        bool _doPreprocessing;
    };



} // namespace openspace





#endif  // __TILE_DATASET_H__