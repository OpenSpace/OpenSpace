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

#include <memory>
#include <set>
#include <queue>
#include <iostream>
#include <unordered_map>

#include "gdal_priv.h"

#include <ghoul/filesystem/file.h>
#include <ghoul/opengl/texture.h>

#include <modules/globebrowsing/tile/tileioresult.h>
#include <modules/globebrowsing/tile/tiledatatype.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/other/threadpool.h>

namespace openspace {
    using namespace ghoul::opengl;
    using namespace ghoul::filesystem;


    struct TileDataLayout {
        TileDataLayout();
        TileDataLayout(GDALDataset* dataSet, GLuint glType);

        GDALDataType gdalType;
        GLuint glType;

        size_t bytesPerDatum;
        size_t numRasters;
        size_t bytesPerPixel;

        TextureFormat textureFormat;
    };

    struct GdalDataRegion {
        GdalDataRegion(GDALDataset* dataSet, const ChunkIndex& chunkIndex, int tileLevelDifference);

        glm::uvec2 pixelStart;
        glm::uvec2 pixelEnd;
        glm::uvec2 numPixels;

        int overview;

        static glm::uvec2 geodeticToPixel(GDALDataset* dataSet, const Geodetic2& geo);

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

       
        std::shared_ptr<TileIOResult> readTileData(ChunkIndex chunkIndex);


        int getMaximumLevel() const;
        TileDepthTransform getDepthTransform() const;
        const TileDataLayout& getDataLayout() const;

        const static glm::ivec2 tilePixelStartOffset;
        const static glm::ivec2 tilePixelSizeDifference;


    private:

        

        //////////////////////////////////////////////////////////////////////////////////
        //                             HELPER FUNCTIONS                                 //
        //////////////////////////////////////////////////////////////////////////////////

        const int calculateMaxLevel(int tileLevelDifference);

        TileDepthTransform calculateTileDepthTransform();

        std::shared_ptr<TilePreprocessData> preprocess(const char* imageData,
            const GdalDataRegion& region, const TileDataLayout& dataLayout);


        static int calculateTileLevelDifference(GDALDataset* dataset, int minimumPixelSize);

        static char* getImageDataFlippedY(const GdalDataRegion& region,
            const TileDataLayout& dataLayout, const char* imageData);


        //////////////////////////////////////////////////////////////////////////////////
        //                              MEMBER VARIABLES                                //
        //////////////////////////////////////////////////////////////////////////////////

        static bool GdalHasBeenInitialized;

        int _maxLevel;
        double _tileLevelDifference;

        TileDepthTransform _depthTransform;

        GDALDataset* _dataset;
        TileDataLayout _dataLayout;

        bool _doPreprocessing;
    };



} // namespace openspace





#endif  // __TILE_DATASET_H__