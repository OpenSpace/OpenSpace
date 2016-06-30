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

    typedef glm::ivec2 PixelCoordinate;

    struct PixelRegion {

        PixelRegion() : start(0), numPixels(0) { }
        PixelRegion(const PixelRegion& o) : start(o.start), numPixels(o.numPixels) { }
        PixelRegion(const PixelCoordinate& pixelStart, const PixelCoordinate& numberOfPixels)
            : start(pixelStart), numPixels(numberOfPixels) { }


        PixelCoordinate start;
        PixelCoordinate numPixels;

        void pad(const PixelRegion& padding) {
            start += padding.start;
            numPixels += padding.numPixels;
        }

        void clampTo(const PixelRegion& boundingRegion) {
            start = glm::max(start, boundingRegion.start);
            numPixels = glm::min(end(), boundingRegion.end()) - start;
        }

        PixelCoordinate end() const {
            return start + numPixels;
        }

        void scale(const glm::dvec2& s) {
            start = PixelCoordinate(glm::round(s * glm::dvec2(start)));
            numPixels = PixelCoordinate(glm::round(s * glm::dvec2(numPixels)));
        }

        void scale(double s) {
            scale(glm::dvec2(s));
        }

        void downscalePow2(int exponent) {
            start.x >>= exponent;
            start.y >>= exponent;
            numPixels.x >>= exponent;
            numPixels.y >>= exponent;
        }

        void upscalePow2(int exponent) {
            start.x <<= exponent;
            start.y <<= exponent;
            numPixels.x <<= exponent;
            numPixels.y <<= exponent;
        }
    };


    struct IODescription {
        struct ReadData {
            int overview;
            PixelRegion region;
        } read;

        struct WriteData {
            PixelRegion region; // should always start at 0,0
            size_t bytesPerLine;
            size_t totalNumBytes;
        } write;
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


        int maxChunkLevel();
        TileDepthTransform getDepthTransform() const;
        const TileDataLayout& getDataLayout() const;

        const static glm::ivec2 tilePixelStartOffset;
        const static glm::ivec2 tilePixelSizeDifference;
        const static PixelRegion padding; // same as the two above
      


    private:

        IODescription getIODescription(const ChunkIndex& chunkIndex);
        

        //////////////////////////////////////////////////////////////////////////////////
        //                           GDAL HELPER FUNCTIONS                              //
        //////////////////////////////////////////////////////////////////////////////////

        PixelRegion gdalPixelRegion(const GeodeticPatch& geodeticPatch) const;
        int gdalOverview(const PixelCoordinate& baseRegionSize) const;
        int gdalOverview(const ChunkIndex& chunkIndex) const;
        bool gdalHasOverviews() const;
        PixelRegion gdalPixelRegion(GDALRasterBand* rasterBand) const;
        GDALRasterBand* gdalRasterBand(int overview, int raster = 1) const;


        //////////////////////////////////////////////////////////////////////////////////
        //                                Initialization                                //
        //////////////////////////////////////////////////////////////////////////////////
        TileDepthTransform calculateTileDepthTransform();
        int calculateTileLevelDifference(int minimumPixelSize);


        //////////////////////////////////////////////////////////////////////////////////
        //                          ReadTileData helper functions                       //
        //////////////////////////////////////////////////////////////////////////////////
        PixelCoordinate geodeticToPixel(const Geodetic2& geo) const;
        char* getImageDataFlippedY(const PixelRegion& region, const char* imageData);
        std::shared_ptr<TilePreprocessData> preprocess(const char* imageData, const PixelRegion& region);


        //////////////////////////////////////////////////////////////////////////////////
        //                              MEMBER VARIABLES                                //
        //////////////////////////////////////////////////////////////////////////////////


        int _maxLevel;
        double _tileLevelDifference;

        TileDepthTransform _depthTransform;

        GDALDataset* _dataset;
        TileDataLayout _dataLayout;

        bool _doPreprocessing;

        static bool GdalHasBeenInitialized;

    };



} // namespace openspace





#endif  // __TILE_DATASET_H__