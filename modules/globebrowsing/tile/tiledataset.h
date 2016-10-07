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


#include <ghoul/filesystem/file.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/misc/threadpool.h>

#include <modules/globebrowsing/tile/tiledatatype.h>
#include <modules/globebrowsing/tile/pixelregion.h>
#include <modules/globebrowsing/geometry/geodetic2.h>


namespace openspace {

    struct TileDataLayout {
        TileDataLayout();
        TileDataLayout(GDALDataset* dataSet, GLuint preferredGlType);

        GDALDataType gdalType;
        GLuint glType;

        size_t bytesPerDatum;
        size_t numRasters;
        size_t bytesPerPixel;

        TextureFormat textureFormat;
    };

    struct IODescription {
        
        struct ReadData {
            int overview;
            PixelRegion region;
        } read;

        struct WriteData {
            PixelRegion region;
            size_t bytesPerLine; 
            size_t totalNumBytes;
        } write;


        IODescription cut(PixelRegion::Side side, int pos);
    };



    using namespace ghoul::opengl;
    using namespace ghoul::filesystem;


    class TileDataset {
    public:

        struct Configuration {
            bool doPreProcessing;
            int minimumTilePixelSize;
            GLuint dataType = 0; // default = no datatype reinterpretation
        };

        
        /**
        * Opens a GDALDataset in readonly mode and calculates meta data required for 
        * reading tile using a ChunkIndex.
        *
        * \param gdalDatasetDesc  - A path to a specific file or raw XML describing the dataset 
        * \param minimumPixelSize - minimum number of pixels per side per tile requested 
        * \param datatype         - datatype for storing pixel data in requested tile
        */
        TileDataset(const std::string& gdalDatasetDesc, const Configuration& config);

        ~TileDataset();


        //////////////////////////////////////////////////////////////////////////////////
        //                              Public interface                                //
        //////////////////////////////////////////////////////////////////////////////////
        std::shared_ptr<TileIOResult> readTileData(ChunkIndex chunkIndex);
        std::shared_ptr<TileIOResult> defaultTileData();
        int maxChunkLevel();
        TileDepthTransform getDepthTransform();
        const TileDataLayout& getDataLayout();
        void reset();


        const static glm::ivec2 tilePixelStartOffset;
        const static glm::ivec2 tilePixelSizeDifference;
        const static PixelRegion padding; // same as the two above


    private:

        //////////////////////////////////////////////////////////////////////////////////
        //                                Initialization                                //
        //////////////////////////////////////////////////////////////////////////////////

        void initialize();
        void ensureInitialized();
        TileDepthTransform calculateTileDepthTransform();
        int calculateTileLevelDifference(int minimumPixelSize);


        //////////////////////////////////////////////////////////////////////////////////
        //                            GDAL helper methods                               //
        //////////////////////////////////////////////////////////////////////////////////

        void gdalEnsureInitialized();
        void setGdalProxyConfiguration();
        GDALDataset* gdalDataset(const std::string& gdalDatasetDesc);
        bool gdalHasOverviews() const;
        int gdalOverview(const PixelRange& baseRegionSize) const;
        int gdalOverview(const ChunkIndex& chunkIndex) const;
        int gdalVirtualOverview(const ChunkIndex& chunkIndex) const;
        PixelRegion gdalPixelRegion(const GeodeticPatch& geodeticPatch) const;
        PixelRegion gdalPixelRegion(GDALRasterBand* rasterBand) const;
        GDALRasterBand* gdalRasterBand(int overview, int raster = 1) const;


        //////////////////////////////////////////////////////////////////////////////////
        //                          ReadTileData helper functions                       //
        //////////////////////////////////////////////////////////////////////////////////

        PixelCoordinate geodeticToPixel(const Geodetic2& geo) const;
        Geodetic2 pixelToGeodetic(const PixelCoordinate& p) const;
        IODescription getIODescription(const ChunkIndex& chunkIndex) const;
        char* readImageData(IODescription& io, CPLErr& worstError) const;
        CPLErr rasterIO(GDALRasterBand* rasterBand, const IODescription& io, char* dst) const;
        CPLErr repeatedRasterIO(GDALRasterBand* rasterBand, const IODescription& io, char* dst, int depth = 0) const;
        std::shared_ptr<TilePreprocessData> preprocess(std::shared_ptr<TileIOResult> result, const PixelRegion& region) const;
        CPLErr postProcessErrorCheck(std::shared_ptr<const TileIOResult> ioResult, const IODescription& io) const;



        //////////////////////////////////////////////////////////////////////////////////
        //                              Member variables                                //
        //////////////////////////////////////////////////////////////////////////////////

        // init data
        struct InitData {
            std::string initDirectory;
            std::string gdalDatasetDesc;
            int minimumPixelSize;
            GLuint dataType;
        } _initData;
        
        struct Cached {
            int _maxLevel = -1;
            double _tileLevelDifference;
        } _cached;

        const Configuration _config;


        GDALDataset* _dataset;
        TileDepthTransform _depthTransform;
        TileDataLayout _dataLayout;

        static bool GdalHasBeenInitialized;
        bool hasBeenInitialized;
    };



} // namespace openspace



#endif  // __TILE_DATASET_H__