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

        enum Side {
            LEFT = 0, TOP, RIGHT, BOTTOM,
        };

        PixelRegion() : start(0), numPixels(0) { }
        PixelRegion(const PixelRegion& o) : start(o.start), numPixels(o.numPixels) { }
        PixelRegion(const PixelCoordinate& pixelStart, const PixelCoordinate& numberOfPixels)
            : start(pixelStart), numPixels(numberOfPixels) { }

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

        void resizeToStartAt(const PixelCoordinate& c) {
            numPixels -= (c - start);
            start = c;
        }

        void setLeft(int x) {
            numPixels.x += (start.x - x);
            start.x = x;
        }

        void setTop(int p) {
            numPixels.y += (start.y - p);
            start.y = p;
        }

        void setRight(int x) {
            numPixels.x = x - start.x;
        }

        void setBottom(int y) {
            numPixels.y = y - start.y;
        }



        void alignLeft(int x) { start.x = x; }
        void alignTop(int y) { start.y = y; }
        void alignRight(int x) { start.x = x - numPixels.x; }
        void alignBottom(int y) { start.y = y - numPixels.y; }

        void scale(const glm::dvec2& s) {
            start = PixelCoordinate(glm::round(s * glm::dvec2(start)));
            numPixels = PixelCoordinate(glm::round(s * glm::dvec2(numPixels)));
        }

        void scale(double s) {
            scale(glm::dvec2(s));
        }

        int area() {
            return numPixels.x * numPixels.y;
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

        int edge(Side side) const {
            switch (side) {
            case LEFT: return start.x;
            case TOP: return start.y;
            case RIGHT: return start.x + numPixels.x;
            case BOTTOM: return start.y + numPixels.y;
            }
        }

        int edgeDirectionSign(Side side) const {
            return side < RIGHT ? -1 : 1;
        }

        void align(Side side, int pos) {
            switch (side) {
            case LEFT: alignLeft(pos); break;
            case TOP: alignTop(pos); break;
            case RIGHT: alignRight(pos); break;
            case BOTTOM: alignBottom(pos); break;
            }
        }

        void move(Side side, int amount) {
            switch (side) {
            case LEFT: start.x -= amount; break;
            case TOP: start.y -= amount; break;
            case RIGHT: start.x += amount; break;
            case BOTTOM: start.y += amount; break;
            }
        }

        void setSide(Side side, int pos) {
            switch (side) {
            case LEFT: setLeft(pos); break;
            case TOP: setTop(pos); break;
            case RIGHT: setRight(pos); break;
            case BOTTOM: setBottom(pos); break;
            }
        }

        bool lineIntersect(Side side, int p) {
            switch (side)
            {
            case openspace::PixelRegion::LEFT:
            case openspace::PixelRegion::RIGHT:
                return start.x <= p && p <= (start.x + numPixels.x);
                
            case openspace::PixelRegion::TOP:
            case openspace::PixelRegion::BOTTOM:
                return start.y <= p && p <= (start.y + numPixels.y);
            }
        }

        bool isInside(const PixelRegion& r) const {
            PixelCoordinate e = end();
            PixelCoordinate re = r.end();
            return r.start.x <= start.x && e.x <= re.x
                && r.start.y <= start.y && e.y <= re.y;
        }

        PixelRegion cut(Side side, int p) {
            if (!lineIntersect(side, p)) {
                return PixelRegion({ 0, 0 }, {0, 0});
            }

            PixelRegion cutOff(*this);
            int cutSize = 0;
            switch (side) {
            case LEFT:
                setLeft(p);
                cutOff.setRight(p - cutSize);
                break;
            case TOP:
                setTop(p);
                cutOff.setBottom(p - cutSize);
                break;
            case RIGHT:
                setRight(p);
                cutOff.setLeft(p + cutSize);
                break;
            case BOTTOM:
                setBottom(p);
                cutOff.setTop(p + cutSize);
                break;
            }
            return cutOff;
        }

        PixelRegion cutDelta(Side side, int p) {
            if (p < 1) {
                return PixelRegion({ 0, 0 }, { 0, 0 });
            }
            else return cut(side, edge(side) - edgeDirectionSign(side) * p);
        }

        bool equals(const PixelRegion& r) const {
            return start == r.start && numPixels == r.numPixels;
        }

        PixelCoordinate start;
        PixelCoordinate numPixels;
    };

    


    struct IODescription {
        IODescription cut(PixelRegion::Side side, int pos) {
            PixelRegion readPreCut = read.region;
            PixelRegion writePreCut = write.region;

            IODescription whatCameOff = *this;
            whatCameOff.read.region = read.region.cut(side, pos);

            PixelCoordinate cutSize = whatCameOff.read.region.numPixels;
            int cutWidth = (side % 2 == 0) ? cutSize.x : cutSize.y;
            whatCameOff.write.region = write.region.cutDelta(side, cutWidth);
            

            if (cutWidth == 0) {
                ghoul_assert(read.region.equals(readPreCut), "Read region should not have been modified");
                ghoul_assert(write.region.equals(writePreCut), "Write region should not have been modified");
            }
            
            return whatCameOff;
        }

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


        //////////////////////////////////////////////////////////////////////////////////
        //                              Public interface                                //
        //////////////////////////////////////////////////////////////////////////////////
        std::shared_ptr<TileIOResult> readTileData(ChunkIndex chunkIndex);
        int maxChunkLevel();
        TileDepthTransform getDepthTransform() const;
        const TileDataLayout& getDataLayout() const;


        const static glm::ivec2 tilePixelStartOffset;
        const static glm::ivec2 tilePixelSizeDifference;
        const static PixelRegion padding; // same as the two above


    private:

        //////////////////////////////////////////////////////////////////////////////////
        //                                Initialization                                //
        //////////////////////////////////////////////////////////////////////////////////

        TileDepthTransform calculateTileDepthTransform();
        int calculateTileLevelDifference(int minimumPixelSize);


        //////////////////////////////////////////////////////////////////////////////////
        //                            GDAL helper methods                               //
        //////////////////////////////////////////////////////////////////////////////////

        bool gdalHasOverviews() const;
        int gdalOverview(const PixelCoordinate& baseRegionSize) const;
        int gdalOverview(const ChunkIndex& chunkIndex) const;
        PixelRegion gdalPixelRegion(const GeodeticPatch& geodeticPatch) const;
        PixelRegion gdalPixelRegion(GDALRasterBand* rasterBand) const;
        GDALRasterBand* gdalRasterBand(int overview, int raster = 1) const;


        //////////////////////////////////////////////////////////////////////////////////
        //                          ReadTileData helper functions                       //
        //////////////////////////////////////////////////////////////////////////////////

        PixelCoordinate geodeticToPixel(const Geodetic2& geo) const;
        IODescription getIODescription(const ChunkIndex& chunkIndex) const;
        char* readImageData(IODescription& io, CPLErr& worstError) const;
        CPLErr rasterIO(GDALRasterBand* rasterBand, const IODescription& io, char* dst) const;
        CPLErr repeatedRasterIO(GDALRasterBand* rasterBand, const IODescription& io, char* dst) const;
        std::shared_ptr<TilePreprocessData> preprocess(std::shared_ptr<TileIOResult> result, const PixelRegion& region) const;
        CPLErr postProcessErrorCheck(std::shared_ptr<const TileIOResult> ioResult, const IODescription& io) const;



        //////////////////////////////////////////////////////////////////////////////////
        //                              Member variables                                //
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