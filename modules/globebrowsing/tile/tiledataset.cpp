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
#include <ogr_spatialref.h>

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

    std::ostream& operator<<(std::ostream& os, const openspace::PixelRegion& pr) {
        return os << pr.start.x << ", " << pr.start.y << " with size " << pr.numPixels.x << ", " << pr.numPixels.y;
    }

    //////////////////////////////////////////////////////////////////////////////////
    //                             Tile Data Layout                                 //
    //////////////////////////////////////////////////////////////////////////////////

    TileDataLayout::TileDataLayout() {

    }

    TileDataLayout::TileDataLayout(GDALDataset* dataSet, GLuint _glType) {
        // Assume all raster bands have the same data type
        gdalType = _glType != 0 ? TileDataType::getGdalDataType(glType) : dataSet->GetRasterBand(1)->GetRasterDataType();
        glType = TileDataType::getOpenGLDataType(gdalType);
        numRasters = dataSet->GetRasterCount();
        bytesPerDatum = TileDataType::numberOfBytes(gdalType);
        bytesPerPixel = bytesPerDatum * numRasters;
        textureFormat = TileDataType::getTextureFormat(numRasters, gdalType);
    }
  






    //////////////////////////////////////////////////////////////////////////////////
    //                               Tile Dataset                                   //
    //////////////////////////////////////////////////////////////////////////////////

    const glm::ivec2 TileDataset::tilePixelStartOffset = glm::ivec2(-2);
    const glm::ivec2 TileDataset::tilePixelSizeDifference = glm::ivec2(4);

    const PixelRegion TileDataset::padding = PixelRegion(tilePixelStartOffset, tilePixelSizeDifference);


    bool TileDataset::GdalHasBeenInitialized = false;

    TileDataset::TileDataset(const std::string& gdalDatasetDesc, int minimumPixelSize,
        bool doPreprocessing, GLuint dataType)
        : _doPreprocessing(doPreprocessing)
        , _maxLevel(-1)
    {
        // 1. First make sure GDAL has been initialized
        if (!GdalHasBeenInitialized) {
            GDALAllRegister();
            CPLSetConfigOption("GDAL_DATA", absPath("${MODULE_GLOBEBROWSING}/gdal_data").c_str());
            GdalHasBeenInitialized = true;
        }

        //2. Secondly, open the GDAL dataset. Other methods depends on this
        _dataset = (GDALDataset *)GDALOpen(gdalDatasetDesc.c_str(), GA_ReadOnly);
        if (!_dataset) {
            throw ghoul::RuntimeError("Failed to load dataset:\n" + gdalDatasetDesc);
        }

        //3. Do any other initialization needed for the TileDataset
        _dataLayout = TileDataLayout(_dataset, dataType);
        _depthTransform = calculateTileDepthTransform();
        _tileLevelDifference = calculateTileLevelDifference(minimumPixelSize);

        LDEBUG(gdalDatasetDesc << " - " << _tileLevelDifference);
    }


    TileDataset::~TileDataset() {
        delete _dataset;
    }




    //////////////////////////////////////////////////////////////////////////////////
    //                              Public interface                                //
    //////////////////////////////////////////////////////////////////////////////////

    std::shared_ptr<TileIOResult> TileDataset::readTileData(ChunkIndex chunkIndex) {
        IODescription io = getIODescription(chunkIndex);
        CPLErr worstError = CPLErr::CE_None;

        // Build the Tile IO Result from the data we queried
        std::shared_ptr<TileIOResult> result = std::make_shared<TileIOResult>();
        result->imageData = readImageData(io, worstError);
        result->error = worstError;
        result->chunkIndex = chunkIndex;
        result->dimensions = glm::uvec3(io.write.region.numPixels, 1);
        result->nBytesImageData = io.write.totalNumBytes;
        
        if (_doPreprocessing) {
            result->preprocessData = preprocess(result, io.write.region);
            result->error = std::max(result->error, postProcessErrorCheck(result, io));
        }

        return result;
    }

    int TileDataset::maxChunkLevel() {
        if (_maxLevel < 0) {
            int numOverviews = _dataset->GetRasterBand(1)->GetOverviewCount();
            _maxLevel = -_tileLevelDifference;
            if (numOverviews > 0) {
                _maxLevel += numOverviews - 1;
            }
        }
        return _maxLevel;
    }

    TileDepthTransform TileDataset::getDepthTransform() const {
        return _depthTransform;
    }

    const TileDataLayout& TileDataset::getDataLayout() const {
        return _dataLayout;
    }





    //////////////////////////////////////////////////////////////////////////////////
    //                                Initialization                                //
    //////////////////////////////////////////////////////////////////////////////////

    int TileDataset::calculateTileLevelDifference(int minimumPixelSize) {
        GDALRasterBand* firstBand = _dataset->GetRasterBand(1);
        GDALRasterBand* maxOverview;
        int numOverviews = firstBand->GetOverviewCount();
        int sizeLevel0;
        if (numOverviews <= 0) { // No overviews. Use first band.
            maxOverview = firstBand;
        }
        else { // Pick the highest overview.
            maxOverview = firstBand->GetOverview(numOverviews - 1);
        }
        sizeLevel0 = maxOverview->GetXSize();
        double diff = log2(minimumPixelSize) - log2(sizeLevel0);
        return diff;
    }

    TileDepthTransform TileDataset::calculateTileDepthTransform() {
        GDALRasterBand* firstBand = _dataset->GetRasterBand(1);
        // Floating point types does not have a fix maximum or minimum value and
        // can not be normalized when sampling a texture. Hence no rescaling is needed.
        bool isFloat = (_dataLayout.gdalType == GDT_Float32 || _dataLayout.gdalType == GDT_Float64);
        double maximumValue = isFloat ? 1.0 : TileDataType::getMaximumValue(_dataLayout.gdalType);

        TileDepthTransform transform;
        transform.depthOffset = firstBand->GetOffset();
        transform.depthScale = firstBand->GetScale() * maximumValue;
        return transform;
    }





    //////////////////////////////////////////////////////////////////////////////////
    //                            GDAL helper methods                               //
    //////////////////////////////////////////////////////////////////////////////////


    bool TileDataset::gdalHasOverviews() const {
        return _dataset->GetRasterBand(1)->GetOverviewCount() > 0;
    }

    int TileDataset::gdalOverview(const PixelCoordinate& regionSizeOverviewZero) const {
        GDALRasterBand* firstBand = _dataset->GetRasterBand(1);

        int minNumPixels0 = glm::min(regionSizeOverviewZero.x, regionSizeOverviewZero.y);

        int overviews = firstBand->GetOverviewCount();
        GDALRasterBand* maxOverview = overviews ? firstBand->GetOverview(overviews - 1) : firstBand;
        
        int sizeLevel0 = maxOverview->GetXSize();
        // The dataset itself may not have overviews but even if it does not, an overview
        // for the data region can be calculated and possibly be used to sample greater
        // Regions of the original dataset.
        int ov = std::log2(minNumPixels0) - std::log2(sizeLevel0 + 1) - _tileLevelDifference;
        ov = glm::clamp(ov, 0, overviews - 1);
        
        return ov;
    }

    int TileDataset::gdalOverview(const ChunkIndex& chunkIndex) const {
        int overviews = _dataset->GetRasterBand(1)->GetOverviewCount();
        int ov = overviews - (chunkIndex.level + _tileLevelDifference + 1);
        return glm::clamp(ov, 0, overviews - 1);
    }

    PixelRegion TileDataset::gdalPixelRegion(GDALRasterBand* rasterBand) const {
        PixelRegion gdalRegion;
        gdalRegion.start.x = 0;
        gdalRegion.start.y = 0;
        gdalRegion.numPixels.x = rasterBand->GetXSize();
        gdalRegion.numPixels.y = rasterBand->GetYSize();
        return gdalRegion;
    }

    PixelRegion TileDataset::gdalPixelRegion(const GeodeticPatch& geodeticPatch) const {
        Geodetic2 nwCorner = geodeticPatch.getCorner(Quad::NORTH_WEST);
        Geodetic2 swCorner = geodeticPatch.getCorner(Quad::SOUTH_EAST);
        PixelCoordinate pixelStart = geodeticToPixel(nwCorner);
        PixelCoordinate pixelEnd = geodeticToPixel(swCorner);
        PixelRegion gdalRegion(pixelStart, pixelEnd - pixelStart);
        return gdalRegion;
    }

    GDALRasterBand* TileDataset::gdalRasterBand(int overview, int raster) const {
        GDALRasterBand* rasterBand = _dataset->GetRasterBand(raster);
        return gdalHasOverviews() ? rasterBand->GetOverview(overview) : rasterBand;
    }





    //////////////////////////////////////////////////////////////////////////////////
    //                          ReadTileData helper functions                       //
    //////////////////////////////////////////////////////////////////////////////////

    PixelCoordinate TileDataset::geodeticToPixel(const Geodetic2& geo) const {

        double padfTransform[6];
        CPLErr err = _dataset->GetGeoTransform(padfTransform);

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
        double divisor = (a[2] * b[1] - a[1] * b[2]);
        ghoul_assert(divisor != 0.0, "Division by zero!");
        //ghoul_assert(a[2] != 0.0, "a2 must not be zero!");
        double P = (a[0] * b[2] - a[2] * b[0] + a[2] * Y - b[2] * X) / divisor;
        double L = (-a[0] * b[1] + a[1] * b[0] - a[1] * Y + b[1] * X) / divisor;
        // ref: https://www.wolframalpha.com/input/?i=X+%3D+a0+%2B+a1P+%2B+a2L,+Y+%3D+b0+%2B+b1P+%2B+b2L,+solve+for+P+and+L

        double Xp = a[0] + P*a[1] + L*a[2];
        double Yp = b[0] + P*b[1] + L*b[2];

        ghoul_assert(abs(X - Xp) < 1e-10, "inverse should yield X as before");
        ghoul_assert(abs(Y - Yp) < 1e-10, "inverse should yield Y as before");

        return PixelCoordinate(glm::round(P), glm::round(L));
    }

    IODescription TileDataset::getIODescription(const ChunkIndex& chunkIndex) const {
        // Calculate suitable overview and corresponding pixel region
        int overview = gdalOverview(chunkIndex);
        PixelRegion region = gdalPixelRegion(chunkIndex); // pixel region at overview zero
        region.downscalePow2(overview + 1); // pixel region at suitable overview 

        // Create an IORegion based on that overview pixel region
        IODescription io;
        io.read.overview = overview;
        io.read.region = region;
        io.write.region = region;

        // Handle the case where the dataset does not have overviews
        if (!gdalHasOverviews()) {
            io.read.region.upscalePow2(overview + 1);
            io.read.overview = 0; // no overview
        }

        // For correct sampling in height dataset, we need to pad the texture tile
        io.read.region.pad(padding);
        io.write.region.pad(padding);
        io.write.region.start = PixelCoordinate(0, 0); // write region starts in origin
        
        

        // Doing this may cause invalid regions, i.e. having negative pixel coordinates 
        // or being too large etc. For now, just clamp 
        PixelRegion overviewRegion = gdalPixelRegion(gdalRasterBand(overview));

        //if (chunkIndex.level < 3) {
        //    io.read.region.clampTo(overviewRegion);
        //    io.write.region.clampTo(overviewRegion);
        //}
        

        io.write.bytesPerLine = _dataLayout.bytesPerPixel * io.write.region.numPixels.x;
        io.write.totalNumBytes = io.write.bytesPerLine * io.write.region.numPixels.y;

        return io;
    }

    char* TileDataset::readImageData(IODescription& io, CPLErr& worstError) const {
        // allocate memory for the image
        char* imageData = new char[io.write.totalNumBytes];

        // Read the data (each rasterband is a separate channel)
        for (size_t i = 0; i < _dataLayout.numRasters; i++) {
            GDALRasterBand* rasterBand = gdalRasterBand(io.read.overview, i + 1);
            
            // The final destination pointer is offsetted by one datum byte size
            // for every raster (or data channel, i.e. R in RGB)
            char* dataDestination = imageData + (i * _dataLayout.bytesPerDatum);

            CPLErr err = repeatedRasterIO(rasterBand, io, dataDestination);

            // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
            worstError = std::max(worstError, err);
        }

        // GDAL reads pixel lines top to bottom, we want the opposit
        return imageData;
    }

    CPLErr TileDataset::repeatedRasterIO(GDALRasterBand* rasterBand, const IODescription& fullIO, char* dataDestination) const {
        CPLErr worstError = CPLErr::CE_None;

        // NOTE: 
        // Ascii graphics illustrates the implementation details of this method, for one  
        // specific case. Even though the illustrated case is specific, readers can 
        // hopefully find it useful to get the general idea.

        // Make a copy of the full IO desription as we will have to modify it
        IODescription io = fullIO;
        PixelRegion gdalRegion = gdalPixelRegion(rasterBand);


        // Example: 
        // We have an io description that defines a WRITE and a READ region.
        // In this case the READ region extends outside of the defined gdal region,
        // meaning we will have to do wrapping


        // io.write.region             io.read.region
        //    |                         |
        //    V                         V
        // +-------+                +-------+ 
        // |       |                |       |--------+ 
        // |       |                |       |        |
        // |       |                |       |        |
        // +-------+                +-------+        |
        //                            |              | <-- gdalRegion  
        //                            |              |
        //                            +--------------+


        LDEBUG("-");
        LDEBUG("repeated read: " << io.read.region);
        LDEBUG("repeated write: " << io.write.region);

        bool didCutOff = false;

        if (!io.read.region.isInside(gdalRegion)) {
            //  Loop through each side: left, top, right, bottom
            for (int i = 0; i < 4; ++i) {

                // Example: 
                // We are currently considering the left side of the pixel region
                PixelRegion::Side side = (PixelRegion::Side) i;
                IODescription cutoff = io.cut(side, gdalRegion.edge(side));

                // Example: 
                // We cut off the left part that was outside the gdal region, and we now 
                // have an additional io description for the cut off region. 
                // Note that the cut-method used above takes care of the corresponding 
                // WRITE region for us.

                // cutoff.write.region    cutoff.read.region
                //  |  io.write.region     |  io.read.region
                //  |   |                  |   |
                //  V   V                  V   V
                // +-+-----+               +-+-----+ 
                // | |     |               | |     |--------+
                // | |     |               | |     |        |
                // | |     |               | |     |        |
                // +-+-----+               +-+-----+        |
                //                           |              | <-- gdalRegion  
                //                           |              |
                //                           +--------------+

                if (cutoff.read.region.area() > 0) {
                    didCutOff = true;

                    // Wrap by repeating
                    PixelRegion::Side oppositeSide = (PixelRegion::Side) ((i + 2) % 4);

                    cutoff.read.region.align(oppositeSide, gdalRegion.edge(oppositeSide));

                    // Example:
                    // The cut off region is wrapped to the opposite side of the region,
                    // i.e. "repeated". Note that we don't want WRITE region to change, 
                    // we're only wrapping the READ region.

                    // cutoff.write.region   io.read.region cutoff.read.region
                    //  |  io.write.region        |          |
                    //  |   |                     V          V
                    //  V   V                  +-----+      +-+
                    // +-+-----+               |     |------| |
                    // | |     |               |     |      | | 
                    // | |     |               |     |      | |
                    // | |     |               +-----+      +-+
                    // +-+-----+               |              | <-- gdalRegion  
                    //                         |              |
                    //                         +--------------+

                    // Example:
                    // The cutoff region has been repeated along one of its sides, but 
                    // as we can see in this example, it still has a top part outside the
                    // defined gdal region. This is handled through recursion.
                    CPLErr err = repeatedRasterIO(rasterBand, cutoff, dataDestination);

                    worstError = std::max(worstError, err);
                }
            }
        }

       

        CPLErr err = rasterIO(rasterBand, io, dataDestination);
        worstError = std::max(worstError, err);
        return worstError;
    }

    CPLErr TileDataset::rasterIO(GDALRasterBand* rasterBand, const IODescription& io, char* dataDestination) const {
        PixelRegion gdalRegion = gdalPixelRegion(rasterBand);

        LDEBUG("rasterIO read: " << io.read.region);
        LDEBUG("rasterIO write: " << io.write.region);

        ghoul_assert(io.read.region.isInside(gdalRegion), "write region of bounds!");

        ghoul_assert(io.write.region.start.x >= 0 && io.write.region.start.y >= 0, "Invalid write region");

        PixelCoordinate end = io.write.region.end();
        size_t largestIndex = (end.y - 1) * io.write.bytesPerLine + (end.x - 1) * _dataLayout.bytesPerPixel;
        ghoul_assert(largestIndex <= io.write.totalNumBytes, "Invalid write region");

        char * dataDest = dataDestination;

        // OBS! GDAL reads pixels top to bottom, but we want our pixels bottom to top.
        // Therefore, we increment the destination pointer to the last line on in the 
        // buffer, and the we specify in the rasterIO call that we want negative line 
        // spacing. Doing this compensates the flipped Y axis
        dataDest += (io.write.totalNumBytes - io.write.bytesPerLine);

        // handle requested write region
        dataDest -= io.write.region.start.y * io.write.bytesPerLine; // note -= since flipped y axis
        dataDest += io.write.region.start.x * _dataLayout.bytesPerPixel;

        
        return rasterBand->RasterIO(
            GF_Read,
            io.read.region.start.x,             // Begin read x
            io.read.region.start.y,             // Begin read y
            io.read.region.numPixels.x,         // width to read x
            io.read.region.numPixels.y,         // width to read y
            dataDest,                           // Where to put data
            io.write.region.numPixels.x,        // width to write x in destination
            io.write.region.numPixels.y,        // width to write y in destination
            _dataLayout.gdalType,		        // Type
            _dataLayout.bytesPerPixel,	        // Pixel spacing
            -io.write.bytesPerLine);             // Line spacing
    }


    std::shared_ptr<TilePreprocessData> TileDataset::preprocess(std::shared_ptr<TileIOResult> result, const PixelRegion& region) const {
        size_t bytesPerLine = _dataLayout.bytesPerPixel * region.numPixels.x;
        size_t totalNumBytes = bytesPerLine * region.numPixels.y;

        TilePreprocessData* preprocessData = new TilePreprocessData();
        preprocessData->maxValues.resize(_dataLayout.numRasters);
        preprocessData->minValues.resize(_dataLayout.numRasters);

        for (size_t c = 0; c < _dataLayout.numRasters; c++) {
            preprocessData->maxValues[c] = -FLT_MAX;
            preprocessData->minValues[c] = FLT_MAX;
        }

        for (size_t y = 0; y < region.numPixels.y; y++) {
            size_t yi_flipped = y * bytesPerLine;
            size_t yi = (region.numPixels.y - 1 - y) * bytesPerLine;
            size_t i = 0;
            for (size_t x = 0; x < region.numPixels.x; x++) {
                for (size_t c = 0; c < _dataLayout.numRasters; c++) {

                    float val = TileDataType::interpretFloat(_dataLayout.gdalType, &(result->imageData[yi + i]));
                    preprocessData->maxValues[c] = std::max(val, preprocessData->maxValues[c]);
                    preprocessData->minValues[c] = std::min(val, preprocessData->minValues[c]);

                    i += _dataLayout.bytesPerDatum;
                }
            }
        }

        for (size_t c = 0; c < _dataLayout.numRasters; c++) {
            if (preprocessData->maxValues[c] > 8800.0f) {
                //LDEBUG("Bad preprocess data: " << preprocessData->maxValues[c] << " at " << region.chunkIndex);
            }
        }

        return std::shared_ptr<TilePreprocessData>(preprocessData);
    }

    CPLErr TileDataset::postProcessErrorCheck(std::shared_ptr<const TileIOResult> result, const IODescription& io) const{
        int success;
        double missingDataValue = gdalRasterBand(io.read.overview)->GetNoDataValue(&success);
        if (!success) {
            missingDataValue = 32767; // missing data value for TERRAIN.wms. Should be specified in xml
        }

        bool hasMissingData = false;
        for (size_t c = 0; c < _dataLayout.numRasters; c++) {
            hasMissingData |= result->preprocessData->maxValues[c] == missingDataValue;
        }
        bool onHighLevel = result->chunkIndex.level > 6;
        if (hasMissingData && onHighLevel) {
            return CE_Fatal;
        }
        return CE_None;
    }


}  // namespace openspace
