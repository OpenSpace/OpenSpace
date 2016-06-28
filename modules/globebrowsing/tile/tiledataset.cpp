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
    //                             GDAL Data Region                                 //
    //////////////////////////////////////////////////////////////////////////////////

    GdalDataRegion::GdalDataRegion(GDALDataset * dataSet,
        const ChunkIndex& chunkIndex, int tileLevelDifference) {

        GDALRasterBand* firstBand = dataSet->GetRasterBand(1);

        // Assume all raster bands have the same data type

        // Level = overviewCount - overview (default, levels may be overridden)
        int numOverviews = firstBand->GetOverviewCount();


        // Generate a patch from the chunkIndex, extract the bounds which
        // are used to calculated where in the GDAL data set to read data. 
        // pixelStart0 and pixelEnd0 defines the interval in the pixel space 
        // at overview 0
        GeodeticPatch patch = GeodeticPatch(chunkIndex);

        glm::uvec2 pixelStart0 = geodeticToPixel(dataSet, patch.getCorner(Quad::NORTH_WEST));
        glm::uvec2 pixelEnd0 = geodeticToPixel(dataSet, patch.getCorner(Quad::SOUTH_EAST));
        glm::uvec2 numPixels0 = pixelEnd0 - pixelStart0;
        

        // Calculate a suitable overview to choose from the GDAL dataset
        int minNumPixels0 = glm::min(numPixels0.x, numPixels0.y);
        GDALRasterBand* maxOverview;
        if (numOverviews <= 0) {
            maxOverview = firstBand;
        }
        else {
            maxOverview = firstBand->GetOverview(numOverviews - 1);
        }
        int sizeLevel0 = maxOverview->GetXSize();
        // The dataset itself may not have overviews but even if it does not, an overview
        // for the data region can be calculated and possibly be used to sample greater
        // Regions of the original dataset.
        int ov = std::log2(minNumPixels0) - std::log2(sizeLevel0 + 1) - tileLevelDifference;

        if (numOverviews > 0) {
            ov = glm::clamp(ov, 0, numOverviews - 1);
        }

        // Convert the interval [pixelStart0, pixelEnd0] to pixel space at 
        // the calculated suitable overview, ov. using a >> b = a / 2^b
        int toShift = ov + 1;

        // Set member variables
        overview = ov;

        pixelStart = glm::uvec2(pixelStart0.x >> toShift, pixelStart0.y >> toShift);
        pixelEnd = glm::uvec2(pixelEnd0.x >> toShift, pixelEnd0.y >> toShift);
        numPixels = pixelEnd - pixelStart;

    }

    glm::uvec2 GdalDataRegion::geodeticToPixel(GDALDataset* dataSet, const Geodetic2& geo) {

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

        return glm::uvec2(glm::round(P), glm::round(L));
    }



    






    //////////////////////////////////////////////////////////////////////////////////
    //                               Tile Dataset                                   //
    //////////////////////////////////////////////////////////////////////////////////

    bool TileDataset::GdalHasBeenInitialized = false;

    TileDataset::TileDataset(const std::string& gdalDatasetDesc, int minimumPixelSize,
        bool doPreprocessing, GLuint dataType)
        : _minimumPixelSize(minimumPixelSize)
        , _doPreprocessing(doPreprocessing)
        , _maxLevel(-1)
    {
        if (!GdalHasBeenInitialized) {
            GDALAllRegister();
            CPLSetConfigOption("GDAL_DATA", absPath("${MODULE_GLOBEBROWSING}/gdal_data").c_str());

            GdalHasBeenInitialized = true;
        }

        _dataset = (GDALDataset *)GDALOpen(gdalDatasetDesc.c_str(), GA_ReadOnly);
        if (!_dataset) {
            throw ghoul::RuntimeError("Failed to load dataset:\n" + gdalDatasetDesc);
        }
        _dataLayout = TileDataLayout(_dataset, dataType);

        _depthTransform = calculateTileDepthTransform();
        _tileLevelDifference = calculateTileLevelDifference(_dataset, minimumPixelSize);
        LDEBUG(gdalDatasetDesc << " - " << _tileLevelDifference);
        _maxLevel = calculateMaxLevel(_tileLevelDifference);
    }


    TileDataset::~TileDataset() {
        delete _dataset;
    }

    int TileDataset::calculateTileLevelDifference(GDALDataset* dataset, int minimumPixelSize) {
        GDALRasterBand* firstBand = dataset->GetRasterBand(1);
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
        return log2(minimumPixelSize) - log2(sizeLevel0);
    }

    const int TileDataset::calculateMaxLevel(int tileLevelDifference) {
        int numOverviews = _dataset->GetRasterBand(1)->GetOverviewCount();
        if (numOverviews <= 0) { // No overviews.
            return -tileLevelDifference;
        }
        else { // Use the overview to get the maximum level.
            return numOverviews - 1 - tileLevelDifference;
        }
    }

    TileDepthTransform TileDataset::calculateTileDepthTransform() {
        GDALRasterBand* firstBand = _dataset->GetRasterBand(1);
        // Floating point types does not have a fix maximum or minimum value and
        // can not be normalized when sampling a texture. Hence no rescaling is needed.
        double maximumValue = (_dataLayout.gdalType == GDT_Float32 || _dataLayout.gdalType == GDT_Float64) ?
            1.0 : TileDataType::getMaximumValue(_dataLayout.gdalType);

        TileDepthTransform transform;
        transform.depthOffset = firstBand->GetOffset();
        transform.depthScale = firstBand->GetScale() * maximumValue;
        return transform;
    }

    int TileDataset::getMaximumLevel() const {
        return _maxLevel;
    }

    TileDepthTransform TileDataset::getDepthTransform() const {
        return _depthTransform;
    }

    std::shared_ptr<TileIOResult> TileDataset::readTileData(ChunkIndex chunkIndex) {
        GdalDataRegion region(_dataset, chunkIndex, _tileLevelDifference);

        size_t bytesPerLine = _dataLayout.bytesPerPixel * region.numPixels.x;
        size_t totalNumBytes = bytesPerLine * region.numPixels.y;
        char* imageData = new char[totalNumBytes];

        CPLErr worstError = CPLErr::CE_None;

        // Read the data (each rasterband is a separate channel)
        for (size_t i = 0; i < _dataLayout.numRasters; i++) {
            GDALRasterBand* rasterBand;
            int pixelSourceScale = 1;
            if (_dataset->GetRasterBand(i + 1)->GetOverviewCount() <= 0){
                rasterBand = _dataset->GetRasterBand(i + 1);
                pixelSourceScale = pow(2, region.overview + 1);
            }
            else {
                rasterBand = _dataset->GetRasterBand(i + 1)->GetOverview(region.overview);
                pixelSourceScale = 1;
            }
            
            char* dataDestination = imageData + (i * _dataLayout.bytesPerDatum);
            
            int pixelStartX = region.pixelStart.x * pixelSourceScale;
            int pixelStartY = region.pixelStart.y * pixelSourceScale;
            int pixelWidthX = region.numPixels.x * pixelSourceScale;
            int pixelWidthY = region.numPixels.y * pixelSourceScale;

            // Clamp to be inside dataset
            pixelStartX = glm::max(pixelStartX, 0);
            pixelStartY = glm::max(pixelStartY, 0);
            pixelWidthX = glm::min(pixelStartX + pixelWidthX, rasterBand->GetXSize()) - pixelStartX;
            pixelWidthY = glm::min(pixelStartY + pixelWidthY, rasterBand->GetYSize()) - pixelStartY;

            CPLErr err = rasterBand->RasterIO(
                GF_Read,
                pixelStartX,           // Begin read x
                pixelStartY,           // Begin read y
                pixelWidthX,            // width to read x
                pixelWidthY,            // width to read y
                dataDestination,               // Where to put data
                region.numPixels.x,            // width to write x in destination
                region.numPixels.y,            // width to write y in destination
                _dataLayout.gdalType,		   // Type
                _dataLayout.bytesPerPixel,	   // Pixel spacing
                bytesPerLine);      // Line spacing

            // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
            worstError = std::max(worstError, err);
        }

        std::shared_ptr<TileIOResult> result(new TileIOResult);
        result->chunkIndex = chunkIndex;
        result->imageData = getImageDataFlippedY(region, _dataLayout, imageData);
        result->dimensions = glm::uvec3(region.numPixels, 1);
        result->nBytesImageData = _dataLayout.bytesPerPixel * region.numPixels.x * region.numPixels.y;
        result->error = worstError;
        if (_doPreprocessing) {
            result->preprocessData = preprocess(imageData, region, _dataLayout);
            int success;
            auto gdalOverview = _dataset->GetRasterBand(1)->GetOverview(region.overview);
            double missingDataValue = gdalOverview->GetNoDataValue(&success);
            if (!success) {
                missingDataValue = 32767; // missing data value
            }
            bool hasMissingData = false;
            for (size_t c = 0; c < _dataLayout.numRasters; c++) {
                hasMissingData |= result->preprocessData->maxValues[c] == missingDataValue;
            }
            bool onHighLevel = chunkIndex.level > 6;
            if (hasMissingData && onHighLevel) {
                result->error = CE_Fatal;
            }
        }

        delete[] imageData;
        return result;
    }

    char* TileDataset::getImageDataFlippedY(const GdalDataRegion& region,
        const TileDataLayout& dataLayout, const char* imageData) 
    {
        size_t bytesPerLine = dataLayout.bytesPerPixel * region.numPixels.x;
        size_t totalNumBytes = bytesPerLine * region.numPixels.y;

        // GDAL reads image data top to bottom. We want the opposite.
        char* imageDataYflipped = new char[totalNumBytes];
        for (size_t y = 0; y < region.numPixels.y; y++) {
            size_t yi_flipped = y * bytesPerLine;
            size_t yi = (region.numPixels.y - 1 - y) * bytesPerLine;
            size_t i = 0;
            for (size_t x = 0; x < region.numPixels.x; x++) {
                for (size_t c = 0; c < dataLayout.numRasters; c++) {
                    for (size_t b = 0; b < dataLayout.bytesPerDatum; b++) {
                        imageDataYflipped[yi_flipped + i] = imageData[yi + i];
                        i++;
                    }
                }
            }
        }

        return imageDataYflipped;
    }


    const TileDataLayout& TileDataset::getDataLayout() const {
        return _dataLayout;
    }


    std::shared_ptr<TilePreprocessData> TileDataset::preprocess(const char* imageData,
        const GdalDataRegion& region, const TileDataLayout& dataLayout)
    {
        size_t bytesPerLine = dataLayout.bytesPerPixel * region.numPixels.x;
        size_t totalNumBytes = bytesPerLine * region.numPixels.y;

        TilePreprocessData* preprocessData = new TilePreprocessData();
        preprocessData->maxValues.resize(dataLayout.numRasters);
        preprocessData->minValues.resize(dataLayout.numRasters);

        for (size_t c = 0; c < dataLayout.numRasters; c++) {
            preprocessData->maxValues[c] = -FLT_MAX;
            preprocessData->minValues[c] = FLT_MAX;
        }

        for (size_t y = 0; y < region.numPixels.y; y++) {
            size_t yi_flipped = y * bytesPerLine;
            size_t yi = (region.numPixels.y - 1 - y) * bytesPerLine;
            size_t i = 0;
            for (size_t x = 0; x < region.numPixels.x; x++) {
                for (size_t c = 0; c < dataLayout.numRasters; c++) {

                    float val = TileDataType::interpretFloat(dataLayout.gdalType, &(imageData[yi + i]));
                    preprocessData->maxValues[c] = std::max(val, preprocessData->maxValues[c]);
                    preprocessData->minValues[c] = std::min(val, preprocessData->minValues[c]);

                    i += dataLayout.bytesPerDatum;
                }
            }
        }
        for (size_t c = 0; c < dataLayout.numRasters; c++) {
            if (preprocessData->maxValues[c] > 8800.0f) {
                //LDEBUG("Bad preprocess data: " << preprocessData->maxValues[c] << " at " << region.chunkIndex);
            }
        }

        return std::shared_ptr<TilePreprocessData>(preprocessData);
    }

    


}  // namespace openspace
