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

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/other/tileprovider.h>

#include <openspace/engine/downloadmanager.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <sstream>




namespace {
    const std::string _loggerCat = "TileProvider";
}


namespace openspace {

    bool TileProvider::hasInitializedGDAL = false;

    TileProvider::TileProvider(const std::string& filePath, int tileCacheSize)
    : _filePath(filePath)
    , _tileCache(tileCacheSize) // setting cache size
    {
        int downloadApplicationVersion = 1;
        if (!DownloadManager::isInitialized()) {
            DownloadManager::initialize("../tmp_openspace_downloads/", downloadApplicationVersion);
        }
        
        if (!hasInitializedGDAL) {
            GDALAllRegister();
            hasInitializedGDAL = true;
        }

        std::string absFilePath = absPath(filePath);
        _gdalDataSet = (GDALDataset *)GDALOpen(absFilePath.c_str(), GA_ReadOnly);
        //auto desc = _gdalDataSet->GetDescription();
        
        ghoul_assert(_gdalDataSet != nullptr, "Failed to load dataset: " << filePath);

    }

    TileProvider::~TileProvider(){
        delete _gdalDataSet;
    }


    void TileProvider::prerender() {
        while (_tileLoadManager.numFinishedJobs() > 0) {
            auto finishedJob = _tileLoadManager.popFinishedJob();
            std::shared_ptr<UninitializedTextureTile> uninitedTex = finishedJob->product();
            HashKey key = uninitedTex->tileIndex.hashKey();
            std::shared_ptr<Texture> texture = initializeTexture(uninitedTex);
            _tileCache.put(key, texture);
        }
    }


    std::shared_ptr<Texture> TileProvider::getTile(const GeodeticTileIndex& tileIndex) {
        HashKey hashkey = tileIndex.hashKey();
        
        if (_tileCache.exist(hashkey)) {
            return _tileCache.get(hashkey);
        }
        else {
            // enque load job
            std::shared_ptr<TextureTileLoadJob> job = std::shared_ptr<TextureTileLoadJob>(
                new TextureTileLoadJob(this, tileIndex));

            _tileLoadManager.enqueueJob(job);

            // map key to nullptr while tile is loaded
            _tileCache.put(hashkey, nullptr);
            return nullptr;
        }
    }





    std::shared_ptr<UninitializedTextureTile> TileProvider::getUninitializedTextureTile(const GeodeticTileIndex& tileIndex) {
        int nRasters = _gdalDataSet->GetRasterCount();

        ghoul_assert(nRasters > 0, "Bad dataset. Contains no rasterband.");

        GDALRasterBand* firstBand = _gdalDataSet->GetRasterBand(1);

        // Level = overviewCount - overview
        int numOverviews = firstBand->GetOverviewCount();


        int xSize0 = firstBand->GetXSize();
        int ySize0 = firstBand->GetYSize();

        GeodeticPatch patch = GeodeticPatch(tileIndex);
        glm::uvec2 pixelStart0 = _converter.geodeticToPixel(_gdalDataSet, patch.northWestCorner());
        glm::uvec2 pixelEnd0 = _converter.geodeticToPixel(_gdalDataSet, patch.southEastCorner());
        glm::uvec2 numberOfPixels0 = pixelEnd0 - pixelStart0;


        int minNumPixels0 = glm::min(numberOfPixels0.x, numberOfPixels0.y);

        int minNumPixelsRequired = 512;
        int ov = log2(minNumPixels0) - log2(minNumPixelsRequired);

        ov = glm::clamp(ov, 0, numOverviews - 1);

        glm::uvec2 pixelStart(pixelStart0.x >> (ov + 1), pixelStart0.y >> (ov + 1));
        glm::uvec2 numberOfPixels(numberOfPixels0.x >> (ov + 1), numberOfPixels0.y >> (ov + 1));



        // GDAL reads image data top to bottom
        GLubyte* imageData = new GLubyte[numberOfPixels.x * numberOfPixels.y * nRasters];

        // Read the data (each rasterband is a separate channel)
        for (size_t i = 0; i < nRasters; i++) {
            GDALRasterBand* rasterBand = _gdalDataSet->GetRasterBand(i + 1)->GetOverview(ov);

            int xSize = rasterBand->GetXSize();
            int ySize = rasterBand->GetYSize();

            rasterBand->RasterIO(
                GF_Read,
                pixelStart.x,					// Begin read x
                pixelStart.y,					// Begin read y
                numberOfPixels.x,				// width to read x
                numberOfPixels.y,				// width to read y
                imageData + i,				// Where to put data
                numberOfPixels.x,				// width to write x in destination
                numberOfPixels.y,				// width to write y in destination
                GDT_Byte,					// Type
                sizeof(GLubyte) * nRasters,	// Pixel spacing
                0);							// Line spacing
        }

        // GDAL reads image data top to bottom. We want the opposite.
        GLubyte* imageDataYflipped = new GLubyte[numberOfPixels.x * numberOfPixels.y * nRasters];
        for (size_t y = 0; y < numberOfPixels.y; y++) {
            for (size_t x = 0; x < numberOfPixels.x; x++) {
                imageDataYflipped[x + y * numberOfPixels.x] = imageData[x + (numberOfPixels.y - y) * numberOfPixels.x];
            }
        }

        delete[] imageData;

        glm::uvec3 dims(numberOfPixels.x, numberOfPixels.y, 1);
        GdalDataConverter::TextureFormat textrureFormat = _converter.getTextureFormatFromRasterCount(nRasters);
        UninitializedTextureTile* uninitedTexPtr = new UninitializedTextureTile(imageDataYflipped, dims, textrureFormat, tileIndex);
        std::shared_ptr<UninitializedTextureTile> uninitedTex = std::shared_ptr<UninitializedTextureTile>(uninitedTexPtr);
        return uninitedTex;
    }


    std::shared_ptr<Texture> TileProvider::initializeTexture(std::shared_ptr<UninitializedTextureTile> uninitedTexture) {
        Texture* tex = new Texture(
            static_cast<void*>(uninitedTexture->imageData),
            uninitedTexture->dimensions,
            uninitedTexture->texFormat.ghoulFormat,
            uninitedTexture->texFormat.glFormat,
            GL_UNSIGNED_BYTE,
            Texture::FilterMode::Linear,
            Texture::WrappingMode::Repeat);

        // The texture should take ownership of the data
        std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(tex);

        texture->uploadTexture();
        texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
        texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

        return texture;
    }



}  // namespace openspace
