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

    }


    std::shared_ptr<Texture> TileProvider::getTile(const GeodeticTileIndex& tileIndex) {
        HashKey hashkey = tileIndex.hashKey();
        
        if (_tileCache.exist(hashkey)) {
            return _tileCache.get(hashkey);
        }
        else {
            //GeodeticTileIndex ti0 = { 0, 0, 0 };
            //auto texture = _converter.convertToOpenGLTexture(_gdalDataSet, tileIndex, GL_UNSIGNED_BYTE);
            auto texture = getTileInternal(tileIndex, GL_UNSIGNED_BYTE);
            texture->uploadTexture();
            texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
            texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
            _tileCache.put(hashkey, texture);
            return texture;
        }
    }





    std::shared_ptr<Texture> TileProvider::getTileInternal(const GeodeticTileIndex& tileIndex, int GLType) {



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

        int minNumPixelsRequired = 256;
        int ov = log2(minNumPixels0) - log2(minNumPixelsRequired);

        ov = glm::clamp(ov, 0, numOverviews-1);

        glm::uvec2 pixelStart(pixelStart0.x >> (ov+1), pixelStart0.y >> (ov + 1));
        glm::uvec2 numberOfPixels(numberOfPixels0.x >> (ov + 1), numberOfPixels0.y >> (ov + 1));

        // For testing
        /*pixelStart = glm::uvec2(0, 0);
        numberOfPixels = glm::uvec2(512, 256);
        ov = 15;
        */
        // The data that the texture should read
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

        GLubyte* imageDataYflipped = new GLubyte[numberOfPixels.x * numberOfPixels.y * nRasters];
        for (size_t y = 0; y < numberOfPixels.y; y++) {
            for (size_t x = 0; x < numberOfPixels.x; x++) {
                imageDataYflipped[x + y * numberOfPixels.x] = imageData[x + (numberOfPixels.y - y) * numberOfPixels.x];
            }
        }


        GdalDataConverter::TextureFormat textrureFormat = _converter.getTextureFormatFromRasterCount(nRasters);


        Texture* tex = new Texture(
            static_cast<void*>(imageDataYflipped),
            glm::uvec3(numberOfPixels.x, numberOfPixels.y, 1),
            textrureFormat.ghoulFormat,
            textrureFormat.glFormat,
            GL_UNSIGNED_BYTE,
            Texture::FilterMode::Linear,
            Texture::WrappingMode::Repeat);

        // The texture should take ownership of the data
        std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(tex);

        delete[] imageData;

        // Do not free imageData since the texture now has ownership of it
        return texture;

    }

}  // namespace openspace
