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

    TileProvider::TileProvider(
        const std::string& filePath,
        int tileCacheSize,
        int minimumPixelSize)
    : _filePath(filePath)
    , _tileCache(tileCacheSize) // setting cache size
    {
        // Set a temporary texture
        std::string fileName = "textures/earth_bluemarble.jpg";
        _defaultTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(fileName)));

        if (_defaultTexture) {
            LDEBUG("Loaded texture from '" << fileName << "'");
            _defaultTexture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //_testTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            _defaultTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
            _defaultTexture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
        }


        int downloadApplicationVersion = 1;
        if (!DownloadManager::isInitialized()) {
            DownloadManager::initialize(
                "../tmp_openspace_downloads/",
                downloadApplicationVersion);
        }
        
        if (!hasInitializedGDAL) {
            GDALAllRegister();
            hasInitializedGDAL = true;
        }

        std::string absFilePath = absPath(filePath);
        _gdalDataSet = (GDALDataset *)GDALOpen(absFilePath.c_str(), GA_ReadOnly);
        //auto desc = _gdalDataSet->GetDescription();
        
        ghoul_assert(_gdalDataSet != nullptr, "Failed to load dataset: " << filePath);

        GDALRasterBand* firstBand = _gdalDataSet->GetRasterBand(1);
        int numOverviews = firstBand->GetOverviewCount();
        int sizeLevel0 = firstBand->GetOverview(numOverviews - 1)->GetXSize();

        _tileLevelDifference = log2(minimumPixelSize) - log2(sizeLevel0);
    }

    TileProvider::~TileProvider(){
        delete _gdalDataSet;
    }


    void TileProvider::prerender() {
        while (_tileLoadManager.numFinishedJobs() > 0) {
            auto finishedJob = _tileLoadManager.popFinishedJob();
            std::shared_ptr<UninitializedTextureTile> uninitedTex =
                finishedJob->product();
            HashKey key = uninitedTex->chunkIndex.hashKey();
            std::shared_ptr<Texture> texture = initializeTexture(uninitedTex);
            _tileCache.put(key, texture);
        }
    }


    Tile TileProvider::getMostHiResTile(ChunkIndex chunkIndex) {
        std::shared_ptr<Texture> tex = nullptr;
        glm::vec2 uvOffset(0, 0);
        glm::vec2 uvScale(1, 1);
        
        // Check if we are trying to get a texture for a very small patch.
        // In that case, use the biggest one defined for the dataset.
        int maximumAllowedLevel =
            _gdalDataSet->GetRasterBand(1)->GetOverviewCount() - 1;
        int levelInDataset = chunkIndex.level + _tileLevelDifference;
        int timesToStepUp = levelInDataset - maximumAllowedLevel;
        for (int i = 0; i < timesToStepUp; i++)
        {
            uvScale *= 0.5;
            uvOffset *= 0.5;

            if (chunkIndex.isEastChild()) {
                uvOffset.x += 0.5;
            }

            // In OpenGL, positive y direction is up
            if (chunkIndex.isNorthChild()) {
                uvOffset.y += 0.5;
            }

            chunkIndex = chunkIndex.parent();
        }
        
        // We also need to check if the wanted texture is available. If not, go up a level
        while (true) {
            tex = getOrStartFetchingTile(chunkIndex);

            if (tex != nullptr) {
                break;
            }

            if (!chunkIndex.hasParent()) {
                tex = getDefaultTexture();
                break;
            }

            // If we have a parent, calculate the UV offset and scale from the chunkIndex
            else {
                uvScale *= 0.5;
                uvOffset *= 0.5;

                if (chunkIndex.isEastChild()) {
                    uvOffset.x += 0.5;
                }

                // In OpenGL, positive y direction is up
                if (chunkIndex.isNorthChild()) {
                    uvOffset.y += 0.5;
                }

                chunkIndex = chunkIndex.parent();
            }
        }
        
        return { tex, uvOffset, uvScale };
    }

    std::shared_ptr<Texture> TileProvider::getOrStartFetchingTile(ChunkIndex chunkIndex) {
        
        HashKey hashkey = chunkIndex.hashKey();
        
        if (_tileCache.exist(hashkey)) {
            return _tileCache.get(hashkey);
        }
        else {
            // enque load job
            std::shared_ptr<TextureTileLoadJob> job = std::shared_ptr<TextureTileLoadJob>(
                new TextureTileLoadJob(this, chunkIndex));

            _tileLoadManager.enqueueJob(job);

            // map key to nullptr while tile is loaded
            _tileCache.put(hashkey, nullptr);
            return nullptr;
        }
    }



    std::shared_ptr<Texture> TileProvider::getDefaultTexture() {
        return _defaultTexture;
    }

    std::shared_ptr<UninitializedTextureTile> TileProvider::getUninitializedTextureTile(
        const ChunkIndex& chunkIndex) {
        
        // We assume here that all rasterbands have the same data type
        GDALDataType gdalType = _gdalDataSet->GetRasterBand(1)->GetRasterDataType();

        switch (gdalType)
        {
        case GDT_Byte:
            return _uByteConverter.getUninitializedTextureTile(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_UInt16:
            return _uShortConverter.getUninitializedTextureTile(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_Int16:
            return _shortConverter.getUninitializedTextureTile(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_UInt32:
            return _uIntConverter.getUninitializedTextureTile(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_Int32:
            return _intConverter.getUninitializedTextureTile(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_Float32:
            return _floatConverter.getUninitializedTextureTile(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);

            break;
        case GDT_Float64:
            return _doubleConverter.getUninitializedTextureTile(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);

            break;
        default:
            LERROR("GDAL data type unknown to OpenGL: " << gdalType);
        }
        return nullptr;
    }

    std::shared_ptr<Texture> TileProvider::initializeTexture(
        std::shared_ptr<UninitializedTextureTile> uninitedTexture) {
        Texture* tex = new Texture(
            uninitedTexture->imageData,
            uninitedTexture->dimensions,
            uninitedTexture->texFormat.ghoulFormat,
            uninitedTexture->texFormat.glFormat,
            uninitedTexture->glType,
            Texture::FilterMode::Linear,
            Texture::WrappingMode::ClampToEdge);

        // The texture should take ownership of the data
        std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(tex);

        texture->uploadTexture();
        return texture;
    }



}  // namespace openspace
