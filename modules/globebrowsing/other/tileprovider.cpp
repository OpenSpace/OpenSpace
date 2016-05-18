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
        int minimumPixelSize,
        int framesUntilRequestFlush)
    : _filePath(filePath)
    , _tileCache(tileCacheSize) // setting cache size
    , _framesSinceLastRequestFlush(0)
    , _framesUntilRequestFlush(framesUntilRequestFlush)
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

        GDALDataType gdalType = firstBand->GetRasterDataType();
        double maximumValue = (gdalType == GDT_Float32 || gdalType == GDT_Float64) ?
            1.0 : firstBand->GetMaximum();

        _depthTransform.depthOffset = firstBand->GetOffset();
        _depthTransform.depthScale = firstBand->GetScale() * maximumValue;


        
    }

    TileProvider::~TileProvider(){
        clearRequestQueue();
        delete _gdalDataSet;
    }


    void TileProvider::prerender() {
        initTexturesFromLoadedData();

        if (_framesSinceLastRequestFlush++ > _framesUntilRequestFlush) {
            clearRequestQueue();
        }
    }

    void TileProvider::initTexturesFromLoadedData() {
        while (_tileLoadManager.numFinishedJobs() > 0) {
            auto finishedJob = _tileLoadManager.popFinishedJob();
            std::shared_ptr<TextureData> uninitedTex =
                finishedJob->product();
            HashKey key = uninitedTex->chunkIndex.hashKey();
            std::shared_ptr<Texture> texture = initializeTexture(uninitedTex);
            _tileCache.put(key, texture);
        }
    }

    void TileProvider::clearRequestQueue() {
        _tileLoadManager.clearEnqueuedJobs();
        _queuedTileRequests.clear();
        _framesSinceLastRequestFlush = 0;
    }


    Tile TileProvider::getHighestResolutionTile(ChunkIndex chunkIndex) {
        TileUvTransform uvTransform;
        uvTransform.uvOffset = glm::vec2(0, 0);
        uvTransform.uvScale = glm::vec2(1, 1);

        int numOverviews = _gdalDataSet->GetRasterBand(1)->GetOverviewCount();
        int maximumLevel = numOverviews - 1 - _tileLevelDifference;
        while(chunkIndex.level > maximumLevel){
            transformFromParent(chunkIndex, uvTransform);
            chunkIndex = chunkIndex.parent();
        }
        
        return getOrEnqueueHighestResolutionTile(chunkIndex, uvTransform);
    }

    Tile TileProvider::getOrEnqueueHighestResolutionTile(const ChunkIndex& chunkIndex, 
        TileUvTransform& uvTransform) 
    {
        HashKey key = chunkIndex.hashKey();
        if (_tileCache.exist(key)) {
            return { _tileCache.get(key), uvTransform };
        }
        else if (chunkIndex.level <= 1) {
            return { getDefaultTexture(), uvTransform };
        }
        else {
            // We don't have the tile for the requested level
            // --> check if the parent has a tile we can use
            transformFromParent(chunkIndex, uvTransform);
            Tile tile = getOrEnqueueHighestResolutionTile(chunkIndex.parent(), uvTransform);

            // As we didn't have this tile, push it to the request queue
            // post order enqueueing tiles --> enqueue tiles at low levels first
            enqueueTileRequest(chunkIndex);

            return tile;
        }
    }



    void TileProvider::transformFromParent(const ChunkIndex& chunkIndex, TileUvTransform& uv) const {
        uv.uvOffset *= 0.5;
        uv.uvScale *= 0.5;

        if (chunkIndex.isEastChild()) {
            uv.uvOffset.x += 0.5;
        }

        // In OpenGL, positive y direction is up
        if (chunkIndex.isNorthChild()) {
            uv.uvOffset.y += 0.5;
        }
    }


    std::shared_ptr<Texture> TileProvider::getOrStartFetchingTile(ChunkIndex chunkIndex) {
        HashKey hashkey = chunkIndex.hashKey();
        if (_tileCache.exist(hashkey)) {
            return _tileCache.get(hashkey);
        }
        else {
            enqueueTileRequest(chunkIndex);
            return nullptr;
        }
    }

    bool TileProvider::enqueueTileRequest(const ChunkIndex& chunkIndex) {
        HashKey key = chunkIndex.hashKey();
        bool tileHasBeenQueued = _queuedTileRequests.find(key) != _queuedTileRequests.end();
        if (!tileHasBeenQueued) {
            // enque load job
            std::shared_ptr<TextureTileLoadJob> job = std::shared_ptr<TextureTileLoadJob>(
                new TextureTileLoadJob(this, chunkIndex));

            _tileLoadManager.enqueueJob(job);

            _queuedTileRequests.insert(key);
        }
        return !tileHasBeenQueued;
    }



    std::shared_ptr<Texture> TileProvider::getDefaultTexture() {
        return _defaultTexture;
    }

    TileDepthTransform TileProvider::depthTransform() {
        return _depthTransform;
    }


    std::shared_ptr<TextureData> TileProvider::getTextureData(
        const ChunkIndex& chunkIndex) {
        
        // We assume here that all rasterbands have the same data type
        GDALDataType gdalType = _gdalDataSet->GetRasterBand(1)->GetRasterDataType();

        switch (gdalType)
        {
        case GDT_Byte:
            return _uByteTextureTileDataProvider.getTextureData(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_UInt16:
            return _uShortTextureDataProvider.getTextureData(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_Int16:
            return _shortTextureDataProvider.getTextureData(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_UInt32:
            return _uIntTextureDataProvider.getTextureData(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_Int32:
            return _intTextureDataProvider.getTextureData(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);
            break;
        case GDT_Float32:
            return _floatTextureDataProvider.getTextureData(
                _gdalDataSet,
                chunkIndex,
                _tileLevelDifference);

            break;
        case GDT_Float64:
            return _doubleTextureDataProvider.getTextureData(
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
        std::shared_ptr<TextureData> uninitedTexture) {
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
