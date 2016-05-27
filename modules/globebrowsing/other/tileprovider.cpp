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
#include <modules/globebrowsing/other/tileprovidermanager.h>

#include <modules/globebrowsing/globes/chunkindex.h>

#include <openspace/engine/downloadmanager.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <sstream>




namespace {
    const std::string _loggerCat = "TileProvider";
}


namespace openspace {

    TileProvider::TileProvider(std::shared_ptr<AsyncTileDataProvider> tileReader, int tileCacheSize,
        int framesUntilFlushRequestQueue)
        : _asyncTextureDataProvider(tileReader)
        , _tileCache(tileCacheSize)
        , _framesSinceLastRequestFlush(0)
    {
        initDefaultTexture();
    }


    TileProvider::~TileProvider(){
        clearRequestQueue();
    }

    void TileProvider::initDefaultTexture() {
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
    }


    void TileProvider::prerender() {
        initTexturesFromLoadedData();
        if (_framesSinceLastRequestFlush++ > _framesUntilRequestFlush) {
            clearRequestQueue();
        }
    }

    void TileProvider::initTexturesFromLoadedData() {
        while (_asyncTextureDataProvider->hasLoadedTextureData()) {
            std::shared_ptr<TileIOResult> tileIOResult = _asyncTextureDataProvider->nextTileIOResult();
            initializeAndAddToCache(tileIOResult);
        }
    }

    void TileProvider::clearRequestQueue() {
        _asyncTextureDataProvider->clearRequestQueue();
        _framesSinceLastRequestFlush = 0;
    }


    Tile TileProvider::getHighestResolutionTile(ChunkIndex chunkIndex) {
        TileUvTransform uvTransform;
        uvTransform.uvOffset = glm::vec2(0, 0);
        uvTransform.uvScale = glm::vec2(1, 1);

        int maximumLevel = _asyncTextureDataProvider->getTextureDataProvider()->getMaximumLevel();

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
        if (_tileCache.exist(key) && _tileCache.get(key).ioError == CPLErr::CE_None) {
            std::shared_ptr<Texture> texture = _tileCache.get(key).texture;
            return { texture, uvTransform };
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
            _asyncTextureDataProvider->enqueueTextureData(chunkIndex);

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
            return _tileCache.get(hashkey).texture;
        }
        else {
            _asyncTextureDataProvider->enqueueTextureData(chunkIndex);
            return nullptr;
        }
    }


    std::shared_ptr<Texture> TileProvider::getDefaultTexture() {
        return _defaultTexture;
    }

    TileDepthTransform TileProvider::depthTransform() {
        return _asyncTextureDataProvider->getTextureDataProvider()->getDepthTransform();
    }


    void TileProvider::initializeAndAddToCache(std::shared_ptr<TileIOResult> tileIOResult) {

        std::shared_ptr<RawTileData> tileData = tileIOResult->rawTileData;
        HashKey key = tileData->chunkIndex.hashKey();
        Texture* texturePtr = new Texture(
            tileData->imageData,
            tileData->dimensions,
            tileData->texFormat.ghoulFormat,
            tileData->texFormat.glFormat,
            tileData->glType,
            Texture::FilterMode::Linear,
            Texture::WrappingMode::ClampToEdge);

        // The texture should take ownership of the data
        std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(texturePtr);
        texture->uploadTexture();
        
        MetaTexture metaTexture = { texture, tileIOResult->error };

        _tileCache.put(key, metaTexture);
    }



}  // namespace openspace
