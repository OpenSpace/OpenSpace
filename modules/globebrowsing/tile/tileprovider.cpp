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

#include <modules/globebrowsing/geometry/geodetic2.h>

#include <modules/globebrowsing/tile/tileprovider.h>
#include <modules/globebrowsing/tile/tileprovidermanager.h>

#include <modules/globebrowsing/chunk/chunkindex.h>

#include <openspace/engine/downloadmanager.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <sstream>




namespace {
    const std::string _loggerCat = "TileProvider";
}


namespace openspace {

    const Tile Tile::TileUnavailable = {nullptr, nullptr, Tile::Status::Unavailable };



    CachingTileProvider::CachingTileProvider(std::shared_ptr<AsyncTileDataProvider> tileReader, 
        std::shared_ptr<TileCache> tileCache,
        int framesUntilFlushRequestQueue)
        : _asyncTextureDataProvider(tileReader)
        , _tileCache(tileCache)
        , _framesSinceLastRequestFlush(0)
    {
        
    }


    CachingTileProvider::~CachingTileProvider(){
        clearRequestQueue();
    }


    void CachingTileProvider::prerender() {
        initTexturesFromLoadedData();
        if (_framesSinceLastRequestFlush++ > _framesUntilRequestFlush) {
            clearRequestQueue();
        }
    }

    void CachingTileProvider::initTexturesFromLoadedData() {
        while (_asyncTextureDataProvider->hasLoadedTextureData()) {
            std::shared_ptr<TileIOResult> tileIOResult = _asyncTextureDataProvider->nextTileIOResult();
            initializeAndAddToCache(tileIOResult);
        }
    }

    void CachingTileProvider::clearRequestQueue() {
        _asyncTextureDataProvider->clearRequestQueue();
        _framesSinceLastRequestFlush = 0;
    }


    TileAndTransform CachingTileProvider::getHighestResolutionTile(ChunkIndex chunkIndex, int parents) {
        TileUvTransform uvTransform;
        uvTransform.uvOffset = glm::vec2(0, 0);
        uvTransform.uvScale = glm::vec2(1, 1);


        for (int i = 0; i < parents && chunkIndex.level > 1; i++) {
            transformFromParent(chunkIndex, uvTransform);
            chunkIndex = chunkIndex.parent();
        }

        int maximumLevel = _asyncTextureDataProvider->getTextureDataProvider()->getMaximumLevel();
        while(chunkIndex.level > maximumLevel){
            transformFromParent(chunkIndex, uvTransform);
            chunkIndex = chunkIndex.parent();
        }
        
        return getOrEnqueueHighestResolutionTile(chunkIndex, uvTransform);
    }

    Tile::Status CachingTileProvider::getTileStatus(const ChunkIndex& chunkIndex) {
        auto tileDataset = _asyncTextureDataProvider->getTextureDataProvider();
        if (chunkIndex.level > tileDataset->getMaximumLevel()) {
            return Tile::Status::OutOfRange;
        }

        HashKey key = chunkIndex.hashKey();

        if (_tileCache->exist(key)) {
            return _tileCache->get(key).status;
        }

        return Tile::Status::Unavailable;
    }


    TileAndTransform CachingTileProvider::getOrEnqueueHighestResolutionTile(const ChunkIndex& chunkIndex, 
        TileUvTransform& uvTransform) 
    {
        
        HashKey key = chunkIndex.hashKey();
        
        if (_tileCache->exist(key)) {
            auto tile = _tileCache->get(key);
            if (tile.status == Tile::Status::OK) {
                return{ _tileCache->get(key), uvTransform };
            }

        }
        if (chunkIndex.level < 1) {
            return{ Tile::TileUnavailable, uvTransform };
        }
        else {
            // We don't have the tile for the requested level
            // --> check if the parent has a tile we can use
            transformFromParent(chunkIndex, uvTransform);
            TileAndTransform tile = getOrEnqueueHighestResolutionTile(chunkIndex.parent(), uvTransform);

            // As we didn't have this tile, push it to the request queue
            // post order enqueueing tiles --> enqueue tiles at low levels first
            _asyncTextureDataProvider->enqueueTextureData(chunkIndex);

            return tile;
        }
    }



    void CachingTileProvider::transformFromParent(const ChunkIndex& chunkIndex, TileUvTransform& uv) const {
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


    Tile CachingTileProvider::getOrStartFetchingTile(ChunkIndex chunkIndex) {
        HashKey hashkey = chunkIndex.hashKey();
        if (_tileCache->exist(hashkey)) {
            return _tileCache->get(hashkey);
        }
        else {
            _asyncTextureDataProvider->enqueueTextureData(chunkIndex);
            return Tile::TileUnavailable;
        }
    }

    TileDepthTransform CachingTileProvider::depthTransform() {
        return _asyncTextureDataProvider->getTextureDataProvider()->getDepthTransform();
    }


    void CachingTileProvider::initializeAndAddToCache(std::shared_ptr<TileIOResult> tileIOResult) {
        HashKey key = tileIOResult->chunkIndex.hashKey();
        TileDataset::DataLayout dataLayout = _asyncTextureDataProvider->getTextureDataProvider()->getDataLayout();
        Texture* texturePtr = new Texture(
            tileIOResult->imageData,
            tileIOResult->dimensions,
            dataLayout.textureFormat.ghoulFormat,
            dataLayout.textureFormat.glFormat,
            dataLayout.glType,
            Texture::FilterMode::Linear,
            Texture::WrappingMode::ClampToEdge);
        
        // The texture should take ownership of the data
        std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(texturePtr);
        //texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

        texture->uploadTexture();
        

        Tile tile = {
            texture,
            tileIOResult->preprocessData,
            tileIOResult->error == CE_None ? Tile::Status::OK : Tile::Status::IOError
        };

        _tileCache->put(key, tile);
    }



}  // namespace openspace
