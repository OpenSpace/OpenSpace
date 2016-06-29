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
#include <modules/globebrowsing/tile/tilediskcache.h>
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

    SingleImagePrivoder::SingleImagePrivoder(const std::string& imagePath) {
        _tile = Tile();
        _tile.texture = std::shared_ptr<Texture>(ghoul::io::TextureReader::ref().loadTexture(imagePath).release());
        _tile.status = _tile.texture != nullptr ? Tile::Status::OK : Tile::Status::IOError;
        _tile.preprocessData = nullptr;

        _tile.texture->uploadTexture();
        _tile.texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    }

    Tile SingleImagePrivoder::getTile(const ChunkIndex& chunkIndex) {
        return _tile;
    }

    Tile::Status SingleImagePrivoder::getTileStatus(const ChunkIndex& index) {
        return _tile.status;
    }

    TileDepthTransform SingleImagePrivoder::depthTransform() {
        TileDepthTransform transform;
        transform.depthOffset = 0.0f;
        transform.depthScale = 1.0f;
        return transform;
    }

    void SingleImagePrivoder::update() {
        // nothing to be done
    }

    int SingleImagePrivoder::maxLevel() {
        return 1337; // unlimited
    }

    






    CachingTileProvider::CachingTileProvider(std::shared_ptr<AsyncTileDataProvider> tileReader, 
        std::shared_ptr<TileCache> tileCache,
        int framesUntilFlushRequestQueue)
        : _asyncTextureDataProvider(tileReader)
        , _tileCache(tileCache)
        , _framesUntilRequestFlush(framesUntilFlushRequestQueue)
        , _framesSinceLastRequestFlush(0)
    {
        
    }


    CachingTileProvider::~CachingTileProvider(){
        clearRequestQueue();
    }


    void CachingTileProvider::update() {
        initTexturesFromLoadedData();
        if (_framesSinceLastRequestFlush++ > _framesUntilRequestFlush) {
            clearRequestQueue();
        }
    }

    int CachingTileProvider::maxLevel() {
        return _asyncTextureDataProvider->getTextureDataProvider()->getMaximumLevel();
    }

    Tile CachingTileProvider::getTile(const ChunkIndex& chunkIndex) {
        Tile tile = Tile::TileUnavailable;

        
        if (chunkIndex.level > maxLevel()) {
            tile.status = Tile::Status::OutOfRange;
            return tile;
        }

        ChunkHashKey key = chunkIndex.hashKey();

        if (_tileCache->exist(key)) {
            return _tileCache->get(key);
        }
        else {
            _asyncTextureDataProvider->enqueueTextureData(chunkIndex);
        }
        
        return tile;
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

    Tile::Status CachingTileProvider::getTileStatus(const ChunkIndex& chunkIndex) {
        auto tileDataset = _asyncTextureDataProvider->getTextureDataProvider();
        if (chunkIndex.level > tileDataset->getMaximumLevel()) {
            return Tile::Status::OutOfRange;
        }

        ChunkHashKey key = chunkIndex.hashKey();

        if (_tileCache->exist(key)) {
            return _tileCache->get(key).status;
        }

        return Tile::Status::Unavailable;
    }


    Tile CachingTileProvider::getOrStartFetchingTile(ChunkIndex chunkIndex) {
        ChunkHashKey hashkey = chunkIndex.hashKey();
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
        ChunkHashKey key = tileIOResult->chunkIndex.hashKey();
        TileDataLayout dataLayout = _asyncTextureDataProvider->getTextureDataProvider()->getDataLayout();
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
        
        texture->uploadTexture();
        // AnisotropicMipMap must be set after texture is uploaded. Why?!
        texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

        Tile tile = {
            texture,
            tileIOResult->preprocessData,
            tileIOResult->error == CE_None ? Tile::Status::OK : Tile::Status::IOError
        };

        _tileCache->put(key, tile);
    }



}  // namespace openspace
