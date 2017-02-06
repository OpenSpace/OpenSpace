/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/tile/tileprovider/cachingtileprovider.h>

#include <modules/globebrowsing/tile/asynctilereader.h>
#include <modules/globebrowsing/tile/tiledataset.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>

namespace {
    const char* _loggerCat = "CachingTileProvider";

    const char* KeyDoPreProcessing = "DoPreProcessing";
    const char* KeyMinimumPixelSize = "MinimumPixelSize";
    const char* KeyFilePath = "FilePath";
    const char* KeyCacheSize = "CacheSize";
    const char* KeyFlushInterval = "FlushInterval";
}

namespace openspace {
namespace globebrowsing {

CachingTileProvider::CachingTileProvider(const ghoul::Dictionary& dictionary) 
    : _framesSinceLastRequestFlush(0)
{
    std::string name = "Name unspecified";
    dictionary.getValue("Name", name);
    std::string _loggerCat = "CachingTileProvider : " + name;

    // 1. Get required Keys
    std::string filePath;
    if (!dictionary.getValue<std::string>(KeyFilePath, filePath)) {
        throw std::runtime_error(std::string("Must define key '") + KeyFilePath + "'");
    }

    // 2. Initialize default values for any optional Keys
    TileDataset::Configuration config;
    config.doPreProcessing = false;
    config.minimumTilePixelSize = 512;
        
    // getValue does not work for integers
    double minimumPixelSize; 
    double cacheSize = 512;
    double framesUntilRequestFlush = 60;

    // 3. Check for used spcified optional keys
    if (dictionary.getValue<bool>(KeyDoPreProcessing, config.doPreProcessing)) {
        LDEBUG("Default doPreProcessing overridden: " << config.doPreProcessing);
    }
    if (dictionary.getValue<double>(KeyMinimumPixelSize, minimumPixelSize)) {
        LDEBUG("Default minimumPixelSize overridden: " << minimumPixelSize);
        config.minimumTilePixelSize = static_cast<int>(minimumPixelSize); 
    }
    if (dictionary.getValue<double>(KeyCacheSize, cacheSize)) {
        LDEBUG("Default cacheSize overridden: " << cacheSize);
    }
    if (dictionary.getValue<double>(KeyFlushInterval, framesUntilRequestFlush)) {
        LDEBUG("Default framesUntilRequestFlush overridden: " <<
            framesUntilRequestFlush);
    }

    // Initialize instance variables
    auto tileDataset = std::make_shared<TileDataset>(filePath, config);

    // only one thread per provider supported atm
    // (GDAL does not handle multiple threads for a single dataset very well
    // currently)
    auto threadPool = std::make_shared<ThreadPool>(1);

    _asyncTextureDataProvider = std::make_shared<AsyncTileDataProvider>(
        tileDataset, threadPool);
    _tileCache = std::make_shared<TileCache>(static_cast<size_t>(cacheSize));
    _framesUntilRequestFlush = framesUntilRequestFlush;
}

CachingTileProvider::CachingTileProvider(
                                        std::shared_ptr<AsyncTileDataProvider> tileReader, 
                                        std::shared_ptr<TileCache> tileCache,
                                        int framesUntilFlushRequestQueue)
    : _asyncTextureDataProvider(tileReader)
    , _tileCache(tileCache)
    , _framesUntilRequestFlush(framesUntilFlushRequestQueue)
    , _framesSinceLastRequestFlush(0)
{}

CachingTileProvider::~CachingTileProvider(){
    clearRequestQueue();
}

void CachingTileProvider::update() {
    initTexturesFromLoadedData();
    if (_framesSinceLastRequestFlush++ > _framesUntilRequestFlush) {
        clearRequestQueue();
    }
}

void CachingTileProvider::reset() {
    _tileCache->clear();
    _asyncTextureDataProvider->reset();
}

int CachingTileProvider::maxLevel() {
    return _asyncTextureDataProvider->getTextureDataProvider()->maxChunkLevel();
}

Tile CachingTileProvider::getTile(const TileIndex& tileIndex) {
    Tile tile = Tile::TileUnavailable;

    if (tileIndex.level > maxLevel()) {
        tile.status = Tile::Status::OutOfRange;
        return tile;
    }

    TileHashKey key = tileIndex.hashKey();

    if (_tileCache->exist(key)) {
        return _tileCache->get(key);
    }
    else {
        _asyncTextureDataProvider->enqueueTileIO(tileIndex);
    }
        
    return tile;
}

float CachingTileProvider::noDataValueAsFloat() {
    return _asyncTextureDataProvider->noDataValueAsFloat();
}

Tile CachingTileProvider::getDefaultTile() {
    if (_defaultTile.texture == nullptr) {
        _defaultTile = createTile(
            _asyncTextureDataProvider->getTextureDataProvider()->defaultTileData()
        );
    }
    return _defaultTile;
}

void CachingTileProvider::initTexturesFromLoadedData() {
    auto rawTiles = _asyncTextureDataProvider->getRawTiles();
    for (auto rawTile : rawTiles){
        TileHashKey key = rawTile->tileIndex.hashKey();
        Tile tile = createTile(rawTile);
        _tileCache->put(key, tile);
    }
}

void CachingTileProvider::clearRequestQueue() {
    _asyncTextureDataProvider->clearRequestQueue();
    _framesSinceLastRequestFlush = 0;
}

Tile::Status CachingTileProvider::getTileStatus(const TileIndex& tileIndex) {
    auto tileDataset = _asyncTextureDataProvider->getTextureDataProvider();
    if (tileIndex.level > tileDataset->maxChunkLevel()) {
        return Tile::Status::OutOfRange;
    }

    TileHashKey key = tileIndex.hashKey();

    if (_tileCache->exist(key)) {
        return _tileCache->get(key).status;
    }

    return Tile::Status::Unavailable;
}

TileDepthTransform CachingTileProvider::depthTransform() {
    return _asyncTextureDataProvider->getTextureDataProvider()->getDepthTransform();
}

Tile CachingTileProvider::createTile(std::shared_ptr<RawTile> rawTile) {
    if (rawTile->error != CE_None) {
        return{ nullptr, nullptr, Tile::Status::IOError };
    }

    TileHashKey key = rawTile->tileIndex.hashKey();
    TileDataLayout dataLayout =
        _asyncTextureDataProvider->getTextureDataProvider()->getDataLayout();
        
    // The texture should take ownership of the data
    std::shared_ptr<Texture> texture = std::make_shared<Texture>(
        rawTile->imageData,
        rawTile->dimensions,
        dataLayout.textureFormat.ghoulFormat,
        dataLayout.textureFormat.glFormat,
        dataLayout.glType,
        Texture::FilterMode::Linear,
        Texture::WrappingMode::ClampToEdge);
        
    texture->uploadTexture();

    // AnisotropicMipMap must be set after texture is uploaded. Why?!
    texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

    Tile tile = {
        texture,
        rawTile->tileMetaData,
        Tile::Status::OK
    };

    return tile;
}

} // namespace globebrowsing
} // namespace openspace
