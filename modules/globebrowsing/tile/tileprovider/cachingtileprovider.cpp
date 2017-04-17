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

#include <modules/globebrowsing/tile/asynctiledataprovider.h>
#include <modules/globebrowsing/tile/rawtiledatareader/gdalrawtiledatareader.h>
#include <modules/globebrowsing/tile/rawtiledatareader/simplerawtiledatareader.h>
#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/cache/memoryawaretilecache.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>

namespace {
    const char* _loggerCat = "CachingTileProvider";

    const char* KeyDoPreProcessing = "DoPreProcessing";
    const char* KeyTilePixelSize = "TilePixelSize";
    const char* KeyFilePath = "FilePath";
    const char* KeyFlushInterval = "FlushInterval";
}

namespace openspace {
namespace globebrowsing {
namespace tileprovider {
    
CachingTileProvider::CachingTileProvider(const ghoul::Dictionary& dictionary) 
    : TileProvider(dictionary)
    , _framesSinceLastRequestFlush(0)
    , _defaultTile(Tile::TileUnavailable)
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
    RawTileDataReader::Configuration config;
    config.doPreProcessing = false;
    config.tilePixelSize = 512;
        
    // getValue does not work for integers
    double minimumPixelSize;
    double framesUntilRequestFlush = 60;

    // 3. Check for used spcified optional keys
    if (dictionary.getValue<bool>(KeyDoPreProcessing, config.doPreProcessing)) {
        LDEBUG("Default doPreProcessing overridden: " << config.doPreProcessing);
    }
    if (dictionary.getValue<double>(KeyTilePixelSize, minimumPixelSize)) {
        LDEBUG("Default minimumPixelSize overridden: " << minimumPixelSize);
        config.tilePixelSize = static_cast<int>(minimumPixelSize); 
    }
    if (dictionary.getValue<double>(KeyFlushInterval, framesUntilRequestFlush)) {
        LDEBUG("Default framesUntilRequestFlush overridden: " <<
            framesUntilRequestFlush);
    }

    // Initialize instance variables
#ifdef GLOBEBROWSING_USE_GDAL
    auto tileDataset = std::make_shared<GdalRawTileDataReader>(filePath, config);
#else // GLOBEBROWSING_USE_GDAL
    auto tileDataset = std::make_shared<SimpleRawTileDataReader>(filePath, config);
#endif // GLOBEBROWSING_USE_GDAL

    // only one thread per provider supported atm
    // (GDAL does not handle multiple threads for a single dataset very well
    // currently)
    auto threadPool = std::make_shared<ThreadPool>(1);

    _asyncTextureDataProvider = std::make_shared<AsyncTileDataProvider>(
        tileDataset, threadPool);
    _framesUntilRequestFlush = framesUntilRequestFlush;
}

CachingTileProvider::CachingTileProvider(
                                        std::shared_ptr<AsyncTileDataProvider> tileReader, 
                                        int framesUntilFlushRequestQueue)
    : _asyncTextureDataProvider(tileReader)
    , _framesUntilRequestFlush(framesUntilFlushRequestQueue)
    , _framesSinceLastRequestFlush(0)
    , _defaultTile(Tile::TileUnavailable)
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
    cache::MemoryAwareTileCache::ref().clear();
    //_asyncTextureDataProvider->reset();
}

int CachingTileProvider::maxLevel() {
    return _asyncTextureDataProvider->getRawTileDataReader()->maxChunkLevel();
}

Tile CachingTileProvider::getTile(const TileIndex& tileIndex) {
    if (tileIndex.level > maxLevel()) {
        return Tile(nullptr, nullptr, Tile::Status::OutOfRange);
    }

    cache::ProviderTileKey key = { tileIndex, uniqueIdentifier() };

    if (cache::MemoryAwareTileCache::ref().exist(key)) {
        return cache::MemoryAwareTileCache::ref().get(key);
    }
    else {
        _asyncTextureDataProvider->enqueueTileIO(tileIndex);
    }
        
    return Tile::TileUnavailable;
}

float CachingTileProvider::noDataValueAsFloat() {
    return _asyncTextureDataProvider->noDataValueAsFloat();
}

Tile CachingTileProvider::getDefaultTile() {
    if (_defaultTile.texture() == nullptr) {
        _defaultTile = createTile(
            _asyncTextureDataProvider->getRawTileDataReader()->defaultTileData()
        );
    }
    return _defaultTile;
}

void CachingTileProvider::initTexturesFromLoadedData() {
    std::shared_ptr<RawTile> rawTile = _asyncTextureDataProvider->popFinishedRawTile();
    if (rawTile) {
        cache::ProviderTileKey key = { rawTile->tileIndex, uniqueIdentifier() };
        Tile tile = createTile(rawTile);
        cache::MemoryAwareTileCache::ref().put(key, tile);
    }
}

void CachingTileProvider::clearRequestQueue() {
    _asyncTextureDataProvider->clearRequestQueue();
    _framesSinceLastRequestFlush = 0;
}

Tile::Status CachingTileProvider::getTileStatus(const TileIndex& tileIndex) {
    auto rawTileDataReader = _asyncTextureDataProvider->getRawTileDataReader();
    if (tileIndex.level > rawTileDataReader->maxChunkLevel()) {
        return Tile::Status::OutOfRange;
    }

    cache::ProviderTileKey key = { tileIndex, uniqueIdentifier() };

    if (cache::MemoryAwareTileCache::ref().exist(key)) {
        return cache::MemoryAwareTileCache::ref().get(key).status();
    }

    return Tile::Status::Unavailable;
}

TileDepthTransform CachingTileProvider::depthTransform() {
    return _asyncTextureDataProvider->getRawTileDataReader()->getDepthTransform();
}

Tile CachingTileProvider::createTile(std::shared_ptr<RawTile> rawTile) {
    if (rawTile->error != RawTile::ReadError::None) {
        return Tile(nullptr, nullptr, Tile::Status::IOError);
    }

    //TileDataLayout dataLayout =
    //   _asyncTextureDataProvider->getTextureDataProvider()->getDataLayout();
        
    // The texture should take ownership of the data
    using ghoul::opengl::Texture;
    std::shared_ptr<Texture> texture = std::make_shared<Texture>(
        rawTile->imageData,
        rawTile->dimensions,
        rawTile->textureFormat.ghoulFormat,
        rawTile->textureFormat.glFormat,
        rawTile->glType,
        Texture::FilterMode::Linear,
        Texture::WrappingMode::ClampToEdge);
        
    texture->uploadTexture();

    // AnisotropicMipMap must be set after texture is uploaded
    texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

    return Tile(texture, rawTile->tileMetaData, Tile::Status::OK);
}

} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace
