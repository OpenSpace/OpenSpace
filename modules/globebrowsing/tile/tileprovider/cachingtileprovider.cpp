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

#include <modules/globebrowsing/cache/memoryawaretilecache.h>
#include <modules/globebrowsing/rendering/layer/layergroupid.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/tile/asynctiledataprovider.h>
#include <modules/globebrowsing/tile/rawtiledatareader/gdalrawtiledatareader.h>
#include <modules/globebrowsing/tile/rawtiledatareader/simplerawtiledatareader.h>
#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/rawtiledatareader/iodescription.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/texture.h>

namespace {
    const char* _loggerCat = "CachingTileProvider";

    const char* KeyPerformPreProcessing = "PerformPreProcessing";
    const char* KeyTilePixelSize = "TilePixelSize";
    const char* KeyFilePath = "FilePath";
    const char* KeyBasePath = "BasePath";
    const char* KeyFlushInterval = "FlushInterval";
    const char* KeyPreCacheLevel = "PreCacheLevel";
}

namespace openspace {
namespace globebrowsing {
namespace tileprovider {
    
CachingTileProvider::CachingTileProvider(const ghoul::Dictionary& dictionary) 
    : TileProvider(dictionary)
{
    _name = "Name unspecified";
    dictionary.getValue("Name", _name);
    std::string _loggerCat = "CachingTileProvider : " + _name;

    // 1. Get required Keys
    std::string filePath;
    if (!dictionary.getValue<std::string>(KeyFilePath, filePath)) {
        throw std::runtime_error(std::string("Must define key '") + KeyFilePath + "'");
    }

    layergroupid::ID layerGroupID;
    if (!dictionary.getValue<layergroupid::ID>("LayerGroupID", layerGroupID)) {
        ghoul_assert(false, "Unknown layer group id");
    }

    // 2. Initialize default values for any optional Keys
    // getValue does not work for integers
    double pixelSize = 0.0;
    int tilePixelSize = 0;
    if (dictionary.getValue<double>(KeyTilePixelSize, pixelSize)) {
        LDEBUG("Default pixel size overridden: " << pixelSize);
        tilePixelSize = static_cast<int>(pixelSize); 
    }
    
    TileTextureInitData initData(LayerManager::getTileTextureInitData(
        layerGroupID, tilePixelSize));
  
    bool performPreProcessing =
        LayerManager::shouldPerformPreProcessingOnLayergroup(layerGroupID);
    if (dictionary.getValue<bool>(KeyPerformPreProcessing, performPreProcessing)) {
        LDEBUG("Default PerformPreProcessing overridden: " << performPreProcessing);
    }
    RawTileDataReader::PerformPreprocessing preprocess =
        performPreProcessing ? RawTileDataReader::PerformPreprocessing::Yes :
        RawTileDataReader::PerformPreprocessing::No;
    
    std::string basePath;
    dictionary.getValue(KeyBasePath, basePath);

    // Initialize instance variables
#ifdef GLOBEBROWSING_USE_GDAL
    auto tileDataset = std::make_shared<GdalRawTileDataReader>(filePath, initData, basePath, preprocess);
#else // GLOBEBROWSING_USE_GDAL
    auto tileDataset = std::make_shared<SimpleRawTileDataReader>(filePath, initData, preprocess);
#endif // GLOBEBROWSING_USE_GDAL

    _asyncTextureDataProvider = std::make_shared<AsyncTileDataProvider>(tileDataset);

    if (dictionary.hasKeyAndValue<double>(KeyPreCacheLevel)) {
        int preCacheLevel = static_cast<int>(dictionary.value<double>(KeyPreCacheLevel));
        LDEBUG("Precaching '" << filePath << "' with level '" << preCacheLevel << "'");
        for (int level = 0; level <= preCacheLevel; ++level) {
            for (int x = 0; x <= level * 2; ++x) {
                for (int y = 0; y <= level; ++y) {
                    _asyncTextureDataProvider->enqueueTileIO({ x, y, level });
                }
            }
        }
    }
}

CachingTileProvider::CachingTileProvider(
    std::shared_ptr<AsyncTileDataProvider> tileReader)
    : _asyncTextureDataProvider(tileReader)
{ }

CachingTileProvider::~CachingTileProvider()
{ }

void CachingTileProvider::update() {
    _asyncTextureDataProvider->update();
    initTexturesFromLoadedData();
}

void CachingTileProvider::reset() {
    cache::MemoryAwareTileCache::ref().clear();
    _asyncTextureDataProvider->reset();
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

void CachingTileProvider::initTexturesFromLoadedData() {
    std::shared_ptr<RawTile> rawTile = _asyncTextureDataProvider->popFinishedRawTile();
    if (rawTile) {
        cache::ProviderTileKey key = { rawTile->tileIndex, uniqueIdentifier() };
        if (!cache::MemoryAwareTileCache::ref().exist(key)) {
            cache::MemoryAwareTileCache::ref().createTileAndPut(key, rawTile);
        }
        else {
            ghoul_assert(false, "Tile is already existing in cache.");
        }
    }
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

} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace
