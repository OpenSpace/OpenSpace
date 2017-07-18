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

#include <modules/globebrowsing/tile/tileprovider/defaulttileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/cache/memoryawaretilecache.h>
#include <modules/globebrowsing/rendering/layer/layergroupid.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/tile/asynctiledataprovider.h>
#include <modules/globebrowsing/tile/rawtiledatareader/gdalrawtiledatareader.h>
#include <modules/globebrowsing/tile/rawtiledatareader/simplerawtiledatareader.h>
#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/rawtiledatareader/iodescription.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/texture.h>

namespace {
    const char* KeyPerformPreProcessing = "PerformPreProcessing";
    const char* KeyTilePixelSize = "TilePixelSize";
    const char* KeyFilePath = "FilePath";
    const char* KeyBasePath = "BasePath";
    const char* KeyPreCacheLevel = "PreCacheLevel";
}

namespace openspace {
namespace globebrowsing {
namespace tileprovider {
    
DefaultTileProvider::DefaultTileProvider(const ghoul::Dictionary& dictionary) 
    : TileProvider(dictionary)
    , _filePath("filePath", "File Path", "")
    , _tilePixelSize("tilePixelSize", "Tile Pixel Size", 32, 32, 1024)
    , _preCacheLevel(0)
{
    _tileCache = OsEng.moduleEngine().module<GlobeBrowsingModule>()->tileCache();
    _name = "Name unspecified";
    dictionary.getValue("Name", _name);
    std::string _loggerCat = "DefaultTileProvider : " + _name;

    // 1. Get required Keys
    std::string filePath;
    dictionary.getValue<std::string>(KeyFilePath, filePath);
    //filePath = absPath(filePath);
    _filePath.setValue(filePath);

    if (!dictionary.getValue<layergroupid::GroupID>("LayerGroupID", _layerGroupID)) {
        ghoul_assert(false, "Unknown layer group id");
    }

    // 2. Initialize default values for any optional Keys
    // getValue does not work for integers
    double pixelSize = 0.0;
    int tilePixelSize = 0;
    if (dictionary.getValue<double>(KeyTilePixelSize, pixelSize)) {
        LDEBUG("Default pixel size overridden: " << pixelSize);
        tilePixelSize = pixelSize; 
    }
    TileTextureInitData initData(LayerManager::getTileTextureInitData(
        _layerGroupID, tilePixelSize));
    _tilePixelSize.setValue(initData.dimensionsWithoutPadding().x);

    _performPreProcessing =
        LayerManager::shouldPerformPreProcessingOnLayergroup(_layerGroupID);
    if (dictionary.getValue<bool>(KeyPerformPreProcessing, _performPreProcessing)) {
        LDEBUG("Default PerformPreProcessing overridden: " << _performPreProcessing);
    }

    if (dictionary.hasKeyAndValue<double>(KeyPreCacheLevel)) {
        _preCacheLevel = static_cast<int>(dictionary.value<double>(KeyPreCacheLevel));
    }

    dictionary.getValue(KeyBasePath, _basePath);

    initAsyncTileDataReader(initData);

    // Properties
    addProperty(_filePath);
    addProperty(_tilePixelSize);
}

DefaultTileProvider::DefaultTileProvider(
    std::shared_ptr<AsyncTileDataProvider> tileReader)
    : _asyncTextureDataProvider(tileReader)
    , _filePath("filePath", "File Path", "")
    , _tilePixelSize("tilePixelSize", "Tile Pixel Size", 32, 32, 1024)
{ }

DefaultTileProvider::~DefaultTileProvider()
{ }

void DefaultTileProvider::update() {
    if (_asyncTextureDataProvider) {
        _asyncTextureDataProvider->update();
        initTexturesFromLoadedData();
        if (_asyncTextureDataProvider->shouldBeDeleted()) {
            _asyncTextureDataProvider = nullptr;
            TileTextureInitData initData(LayerManager::getTileTextureInitData(
                _layerGroupID, _tilePixelSize));
            initAsyncTileDataReader(initData);
        }
    }
}

void DefaultTileProvider::reset() {
    _tileCache->clear();
    if (_asyncTextureDataProvider) {
        _asyncTextureDataProvider->prepairToBeDeleted();
    }
    else {
        TileTextureInitData initData(LayerManager::getTileTextureInitData(
            _layerGroupID, _tilePixelSize));
        initAsyncTileDataReader(initData);
    }
}

int DefaultTileProvider::maxLevel() {
    if (_asyncTextureDataProvider) {
        return _asyncTextureDataProvider->getRawTileDataReader()->maxChunkLevel();
    }
    else {
        // Current theoretical maximum based on the number of hashes that are possible
        // to uniquely identify a tile. See ProviderTileHasher in memoryawaretilecache.h
        return 22;
    }
}

Tile DefaultTileProvider::getTile(const TileIndex& tileIndex) {
    if (_asyncTextureDataProvider) {
        if (tileIndex.level > maxLevel()) {
            return Tile(nullptr, nullptr, Tile::Status::OutOfRange);
        }

        cache::ProviderTileKey key = { tileIndex, uniqueIdentifier() };

        Tile tile = _tileCache->get(key);

        if (tile.texture() == nullptr) {
            _asyncTextureDataProvider->enqueueTileIO(tileIndex);
        }

        return tile;
    }
    else {
        return Tile(nullptr, nullptr, Tile::Status::Unavailable);
    }
}

float DefaultTileProvider::noDataValueAsFloat() {
    if (_asyncTextureDataProvider) {
        return _asyncTextureDataProvider->noDataValueAsFloat();
    }
    else {
        return std::numeric_limits<float>::min();
    }
}

void DefaultTileProvider::initTexturesFromLoadedData() {
    if (_asyncTextureDataProvider) {
        std::shared_ptr<RawTile> rawTile = _asyncTextureDataProvider->popFinishedRawTile();
        if (rawTile) {
            cache::ProviderTileKey key = { rawTile->tileIndex, uniqueIdentifier() };
            ghoul_assert(!_tileCache->exist(key), "Tile must not be existing in cache");
            _tileCache->createTileAndPut(key, rawTile);
        }
    }
}

void DefaultTileProvider::initAsyncTileDataReader(TileTextureInitData initData) {
    std::string _loggerCat = "DefaultTileProvider : " + _name;

    RawTileDataReader::PerformPreprocessing preprocess =
        _performPreProcessing ? RawTileDataReader::PerformPreprocessing::Yes :
        RawTileDataReader::PerformPreprocessing::No;

    // Initialize instance variables
#ifdef GLOBEBROWSING_USE_GDAL
    auto tileDataset = std::make_shared<GdalRawTileDataReader>(_filePath, initData,
                                                               _basePath, preprocess);
#else // GLOBEBROWSING_USE_GDAL
    auto tileDataset = std::make_shared<SimpleRawTileDataReader>(_filePath, initData,
                                                                 preprocess);
#endif // GLOBEBROWSING_USE_GDAL

    _asyncTextureDataProvider = std::make_shared<AsyncTileDataProvider>(_name, tileDataset);

    // Tiles are only available for levels 2 and higher.
    if (_preCacheLevel >= 2) {
        LDEBUG("Precaching '" << _filePath << "' with level '" << _preCacheLevel << "'");
        for (int level = 0; level <= _preCacheLevel; ++level) {
            for (int x = 0; x <= level * 2; ++x) {
                for (int y = 0; y <= level; ++y) {
                    _asyncTextureDataProvider->enqueueTileIO({ x, y, level });
                }
            }
        }
    }
}

Tile::Status DefaultTileProvider::getTileStatus(const TileIndex& tileIndex) {
    if (_asyncTextureDataProvider) {
        auto rawTileDataReader = _asyncTextureDataProvider->getRawTileDataReader();
        if (tileIndex.level > rawTileDataReader->maxChunkLevel()) {
            return Tile::Status::OutOfRange;
        }

        cache::ProviderTileKey key = { tileIndex, uniqueIdentifier() };

        return _tileCache->get(key).status();
    }
    else {
        return Tile::Status::Unavailable;
    }
}

TileDepthTransform DefaultTileProvider::depthTransform() {
    if (_asyncTextureDataProvider) {
        return _asyncTextureDataProvider->getRawTileDataReader()->getDepthTransform();
    }
    else {
        return { 1.0f, 0.0f };
    }
}

} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace
