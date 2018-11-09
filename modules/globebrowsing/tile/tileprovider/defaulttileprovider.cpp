/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/globebrowsing/cache/memoryawaretilecache.h>
#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/tile/asynctiledataprovider.h>
#include <modules/globebrowsing/tile/rawtiledatareader/gdalrawtiledatareader.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* KeyPerformPreProcessing = "PerformPreProcessing";
    constexpr const char* KeyTilePixelSize = "TilePixelSize";
    constexpr const char* KeyFilePath = "FilePath";
    constexpr const char* KeyPreCacheLevel = "PreCacheLevel";
    constexpr const char* KeyPadTiles = "PadTiles";

    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "The path of the GDAL file or the image file that is to be used in this tile "
        "provider."
    };

    constexpr openspace::properties::Property::PropertyInfo TilePixelSizeInfo = {
        "TilePixelSize",
        "Tile Pixel Size",
        "This value is the preferred size (in pixels) for each tile. Choosing the right "
        "value is a tradeoff between more efficiency (larger images) and better quality "
        "(smaller images). The tile pixel size has to be smaller than the size of the "
        "complete image if a single image is used."
    };
}

namespace openspace::globebrowsing::tileprovider {

DefaultTileProvider::DefaultTileProvider(const ghoul::Dictionary& dictionary)
    : TileProvider(dictionary)
    , _filePath(FilePathInfo, "")
    , _tilePixelSize(TilePixelSizeInfo, 32, 32, 2048)
{
    _tileCache = global::moduleEngine.module<GlobeBrowsingModule>()->tileCache();
    _name = "Name unspecified";
    dictionary.getValue("Name", _name);
    std::string _loggerCat = "DefaultTileProvider : " + _name;

    // 1. Get required Keys
    _filePath = dictionary.value<std::string>(KeyFilePath);

    if (!dictionary.getValue<layergroupid::GroupID>("LayerGroupID", _layerGroupID)) {
        ghoul_assert(false, "Unknown layer group id");
    }

    // 2. Initialize default values for any optional Keys
    // getValue does not work for integers
    double pixelSize = 0.0;
    int tilePixelSize = 0;
    if (dictionary.getValue<double>(KeyTilePixelSize, pixelSize)) {
        LDEBUG(fmt::format("Default pixel size overridden: {}", pixelSize));
        tilePixelSize = static_cast<int>(pixelSize);
    }

    dictionary.getValue<bool>(KeyPadTiles, _padTiles);

    TileTextureInitData initData(LayerManager::getTileTextureInitData(
        _layerGroupID,
        LayerManager::PadTiles(_padTiles),
        tilePixelSize
    ));
    _tilePixelSize = initData.dimensions().x;

    _performPreProcessing = LayerManager::shouldPerformPreProcessingOnLayergroup(
        _layerGroupID
    );
    if (dictionary.getValue<bool>(KeyPerformPreProcessing, _performPreProcessing)) {
        LDEBUG(fmt::format(
            "Default PerformPreProcessing overridden: {}", _performPreProcessing
        ));
    }

    if (dictionary.hasKeyAndValue<double>(KeyPreCacheLevel)) {
        _preCacheLevel = static_cast<int>(dictionary.value<double>(KeyPreCacheLevel));
    }

    initAsyncTileDataReader(initData);

    // Properties
    addProperty(_filePath);
    addProperty(_tilePixelSize);
}

DefaultTileProvider::DefaultTileProvider(std::shared_ptr<AsyncTileDataProvider> reader)
    : _asyncTextureDataProvider(std::move(reader))
    , _filePath(FilePathInfo, "")
    , _tilePixelSize(TilePixelSizeInfo, 32, 32, 2048)
{}

DefaultTileProvider::~DefaultTileProvider() {} // NOLINT

void DefaultTileProvider::update() {
    if (_asyncTextureDataProvider) {
        _asyncTextureDataProvider->update();
        initTexturesFromLoadedData();
        if (_asyncTextureDataProvider->shouldBeDeleted()) {
            _asyncTextureDataProvider = nullptr;
            TileTextureInitData initData(LayerManager::getTileTextureInitData(
                _layerGroupID,
                LayerManager::PadTiles(_padTiles),
                _tilePixelSize
            ));
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
            _layerGroupID,
            LayerManager::PadTiles(_padTiles),
            _tilePixelSize
        ));
        initAsyncTileDataReader(initData);
    }
}

int DefaultTileProvider::maxLevel() {
    if (_asyncTextureDataProvider) {
        return _asyncTextureDataProvider->rawTileDataReader()->maxChunkLevel();
    }
    else {
        // Current theoretical maximum based on the number of hashes that are possible
        // to uniquely identify a tile. See ProviderTileHasher in memoryawaretilecache.h
        return 22;
    }
}

Tile DefaultTileProvider::tile(const TileIndex& tileIndex) {
    if (_asyncTextureDataProvider) {
        if (tileIndex.level > maxLevel()) {
            return Tile(nullptr, nullptr, Tile::Status::OutOfRange);
        }

        const cache::ProviderTileKey key = { tileIndex, uniqueIdentifier() };

        const Tile tile = _tileCache->get(key);

        if (!tile.texture()) {
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
        std::shared_ptr<RawTile> tile = _asyncTextureDataProvider->popFinishedRawTile();
        if (tile) {
            const cache::ProviderTileKey key = { tile->tileIndex, uniqueIdentifier() };
            ghoul_assert(!_tileCache->exist(key), "Tile must not be existing in cache");
            _tileCache->createTileAndPut(key, *tile);
        }
    }
}

void DefaultTileProvider::initAsyncTileDataReader(TileTextureInitData initData) {
    const std::string _loggerCat = "DefaultTileProvider : " + _name;

    RawTileDataReader::PerformPreprocessing preprocess =
        _performPreProcessing ? RawTileDataReader::PerformPreprocessing::Yes :
        RawTileDataReader::PerformPreprocessing::No;

    // Initialize instance variables
#ifdef GLOBEBROWSING_USE_GDAL
    std::shared_ptr<GdalRawTileDataReader> tileDataset =
        std::make_shared<GdalRawTileDataReader>(_filePath, initData, preprocess);
#else // GLOBEBROWSING_USE_GDAL
    std::shared_ptr<SimpleRawTileDataReader> tileDataset =
        std::make_shared<SimpleRawTileDataReader>(_filePath, initData, preprocess);
#endif // GLOBEBROWSING_USE_GDAL

    _asyncTextureDataProvider = std::make_shared<AsyncTileDataProvider>(
        _name,
        tileDataset
    );

    // Tiles are only available for levels 2 and higher.
    if (_preCacheLevel >= 2) {
        LDEBUG(fmt::format(
            "Precaching '{}' with level '{}'", _filePath.value(), _preCacheLevel
        ));
        for (int level = 0; level <= _preCacheLevel; ++level) {
            for (int x = 0; x <= level * 2; ++x) {
                for (int y = 0; y <= level; ++y) {
                    _asyncTextureDataProvider->enqueueTileIO({ x, y, level });
                }
            }
        }
    }
}

Tile::Status DefaultTileProvider::tileStatus(const TileIndex& tileIndex) {
    if (_asyncTextureDataProvider) {
        const std::shared_ptr<RawTileDataReader>& rawTileDataReader =
            _asyncTextureDataProvider->rawTileDataReader();

        if (tileIndex.level > rawTileDataReader->maxChunkLevel()) {
            return Tile::Status::OutOfRange;
        }

        const cache::ProviderTileKey key = { tileIndex, uniqueIdentifier() };

        return _tileCache->get(key).status();
    }
    else {
        return Tile::Status::Unavailable;
    }
}

TileDepthTransform DefaultTileProvider::depthTransform() {
    if (_asyncTextureDataProvider) {
        return _asyncTextureDataProvider->rawTileDataReader()->depthTransform();
    }
    else {
        return { 1.f, 0.f };
    }
}

} // namespace openspace::globebrowsing::tileprovider
