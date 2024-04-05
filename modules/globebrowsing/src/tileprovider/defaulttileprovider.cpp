/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/globebrowsing/src/tileprovider/defaulttileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "The path of the GDAL file or the image file that is to be used in this tile "
        "provider",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TilePixelSizeInfo = {
        "TilePixelSize",
        "Tile Pixel Size",
        "This value is the preferred size (in pixels) for each tile. Choosing the right "
        "value is a tradeoff between more efficiency (larger images) and better quality "
        "(smaller images). The tile pixel size has to be smaller than the size of the "
        "complete image if a single image is used",
        // @VISIBILITY(3.33)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo CompressionInfo = {
        "Compression",
        "Compression Algorithm",
        "The compression algorithm to use for MRF cached tiles",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    enum class [[codegen::stringify()]] Compression {
        PNG = 0,
        JPEG,
        LERC
    };

    struct [[codegen::Dictionary(DefaultTileProvider)]] Parameters {
        // User-facing name of this tile provider
        std::optional<std::string> name;

        // Identifier of the enclosing layer to which tiles are provided
        std::optional<std::string> identifier;

        // The path to the file that is loaded by GDAL to produce tiles. Since GDAL
        // supports it, this can also be the textual representation of the contents of a
        // loading file
        std::string filePath;

        // The layer into which this tile provider is loaded
        int layerGroupID;

        // [[codegen::verbatim(TilePixelSizeInfo.description)]]
        std::optional<int> tilePixelSize;

        // Determines if the tiles should be preprocessed before uploading to the GPU
        std::optional<bool> performPreProcessing;

        struct CacheSettings {
            // Specifies whether to use caching or not
            std::optional<bool> enabled;

            // [[codegen::verbatim(CompressionInfo.description)]]
            enum class [[codegen::map(Compression)]] Compression {
                PNG = 0,
                JPEG,
                LERC
            };

            // The compression algorithm to use for cached tiles
            std::optional<Compression> compression;

            // The quality setting of the compression alogrithm, only valid for JPEG
            std::optional<int> quality [[codegen::inrange(0, 100)]];

            // The block-size of the MRF cache
            std::optional<int> blockSize [[codegen::greater(0)]];
        };
        // Specifies the cache settings that should be applied to this layer
        std::optional<CacheSettings> cacheSettings;

        // The name of the enclosing globe
        std::optional<std::string> globeName;

    };
#include "defaulttileprovider_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation DefaultTileProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_defaulttileprovider");
}

DefaultTileProvider::DefaultTileProvider(const ghoul::Dictionary& dictionary)
    : _filePath(FilePathInfo, "")
    , _tilePixelSize(TilePixelSizeInfo, 32, 32, 2048)
{
    ZoneScoped;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    name = p.name.value_or("Name unspecified");
    const std::string _loggerCat = std::format("DefaultTileProvider ({})", name);

    // 1. Get required Keys
    _filePath = p.filePath;

    _layerGroupID = layers::Group::ID(p.layerGroupID);

    // 2. Initialize default values for any optional Keys
    // getValue does not work for integers
    const int pixelSize = p.tilePixelSize.value_or(0);

    // Only preprocess height layers by default
    _performPreProcessing = (_layerGroupID == layers::Group::ID::HeightLayers);
    _performPreProcessing = p.performPreProcessing.value_or(_performPreProcessing);

    // Get the name of the layergroup to which this layer belongs
    auto it = std::find_if(
        layers::Groups.begin(),
        layers::Groups.end(),
        [id = _layerGroupID](const layers::Group& gi) {
            return gi.id == id;
        }
    );

    std::string layerGroup =
        it != layers::Groups.end() ?
        std::string(it->name) :
        std::to_string(static_cast<int>(_layerGroupID));

    std::string identifier = p.identifier.value_or("unspecified");
    std::string enclosing = p.globeName.value_or("unspecified");

    std::string path = std::format("{}/{}/{}/", enclosing, layerGroup, identifier);

    const GlobeBrowsingModule& mod = *global::moduleEngine->module<GlobeBrowsingModule>();
    bool enabled = mod.isMRFCachingEnabled();
    Compression compression =
        _layerGroupID == layers::Group::ID::HeightLayers ?
        Compression::LERC :
        Compression::JPEG;
    int quality = 75;
    int blockSize = 1024;
    if (p.cacheSettings.has_value()) {
        enabled = p.cacheSettings->enabled.value_or(enabled);
        if (p.cacheSettings->compression.has_value()) {
            compression = codegen::map<Compression>(*p.cacheSettings->compression);
        }
        quality = p.cacheSettings->quality.value_or(quality);
        blockSize = p.cacheSettings->blockSize.value_or(blockSize);
    }

    _cacheProperties.enabled = enabled;
    _cacheProperties.path = std::move(path);
    _cacheProperties.quality = quality;
    _cacheProperties.blockSize = blockSize;
    _cacheProperties.compression = codegen::toString(compression);

    TileTextureInitData initData = TileTextureInitData(
        tileTextureInitData(_layerGroupID, pixelSize)
    );
    _tilePixelSize = initData.dimensions.x;
    initAsyncTileDataReader(std::move(initData), _cacheProperties);

    addProperty(_filePath);
    addProperty(_tilePixelSize);
}

void DefaultTileProvider::initAsyncTileDataReader(TileTextureInitData initData,
                                                  TileCacheProperties cacheProperties)
{
    ZoneScoped;

    _asyncTextureDataProvider = std::make_unique<AsyncTileDataProvider>(
        name,
        std::make_unique<RawTileDataReader>(
            _filePath,
            std::move(initData),
            std::move(cacheProperties),
            RawTileDataReader::PerformPreprocessing(_performPreProcessing)
        )
    );
}

Tile DefaultTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped;

    ghoul_assert(_asyncTextureDataProvider, "No data provider");
    if (tileIndex.level > maxLevel()) {
        return Tile{ nullptr, std::nullopt, Tile::Status::OutOfRange };
    }
    const cache::ProviderTileKey key = {
        .tileIndex = tileIndex,
        .providerID = uniqueIdentifier
    };
    cache::MemoryAwareTileCache* tileCache =
        global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();
    Tile tile = tileCache->get(key);
    if (!tile.texture) {
        _asyncTextureDataProvider->enqueueTileIO(tileIndex);
    }

    return tile;
}

Tile::Status DefaultTileProvider::tileStatus(const TileIndex& index) {
    ghoul_assert(_asyncTextureDataProvider, "No data provider");
    const RawTileDataReader& reader = _asyncTextureDataProvider->rawTileDataReader();

    if (index.level > reader.maxChunkLevel()) {
        return Tile::Status::OutOfRange;
    }

    const cache::ProviderTileKey key = {
        .tileIndex = index,
        .providerID = uniqueIdentifier
    };
    cache::MemoryAwareTileCache* tileCache =
        global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();
    return tileCache->get(key).status;
}

TileDepthTransform DefaultTileProvider::depthTransform() {
    ghoul_assert(_asyncTextureDataProvider, "No data provider");
    return _asyncTextureDataProvider->rawTileDataReader().depthTransform();
}

void DefaultTileProvider::update() {
    ghoul_assert(_asyncTextureDataProvider, "No data provider");
    _asyncTextureDataProvider->update();

    std::optional<RawTile> tile = _asyncTextureDataProvider->popFinishedRawTile();
    if (tile) {
        const cache::ProviderTileKey key = {
            .tileIndex = tile->tileIndex,
            .providerID = uniqueIdentifier
        };
        cache::MemoryAwareTileCache* tileCache =
            global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();
        ghoul_assert(!tileCache->exist(key), "Tile must not be existing in cache");
        tileCache->createTileAndPut(key, std::move(*tile));
    }

    if (_asyncTextureDataProvider->shouldBeDeleted()) {
        initAsyncTileDataReader(
            tileTextureInitData(_layerGroupID, _tilePixelSize),
            _cacheProperties
        );
    }
}

void DefaultTileProvider::reset() {
    global::moduleEngine->module<GlobeBrowsingModule>()->tileCache()->clear();
    ghoul_assert(_asyncTextureDataProvider, "No data provider");
    _asyncTextureDataProvider->prepareToBeDeleted();
}

int DefaultTileProvider::minLevel() {
    ghoul_assert(_asyncTextureDataProvider, "No data provider");
    return 1;
}

int DefaultTileProvider::maxLevel() {
    ghoul_assert(_asyncTextureDataProvider, "No data provider");
    return _asyncTextureDataProvider->rawTileDataReader().maxChunkLevel();
}

float DefaultTileProvider::noDataValueAsFloat() {
    ghoul_assert(_asyncTextureDataProvider, "No data provider");
    return _asyncTextureDataProvider->noDataValueAsFloat();
}

} // namespace openspace::globebrowsing
