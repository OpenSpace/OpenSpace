/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <modules/globebrowsing/src/asynctiledataprovider.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>

namespace {
    constexpr const char* KeyFilePath = "FilePath";
    constexpr const char* KeyPerformPreProcessing = "PerformPreProcessing";
    constexpr const char* KeyTilePixelSize = "TilePixelSize";
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
} // namespace

namespace openspace::globebrowsing {

DefaultTileProvider::DefaultTileProvider(const ghoul::Dictionary& dictionary)
    : filePath(FilePathInfo, "")
    , tilePixelSize(TilePixelSizeInfo, 32, 32, 2048)
{
    ZoneScoped

    tileCache = global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();
    name = "Name unspecified";
    if (dictionary.hasValue<std::string>("Name")) {
        name = dictionary.value<std::string>("Name");
    }
    std::string _loggerCat = "DefaultTileProvider (" + name + ")";

    // 1. Get required Keys
    filePath = dictionary.value<std::string>(KeyFilePath);
    layerGroupID =
        static_cast<layergroupid::GroupID>(dictionary.value<int>("LayerGroupID"));

    // 2. Initialize default values for any optional Keys
    // getValue does not work for integers
    int pixelSize = 0;
    if (dictionary.hasValue<double>(KeyTilePixelSize)) {
        pixelSize = static_cast<int>(dictionary.value<double>(KeyTilePixelSize));
        LDEBUG(fmt::format("Default pixel size overridden: {}", pixelSize));
    }

    if (dictionary.hasValue<bool>(KeyPadTiles)) {
        padTiles = dictionary.value<bool>(KeyPadTiles);
    }

    TileTextureInitData initData(
        tileTextureInitData(layerGroupID, padTiles, pixelSize)
    );
    tilePixelSize = initData.dimensions.x;


    // Only preprocess height layers by default
    switch (layerGroupID) {
    case layergroupid::GroupID::HeightLayers: performPreProcessing = true; break;
    default:                                  performPreProcessing = false; break;
    }

    if (dictionary.hasValue<bool>(KeyPerformPreProcessing)) {
        performPreProcessing = dictionary.value<bool>(KeyPerformPreProcessing);
        LDEBUG(fmt::format(
            "Default PerformPreProcessing overridden: {}", performPreProcessing
        ));
    }

    initAsyncTileDataReader(initData);

    addProperty(filePath);
    addProperty(tilePixelSize);
}

void DefaultTileProvider::initAsyncTileDataReader(TileTextureInitData initData) {
    ZoneScoped

    asyncTextureDataProvider = std::make_unique<AsyncTileDataProvider>(
        name,
        std::make_unique<RawTileDataReader>(
            filePath,
            initData,
            RawTileDataReader::PerformPreprocessing(performPreProcessing)
        )
    );
}

Tile DefaultTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped
    if (asyncTextureDataProvider) {
        if (tileIndex.level > maxLevel()) {
            return Tile{ nullptr, std::nullopt, Tile::Status::OutOfRange };
        }
        const cache::ProviderTileKey key = { tileIndex, uniqueIdentifier };
        Tile tile = tileCache->get(key);
        if (!tile.texture) {
            //TracyMessage("Enqueuing tile", 32);
            asyncTextureDataProvider->enqueueTileIO(tileIndex);
        }

        return tile;
    }
    else {
        return Tile{ nullptr, std::nullopt, Tile::Status::Unavailable };
    }
}

Tile::Status DefaultTileProvider::tileStatus(const TileIndex& index) {
    if (asyncTextureDataProvider) {
        const RawTileDataReader& reader = asyncTextureDataProvider->rawTileDataReader();

        if (index.level > reader.maxChunkLevel()) {
            return Tile::Status::OutOfRange;
        }

        const cache::ProviderTileKey key = { index, uniqueIdentifier };
        return tileCache->get(key).status;
    }
    else {
        return Tile::Status::Unavailable;
    }
}

TileDepthTransform DefaultTileProvider::depthTransform() {
    if (asyncTextureDataProvider) {
        return asyncTextureDataProvider->rawTileDataReader().depthTransform();
    }
    else {
        return { 1.f, 0.f };
    }
}

void DefaultTileProvider::update() {
    if (!asyncTextureDataProvider) {
        return;
    }

    asyncTextureDataProvider->update();

    std::optional<RawTile> tile = asyncTextureDataProvider->popFinishedRawTile();
    if (tile) {
        const cache::ProviderTileKey key = { tile->tileIndex, uniqueIdentifier };
        ghoul_assert(!tileCache->exist(key), "Tile must not be existing in cache");
        tileCache->createTileAndPut(key, std::move(*tile));
    }


    if (asyncTextureDataProvider->shouldBeDeleted()) {
        asyncTextureDataProvider = nullptr;
        initAsyncTileDataReader(
            tileTextureInitData(layerGroupID, padTiles, tilePixelSize)
        );
    }
}

void DefaultTileProvider::reset() {
    tileCache->clear();
    if (asyncTextureDataProvider) {
        asyncTextureDataProvider->prepareToBeDeleted();
    }
    else {
        initAsyncTileDataReader(
            tileTextureInitData(layerGroupID, padTiles, tilePixelSize)
        );
    }
}

int DefaultTileProvider::maxLevel() {
    // 22 is the current theoretical maximum based on the number of hashes that are
    // possible to uniquely identify a tile. See ProviderTileHasher in
    // memoryawaretilecache.h
    return asyncTextureDataProvider ?
        asyncTextureDataProvider->rawTileDataReader().maxChunkLevel() :
        22;
}

float DefaultTileProvider::noDataValueAsFloat() {
    return asyncTextureDataProvider ?
        asyncTextureDataProvider->noDataValueAsFloat() :
        std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
