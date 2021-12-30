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

#include <modules/globebrowsing/src/tileprovider/tileproviderbyindex.h>

namespace {
    constexpr const char* KeyDefaultProvider = "DefaultProvider";
    constexpr const char* KeyProviders = "IndexTileProviders";
    constexpr const char* KeyTileIndex = "TileIndex";
    constexpr const char* KeyTileProvider = "TileProvider";
} // namespace

namespace openspace::globebrowsing {

TileProviderByIndex::TileProviderByIndex(const ghoul::Dictionary& dictionary) {
    ZoneScoped

    const ghoul::Dictionary& defaultProviderDict = dictionary.value<ghoul::Dictionary>(
        KeyDefaultProvider
    );

    layergroupid::TypeID typeID;
    if (defaultProviderDict.hasValue<std::string>("Type")) {
        const std::string& t = defaultProviderDict.value<std::string>("Type");
        typeID = ghoul::from_string<layergroupid::TypeID>(t);

        if (typeID == layergroupid::TypeID::Unknown) {
            throw ghoul::RuntimeError("Unknown layer type: " + t);
        }
    }
    else {
        typeID = layergroupid::TypeID::DefaultTileLayer;
    }

    defaultTileProvider = createFromDictionary(typeID, defaultProviderDict);

    const ghoul::Dictionary& indexProvidersDict = dictionary.value<ghoul::Dictionary>(
        KeyProviders
    );
    for (size_t i = 1; i <= indexProvidersDict.size(); i++) {
        ghoul::Dictionary indexProviderDict = indexProvidersDict.value<ghoul::Dictionary>(
            std::to_string(i)
            );
        ghoul::Dictionary tileIndexDict = indexProviderDict.value<ghoul::Dictionary>(
            KeyTileIndex
        );
        ghoul::Dictionary providerDict = indexProviderDict.value<ghoul::Dictionary>(
            KeyTileProvider
        );

        constexpr const char* KeyLevel = "Level";
        constexpr const char* KeyX = "X";
        constexpr const char* KeyY = "Y";

        int level = static_cast<int>(tileIndexDict.value<double>(KeyLevel));
        ghoul_assert(level < std::numeric_limits<uint8_t>::max(), "Level too large");
        int x = static_cast<int>(tileIndexDict.value<double>(KeyX));
        int y = static_cast<int>(tileIndexDict.value<double>(KeyY));

        const TileIndex tileIndex(x, y, static_cast<uint8_t>(level));

        layergroupid::TypeID providerTypeID = layergroupid::TypeID::DefaultTileLayer;
        if (defaultProviderDict.hasValue<std::string>("Type")) {
            const std::string& t = defaultProviderDict.value<std::string>("Type");
            providerTypeID = ghoul::from_string<layergroupid::TypeID>(t);

            if (providerTypeID == layergroupid::TypeID::Unknown) {
                throw ghoul::RuntimeError("Unknown layer type: " + t);
            }
        }

        std::unique_ptr<TileProvider> stp = createFromDictionary(
            providerTypeID,
            providerDict
        );
        TileIndex::TileHashKey key = tileIndex.hashKey();
        tileProviderMap.insert(std::make_pair(key, std::move(stp)));
    }
}

Tile TileProviderByIndex::tile(const TileIndex& tileIndex) {
    ZoneScoped
    const auto it = tileProviderMap.find(tileIndex.hashKey());
    const bool hasProvider = it != tileProviderMap.end();
    return hasProvider ? it->second->tile(tileIndex) : Tile();
}

Tile::Status TileProviderByIndex::tileStatus(const TileIndex& index) {
    const auto it = tileProviderMap.find(index.hashKey());
    const bool hasProvider = it != tileProviderMap.end();
    return hasProvider ? it->second->tileStatus(index) : Tile::Status::Unavailable;
}

TileDepthTransform TileProviderByIndex::depthTransform() {
    return defaultTileProvider->depthTransform();
}

void TileProviderByIndex::update() {
    using K = TileIndex::TileHashKey;
    using V = std::unique_ptr<TileProvider>;
    for (std::pair<const K, V>& it : tileProviderMap) {
        it.second->update();
    }
    defaultTileProvider->update();
}

void TileProviderByIndex::reset() {
    using K = TileIndex::TileHashKey;
    using V = std::unique_ptr<TileProvider>;
    for (std::pair<const K, V>& it : tileProviderMap) {
        it.second->reset();
    }
    defaultTileProvider->reset();
}

int TileProviderByIndex::maxLevel() {
    return defaultTileProvider->maxLevel();
}

float TileProviderByIndex::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
