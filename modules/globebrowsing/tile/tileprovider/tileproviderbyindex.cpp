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

#include <modules/globebrowsing/tile/tileprovider/tileproviderbyindex.h>

#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <ghoul/misc/dictionary.h>

namespace {
    constexpr const char* KeyDefaultProvider = "DefaultProvider";
    constexpr const char* KeyProviders = "IndexTileProviders";
    constexpr const char* KeyTileIndex = "TileIndex";
    constexpr const char* KeyTileProvider = "TileProvider";
} // namespace

namespace openspace::globebrowsing::tileprovider {

TileProviderByIndex::TileProviderByIndex(const ghoul::Dictionary& dictionary) {
    const ghoul::Dictionary& defaultProviderDict = dictionary.value<ghoul::Dictionary>(
        KeyDefaultProvider
    );

    layergroupid::TypeID type;
    if (defaultProviderDict.hasKeyAndValue<std::string>("Type")) {
        const std::string& t = defaultProviderDict.value<std::string>("Type");
        type = layergroupid::getTypeIDFromTypeString(t);

        if (type == layergroupid::TypeID::Unknown) {
            throw ghoul::RuntimeError("Unknown layer type: " + t);
        }
    }
    else {
        type = layergroupid::TypeID::DefaultTileLayer;
    }

    _defaultTileProvider = TileProvider::createFromDictionary(type, defaultProviderDict);

    const ghoul::Dictionary& indexProvidersDict = dictionary.value<ghoul::Dictionary>(
        KeyProviders
    );
    for (size_t i = 0; i < indexProvidersDict.size(); i++) {
        ghoul::Dictionary indexProviderDict = indexProvidersDict.value<ghoul::Dictionary>(
            std::to_string(i + 1)
        );
        ghoul::Dictionary tileIndexDict = indexProviderDict.value<ghoul::Dictionary>(
            KeyTileIndex
        );
        ghoul::Dictionary providerDict = indexProviderDict.value<ghoul::Dictionary>(
            KeyTileProvider
        );

        const TileIndex tileIndex(tileIndexDict);

        layergroupid::TypeID providerTypeID;
        if (defaultProviderDict.hasKeyAndValue<std::string>("Type")) {
            const std::string& t = defaultProviderDict.value<std::string>("Type");
            providerTypeID = layergroupid::getTypeIDFromTypeString(t);

            if (providerTypeID == layergroupid::TypeID::Unknown) {
                throw ghoul::RuntimeError("Unknown layer type: " + t);
            }
        }
        else {
            providerTypeID = layergroupid::TypeID::DefaultTileLayer;
        }

        std::shared_ptr<TileProvider> stp = TileProvider::createFromDictionary(
            providerTypeID,
            providerDict
        );
        TileIndex::TileHashKey key = tileIndex.hashKey();
        _tileProviderMap.insert(std::make_pair(key, stp));
    }
}

Tile TileProviderByIndex::tile(const TileIndex& tileIndex) {
    const auto it = _tileProviderMap.find(tileIndex.hashKey());
    const bool hasProvider = it != _tileProviderMap.end();
    return hasProvider ? it->second->tile(tileIndex) : Tile::TileUnavailable;
}

Tile::Status TileProviderByIndex::tileStatus(const TileIndex& tileIndex) {
    const auto it = _tileProviderMap.find(tileIndex.hashKey());
    const bool hasProvider = it != _tileProviderMap.end();
    return hasProvider ? it->second->tileStatus(tileIndex) : Tile::Status::Unavailable;
}

TileDepthTransform TileProviderByIndex::depthTransform() {
    return _defaultTileProvider->depthTransform();
}

void TileProviderByIndex::update() {
    using K = TileIndex::TileHashKey;
    using V = std::shared_ptr<TileProvider>;
    for (std::pair<const K, V>& it : _tileProviderMap) {
        it.second->update();
    }
    _defaultTileProvider->update();
}

void TileProviderByIndex::reset() {
    using K = TileIndex::TileHashKey;
    using V = std::shared_ptr<TileProvider>;
    for (std::pair<const K, V>& it : _tileProviderMap) {
        it.second->reset();
    }
    _defaultTileProvider->reset();
}

int TileProviderByIndex::maxLevel() {
    return _defaultTileProvider->maxLevel();
}

TileProvider* TileProviderByIndex::indexProvider(const TileIndex& tileIndex) const {
    const auto it = _tileProviderMap.find(tileIndex.hashKey());
    return (it != _tileProviderMap.end()) ? it->second.get() : nullptr;
}

} // namespace openspace::globebrowsing::tileprovider
