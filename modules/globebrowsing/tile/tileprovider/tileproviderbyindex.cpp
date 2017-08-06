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

#include <modules/globebrowsing/tile/tileprovider/tileproviderbyindex.h>

#include <ghoul/misc/dictionary.h>

namespace {
    const char* _loggerCat = "TileProviderByIndex";
    
    const char* KeyDefaultProvider = "DefaultProvider";
    const char* KeyProviders = "IndexTileProviders";
    const char* KeyTileIndex = "TileIndex";
    const char* KeyTileProvider = "TileProvider";
} // namespace

namespace openspace::globebrowsing::tileprovider {

TileProviderByIndex::TileProviderByIndex(const ghoul::Dictionary& dictionary) {
    ghoul::Dictionary defaultProviderDict = dictionary.value<ghoul::Dictionary>(
        KeyDefaultProvider
        );

    std::string typeString;
    defaultProviderDict.getValue("Type", typeString);
    layergroupid::TypeID typeID = layergroupid::TypeID::Unknown;
    if (typeString.empty()) {
        typeID = layergroupid::TypeID::DefaultTileLayer;
    }
    else {
        typeID = layergroupid::getTypeIDFromTypeString(typeString);
    }

    if (typeID == layergroupid::TypeID::Unknown) {
        throw ghoul::RuntimeError("Unknown layer type: " + typeString);
    }
    
    _defaultTileProvider = TileProvider::createFromDictionary(
        typeID, defaultProviderDict
    );
    
    ghoul::Dictionary indexProvidersDict = dictionary.value<ghoul::Dictionary>(
        KeyProviders
        );
    for (size_t i = 0; i < indexProvidersDict.size(); i++) {
        std::string dictKey = std::to_string(i + 1);
        ghoul::Dictionary indexProviderDict = indexProvidersDict.value<ghoul::Dictionary>(
            dictKey
            );
        ghoul::Dictionary tileIndexDict = indexProviderDict.value<ghoul::Dictionary>(
            KeyTileIndex
            );
        ghoul::Dictionary providerDict = indexProviderDict.value<ghoul::Dictionary>(
            KeyTileProvider
            );
            
        TileIndex tileIndex(tileIndexDict);
      
        std::string providerTypeString;
        defaultProviderDict.getValue("Type", providerTypeString);
        layergroupid::TypeID providerTypeID = layergroupid::TypeID::Unknown;
        if (providerTypeString.empty()) {
            providerTypeID = layergroupid::TypeID::DefaultTileLayer;
        }
        else {
            providerTypeID = layergroupid::getTypeIDFromTypeString(providerTypeString);
        }

        if (providerTypeID == layergroupid::TypeID::Unknown) {
            throw ghoul::RuntimeError("Unknown layer type: " + providerTypeString);
        }
    
        std::shared_ptr<TileProvider> stp = TileProvider::createFromDictionary(
            providerTypeID,
            providerDict
        );
        TileIndex::TileHashKey key = tileIndex.hashKey();
        _tileProviderMap.insert(std::make_pair(key, stp));
    }
}

Tile TileProviderByIndex::getTile(const TileIndex& tileIndex) {
    auto it = _tileProviderMap.find(tileIndex.hashKey());
    bool hasProvider = it != _tileProviderMap.end();
    return hasProvider ? it->second->getTile(tileIndex) : Tile::TileUnavailable;
}

Tile::Status TileProviderByIndex::getTileStatus(const TileIndex& tileIndex) {
    auto it = _tileProviderMap.find(tileIndex.hashKey());
    bool hasProvider = it != _tileProviderMap.end();
    return hasProvider ? it->second->getTileStatus(tileIndex) : Tile::Status::Unavailable;
}

TileDepthTransform TileProviderByIndex::depthTransform() {
    return _defaultTileProvider->depthTransform();
}

void TileProviderByIndex::update() {
    for (auto& it : _tileProviderMap){
        it.second->update();
    }
    _defaultTileProvider->update();
}

void TileProviderByIndex::reset() {
    for (auto& it : _tileProviderMap) {
        it.second->reset();
    }
    _defaultTileProvider->reset();
}

int TileProviderByIndex::maxLevel() {
    return _defaultTileProvider->maxLevel();
}

TileProvider* TileProviderByIndex::indexProvider(const TileIndex& tileIndex) const {
    auto it = _tileProviderMap.find(tileIndex.hashKey());
    return (it != _tileProviderMap.end()) ? it->second.get() : nullptr;
}

} // namespace openspace::globebrowsing::tileprovider
