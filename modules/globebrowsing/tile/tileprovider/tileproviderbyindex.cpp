/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/tile/tileprovider/tileproviderbyindex.h>
#include <modules/globebrowsing/tile/tileindex.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <openspace/engine/openspaceengine.h>

namespace {
    const std::string _loggerCat = "TileProviderByIndex";
    
    const std::string KeyDefaultProvider = "DefaultProvider";
    const std::string KeyProviders = "IndexTileProviders";
    const std::string KeyTileIndex = "TileIndex";
    const std::string KeyTileProvider = "TileProvider";
}

namespace openspace {
namespace globebrowsing {

    TileProviderByIndex::TileProviderByIndex(const ghoul::Dictionary& dictionary) {
        ghoul::Dictionary defaultProviderDict = dictionary.value<ghoul::Dictionary>(KeyDefaultProvider);
        TileProvider * defaultProvider = TileProvider::createFromDictionary(defaultProviderDict);
        _defaultTileProvider = std::shared_ptr<TileProvider>(defaultProvider);
        
        ghoul::Dictionary indexProvidersDict = dictionary.value<ghoul::Dictionary>(KeyProviders);
        for (size_t i = 0; i < indexProvidersDict.size(); i++) {
            std::string dictKey = std::to_string(i + 1);
            ghoul::Dictionary indexProviderDict = indexProvidersDict.value<ghoul::Dictionary>(dictKey);
            ghoul::Dictionary tileIndexDict = indexProviderDict.value<ghoul::Dictionary>(KeyTileIndex);
            ghoul::Dictionary providerDict = indexProviderDict.value<ghoul::Dictionary>(KeyTileProvider);
            
            TileIndex tileIndex(tileIndexDict);
            TileProvider* tileProvider = TileProvider::createFromDictionary(providerDict);
            std::shared_ptr<TileProvider> stp = std::shared_ptr<TileProvider>(tileProvider);
            TileHashKey key = tileIndex.hashKey();
            _tileProviderMap.insert(std::make_pair(key, stp));
        }
    }

    Tile TileProviderByIndex::getTile(const TileIndex& tileIndex) {
        auto it = _tileProviderMap.find(tileIndex.hashKey());
        bool hasProvider = it != _tileProviderMap.end();
        return hasProvider ? it->second->getTile(tileIndex) : Tile::TileUnavailable;
    }

    Tile TileProviderByIndex::getDefaultTile() {
        return _defaultTileProvider->getDefaultTile();
    }

    Tile::Status TileProviderByIndex::getTileStatus(const TileIndex& tileIndex) {
        auto it = _tileProviderMap.find(tileIndex.hashKey());
        bool hasProvider = it != _tileProviderMap.end();
        return hasProvider ? it->second->getTileStatus(tileIndex) : Tile::Status::Unavailable;
    }

    TileDepthTransform TileProviderByIndex::depthTransform() {
        _defaultTileProvider->depthTransform();
    }

    void TileProviderByIndex::update() {
        for(auto it : _tileProviderMap){
            it.second->update();
        }
        _defaultTileProvider->update();
    }

    void TileProviderByIndex::reset() {
        for(auto it : _tileProviderMap){
            it.second->reset();
        }
        _defaultTileProvider->reset();
    }

    int TileProviderByIndex::maxLevel() {
        return _defaultTileProvider->maxLevel();
    }

    TileProvider* TileProviderByIndex::indexProvider(const TileIndex& tileIndex) const{
        auto it = _tileProviderMap.find(tileIndex.hashKey());
        return (it != _tileProviderMap.end()) ? it->second.get() : nullptr;
    }

} // namespace globebrowsing
} // namespace openspace
