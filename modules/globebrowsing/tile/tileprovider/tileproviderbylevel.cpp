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

#include <modules/globebrowsing/tile/tileprovider/tileproviderbylevel.h>
#include <modules/globebrowsing/rendering/layer/layergroupid.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/logging/logmanager.h>

namespace {
    const char* KeyProviders = "LevelTileProviders";
    const char* KeyMaxLevel = "MaxLevel";
    const char* KeyTileProvider = "TileProvider";
    const char* KeyLayerGroupID = "LayerGroupID";
} // namespace

namespace openspace::globebrowsing::tileprovider {

TileProviderByLevel::TileProviderByLevel(const ghoul::Dictionary& dictionary) {
    std::string name = "Name unspecified";
    dictionary.getValue("Name", name);
  
    layergroupid::GroupID layerGroupID;
    dictionary.getValue(KeyLayerGroupID, layerGroupID);
  
    ghoul::Dictionary providers;
    if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeyProviders)) {
        providers = dictionary.value<ghoul::Dictionary>(KeyProviders);
    }
    
    for (size_t i = 0; i < providers.size(); i++) {
        std::string dictKey = std::to_string(i + 1);
        ghoul::Dictionary levelProviderDict = providers.value<ghoul::Dictionary>(
            dictKey
        );
        double floatMaxLevel;
        int maxLevel = 0;
        if (!levelProviderDict.getValue<double>(KeyMaxLevel, floatMaxLevel)) {
            throw std::runtime_error(
                "Must define key '" + std::string(KeyMaxLevel) + "'"
            );
        }
        maxLevel = std::round(floatMaxLevel);
            
        ghoul::Dictionary providerDict;
        if (!levelProviderDict.getValue<ghoul::Dictionary>(KeyTileProvider, providerDict)) {
            throw std::runtime_error(
                "Must define key '" + std::string(KeyTileProvider) + "'"
            );
        }
        providerDict.setValue(KeyLayerGroupID, layerGroupID);

        std::string typeString;
        providerDict.getValue("Type", typeString);
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

        _levelTileProviders.push_back(
            std::shared_ptr<TileProvider>(TileProvider::createFromDictionary(typeID, providerDict))
        );

        std::string providerName;
        providerDict.getValue("Name", providerName);
        _levelTileProviders.back()->setName(providerName);
        addPropertySubOwner(_levelTileProviders.back().get());
        
        // Ensure we can represent the max level
        if (static_cast<int>(_providerIndices.size()) < maxLevel) {
            _providerIndices.resize(maxLevel+1, -1);
        }
            
        // map this level to the tile provider index
        _providerIndices[maxLevel] = _levelTileProviders.size() - 1;
    }

    // Fill in the gaps (value -1) in provider indices, from back to end
    for (int i = _providerIndices.size() - 2; i >= 0; --i) {
        if(_providerIndices[i] == -1){
            _providerIndices[i] = _providerIndices[i+1];
        }
    }
}

Tile TileProviderByLevel::getTile(const TileIndex& tileIndex) {
    TileProvider* provider = levelProvider(tileIndex.level);
    if (provider) {
        return provider->getTile(tileIndex);
    }
    else {
        return Tile::TileUnavailable;
    }
}

Tile::Status TileProviderByLevel::getTileStatus(const TileIndex& index) {
    TileProvider* provider = levelProvider(index.level);
    if (provider) {
        return provider->getTileStatus(index);
    }
    else {
        return Tile::Status::Unavailable;
    }
}

TileDepthTransform TileProviderByLevel::depthTransform() {
    TileDepthTransform transform;
    transform.depthOffset = 0.0f;
    transform.depthScale = 1.0f;
    return transform;
}

void TileProviderByLevel::update() {
    for(auto provider : _levelTileProviders){
        provider->update();
    }
}

void TileProviderByLevel::reset() {
    for(auto provider : _levelTileProviders){
        provider->reset();
    }
}

int TileProviderByLevel::maxLevel() {
    return _providerIndices.size()-1;
}

int TileProviderByLevel::providerIndex(int level) const {
    int clampedLevel = std::max(
        0,
        std::min(level, static_cast<int>(_providerIndices.size()-1))
    );
    return _providerIndices[clampedLevel];
}

TileProvider* TileProviderByLevel::levelProvider(int level) const {
    if (_levelTileProviders.size() > 0) {
        return _levelTileProviders[providerIndex(level)].get();
    }
    else {
        return nullptr;
    }
}

} // namespace openspace::globebrowsing::tileprovider
