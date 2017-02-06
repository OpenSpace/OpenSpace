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

#include <ghoul/misc/dictionary.h>

namespace {
    const std::string _loggerCat = "TileProviderByLevel";

    const char* KeyProviders = "LevelTileProviders";
    const char* KeyMaxLevel = "MaxLevel";
    const char* KeyTileProvider = "TileProvider";
}

namespace openspace {
namespace globebrowsing {

TileProviderByLevel::TileProviderByLevel(const ghoul::Dictionary& dictionary) {
    ghoul::Dictionary levelProvidersDict = dictionary.value<ghoul::Dictionary>(
        KeyProviders
    );

    for (size_t i = 0; i < levelProvidersDict.size(); i++) {
        std::string dictKey = std::to_string(i + 1);
        ghoul::Dictionary levelProviderDict = levelProvidersDict.value<ghoul::Dictionary>(
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
        if (!levelProviderDict.getValue<ghoul::Dictionary>(KeyTileProvider, providerDict))
        {
            throw std::runtime_error(
                "Must define key '" + std::string(KeyTileProvider) + "'"
            );
        }
            
        TileProvider* tileProvider = TileProvider::createFromDictionary(providerDict);
        _levelTileProviders.push_back(std::shared_ptr<TileProvider>(tileProvider));
            
        // Ensure we can represent the max level
        if(_providerIndices.size() < maxLevel){
            _providerIndices.resize(maxLevel+1, -1);
        }
            
        // map this level to the tile provider index
        _providerIndices[maxLevel] = _levelTileProviders.size() - 1;
    }
        
    // Fill in the gaps (value -1) in provider indices, from back to end
    for(int i = _providerIndices.size() - 2; i >= 0; --i){
        if(_providerIndices[i] == -1){
            _providerIndices[i] = _providerIndices[i+1];
        }
    }
}

Tile TileProviderByLevel::getTile(const TileIndex& tileIndex) {
    return levelProvider(tileIndex.level)->getTile(tileIndex);
}

Tile TileProviderByLevel::getDefaultTile() {
    return levelProvider(0)->getDefaultTile();
}

Tile::Status TileProviderByLevel::getTileStatus(const TileIndex& index) {
    return levelProvider(index.level)->getTileStatus(index);
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
    int clampedLevel = std::max(0, std::min(level, (int)_providerIndices.size()-1));
    return _providerIndices[clampedLevel];
}

TileProvider* TileProviderByLevel::levelProvider(int level) const {
    return _levelTileProviders[providerIndex(level)].get();
}

} // namespace globebrowsing
} // namespace openspace
