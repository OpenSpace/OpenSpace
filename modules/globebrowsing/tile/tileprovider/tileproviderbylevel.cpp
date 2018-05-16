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

#include <modules/globebrowsing/tile/tileprovider/tileproviderbylevel.h>

#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tileindex.h>

namespace {
    constexpr const char* KeyProviders = "LevelTileProviders";
    constexpr const char* KeyMaxLevel = "MaxLevel";
    constexpr const char* KeyTileProvider = "TileProvider";
    constexpr const char* KeyLayerGroupID = "LayerGroupID";
} // namespace

namespace openspace::globebrowsing::tileprovider {

TileProviderByLevel::TileProviderByLevel(const ghoul::Dictionary& dictionary) {
    layergroupid::GroupID layerGroupID;
    dictionary.getValue(KeyLayerGroupID, layerGroupID);

    ghoul::Dictionary providers;
    if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeyProviders)) {
        providers = dictionary.value<ghoul::Dictionary>(KeyProviders);
    }

    for (size_t i = 0; i < providers.size(); i++) {
        ghoul::Dictionary levelProviderDict = providers.value<ghoul::Dictionary>(
            std::to_string(i + 1)
        );
        double floatMaxLevel = levelProviderDict.value<double>(KeyMaxLevel);
        int maxLevel = static_cast<int>(std::round(floatMaxLevel));

        ghoul::Dictionary providerDict = levelProviderDict.value<ghoul::Dictionary>(
            KeyTileProvider
        );
        providerDict.setValue(KeyLayerGroupID, layerGroupID);

        layergroupid::TypeID typeID;
        if (providerDict.hasKeyAndValue<std::string>("Type")) {
            const std::string& typeString = providerDict.value<std::string>("Type");
            typeID = layergroupid::getTypeIDFromTypeString(typeString);

            if (typeID == layergroupid::TypeID::Unknown) {
                throw ghoul::RuntimeError("Unknown layer type: " + typeString);
            }
        }
        else {
            typeID = layergroupid::TypeID::DefaultTileLayer;
        }

        _levelTileProviders.push_back(
            std::shared_ptr<TileProvider>(
                TileProvider::createFromDictionary(typeID, providerDict)
            )
        );

        std::string providerIdentifier;
        providerDict.getValue("Identifier", providerIdentifier);
        _levelTileProviders.back()->setIdentifier(providerIdentifier);

        std::string providerName;
        providerDict.getValue("Name", providerName);
        _levelTileProviders.back()->setGuiName(providerName);

        addPropertySubOwner(_levelTileProviders.back().get());

        // Ensure we can represent the max level
        if (static_cast<int>(_providerIndices.size()) < maxLevel) {
            _providerIndices.resize(maxLevel+1, -1);
        }

        // map this level to the tile provider index
        _providerIndices[maxLevel] = static_cast<int>(_levelTileProviders.size()) - 1;
    }

    // Fill in the gaps (value -1) in provider indices, from back to end
    for (int i = static_cast<int>(_providerIndices.size()) - 2; i >= 0; --i) {
        if (_providerIndices[i] == -1) {
            _providerIndices[i] = _providerIndices[i+1];
        }
    }
}

bool TileProviderByLevel::initialize() {
    bool success = TileProvider::initialize();
    for (const std::shared_ptr<TileProvider>& tp : _levelTileProviders) {
        success &= tp->initialize();
    }

    return success;
}

bool TileProviderByLevel::deinitialize() {
    bool success = true;
    for (const std::shared_ptr<TileProvider>& tp : _levelTileProviders) {
        success &= tp->deinitialize();
    }

    return TileProvider::deinitialize() && success;
}

Tile TileProviderByLevel::tile(const TileIndex& tileIndex) {
    TileProvider* provider = levelProvider(tileIndex.level);
    if (provider) {
        return provider->tile(tileIndex);
    }
    else {
        return Tile::TileUnavailable;
    }
}

Tile::Status TileProviderByLevel::tileStatus(const TileIndex& index) {
    TileProvider* provider = levelProvider(index.level);
    if (provider) {
        return provider->tileStatus(index);
    }
    else {
        return Tile::Status::Unavailable;
    }
}

TileDepthTransform TileProviderByLevel::depthTransform() {
    TileDepthTransform transform { 0.f, 1.f };
    return transform;
}

void TileProviderByLevel::update() {
    for (const std::shared_ptr<TileProvider>& provider : _levelTileProviders) {
        provider->update();
    }
}

void TileProviderByLevel::reset() {
    for (const std::shared_ptr<TileProvider>& provider : _levelTileProviders){
        provider->reset();
    }
}

int TileProviderByLevel::maxLevel() {
    return static_cast<int>(_providerIndices.size() - 1);
}

int TileProviderByLevel::providerIndex(int level) const {
    int clampedLevel = std::max(
        0,
        std::min(level, static_cast<int>(_providerIndices.size() - 1))
    );
    return _providerIndices[clampedLevel];
}

TileProvider* TileProviderByLevel::levelProvider(int level) const {
    if (!_levelTileProviders.empty()) {
        return _levelTileProviders[providerIndex(level)].get();
    }
    else {
        return nullptr;
    }
}

} // namespace openspace::globebrowsing::tileprovider
