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

#include <modules/globebrowsing/src/tileprovider/tileproviderbylevel.h>

namespace {
    constexpr const char* KeyProviders = "LevelTileProviders";
    constexpr const char* KeyMaxLevel = "MaxLevel";
    constexpr const char* KeyTileProvider = "TileProvider";
    constexpr const char* KeyLayerGroupID = "LayerGroupID";
} // namespace

namespace openspace::globebrowsing {

TileProviderByLevel::TileProviderByLevel(const ghoul::Dictionary& dictionary) {
    ZoneScoped

    layergroupid::GroupID layerGroupID = static_cast<layergroupid::GroupID>(
        dictionary.value<int>(KeyLayerGroupID)
    );

    if (dictionary.hasValue<ghoul::Dictionary>(KeyProviders)) {
        ghoul::Dictionary providers = dictionary.value<ghoul::Dictionary>(KeyProviders);

        for (size_t i = 1; i <= providers.size(); i++) {
            ghoul::Dictionary levelProviderDict = providers.value<ghoul::Dictionary>(
                std::to_string(i)
            );
            double floatMaxLevel = levelProviderDict.value<double>(KeyMaxLevel);
            int maxLevel = static_cast<int>(std::round(floatMaxLevel));

            ghoul::Dictionary providerDict = levelProviderDict.value<ghoul::Dictionary>(
                KeyTileProvider
            );
            providerDict.setValue(KeyLayerGroupID, static_cast<int>(layerGroupID));

            layergroupid::TypeID typeID;
            if (providerDict.hasValue<std::string>("Type")) {
                const std::string& typeString = providerDict.value<std::string>("Type");
                typeID = ghoul::from_string<layergroupid::TypeID>(typeString);

                if (typeID == layergroupid::TypeID::Unknown) {
                    throw ghoul::RuntimeError("Unknown layer type: " + typeString);
                }
            }
            else {
                typeID = layergroupid::TypeID::DefaultTileLayer;
            }

            std::unique_ptr<TileProvider> tp = createFromDictionary(typeID, providerDict);

            std::string provId = providerDict.value<std::string>("Identifier");
            tp->setIdentifier(provId);
            std::string providerName = providerDict.value<std::string>("Name");
            tp->setGuiName(providerName);
            addPropertySubOwner(tp.get());

            levelTileProviders.push_back(std::move(tp));

            // Ensure we can represent the max level
            if (static_cast<int>(providerIndices.size()) < maxLevel) {
                providerIndices.resize(maxLevel + 1, -1);
            }

            // map this level to the tile provider index
            providerIndices[maxLevel] = static_cast<int>(levelTileProviders.size()) - 1;
        }
    }

    // Fill in the gaps (value -1 ) in provider indices, from back to end
    for (int i = static_cast<int>(providerIndices.size()) - 2; i >= 0; --i) {
        if (providerIndices[i] == -1) {
            providerIndices[i] = providerIndices[i + 1];
        }
    }
}

void TileProviderByLevel::internalInitialize() {
    for (const std::unique_ptr<TileProvider>& prov : levelTileProviders) {
        prov->initialize();
    }
}

void TileProviderByLevel::internalDeinitialize() {
    for (const std::unique_ptr<TileProvider>& prov : levelTileProviders) {
        prov->deinitialize();
    }
}

Tile TileProviderByLevel::tile(const TileIndex& tileIndex) {
    ZoneScoped

    TileProvider* provider = levelProvider(tileIndex.level);
    if (provider) {
        return provider->tile(tileIndex);
    }
    else {
        return Tile();
    }
}

Tile::Status TileProviderByLevel::tileStatus(const TileIndex& index) {
    TileProvider* provider = levelProvider(index.level);
    return provider ? provider->tileStatus(index) : Tile::Status::Unavailable;
}

TileProvider* TileProviderByLevel::levelProvider(int level) const {
    ZoneScoped

    if (!levelTileProviders.empty()) {
        int clampedLevel = glm::clamp(
            level,
            0,
            static_cast<int>(providerIndices.size() - 1)
        );
        int idx = providerIndices[clampedLevel];
        return levelTileProviders[idx].get();
    }
    else {
        return nullptr;
    }
}

TileDepthTransform TileProviderByLevel::depthTransform() {
    return { 0.f, 1.f };
}

void TileProviderByLevel::update() {
    for (const std::unique_ptr<TileProvider>& provider : levelTileProviders) {
        provider->update();
    }
}

void TileProviderByLevel::reset() {
    for (const std::unique_ptr<TileProvider>& provider : levelTileProviders) {
        provider->reset();
    }
}

int TileProviderByLevel::maxLevel() {
    return static_cast<int>(providerIndices.size() - 1);
}

float TileProviderByLevel::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
