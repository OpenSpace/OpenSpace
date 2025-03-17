/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/documentation/documentation.h>

namespace {
    // This tile provider will switch between different tile providers specified within
    // based on the level of detail that is requested by the Globe. All other things being
    // equal, this corresponds to the distance of the camera to the planet, with a closer
    // distance resulting in a higher lever. Due to technical reasons, the available
    // levels are in the range [2, 22] and each increase in levels corresponds to a
    // doubling in the effective resolution. For a given requested level, the tile
    // provider that has the largest `MaxLevel` that is not greater than the requested
    // level will be used.
    struct [[codegen::Dictionary(TileProviderByLevel)]] Parameters {
        // Each collection describes a distinct layer which can be toggled at a specified
        // max level at which it is requested.
        struct Provider {
            // The maximum level until which the tile provider is used. This number is
            // inclusive, meaning that a value of 4 causes the tile provider to be used
            // at level 4 but not at level 5.
            int maxLevel [[codegen::greaterequal(0)]];

            // The tile provider that should be used at this stage.
            ghoul::Dictionary tileProvider [[codegen::reference("globebrowsing_layer")]];
        };

        // The list of all tile providers that are used by this TileProviderByLevel.
        std::vector<Provider> levelTileProviders;

        // The layer needs to know about the LayerGroupID this but we don't want it to be
        // part of the parameters struct as that would mean it would be visible to the end
        // user, which we don't want since this value just comes from whoever creates it,
        // not the user.
        int layerGroupID [[codegen::private()]];
    };
#include "tileproviderbylevel_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation TileProviderByLevel::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_tileproviderbylevel");
}

TileProviderByLevel::TileProviderByLevel(const ghoul::Dictionary& dictionary) {
    ZoneScoped;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    for (Parameters::Provider provider : p.levelTileProviders) {
        ghoul::Dictionary& tileProviderDict = provider.tileProvider;
        tileProviderDict.setValue("LayerGroupID", p.layerGroupID);

        // Pass down the caching information from the enclosing dictionary
        if (dictionary.hasValue<std::string>("GlobeName")) {
            tileProviderDict.setValue(
                "GlobeName",
                dictionary.value<std::string>("GlobeName")
            );
        }

        std::unique_ptr<TileProvider> tp = createFromDictionary(tileProviderDict);

        const std::string provId = tileProviderDict.value<std::string>("Identifier");
        tp->setIdentifier(provId);
        const std::string providerName = tileProviderDict.value<std::string>("Name");
        tp->setGuiName(providerName);
        addPropertySubOwner(tp.get());

        _levelTileProviders.push_back(std::move(tp));

        // Ensure we can represent the max level
        if (static_cast<int>(_providerIndices.size()) < provider.maxLevel) {
            _providerIndices.resize(provider.maxLevel + 1, -1);
        }

        // map this level to the tile provider index
        _providerIndices[provider.maxLevel] =
            static_cast<int>(_levelTileProviders.size()) - 1;
    }

    // Fill in the gaps (value -1) in provider indices, from back to end
    for (int i = static_cast<int>(_providerIndices.size()) - 2; i >= 0; --i) {
        if (_providerIndices[i] == -1) {
            _providerIndices[i] = _providerIndices[i + 1];
        }
    }
}

void TileProviderByLevel::internalInitialize() {
    for (const std::unique_ptr<TileProvider>& prov : _levelTileProviders) {
        prov->initialize();
    }
}

void TileProviderByLevel::internalDeinitialize() {
    for (const std::unique_ptr<TileProvider>& prov : _levelTileProviders) {
        prov->deinitialize();
    }
}

Tile TileProviderByLevel::tile(const TileIndex& tileIndex) {
    ZoneScoped;

    TileProvider* provider = levelProvider(tileIndex.level);
    return provider ? provider->tile(tileIndex) : Tile();
}

Tile::Status TileProviderByLevel::tileStatus(const TileIndex& index) {
    TileProvider* provider = levelProvider(index.level);
    return provider ? provider->tileStatus(index) : Tile::Status::Unavailable;
}

TileProvider* TileProviderByLevel::levelProvider(int level) const {
    ZoneScoped;

    if (!_levelTileProviders.empty()) {
        const int clampedLevel = std::clamp(
            level,
            0,
            static_cast<int>(_providerIndices.size() - 1)
        );
        const int idx = _providerIndices[clampedLevel];
        return _levelTileProviders[idx].get();
    }
    else {
        return nullptr;
    }
}

TileDepthTransform TileProviderByLevel::depthTransform() {
    return { 0.f, 1.f };
}

void TileProviderByLevel::update() {
    for (const std::unique_ptr<TileProvider>& provider : _levelTileProviders) {
        provider->update();
    }
}

void TileProviderByLevel::reset() {
    for (const std::unique_ptr<TileProvider>& provider : _levelTileProviders) {
        provider->reset();
    }
}

int TileProviderByLevel::minLevel() {
    return 1;
}

int TileProviderByLevel::maxLevel() {
    return static_cast<int>(_providerIndices.size() - 1);
}

float TileProviderByLevel::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
