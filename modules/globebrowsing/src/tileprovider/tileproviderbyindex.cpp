/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <openspace/documentation/documentation.h>

namespace {
    struct [[codegen::Dictionary(TileProviderByIndex)]] Parameters {
        ghoul::Dictionary defaultProvider;

        struct IndexProvider {
            struct Index {
                // The x coordinate for this index. This specifies the horizontal
                // direction (longitude) component
                int x [[codegen::greaterequal(0)]];

                // The y coordinate for this index. This specifies the vertical direction
                // (latitude) component
                int y [[codegen::greaterequal(0)]];

                // The z-level which corresponds to the depth of the tile pyramid, which
                // directly impacts the applied resolution of the tileprovider shown here
                int level [[codegen::inrange(0, 255)]];
            };
            // The index for which the provided tile provider is used
            Index tileIndex;

            // The dictionary that described the tileprovider to be used by the provided
            // index
            ghoul::Dictionary tileProvider;
        };

        // The list of all tileprovides and the indices at which they are used
        std::vector<IndexProvider> indexTileProviders;
    };
#include "tileproviderbyindex_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation TileProviderByIndex::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_tileproviderbyindex");
}

TileProviderByIndex::TileProviderByIndex(const ghoul::Dictionary& dictionary) {
    ZoneScoped

    const Parameters p = codegen::bake<Parameters>(dictionary);

    layers::Layer::ID typeID = layers::Layer::ID::DefaultTileLayer;
    if (p.defaultProvider.hasValue<std::string>("Type")) {
        std::string type = p.defaultProvider.value<std::string>("Type");
        typeID = ghoul::from_string<layers::Layer::ID>(type);

        if (typeID == layers::Layer::ID::Unknown) {
            throw ghoul::RuntimeError("Unknown layer type: " + type);
        }
    }

    _defaultTileProvider = createFromDictionary(typeID, p.defaultProvider);

    for (const Parameters::IndexProvider& ip : p.indexTileProviders) {
        const TileIndex tileIndex(
            ip.tileIndex.x,
            ip.tileIndex.y,
            static_cast<uint8_t>(ip.tileIndex.level)
        );

        layers::Layer::ID providerID = layers::Layer::ID::DefaultTileLayer;
        if (ip.tileProvider.hasValue<std::string>("Type")) {
            std::string type = ip.tileProvider.value<std::string>("Type");
            providerID = ghoul::from_string<layers::Layer::ID>(type);

            if (providerID == layers::Layer::ID::Unknown) {
                throw ghoul::RuntimeError("Unknown layer type: " + type);
            }
        }

        std::unique_ptr<TileProvider> stp = createFromDictionary(
            providerID,
            ip.tileProvider
        );
        TileIndex::TileHashKey key = tileIndex.hashKey();
        _providers.insert(std::make_pair(key, std::move(stp)));
    }
}

Tile TileProviderByIndex::tile(const TileIndex& tileIndex) {
    ZoneScoped
    const auto it = _providers.find(tileIndex.hashKey());
    const bool hasProvider = it != _providers.end();
    return hasProvider ? it->second->tile(tileIndex) : Tile();
}

Tile::Status TileProviderByIndex::tileStatus(const TileIndex& index) {
    const auto it = _providers.find(index.hashKey());
    const bool hasProvider = it != _providers.end();
    return hasProvider ? it->second->tileStatus(index) : Tile::Status::Unavailable;
}

TileDepthTransform TileProviderByIndex::depthTransform() {
    return _defaultTileProvider->depthTransform();
}

void TileProviderByIndex::update() {
    using K = TileIndex::TileHashKey;
    using V = std::unique_ptr<TileProvider>;
    for (std::pair<const K, V>& it : _providers) {
        it.second->update();
    }
    _defaultTileProvider->update();
}

void TileProviderByIndex::reset() {
    using K = TileIndex::TileHashKey;
    using V = std::unique_ptr<TileProvider>;
    for (std::pair<const K, V>& it : _providers) {
        it.second->reset();
    }
    _defaultTileProvider->reset();
}

int TileProviderByIndex::minLevel() {
    return 1;
}

int TileProviderByIndex::maxLevel() {
    return _defaultTileProvider->maxLevel();
}

float TileProviderByIndex::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
