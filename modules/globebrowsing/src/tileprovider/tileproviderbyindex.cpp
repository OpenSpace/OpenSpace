/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
    // This TileProvider provides the ability to override the contents for tiles at
    // specific indices. A default tile provider has to be specified that is used by
    // default for the entire globe. If a tile provider is specified for a specific tile,
    // then the default tile provide is used for all other indices and the specialized
    // tile provider `P` is used for the specified index. Any number of specialized tile
    // providers can be provided to overwrite specific locations on the globe.
    //
    // This tile provider can be used to, for example, show an inset image that is merged
    // with a larger globe-spanning image.
    struct [[codegen::Dictionary(TileProviderByIndex)]] Parameters {
        ghoul::Dictionary defaultTileProvider
            [[codegen::reference("globebrowsing_layer")]];

        // An IndexProvider is a tile provider that is only valid for a specific
        // combination of x, y, and level. Whenever a globe tries to render a tile and
        // this tile provider has an IndexProvider of that index, it will use the
        // specialized tile provider instead.
        struct IndexProvider {
            struct Index {
                // The x coordinate for this index. This specifies the horizontal
                // direction (longitude) component. Acceptable values for this coordinate
                // have to be smaller than $2 * 2^{level}$.
                int x [[codegen::greaterequal(0)]];

                // The y coordinate for this index. This specifies the vertical direction
                // (latitude) component. Acceptable values for this coordinate have to be
                // smaller than $2^{level}$.
                int y [[codegen::greaterequal(0)]];

                // The z-level which corresponds to the depth of the tile pyramid, which
                // directly impacts the applied resolution of the tileprovider shown here.
                // Not that _in general_ the level would start at 2.
                int level [[codegen::inrange(0, 23)]];
            };
            // The index for which the provided tile provider is used.
            Index index;

            // The dictionary that describes the TileProvider to be used by the provided
            // `index`.
            ghoul::Dictionary tileProvider [[codegen::reference("globebrowsing_layer")]];
        };

        // The list of all TileProviders and the indices at which they are used.
        std::vector<IndexProvider> tileProviders;
    };
#include "tileproviderbyindex_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation TileProviderByIndex::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_tileproviderbyindex");
}

TileProviderByIndex::TileProviderByIndex(const ghoul::Dictionary& dictionary) {
    ZoneScoped;

    Parameters p = codegen::bake<Parameters>(dictionary);

    // For now we need to inject the LayerGroupID this way. We don't want it to be part of
    // the parameters struct as that would mean it would be visible to the end user, which
    // we don't want since this value just comes from whoever creates it, not the user
    ghoul_assert(dictionary.hasValue<int>("LayerGroupID"), "No Layer Group ID provided");
    const layers::Group::ID group = static_cast<layers::Group::ID>(
        dictionary.value<int>("LayerGroupID")
    );

    layers::Layer::ID typeID = layers::Layer::ID::DefaultTileProvider;
    if (p.defaultTileProvider.hasValue<std::string>("Type")) {
        const std::string type = p.defaultTileProvider.value<std::string>("Type");
        typeID = ghoul::from_string<layers::Layer::ID>(type);
    }

    p.defaultTileProvider.setValue("LayerGroupID", static_cast<int>(group));
    _defaultTileProvider = createFromDictionary(typeID, p.defaultTileProvider);

    for (Parameters::IndexProvider& ip : p.tileProviders) {
        const TileIndex tileIndex = TileIndex(
            ip.index.x,
            ip.index.y,
            static_cast<uint8_t>(ip.index.level)
        );

        layers::Layer::ID providerID = layers::Layer::ID::DefaultTileProvider;
        if (ip.tileProvider.hasValue<std::string>("Type")) {
            const std::string type = ip.tileProvider.value<std::string>("Type");
            providerID = ghoul::from_string<layers::Layer::ID>(type);
        }

        ip.tileProvider.setValue("LayerGroupID", static_cast<int>(group));
        std::unique_ptr<TileProvider> stp = createFromDictionary(
            providerID,
            ip.tileProvider
        );
        const TileIndex::TileHashKey key = tileIndex.hashKey();
        _providers.emplace(key, std::move(stp));
    }
}

Tile TileProviderByIndex::tile(const TileIndex& tileIndex) {
    ZoneScoped;
    const auto it = _providers.find(tileIndex.hashKey());
    const bool hasProvider = it != _providers.end();
    return hasProvider ?
        it->second->tile(tileIndex) :
        _defaultTileProvider->tile(tileIndex);
}

Tile::Status TileProviderByIndex::tileStatus(const TileIndex& index) {
    const auto it = _providers.find(index.hashKey());
    const bool hasProvider = it != _providers.end();
    return hasProvider ?
        it->second->tileStatus(index) :
        _defaultTileProvider->tileStatus(index);
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
    int result = _defaultTileProvider->maxLevel();

    using K = TileIndex::TileHashKey;
    using V = std::unique_ptr<TileProvider>;
    for (std::pair<const K, V>& it : _providers) {
        TileIndex index = TileIndex(it.first);
        result = std::max(result, static_cast<int>(index.level));
    }

    return result;
}

float TileProviderByIndex::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
