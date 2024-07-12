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

#include <modules/globebrowsing/src/tileprovider/tileindextileprovider.h>

#include <openspace/documentation/documentation.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo UniqueBackgroundColors = {
        "UniqueBackgroundColor",
        "Unique Background Color",
        "If 'true' each index tile will have a unique background color assigned to it.",
        openspace::properties::Property::Visibility::Developer
    };

    struct [[codegen::Dictionary(TileIndexTileProvider)]] Parameters {
        // [[codegen::verbatim(UniqueBackgroundColors.description)]]
        std::optional<bool> uniqueBackgroundColors;
    };
#include "tileindextileprovider_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation TileIndexTileProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_tileindextileprovider");
}

TileIndexTileProvider::TileIndexTileProvider(const ghoul::Dictionary& dictionary)
    : TextTileProvider(tileTextureInitData(layers::Group::ID::ColorLayers, false))
    , _uniqueBackgroundColors(UniqueBackgroundColors, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _uniqueBackgroundColors = p.uniqueBackgroundColors.value_or(_uniqueBackgroundColors);
    addProperty(_uniqueBackgroundColors);
}

Tile TileIndexTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped;
    const std::string text = std::format(
        "level: {}\nx: {}\ny: {}", tileIndex.level, tileIndex.x, tileIndex.y
    );
    const glm::vec2 position = glm::vec2(
        initData.dimensions.x / 4 -
        (initData.dimensions.x / 32) * log10(1 << tileIndex.level),
        initData.dimensions.y / 2 + fontSize
    );

    if (_uniqueBackgroundColors) {
        const TileIndex::TileHashKey key = tileIndex.hashKey();
        size_t hash = std::hash<TileIndex::TileHashKey>{}(key);

        // This is pretty ugly, but it's just for debugging and it is reproducable... We
        // take the first three bytes of the hash, treat them as an 8-bit unsigned integer
        // which makes them [0, 255] and also pseudorandom while being always the same for
        // each tile. If we divide the resulting number by 255 we get a value [0, 1] that
        // we can use for the color channel

        const uint8_t red = reinterpret_cast<uint8_t*>(&hash)[0];
        const uint8_t green = reinterpret_cast<uint8_t*>(&hash)[1];
        const uint8_t blue = reinterpret_cast<uint8_t*>(&hash)[2];

        const glm::vec4 backgroundColor = glm::vec4(
            static_cast<float>(red) / 255.f,
            static_cast<float>(green) / 255.f,
            static_cast<float>(blue) / 255.f,
            0.75f
        );

        return TextTileProvider::renderTile(
            tileIndex,
            text,
            position,
            glm::vec4(1.f),
            backgroundColor
        );
    }
    else {
        return TextTileProvider::renderTile(tileIndex, text, position, glm::vec4(1.f));
    }
}

Tile::Status TileIndexTileProvider::tileStatus(const TileIndex&) {
    return Tile::Status::OK;
}

TileDepthTransform TileIndexTileProvider::depthTransform() {
    return { 0.f, 1.f };
}

void TileIndexTileProvider::update() {}

int TileIndexTileProvider::minLevel() {
    return 1;
}

int TileIndexTileProvider::maxLevel() {
    return 1337; // unlimited
}

float TileIndexTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
