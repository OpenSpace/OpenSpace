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

#include <modules/globebrowsing/src/tileprovider/tileindextileprovider.h>

namespace openspace::globebrowsing {

TileIndexTileProvider::TileIndexTileProvider(const ghoul::Dictionary&)
    : TextTileProvider(tileTextureInitData(layers::Group::ID::ColorLayers, false))
{}

Tile TileIndexTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped
    std::string text = fmt::format(
        "level: {}\nx: {}\ny: {}", tileIndex.level, tileIndex.x, tileIndex.y
    );
    glm::vec2 textPosition = glm::vec2(
        initData.dimensions.x / 4 -
        (initData.dimensions.x / 32) * log10(1 << tileIndex.level),
        initData.dimensions.y / 2 + fontSize
    );

    return TextTileProvider::renderTile(tileIndex, text, textPosition, glm::vec4(1.f));
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
