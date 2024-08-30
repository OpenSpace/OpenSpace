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

#include <modules/globebrowsing/src/tileprovider/forcehighresolutiontileprovider.h>

namespace {
}

namespace openspace::globebrowsing {

ForceHighResolutionTileProvider::ForceHighResolutionTileProvider(const ghoul::Dictionary& dictionary) {}

ForceHighResolutionTileProvider::~ForceHighResolutionTileProvider() {}

Tile ForceHighResolutionTileProvider::tile(const globebrowsing::TileIndex& tileIndex) {
    return Tile{ 0, std::nullopt, Tile::Status::OK };
}

Tile::Status ForceHighResolutionTileProvider::tileStatus(const globebrowsing::TileIndex& tileIndex) {
    return globebrowsing::Tile::Status::OK;
}

TileDepthTransform ForceHighResolutionTileProvider::depthTransform() {
    return { 0.f, 1.f };
}

void ForceHighResolutionTileProvider::update() {
    ZoneScoped;
}

int ForceHighResolutionTileProvider::minLevel() {
    return 1;
}

int ForceHighResolutionTileProvider::maxLevel() {
    return 1337;
}

float ForceHighResolutionTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

void ForceHighResolutionTileProvider::reset() {
}

} // namespace openspace::globebrowsing
