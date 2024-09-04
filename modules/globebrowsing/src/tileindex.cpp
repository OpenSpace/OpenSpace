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

#include <modules/globebrowsing/src/tileindex.h>

namespace openspace::globebrowsing {

bool operator==(const TileIndex& lhs, const TileIndex& rhs) {
    return (lhs.x == rhs.x) && (lhs.y == rhs.y) && (lhs.level == rhs.level);
}

TileIndex::TileIndex(uint32_t x_, uint32_t y_, uint8_t level_)
    : x(x_)
    , y(y_)
    , level(level_)
{}

TileIndex::TileIndex(TileHashKey hash) {
    // Extract the level as the lowest 5 bits
    uint64_t hlevel = hash & ((1 << 5) - 1);
    ghoul_assert(hlevel < (1 << 5), "Error in hashing for level");
    // And then shift the remainder down so that the next value is in the lowest bits
    hash -= hlevel;
    hash = hash >> 5;

    // Extract the x as the lowest 30 bits
    uint64_t hx = hash & ((1 << 30) - 1);
    ghoul_assert(hx < (1 << 30), "Error in hashing for x");
    // And then shift the remainder down so that the next value is in the lowest bits
    hash -= hx;
    hash = hash >> 30;

    // The remainder in the hash must then be the y index
    uint64_t hy = hash;
    ghoul_assert(hy < (1 << 29), "Error in hashing for y");

    level = static_cast<uint8_t>(hlevel);
    x = static_cast<uint32_t>(hx);
    y = static_cast<uint32_t>(hy);
}

TileIndex TileIndex::child(Quad q) const {
    return TileIndex(2 * x + q % 2, 2 * y + q / 2, level + 1);
}

glm::vec2 TileIndex::positionRelativeParent() const {
    const bool isEastChild = (x % 2 == 1);
    const bool isNorthChild = (y % 2 == 0);

    // In OpenGL, positive y direction is up
    return glm::vec2(isEastChild ? 0.5f : 0.f, isNorthChild ? 0.5f : 0.f);
}

// Creates a hash which can be used as key in hash maps.
//
// +-------+------------+-------+------------+
// | USAGE | BIT RANGE  | #BITS | MAX VALUE  |
// +-------+------------+-------+------------+
// | level |   0 -  5   |   5   |         31 |
// |     x |   5 - 35   |  30   | 1073741824 |
// |     y |  35 - 64   |  29   |  536870912 |
// +-------+------------+-------+------------+
TileIndex::TileHashKey TileIndex::hashKey() const {
    TileHashKey key = 0LL;
    key |= level;
    key |= x << 5;
    key |= static_cast<TileHashKey>(y) << 35;

    return key;
}

} // namespace openspace::globebrowsing
