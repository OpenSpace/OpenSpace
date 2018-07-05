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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE_INDEX___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE_INDEX___H__

#include <modules/globebrowsing/tile/quad.h>

#include <ghoul/glm.h>
#include <stdint.h>

namespace ghoul { class Dictionary; }

namespace openspace::globebrowsing {

struct Geodetic2;

enum CardinalDirection {
    WEST = 0,
    EAST,
    NORTH,
    SOUTH,
};

struct TileIndex {
    using TileHashKey = uint64_t;

    int x, y, level;

    TileIndex(int x_ = 0, int y_ = 0, int level_ = 0);
    TileIndex(const TileIndex& other) = default;

    /**
     * Creates the geodetic tile index for the Geodetic patch that covers the
     * point p at the specified level
     */
    TileIndex(const Geodetic2& point, int level_);

    /**
     * Initializes a TileIndex from a Dictionary
     */
    TileIndex(const ghoul::Dictionary& dict);


    bool hasParent() const;

    TileIndex parent() const;

    bool operator==(const TileIndex& other) const;

    TileIndex& operator--();
    TileIndex operator--(int);

    TileIndex& operator-=(unsigned int levels);

    bool isWestChild() const;
    bool isEastChild() const;
    bool isNorthChild() const;
    bool isSouthChild() const;

    TileIndex child(Quad q) const;

    glm::vec2 positionRelativeParent() const;


    std::string toString() const;

    /**
     * Gets the tile at a specified offset from this tile.
     * Accepts delta indices ranging from [-2^level, Infinity[
     */
    TileIndex relatedTile(int deltaX, int deltaY) const;

    int manhattan(const TileIndex& other) const;

    TileHashKey hashKey() const;
};

std::ostream& operator<<(std::ostream& os, const TileIndex& ti);

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_INDEX___H__
