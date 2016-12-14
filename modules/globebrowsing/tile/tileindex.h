/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <ghoul/glm.h>
#include <stdint.h>

namespace ghoul {
    class Dictionary;
}

namespace openspace {
namespace globebrowsing {

class Geodetic2;

enum Quad {
    NORTH_WEST = 0,
    NORTH_EAST,
    SOUTH_WEST,
    SOUTH_EAST
};

enum CardinalDirection {
    WEST = 0,
    EAST,
    NORTH,
    SOUTH,
};

using TileHashKey = uint64_t;

struct TileIndex {
    int x, y, level;
    
    TileIndex(int x = 0, int y = 0, int level = 0);
    TileIndex(const TileIndex& other);

    /**
     * Creates the geodetic tile index for the Geodetic patch that covers the
     * point p at the specified level
     */
    TileIndex(const Geodetic2& point, int level);

    /**
     * Initializes a TileIndex from a Dictionary
     */
    TileIndex(const ghoul::Dictionary& dict);


    bool hasParent() const {
        return level > 0;
    }

    TileIndex parent() const;
    
    TileIndex& operator--();
    TileIndex operator--(int);

    TileIndex& operator-=(unsigned int levels);

    inline bool isWestChild() const {
        return x % 2 == 0;
    }

    inline bool isEastChild() const {
        return x % 2 == 1;
    }

    inline bool isNorthChild() const {
        return y % 2 == 0;
    }

    inline bool isSouthChild() const {
        return y % 2 == 1;
    }

    TileIndex child(Quad q) const;

    glm::vec2 positionRelativeParent() const;


    std::string toString() const;

    /**
     * Gets the tile at a specified offset from this tile.
     * Accepts delta indices ranging from [-2^level, Infinity[
     */
    TileIndex getRelatedTile(int deltaX, int deltaY) const;

    int manhattan(const TileIndex& other) const;

    TileHashKey hashKey() const;

    bool operator==(const TileIndex& other) const;
};

std::ostream& operator<<(std::ostream& os, const TileIndex& ti);

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_INDEX___H__
