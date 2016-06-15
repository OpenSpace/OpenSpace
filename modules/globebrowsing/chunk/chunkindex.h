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

#ifndef __CHUNK_INDEX_H__
#define __CHUNK_INDEX_H__

#include <glm/glm.hpp>
#include <vector>



namespace openspace {
    

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



using HashKey = unsigned long;


struct ChunkIndex {
    

    int x, y, level;
    

    ChunkIndex() : x(0), y(0), level(0) { }
    ChunkIndex(int x, int y, int level) : x(x), y(y), level(level) { }
    ChunkIndex(const ChunkIndex& other) : x(other.x), y(other.y), level(other.level) { }
    ChunkIndex(const Geodetic2& point, int level);


    bool hasParent() const {
        return level > 0;
    }

    ChunkIndex parent() const;

    bool isWestChild() const {
        return x % 2 == 0;
    }

    bool isEastChild() const {
        return x % 2 == 1;
    }

    bool isNorthChild() const {
        return y % 2 == 0;
    }

    bool isSouthChild() const {
        return y % 2 == 1;
    }

    ChunkIndex child(Quad q) const;


    std::string toString() const;

    /**
    Gets the tile at a specified offset from this tile.
    Accepts delta indices ranging from [-2^level, Infinity[
    */
    ChunkIndex getRelatedTile(int deltaX, int deltaY) const;

    int manhattan(const ChunkIndex& other) const;

    HashKey hashKey() const;

    bool operator==(const ChunkIndex& other) const;
};


std::ostream& operator<<(std::ostream& os, const ChunkIndex& ti);


} // namespace openspace



#endif // __CHUNK_INDEX_H__
