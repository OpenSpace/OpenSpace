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

#include <modules/globebrowsing/chunk/chunkindex.h>
#include <modules/globebrowsing/geometry/geodetic2.h>

#include <sstream>

namespace {
    const std::string _loggerCat = "ChunkIndex";
}

namespace openspace {

    /**
    Creates the geodetic tile index for the Geodetic patch that covers the
    point p at the specified level
    */
    ChunkIndex::ChunkIndex(const Geodetic2& point, int level)
    : level(level) {
        int numIndicesAtLevel = 1 << level;
        double u = 0.5 + point.lon / (2 * M_PI);
        double v = 0.25 - point.lat / (2 * M_PI);
        double xIndexSpace = u * numIndicesAtLevel;
        double yIndexSpace = v * numIndicesAtLevel;

        x = floor(xIndexSpace);
        y = floor(yIndexSpace);
    }

    ChunkIndex ChunkIndex::child(Quad q) const {
        return ChunkIndex(2 * x + q % 2, 2 * y + q / 2, level + 1);
    }

    ChunkIndex ChunkIndex::parent() const {
        //ghoul_assert(level > 0, "tile at level 0 has no parent!");
        return ChunkIndex(x / 2, y / 2, level - 1);
    }

    ChunkIndex& ChunkIndex::operator--() {
        x /= 2;
        y /= 2;
        level--;
        return *this;
    }

    ChunkIndex ChunkIndex::operator--(int) {
        ChunkIndex tmp(*this);
        --(*this);
        return tmp;
    }

    ChunkIndex& ChunkIndex::operator-=(unsigned int levels) {
        x <<= levels;
        y <<= levels;
        level -= levels;
        return *this;
    }


    /**
    Gets the tile at a specified offset from this tile.
    Accepts delta indices ranging from [-2^level, Infinity[
    */
    ChunkIndex ChunkIndex::getRelatedTile(int deltaX, int deltaY) const {
        int indicesAtThisLevel = 1 << level;
        int newX = (indicesAtThisLevel + x + deltaX) % indicesAtThisLevel;
        int newY = (indicesAtThisLevel + y + deltaY) % indicesAtThisLevel;
        return ChunkIndex(newX, newY, level);
    }

    int ChunkIndex::manhattan(const ChunkIndex& other) const {
        ghoul_assert(level == other.level, "makes no sense if not on same level");
        return std::abs(x - other.x) + std::abs(y - other.y);
    }

    HashKey ChunkIndex::hashKey() const {
        return x ^ (y << 16) ^ (level << 24);
    }


    std::string ChunkIndex::toString() const {
        std::stringstream ss;
        for (int i = level; i > 0; i--){
            char digit = '0';
            int mask = 1 << (i - 1);
            if ((x & mask) != 0) {
                digit++;
            }
            if ((y & mask) != 0) {
                digit++;
                digit++;
            }
            ss << digit;
        }
        return ss.str();
    }

    bool ChunkIndex::operator==(const ChunkIndex& other) const {
        return x == other.x && y == other.y && level == other.level;
    }

    std::ostream& operator<<(std::ostream& os, const ChunkIndex& ci) {
        os << "{ x = " << ci.x << ", y = " << ci.y << ", level = " << ci.level << " }";
        return os;
    }


} // namespace openspace
