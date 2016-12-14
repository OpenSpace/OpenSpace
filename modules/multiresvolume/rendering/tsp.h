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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___TSP___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___TSP___H__

#include <string>
#include <vector>
#include <list>
#include <iostream>
#include <fstream>

#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {
class TSP {
public:

    struct Header {
        unsigned int gridType_;
        unsigned int numOrigTimesteps_;
        unsigned int numTimesteps_;
        unsigned int xBrickDim_;
        unsigned int yBrickDim_;
        unsigned int zBrickDim_;
        unsigned int xNumBricks_;
        unsigned int yNumBricks_;
        unsigned int zNumBricks_;

    };

    enum NodeData {
        BRICK_INDEX = 0,
        CHILD_INDEX,
        SPATIAL_ERR,
        TEMPORAL_ERR,
        NUM_DATA
    };

    TSP(const std::string& filename);
    ~TSP();

    // load performs readHeader, readCache, writeCache and construct 
    // in the correct sequence
    bool load();

    bool readHeader();
    bool readCache();
    bool writeCache();
    bool construct();
    bool initalizeSSO();

    const Header& header() const;
    static long long dataPosition();
    std::ifstream& file();
    unsigned int numTotalNodes() const;
    unsigned int numValuesPerNode() const;
    unsigned int numBSTNodes() const;
    unsigned int numBSTLevels() const;
    unsigned int numOTNodes() const;
    unsigned int numOTLevels() const;
    unsigned int brickDim() const;
    unsigned int paddedBrickDim() const;
    unsigned int numBricksPerAxis() const;
    GLuint ssbo() const;

    bool calculateSpatialError();
    bool calculateTemporalError();

    float getSpatialError(unsigned int _brickIndex);
    float getTemporalError(unsigned int _brickIndex);
    unsigned int getFirstOctreeChild(unsigned int _brickIndex);

    unsigned int getBstLeft(unsigned int _brickIndex);
    unsigned int getBstRight(unsigned int _brickIndex);

    bool isBstLeaf(unsigned int _brickIndex);
    bool isOctreeLeaf(unsigned int _brickIndex);

private:
    // Returns a list of the octree leaf nodes that a given input 
    // brick covers. If the input is already a leaf, the list will
    // only contain that one index.
    std::list<unsigned int> CoveredLeafBricks(unsigned int _brickIndex);

    // Returns a list of the BST leaf nodes that a given input brick
    // covers (at the same spatial subdivision level).
    std::list<unsigned int> CoveredBSTLeafBricks(unsigned int _brickIndex);

    // Return a list of eight children brick incices given a brick index
    std::list<unsigned int> ChildBricks(unsigned int _brickIndex);

    std::string _filename;
    std::ifstream _file;
    std::streampos _dataOffset;

    // Holds the actual structure
    std::vector<int> data_;
    GLuint _dataSSBO;

    // Data from file
    Header _header;

    // Additional metadata
    unsigned int paddedBrickDim_;
    unsigned int numTotalNodes_;
    unsigned int numBSTLevels_;
    unsigned int numBSTNodes_;
    unsigned int numOTLevels_;
    unsigned int numOTNodes_;

    const unsigned int paddingWidth_ = 1;

    // Error stats
    float minSpatialError_;
    float maxSpatialError_;
    float medianSpatialError_;
    float minTemporalError_;
    float maxTemporalError_;
    float medianTemporalError_;

}; // class TSP

}  // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___TSP___H__
