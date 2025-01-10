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

#ifndef __OPENSPACE_MODULE_MULTIRESVOLUME___TSP___H__
#define __OPENSPACE_MODULE_MULTIRESVOLUME___TSP___H__

#include <ghoul/opengl/ghoul_gl.h>
#include <filesystem>
#include <fstream>
#include <list>
#include <string>
#include <vector>

namespace openspace {

class TSP {
public:
    struct Header {
        unsigned int gridType;
        unsigned int numOrigTimesteps;
        unsigned int numTimesteps;
        unsigned int xBrickDim;
        unsigned int yBrickDim;
        unsigned int zBrickDim;
        unsigned int xNumBricks;
        unsigned int yNumBricks;
        unsigned int zNumBricks;
    };

    enum NodeData {
        BRICK_INDEX = 0,
        CHILD_INDEX,
        SPATIAL_ERR,
        TEMPORAL_ERR,
        NUM_DATA
    };

    TSP(const std::filesystem::path& filename);
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

    float spatialError(unsigned int brickIndex) const;
    float temporalError(unsigned int brickIndex) const;
    unsigned int firstOctreeChild(unsigned int brickIndex) const;

    unsigned int bstLeft(unsigned int brickIndex) const;
    unsigned int bstRight(unsigned int brickIndex) const;

    bool isBstLeaf(unsigned int brickIndex) const;
    bool isOctreeLeaf(unsigned int brickIndex) const;

private:
    /**
     * Returns a list of the octree leaf nodes that a given input brick covers. If the
     * input is already a leaf, the list will only contain that one index.
     */
    std::list<unsigned int> coveredLeafBricks(unsigned int brickIndex) const;

    /**
     * Returns a list of the BST leaf nodes that a given input brick covers (at the same
     * spatial subdivision level).
     */
    std::list<unsigned int> coveredBSTLeafBricks(unsigned int brickIndex) const;

    /**
     * Return a list of eight children brick incices given a brick index.
     */
    std::list<unsigned int> childBricks(unsigned int brickIndex);

    std::filesystem::path _filename;
    std::ifstream _file;
    std::streampos _dataOffset;

    // Holds the actual structure
    std::vector<int> _data;
    GLuint _dataSSBO = 0;

    // Data from file
    Header _header;

    // Additional metadata
    unsigned int _paddedBrickDim = 0;
    unsigned int _numTotalNodes = 0;
    unsigned int _numBSTLevels = 0;
    unsigned int _numBSTNodes = 0;
    unsigned int _numOTLevels = 0;
    unsigned int _numOTNodes = 0;

    const unsigned int _paddingWidth = 1;

    // Error stats
    float _minSpatialError = 0.f;
    float _maxSpatialError = 0.f;
    float _medianSpatialError = 0.f;
    float _minTemporalError = 0.f;
    float _maxTemporalError = 0.f;
    float _medianTemporalError = 0.f;
};

}  // namespace openspace

#endif // __OPENSPACE_MODULE_MULTIRESVOLUME___TSP___H__
