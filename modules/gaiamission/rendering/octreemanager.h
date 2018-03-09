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

#ifndef __OPENSPACE_MODULE_GAIAMISSION___OCTREEMANAGER___H__
#define __OPENSPACE_MODULE_GAIAMISSION___OCTREEMANAGER___H__

#include <vector>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {

class OctreeManager {
public:
    struct LeafNode {
        std::vector<float> _data;
        int _originX;
        int _originY;
        int _originZ;
        size_t _halfDimesion;
        size_t _numStars;
    };

    OctreeManager();
    ~OctreeManager();

    bool constructOctree(size_t totalDepth);
    size_t getLeafIndex(float posX, float posY, float posZ, float origX = 0.0, 
        float origY = 0.0, float origZ = 0.0, int depth = 1, size_t index = 0);
    void insert(size_t insertIndex, std::vector<float> starValues);
    void printStarsPerNode() const;

    size_t numTotalNodes() const;
    size_t numStarsPerNode(size_t nodeIndex) const;
    size_t numNodesPerFile() const;
    size_t totalDepth() const;

private:
    const size_t MAX_DIST = 100; // Radius of Gaia DR1 in kParsec
    std::vector<LeafNode> _allLeafNodes;

    size_t getFirstChild(size_t idx) const;
    size_t getParent(size_t idx) const;

    size_t _totalNodes;
    size_t _numNodesPerFile;
    size_t _totalDepth;
    size_t _numLeafNodes;
    size_t _numInnerNodes;

}; // class OctreeManager

}  // namespace openspace

#endif // __OPENSPACE_MODULE_GAIAMISSION___OCTREEMANAGER___H__
