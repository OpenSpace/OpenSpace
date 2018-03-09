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

#include <modules/gaiamission/rendering/octreemanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>

#include <algorithm>
#include <math.h>
#include <queue>

namespace {
    constexpr const char* _loggerCat = "OctreeManager";
} // namespace

namespace openspace {

OctreeManager::OctreeManager()
    : _totalNodes(0)
    , _numNodesPerFile(0)
    , _totalDepth(0)
    , _numLeafNodes(0)
    , _numInnerNodes(0)
{

}

OctreeManager::~OctreeManager() {   }


bool OctreeManager::constructOctree(size_t totalDepth) {
    
    LDEBUG("Constructing Octree");

    _totalDepth = totalDepth;
    _totalNodes = static_cast<size_t>((pow(8, _totalDepth + 1) - 1) / 7);
    _numLeafNodes = static_cast<size_t>(pow(8, _totalDepth));
    _numInnerNodes = _totalNodes - _numLeafNodes;

    _allLeafNodes.clear();
    _allLeafNodes.resize(_numLeafNodes);

    for (size_t i = 0; i < _numLeafNodes; ++i) {
        _allLeafNodes[i] = LeafNode();
        _allLeafNodes[i]._numStars = 0;
        _allLeafNodes[i]._halfDimesion = MAX_DIST / static_cast<size_t>(pow(2, _totalDepth));
        _allLeafNodes[i]._data = std::vector<float>();
    }
   
    return true;
}

size_t OctreeManager::getLeafIndex(float posX, float posY, float posZ, float origX, 
    float origY, float origZ, int depth, size_t index) {

    // Get the corresponding leaf node index by recursion.
    if (depth > _totalDepth) return index;
    //LDEBUG("Current Depth: " + std::to_string(depth));

    // Get the halfDimension and starting index of this layer.
    size_t halfDim = MAX_DIST / static_cast<size_t>(pow(2, depth));
    //size_t index = static_cast<size_t>((pow(8, depth) - 1) / 7);;
    index = getFirstChild(index);

    if (posX < origX) {
        index += 1;
        origX = origX - halfDim;
    }
    else {
        origX = origX + halfDim;
    }
    if (posY < origY) {
        index += 2;
        origY = origY - halfDim;
    }
    else {
        origY = origY + halfDim;
    }
    if (posZ < origZ) {
        index += 4;
        origZ = origZ - halfDim;
    }
    else {
        origZ = origZ + halfDim;
    }

    return getLeafIndex(posX, posY, posZ, origX, origY, origZ, ++depth, index);
}

void OctreeManager::insert(size_t insertIndex, std::vector<float> starValues) {

    size_t leafIndex = insertIndex - _numInnerNodes;
    _allLeafNodes[leafIndex]._numStars++;
    _allLeafNodes[leafIndex]._data.insert(_allLeafNodes[leafIndex]._data.end(),
        starValues.begin(), starValues.end());

}

void OctreeManager::printStarsPerNode() const {

    for (size_t i = 0; i < _numLeafNodes; ++i) {
        LINFO("NumStars in node " + std::to_string(i) + ": " + std::to_string(numStarsPerNode(i)));
    }

}

size_t OctreeManager::numTotalNodes() const { 
    return _totalNodes;
}

size_t OctreeManager::numStarsPerNode(size_t nodeIndex) const {
    return _allLeafNodes[nodeIndex]._numStars;
}

size_t OctreeManager::numNodesPerFile() const { 
    return _numNodesPerFile;
}

size_t OctreeManager::totalDepth() const {
    return _totalDepth;
}

size_t OctreeManager::getFirstChild(size_t idx) const {
    return (8 * idx + 1);
}

size_t OctreeManager::getParent(size_t idx) const {
    if (idx > 0) {
        return static_cast<size_t>(floor((idx - 1) / 8));
    }
    return 0;
}

}
