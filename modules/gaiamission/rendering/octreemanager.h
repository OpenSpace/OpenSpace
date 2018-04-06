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
#include <stack>
#include <unordered_map>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {

class OctreeCuller;

class OctreeManager {
public:
    struct OctreeNode {
        std::shared_ptr<OctreeNode> Children[8];
        std::vector<float> data;
        float originX;
        float originY;
        float originZ;
        float halfDimension;
        size_t numStars;
        bool isLeaf;
        int vboIndex;
        size_t lodInUse;
    };

    OctreeManager();
    ~OctreeManager();

    void initOctree();
    void initVBOIndexStack(int maxIndex);
    void insert(std::vector<float> starValues);
    void printStarsPerNode() const;
    std::unordered_map<int, std::vector<float>> traverseData(const glm::mat4 mvp,
        const glm::vec2 screenSize, int& deltaStars);
    std::vector<float> getAllData();

    size_t numLeafNodes() const;
    size_t maxStarsPerNode() const;
    size_t biggestChunkIndexInUse() const;
    size_t totalNodes() const;

private:
    const size_t MAX_DIST = 5; // [kPc] Radius of Gaia DR1 is ~100 kParsec.
    // Stars/node depend on max_dist because it needs to be big enough to hold all stars
    // that falls outside of the biggest nodes, otherwise it causes a stack overflow.
    const size_t MAX_STARS_PER_NODE = 10000; 
    const int DEFAULT_INDEX = -1;
    const float MIN_TOTAL_PIXELS_LOD = 0.0; // Will be multiplied by depth.
    const int FIRST_LOD_DEPTH = 3; // No layer beneath this will not store any LOD cache.

    size_t getChildIndex(float posX, float posY, float posZ,
        float origX = 0.0, float origY = 0.0, float origZ = 0.0);
    bool insertInNode(std::shared_ptr<OctreeNode> node,
        std::vector<float> starValues, int depth = 1);
    void constructLodCache(std::shared_ptr<OctreeNode> node);
    void insertStarInLodCache(std::shared_ptr<OctreeNode> node, std::vector<float> starValues);
    std::string printStarsPerNode(std::shared_ptr<OctreeNode> node,
        std::string prefix) const;
    std::unordered_map<int, std::vector<float>> checkNodeIntersection(std::shared_ptr<
        OctreeNode> node, const glm::mat4 mvp, const glm::vec2 screenSize, int& deltaStars);
    std::unordered_map<int, std::vector<float>> removeNodeFromCache(std::shared_ptr<
        OctreeNode> node, int& deltaStars, bool recursive = true);
    std::vector<float> getNodeData(std::shared_ptr<OctreeNode> node);

    std::unique_ptr<OctreeNode> _root;
    std::unique_ptr<OctreeCuller> _culler;
    std::stack<int> _freeSpotsInVBO;
    std::set<int> _removedKeysInPrevCall;

    size_t _totalNodes;
    size_t _numNodesPerFile;
    size_t _totalDepth;
    size_t _numLeafNodes;
    size_t _numInnerNodes;
    size_t _biggestChunkIndexInUse;
    size_t _valuesPerStar;
    int _maxStackSize;
    bool _rebuildVBO;

}; // class OctreeManager

}  // namespace openspace

#endif // __OPENSPACE_MODULE_GAIAMISSION___OCTREEMANAGER___H__
