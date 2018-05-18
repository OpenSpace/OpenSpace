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
#include <map>
#include <queue>
#include <mutex>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <modules/gaiamission/rendering/renderoption.h>

namespace openspace {

class OctreeCuller;

class OctreeManager {
public:
    struct OctreeNode {
        std::shared_ptr<OctreeNode> Children[8];
        std::vector<float> posData;
        std::vector<float> colData;
        std::vector<float> velData;
        std::vector<std::pair<float, size_t>> magOrder;
        float originX;
        float originY;
        float originZ;
        float halfDimension;
        size_t numStars;
        bool isLeaf;
        bool isLoaded;
        bool hasLoadedDescendant;
        std::mutex loadingLock;
        int bufferIndex;
        unsigned long long octreePositionIndex;
    };

    OctreeManager();
    ~OctreeManager();

    void initOctree(const long long& cpuRamBudget = 0, int maxDist = 0, 
        int maxStarsPerNode = 0);
    void initBufferIndexStack(const long long& maxNodes, bool useVBO, 
        bool datasetFitInMemory);
    void insert(const std::vector<float>& starValues);
    void sliceLodData(size_t branchIndex = 8);
    void printStarsPerNode() const;

    void fetchSurroundingNodes(const glm::dvec3& cameraPos, size_t chunkSizeInBytes);
    std::map<int, std::vector<float>> traverseData(const glm::mat4& mvp, 
        const glm::vec2& screenSize, int& deltaStars, gaiamission::RenderOption option, 
        const float& lodPixelThreshold);

    std::vector<float> getAllData(gaiamission::RenderOption option);
    void clearAllData(int branchIndex = -1);

    void writeToFile(std::ofstream& outFileStream, bool writeData);
    int readFromFile(std::ifstream& inFileStream, bool readData, 
        const std::string& folderPath = std::string());

    void writeToMultipleFiles(const std::string& outFolderPath, size_t branchIndex);

    size_t numLeafNodes() const;
    size_t numInnerNodes() const;
    size_t totalNodes() const;
    size_t totalDepth() const;
    size_t maxDist() const;
    size_t maxStarsPerNode() const;
    size_t biggestChunkIndexInUse() const;
    size_t numFreeSpotsInBuffer() const;
    long long cpuRamBudget() const;

private:
    const size_t POS_SIZE = 3;
    const size_t COL_SIZE = 2;
    const size_t VEL_SIZE = 3;

    // MAX_DIST [kPc] - Determines the depth of Octree together with MAX_STARS_PER_NODE.
    // A smaller distance is better (i.e. a smaller total depth) and a smaller MAX_STARS 
    // is also better (i.e. finer borders and fewer nodes/less data needs to be uploaded 
    // to the GPU), but MAX_STARS still needs to be big enough to be able to swallow all 
    // stars that falls outside of top border nodes, otherwise it causes a stack overflow 
    // when building Octree. However, fewer total nodes (i.e. bigger Stars/Node) reduces 
    // traversing time which is preferable, especially with big datasets 
    // DR1_TGAS [2M] - A MAX_DIST of 5 kPc works fine with down to 1 kSPN.
    // DR1_full [1.2B] - A MAX_DIST of 10 kPc works fine with most SPN.
    // DR2_rv [7.2M] - A MAX_DIST of 15 kPc works fine with down to 10 kSPN.
    // DR2_subset [42.9M] - A MAX_DIST of 100 kPc works fine with down to 20 kSPN.
    // DR2_full [1.7B] - A MAX_DIST of 1000 kPc works fine with down to 100 kSPN.
    size_t MAX_DIST = 100; // [kPc]
    size_t MAX_STARS_PER_NODE = 20000; 

    const int DEFAULT_INDEX = -1;
    const std::string BINARY_SUFFIX = ".bin"; 

    size_t getChildIndex(const float& posX, const float& posY, const float& posZ,
        const float& origX = 0.0, const float& origY = 0.0, const float& origZ = 0.0);
    bool insertInNode(std::shared_ptr<OctreeNode> node,
        const std::vector<float>& starValues, int depth = 1);
    void sliceNodeLodCache(std::shared_ptr<OctreeNode> node);
    void storeStarData(std::shared_ptr<OctreeNode> node, 
        const std::vector<float>& starValues);
    std::string printStarsPerNode(std::shared_ptr<OctreeNode> node, 
        const std::string& prefix) const;
    std::map<int, std::vector<float>> checkNodeIntersection(std::shared_ptr<OctreeNode> node, 
        const glm::mat4& mvp, const glm::vec2& screenSize, int& deltaStars, 
        gaiamission::RenderOption option);
    std::map<int, std::vector<float>> removeNodeFromCache(std::shared_ptr<OctreeNode> node, 
        int& deltaStars, bool recursive = true);
    std::vector<float> getNodeData(std::shared_ptr<OctreeNode> node, 
        gaiamission::RenderOption option);
    void clearNodeData(std::shared_ptr<OctreeNode> node);
    void createNodeChildren(std::shared_ptr<OctreeNode> node);
    bool updateBufferIndex(std::shared_ptr<OctreeNode> node);

    void writeNodeToFile(std::ofstream& outFileStream, 
        std::shared_ptr<OctreeNode> node, bool writeData);
    int readNodeFromFile(std::ifstream& inFileStream, 
        std::shared_ptr<OctreeNode> node, bool readData);

    void writeNodeToMultipleFiles(const std::string& outFilePrefix, 
        std::shared_ptr<OctreeNode> node, bool threadWrites);
    void findAndFetchNeighborNode(const unsigned long long& firstParentId, int x, int y, int z);
    void fetchChildrenNodes(std::shared_ptr<OctreeManager::OctreeNode> parentNode, 
        bool recursive);
    void fetchNodeDataFromFile(std::shared_ptr<OctreeNode> node);

    /**
    * Loops though all nodes in <nodesToremove> and removes them. Also checks if any
    * ancestor should change the <hasLoadedDescendant> flag.
    */
    void removeNodesFromRam(const std::vector<unsigned long long>& nodesToRemove);
    /**
     * Removes data in specified node from main memory and updates budget and flags 
     * accordingly.
     */
    void removeNode(std::shared_ptr<OctreeManager::OctreeNode> node);
    void propagateUnloadedNodes(std::vector<std::shared_ptr<OctreeNode>> ancestorNodes);

    std::vector<float> constructInsertData(std::shared_ptr<OctreeNode> node, 
        gaiamission::RenderOption option, int& deltaStars);

    std::shared_ptr<OctreeNode> _root;
    std::unique_ptr<OctreeCuller> _culler;
    std::stack<int> _freeSpotsInBuffer;
    std::set<int> _removedKeysInPrevCall;

    std::queue<unsigned long long> _leastRecentlyFetchedNodes;

    size_t _totalDepth;
    size_t _numLeafNodes;
    size_t _numInnerNodes;
    size_t _biggestChunkIndexInUse;
    size_t _valuesPerStar;
    float _minTotalPixelsLod;
    
    size_t _maxStackSize;
    bool _rebuildBuffer;
    bool _useVBO;
    bool _streamOctree;
    bool _datasetFitInMemory;
    long long _cpuRamBudget;
    long long _maxCpuRamBudget;
    unsigned long long _parentNodeOfCamera;
    std::string _streamFolderPath;

}; // class OctreeManager

}  // namespace openspace

#endif // __OPENSPACE_MODULE_GAIAMISSION___OCTREEMANAGER___H__
