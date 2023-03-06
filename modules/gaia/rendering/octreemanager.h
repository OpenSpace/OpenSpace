/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_GAIA___OCTREEMANAGER___H__
#define __OPENSPACE_MODULE_GAIA___OCTREEMANAGER___H__

#include <modules/gaia/rendering/gaiaoptions.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <map>
#include <mutex>
#include <queue>
#include <stack>
#include <vector>

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

    OctreeManager() = default;
    ~OctreeManager() = default;

    /**
     * Initializes a one layer Octree with root and 8 children that covers all stars.
     *
     * \param maxDist together with \param maxstarsPerNode (if defined) determines the
     * depth of the tree as well as how many nodes will be created.
     */
    void initOctree(long long cpuRamBudget = 0, int maxDist = 0, int maxStarsPerNode = 0);

    /**
     * Initializes a stack of size \param maxNodes that keeps track of all free spot in
     * buffer stream. Can be used to trigger a rebuild of buffer(s).
     *
     * \param useVBO defines if VBO or SSBO is used as buffer(s)
     * \param datasetFitInMemory defines if streaming of nodes during runtime is used
     */
    void initBufferIndexStack(long long maxNodes, bool useVBO, bool datasetFitInMemory);

    /**
     * Inserts star values in correct position in Octree. Makes use of a recursive
     * traversal strategy. Internally calls <code>insertInNode()</code>
     */
    void insert(const std::vector<float>& starValues);

    /**
     * Slices LOD data so only the MAX_STARS_PER_NODE brightest stars are stored in inner
     * nodes. If \p branchIndex is defined then only that branch will be sliced.
     * Calls <code>sliceNodeLodCache()</code> internally.
     */
    void sliceLodData(size_t branchIndex = 8);

    /**
     * Prints the whole tree structure, including number of stars per node, number of
     * nodes, tree depth and if node is a leaf.
     * Calls <code>printStarsPerNode(node, prefix)</code> internally.
     */
    void printStarsPerNode() const;

    /**
     * Used while streaming nodes from files. Checks if any nodes need to be loaded or
     * unloaded. If entire dataset fits in RAM then the whole dataset will be loaded
     * asynchronously. Otherwise only nodes close to the camera will be fetched.
     * When RAM stars to fill up least-recently used nodes will start to unload.
     * Calls <code>findAndFetchNeighborNode()</code> and
     * <code>removeNodesFromRam()</code> internally.
     */
    void fetchSurroundingNodes(const glm::dvec3& cameraPos, size_t chunkSizeInBytes,
        const glm::ivec2& additionalNodes);

    /**
     * Builds render data structure by traversing the Octree and checking for intersection
     * with view frustum. Every vector in map contains data for one node.
     * The corresponding integer key is the index where chunk should be inserted into
     * streaming buffer. Calls <code>checkNodeIntersection()</code> for every branch.
     * \pdeltaStars keeps track of how many stars that were added/removed this render
     * call.
     */
    std::map<int, std::vector<float>> traverseData(const glm::dmat4& mvp,
        const glm::vec2& screenSize, int& deltaStars, gaia::RenderMode mode,
        float lodPixelThreshold);

    /**
     * Builds full render data structure by traversing all leaves in the Octree.
     */
    std::vector<float> getAllData(gaia::RenderMode mode);

    /**
     * Removes all data from Octree, or only from a specific branch if specified.
     * \param branchIndex defined which branch to clear if defined.
     */
    void clearAllData(int branchIndex = -1);

    /**
     * Write entire Octree structure to a binary file. \param writeData defines if data
     * should be included or if only structure should be written to the file.
     * Calls <code>writeNodeToFile()</code> which recursively writes all nodes.
     */
    void writeToFile(std::ofstream& outFileStream, bool writeData);

    /**
     * Read a constructed Octree from a file. \returns the total number of (distinct)
     * stars read.
     *
     * \param readData defines if full data or only structure should be read.
     *        Calls <code>readNodeFromFile()</code> which recursively reads all nodes.
     */
    int readFromFile(std::ifstream& inFileStream, bool readData,
        const std::string& folderPath = std::string());

    /**
     * Write specified part of Octree to multiple files, including all data.
     * \param branchIndex defines which branch to write.
     * Clears specified branch after writing is done.
     * Calls <code>writeNodeToMultipleFiles()</code> for the specified branch.
     */
    void writeToMultipleFiles(const std::string& outFolderPath, size_t branchIndex);

    /**
     * Getters.
     */
    size_t numLeafNodes() const;
    size_t numInnerNodes() const;
    size_t totalNodes() const;
    size_t totalDepth() const;
    size_t maxDist() const;
    size_t maxStarsPerNode() const;
    size_t biggestChunkIndexInUse() const;
    size_t numFreeSpotsInBuffer() const;
    bool isRebuildOngoing() const;

    /**
     * \returns current CPU RAM budget in bytes.
     */
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
    // DR2_subset [42.9M] - A MAX_DIST of 100 kPc works fine with 20 kSPN.
    // DR2_full [1.7B] - A MAX_DIST of 250 kPc works fine with 150 kSPN.
    size_t MAX_DIST = 2; // [kPc]
    size_t MAX_STARS_PER_NODE = 2000;

    const int DEFAULT_INDEX = -1;
    const std::string BINARY_SUFFIX = ".bin";

    /**
     * \returns the correct index of child node. Maps [1,1,1] to 0 and [-1,-1,-1] to 7.
     */
    size_t getChildIndex(float posX, float posY, float posZ, float origX = 0.f,
        float origY = 0.f, float origZ = 0.f);

    /**
     * Private help function for <code>insert()</code>. Inserts star into node if leaf and
     * numStars < MAX_STARS_PER_NODE. If a leaf goes above the threshold it is subdivided
     * into 8 new nodes.
     * If node is an inner node, then star is stores in LOD cache if it is among the
     * brightest stars in all children.
     */
    bool insertInNode(OctreeNode& node, const std::vector<float>& starValues,
        int depth = 1);

    /**
     * Slices LOD cache data in node to the MAX_STARS_PER_NODE brightest stars. This needs
     * to be called after the last star has been inserted into Octree but before it is
     * saved to file(s). Slices all descendants recursively.
     */
    void sliceNodeLodCache(OctreeNode& node);

    /**
     * Private help function for <code>insertInNode()</code>. Stores star data in node and
     * keeps track of the brightest stars all children.
     */
    void storeStarData(OctreeNode& node, const std::vector<float>& starValues);

    /**
     * Private help function for <code>printStarsPerNode()</code>. \returns an accumulated
     * string containing all descendant nodes.
     */
    std::string printStarsPerNode(const OctreeNode& node,
        const std::string& prefix) const;

    /**
     * Private help function for <code>traverseData()</code>. Recursively checks which
     * nodes intersect with the view frustum (interpreted as an AABB) and decides if data
     * should be optimized away or not. Keeps track of which nodes that are visible and
     * loaded (if streaming). \param deltaStars keeps track of how many stars that were
     * added/removed this render call.
     */
    std::map<int, std::vector<float>> checkNodeIntersection(OctreeNode& node,
        const glm::dmat4& mvp, const glm::vec2& screenSize, int& deltaStars,
        gaia::RenderMode mode);

    /**
     * Checks if specified node existed in cache, and removes it if that's the case.
     * If node is an inner node then all children will be checked recursively as well as
     * long as \param recursive is not set to false. \param deltaStars keeps track of how
     * many stars that were removed.
     */
    std::map<int, std::vector<float>> removeNodeFromCache(OctreeNode& node,
        int& deltaStars, bool recursive = true);

    /**
     * Get data in node and its descendants regardless if they are visible or not.
     */
    std::vector<float> getNodeData(const OctreeNode& node, gaia::RenderMode mode);

    /**
     * Clear data from node and its descendants and shrink vectors to deallocate memory.
     */
    void clearNodeData(OctreeNode& node);

    /**
     * Contruct default children nodes for specified node.
     */
    void createNodeChildren(OctreeNode& node);

    /**
     * Checks if node should be inserted into stream or not. \returns true if it should,
     * (i.e. it doesn't already exists, there is room for it in the buffer and node data
     * is loaded if streaming). \returns false otherwise.
     */
    bool updateBufferIndex(OctreeNode& node);

    /**
     * Node should be inserted into stream. This function \returns the data to be
     * inserted. If VBOs are used then the chunks will be appended by zeros, otherwise
     * only the star data corresponding to RenderOption \param option will be inserted.
     *
     * \param deltaStars keeps track of how many stars that were added.
     */
    std::vector<float> constructInsertData(const OctreeNode& node,
        gaia::RenderMode mode, int& deltaStars);

    /**
     * Write a node to outFileStream. \param writeData defines if data should be included
     * or if only structure should be written.
     */
    void writeNodeToFile(std::ofstream& outFileStream, const OctreeNode& node,
        bool writeData);

    /**
     * Read a node from file and its potential children. \param readData defines if full
     * data or only structure should be read.
     * \returns accumulated sum of all read stars in node and its descendants.
     */
    int readNodeFromFile(std::ifstream& inFileStream, OctreeNode& node, bool readData);

    /**
     * Write node data to a file. \param outFilePrefix specifies the accumulated path
     * and name of the file. If \param threadWrites is set to true then one new thread
     * will be created for each child to write its descendents.
     */
    void writeNodeToMultipleFiles(const std::string& outFilePrefix, OctreeNode& node,
        bool threadWrites);

    /**
     * Finds the neighboring node on the same level (or a higher level if there is no
     * corresponding level) in the specified direction. Also fetches data from found node
     * if it's not already loaded. \param additionalLevelsToFetch determines if any
     * descendants of the found node should be fetched as well (if they exists).
     */
    void findAndFetchNeighborNode(unsigned long long firstParentId, int x, int y, int z,
        int additionalLevelsToFetch);

    /**
     * Fetches data from all children of \param parentNode, as long as it's not already
     * fetched, it exists and it can fit in RAM.
     * \param additionalLevelsToFetch determines how many levels of descendants to fetch.
     * If it is set to 0 no additional level will be fetched.
     * If it is set to a negative value then all descendants will be fetched recursively.
     * Calls <code>fetchNodeDataFromFile()</code> for every child that passes the tests.
     */
    void fetchChildrenNodes(OctreeNode& parentNode, int additionalLevelsToFetch);

    /**
     * Fetches data for specified node from file.
     * OBS! Only call if node file exists (i.e. node has any data, node->numStars > 0)
     * and is not already loaded.
     */
    void fetchNodeDataFromFile(OctreeNode& node);

    /**
    * Loops though all nodes in \param nodesToRemove and clears them from RAM.
    * Also checks if any ancestor should change the <code>hasLoadedDescendant</code> flag
    * by calling <code>propagateUnloadedNodes()</code> with all ancestors.
    */
    void removeNodesFromRam(const std::vector<unsigned long long>& nodesToRemove);

    /**
     * Removes data in specified node from main memory and updates RAM budget and flags
     * accordingly.
     */
    void removeNode(OctreeNode& node);

    /**
     * Loops through \param ancestorNodes backwards and checks if parent node has any
     * loaded descendants left. If not, then flag <code>hasLoadedDescendant</code> will be
     * set to false for that parent node and next parent in line will be checked.
     */
    void propagateUnloadedNodes(std::vector<std::shared_ptr<OctreeNode>> ancestorNodes);

    std::shared_ptr<OctreeNode> _root;
    std::unique_ptr<OctreeCuller> _culler;
    std::stack<int> _freeSpotsInBuffer;
    std::set<int> _removedKeysInPrevCall;
    std::queue<unsigned long long> _leastRecentlyFetchedNodes;
    std::mutex _leastRecentlyFetchedNodesMutex;

    size_t _totalDepth = 0;
    size_t _numLeafNodes = 0;
    size_t _numInnerNodes = 0;
    size_t _biggestChunkIndexInUse = 0;
    size_t _valuesPerStar = 0;
    float _minTotalPixelsLod = 0.f;

    size_t _maxStackSize = 0;
    bool _rebuildBuffer = false;
    bool _useVBO = false;
    bool _streamOctree = false;
    bool _datasetFitInMemory = false;
    long long _cpuRamBudget = 0;
    long long _maxCpuRamBudget = 0;
    unsigned long long _parentNodeOfCamera = 8;
    std::string _streamFolderPath;
    size_t _traversedBranchesInRenderCall = 0;

}; // class OctreeManager

}  // namespace openspace

#endif // __OPENSPACE_MODULE_GAIA___OCTREEMANAGER___H__
