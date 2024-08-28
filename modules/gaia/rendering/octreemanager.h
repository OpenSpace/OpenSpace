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

#ifndef __OPENSPACE_MODULE_GAIA___OCTREEMANAGER___H__
#define __OPENSPACE_MODULE_GAIA___OCTREEMANAGER___H__

#include <modules/gaia/rendering/gaiaoptions.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <array>
#include <filesystem>
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
        std::array<std::shared_ptr<OctreeNode>, 8> children;
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
     */
    void initOctree(long long cpuRamBudget = 0, int maxDist = 0, int maxStarsPerNode = 0);

    /**
     * Initializes a stack. Can be used to trigger a rebuild of buffer(s).
     *
     * \param maxNodes The maximum number of nodes in the buffer
     * \param useVBO Defines if VBO or SSBO is used as buffer(s)
     * \param datasetFitInMemory Defines if streaming of nodes during runtime is used
     */
    void initBufferIndexStack(long long maxNodes, bool useVBO, bool datasetFitInMemory);

    /**
     * Inserts star values in correct position in Octree. Makes use of a recursive
     * traversal strategy. Internally calls #insertInNode.
     */
    void insert(const std::vector<float>& starValues);

    /**
     * Slices LOD data so only the MAX_STARS_PER_NODE brightest stars are stored in inner
     * nodes. If \p branchIndex is defined then only that branch will be sliced. Calls
     * #sliceNodeLodCache internally.
     */
    void sliceLodData(size_t branchIndex = 8);

    /**
     * Prints the whole tree structure, including number of stars per node, number of
     * nodes, tree depth and if node is a leaf. Calls
     * #printStarsPerNode(const OctreeNode&, const std::string&) const internally.
     */
    void printStarsPerNode() const;

    /**
     * Used while streaming nodes from files. Checks if any nodes need to be loaded or
     * unloaded. If entire dataset fits in RAM then the whole dataset will be loaded
     * asynchronously. Otherwise only nodes close to the camera will be fetched. When RAM
     * stars to fill up least-recently used nodes will start to unload. Calls
     * #findAndFetchNeighborNode and #removeNodesFromRam internally.
     */
    void fetchSurroundingNodes(const glm::dvec3& cameraPos, size_t chunkSizeInBytes,
        const glm::ivec2& additionalNodes);

    /**
     * Builds render data structure by traversing the Octree and checking for intersection
     * with view frustum. Every vector in map contains data for one node. The
     * corresponding integer key is the index where chunk should be inserted into
     * streaming buffer. Calls #checkNodeIntersection for every branch. \p deltaStars
     * keeps track of how many stars that were added/removed this render call.
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
     *
     * \param branchIndex Defined which branch to clear if defined
     */
    void clearAllData(int branchIndex = -1);

    /**
     * Write entire Octree structure to a binary file.
     *
     * \param outFileStream the stream to which the file will be written
     * \param writeData defines if data should be included or if only structure should be
     *        written to the file. Calls #writeNodeToFile which recursively writes all
     *        nodes
     */
    void writeToFile(std::ofstream& outFileStream, bool writeData);

    /**
     * Read a constructed Octree from a file.
     *
     * \param inFileStream the stream from which the octree should be loaded
     * \param readData defines if full data or only structure should be read.
     *        Calls `readNodeFromFile()` which recursively reads all nodes
     * \param folderPath the path to the folder where the binary files are located
     * \return the total number of (distinct) stars read
     */
    int readFromFile(std::ifstream& inFileStream, bool readData,
        const std::filesystem::path& folderPath = std::filesystem::path());

    /**
     * Write specified part of Octree to multiple files, including all data.
     *
     * \param outFolderPath The path where files should be written
     * \param branchIndex Defines which branch to write. Clears specified branch after
     *        writing is done. Calls `writeNodeToMultipleFiles()` for the specified branch
     */
    void writeToMultipleFiles(const std::filesystem::path& outFolderPath,
        size_t branchIndex);

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
     * \return current CPU RAM budget in bytes.
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
     * Private help function for `insert()`. Inserts star into node if leaf and
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
     * Private help function for `insertInNode()`. Stores star data in node and
     * keeps track of the brightest stars all children.
     */
    void storeStarData(OctreeNode& node, const std::vector<float>& starValues) const;

    /**
     * Private help function for `printStarsPerNode()`.
     *
     * \param node the node for which the stars should be printed
     * \param prefix the prefix that should be added to the string
     * \return an accumulated string containing all descendant nodes.
     */
    std::string printStarsPerNode(const OctreeNode& node,
        const std::string& prefix) const;

    /**
     * Private help function for `traverseData()`. Recursively checks which
     * nodes intersect with the view frustum (interpreted as an AABB) and decides if data
     * should be optimized away or not. Keeps track of which nodes that are visible and
     * loaded (if streaming).
     *
     * \param node the node that should be checked
     * \param mvp the model-view-projection matrix used to check intersection
     * \param screenSize the size of the screen in pixels
     * \param deltaStars keeps track of how many stars that were added/removed this render
     *        call
     * \param mode the render mode that should be used
     */
    std::map<int, std::vector<float>> checkNodeIntersection(OctreeNode& node,
        const glm::dmat4& mvp, const glm::vec2& screenSize, int& deltaStars,
        gaia::RenderMode mode);

    /**
     * Checks if specified node existed in cache, and removes it if that's the case.
     * If node is an inner node then all children will be checked recursively as well as
     * long as \p recursive is not set to false.
     *
     * \param node the node that should be removed
     * \param deltaStars keeps track of how many stars that were removed.
     * \param recursive defines if decentents should be removed as well
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
     * Checks if node should be inserted into stream or not.
     *
     * \return `true` if it should, (i.e. it doesn't already exists, there is room for it
     *         in the buffer and node data is loaded if streaming), `false` otherwise
     */
    bool updateBufferIndex(OctreeNode& node);

    /**
     * Node should be inserted into stream.
     *
     * \param node the node that should be inserted
     * \param mode the render mode that should be used
     * \param deltaStars keeps track of how many stars that were added.
     * \return the data to be inserted
     */
    std::vector<float> constructInsertData(const OctreeNode& node,
        gaia::RenderMode mode, int& deltaStars) const;

    /**
     * Write a node to outFileStream.
     *
     * \param outFileStream the stream to which the node will be written
     * \param node the OctreeNode that should be written to file
     * \param writeData defines if data should be included or if only structure should be
     *        written
     */
    void writeNodeToFile(std::ofstream& outFileStream, const OctreeNode& node,
        bool writeData);

    /**
     * Read a node from file and its potential children.
     *
     * \param inFileStream the stream from which the node will be read
     * \param node the file will be read into this node
     * \param readData defines if full data or only structure should be read
     * \return accumulated sum of all read stars in node and its descendants.
     */
    int readNodeFromFile(std::ifstream& inFileStream, OctreeNode& node, bool readData);

    /**
     * Write node data to a file.
     *
     * \param outFilePrefix specifies the accumulated path and name of the file
     * \param node the OctreeNode that should be written to file
     * \param threadWrites is set to true then one new thread will be created for each
     *        child to write its descendents
     */
    void writeNodeToMultipleFiles(const std::string& outFilePrefix, OctreeNode& node,
        bool threadWrites);

    /**
     * Finds the neighboring node on the same level (or a higher level if there is no
     * corresponding level) in the specified direction. Also fetches data from found node
     * if it's not already loaded.
     *
     * \param firstParentId the id of the first parent node that should be checked
     * \param x the x coordinate of the node that should be found
     * \param y the y coordinate of the node that should be found
     * \param z the z coordinate of the node that should be found
     * \param additionalLevelsToFetch determines if any descendants of the found node
     *        should be fetched as well (if they exists).
     */
    void findAndFetchNeighborNode(unsigned long long firstParentId, int x, int y, int z,
        int additionalLevelsToFetch);

    /**
     * Fetches data from all children of the \p parentNode, as long as it's not already
     * fetched, it exists and it can fit in RAM.
     *
     * \param parentNode the node whose children should be fetched
     * \param additionalLevelsToFetch determines how many levels of descendants to fetch.
     *        If it is set to 0 no additional level will be fetched. If it is set to a
     *        negative value then all descendants will be fetched recursively. Calls
     *        #fetchNodeDataFromFile for every child that passes the tests
     */
    void fetchChildrenNodes(OctreeNode& parentNode, int additionalLevelsToFetch);

    /**
     * Fetches data for specified node from file.
     * OBS! Only call if node file exists (i.e. node has any data, node->numStars > 0)
     * and is not already loaded.
     */
    void fetchNodeDataFromFile(OctreeNode& node);

    /**
    * Loops though all nodes in \p nodesToRemove and clears them from RAM. Also checks if
    * any ancestor should change the `hasLoadedDescendant` flag by calling
    * #propagateUnloadedNodes() with all ancestors.
    *
    * \param nodesToRemove list of the nodes that should be deleted
    */
    void removeNodesFromRam(const std::vector<unsigned long long>& nodesToRemove);

    /**
     * Removes data in specified node from main memory and updates RAM budget and flags
     * accordingly.
     */
    void removeNode(OctreeNode& node);

    /**
     * Loops through \p ancestorNodes backwards and checks if parent node has any
     * loaded descendants left. If not, then flag `hasLoadedDescendant` will be
     * set to false for that parent node and next parent in line will be checked.
     *
     * \param ancestorNodes the list of ancestors that should be checked
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
    std::filesystem::path _streamFolderPath;
    size_t _traversedBranchesInRenderCall = 0;

}; // class OctreeManager

}  // namespace openspace

#endif // __OPENSPACE_MODULE_GAIA___OCTREEMANAGER___H__
