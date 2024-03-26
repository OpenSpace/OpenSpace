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

#include <modules/gaia/rendering/octreemanager.h>

#include <modules/gaia/rendering/octreeculler.h>
#include <openspace/util/distanceconstants.h>
#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <fstream>
#include <thread>

namespace {
    constexpr std::string_view _loggerCat = "OctreeManager";

    /**
     * \return the correct index of child node. Maps [1,1,1] to 0 and [-1,-1,-1] to 7
     */
    size_t childIndex(float posX, float posY, float posZ, float origX = 0.f,
                      float origY = 0.f, float origZ = 0.f)
    {
        size_t index = 0;
        if (posX < origX) {
            index += 1;
        }
        if (posY < origY) {
            index += 2;
        }
        if (posZ < origZ) {
            index += 4;
        }
        return index;
    }
} // namespace

namespace openspace {

void OctreeManager::initOctree(long long cpuRamBudget, int maxDist, int maxStarsPerNode) {
    if (_root) {
        LDEBUG("Clear existing Octree");
        clearAllData();
    }

    LDEBUG("Initializing new Octree");
    _root = std::make_shared<OctreeNode>();
    _root->octreePositionIndex = 8;

    // Initialize the culler. The NDC.z of the comparing corners are always -1 or 1
    globebrowsing::AABB3 box;
    box.min = glm::vec3(-1.f, -1.f, 0.f);
    box.max = glm::vec3(1.f, 1.f, 100.f);
    _culler = std::make_unique<OctreeCuller>(box);
    _removedKeysInPrevCall = std::set<int>();
    _leastRecentlyFetchedNodes = std::queue<unsigned long long>();

    // Reset default values when rebuilding the Octree during runtime
    _numInnerNodes = 0;
    _numLeafNodes = 0;
    _totalDepth = 0;
    _valuesPerStar = POS_SIZE + COL_SIZE + VEL_SIZE;
    _maxCpuRamBudget = cpuRamBudget;
    _cpuRamBudget = cpuRamBudget;
    _parentNodeOfCamera = 8;

    if (maxDist > 0) {
        MAX_DIST = static_cast<size_t>(maxDist);
    }
    if (maxStarsPerNode > 0) {
        MAX_STARS_PER_NODE = static_cast<size_t>(maxStarsPerNode);
    }

    for (size_t i = 0; i < 8; i++) {
        _numLeafNodes++;
        _root->children[i] = std::make_shared<OctreeNode>();
        _root->children[i]->posData = std::vector<float>();
        _root->children[i]->colData = std::vector<float>();
        _root->children[i]->velData = std::vector<float>();
        _root->children[i]->magOrder = std::vector<std::pair<float, size_t>>();
        _root->children[i]->isLeaf = true;
        _root->children[i]->isLoaded = false;
        _root->children[i]->hasLoadedDescendant = false;
        _root->children[i]->bufferIndex = DEFAULT_INDEX;
        _root->children[i]->octreePositionIndex = 80 + i;
        _root->children[i]->numStars = 0;
        _root->children[i]->halfDimension = MAX_DIST / 2.f;
        _root->children[i]->originX = (i % 2 == 0) ?
            _root->children[i]->halfDimension :
            -_root->children[i]->halfDimension;
        _root->children[i]->originY = (i % 4 < 2) ?
            _root->children[i]->halfDimension :
            -_root->children[i]->halfDimension;
        _root->children[i]->originZ = (i < 4) ?
            _root->children[i]->halfDimension :
            -_root->children[i]->halfDimension;
    }
}

void OctreeManager::initBufferIndexStack(long long maxNodes, bool useVBO,
                                         bool datasetFitInMemory)
{
    // Clear stack if we've used it before
    _biggestChunkIndexInUse = 0;
    _freeSpotsInBuffer = std::stack<int>();
    _rebuildBuffer = true;
    _useVBO = useVBO;
    _datasetFitInMemory = datasetFitInMemory;

    // Build stack back-to-front.
    for (long long idx = maxNodes - 1; idx >= 0; --idx) {
        _freeSpotsInBuffer.push(static_cast<int>(idx));
    }
    _maxStackSize = _freeSpotsInBuffer.size();
    LINFO("StackSize: " + std::to_string(maxNodes));
}

void OctreeManager::insert(const std::vector<float>& starValues) {
    const size_t index = childIndex(starValues[0], starValues[1], starValues[2]);

    insertInNode(*_root->children[index], starValues);
}

void OctreeManager::sliceLodData(size_t branchIndex) {
    if (branchIndex != 8) {
        sliceNodeLodCache(*_root->children[branchIndex]);
    }
    else {
        for (int i = 0; i < 7; i++) {
            sliceNodeLodCache(*_root->children[i]);
        }
    }
}

void OctreeManager::printStarsPerNode() const {
    std::string accumulatedString;

    for (int i = 0; i < 8; i++) {
        const std::string prefix = "{" + std::to_string(i);
        accumulatedString += printStarsPerNode(*_root->children[i], prefix);
    }
    LINFO(std::format("Number of stars per node: \n{}", accumulatedString));
    LINFO(std::format("Number of leaf nodes: {}", std::to_string(_numLeafNodes)));
    LINFO(std::format("Number of inner nodes: {}", std::to_string(_numInnerNodes)));
    LINFO(std::format("Depth of tree: {}", std::to_string(_totalDepth)));
}

void OctreeManager::fetchSurroundingNodes(const glm::dvec3& cameraPos,
                                          size_t chunkSizeInBytes,
                                          const glm::ivec2& additionalNodes)
{

    // If entire dataset fits in RAM then load the entire dataset asynchronously now.
    // Nodes will be rendered when they've been made available
    if (_datasetFitInMemory) {
        // Only traverse Octree once
        if (_parentNodeOfCamera == 8) {
            // Fetch first layer of children
            fetchChildrenNodes(*_root, 0);

            for (const std::shared_ptr<OctreeNode>& child : _root->children) {
                // Check so branch doesn't have a single layer
                if (child->isLeaf) {
                    continue;
                }

                // Use multithreading to load files and detach thread from main execution
                // so it can execute independently. Thread will be destroyed when
                // finished
                std::thread([this, n = child]() {
                    fetchChildrenNodes(*n, -1);
                }).detach();
            }
            _parentNodeOfCamera = 0;
        }
        return;
    }

    // Get leaf node in which the camera resides
    const glm::vec3 fCameraPos = cameraPos / (1000.0 * distanceconstants::Parsec);
    size_t idx = childIndex(fCameraPos.x, fCameraPos.y, fCameraPos.z);
    std::shared_ptr<OctreeNode> node = _root->children[idx];

    while (!node->isLeaf) {
        idx = childIndex(
            fCameraPos.x,
            fCameraPos.y,
            fCameraPos.z,
            node->originX,
            node->originY,
            node->originZ
        );
        node = node->children[idx];
    }
    const unsigned long long leafId = node->octreePositionIndex;
    const unsigned long long firstParentId = leafId / 10;

    // Return early if camera resides in the same first parent as before.
    // Otherwise camera has moved and may need to load more nodes
    if (_parentNodeOfCamera == firstParentId) {
        return;
    }
    _parentNodeOfCamera = firstParentId;

    // Each parent level may be root, make sure to propagate it in that case!
    const unsigned long long secondParentId = (firstParentId == 8) ? 8 : leafId / 100;
    const unsigned long long thirdParentId = (secondParentId == 8) ? 8 : leafId / 1000;
    const unsigned long long fourthParentId = (thirdParentId == 8) ? 8 : leafId / 10000;
    const unsigned long long fifthParentId = (fourthParentId == 8) ? 8 : leafId / 100000;

    // Get the number of levels to fetch from user input
    int additionalLevelsToFetch = additionalNodes.y;

    // Get more descendants when closer to root.
    if (_parentNodeOfCamera < 80000) {
        additionalLevelsToFetch++;
    }

    // Get the 3^3 closest parents and load all their (eventual) children
    for (int x = -1; x <= 1; x += 1) {
        for (int y = -2; y <= 2; y += 2) {
            for (int z = -4; z <= 4; z += 4) {
                // Fetch all stars the 216 closest leaf nodes
                findAndFetchNeighborNode(
                    firstParentId,
                    x,
                    y,
                    z,
                    additionalLevelsToFetch
                );
                // Fetch LOD stars from 208 parents one and two layer(s) up
                if (x != 0 || y != 0 || z != 0) {
                    if (additionalNodes.x > 0) {
                        findAndFetchNeighborNode(
                            secondParentId,
                            x,
                            y,
                            z,
                            additionalLevelsToFetch
                        );
                    }
                    if (additionalNodes.x > 1) {
                        findAndFetchNeighborNode(
                            thirdParentId,
                            x,
                            y,
                            z,
                            additionalLevelsToFetch
                        );
                    }
                    if (additionalNodes.x > 2) {
                        findAndFetchNeighborNode(
                            fourthParentId,
                            x,
                            y,
                            z,
                            additionalLevelsToFetch
                        );
                    }
                    if (additionalNodes.x > 3) {
                        findAndFetchNeighborNode(
                            fifthParentId,
                            x,
                            y,
                            z,
                            additionalLevelsToFetch
                        );
                    }
                }
            }
        }
    }

    // Check if we should remove any nodes from RAM
    const long long tenthOfRamBudget = _maxCpuRamBudget / 10;
    if (_cpuRamBudget < tenthOfRamBudget) {
        const long long bytesToTenthOfRam = tenthOfRamBudget - _cpuRamBudget;
        size_t nNodesToRemove = static_cast<size_t>(bytesToTenthOfRam / chunkSizeInBytes);
        std::vector<unsigned long long> nodesToRemove;
        while (nNodesToRemove > 0) {
            // Dequeue nodes that were least recently fetched by findAndFetchNeighborNode
            nodesToRemove.push_back(_leastRecentlyFetchedNodes.front());
            _leastRecentlyFetchedNodes.pop();
            nNodesToRemove--;
        }
        // Use asynchronous removal.
        if (!nodesToRemove.empty()) {
            std::thread(&OctreeManager::removeNodesFromRam, this, nodesToRemove).detach();
        }
    }
}

void OctreeManager::findAndFetchNeighborNode(unsigned long long firstParentId, int x,
                                             int y, int z, int additionalLevelsToFetch)
{
    unsigned long long parentId = firstParentId;
    std::stack<int> indexStack;

    // Fetch first layer children if we're already at root
    if (parentId == 8) {
        fetchChildrenNodes(*_root, 0);
        return;
    }

    //----------------- Change first index -------------------//
    int nodeIndex = parentId % 10;

    int dx = (nodeIndex % 2 == 0) ? 1 : -1;
    int dy = (nodeIndex % 4 < 2) ? 2 : -2;
    int dz = (nodeIndex < 4) ? 4 : -4;

    // Determine if we need to switch any side (find a higher common parent)
    bool needToSwitchX = (x == dx);
    bool needToSwitchY = (y == dy);
    bool needToSwitchZ = (z == dz);

    if (!needToSwitchX) {
        x = -x;
    }
    if (!needToSwitchY) {
        y = -y;
    }
    if (!needToSwitchZ) {
        z = -z;
    }

    // Update node index and store it at the back of our stack
    nodeIndex += x + y + z;
    indexStack.push(nodeIndex);
    parentId /= 10;

    //--------- Change all indices until we find a common parent --------------//
    while (parentId != 8 && (needToSwitchX || needToSwitchY || needToSwitchZ)) {
        nodeIndex = parentId % 10;

        dx = (nodeIndex % 2 == 0) ? 1 : -1;
        dy = (nodeIndex % 4 < 2) ? 2 : -2;
        dz = (nodeIndex < 4) ? 4 : -4;

        if (needToSwitchX) {
            if (x != dx) {
                needToSwitchX = false;
            }
            nodeIndex += dx;
        }
        if (needToSwitchY) {
            if (y != dy) {
                needToSwitchY = false;
            }
            nodeIndex += dy;
        }
        if (needToSwitchZ) {
            if (z != dz) {
                needToSwitchZ = false;
            }
            nodeIndex += dz;
        }

        indexStack.push(nodeIndex);
        parentId /= 10;
    }

    // Take care of edge cases. If we got to the root but still need to switch to a
    // common parent then no neighbor exists in that direction
    if (needToSwitchX || needToSwitchY || needToSwitchZ) {
        return;
    }

    // Continue to root if we didn't reach it
    while (parentId != 8) {
        nodeIndex = parentId % 10;
        indexStack.push(nodeIndex);
        parentId /= 10;
    }

    // Traverse to that parent node (as long as such a child exists!)
    std::shared_ptr<OctreeNode> node = _root;
    while (!indexStack.empty() && !node->children[indexStack.top()]->isLeaf) {
        node = node->children[indexStack.top()];
        node->hasLoadedDescendant = true;
        indexStack.pop();
    }

    // Fetch all children nodes from found parent. Use multithreading to load files
    // asynchronously! Detach thread from main execution so it can execute independently.
    // Thread will then be destroyed when it has finished
    std::thread([this, node, additionalLevelsToFetch]() {
        fetchChildrenNodes(*node, additionalLevelsToFetch);
    }).detach();
}

std::map<int, std::vector<float>> OctreeManager::traverseData(const glm::dmat4& mvp,
                                                              const glm::vec2& screenSize,
                                                              int& deltaStars,
                                                              gaia::RenderMode mode,
                                                              float lodPixelThreshold)
{
    std::map<int, std::vector<float>> renderData;
    bool innerRebuild = false;
    _minTotalPixelsLod = lodPixelThreshold;

    // Reclaim indices from previous render call
    for (auto removedKey = _removedKeysInPrevCall.rbegin();
         removedKey != _removedKeysInPrevCall.rend(); ++removedKey) {

        // Uses a reverse loop to try to decrease the biggest chunk
        if (*removedKey == static_cast<int>(_biggestChunkIndexInUse) - 1) {
            _biggestChunkIndexInUse = *removedKey;
            LDEBUG(std::format(
                "Decreased size to: {} Free Spots in VBO: {}",
                _biggestChunkIndexInUse, _freeSpotsInBuffer.size()
            ));
        }
        _freeSpotsInBuffer.push(*removedKey);
    }
    // Clear cache of removed keys before next render call
    if (!_removedKeysInPrevCall.empty()) {
        _removedKeysInPrevCall.clear();
    }

    // Rebuild VBO from scratch if we're not using most of it but have a high max index
    if ((_biggestChunkIndexInUse > _maxStackSize * 4 / 5) &&
        (_freeSpotsInBuffer.size() > _maxStackSize * 5 / 6))
    {
        LDEBUG(std::format(
            "Rebuilding VBO. Biggest Chunk: {}  4/5: {} FreeSpotsInVBO: {} 5/6: {}",
            _biggestChunkIndexInUse, _maxStackSize * 4 / 5, _freeSpotsInBuffer.size(),
            _maxStackSize * 5 / 6
        ));
        initBufferIndexStack(_maxStackSize, _useVBO, _datasetFitInMemory);
        innerRebuild = true;
    }

    // Check if entire tree is too small to see, and if so remove it
    std::vector<glm::dvec4> corners(8);
    const float fMaxDist = static_cast<float>(MAX_DIST);
    for (int i = 0; i < 8; i++) {
        const float x = (i % 2 == 0) ? fMaxDist : -fMaxDist;
        const float y = (i % 4 < 2) ? fMaxDist : -fMaxDist;
        const float z = (i < 4) ? fMaxDist : -fMaxDist;
        const glm::dvec3 pos = glm::dvec3(x, y, z) * 1000.0 * distanceconstants::Parsec;
        corners[i] = glm::dvec4(pos, 1.0);
    }
    if (!_culler->isVisible(corners, mvp)) {
        return renderData;
    }
    const glm::vec2 nodeSize = _culler->getNodeSizeInPixels(corners, mvp, screenSize);
    const float totalPixels = nodeSize.x * nodeSize.y;
    if (totalPixels < _minTotalPixelsLod * 2) {
        // Remove LOD from first layer of children
        for (const std::shared_ptr<OctreeNode>& child : _root->children) {
            std::map<int, std::vector<float>> tmpData = removeNodeFromCache(
                *child,
                deltaStars
            );
            renderData.insert(tmpData.begin(), tmpData.end());
        }
        return renderData;
    }

    for (size_t i = 0; i < 8; i++) {
        if (i < _traversedBranchesInRenderCall) {
            continue;
        }

        std::map<int, std::vector<float>> tmpData = checkNodeIntersection(
            *_root->children[i],
            mvp,
            screenSize,
            deltaStars,
            mode
        );

        // Avoid freezing when switching render mode for large datasets by only fetching
        // one branch at a time when rebuilding buffer
        if (_rebuildBuffer) {
            _traversedBranchesInRenderCall++;
        }

        // Observe that if there exists identical keys in renderData then those values in
        // tmpData will be ignored! Thus we store the removed keys until next render call
        renderData.insert(tmpData.begin(), tmpData.end());
    }

    if (_rebuildBuffer) {
        if (_useVBO) {
            // We need to overwrite bigger indices that had data before! No need for SSBO
            std::map<int, std::vector<float>> idxToRemove;
            for (const int idx : _removedKeysInPrevCall) {
                idxToRemove[idx] = std::vector<float>();
            }

            // This will only insert indices that doesn't already exist in map
            // (i.e. > biggestIdx)
            renderData.insert(idxToRemove.begin(), idxToRemove.end());
        }
        if (innerRebuild) {
            deltaStars = 0;
        }

        // Clear potential removed keys for both VBO and SSBO
        _removedKeysInPrevCall.clear();

        LDEBUG(std::format(
            "After rebuilding branch {} - Biggest chunk: {} Free spots in buffer: {}",
            _traversedBranchesInRenderCall, _biggestChunkIndexInUse,
            _freeSpotsInBuffer.size()
        ));

        // End rebuild when all branches has been fetched
        if (_traversedBranchesInRenderCall == 8) {
            _rebuildBuffer = false;
            _traversedBranchesInRenderCall = 0;
        }
    }
    return renderData;
}

std::vector<float> OctreeManager::getAllData(gaia::RenderMode mode) {
    std::vector<float> fullData;

    for (const std::shared_ptr<OctreeNode>& child : _root->children) {
        std::vector<float> tmpData = getNodeData(*child, mode);
        fullData.insert(fullData.end(), tmpData.begin(), tmpData.end());
    }
    return fullData;
}

void OctreeManager::clearAllData(int branchIndex) {
    // Don't clear everything if not needed
    if (branchIndex != -1) {
        clearNodeData(*_root->children[branchIndex]);
    }
    else {
        for (const std::shared_ptr<OctreeNode>& child : _root->children) {
            clearNodeData(*child);
        }
    }
}

void OctreeManager::writeToFile(std::ofstream& outFileStream, bool writeData) {
    outFileStream.write(reinterpret_cast<const char*>(&_valuesPerStar), sizeof(int32_t));
    outFileStream.write(
        reinterpret_cast<const char*>(&MAX_STARS_PER_NODE),
        sizeof(int32_t)
    );
    outFileStream.write(reinterpret_cast<const char*>(&MAX_DIST), sizeof(int32_t));

    // Use pre-traversal (Morton code / Z-order)
    for (const std::shared_ptr<OctreeNode>& child : _root->children) {
        writeNodeToFile(outFileStream, *child, writeData);
    }
}

void OctreeManager::writeNodeToFile(std::ofstream& outFileStream, const OctreeNode& node,
                                    bool writeData)
{
    // Write node structure
    bool isLeaf = node.isLeaf;
    int32_t numStars = static_cast<int32_t>(node.numStars);
    outFileStream.write(reinterpret_cast<const char*>(&isLeaf), sizeof(bool));
    outFileStream.write(reinterpret_cast<const char*>(&numStars), sizeof(int32_t));

    // Write node data if specified
    if (writeData) {
        std::vector<float> nodeData = node.posData;
        nodeData.insert(nodeData.end(), node.colData.begin(), node.colData.end());
        nodeData.insert(nodeData.end(), node.velData.begin(), node.velData.end());
        int32_t nDataSize = static_cast<int32_t>(nodeData.size());
        const size_t nBytes = nDataSize * sizeof(float);

        outFileStream.write(reinterpret_cast<const char*>(&nDataSize), sizeof(int32_t));
        if (nDataSize > 0) {
            outFileStream.write(reinterpret_cast<const char*>(nodeData.data()), nBytes);
        }
    }

    // Write children to file (in Morton order) if we're in an inner node
    if (!node.isLeaf) {
        for (const std::shared_ptr<OctreeNode>& child : node.children) {
            writeNodeToFile(outFileStream, *child, writeData);
        }
    }
}

int OctreeManager::readFromFile(std::ifstream& inFileStream, bool readData,
                                const std::string& folderPath)
{
    int nStarsRead = 0;
    const int oldMaxdist = static_cast<int>(MAX_DIST);

    // If we're not reading data then we need to stream from files later on
    _streamOctree = !readData;
    if (_streamOctree) {
        _streamFolderPath = folderPath;
    }

    _valuesPerStar = 0;
    inFileStream.read(reinterpret_cast<char*>(&_valuesPerStar), sizeof(int32_t));
    inFileStream.read(reinterpret_cast<char*>(&MAX_STARS_PER_NODE), sizeof(int32_t));
    inFileStream.read(reinterpret_cast<char*>(&MAX_DIST), sizeof(int32_t));

    LDEBUG(std::format(
        "Max stars per node in read Octree: {} - Radius of root layer: {}",
        MAX_STARS_PER_NODE, MAX_DIST
    ));

    // Octree Manager root halfDistance must be updated before any nodes are created
    if (static_cast<int>(MAX_DIST) != oldMaxdist) {
        for (size_t i = 0; i < 8; i++) {
            _root->children[i]->halfDimension = MAX_DIST / 2.f;
            _root->children[i]->originX = (i % 2 == 0) ?
                _root->children[i]->halfDimension :
                -_root->children[i]->halfDimension;
            _root->children[i]->originY = (i % 4 < 2) ?
                _root->children[i]->halfDimension :
                -_root->children[i]->halfDimension;
            _root->children[i]->originZ = (i < 4) ?
                _root->children[i]->halfDimension :
                -_root->children[i]->halfDimension;
        }
    }

    if (_valuesPerStar != (POS_SIZE + COL_SIZE + VEL_SIZE)) {
        LERROR("Read file doesn't have the same structure of render parameters");
    }

    // Use the same technique to construct octree from file
    for (const std::shared_ptr<OctreeNode>& child : _root->children) {
        nStarsRead += readNodeFromFile(inFileStream, *child, readData);
    }
    return nStarsRead;
}

int OctreeManager::readNodeFromFile(std::ifstream& inFileStream, OctreeNode& node,
                                    bool readData)
{
    // Read node structure.
    bool isLeaf = false;
    int32_t numStars = 0;

    inFileStream.read(reinterpret_cast<char*>(&isLeaf), sizeof(bool));
    inFileStream.read(reinterpret_cast<char*>(&numStars), sizeof(int32_t));

    node.isLeaf = isLeaf;
    node.numStars = numStars;

    // Read node data if specified.
    if (readData) {
        int32_t nDataSize = 0;
        inFileStream.read(reinterpret_cast<char*>(&nDataSize), sizeof(int32_t));

        if (nDataSize > 0) {
            std::vector<float> fetchedData(nDataSize, 0.f);
            const size_t nBytes = nDataSize * sizeof(float);

            inFileStream.read(reinterpret_cast<char*>(fetchedData.data()), nBytes);

            const int starsInNode = static_cast<int>(nDataSize / _valuesPerStar);
            const auto posEnd = fetchedData.begin() + (starsInNode * POS_SIZE);
            const auto colEnd = posEnd + (starsInNode * COL_SIZE);
            const auto velEnd = colEnd + (starsInNode * VEL_SIZE);
            node.posData = std::vector<float>(fetchedData.begin(), posEnd);
            node.colData = std::vector<float>(posEnd, colEnd);
            node.velData = std::vector<float>(colEnd, velEnd);
        }
    }

    // Create children if we're in an inner node and read from the corresponding nodes
    if (!node.isLeaf) {
        numStars = 0;
        createNodeChildren(node);
        for (const std::shared_ptr<OctreeNode>& child : node.children) {
            numStars += readNodeFromFile(inFileStream, *child, readData);
        }
    }

    // Return the number of stars in the entire branch, no need to check children
    return numStars;
}

void OctreeManager::writeToMultipleFiles(const std::string& outFolderPath,
                                         size_t branchIndex)
{
    // Write entire branch to disc, with one file per node
    const std::string outFilePrefix = outFolderPath + std::to_string(branchIndex);
    // More threads doesn't make it much faster, disk speed still the limiter
    writeNodeToMultipleFiles(outFilePrefix, *_root->children[branchIndex], false);

    // Clear all data in branch.
    LINFO(std::format("Clear all data from branch {} in octree", branchIndex));
    clearNodeData(*_root->children[branchIndex]);
}

void OctreeManager::writeNodeToMultipleFiles(const std::string& outFilePrefix,
                                             OctreeNode& node, bool threadWrites)
{
    // Prepare node data, save nothing else.
    std::vector<float> nodeData = node.posData;
    nodeData.insert(nodeData.end(), node.colData.begin(), node.colData.end());
    nodeData.insert(nodeData.end(), node.velData.begin(), node.velData.end());
    const int32_t nDataSize = static_cast<int32_t>(nodeData.size());
    const size_t nBytes = nDataSize * sizeof(nodeData[0]);

    // Only open output stream if we have any values to write.
    if (nDataSize > 0) {
        // Use Morton code to name file (placement in Octree).
        std::string outPath = outFilePrefix + BINARY_SUFFIX;
        std::ofstream outFileStream(outPath, std::ofstream::binary);
        if (outFileStream.good()) {
            // LINFO("Write " + std::to_string(nDataSize) + " values to " + outPath);
            outFileStream.write(
                reinterpret_cast<const char*>(&nDataSize),
                sizeof(int32_t)
            );
            outFileStream.write(reinterpret_cast<const char*>(nodeData.data()), nBytes);

            outFileStream.close();
        }
        else {
            LERROR(std::format("Error opening file '{}' as output data file", outPath));
        }
    }

    // Recursively write children to file (in Morton order) if we're in an inner node
    if (!node.isLeaf) {
        std::vector<std::thread> writeThreads(8);
        for (size_t i = 0; i < 8; i++) {
            const std::string newOutFilePrefix = outFilePrefix + std::to_string(i);
            if (threadWrites) {
                // Divide writing to new threads to speed up the process
                std::thread t(
                    [this, newOutFilePrefix, n = node.children[i]]() {
                        writeNodeToMultipleFiles(newOutFilePrefix, *n, false);
                    }
                );
                writeThreads[i] = std::move(t);
            }
            else {
                writeNodeToMultipleFiles(newOutFilePrefix, *node.children[i], false);
            }
        }
        if (threadWrites) {
            // Make sure all threads are done.
            for (int thread = 0; thread < 8; ++thread) {
                writeThreads[thread].join();
            }
        }
    }
}

void OctreeManager::fetchChildrenNodes(OctreeNode& parentNode,
                                       int additionalLevelsToFetch)
{
    // Lock node to make sure nobody else are trying to load the same children
    const std::lock_guard lock(parentNode.loadingLock);

    for (const std::shared_ptr<OctreeNode>& child : parentNode.children) {
        // Fetch node data if we're streaming and it doesn't exist in RAM yet.
        // (As long as there is any RAM budget left and node actually has any data!)
        if (!child->isLoaded && (child->numStars > 0) &&
            _cpuRamBudget > static_cast<long long>(child->numStars
                                                  * (POS_SIZE + COL_SIZE + VEL_SIZE) * 4))
        {
            fetchNodeDataFromFile(*child);
        }

        // Fetch all Children's Children if recursive is set to true
        if (additionalLevelsToFetch != 0 && !child->isLeaf) {
            fetchChildrenNodes(*child, --additionalLevelsToFetch);
        }
    }
}

void OctreeManager::fetchNodeDataFromFile(OctreeNode& node) {
    // Remove root ID ("8") from index before loading file
    std::string posId = std::to_string(node.octreePositionIndex);
    posId.erase(posId.begin());

    const std::string inFilePath = _streamFolderPath + posId + BINARY_SUFFIX;
    std::ifstream inFileStream(inFilePath, std::ifstream::binary);
    // LINFO("Fetch node data file: " + inFilePath);

    if (inFileStream.good()) {
        // Read node data
        int32_t nDataSize = 0;

        // Octree knows if we have any data in this node = it exists
        // Otherwise don't call this function!
        inFileStream.read(reinterpret_cast<char*>(&nDataSize), sizeof(int32_t));

        std::vector<float> readData(nDataSize, 0.f);
        const int nBytes = nDataSize * sizeof(float);
        if (nDataSize > 0) {
            inFileStream.read(reinterpret_cast<char*>(readData.data()), nBytes);
        }

        const int starsInNode = static_cast<int>(nDataSize / _valuesPerStar);
        const auto posEnd = readData.begin() + (starsInNode * POS_SIZE);
        const auto colEnd = posEnd + (starsInNode * COL_SIZE);
        const auto velEnd = colEnd + (starsInNode * VEL_SIZE);
        node.posData = std::vector<float>(readData.begin(), posEnd);
        node.colData = std::vector<float>(posEnd, colEnd);
        node.velData = std::vector<float>(colEnd, velEnd);

        // Keep track of nodes that are loaded and update CPU RAM budget
        node.isLoaded = true;
        if (!_datasetFitInMemory) {
            const std::lock_guard g(_leastRecentlyFetchedNodesMutex);
            _leastRecentlyFetchedNodes.push(node.octreePositionIndex);
        }
        _cpuRamBudget -= nBytes;
    }
    else {
        LERROR("Error opening node data file: " + inFilePath);
    }
}

void OctreeManager::removeNodesFromRam(
                                     const std::vector<unsigned long long>& nodesToRemove)
{
    // LINFO("Removed " + std::to_string(nodesToRemove.size()) + " nodes from RAM");

    for (unsigned long long nodePosIndex : nodesToRemove) {
        std::stack<int> indexStack;
        while (nodePosIndex != 8) {
            const int nodeIndex = nodePosIndex % 10;
            indexStack.push(nodeIndex);
            nodePosIndex /= 10;
        }

        // Traverse to node and remove it.
        std::shared_ptr<OctreeNode> node = _root;
        std::vector<std::shared_ptr<OctreeNode>> ancestors;
        while (!indexStack.empty()) {
            ancestors.push_back(node);
            node = node->children[indexStack.top()];
            indexStack.pop();
        }
        removeNode(*node);

        propagateUnloadedNodes(ancestors);
    }
}

void OctreeManager::removeNode(OctreeNode& node) {
    // Lock node to make sure nobody else is trying to access it while removing
    const std::lock_guard lock(node.loadingLock);

    const int nBytes = static_cast<int>(
        node.numStars * _valuesPerStar * sizeof(node.posData[0])
    );
    // Keep track of which nodes that are loaded and update CPU RAM budget
    node.isLoaded = false;
    _cpuRamBudget += nBytes;

    // Clear data
    node.posData.clear();
    node.posData.shrink_to_fit();
    node.colData.clear();
    node.colData.shrink_to_fit();
    node.velData.clear();
    node.velData.shrink_to_fit();
}

void OctreeManager::propagateUnloadedNodes(
                                   std::vector<std::shared_ptr<OctreeNode>> ancestorNodes)
{
    std::shared_ptr<OctreeNode> parentNode = ancestorNodes.back();
    while (parentNode->octreePositionIndex != 8) {
        // Check if any children of inner node is still loaded, or has loaded descendants
        if (parentNode->children[0]->isLoaded || parentNode->children[1]->isLoaded ||
            parentNode->children[2]->isLoaded || parentNode->children[3]->isLoaded ||
            parentNode->children[4]->isLoaded || parentNode->children[5]->isLoaded ||
            parentNode->children[6]->isLoaded || parentNode->children[7]->isLoaded ||
            parentNode->children[0]->hasLoadedDescendant ||
            parentNode->children[1]->hasLoadedDescendant ||
            parentNode->children[2]->hasLoadedDescendant ||
            parentNode->children[3]->hasLoadedDescendant ||
            parentNode->children[4]->hasLoadedDescendant ||
            parentNode->children[5]->hasLoadedDescendant ||
            parentNode->children[6]->hasLoadedDescendant ||
            parentNode->children[7]->hasLoadedDescendant)
        {
            return;
        }
        // Else all children has been unloaded and we can update parent flag
        parentNode->hasLoadedDescendant = false;
        // LINFO("Removed ancestor: " + std::to_string(parentNode->octreePositionIndex));

        // Propagate change upwards
        ancestorNodes.pop_back();
        parentNode = ancestorNodes.back();
    }
}

size_t OctreeManager::numLeafNodes() const {
    return _numLeafNodes;
}

size_t OctreeManager::numInnerNodes() const {
    return _numInnerNodes;
}

size_t OctreeManager::totalNodes() const {
    return _numLeafNodes + _numInnerNodes;
}

size_t OctreeManager::totalDepth() const {
    return _totalDepth;
}

size_t OctreeManager::maxDist() const {
    return MAX_DIST;
}

size_t OctreeManager::maxStarsPerNode() const {
    return MAX_STARS_PER_NODE;
}

size_t OctreeManager::biggestChunkIndexInUse() const {
    return _biggestChunkIndexInUse;
}

size_t OctreeManager::numFreeSpotsInBuffer() const {
    return _freeSpotsInBuffer.size();
}

long long OctreeManager::cpuRamBudget() const {
    return _cpuRamBudget;
}

bool OctreeManager::isRebuildOngoing() const {
    return _rebuildBuffer;
}

bool OctreeManager::insertInNode(OctreeNode& node, const std::vector<float>& starValues,
                                 int depth)
{
    if (node.isLeaf && node.numStars < MAX_STARS_PER_NODE) {
        // Node is a leaf and it's not yet full -> insert star
        storeStarData(node, starValues);

        if (depth > static_cast<int>(_totalDepth)) {
            _totalDepth = depth;
        }
        return true;
    }
    else if (node.isLeaf) {
        // Too many stars in leaf node, subdivide into 8 new nodes.
        // Create children and clean up parent
        createNodeChildren(node);

        // Distribute stars from parent node into children
        for (size_t n = 0; n < MAX_STARS_PER_NODE; ++n) {
            // Position data.
            auto posBegin = node.posData.begin() + n * POS_SIZE;
            auto posEnd = posBegin + POS_SIZE;
            std::vector<float> tmpValues(posBegin, posEnd);
            // Color data.
            auto colBegin = node.colData.begin() + n * COL_SIZE;
            auto colEnd = colBegin + COL_SIZE;
            tmpValues.insert(tmpValues.end(), colBegin, colEnd);
            // Velocity data.
            auto velBegin = node.velData.begin() + n * VEL_SIZE;
            auto velEnd = velBegin + VEL_SIZE;
            tmpValues.insert(tmpValues.end(), velBegin, velEnd);

            // Find out which child that will inherit the data and store it
            const size_t index = childIndex(
                tmpValues[0],
                tmpValues[1],
                tmpValues[2],
                node.originX,
                node.originY,
                node.originZ
            );
            insertInNode(*node.children[index], tmpValues, depth);
        }

        // Sort magnitudes in inner node.
        // (The last value will be used as comparison for what to store in LOD cache)
        std::sort(node.magOrder.begin(), node.magOrder.end());
    }

    // Node is an inner node, keep recursion going.
    // This will also take care of the new star when a subdivision has taken place
    const size_t index = childIndex(
        starValues[0],
        starValues[1],
        starValues[2],
        node.originX,
        node.originY,
        node.originZ
    );

    // Determine if new star should be kept in our LOD cache.
    // Keeps track of the brightest nodes in children.
    if (starValues[POS_SIZE] < node.magOrder[MAX_STARS_PER_NODE - 1].first) {
        storeStarData(node, starValues);
    }

    return insertInNode(*node.children[index], starValues, ++depth);
}

void OctreeManager::sliceNodeLodCache(OctreeNode& node) {
    // Slice stored LOD data in inner nodes
    if (!node.isLeaf) {
        // Sort by magnitude. Inverse relation (i.e. a lower magnitude means a brighter
        // star!)
        std::sort(node.magOrder.begin(), node.magOrder.end());
        node.magOrder.resize(MAX_STARS_PER_NODE);

        std::vector<float> tmpPos;
        std::vector<float> tmpCol;
        std::vector<float> tmpVel;
        // Ordered map contain the MAX_STARS_PER_NODE brightest stars in all children
        for (auto const& [absMag, placement] : node.magOrder) {
            auto posBegin = node.posData.begin() + placement * POS_SIZE;
            auto colBegin = node.colData.begin() + placement * COL_SIZE;
            auto velBegin = node.velData.begin() + placement * VEL_SIZE;
            tmpPos.insert(tmpPos.end(), posBegin, posBegin + POS_SIZE);
            tmpCol.insert(tmpCol.end(), colBegin, colBegin + COL_SIZE);
            tmpVel.insert(tmpVel.end(), velBegin, velBegin + VEL_SIZE);
        }
        node.posData = std::move(tmpPos);
        node.colData = std::move(tmpCol);
        node.velData = std::move(tmpVel);
        node.numStars = node.magOrder.size(); // = MAX_STARS_PER_NODE

        for (const std::shared_ptr<OctreeNode>& child : node.children) {
            sliceNodeLodCache(*child);
        }
    }
}

void OctreeManager::storeStarData(OctreeNode& node,
                                  const std::vector<float>& starValues) const
{
    // Insert star data at the back of vectors and store a vector with pairs consisting of
    // star magnitude and insert index for later sorting and slicing of LOD cache
    const float mag = starValues[POS_SIZE];
    node.magOrder.insert(node.magOrder.end(), std::make_pair(mag, node.numStars));
    node.numStars++;

    // If LOD is growing too large then sort it and resize to [chunk size] to avoid too
    // much RAM usage and increase threshold for adding new stars
    if (node.magOrder.size() > MAX_STARS_PER_NODE * 2) {
        std::sort(node.magOrder.begin(), node.magOrder.end());
        node.magOrder.resize(MAX_STARS_PER_NODE);
    }

    auto posEnd = starValues.begin() + POS_SIZE;
    auto colEnd = posEnd + COL_SIZE;
    node.posData.insert(node.posData.end(), starValues.begin(), posEnd);
    node.colData.insert(node.colData.end(), posEnd, colEnd);
    node.velData.insert(node.velData.end(), colEnd, starValues.end());
}

std::string OctreeManager::printStarsPerNode(const OctreeNode& node,
                                             const std::string& prefix) const
{

    // Print both inner and leaf nodes
    auto str = prefix + "} : " + std::to_string(node.numStars);

    if (node.isLeaf) {
        return str + " - [Leaf] \n";
    }
    else {
        str += std::format("LOD: {} - [Parent]\n", node.posData.size() / POS_SIZE);
        for (int i = 0; i < 8; i++) {
            auto pref = prefix + "->" + std::to_string(i);
            str += printStarsPerNode(*node.children[i], pref);
        }
        return str;
    }
}

std::map<int, std::vector<float>> OctreeManager::checkNodeIntersection(OctreeNode& node,
                                                                    const glm::dmat4& mvp,
                                                              const glm::vec2& screenSize,
                                                                          int& deltaStars,
                                                                    gaia::RenderMode mode)
{
    std::map<int, std::vector<float>> fetchedData;

    // Calculate the corners of the node
    std::vector<glm::dvec4> corners(8);
    for (int i = 0; i < 8; i++) {
        const float x = (i % 2 == 0) ?
            node.originX + node.halfDimension :
            node.originX - node.halfDimension;
        const float y = (i % 4 < 2) ?
            node.originY + node.halfDimension :
            node.originY - node.halfDimension;
        const float z = (i < 4) ?
            node.originZ + node.halfDimension :
            node.originZ - node.halfDimension;
        const glm::dvec3 pos = glm::dvec3(x, y, z) * 1000.0 * distanceconstants::Parsec;
        corners[i] = glm::dvec4(pos, 1.0);
    }

    // Check if node is visible from camera. If not then return early
    if (!(_culler->isVisible(corners, mvp))) {
        // Check if this node or any of its children existed in cache previously.
        // If so, then remove them from cache and add those indices to stack
        fetchedData = removeNodeFromCache(node, deltaStars);
        return fetchedData;
    }

    // Remove node if it has been unloaded while still in view.
    // (While streaming big datasets.)
    if (node.bufferIndex != DEFAULT_INDEX && !node.isLoaded && _streamOctree &&
        !_datasetFitInMemory)
    {
        fetchedData = removeNodeFromCache(node, deltaStars);
        return fetchedData;
    }

    // Take care of inner nodes.
    if (!(node.isLeaf)) {
        const glm::vec2 nodeSize = _culler->getNodeSizeInPixels(corners, mvp, screenSize);
        const float totalPixels = nodeSize.x * nodeSize.y;

        // Check if we should return any LOD cache data. If we're streaming a big dataset
        // from files and inner node is visible and loaded, then it should be rendered
        // (as long as it doesn't have loaded children because then we should traverse to
        // lowest loaded level and render it instead)
        if ((totalPixels < _minTotalPixelsLod) || (_streamOctree &&
            !_datasetFitInMemory && node.isLoaded && !node.hasLoadedDescendant))
        {
            // Get correct insert index from stack if node didn't exist already. Otherwise
            // we will overwrite the old data. Key merging is not a problem here
            if ((node.bufferIndex == DEFAULT_INDEX) || _rebuildBuffer) {
                // Return empty if we couldn't claim a buffer stream index
                if (!updateBufferIndex(node)) {
                    return fetchedData;
                }

                // We're in an inner node, remove indices from potential children in cache
                for (const std::shared_ptr<OctreeNode>& child : node.children) {
                    std::map<int, std::vector<float>> tmpData = removeNodeFromCache(
                        *child,
                        deltaStars
                    );
                    fetchedData.insert(tmpData.begin(), tmpData.end());
                }

                // Insert data and adjust stars added in this frame.
                fetchedData[node.bufferIndex] = constructInsertData(
                    node,
                    mode,
                    deltaStars
                );
            }
            return fetchedData;
        }
    }
    // Return node data if node is a leaf
    else {
        // If node already is in cache then skip it, otherwise store it
        if ((node.bufferIndex == DEFAULT_INDEX) || _rebuildBuffer) {
            // Return empty if we couldn't claim a buffer stream index
            if (!updateBufferIndex(node)) {
                return fetchedData;
            }

            // Insert data and adjust stars added in this frame.
            fetchedData[node.bufferIndex] = constructInsertData(
                node,
                mode,
                deltaStars
            );
        }
        return fetchedData;
    }

    // We're in a big, visible inner node -> remove it from cache if it existed.
    // But not its children -> set recursive check to false
    fetchedData = removeNodeFromCache(node, deltaStars, false);

    // Recursively check if children should be rendered.
    for (const std::shared_ptr<OctreeNode>& child : node.children) {
        // Observe that if there exists identical keys in fetchedData then those values in
        // tmpData will be ignored! Thus we store the removed keys until next render call
        std::map<int, std::vector<float>> tmpData = checkNodeIntersection(
            *child,
            mvp,
            screenSize,
            deltaStars,
            mode
        );
        fetchedData.insert(tmpData.begin(), tmpData.end());
    }
    return fetchedData;
}

std::map<int, std::vector<float>> OctreeManager::removeNodeFromCache(OctreeNode& node,
                                                                     int& deltaStars,
                                                                     bool recursive)
{
    std::map<int, std::vector<float>> keysToRemove;

    // If we're in rebuilding mode then there is no need to remove any nodes

    // Check if this node was rendered == had a specified index
    if (node.bufferIndex != DEFAULT_INDEX) {

        // Reclaim that index. We need to wait until next render call to use it again
        _removedKeysInPrevCall.insert(node.bufferIndex);

        // Insert dummy node at offset index that should be removed from render
        keysToRemove[node.bufferIndex] = std::vector<float>();

        // Reset index and adjust stars removed this frame
        node.bufferIndex = DEFAULT_INDEX;
        deltaStars -= static_cast<int>(node.numStars);
    }

    // Check children recursively if we're in an inner node
    if (!(node.isLeaf) && recursive) {
        for (const std::shared_ptr<OctreeNode>& child : node.children) {
            std::map<int, std::vector<float>> tmpData = removeNodeFromCache(
                *child,
                deltaStars
            );
            keysToRemove.insert(tmpData.begin(), tmpData.end());
        }
    }
    return keysToRemove;
}

std::vector<float> OctreeManager::getNodeData(const OctreeNode& node,
                                              gaia::RenderMode mode)
{
    // Return node data if node is a leaf
    if (node.isLeaf) {
        int dStars = 0;
        return constructInsertData(node, mode, dStars);
    }

    // If we're not in a leaf, get data from all children recursively
    std::vector<float> nodeData;
    for (const std::shared_ptr<OctreeNode>& child : node.children) {
        std::vector<float> tmpData = getNodeData(*child, mode);
        nodeData.insert(nodeData.end(), tmpData.begin(), tmpData.end());
    }
    return nodeData;
}

void OctreeManager::clearNodeData(OctreeNode& node) {
    // Clear data and its allocated memory
    node.posData.clear();
    node.colData.clear();
    node.velData.clear();

    // Clear magnitudes as well
    //std::vector<std::pair<float, size_t>>().swap(node->magOrder);
    node.magOrder.clear();

    if (!node.isLeaf) {
        // Remove data from all children recursively
        for (const std::shared_ptr<OctreeNode>& child : node.children) {
            clearNodeData(*child);
        }
    }
}

void OctreeManager::createNodeChildren(OctreeNode& node) {
    for (size_t i = 0; i < 8; i++) {
        _numLeafNodes++;
        node.children[i] = std::make_shared<OctreeNode>();
        node.children[i]->isLeaf = true;
        node.children[i]->isLoaded = false;
        node.children[i]->hasLoadedDescendant = false;
        node.children[i]->bufferIndex = DEFAULT_INDEX;
        node.children[i]->octreePositionIndex = (node.octreePositionIndex * 10) + i;
        node.children[i]->numStars = 0;
        node.children[i]->posData = std::vector<float>();
        node.children[i]->colData = std::vector<float>();
        node.children[i]->velData = std::vector<float>();
        node.children[i]->magOrder = std::vector<std::pair<float, size_t>>();
        node.children[i]->halfDimension = node.halfDimension / 2.f;

        // Calculate new origin
        node.children[i]->originX = node.originX;
        node.children[i]->originX += (i % 2 == 0) ?
            node.children[i]->halfDimension :
            -node.children[i]->halfDimension;
        node.children[i]->originY = node.originY;
        node.children[i]->originY += (i % 4 < 2) ?
            node.children[i]->halfDimension :
            -node.children[i]->halfDimension;
        node.children[i]->originZ = node.originZ;
        node.children[i]->originZ += (i < 4) ?
            node.children[i]->halfDimension :
            -node.children[i]->halfDimension;
    }

    // Clean up parent
    node.isLeaf = false;
    _numLeafNodes--;
    _numInnerNodes++;
}

bool OctreeManager::updateBufferIndex(OctreeNode& node) {
    if (node.bufferIndex != DEFAULT_INDEX) {
        // If we're rebuilding Buffer Index Cache then store indices to overwrite later
        _removedKeysInPrevCall.insert(node.bufferIndex);
    }

    // Make sure node isn't loading/unloading as we're checking isLoaded flag
    const std::lock_guard lock(node.loadingLock);

    // Return false if there are no more spots in our buffer, or if we're streaming and
    // node isn't loaded yet, or if node doesn't have any stars
    if (_freeSpotsInBuffer.empty() || (_streamOctree && !node.isLoaded) ||
        node.numStars == 0)
    {
        return false;
    }

    // Get correct insert index from stack
    node.bufferIndex = _freeSpotsInBuffer.top();
    _freeSpotsInBuffer.pop();

    // Keep track of how many chunks are in use (ceiling)
    if (_freeSpotsInBuffer.empty()) {
        _biggestChunkIndexInUse++;
    }
    else if (_freeSpotsInBuffer.top() > static_cast<int>(_biggestChunkIndexInUse)) {
        _biggestChunkIndexInUse = _freeSpotsInBuffer.top();
    }
    return true;
}

std::vector<float> OctreeManager::constructInsertData(const OctreeNode& node,
                                                      gaia::RenderMode mode,
                                                      int& deltaStars) const
{
    // Return early if node doesn't contain any stars
    if (node.numStars == 0) {
        return std::vector<float>();
    }

    // Fill chunk by appending zeroes to data so we overwrite possible earlier values
    // And more importantly so our attribute pointers knows where to read
    auto insertData = std::vector<float>(node.posData.begin(), node.posData.end());
    if (_useVBO) {
        insertData.resize(POS_SIZE * MAX_STARS_PER_NODE, 0.f);
    }
    if (mode != gaia::RenderMode::Static) {
        insertData.insert(insertData.end(), node.colData.begin(), node.colData.end());
        if (_useVBO) {
            insertData.resize((POS_SIZE + COL_SIZE) * MAX_STARS_PER_NODE, 0.f);
        }
        if (mode == gaia::RenderMode::Motion) {
            insertData.insert(insertData.end(), node.velData.begin(), node.velData.end());
            if (_useVBO) {
                insertData.resize(
                    (POS_SIZE + COL_SIZE + VEL_SIZE) * MAX_STARS_PER_NODE, 0.f
                );
            }
        }
    }

    // Update deltaStars
    deltaStars += static_cast<int>(node.numStars);
    return insertData;
}

}  // namespace openspace
