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

#include <modules/gaiamission/rendering/octreeculler.h>
#include <modules/gaiamission/rendering/renderoption.h>
#include <openspace/util/distanceconstants.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>
#include <ghoul/fmt.h>
#include <fstream>
#include <thread>

namespace {
    constexpr const char* _loggerCat = "OctreeManager";
} // namespace

namespace openspace {

OctreeManager::OctreeManager()
    : _totalDepth(0)
    , _numLeafNodes(0)
    , _numInnerNodes(0)
    , _biggestChunkIndexInUse(0)
    , _valuesPerStar(0)
    , _maxStackSize(0)
    , _rebuildBuffer(false)
    , _useVBO(false)
    , _streamOctree(false)
    , _cpuRamBudget(0)
    , _ssboStarStreamBudget(0)
    , _parentNodeOfCamera(8)
    , _streamFolderPath("")
{   }

OctreeManager::~OctreeManager() {   }

// Initialize a one layer Octree with root and 8 children. Covers all stars.
void OctreeManager::initOctree(long long cpuRamBudget) {

    LDEBUG("Initializing Octree");
    _root = std::make_unique<OctreeNode>();

    // Initialize the culler. The NDC.z of the comparing corners are always -1 or 1.
    _culler = std::make_unique<OctreeCuller>(
        globebrowsing::AABB3(glm::vec3(-1, -1, 0), glm::vec3(1, 1, 1e2)) 
    );
    _removedKeysInPrevCall = std::set<int>();

    // Reset default values when rebuilding the Octree during runtime.
    _numInnerNodes = 0;
    _numLeafNodes = 0;
    _totalDepth = 0;
    _valuesPerStar = POS_SIZE + COL_SIZE + VEL_SIZE;
    _cpuRamBudget = cpuRamBudget;
    _parentNodeOfCamera = 8;

    for (size_t i = 0; i < 8; ++i) {
        _numLeafNodes++;
        _root->Children[i] = std::make_unique<OctreeNode>();
        _root->Children[i]->posData = std::vector<float>();
        _root->Children[i]->colData = std::vector<float>();
        _root->Children[i]->velData = std::vector<float>();
        _root->Children[i]->magOrder = std::multimap<float, int>();
        _root->Children[i]->isLeaf = true;
        _root->Children[i]->isLoaded = false;
        _root->Children[i]->bufferIndex = DEFAULT_INDEX;
        _root->Children[i]->octreePositionIndex = 80 + i;
        _root->Children[i]->numStars = 0;
        _root->Children[i]->halfDimension = MAX_DIST / 2.f;
        _root->Children[i]->originX = (i % 2 == 0) ? _root->Children[i]->halfDimension
            : -_root->Children[i]->halfDimension;
        _root->Children[i]->originY = (i % 4 < 2) ? _root->Children[i]->halfDimension
            : -_root->Children[i]->halfDimension;
        _root->Children[i]->originZ = (i < 4) ? _root->Children[i]->halfDimension
            : -_root->Children[i]->halfDimension;
    }

    return;
}

// Initialize a stack that keeps track of all free spot in VBO stream.
void OctreeManager::initBufferIndexStack(long long maxStarsOrNodes, bool useVBO, 
    bool datasetFitInMemory) {
    
    // Clear stack if we've used it before.
    _biggestChunkIndexInUse = 0;
    _freeSpotsInBuffer = std::stack<int>();
    _rebuildBuffer = true;
    _useVBO = useVBO;
    _datasetFitInMemory = datasetFitInMemory;

    // Store the number of stars we can stream if we're using SSBOs.
    /*if(!useVBO) {
        _ssboStarStreamBudget = maxStarsOrNodes;
        // Assume we have an average of 1000 stars per star. TODO: Append stack if we reach the end?
        maxStarsOrNodes /= 1000;
    }*/
    // Build stack back-to-front.
    for (long long idx = maxStarsOrNodes - 1; idx >= 0; --idx) {
        _freeSpotsInBuffer.push(static_cast<int>(idx));
    }
    _maxStackSize = _freeSpotsInBuffer.size();
    LINFO("StackSize: " + std::to_string(maxStarsOrNodes) );
}

// Inserts star values in correct position in Octree.
void OctreeManager::insert(std::vector<float> starValues) {

    size_t index = getChildIndex(starValues[0], starValues[1], starValues[2]);

    insertInNode(_root->Children[index], starValues);
}

// Slices LOD data so only the MAX_STARS_PER_NODE brightest stars are stores in inner nodes.
// If <branchIndex> is defined then only that branch will be sliced.
void OctreeManager::sliceLodData(size_t branchIndex) {
    if (branchIndex != 8) {
        sliceNodeLodCache(_root->Children[branchIndex]);
    }
    else {
        for (int i = 0; i < 7; ++i) {
            sliceNodeLodCache(_root->Children[i]);
        }
    }
}

// Prints the whole tree structure, including number of stars per node.
void OctreeManager::printStarsPerNode() const {

    auto accumulatedString = std::string();

    for (int i = 0; i < 8; ++i) {
        auto prefix = "{" + std::to_string(i);
        accumulatedString += printStarsPerNode(_root->Children[i], prefix);
    }
    LINFO(fmt::format("Number of stars per node: \n{}", accumulatedString));
    LINFO(fmt::format("Number of leaf nodes: {}", std::to_string(_numLeafNodes)));
    LINFO(fmt::format("Number of inner nodes: {}", std::to_string(_numInnerNodes)));
    LINFO(fmt::format("Depth of tree: {}", std::to_string(_totalDepth)));
}

// Checks if any nodes needs to be loaded/removed and if so adds the indices to a reading 
// or removing queue.
void OctreeManager::fetchSurroundingNodes(glm::dvec3 cameraPos, glm::dvec3 cameraViewDir) {
    
    // If entire dataset fits in RAM then load the entire dataset asynchronously now.
    // Nodes will be rendered when they've been made available.
    if (_datasetFitInMemory) {
        // Only traverse Octree once!
        if (_parentNodeOfCamera == 8) {
            for (int i = 0; i < 8; ++i) {
                // Use multithreading to load files and detach thread from main execution 
                // so it can execute independently. Thread will be destroyed when finished!
                std::thread(&OctreeManager::fetchChildrenNodes, this, _root->Children[i], 
                    true).detach();
            }
            _parentNodeOfCamera = 0;
        }
        return;
    }

    // Get leaf node in which the camera resides.
    glm::fvec3 fCameraPos = static_cast<glm::fvec3>(cameraPos / 
        (1000.0 * distanceconstants::Parsec));
    size_t idx = getChildIndex(fCameraPos.x, fCameraPos.y, fCameraPos.z);
    auto node = _root->Children[idx];

    while (!node->isLeaf) {
        idx = getChildIndex(fCameraPos.x, fCameraPos.y, fCameraPos.z,
            node->originX, node->originY, node->originZ);
        node = node->Children[idx];
    }
    unsigned long long leafId = node->octreePositionIndex;
    unsigned long long firstParentId = leafId / 10;
    unsigned long long secondParentId = leafId / 100;
    unsigned long long thirdParentId = leafId / 1000;

    // Return early if camera resides in the same first parent as before!
    // Otherwise camera has moved and may need to load more nodes!
    if (_parentNodeOfCamera == firstParentId) return;
    _parentNodeOfCamera = firstParentId;

    // Get the 3^3 closest parents and load all their (eventual) children.
    for (int x = -1; x <= 1; x+=1) {
        for (int y = -2; y <= 2; y+=2) {
            for (int z = -4; z <= 4; z+=4) {
                // Fetch all stars the 216 closest leaf nodes. 
                findAndFetchNeighborNode(firstParentId, x, y, z);
                // Fetch LOD stars from 208 parents one and two layer(s) up.
                if (x != 0 || y != 0 || z != 0) {
                    findAndFetchNeighborNode(secondParentId, x, y, z);
                    findAndFetchNeighborNode(thirdParentId, x, y, z);
                }
            }
        }
    }

    // TODO: Use CameraViewDir to construct a priority queue for rest of nodes?
    // + Get LOD from outer layer nodes!
    // + Remove nodes that's no longer in our closest proximity or in a lower layer! 
    
}

// Finds the neighboring node on the same level (or a higher level if there is no corresponding level)
// in the specified direction. Also fetches data from found node if it's not already loaded.
void OctreeManager::findAndFetchNeighborNode(unsigned long long firstParentId, int x, int y, int z) {

    unsigned long long parentId = firstParentId;
    auto indexStack = std::stack<int>();

    //----------------- Change first index -------------------//
    int nodeIndex = parentId % 10;

    int dx = (nodeIndex % 2 == 0) ? 1 : -1;
    int dy = (nodeIndex % 4 < 2) ? 2 : -2;
    int dz = (nodeIndex < 4) ? 4 : -4;

    // Determine if we need to switch any side (find a higher common parent). 
    bool needToSwitchX = (x == dx);
    bool needToSwitchY = (y == dy);
    bool needToSwitchZ = (z == dz);

    if (!needToSwitchX) x = -x;
    if (!needToSwitchY) y = -y;
    if (!needToSwitchZ) z = -z;

    // Update node index and store it at the back of our stack.
    nodeIndex += x + y + z;
    indexStack.push(nodeIndex);
    parentId /= 10;

    //--------- Change all indices until we find a common parent --------------//
    while (parentId != 8 && (needToSwitchX || needToSwitchY || needToSwitchZ) ) {

        nodeIndex = parentId % 10;

        dx = (nodeIndex % 2 == 0) ? 1 : -1;
        dy = (nodeIndex % 4 < 2) ? 2 : -2;
        dz = (nodeIndex < 4) ? 4 : -4;

        if (needToSwitchX) {
            if (x != dx) needToSwitchX = false;
            nodeIndex += dx;
        }
        if (needToSwitchY) {
            if (y != dy) needToSwitchY = false;
            nodeIndex += dy;
        }
        if (needToSwitchZ) {
            if (z != dz) needToSwitchZ = false;
            nodeIndex += dz;
        }

        indexStack.push(nodeIndex);
        parentId /= 10;
    }

    // Take care of edge cases. If we got to the root but still need to switch to a 
    // common parent then no neighbor exists in that direction. 
    if (needToSwitchX || needToSwitchY || needToSwitchZ) return;

    // Continue to root if we didn't reach it.
    while (parentId != 8) {
        nodeIndex = parentId % 10;
        indexStack.push(nodeIndex);
        parentId /= 10;
    }

    // Traverse to that parent node (as long as such a child exists!).
    auto node = _root->Children[indexStack.top()];
    indexStack.pop();
    while (!indexStack.empty() && !node->Children[indexStack.top()]->isLeaf) {
        node = node->Children[indexStack.top()];
        indexStack.pop();
    }

    // Fetch all children nodes from found parent. Use multithreading to load files 
    // asynchronously! Detach thread from main execution so it can execute independently. 
    // Thread will then be destroyed when it has finished!
    std::thread(&OctreeManager::fetchChildrenNodes, this, node, false).detach();
}

// Builds render data structure by traversing the Octree and checking for intersection 
// with view frustum. Every vector in map contains data for one node.  
std::map<int, std::vector<float>> OctreeManager::traverseData(const glm::mat4 mvp, 
    const glm::vec2 screenSize, int& deltaStars, gaiamission::RenderOption option) {

    auto renderData = std::map<int, std::vector<float>>();
    bool innerRebuild = false;

    // Reclaim indices from previous render call. 
    for (auto removedKey = _removedKeysInPrevCall.rbegin();
        removedKey != _removedKeysInPrevCall.rend(); ++removedKey) {

        // Uses a reverse loop to try to decrease the biggest chunk.
        if (*removedKey == _biggestChunkIndexInUse - 1) {
            _biggestChunkIndexInUse = *removedKey;
            LINFO("Decreased size to: " + std::to_string(_biggestChunkIndexInUse) +
                " FreeSpotsInVBO: " + std::to_string(_freeSpotsInBuffer.size()));
        }
        _freeSpotsInBuffer.push(*removedKey);
    }
    // Clear cache of removed keys before next render call.
    if (!_removedKeysInPrevCall.empty()) {
        _removedKeysInPrevCall.clear();
    }

    // Rebuild VBO from scratch if we're not using most of it but have a high max index.
    if (_biggestChunkIndexInUse > _maxStackSize * 4 / 5 &&
        _freeSpotsInBuffer.size() > _maxStackSize * 5 / 6) {
        LINFO("Rebuilding VBO! - Biggest Chunk: " + std::to_string(_biggestChunkIndexInUse) +
            " 4/5: " + std::to_string(_maxStackSize * 4 / 5) +
            " FreeSpotsInVBO: " + std::to_string(_freeSpotsInBuffer.size()) +
            " 5/6: " + std::to_string(_maxStackSize * 5 / 6));
        initBufferIndexStack(_maxStackSize, _useVBO, _datasetFitInMemory);
        innerRebuild = true;
    }

    for (size_t i = 0; i < 8; ++i) {
        auto tmpData = checkNodeIntersection(_root->Children[i], mvp, screenSize, deltaStars, 
            option);
        // Observe that if there exists identical keys in renderData then those values in 
        // tmpData will be ignored! Thus we store the removed keys until next render call!
        renderData.insert(tmpData.begin(), tmpData.end());
    }

    if (_rebuildBuffer) {
        if (_useVBO) {
            // We need to overwrite bigger indices that had data before! No need for SSBO.
            auto idxToRemove = std::map<int, std::vector<float>>();
            for (int idx : _removedKeysInPrevCall) {
                idxToRemove[idx] = std::vector<float>();
            }

            // This will only insert indices that doesn't already exist in map (i.e. > biggestIdx).
            renderData.insert(idxToRemove.begin(), idxToRemove.end());
        }
        if (innerRebuild) { deltaStars = 0; }
        
        // Clear potential removed keys for both VBO and SSBO! 
        _removedKeysInPrevCall.clear();
        _rebuildBuffer = false;
        LINFO("After rebuild - Biggest Chunk: " + std::to_string(_biggestChunkIndexInUse) +
            " _freeSpotsInBuffer.size(): " + std::to_string(_freeSpotsInBuffer.size()));
    }

    // TODO: Clean up RAM!
    // update _cpuRamBudget += XX; & node->isLoaded = false;

    return renderData;
}

// Builds full render data structure by traversing all leaves in the Octree. 
std::vector<float> OctreeManager::getAllData(gaiamission::RenderOption option) {
    auto fullData = std::vector<float>();

    for (size_t i = 0; i < 8; ++i) {
        auto tmpData = getNodeData(_root->Children[i], option);
        fullData.insert(fullData.end(), tmpData.begin(), tmpData.end());
    }
    return fullData;
}

// Removes all data from Octree, or from a specific branch if specified. 
void OctreeManager::clearAllData(int branchIndex) {

    // Don't clear everything if not needed.
    if (branchIndex != -1) {
        clearNodeData(_root->Children[branchIndex]);
    }
    else {
        for (size_t i = 0; i < 8; ++i) {
            clearNodeData(_root->Children[i]);
        }
    }
}

// Write entire Octree structure to a file.
// <writeData> defines if data should be included or if only structure should be written. 
void OctreeManager::writeToFile(std::ofstream& outFileStream, bool writeData){
    
    outFileStream.write(reinterpret_cast<const char*>(&_valuesPerStar), sizeof(int32_t));
    outFileStream.write(reinterpret_cast<const char*>(&MAX_STARS_PER_NODE), sizeof(int32_t));
    outFileStream.write(reinterpret_cast<const char*>(&MAX_DIST), sizeof(int32_t));

    // Use pre-traversal (Morton code / Z-order).
    for (size_t i = 0; i < 8; ++i) {
        writeNodeToFile(outFileStream, _root->Children[i], writeData);
    }
}

// Write a node to outFileStream.
// <writeData> defines if data should be included or if only structure should be written. 
void OctreeManager::writeNodeToFile(std::ofstream& outFileStream, 
    std::shared_ptr<OctreeNode> node, bool writeData) {

    // Write node structure.
    bool isLeaf = node->isLeaf;
    int32_t numStars = static_cast<int32_t>(node->numStars);
    outFileStream.write(reinterpret_cast<const char*>(&isLeaf), sizeof(bool));
    outFileStream.write(reinterpret_cast<const char*>(&numStars), sizeof(int32_t));

    // Write node data if specified
    if (writeData) {
        std::vector<float> nodeData = node->posData;
        nodeData.insert(nodeData.end(), node->colData.begin(), node->colData.end());
        nodeData.insert(nodeData.end(), node->velData.begin(), node->velData.end());
        int32_t nDataSize = static_cast<int32_t>(nodeData.size());
        size_t nBytes = nDataSize * sizeof(nodeData[0]);

        outFileStream.write(reinterpret_cast<const char*>(&nDataSize), sizeof(int32_t));
        if (nDataSize > 0) {
            outFileStream.write(reinterpret_cast<const char*>(nodeData.data()), nBytes);
        }
    }

    // Write children to file (in Morton order) if we're in an inner node.
    if (!node->isLeaf) {
        for (size_t i = 0; i < 8; ++i) {
            writeNodeToFile(outFileStream, node->Children[i], writeData);
        }
    }
}


// Read a constructed Octree from a file. Returns the total number of (distinct) stars read.
// <readData> defines if full data or only structure should be read. 
int OctreeManager::readFromFile(std::ifstream& inFileStream, bool readData, std::string folderPath) {
    int nStarsRead = 0;
    int oldMaxdist = static_cast<int>(MAX_DIST);

    // If we're not reading data then we need to stream from files later on.
    _streamOctree = !readData;
    if (_streamOctree) _streamFolderPath = folderPath;

    _valuesPerStar = 0;
    inFileStream.read(reinterpret_cast<char*>(&_valuesPerStar), sizeof(int32_t));
    inFileStream.read(reinterpret_cast<char*>(&MAX_STARS_PER_NODE), sizeof(int32_t));
    inFileStream.read(reinterpret_cast<char*>(&MAX_DIST), sizeof(int32_t));

    LINFO("Max stars per node in read Octree: " + std::to_string(MAX_STARS_PER_NODE) + 
        " - Radius of root layer: " + std::to_string(MAX_DIST));

    // Octree Manager root halfDistance must be updated before any nodes are created!
    if (MAX_DIST != oldMaxdist) {
        for (size_t i = 0; i < 8; ++i) {
            _root->Children[i]->halfDimension = MAX_DIST / 2.f;
            _root->Children[i]->originX = (i % 2 == 0) ? _root->Children[i]->halfDimension
                : -_root->Children[i]->halfDimension;
            _root->Children[i]->originY = (i % 4 < 2) ? _root->Children[i]->halfDimension
                : -_root->Children[i]->halfDimension;
            _root->Children[i]->originZ = (i < 4) ? _root->Children[i]->halfDimension
                : -_root->Children[i]->halfDimension;
        }
    }

    if (_valuesPerStar != (POS_SIZE + COL_SIZE + VEL_SIZE)) {
        LERROR("Read file doesn't have the same structure of render parameters!");
    }

    // Use the same technique to construct octree from file. 
    for (size_t i = 0; i < 8; ++i) {
        nStarsRead += readNodeFromFile(inFileStream, _root->Children[i], readData);
    }
    return nStarsRead;
}

// Read a node from file and its potential children.
// <readData> defines if full data or only structure should be read. 
int OctreeManager::readNodeFromFile(std::ifstream& inFileStream, 
    std::shared_ptr<OctreeNode> node, bool readData) {

    // Read node structure.
    bool isLeaf;
    int32_t numStars = 0;

    inFileStream.read(reinterpret_cast<char*>(&isLeaf), sizeof(bool));
    inFileStream.read(reinterpret_cast<char*>(&numStars), sizeof(int32_t));

    node->isLeaf = isLeaf;
    node->numStars = numStars;

    // Read node data if specified. 
    if (readData) {
        int32_t nDataSize = 0;

        inFileStream.read(reinterpret_cast<char*>(&nDataSize), sizeof(int32_t));

        if (nDataSize > 0) {
            auto fetchedData = std::vector<float>(nDataSize, 0.0f);
            size_t nBytes = nDataSize * sizeof(fetchedData[0]);

            inFileStream.read(reinterpret_cast<char*>(&fetchedData[0]), nBytes);

            int starsInNode = static_cast<int>(nDataSize / _valuesPerStar);
            auto posEnd = fetchedData.begin() + (starsInNode * POS_SIZE);
            auto colEnd = posEnd + (starsInNode * COL_SIZE);
            auto velEnd = colEnd + (starsInNode * VEL_SIZE);
            node->posData = std::move(std::vector<float>(fetchedData.begin(), posEnd));
            node->colData = std::move(std::vector<float>(posEnd, colEnd));
            node->velData = std::move(std::vector<float>(colEnd, velEnd));
        }
    }

    // Create children if we're in an inner node and read from the corresponding nodes.
    if (!node->isLeaf) {
        numStars = 0;
        createNodeChildren(node);
        for (size_t i = 0; i < 8; ++i) {
            numStars += readNodeFromFile(inFileStream, node->Children[i], readData);
        }
    }

    // Return the number of stars in the entire branch, no need to check children.
    return numStars;
}

// Write specified part of Octree to multiple files, including all data.
void OctreeManager::writeToMultipleFiles(std::string outFolderPath, size_t branchIndex) {

    // Write entire branch to disc, with one file per node.
    std::string outFilePrefix = outFolderPath + std::to_string(branchIndex);
    writeNodeToMultipleFiles(outFilePrefix, _root->Children[branchIndex]);
}

// Write node data to a file. outFilePrefix specifies the name of file.
void OctreeManager::writeNodeToMultipleFiles(std::string outFilePrefix, 
    std::shared_ptr<OctreeNode> node) {

    // Prepare node data, save nothing else.
    std::vector<float> nodeData = node->posData;
    nodeData.insert(nodeData.end(), node->colData.begin(), node->colData.end());
    nodeData.insert(nodeData.end(), node->velData.begin(), node->velData.end());
    int32_t nDataSize = static_cast<int32_t>(nodeData.size());
    size_t nBytes = nDataSize * sizeof(nodeData[0]);

    // Only open output stream if we have any values to write.
    if (nDataSize > 0) {

        // Use Morton code to name file (placement in Octree). 
        std::string outPath = outFilePrefix + BINARY_SUFFIX;
        std::ofstream outFileStream(outPath, std::ofstream::binary);
        if (outFileStream.good()) {

            //LINFO("Write " + std::to_string(nDataSize) + " values to " + outPath);
            outFileStream.write(reinterpret_cast<const char*>(&nDataSize), sizeof(int32_t));
            outFileStream.write(reinterpret_cast<const char*>(nodeData.data()), nBytes);

            outFileStream.close();
        }
        else {
            LERROR(fmt::format("Error opening file: {} as output data file.", outPath));
        }
    }

    // Recursively write children to file (in Morton order) if we're in an inner node.
    if (!node->isLeaf) {
        for (size_t i = 0; i < 8; ++i) {
            std::string newOutFilePrefix = outFilePrefix + std::to_string(i);
            writeNodeToMultipleFiles(newOutFilePrefix, node->Children[i]);
        }
    }
}

// Fetch data from all children of <parentNode>, as long as it's not already fetched,
// it exists and can fit in RAM. If <recursive> is true then all children's children will 
// be fetched as well.
void OctreeManager::fetchChildrenNodes(std::shared_ptr<OctreeManager::OctreeNode> parentNode,
    bool recursive) {
    
    for (int i = 0; i < 8; ++i) {
        // Fetch node data if we're streaming and it doesn't exist in RAM yet.
        // (As long as there is any RAM budget left and node actually has any data!)
        if (!parentNode->Children[i]->isLoaded && parentNode->Children[i]->numStars > 0 &&  
            _cpuRamBudget > static_cast<long long>(parentNode->Children[i]->numStars
            * (POS_SIZE + COL_SIZE + VEL_SIZE) * 4) ) {

            fetchNodeDataFromFile(parentNode->Children[i]);
        }

        // Fetch all Children's Children if recursive is set to true!
        if (recursive && !parentNode->Children[i]->isLeaf) {
            fetchChildrenNodes(parentNode->Children[i], true);
        }
    }
}

// Read node data from a specified file. Used in TraveseData to fetch potential nodes for next frame.
// Only call if node file exsists (i.e. has any data) and is not already loaded.
void OctreeManager::fetchNodeDataFromFile(std::shared_ptr<OctreeNode> node) {

    // Remove root ID ("8") from index before loading file.
    std::string posId = std::to_string(node->octreePositionIndex);
    posId.erase(posId.begin());

    std::string inFilePath = _streamFolderPath + posId + BINARY_SUFFIX;
    std::ifstream inFileStream(inFilePath, std::ifstream::binary);
    //LINFO("Fetch node data file: " + inFilePath);

    if (inFileStream.good()) {
        // Read node data.
        int32_t nDataSize = 0;

        // Octree knows if we have any data in this node = it exists. Otherwise don't call it!
        inFileStream.read(reinterpret_cast<char*>(&nDataSize), sizeof(int32_t));

        auto readData = std::vector<float>(nDataSize, 0.0f);
        int nBytes = nDataSize * sizeof(readData[0]);
        if (nDataSize > 0) {
            inFileStream.read(reinterpret_cast<char*>(&readData[0]), nBytes);
        }

        int starsInNode = static_cast<int>(nDataSize / _valuesPerStar);
        auto posEnd = readData.begin() + (starsInNode * POS_SIZE);
        auto colEnd = posEnd + (starsInNode * COL_SIZE);
        auto velEnd = colEnd + (starsInNode * VEL_SIZE);
        node->posData = std::move(std::vector<float>(readData.begin(), posEnd));
        node->colData = std::move(std::vector<float>(posEnd, colEnd));
        node->velData = std::move(std::vector<float>(colEnd, velEnd));

        // Keep track of nodes that are loaded and update CPU RAM budget.
        node->isLoaded = true;
        _cpuRamBudget -= nBytes;
    }
    else {
        LERROR("Error opening node data file: " +  inFilePath);
    }
}

// Return number of leaf nodes in Octree.
size_t OctreeManager::numLeafNodes() const {
    return _numLeafNodes;
}

// Return number of inner nodes in Octree. 
size_t OctreeManager::numInnerNodes() const {
    return _numInnerNodes;
}

// Return the total number of nodes in Octree. 
size_t OctreeManager::totalNodes() const {
    return _numLeafNodes + _numInnerNodes;
}

// Return the total depth of Octree. 
size_t OctreeManager::totalDepth() const {
    return _totalDepth;
}

// Return the set constant of max radius of first layer in Octree.
size_t OctreeManager::maxDist() const {
    return MAX_DIST;
}

// Return the set constant of max stars per node in Octree.
size_t OctreeManager::maxStarsPerNode() const {
    return MAX_STARS_PER_NODE;
}

// Return the largest index that the stack has given out thus far. 
size_t OctreeManager::biggestChunkIndexInUse() const {
    return _biggestChunkIndexInUse;
}

// Return current CPU RAM budget [Bytes]. 
long long OctreeManager::cpuRamBudget() const {
    return _cpuRamBudget;
}

// Return current budget for how many stars we can add to stream when using SSBOs. 
long long OctreeManager::ssboStarStreamBudget() const {
    return _ssboStarStreamBudget;
}

// Returns the correct index of child node. Maps [1,1,1] to 0 and [-1,-1,-1] to 7.
size_t OctreeManager::getChildIndex(float posX, float posY, float posZ, float origX,
    float origY, float origZ) {

    size_t index = 0;
    if (posX < origX) index += 1;
    if (posY < origY) index += 2;
    if (posZ < origZ) index += 4;
    return index;
}

// Private help function for insert(). Recursively checks nodes if they have any
// data that should be rendered this frame, depending on camera.
bool OctreeManager::insertInNode(std::shared_ptr<OctreeNode> node,
    std::vector<float> starValues, int depth) {

    if (node->isLeaf && node->numStars < MAX_STARS_PER_NODE) {
        // Node is a leaf and it's not yet full -> insert star.
        storeStarData(node, starValues);

        if (depth > _totalDepth) {
            _totalDepth = depth;
        }
        return true;
    }
    else if (node->isLeaf) {
        // Too many stars in leaf node, subdivide into 8 new nodes.
        // Create children and clean up parent.
        createNodeChildren(node);

        // Distribute stars from parent node into children. 
        for (size_t n = 0; n < MAX_STARS_PER_NODE; ++n) {
            // Position data.
            auto posBegin = node->posData.begin() + n * POS_SIZE;
            auto posEnd = posBegin + POS_SIZE;
            std::vector<float> tmpValues(posBegin, posEnd);
            // Color data.
            auto colBegin = node->colData.begin() + n * COL_SIZE;
            auto colEnd = colBegin + COL_SIZE;
            tmpValues.insert(tmpValues.end(), colBegin, colEnd);
            // Velocity data.
            auto velBegin = node->velData.begin() + n * VEL_SIZE;
            auto velEnd = velBegin + VEL_SIZE;
            tmpValues.insert(tmpValues.end(), velBegin, velEnd);

            // Find out which child that will inherit the data and store it. 
            size_t index = getChildIndex(tmpValues[0], tmpValues[1], tmpValues[2],
                node->originX, node->originY, node->originZ);
            insertInNode(node->Children[index], tmpValues, depth);
        }
    }

    // Node is an inner node, keep recursion going.
    // This will also take care of the new star when a subdivision has taken place.
    size_t index = getChildIndex(starValues[0], starValues[1], starValues[2],
        node->originX, node->originY, node->originZ);

    // Determine if new star should be kept in our LOD cache. 
    // Keeps track of the MAX_STARS_PER_NODE brightest nodes in children. 
    if (starValues[POS_SIZE] < (*std::prev(node->magOrder.end())).first ) {
        storeStarData(node, starValues);

        // Remove previous dimmest star at the back of ordered map.
        node->magOrder.erase(std::prev(node->magOrder.end()));
    }

    return insertInNode(node->Children[index], starValues, ++depth);
}

// Slices LOD cache data to the MAX_STARS_PER_NODE brightest stars. This needs to be 
// called after the last star has been inserted into Octree.
void OctreeManager::sliceNodeLodCache(std::shared_ptr<OctreeNode> node) {
    
    // Slice stored LOD data in inner nodes.
    if (!node->isLeaf) {
        std::vector<float> tmpPos;
        std::vector<float> tmpCol;
        std::vector<float> tmpVel;

        // Ordered map contain the MAX_STARS_PER_NODE brightest stars in all children!
        for (auto &[absMag, placement] : node->magOrder) {
            auto posBegin = node->posData.begin() + placement * POS_SIZE;
            auto colBegin = node->colData.begin() + placement * COL_SIZE;
            auto velBegin = node->velData.begin() + placement * VEL_SIZE;
            tmpPos.insert(tmpPos.end(), posBegin, posBegin + POS_SIZE);
            tmpCol.insert(tmpCol.end(), colBegin, colBegin + COL_SIZE);
            tmpVel.insert(tmpVel.end(), velBegin, velBegin + VEL_SIZE);
        }
        node->posData = std::move(tmpPos);
        node->colData = std::move(tmpCol);
        node->velData = std::move(tmpVel);
        node->numStars == node->magOrder.size(); // = MAX_STARS_PER_NODE
    }
    
}

// Private help function for insertInNode(). Stores star data in node and keeps track of
// the brightest stars in an ordered map.
void OctreeManager::storeStarData(std::shared_ptr<OctreeNode> node,
    std::vector<float> starValues) {

    // Insert star data at the back of vectors but store the position index in a map, sorted
    // by magnitude. Inverse relation (i.e. a lower magnitude means a brighter star!) 
    // so we can use the built-in weak ordering in map. LOD cache needs to be sliced later.
    float mag = starValues[POS_SIZE];
    node->magOrder.insert(std::make_pair(mag, node->numStars));
    node->numStars++;

    auto posEnd = starValues.begin() + POS_SIZE;
    auto colEnd = posEnd + COL_SIZE;
    node->posData.insert(node->posData.end(), starValues.begin(), posEnd);
    node->colData.insert(node->colData.end(), posEnd, colEnd);
    node->velData.insert(node->velData.end(), colEnd, starValues.end());
}

// Private help function for printStarsPerNode(). Recursively adds all nodes to the string.
std::string OctreeManager::printStarsPerNode(std::shared_ptr<OctreeNode> node,
    std::string prefix) const {

    // Print both inner and leaf nodes. 
    auto str = prefix + "} : " + std::to_string(node->numStars);

    if (node->isLeaf) {
        return str + " - [Leaf] \n";
    }
    else {
        str += " LOD: " + std::to_string(node->posData.size() / POS_SIZE) + " - [Parent] \n";
        for (int i = 0; i < 8; ++i) {
            auto pref = prefix + "->" + std::to_string(i);
            str += printStarsPerNode(node->Children[i], pref);
        }
        return str;
    }
}

// Private help function for traverseData(). Recursively checks which nodes intersects with
// the view frustum (interpreted as an AABB) and decides if data should be optimized away.
std::map<int, std::vector<float>> OctreeManager::checkNodeIntersection(std::shared_ptr<OctreeNode> node, 
    const glm::mat4 mvp, const glm::vec2 screenSize, int& deltaStars, gaiamission::RenderOption option) {

    auto fetchedData = std::map<int, std::vector<float>>();
    glm::vec3 debugPos;
    int depth  = static_cast<int>(log2( MAX_DIST / node->halfDimension ));

    // Calculate the corners of the node. 
    std::vector<glm::dvec4> corners(8);
    for (int i = 0; i < 8; ++i) {
        float x = (i % 2 == 0) ? node->originX + node->halfDimension
            : node->originX - node->halfDimension;
        float y = (i % 4 < 2) ? node->originY + node->halfDimension
            : node->originY - node->halfDimension;
        float z = (i < 4) ? node->originZ + node->halfDimension
            : node->originZ - node->halfDimension;
        glm::dvec3 pos = glm::dvec3(x, y, z) * 1000.0 * distanceconstants::Parsec;
        corners[i] = glm::dvec4(pos, 1.0);
        debugPos = glm::vec3(x, y, z);
    }

    // Check if node is visible from camera. If not then return early.
    if (!(_culler->isVisible(corners, mvp))) {
        // Check if this node or any of its children existed in cache previously. 
        // If so, then remove them from cache and add those indices to stack.
        fetchedData = removeNodeFromCache(node, deltaStars);
        return fetchedData;
    }

    // Take care of inner nodes.
    if (!(node->isLeaf)) {
        glm::vec2 nodeSize = _culler->getNodeSizeInPixels(screenSize);
        float totalPixels = nodeSize.x * nodeSize.y;

        // Check if we should return any LOD cache data. 
        // Multiply MinPixels with depth for smoother culling. 
        // TODO: Improve LOD checks and construction! Use numStars?
        if (totalPixels < MIN_TOTAL_PIXELS_LOD * depth) {

            // Get correct insert index from stack if node didn't exist already. 
            // Otherwise we will overwrite the old data. Key merging is not a problem here. 
            if (node->bufferIndex == DEFAULT_INDEX || _rebuildBuffer) {
                
                // Return empty if we couldn't claim a buffer stream index.
                if (!updateBufferIndex(node)) return fetchedData;

                // We're in an inner node, remove indices from potential children in cache!
                for (int i = 0; i < 8; ++i) {
                    auto tmpData = removeNodeFromCache(node->Children[i], deltaStars);
                    fetchedData.insert(tmpData.begin(), tmpData.end());
                }

                // Insert data and adjust stars added in this frame.
                fetchedData[node->bufferIndex] = constructInsertData(node, option, deltaStars);
            }
            return fetchedData;
        }
    }
    // Return node data if node is a leaf.
    else {
        // If node already is in cache then skip it, otherwise store it.
        if (node->bufferIndex == DEFAULT_INDEX || _rebuildBuffer) {
            
            // Return empty if we couldn't claim a buffer stream index.
            if (!updateBufferIndex(node)) return fetchedData;

            // Insert data and adjust stars added in this frame.
            fetchedData[node->bufferIndex] = constructInsertData(node, option, deltaStars);
        }
        return fetchedData;
    }

    // We're in a big, visible inner node -> remove it from cache if it existed.
    // But not its children -> set recursive check to false.
    fetchedData = removeNodeFromCache(node, deltaStars, false);

    // Recursively check if children should be rendered.
    for (size_t i = 0; i < 8; ++i) {
        // Observe that if there exists identical keys in fetchedData then those values in 
        // tmpData will be ignored! Thus we store the removed keys until next render call!
        auto tmpData = checkNodeIntersection(node->Children[i], mvp, screenSize, deltaStars, 
            option);
        fetchedData.insert(tmpData.begin(), tmpData.end());
    }
    return fetchedData;
}

// Checks if specified node existed in cache, and removes it if that's the case. 
// If node is an inner node then all children will be checked recursively as well.
std::map<int, std::vector<float>> OctreeManager::removeNodeFromCache(std::shared_ptr<OctreeNode> node, 
    int& deltaStars, bool recursive) {
    
    auto keysToRemove = std::map<int, std::vector<float>>();

    // If we're in rebuilding mode then there is no need to remove any nodes.
    if(_rebuildBuffer) return keysToRemove;

    // Check if this node was rendered == had a specified index.
    if (node->bufferIndex != DEFAULT_INDEX) {

        // Reclaim that index. However we need to wait until next render call to use it again!
        _removedKeysInPrevCall.insert(node->bufferIndex);

        // Insert dummy node at offset index that should be removed from render.
        keysToRemove[node->bufferIndex] = std::vector<float>();

        // Reset index and adjust stars removed this frame. 
        node->bufferIndex = DEFAULT_INDEX;
        int nStarsInNode = static_cast<int>(node->posData.size() / POS_SIZE);
        deltaStars -= nStarsInNode;
        if (!_useVBO) _ssboStarStreamBudget += nStarsInNode;
    }

    // Check children recursively if we're in an inner node. 
    if (!(node->isLeaf) && recursive) {
        for (int i = 0; i < 8; ++i) {
            auto tmpData = removeNodeFromCache(node->Children[i], deltaStars);
            keysToRemove.insert(tmpData.begin(), tmpData.end());
        }
    }
    return keysToRemove;
}

// Get data in all leaves regardless if visible or not.
std::vector<float> OctreeManager::getNodeData(std::shared_ptr<OctreeNode> node, 
    gaiamission::RenderOption option) {
    
    // Return node data if node is a leaf.
    if (node->isLeaf) {
        int dStars = 0;
        return constructInsertData(node, option, dStars);
    }

    // If we're not in a leaf, get data from all children recursively.
    auto nodeData = std::vector<float>();
    for (size_t i = 0; i < 8; ++i) {
        auto tmpData = getNodeData(node->Children[i], option);
        nodeData.insert(nodeData.end(), tmpData.begin(), tmpData.end());
    }
    return nodeData;
}

// Clear data from all nodes in Octree.
void OctreeManager::clearNodeData(std::shared_ptr<OctreeNode> node) {

    // Clear data and its allocated memory.
    node->posData.clear();
    node->posData.shrink_to_fit();
    node->colData.clear();
    node->colData.shrink_to_fit();
    node->velData.clear();
    node->velData.shrink_to_fit();

    if (!node->isLeaf) {
        // Remove data from all children recursively.
        for (size_t i = 0; i < 8; ++i) {
            clearNodeData(node->Children[i]);
        }
    }
}


// Contruct children for specified node.
void OctreeManager::createNodeChildren(std::shared_ptr<OctreeNode> node) {
    
    for (size_t i = 0; i < 8; ++i) {
        _numLeafNodes++;
        node->Children[i] = std::make_unique<OctreeNode>();
        node->Children[i]->isLeaf = true;
        node->Children[i]->isLoaded = false;
        node->Children[i]->bufferIndex = DEFAULT_INDEX;
        node->Children[i]->octreePositionIndex = (node->octreePositionIndex * 10) + i;
        node->Children[i]->numStars = 0;
        node->Children[i]->posData = std::vector<float>();
        node->Children[i]->colData = std::vector<float>();
        node->Children[i]->velData = std::vector<float>();
        node->Children[i]->magOrder = std::multimap<float, int>();
        node->Children[i]->halfDimension = node->halfDimension / 2.f;

        // Calculate new origin.
        node->Children[i]->originX = node->originX;
        node->Children[i]->originX += (i % 2 == 0) ? node->Children[i]->halfDimension
            : -node->Children[i]->halfDimension;
        node->Children[i]->originY = node->originY;
        node->Children[i]->originY += (i % 4 < 2) ? node->Children[i]->halfDimension
            : -node->Children[i]->halfDimension;
        node->Children[i]->originZ = node->originZ;
        node->Children[i]->originZ += (i < 4) ? node->Children[i]->halfDimension
            : -node->Children[i]->halfDimension;
    }

    // Clean up parent.
    node->isLeaf = false;
    _numLeafNodes--;
    _numInnerNodes++;
}

bool OctreeManager::updateBufferIndex(std::shared_ptr<OctreeNode> node) {
    if (node->bufferIndex != DEFAULT_INDEX) {
        // If we're rebuilding Buffer Index Cache then store indices to overwrite later. 
        _removedKeysInPrevCall.insert(node->bufferIndex);
    }

    // Return false if there are no more spots in our buffer. 
    // Or if we're streaming and node isn't loaded yet.
    if (_freeSpotsInBuffer.empty() || (_streamOctree && !node->isLoaded) ) {
        return false;
    }

    // Get correct insert index from stack.
    node->bufferIndex = _freeSpotsInBuffer.top();
    _freeSpotsInBuffer.pop();

    // Keep track of how many chunks are in use (ceiling).
    if (_freeSpotsInBuffer.empty()) {
        _biggestChunkIndexInUse++;
    }
    else if (_freeSpotsInBuffer.top() > _biggestChunkIndexInUse) {
        _biggestChunkIndexInUse = _freeSpotsInBuffer.top();
    }
    return true;
}

std::vector<float> OctreeManager::constructInsertData(std::shared_ptr<OctreeNode> node, 
    gaiamission::RenderOption option, int& deltaStars) {

    // Return early if node doesn't contain any stars!
    if (node->numStars == 0) return std::vector<float>();

    // Return empty if we're reached max streaming budget
    int nStarsInNode = static_cast<int>(node->posData.size() / POS_SIZE);
    //if ( !_useVBO && _ssboStarStreamBudget <= nStarsInNode ) return std::vector<float>();

    // Fill chunk by appending zeroes to data so we overwrite possible earlier values.
    // And more importantly so our attribute pointers knows where to read!
    auto insertData = std::vector<float>(node->posData.begin(), node->posData.end());
    if (_useVBO) {
        insertData.resize(POS_SIZE * MAX_STARS_PER_NODE, 0.f);
    }
    if (option != gaiamission::RenderOption::Static) {
        insertData.insert(insertData.end(), node->colData.begin(), node->colData.end());
        if (_useVBO) {
            insertData.resize((POS_SIZE + COL_SIZE) * MAX_STARS_PER_NODE, 0.f);
        }
        if (option == gaiamission::RenderOption::Motion) {
            insertData.insert(insertData.end(), node->velData.begin(), node->velData.end());
            if (_useVBO) {
                insertData.resize((POS_SIZE + COL_SIZE + VEL_SIZE) * MAX_STARS_PER_NODE, 0.f);
            }
        }
    }

    // Update deltaStars and Star streaming budget. 
    deltaStars += nStarsInNode;
    if (!_useVBO) _ssboStarStreamBudget -= nStarsInNode;

    return insertData;
}

}
