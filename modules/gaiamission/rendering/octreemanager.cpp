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
#include <openspace/util/distanceconstants.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>
#include <ghoul/fmt.h>


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
    , _biggestChunkIndexInUse(0)
    , _rebuildVBO(false)
{   }

OctreeManager::~OctreeManager() {   }

// Initialize a one layer Octree with root and 8 children. Covers all stars.
void OctreeManager::initOctree() {

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
    _numNodesPerFile = 0;
    _totalNodes = 0;

    for (size_t i = 0; i < 8; ++i) {
        _numLeafNodes++;
        _root->Children[i] = std::make_unique<OctreeNode>();
        _root->Children[i]->data = std::vector<float>();
        _root->Children[i]->isLeaf = true;
        _root->Children[i]->vboIndex = DEFAULT_INDEX;
        _root->Children[i]->lodInUse = 0;
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
void OctreeManager::initVBOIndexStack(int maxIndex) {
    
    // Clear stack if we've used it before.
    _biggestChunkIndexInUse = 0;
    _freeSpotsInVBO = std::stack<int>();

    // Build stack back-to-front.
    for (int idx = maxIndex - 1; idx >= 0; --idx) {
        _freeSpotsInVBO.push(idx);
    }
    _maxStackSize = static_cast<int>(_freeSpotsInVBO.size());
    LINFO("StackSize: " + std::to_string(_maxStackSize) );
}

// Inserts star values in correct position in Octree.
void OctreeManager::insert(std::vector<float> starValues) {

    size_t index = getChildIndex(starValues[0], starValues[1], starValues[2]);

    insertInNode(_root->Children[index], starValues);
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

// Builds render data structure by traversing the Octree and checking for intersection 
// with view frustum. Every vector in map contains data for one node.  
std::unordered_map<int, std::vector<float>> OctreeManager::traverseData(
    const glm::mat4 mvp, const glm::vec2 screenSize, int& deltaStars) {

    auto renderData = std::unordered_map<int, std::vector<float>>();

    // Reclaim indices from previous render call. 
    for (auto removedKey = _removedKeysInPrevCall.rbegin();
        removedKey != _removedKeysInPrevCall.rend(); ++removedKey) {

        // Uses a reverse loop to try to decrease the biggest chunk.
        if (*removedKey == _biggestChunkIndexInUse - 1) {
            _biggestChunkIndexInUse = *removedKey;
            LINFO("Decreased size to: " + std::to_string(_biggestChunkIndexInUse) +
                " FreeSpotsInVBO: " + std::to_string(_freeSpotsInVBO.size()));
        }
        _freeSpotsInVBO.push(*removedKey);
    }
    // Clear cache of removed keys before next render call.
    _removedKeysInPrevCall.clear();

    // Rebuild VBO from scratch if we're not using most of it but have a high max index.
    if (_biggestChunkIndexInUse > _maxStackSize * 4 / 5 &&
        _freeSpotsInVBO.size() > _maxStackSize * 5 / 6) {
        LINFO("Rebuilding VBO! - Biggest Chunk: " + std::to_string(_biggestChunkIndexInUse) +
            " 4/5: " + std::to_string(_maxStackSize * 4 / 5) +
            " FreeSpotsInVBO: " + std::to_string(_freeSpotsInVBO.size()) +
            " 5/6: " + std::to_string(_maxStackSize * 5 / 6));
        initVBOIndexStack(_maxStackSize);
        _rebuildVBO = true;
    }

    for (size_t i = 0; i < 8; ++i) {
        auto tmpData = checkNodeIntersection(_root->Children[i], mvp, screenSize, deltaStars);
        // Observe that if there exists identical keys in renderData then those values in 
        // tmpData will be ignored! Thus we store the removed keys until next render call!
        renderData.insert(tmpData.begin(), tmpData.end());
    }

    if (_rebuildVBO) {
        // We need to overwrite bigger indices that had data before!
        auto idxToRemove = std::unordered_map<int, std::vector<float>>();
        for (size_t idx : _removedKeysInPrevCall) {
            idxToRemove[idx] = std::vector<float>();
        }
        _removedKeysInPrevCall.clear();

        // This will only insert indices that doesn't already exist in map (i.e. > biggestIdx).
        renderData.insert(idxToRemove.begin(), idxToRemove.end());
        deltaStars = 0;
        _rebuildVBO = false;
        LINFO("After rebuild - Biggest Chunk: " + std::to_string(_biggestChunkIndexInUse) +
            " _freeSpotsInVBO.size(): " + std::to_string(_freeSpotsInVBO.size()));
    }

    return renderData;
}

// Builds full render data structure by traversing all leaves in the Octree. 
std::vector<float> OctreeManager::getAllData() {
    auto fullData = std::vector<float>();

    for (size_t i = 0; i < 8; ++i) {
        auto tmpData = getNodeData(_root->Children[i]);
        fullData.insert(fullData.end(), tmpData.begin(), tmpData.end());
    }
    return fullData;
}

// Return number of leaf nodes in Octree.
size_t OctreeManager::numLeafNodes() const {
    return _numLeafNodes;
}

// Return the set constant of max stars per node in Octree.
size_t OctreeManager::maxStarsPerNode() const {
    return MAX_STARS_PER_NODE;
}

// Return the largest index that the stack has given out thus far. 
size_t OctreeManager::biggestChunkIndexInUse() const {
    return _biggestChunkIndexInUse;
}

// Return the total number of nodes in Octree. 
size_t OctreeManager::totalNodes() const {
    return _numLeafNodes + _numInnerNodes;
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
        node->numStars++;
        node->data.insert(node->data.end(), starValues.begin(), starValues.end());
        if (depth > _totalDepth) {
            _totalDepth = depth;
        }
        return true;
    }
    else if (node->isLeaf) {
        // Too many stars in leaf node, subdivide into 8 new nodes.
        _valuesPerStar = starValues.size();

        // Create children.
        for (size_t i = 0; i < 8; ++i) {
            _numLeafNodes++;
            node->Children[i] = std::make_unique<OctreeNode>();
            node->Children[i]->isLeaf = true;
            node->Children[i]->vboIndex = DEFAULT_INDEX;
            node->Children[i]->lodInUse = 0;
            node->Children[i]->numStars = 0;
            node->Children[i]->data = std::vector<float>();
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

        // Construct an initial LOD cache in inner node for faster traversals during render.
        auto tmpLodNode = std::make_shared<OctreeNode>();
        constructLodCache(tmpLodNode);

        // Distribute stars from parent node into children. 
        for (size_t n = 0; n < node->numStars; ++n) {
            auto first = node->data.begin() + n * _valuesPerStar;
            auto last = node->data.begin() + n * _valuesPerStar + _valuesPerStar;
            std::vector<float> tmpValues(first, last);

            size_t index = getChildIndex(tmpValues[0], tmpValues[1], tmpValues[2],
                node->originX, node->originY, node->originZ);
            insertInNode(node->Children[index], tmpValues, depth);
            
            // Check if we should keep this star in LOD cache. 
            insertStarInLodCache(tmpLodNode, starValues);
        }

        // Clean up parent.
        node->isLeaf = false;
        _numLeafNodes--;
        _numInnerNodes++;
        
        // Copy LOD cache data from the first MAX_STARS_PER_NODE stars.
        // Don't use LOD cache for our more shallow layers.
        node->data = (depth > FIRST_LOD_DEPTH) ? tmpLodNode->data : std::vector<float>();
    }

    // Node is an inner node, keep recursion going.
    // This will also take care of the new star when a subdivision has taken place.
    size_t index = getChildIndex(starValues[0], starValues[1], starValues[2],
        node->originX, node->originY, node->originZ);

    // Determine if new star should be kept in our LOD cache. Don't add if chunk is full.
    // Don't use LOD cache for our more shallow layers.
    if (node->data.size() / _valuesPerStar < MAX_STARS_PER_NODE && depth > FIRST_LOD_DEPTH) {
        insertStarInLodCache(node, starValues);
    }

    // Increase counter for inner node to keep track of total stars in all children as well.
    node->numStars++;
    return insertInNode(node->Children[index], starValues, ++depth);
}

// Private help function for insertInNode(). Constructs our LOD cache with 1 virtual star.
void OctreeManager::constructLodCache(std::shared_ptr<OctreeNode> node) {

    // Add this node's origin as the only value. 
    // This will be used initially for comparisons in insertStarInLodCache().
    std::vector<float> insertData(_valuesPerStar, 0.f);
    insertData[0] = node->originX;
    insertData[1] = node->originY;
    insertData[2] = node->originZ;
    node->data.insert(node->data.end(), insertData.begin(), insertData.end());
}

// Private help function for insertInNode(). Determines if star should be stored in LOD.
void OctreeManager::insertStarInLodCache(std::shared_ptr<OctreeNode> node,
    std::vector<float> starValues) {
    
    // Add star if it is further away from last inserted star with a set threshold.
    std::vector<float> cachePosData(node->data.end() - _valuesPerStar, 
        node->data.end() - _valuesPerStar + 3);
    glm::vec3 lastCachePos(cachePosData[0], cachePosData[1], cachePosData[2]);
    glm::vec3 starPos(starValues[0], starValues[1], starValues[2]);
    float dist = glm::distance(lastCachePos, starPos);
    
    // Add star if it's more than a quarter of node's size away.
    if (dist > node->halfDimension / 2.0) {
        node->data.insert(node->data.end(), starValues.begin(), starValues.end());
    }
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
        str += " LOD: " + std::to_string(node->data.size() / _valuesPerStar) + " - [Parent] \n";
        for (int i = 0; i < 8; ++i) {
            auto pref = prefix + "->" + std::to_string(i);
            str += printStarsPerNode(node->Children[i], pref);
        }
        return str;
    }
}

// Private help function for traverseData(). Recursively checks which nodes intersects with
// the view frustum (interpreted as an AABB) and decides if data should be optimized away.
std::unordered_map<int, std::vector<float>> OctreeManager::checkNodeIntersection(
    std::shared_ptr<OctreeNode> node, const glm::mat4 mvp, const glm::vec2 screenSize,
    int& deltaStars) {

    auto fetchedData = std::unordered_map<int, std::vector<float>>();
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
        auto first = node->data.begin();
        auto lodData = std::vector<float>();

        // Use full data in LOD cache. Multiply MinPixels with depth for smoother culling. 
        if (totalPixels < MIN_TOTAL_PIXELS_LOD * depth) {
            lodData = std::vector<float>(first, node->data.end());
        }

        // Return LOD data if we've triggered any check. 
        if (!lodData.empty()) {
            // Get correct insert index from stack if node didn't exist already. 
            // Otherwise we will overwrite the old data. Key merging is not a problem here. 
            if (node->vboIndex == DEFAULT_INDEX || _rebuildVBO) {
                if (node->vboIndex != DEFAULT_INDEX) {
                    // If we're rebuilding VBO cache then store indices to overwrite later. 
                    _removedKeysInPrevCall.insert(node->vboIndex);
                }
                node->vboIndex = _freeSpotsInVBO.top();
                _freeSpotsInVBO.pop();

                // Keep track of how many chunks are in use (ceiling).
                if (_freeSpotsInVBO.top() > _biggestChunkIndexInUse) {
                    _biggestChunkIndexInUse = _freeSpotsInVBO.top();
                }

                // We're in an inner node, remove indices from potential children in cache!
                for (int i = 0; i < 8; ++i) {
                    auto tmpData = removeNodeFromCache(node->Children[i], deltaStars);
                    fetchedData.insert(tmpData.begin(), tmpData.end());
                }

                // Insert data and adjust stars added in this frame.
                fetchedData[node->vboIndex] = lodData;
                deltaStars += static_cast<int>(lodData.size());
                node->lodInUse = lodData.size();
            }
            else {
                // This node existed in cache before. Check if it was for the same level.
                if (lodData.size() != node->lodInUse) {
                    // Insert data and adjust stars added in this frame.
                    fetchedData[node->vboIndex] = lodData;
                    deltaStars += static_cast<int>(lodData.size()) 
                        - static_cast<int>(node->lodInUse);
                    node->lodInUse = lodData.size();
                }
            }
            return fetchedData;
        }
    }
    // Return node data if node is a leaf.
    else {
        // If node already is in cache then skip it, otherwise store it.
        if (node->vboIndex == DEFAULT_INDEX || _rebuildVBO) {
            if (node->vboIndex != DEFAULT_INDEX) {
                // If we're rebuilding VBO cache then store indices to overwrite later. 
                _removedKeysInPrevCall.insert(node->vboIndex);
            }
            // Get correct insert index from stack.
            node->vboIndex = _freeSpotsInVBO.top();
            _freeSpotsInVBO.pop();

            // Keep track of how many chunks are in use (ceiling).
            if (_freeSpotsInVBO.top() > _biggestChunkIndexInUse) {
                _biggestChunkIndexInUse = _freeSpotsInVBO.top();
            }

            // Insert data and adjust stars added in this frame.
            fetchedData[node->vboIndex] = node->data;
            deltaStars += static_cast<int>(node->data.size());
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
        auto tmpData = checkNodeIntersection(node->Children[i], mvp, screenSize, deltaStars);
        fetchedData.insert(tmpData.begin(), tmpData.end());
    }
    return fetchedData;
}

// Checks if specified node existed in cache, and removes it if that's the case. 
// If node is an inner node then all children will be checked recursively as well.
std::unordered_map<int, std::vector<float>> OctreeManager::removeNodeFromCache(
    std::shared_ptr<OctreeNode> node, int& deltaStars, bool recursive) {
    
    auto keysToRemove = std::unordered_map<int, std::vector<float>>();

    // If we're in rebuilding mode then there is no need to remove any nodes.
    if(_rebuildVBO) return keysToRemove;

    // Check if this node was rendered == had a specified index.
    if (node->vboIndex != DEFAULT_INDEX) {

        // Reclaim that index. However we need to wait until next render call to use it again!
        _removedKeysInPrevCall.insert(node->vboIndex);

        // Insert dummy node at offset index that should be removed from render.
        keysToRemove[node->vboIndex] = std::vector<float>();

        // Reset index and adjust stars removed this frame. 
        node->vboIndex = DEFAULT_INDEX;
        if (node->lodInUse > 0) {
            // If we're removing an inner node from cache then only decrease correct amount. 
            deltaStars -= static_cast<int>(node->lodInUse);
            node->lodInUse = 0;
        }
        else {
            deltaStars -= static_cast<int>(node->data.size());
        }
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
std::vector<float> OctreeManager::getNodeData(std::shared_ptr<OctreeNode> node) {
    
    // Return node data if node is a leaf.
    if (node->isLeaf) {
        return node->data;
    }

    // If we're not in a leaf, get data from all children recursively.
    auto nodeData = std::vector<float>();
    for (size_t i = 0; i < 8; ++i) {
        auto tmpData = getNodeData(node->Children[i]);
        nodeData.insert(nodeData.end(), tmpData.begin(), tmpData.end());
    }
    return nodeData;
}

}
