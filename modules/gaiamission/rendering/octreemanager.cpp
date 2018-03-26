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
{   }

OctreeManager::~OctreeManager() {   }

// Initialize a one layer Octree with root and 8 children. Covers all stars.
void OctreeManager::initOctree() {

    LDEBUG("Initializing Octree");
    _root = std::make_unique<OctreeNode>();
    _culler = std::make_unique<OctreeCuller>(
        globebrowsing::AABB3(glm::vec3(-1, -1, 0), glm::vec3(1, 1, 1e35))
    );
    _freeSpotsInVBO = std::stack<int>();
    _removedKeysInPrevCall = std::set<int>();

    for (size_t i = 0; i < 8; ++i) {
        _numLeafNodes++;
        _root->Children[i] = std::make_unique<OctreeNode>();
        _root->Children[i]->data = std::vector<float>();
        _root->Children[i]->isLeaf = true;
        _root->Children[i]->vboIndex = DEFAULT_INDEX;
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
    // Build stack back-to-front.
    for (int idx = maxIndex; idx >= 0; --idx) {
        _freeSpotsInVBO.push(idx);
    }
    LINFO("StackSize: " + std::to_string(_freeSpotsInVBO.size()) );
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
    LINFO(fmt::format("Number of stars per node: {}", accumulatedString));
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
    for (auto removedKey =_removedKeysInPrevCall.rbegin(); 
        removedKey != _removedKeysInPrevCall.rend(); ++removedKey) {

        // Uses a reverse loop to try to decrease the biggest chunk.
        if (*removedKey == _biggestChunkIndexInUse - 1) {
            _biggestChunkIndexInUse = *removedKey;
            //LINFO("Decreased size to: " + std::to_string(_biggestChunkIndexInUse));
        }
        _freeSpotsInVBO.push(*removedKey);
    }
    // Clear cache of removed keys before next render call.
    _removedKeysInPrevCall.clear();

    for (size_t i = 0; i < 8; ++i) {
        auto tmpData = checkNodeIntersection(_root->Children[i], mvp, screenSize, deltaStars);
        // Observe that if there exists identical keys in renderData then those values in 
        // tmpData will be ignored! Thus we store the removed keys until next render call!
        renderData.insert(tmpData.begin(), tmpData.end());
        /*LINFO("Renderdata.size(): " + std::to_string(renderData.size()) + 
            " tmpData.size(): " + std::to_string(tmpData.size()) + 
            " deltaTraverse: " + std::to_string(deltaStars));*/
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
        size_t valuesPerStar = starValues.size();

        // Create children.
        for (size_t i = 0; i < 8; ++i) {
            _numLeafNodes++;
            node->Children[i] = std::make_unique<OctreeNode>();
            node->Children[i]->isLeaf = true;
            node->Children[i]->vboIndex = DEFAULT_INDEX;
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

        // Distribute stars from parent node into children. 
        for (size_t n = 0; n < node->numStars; ++n) {
            auto first = node->data.begin() + n * valuesPerStar;
            auto last = node->data.begin() + n * valuesPerStar + valuesPerStar;
            std::vector<float> tmpValues(first, last);

            size_t index = getChildIndex(tmpValues[0], tmpValues[1], tmpValues[2],
                node->originX, node->originY, node->originZ);
            insertInNode(node->Children[index], tmpValues, depth);
        }

        // Clean up parent.
        node->isLeaf = false;
        node->data.clear();
        _numLeafNodes--;
        _numInnerNodes++;

        // Store a cache of values for faster LOD traversals during render.
        // TODO (adaal): This is where a study of different methods can take place
        // e.g. use average/median of all points, origin of node or several points!? 
        // Right now we only use the last star.
        node->data.insert(node->data.end(), starValues.begin(), starValues.end());
        node->numStars = 1;
    }

    // Node is an inner node, keep recursion going.
    // This will also take care of the new star when a subdivision has taken place.
    size_t index = getChildIndex(starValues[0], starValues[1], starValues[2],
        node->originX, node->originY, node->originZ);

    return insertInNode(node->Children[index], starValues, ++depth);
}

// Private help function for printStarsPerNode(). Recursively adds all nodes to the string.
std::string OctreeManager::printStarsPerNode(std::shared_ptr<OctreeNode> node,
    std::string prefix) const {

    if (node->isLeaf) {
        return prefix + "} : [" + std::to_string(node->vboIndex) + "] : "
            + std::to_string(node->numStars) + "\n";
    }
    else {
        auto str = std::string();
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
    }

    // Check if node is visible from camera. If not then return early.
    if (!(_culler->isVisible(corners, mvp))) {
        // Check if this node or any of its children existed in cache previously. 
        // If so, then remove them from cache and add those indices to stack.
        fetchedData = removeNodeFromCache(node, deltaStars);
        return fetchedData;
    }

    glm::vec2 nodeSize = _culler->getNodeSizeInPixels(screenSize);
    int totalPixels = static_cast<int>(nodeSize.x * nodeSize.y);

    // If node contains more stars than enclosed pixels then return that amount. 
    /*if (node->numStars > totalPixels) {
        // Store node in cache. 

        auto first = node->data.begin();
        auto last = node->data.begin() + totalPixels;
        fetchedData.insert(fetchedData.end(), first, last);
        return fetchedData;
    }*/

    // Return node data if node is a leaf.
    // Or return cached data in inner node if its size is smaller than set pixels.
    if (node->isLeaf || length(nodeSize) < MIN_SIZE_IN_PIXELS) {

        // Check if node already is in cache, then skip it, otherwise store it.
        if (node->vboIndex == DEFAULT_INDEX) {
            //LINFO("Adds index: " + std::to_string(node->vboIndex));
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
            /*LINFO("vboIndex: " + std::to_string(node->vboIndex) + " Next top: " +
            std::to_string(_freeSpotsInVBO.top()) + " Cache size: " +
            std::to_string(_renderingCache.size()));*/

            // If we choose an inner node, then we need to remove indices 
            // from potential children in cache!
            if (!(node->isLeaf)) {
                for (int i = 0; i < 8; ++i) {
                    auto tmpData = removeNodeFromCache(node->Children[i], deltaStars);
                    fetchedData.insert(tmpData.begin(), tmpData.end());
                }
            }
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

    // Check if this node was rendered == had a specified index.
    if (node->vboIndex != DEFAULT_INDEX) {
        //LINFO("Removes index: " + std::to_string(node->vboIndex));

        // Reclaim that index. However we need to wait until next render call to use it again!
        _removedKeysInPrevCall.insert(node->vboIndex);

        // Insert dummy node at offset index that should be removed from render.
        keysToRemove[node->vboIndex] = std::vector<float>();

        // Reset index and adjust stars removed this frame. 
        node->vboIndex = DEFAULT_INDEX;
        deltaStars -= static_cast<int>(node->data.size());
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

// Insert specified node into cache.
/*void OctreeManager::insertNodeInCache(std::shared_ptr<OctreeNode> node, int& deltaStars) {
    
    
}*/

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
