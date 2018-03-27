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

    // Initialize the culler. The NDC.z of the comparing corners are always -1 or 1.
    _culler = std::make_unique<OctreeCuller>(
        globebrowsing::AABB3(glm::vec3(-1, -1, 0), glm::vec3(1, 1, 1e2)) 
    );
    _freeSpotsInVBO = std::stack<int>();
    _removedKeysInPrevCall = std::set<int>();

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

        // Distribute stars from parent node into children. 
        for (size_t n = 0; n < node->numStars; ++n) {
            auto first = node->data.begin() + n * _valuesPerStar;
            auto last = node->data.begin() + n * _valuesPerStar + _valuesPerStar;
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

        // Store a LOD cache of values in inner node for faster traversals during render.
        constructLodCache(node);
    }

    // Node is an inner node, keep recursion going.
    // This will also take care of the new star when a subdivision has taken place.
    size_t index = getChildIndex(starValues[0], starValues[1], starValues[2],
        node->originX, node->originY, node->originZ);

    // Determine if new star should be kept in our LOD cache.
    insertStarInLodCache(node, starValues);

    // Increase counter for inner node to keep track of total stars in all children as well.
    node->numStars++;
    return insertInNode(node->Children[index], starValues, ++depth);
}

// Private help function for insertInNode(). Constructs our 3 base Levels Of Details.
void OctreeManager::constructLodCache(std::shared_ptr<OctreeNode> node) {

    // Level 0: First value should be origin. Only store positions to begin with.
    std::vector<float> insertData(_valuesPerStar, 0.f);
    insertData[0] = node->originX;
    insertData[1] = node->originY;
    insertData[2] = node->originZ;
    node->data.insert(node->data.end(), insertData.begin(), insertData.end());

    // Level 1: Childrens origins.
    for (size_t i = 0; i < 8; ++i) {
        insertData[0] = node->Children[i]->originX;
        insertData[1] = node->Children[i]->originY;
        insertData[2] = node->Children[i]->originZ;
        node->data.insert(node->data.end(), insertData.begin(), insertData.end());
    }

    // Level 2: Take the first 50 values from each child.
    for (size_t i = 0; i < 8; ++i) {
        auto childStart = node->Children[i]->data.begin();
        // Make sure we have 50 values!
        if (node->Children[i]->numStars > 50) {
            node->data.insert(node->data.end(), childStart, childStart + _valuesPerStar * 50);
        }
        else {
            auto childEnd = node->Children[i]->data.end();
            node->data.insert(node->data.end(), childStart, childEnd);
        }
    }
    // Level 3 & 4 will be constructed later in insertStarInLod().
}

// Private help function for insertInNode(). Determines if star should be stored in LOD.
void OctreeManager::insertStarInLodCache(std::shared_ptr<OctreeNode> node,
    std::vector<float> starValues) {
    // Level 0-2 already constructed when node was split. 
    
    // Level 3: 
    _lod3Stars = 0;

    // Level 4:
    _lod4Stars = 0;
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
        str += " - [Parent] \n";
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

    // Take care of inner nodes and LOD!
    if (!(node->isLeaf)) {
        glm::vec2 nodeSize = _culler->getNodeSizeInPixels(screenSize);
        if (node->halfDimension < (5 / pow(2, 6)) && node->numStars < 1100) {
            //LINFO("NodeSize: " + std::to_string(nodeSize) + " ScreenSize: " + std::to_string(screenSize));
        }
        float totalPixels = nodeSize.x * nodeSize.y;
        auto first = node->data.begin();
        auto lodData = std::vector<float>();

        // Check lowest level first - Level 0:
        if (length(nodeSize) < MIN_DIAGONAL_LOD_0) {
            lodData = std::vector<float>(first, first + LOD_0_STARS * _valuesPerStar);
            //LINFO("LOD0 - Length: " + std::to_string(length(nodeSize)) + " Node: " + std::to_string(debugPos));
        }
        // Level 1:
        else if (length(nodeSize) < MIN_DIAGONAL_LOD_1) {
            lodData = std::vector<float>(first, first + LOD_1_STARS * _valuesPerStar);
            //LINFO("LOD1 - Length: " + std::to_string(length(nodeSize)) + " Node: " + std::to_string(debugPos));
        }
        // Level 2:
        else if (totalPixels < MIN_TOTAL_PIXELS_LOD_2) {
            // TODO: What happens if there isn't enough stars in LOD cache? 
            lodData = std::vector<float>(first, first + LOD_2_STARS * _valuesPerStar);
            //LINFO("LOD2 - TotalPix: " + std::to_string(totalPixels) + " Node: " + std::to_string(debugPos));
        }
        // Level 3:
        /*else if (false) {
            lodData = std::vector<float>(first, first + _lod3Stars * _valuesPerStar);
        }
        // Level 4:
        else if (false) {
            lodData = std::vector<float>(first, first + _lod4Stars * _valuesPerStar);
        }*/

        // Return LOD data if we've triggered any check. 
        if (!lodData.empty()) {
            // Get correct insert index from stack if node didn't exist already. 
            // Otherwise we will overwrite the old data. Key merging is not a problem here. 
            if (node->vboIndex == DEFAULT_INDEX) {
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
        if (node->vboIndex == DEFAULT_INDEX) {
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

            // TODO: This doesn't work well because the nodes isn't refreshed when zooming out
            // since the node is already in cache... Better to always send full leaf when we've
            // actually reached one!
            /*// But don't return all stars if the node is very small!
            if (node->numStars > totalPixels) {
                // TODO: To always take the first stars will probably not yield a good result.
                std::vector<float> subData(
                    node->data.begin(), 
                    node->data.begin() + totalPixels * _valuesPerStar
                );
                fetchedData[node->vboIndex] = subData;
                deltaStars += static_cast<int>(subData.size());
            }
            else {
                // Insert full data. 
                fetchedData[node->vboIndex] = node->data;
                deltaStars += static_cast<int>(node->data.size());
            }*/
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
