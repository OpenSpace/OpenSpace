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
{   }

OctreeManager::~OctreeManager() {   }


bool OctreeManager::initOctree() {
    
    LDEBUG("Initializing Octree");
    _root = std::make_unique<OctreeNode>();
    
    for (size_t i = 0; i < 8; ++i) {
        _numLeafNodes++;
        _root->Children[i] = std::make_unique<OctreeNode>();
        //_root->Children[i]->Parent = _root;
        _root->Children[i]->data = std::vector<float>();
        _root->Children[i]->isLeaf = true;
        _root->Children[i]->numStars = 0;
        _root->Children[i]->halfDimension = MAX_DIST / 2;
        _root->Children[i]->originX = (i%2 == 0) ? _root->Children[i]->halfDimension
            : -_root->Children[i]->halfDimension;
        _root->Children[i]->originY = (i%4 < 2) ? _root->Children[i]->halfDimension
            : -_root->Children[i]->halfDimension;
        _root->Children[i]->originZ = (i < 4) ? _root->Children[i]->halfDimension
            : -_root->Children[i]->halfDimension;
    }

    return true;
}

size_t OctreeManager::getChildIndex(float posX, float posY, float posZ, float origX, 
    float origY, float origZ) {

    size_t index = 0;

    if (posX < origX) index += 1;
    if (posY < origY) index += 2;
    if (posZ < origZ) index += 4;

    return index;
}

bool OctreeManager::insertInNode(std::shared_ptr<OctreeNode> node, 
    std::vector<float> starValues, int depth) {

    if (node->isLeaf && node->numStars < MAX_STARS_PER_NODE) {
        // Node is a leaf and it's not yet full -> insert star.
        node->numStars++;
        node->data.insert(node->data.end(), starValues.begin(), starValues.end());
        if (depth > _totalDepth) _totalDepth = depth;
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
            node->Children[i]->numStars = 0;
            node->Children[i]->data = std::vector<float>();
            node->Children[i]->Parent = node;
            node->Children[i]->halfDimension = node->halfDimension / 2;
            
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

        // Distribute stars from parent node. 
        for (size_t n = 0; n < node->numStars; ++n) {
            auto first = node->data.begin() + n * valuesPerStar;
            auto last = node->data.begin() + n * valuesPerStar + valuesPerStar;
            std::vector<float> tmpValues(first, last);
            //LINFO("tmpValues.size: " + std::to_string(tmpValues.size()));

            size_t index = getChildIndex(tmpValues[0], tmpValues[1], tmpValues[2], 
                node->originX, node->originY, node->originZ);
            insertInNode(node->Children[index], tmpValues, depth);
        }

        // Clean up parent.
        node->isLeaf = false;
        node->data.clear();
        node->numStars = 0;
        _numLeafNodes--;
        _numInnerNodes++;

    }

    // Node is an inner node, keep recursion going.
    // This will also take care of the new star when a subdivision has taken place.
    size_t index = getChildIndex(starValues[0], starValues[1], starValues[2], 
        node->originX, node->originY, node->originZ);

    return insertInNode(node->Children[index], starValues, ++depth);
}

void OctreeManager::insert(std::vector<float> starValues) {

    size_t index = getChildIndex(starValues[0], starValues[1], starValues[2]);

    insertInNode(_root->Children[index], starValues);
}

void OctreeManager::printStarsPerNode() const {

    auto accumulatedString = std::string();

    for (int i = 0; i < 8; ++i) {
        auto prefix = "{" + std::to_string(i);
        accumulatedString += printStarsPerNode(_root->Children[i], prefix);
    }
    LINFO("Number of stars per node: " + accumulatedString);
    LINFO("Number of leaf nodes: " + std::to_string(_numLeafNodes));
    LINFO("Number of inner nodes: " + std::to_string(_numInnerNodes));
    LINFO("Depth of tree: " + std::to_string(_totalDepth));
}

std::string OctreeManager::printStarsPerNode(std::shared_ptr<OctreeNode> node, 
    std::string prefix) const {
    
    if (node->isLeaf) {
        return prefix + "} : " + std::to_string(node->numStars) + "\n";
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

std::vector<float> OctreeManager::traverseData() {
    auto renderData = std::vector<float>();

    //LINFO("RenderData.size: " + std::to_string(renderData.size()));

    return renderData;
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
