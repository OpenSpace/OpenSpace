/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <queue>

#include <ghoul/misc/assert.h>

#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/openspaceengine.h>

#include <modules/globebrowsing/chunk/chunknode.h>
#include <modules/globebrowsing/chunk/chunkedlodglobe.h>
#include <modules/globebrowsing/chunk/culling.h>


namespace {
    const std::string _loggerCat = "ChunkNode";
}

namespace openspace {

int ChunkNode::chunkNodeCount = 0;

ChunkNode::ChunkNode(const Chunk& chunk, ChunkNode* parent)
: _chunk(chunk)
, _parent(parent)
{
    _children[0] = nullptr;
    _children[1] = nullptr;
    _children[2] = nullptr;
    _children[3] = nullptr;
    chunkNodeCount++;
}

ChunkNode::~ChunkNode() {
    chunkNodeCount--;
}

bool ChunkNode::isRoot() const {
    return _parent == nullptr;
}

bool ChunkNode::isLeaf() const {
    return _children[0] == nullptr;
}


// Returns true or false wether this node can be merge or not
bool ChunkNode::updateChunkTree(const RenderData& data) {
    //Geodetic2 center = _chunk.surfacePatch.center();
    //LDEBUG("x: " << patch.x << " y: " << patch.y << " level: " << patch.level << "  lat: " << center.lat << " lon: " << center.lon);

    if (isLeaf()) {
        Chunk::Status status = _chunk.update(data);
        if (status == Chunk::Status::WANT_SPLIT) {
            split();
        }
        return status == Chunk::Status::WANT_MERGE;
    }
    else {
        char requestedMergeMask = 0;
        for (int i = 0; i < 4; ++i) {
            if (_children[i]->updateChunkTree(data)) {
                requestedMergeMask |= (1 << i);
            }
        }

        bool allChildrenWantsMerge = requestedMergeMask == 0xf;
        bool thisChunkWantsSplit = _chunk.update(data) == Chunk::Status::WANT_SPLIT;

        if (allChildrenWantsMerge && !thisChunkWantsSplit) {
            merge();
        }

        return false;
    }
}

void ChunkNode::depthFirst(const std::function<void(const ChunkNode&)>& f) const {
    f(*this);
    if (!isLeaf()) {
        for (int i = 0; i < 4; ++i) {
            _children[i]->depthFirst(f);
        }
    }
}


void ChunkNode::reverseBreadthFirst(const std::function<void(const ChunkNode&)>& f) const {
    std::stack<const ChunkNode*> S;
    std::queue<const ChunkNode*> Q;

    // Loop through nodes in breadths first order
    Q.push(this);
    while (Q.size() > 0) {
        const ChunkNode* node = Q.front(); 
        Q.pop();

        // Add node to future stack
        S.push(node);

        // Add children to queue, if any
        if (!node->isLeaf()) {
            for (int i = 0; i < 4; ++i) {
                Q.push(node->_children[i].get());
            }
        }
        
    }

    // Loop through all nodes in stack, this this will be reversed breadth first 
    while (S.size() > 0) {
        f(*S.top());
        S.pop();
    }
}

void ChunkNode::split(int depth) {
    if (depth > 0 && isLeaf()) {
        for (size_t i = 0; i < 4; i++) {
            Chunk chunk(_chunk.owner(), _chunk.index().child((Quad)i));
            _children[i] = std::unique_ptr<ChunkNode>(new ChunkNode(chunk, this));
        }
    }

    if (depth - 1 > 0) {
        for (int i = 0; i < 4; ++i) {
            _children[i]->split(depth - 1);
        }
    }
}

void ChunkNode::merge() {
    for (int i = 0; i < 4; ++i) {
        if (_children[i] != nullptr) {
            _children[i]->merge();
        }
        _children[i] = nullptr;
    }
    
    ghoul_assert(isLeaf(), "ChunkNode must be leaf after merge");
}

const ChunkNode& ChunkNode::getChild(Quad quad) const {
    return *_children[quad];
}

const Chunk& ChunkNode::getChunk() const {
    return _chunk;
}



} // namespace openspace
