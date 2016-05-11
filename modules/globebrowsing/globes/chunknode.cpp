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

#include <modules/globebrowsing/globes/chunknode.h>

#include <ghoul/misc/assert.h>

#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/openspaceengine.h>

#include <modules/globebrowsing/globes/chunklodglobe.h>
#include <modules/globebrowsing/rendering/culling.h>


namespace {
    const std::string _loggerCat = "ChunkNode";
}

namespace openspace {

int ChunkNode::instanceCount = 0;
int ChunkNode::renderedPatches = 0;

ChunkNode::ChunkNode(const Chunk& chunk, ChunkNode* parent)
: _chunk(chunk)
, _parent(parent)
{
    _children[0] = nullptr;
    _children[1] = nullptr;
    _children[2] = nullptr;
    _children[3] = nullptr;
    instanceCount++;
}

ChunkNode::~ChunkNode() {
    instanceCount--;
}

bool ChunkNode::isRoot() const {
    return _parent == nullptr;
}

bool ChunkNode::isLeaf() const {
    return _children[0] == nullptr;
}


void ChunkNode::render(const RenderData& data) {
    ghoul_assert(isRoot(), "this method should only be invoked on root");
    //LDEBUG("-------------");
    internalUpdateChunkTree(data);
    internalRender(data);
}


// Returns true or false wether this node can be merge or not
bool ChunkNode::internalUpdateChunkTree(const RenderData& data) {
    //Geodetic2 center = _chunk.surfacePatch.center();
    //LDEBUG("x: " << patch.x << " y: " << patch.y << " level: " << patch.level << "  lat: " << center.lat << " lon: " << center.lon);

    if (isLeaf()) {
        Chunk::Status status = _chunk.update(data);
        if (status == Chunk::WANT_SPLIT) {
            split();
        }
        return status == Chunk::WANT_MERGE;
    }
    else {
        
        char requestedMergeMask = 0;
        for (int i = 0; i < 4; ++i) {
            if (_children[i]->internalUpdateChunkTree(data)) {
                requestedMergeMask |= (1 << i);
            }
        }

        // check if all children requested merge
        if (requestedMergeMask == 0xf) {
            merge();

            // re-run this method on this, now that this is a leaf node
            return internalUpdateChunkTree(data);
        }
        return false;
    }	
}


void ChunkNode::internalRender(const RenderData& data) {
    if (isLeaf()) {
        if (_chunk.isVisible()) {
            
            ChunkRenderer& patchRenderer = _chunk.owner()->getPatchRenderer();
            patchRenderer.renderChunk(_chunk, _chunk.owner()->ellipsoid(), data);
            //patchRenderer.renderPatch(_chunk.surfacePatch, data, _chunk.owner->ellipsoid(), _chunk.index);
            ChunkNode::renderedPatches++;
        }
    }
    else {
        for (int i = 0; i < 4; ++i) {
            _children[i]->internalRender(data);
        }
    }
}





void ChunkNode::split(int depth) {
    if (depth > 0 && isLeaf()) {
        auto childIndices = _chunk.index().childIndices();
        for (size_t i = 0; i < childIndices.size(); i++) {
            _children[i] = std::unique_ptr<ChunkNode>(
                new ChunkNode(Chunk(_chunk.owner(), childIndices[i]), this));
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
}

const ChunkNode& ChunkNode::getChild(Quad quad) const {
    return *_children[quad];
}



} // namespace openspace
