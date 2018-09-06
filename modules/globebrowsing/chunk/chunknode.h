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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___CHUNKNODE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___CHUNKNODE___H__

#include <modules/globebrowsing/chunk/chunk.h>

#include <array>
#include <functional>
#include <memory>

namespace openspace::globebrowsing {

class ChunkedLodGlobe;

class ChunkNode {
public:
    ChunkNode(Chunk chunk, ChunkNode* parent = nullptr);

    /**
     * Recursively split the ChunkNode.
     *
     * \param depth defines how deep the recursion should go. If depth == 1 (default),
     * the ChunkNode will only split once.
     */
    void split(int depth = 1);

    /**
     * Deletes all children of the ChunkNode recursively.
     */
    void merge();

    bool isRoot() const;
    bool isLeaf() const;

    void depthFirst(const std::function<void(const ChunkNode&)>& f) const;
    void breadthFirst(const std::function<void(const ChunkNode&)>& f) const;
    void reverseBreadthFirst(const std::function<void(const ChunkNode&)>& f) const;

    const ChunkNode& find(const Geodetic2& location) const;
    const ChunkNode& child(const Quad& quad) const;
    const Chunk& chunk() const;

    /**
     * Updates all children recursively. If this ChunkNode wants to split it will,
     * otherwise check if the children wants to merge. If all children wants to merge
     * and the Status of this Chunk is not Status::WANT_SPLIT it will merge.
     *
     * \returns true if the ChunkNode can merge and false if it can not merge.
    */
    bool updateChunkTree(const RenderData& data);

private:
    ChunkNode* _parent;
    std::array<std::unique_ptr<ChunkNode>, 4> _children;

    Chunk _chunk;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___CHUNKNODE___H__
