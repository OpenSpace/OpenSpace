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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING_CHUNKLEVELEVALUATOR_H__
#define __OPENSPACE_MODULE_GLOBEBROWSING_CHUNKLEVELEVALUATOR_H__

namespace openspace {

struct RenderData;

namespace globebrowsing {

class Chunk;

/**
 * Abstract class defining an interface for accessing a desired level of a Chunk.
 * The desired level can be used in the process of determining whether a Chunk should
 * want to split, merge or do nothing.
*/
class ChunkLevelEvaluator {
public:
    virtual int getDesiredLevel(const Chunk& chunk, const RenderData& data) const = 0;
    static const int UNKNOWN_DESIRED_LEVEL = -1;
};

/**
 * Evaluate the Chunk level depending on the distance from the Camera to the Chunk.
 * This evaluation method aims to keep the screen size (horizontal length and not
 * area) of all chunks constant.
*/
class EvaluateChunkLevelByDistance : public ChunkLevelEvaluator {
public:
    virtual int getDesiredLevel(const Chunk& chunk, const RenderData& data) const;
};

/**
 * Evaluate the chunk level using the area of the non-heightmapped Chunk projected
 * on a sphere with the center in the position of the camera. A Chunk near the
 * horizon will have a small projected area and hence a lower desired level. This
 * evaluation is more forgiving than EvaluateChunkLevelByDistance, meaning it results
 * in lower desired levels.
*/
class EvaluateChunkLevelByProjectedArea : public ChunkLevelEvaluator {
public:
    virtual int getDesiredLevel(const Chunk& chunk, const RenderData& data) const;
};

/**
 * If this chunk has available tile data for any LayerGroup on any of its active
 * Layers it will return an UNKNOWN_DESIRED_LEVEL. If no data is available it will
 * evaluate to a level that is <code>current level -1</code>.
*/
class EvaluateChunkLevelByAvailableTileData : public ChunkLevelEvaluator {
public:
    virtual int getDesiredLevel(const Chunk& chunk, const RenderData& data) const;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING_CHUNKLEVELEVALUATOR_H__
