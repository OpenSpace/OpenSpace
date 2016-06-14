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

#ifndef __CHUNK_LEVEL_EVALUATOR_H__
#define __CHUNK_LEVEL_EVALUATOR_H__

#include <glm/glm.hpp>
#include <vector>
#include <memory>
#include <ostream>

#include <modules/globebrowsing/chunk/chunk.h>


namespace openspace {

    class ChunkLevelEvaluator {
    public:
        virtual int getDesiredLevel(const Chunk& chunk, const RenderData& data) const = 0;
        static const int UNKNOWN_DESIRED_LEVEL = -1;
    };



    class EvaluateChunkLevelByDistance : public ChunkLevelEvaluator {
    public:
        virtual int getDesiredLevel(const Chunk& chunk, const RenderData& data) const;
    };

    class EvaluateChunkLevelByProjectedArea : public ChunkLevelEvaluator {
    public:
        virtual int getDesiredLevel(const Chunk& chunk, const RenderData& data) const;
    };

    class EvaluateChunkLevelByAvailableTileData : public ChunkLevelEvaluator {
    public:
        virtual int getDesiredLevel(const Chunk& chunk, const RenderData& data) const;
    };

}



#endif // __CHUNK_LEVEL_EVALUATOR_H__
