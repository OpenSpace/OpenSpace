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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___PROJECTEDAREAEVALUATOR___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___PROJECTEDAREAEVALUATOR___H__

#include <modules/globebrowsing/chunk/chunklevelevaluator/chunklevelevaluator.h>

namespace openspace::globebrowsing::chunklevelevaluator {

/**
 * Evaluate the chunk level using the area of the non-heightmapped Chunk projected
 * on a sphere with the center in the position of the camera. A Chunk near the
 * horizon will have a small projected area and hence a lower desired level. This
 * evaluation is more forgiving than EvaluateChunkLevelByDistance, meaning it results
 * in lower desired levels.
*/
class ProjectedArea : public Evaluator {
public:
    virtual int desiredLevel(const Chunk& chunk, const RenderData& data) const override;
};

} // namespace openspace::globebrowsing::chunklevelevaluator

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___PROJECTEDAREAEVALUATOR___H__
