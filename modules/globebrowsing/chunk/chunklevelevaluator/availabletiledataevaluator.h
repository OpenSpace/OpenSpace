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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___AVAILABLETILEDATAEVALUATOR___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___AVAILABLETILEDATAEVALUATOR___H__

#include <modules/globebrowsing/chunk/chunklevelevaluator/chunklevelevaluator.h>

namespace openspace::globebrowsing::chunklevelevaluator {

/**
 * If this chunk has available tile data for any LayerGroup on any of its active
 * Layers it will return an UNKNOWN_DESIRED_LEVEL. If no data is available it will
 * evaluate to a level that is <code>current level -1</code>.
*/
class AvailableTileData : public Evaluator {
public:
    virtual ~AvailableTileData() override = default;
    int desiredLevel(const Chunk& chunk, const RenderData& data) const override;
};

} // namespace openspace::globebrowsing::chunklevelevaluator

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___AVAILABLETILEDATAEVALUATOR___H__
