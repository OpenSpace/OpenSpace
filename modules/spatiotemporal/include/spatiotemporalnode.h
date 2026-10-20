/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_SPATIOTEMPORAL___SPATIOTEMPORALNODE___H__
#define __OPENSPACE_MODULE_SPATIOTEMPORAL___SPATIOTEMPORALNODE___H__

#include <openspace/glm.h>
#include <utility>
#include <vector>

namespace openspace {

class SpatioTemporalNode {
public:
    SpatioTemporalNode(SpatioTemporalNode* parent, const std::pair<int, int>& xRange,
        const std::pair<int, int>& yRange, const std::pair<int, int>& timeRange);

    ~SpatioTemporalNode() = default;

private:
    SpatioTemporalNode* _parent;
    std::vector<SpatioTemporalNode*> _children;

    std::pair<int, int> _xRange;
    std::pair<int, int> _yRange;
    std::pair<int, int> _timeRange;
};

   
} // namespace openspace

#endif // __OPENSPACE_MODULE_SPATIOTEMPORAL___SPATIOTEMPORALNODE___H__
