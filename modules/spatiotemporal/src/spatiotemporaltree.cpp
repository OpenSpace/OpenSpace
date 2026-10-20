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

#include <modules/spatiotemporal/include/spatiotemporaltree.h>

#include <openspace/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "SpatioTemporalTree";
} // namespace

namespace openspace {

SpatioTemporalTree::SpatioTemporalTree()
    : _root(nullptr)
{}

SpatioTemporalTree::SpatioTemporalTree(glm::uvec2 dimensions, int nTimes) {
    _root = buildTreeRecursive(
        nullptr,
        std::make_pair(0, static_cast<int>(dimensions.x)),
        std::make_pair(0, static_cast<int>(dimensions.y)),
        std::make_pair(0, nTimes - 1)
    );
}

SpatioTemporalTree::~SpatioTemporalTree() {
    
}

SpatioTemporalNode* SpatioTemporalTree::buildTreeRecursive(SpatioTemporalNode* parent,
                                                        const std::pair<int, int>& xRange,
                                                        const std::pair<int, int>& yRange,
                                                        const std::pair<int, int>& timeRange)
{
    int halfX = (xRange.second - xRange.first) / 2.0 ;
    int halfY = (yRange.second - yRange.first) / 2.0;
    int halfTime = (timeRange.second - timeRange.first) / 2.0;

    // Base case: if the ranges are small enough, create a leaf node
    if (halfX <= 1 || halfY <= 1 || halfTime <= 1)
    {
        return new SpatioTemporalNode(parent, xRange, yRange, timeRange);
    }

    SpatioTemporalNode* node = new SpatioTemporalNode(parent, xRange, yRange, timeRange);

    // Divide x and y into 4 quadrants, and time into 2 halves, creating 8 children
    buildTreeRecursive(
        node,
        std::make_pair(xRange.first, xRange.first + halfX),
        std::make_pair(yRange.first, yRange.first + halfY),
        std::make_pair(timeRange.first, timeRange.first + halfTime)
    );
    buildTreeRecursive(
        node,
        std::make_pair(halfX, xRange.second),
        std::make_pair(yRange.first, yRange.first + halfY),
        std::make_pair(timeRange.first, timeRange.first + halfTime)
    );
    buildTreeRecursive(
        node,
        std::make_pair(xRange.first, xRange.first + halfX),
        std::make_pair(halfY, yRange.second),
        std::make_pair(timeRange.first, timeRange.first + halfTime)
    );
    buildTreeRecursive(
        node,
        std::make_pair(halfX, xRange.second),
        std::make_pair(halfY, yRange.second),
        std::make_pair(timeRange.first, timeRange.first + halfTime)
    );

    buildTreeRecursive(
        node,
        std::make_pair(xRange.first, xRange.first + halfX),
        std::make_pair(yRange.first, yRange.first + halfY),
        std::make_pair(halfTime, timeRange.second)
    );
    buildTreeRecursive(
        node,
        std::make_pair(halfX, xRange.second),
        std::make_pair(yRange.first, yRange.first + halfY),
        std::make_pair(halfTime, timeRange.second)
    );
    buildTreeRecursive(
        node,
        std::make_pair(xRange.first, xRange.first + halfX),
        std::make_pair(halfY, yRange.second),
        std::make_pair(halfTime, timeRange.second)
    );
    buildTreeRecursive(
        node,
        std::make_pair(halfX, xRange.second),
        std::make_pair(halfY, yRange.second),
        std::make_pair(halfTime, timeRange.second)
    );

    return node;
}

} // namespace openspace
