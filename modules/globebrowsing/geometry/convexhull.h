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

#ifndef __CONVEX_HULL_H__
#define __CONVEX_HULL_H__

#include <modules/globebrowsing/geometry/aabb.h>

#include <vector>
#include <stack>

#include <memory>
#include <glm/glm.hpp>

namespace openspace {
namespace globebrowsing {

    using namespace glm;
    
    // Implementation based on 
    // http://www.sanfoundry.com/cpp-program-implement-graham-scan-algorithm-find-convex-hull/

    typedef glm::vec2 Point2;

    class ConvexHull2 {
    public: 
        ConvexHull2();

        static ConvexHull2 grahamScan_NOT_THREAD_SAFE(std::vector<Point2>& points, int yMinIndex = -1);

        const std::vector<Point2> points() const;

        bool intersects(const ConvexHull2& o) const;

        AABB1 projectedRegion(glm::vec2 direction) const;
    
    private:
        bool hasPerpendicularLineWhereProjectedPointsOverlap(const ConvexHull2& other) const;

        static int compare(const void *vp1, const void *vp2);

        static Point2 oneBelowTop(std::stack<Point2>&);
        static void swap(Point2& p1, Point2& p2);

        // returns 0 = colinear, 1 = clockwise, 2 = counterclockwise
        static int orientation(const Point2& p, const Point2& q, const Point2& r);
        static float dist(const Point2& p1, const Point2& p2);

    private:
        static Point2 p0;
        std::vector<Point2> _points;
    };

} // namespace globebrowsing   
} // namespace openspace

#endif  // __CONVEX_HULL_H__