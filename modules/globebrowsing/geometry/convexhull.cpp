/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/geometry/convexhull.h>

namespace openspace {
namespace globebrowsing {

glm::vec2 ConvexHull2::p0(0,0);

ConvexHull2 ConvexHull2::grahamScan_NOT_THREAD_SAFE(std::vector<glm::vec2>& points,
                                                    int yMinIndex)
{
    ConvexHull2 hull;

    if (yMinIndex == -1) {
        yMinIndex = 0;
        float ymin = points[0].y;
        for (int i = 1; i < points.size(); ++i) {
            float y = points[i].y;
            // Pick the bottom-most or chose the left most point in case of tie
            if ((y < ymin) || (ymin == y && points[i].x < points[yMinIndex].x)) {
                ymin = points[i].y; 
                yMinIndex = i;
            }
        }
    }

    // Place the bottom-most point at first position
    swap(points[0], points[yMinIndex]);

    // Sort n-1 points with respect to the first point.  A point p1 comes
    // before p2 in sorted ouput if p2 has larger polar angle (in
    // counterclockwise direction) than p1
    hull.p0 = points[0];
    
    
//    std::sort(
//        points.begin() + 1,
//        points.end(),
//        [](const glm::vec2& a, const glm::vec2& b) {
//            int o = orientation(p0, a, b);
//            if (o == 0) {
//                return (dist(p0, b) >= dist(p0, a)) ? -1 : 1;
//            }
//            
//            return (o == 2) ? -1 : 1;
//        }
//    );
//    
    // Replace with std::sort
    qsort(&(points.data()[1]), points.size()-1, sizeof(glm::vec2), &(hull.compare));
    
    // Create an empty stack and push first three points to it.
    std::stack<glm::vec2> S;
    S.push(points[0]);
    S.push(points[1]);
    S.push(points[2]);

    // Process remaining n-3 points
    for (int i = 3; i < points.size(); i++) {
        // Keep removing top while the angle formed by points next-to-top,
        // top, and points[i] makes a non-left turn
        while (orientation(oneBelowTop(S), S.top(), points[i]) == 1)
            S.pop();
        S.push(points[i]);
    }

    // Now stack has the output points, print contents of stack
    while (!S.empty()) {
        glm::vec2 p = S.top();
        hull._points.push_back(p);
        S.pop();
    }

    return hull;
}

bool ConvexHull2::intersects(const ConvexHull2& other) const {
    // uses Separating Axis Theorem
    // ref: http://stackoverflow.com/questions/753140/how-do-i-determine-if-two-convex-polygons-intersect
    return this->hasPerpendicularLineWhereProjectedPointsOverlap(other) ||
        other.hasPerpendicularLineWhereProjectedPointsOverlap(*this);
}


bool ConvexHull2::hasPerpendicularLineWhereProjectedPointsOverlap(const ConvexHull2& other) const {
    for (size_t i = 1; i < _points.size(); i++) {
        glm::vec2 dividingAxis = _points[i] - _points[i - 1];
        // project all points onto the vector perpendicular to the dividing axis
        glm::vec2 projAxis(glm::normalize(glm::vec2(-dividingAxis.y, dividingAxis.x)));
        const AABB1& myBounds = projectedRegion(projAxis);
        const AABB1& otherBounds = other.projectedRegion(projAxis);
        if (!myBounds.intersects(otherBounds)) {
            return false;
        }
    }

    // test line defined by last-to-first point
    glm::vec2 dividingAxis = _points[0] - _points.back();
    glm::vec2 projAxis(glm::normalize(glm::vec2(-dividingAxis.y, dividingAxis.x)));
    const AABB1& myBounds = projectedRegion(projAxis);
    const AABB1& otherBounds = other.projectedRegion(projAxis);
    if (!myBounds.intersects(otherBounds)) {
        return false;
    }

    return true;
}

AABB1 ConvexHull2::projectedRegion(glm::vec2 direction) const {
    AABB1 projectedRegion;
    for (const glm::vec2& p : _points) {
        projectedRegion.expand(glm::dot(p, direction));
    }
    //for (size_t i = 0; i < _points.size(); i++) {
    //    projectedRegion.expand(glm::dot(_points[i], direction));
    //}
    return projectedRegion;
}

glm::vec2 ConvexHull2::oneBelowTop(std::stack<glm::vec2>& S) {
    glm::vec2 p = S.top();
    S.pop();
    glm::vec2 res = S.top();
    S.push(p);
    return res;
}

void ConvexHull2::swap(glm::vec2& p1, glm::vec2& p2) {
    glm::vec2 temp = p1;
    p1 = p2;
    p2 = temp;
}

const std::vector<glm::vec2> ConvexHull2::points() const {
    return _points;
}

int ConvexHull2::orientation(const glm::vec2& p, const glm::vec2& q, const glm::vec2& r) {
    float val = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);
    return (val == 0) ? 0 : (val > 0) ? 1 : 2;
}

float ConvexHull2::dist(const glm::vec2& p1, const glm::vec2& p2) {
    return (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y);
}

    int ConvexHull2::compare(const void *vp1, const void *vp2) {
        glm::vec2* p1 = (glm::vec2 *)vp1;
        glm::vec2* p2 = (glm::vec2 *)vp2;
        
        // Find orientation
        int o = orientation(p0, *p1, *p2);
        if (o == 0) {
            return (dist(p0, *p2) >= dist(p0, *p1)) ? -1 : 1;
        }
        
        return (o == 2) ? -1 : 1;
    }
    

    
} // namespace globebrowsing
} // namespace openspace
