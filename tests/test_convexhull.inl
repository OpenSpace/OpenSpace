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

#include "gtest/gtest.h"

#include <openspace/scene/scenegraphnode.h>
#include <modules/globebrowsing/geometry/convexhull.h>

#include <fstream>
#include <glm/glm.hpp>

using namespace openspace;

class ConvexHull2Test : public testing::Test {};



TEST_F(ConvexHull2Test, basic) {
    
    // points
    // 2     x
    // 1     x
    // 0  x     x
    //   -1  0  1

    std::vector<Point2> points = {
        { -1.0, 0.0 },
        { 1.0, 0.0 },
        { 0.0, 2.0 },
        { 0.0, 1.0 }
    };

    
    // Convex hull
    // 2     x
    // 1   /   \  
    // 0  x ___ x
    //   -1  0  1

    ConvexHull2 hull = ConvexHull2::grahamScan_NOT_THREAD_SAFE(points);

    

    EXPECT_EQ(3, hull.points().size()) << "Should have 3 points";
    EXPECT_EQ(4, points.size()) << "Should have 4 points";

}

TEST_F(ConvexHull2Test, intersection) {
    std::vector<Point2> points1 = {
        { -1.0, 0.0 },
        { 1.0, 0.0 },
        { 0.0, 2.0 },
        { 0.0, 1.0 }
    };
    ConvexHull2 hull1 = ConvexHull2::grahamScan_NOT_THREAD_SAFE(points1);

    std::vector<Point2> points2 = {
        { 0.0, 0.0 },
        { 2.0, 0.0 },
        { 1.0, 2.0 },
        { 1.0, 1.0 }
    };    
    ConvexHull2 hull2 = ConvexHull2::grahamScan_NOT_THREAD_SAFE(points2);


    // Convex hull
    // 3
    // 2     x  x
    // 1   /  /\  \
    // 0  x _x__x__x
    //   -1  0  1  2  3

    EXPECT_TRUE(hull1.intersects(hull2)) << "They should intersect";
}


TEST_F(ConvexHull2Test, non_intersection) {
    std::vector<Point2> points1 = {
        { -2.0, 0.0 },
        { 2.0, 0.0 },
        { 0.0, 2.0 },
    };
    ConvexHull2 hull1 = ConvexHull2::grahamScan_NOT_THREAD_SAFE(points1);

    std::vector<Point2> points2 = {
        { 1.0, 2.0 },
        { 3.0, 0.0 },
        { 5.0, 2.0 }
    };
    ConvexHull2 hull2 = ConvexHull2::grahamScan_NOT_THREAD_SAFE(points2);


    // Convex hull
    // 3
    // 2        x  x-----------x
    // 1    _-'   '-_'-_   _-'
    // 0  x___________x  x
    //   -2 -1  0  1  2  3  4  5

    EXPECT_FALSE(hull1.intersects(hull2)) << "They should not intersect";
}
