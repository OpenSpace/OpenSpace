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
#include <modules/globebrowsing/rendering/aabb.h>

#include <fstream>
#include <glm/glm.hpp>

using namespace openspace;

class AABBTest : public testing::Test {};



TEST_F(AABBTest, Contains2) {

    AABB2 a1;
    AABB2 a2;
    /*
        a1
        +-----+
        |+---+|
        ||a2 ||
        |+---+|
        +-----+
    */

    a1.expand(glm::vec2(0, 0));
    a1.expand(glm::vec2(1, 1));

    a2.expand(glm::vec2(0.1, 0.1));
    a2.expand(glm::vec2(0.9, 0.9));

    EXPECT_TRUE(a1.contains(a2)) << "a1 should contain a2";
    EXPECT_FALSE(a2.contains(a1)) << "a2 should not contain a1";
    EXPECT_TRUE(a1.intersects(a2)) << "a1 should intersect a2";
    EXPECT_TRUE(a2.intersects(a1)) << "a2 should intersect a1";

    EXPECT_EQ(AABBSpatialRelation::Containing, a1.relationTo(a2)) << "a1 contains a2";
    EXPECT_EQ(AABBSpatialRelation::Contained, a2.relationTo(a1)) << "a2 contained by a1";

}


TEST_F(AABBTest, Intersects2) {
    AABB2 a1;
    AABB2 a2;

    /*
    a1
    +-----+
    |     |
    |     |   a2
    |    ++----+
    +----++    |
         |     |
         |     |
         +-----+
    */

    a1.expand(glm::vec2(0, 0));
    a1.expand(glm::vec2(1, 1));

    a2.expand(glm::vec2(0.9, 0.9));
    a2.expand(glm::vec2(1.9, 1.9));

    EXPECT_FALSE(a1.contains(a2)) << "a1 should not contain a2";
    EXPECT_FALSE(a2.contains(a1)) << "a2 should not contain a1";
    EXPECT_TRUE(a1.intersects(a2)) << "a1 should intersect a2";
    EXPECT_TRUE(a2.intersects(a1)) << "a2 should intersect a1";

    EXPECT_EQ(AABBSpatialRelation::Intersecting, a1.relationTo(a2)) << "They should intersect";
    EXPECT_EQ(AABBSpatialRelation::Intersecting, a2.relationTo(a1)) << "They should intersect";

}



TEST_F(AABBTest, Contains3) {
    AABB3 a1;
    AABB3 a2;

    a1.expand(glm::vec3(0, 0, 0));
    a1.expand(glm::vec3(1, 1, 1));

    a2.expand(glm::vec3(0.1, 0.1, 0.1));
    a2.expand(glm::vec3(0.9, 0.9, 0.9));

    EXPECT_TRUE(a1.contains(a2)) << "a1 should contain a2";
    EXPECT_FALSE(a2.contains(a1)) << "a2 should not contain a1";
    EXPECT_TRUE(a1.intersects(a2)) << "a1 should intersect a2";
    EXPECT_TRUE(a2.intersects(a1)) << "a2 should intersect a1";

    EXPECT_EQ(AABBSpatialRelation::Containing, a1.relationTo(a2)) << "a1 contains a2";
    EXPECT_EQ(AABBSpatialRelation::Contained, a2.relationTo(a1)) << "a2 contained by a1";

}


TEST_F(AABBTest, Intersects3) {
    AABB3 a1;
    AABB3 a2;

    a1.expand(glm::vec3(0, 0, 0));
    a1.expand(glm::vec3(1, 1, 1));

    a2.expand(glm::vec3(0.9, 0.9, 0.9));
    a2.expand(glm::vec3(1.9, 1.9, 1.9));

    EXPECT_TRUE(a1.intersects(a2)) << "a1 should intersect a2";
    EXPECT_TRUE(a2.intersects(a1)) << "a2 should intersect a1";
    EXPECT_FALSE(a1.contains(a2)) << "a1 should not contain a2";
    EXPECT_FALSE(a2.contains(a1)) << "a2 should not contain a1";

    EXPECT_EQ(AABBSpatialRelation::Intersecting, a1.relationTo(a2)) << "They should intersect";
    EXPECT_EQ(AABBSpatialRelation::Intersecting, a2.relationTo(a1)) << "They should intersect";
}

