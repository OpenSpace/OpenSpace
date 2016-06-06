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
#include <openspace/../modules/globebrowsing/rendering/aabb.h>

#include <fstream>
#include <glm/glm.hpp>

using namespace openspace;

class AABBTest : public testing::Test {};



TEST_F(AABBTest, ContainsAABB3) {
    AABB3 a1;
    AABB3 a2;

    a1.expand(glm::vec3(0, 0, 0));
    a1.expand(glm::vec3(1, 1, 1));

    a2.expand(glm::vec3(0.1, 0.1, 0.1));
    a2.expand(glm::vec3(0.9, 0.9, 0.9));

    EXPECT_TRUE(a1.contains(a2)) << "a1 should contain a2";
    EXPECT_FALSE(a2.contains(a1)) << "a2 should not contain a1";
}

TEST_F(AABBTest, ContainsAABB2) {
	
    AABB2 a1;
    AABB2 a2;
    
    a1.expand(glm::vec2(0, 0));
    a1.expand(glm::vec2(1, 1));

    a2.expand(glm::vec2(0.1, 0.1));
    a2.expand(glm::vec2(0.9, 0.9));

    EXPECT_TRUE(a1.contains(a2)) << "a1 should contain a2";
    EXPECT_FALSE(a2.contains(a1)) << "a2 should not contain a1";
}
