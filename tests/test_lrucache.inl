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

#include <modules/globebrowsing/datastructures/lrucache.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/glm.hpp>

class LRUCacheTest : public testing::Test {};

using namespace openspace;

TEST_F(LRUCacheTest, Get) {
	LRUCache<int, std::string> lru(4);
	lru.put(1, "hej");
	lru.put(12, "san");
	ASSERT_STREQ(lru.get(1).c_str(), "hej") << "testing get";
}
TEST_F(LRUCacheTest, CleaningCache) {
	LRUCache<int, double> lru(4);
	lru.put(1, 1.2);
	lru.put(12, 2.3);
	lru.put(123, 33.4);
	lru.put(1234, 4.5);
	lru.put(12345, 6.7);
	ASSERT_FALSE(lru.exist(1)) << "Element should have been cleaned out of cache";
	ASSERT_TRUE(lru.exist(12)) << "Element should remain in cache";
}
