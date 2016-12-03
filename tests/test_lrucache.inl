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

#include <modules/globebrowsing/other/lrucache.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/glm.hpp>

class LRUCacheTest : public testing::Test {};

TEST_F(LRUCacheTest, Get) {
    openspace::globebrowsing::LRUCache<int, std::string> lru(4);
    lru.put(1, "hej");
    lru.put(12, "san");
    ASSERT_STREQ(lru.get(1).c_str(), "hej") << "testing get";
}

TEST_F(LRUCacheTest, CleaningCache) {
    openspace::globebrowsing::LRUCache<int, double> lru(4);
    lru.put(1, 1.2);
    lru.put(12, 2.3);
    lru.put(123, 33.4);
    lru.put(1234, 4.5);
    lru.put(12345, 6.7);
    ASSERT_FALSE(lru.exist(1)) << "Element should have been cleaned out of cache";
    ASSERT_TRUE(lru.exist(12)) << "Element should remain in cache";
}

struct MyKey {
    int x, y;
};

bool operator==(const MyKey& a, const MyKey& b) {
    return a.x == b.x && a.y == b.y;
}

std::ostream& operator<<(std::ostream& o, const MyKey& key) {
    return o << key.x << ", " << key.y;
}

// custom specialization of std::hash can be injected in namespace std
namespace std {
    template<> struct hash<MyKey> {
        std::size_t operator()(MyKey const& s) const {
            return s.x ^ (s.y << 1);
        }
    };
}

TEST_F(LRUCacheTest, StructKey) {
    openspace::globebrowsing::LRUCache<MyKey, std::string> lru(4);

    // These two custom keys should be treated as equal
    MyKey key1 = { 2, 3 };
    MyKey key2 = { 2, 3 };

    std::string val1 = "value 1";
    std::string val2 = "value 2";


    lru.put(key1, val1);
    ASSERT_TRUE(lru.exist(key1));
    ASSERT_EQ(lru.get(key1), val1);

    // Putting key2 should replace key1
    lru.put(key2, val2);
    ASSERT_EQ(key1, key2) << "key 1 and key2 should be considered equal";
    ASSERT_TRUE(lru.exist(key2));
    ASSERT_EQ(lru.get(key1), val2);
    ASSERT_EQ(lru.get(key2), val2);
    
}