/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <catch2/catch_test_macros.hpp>

#include <modules/globebrowsing/src/lrucache.h>
#include <glm/glm.hpp>

namespace {
    struct DefaultHasher {
        unsigned long long operator()(int var) const {
            return static_cast<unsigned long long>(var);
        }
    };

    struct MyKey {
        int x, y;
    };

    bool operator==(const MyKey& a, const MyKey& b) {
        return a.x == b.x && a.y == b.y;
    }

    std::ostream& operator<<(std::ostream& o, const MyKey& key) {
        return o << key.x << ", " << key.y;
    }

    // custom specialization
    struct DefaultHasherMyKey {
        unsigned long long operator()(const MyKey& s) const {
            return s.x ^ (s.y << 1);
        }
    };
} // namespace

TEST_CASE("LRUCache: Get", "[lrucache]") {
    openspace::globebrowsing::cache::LRUCache<int, std::string, DefaultHasher> lru(4);
    lru.put(1, "hej");
    lru.put(12, "san");
    CHECK(lru.get(1) == "hej");
}

TEST_CASE("LRUCache: CleaningCache", "[lrucache]") {
    openspace::globebrowsing::cache::LRUCache<int, double, DefaultHasher> lru(4);
    lru.put(1, 1.2);
    lru.put(12, 2.3);
    lru.put(123, 33.4);
    lru.put(1234, 4.5);
    lru.put(12345, 6.7);
    CHECK_FALSE(lru.exist(1));
    CHECK(lru.exist(12));
}

TEST_CASE("LRUCache: StructKey", "[lrucache]") {
    openspace::globebrowsing::cache::LRUCache<
        MyKey, std::string, DefaultHasherMyKey
    > lru(4);

    // These two custom keys should be treated as equal
    MyKey key1 = { 2, 3 };
    MyKey key2 = { 2, 3 };

    std::string val1 = "value 1";
    std::string val2 = "value 2";


    lru.put(key1, val1);
    CHECK(lru.exist(key1));
    CHECK(lru.get(key1) == val1);

    // Putting key2 should replace key1
    lru.put(key2, val2);
    CHECK(key1 == key2);
    CHECK(lru.exist(key2));
    CHECK(lru.get(key1) == val2);
    CHECK(lru.get(key2) == val2);
}
