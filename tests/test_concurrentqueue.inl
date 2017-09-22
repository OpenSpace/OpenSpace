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

#include "gtest/gtest.h"

#include <openspace/util/concurrentqueue.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/glm.hpp>

class ConcurrentQueueTest : public testing::Test {};

TEST_F(ConcurrentQueueTest, Basic) {
    using namespace openspace;

    ConcurrentQueue<int> q1;
    q1.push(4);
    int val = q1.pop();
    std::cout << val << std::endl;
}

/*
TEST_F(ConcurrentQueueTest, SharedPtr) {
    ConcurrentQueue<std::shared_ptr<int>> q1;
    std::shared_ptr<int> i1 = std::shared_ptr<int>(new int(1337));

    q1.push(i1);
    auto val = q1.pop();
    std::cout << *val << std::endl;
}
*/
