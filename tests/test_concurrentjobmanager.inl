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

#include <modules/globebrowsing/other/concurrentjobmanager.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/glm.hpp>




class ConcurrentJobManagerTest : public testing::Test {};


using namespace openspace;


struct TestJob : public Job<int> {
    virtual void execute() {
        std::cout << "executing l33t job" << std::endl;
        prod = 1337;
    }

    virtual int product() {
        return prod;
    }

    int prod;
};


TEST_F(ConcurrentJobManagerTest, Basic) {
    ConcurrentJobManager<int> jobManager;
    std::unique_ptr<TestJob> testJob = std::unique_ptr<TestJob>(new TestJob());

    jobManager.enqueueFutureJob(std::move(testJob));
    jobManager.startInSeparateThread();

    using namespace std::chrono_literals;
    std::this_thread::sleep_for(2s);
    
    auto finishedJob = jobManager.popFinishedJob();

    int product = finishedJob->product();
    std::cout << "product is " << product << std::endl;
}