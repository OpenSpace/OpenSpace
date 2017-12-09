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

#include <openspace/util/concurrentjobmanager.h>

#include <ghoul/misc/threadpool.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/glm.hpp>

class ConcurrentJobManagerTest : public testing::Test {};

struct TestJob : public openspace::Job<int> {
    TestJob(int jobExecutingTime)
        : _jobExecutingTime(jobExecutingTime)
    {}

    virtual void execute() {
        std::cout << "Executing job ... " << std::endl;
        std::this_thread::sleep_for(std::chrono::milliseconds(_jobExecutingTime));
        
        prod = 1337;
        std::cout << "Finished job" << std::endl;
    }

    virtual std::shared_ptr<int> product() {
        return std::make_shared<int>(prod);
    }

private:
    int _jobExecutingTime;
    int prod;
};



TEST_F(ConcurrentJobManagerTest, Basic) {
    using namespace openspace;

    ConcurrentJobManager<int> jobManager(ThreadPool(1));

    auto testJob1 = std::shared_ptr<TestJob>(new TestJob(20));
    auto testJob2 = std::shared_ptr<TestJob>(new TestJob(20));
    
    jobManager.enqueueJob(testJob1);
    jobManager.enqueueJob(testJob2);

    std::this_thread::sleep_for(std::chrono::milliseconds(10));
    EXPECT_EQ(jobManager.numFinishedJobs(), 0) << "A 20ms job should not be done after 10ms";

    std::this_thread::sleep_for(std::chrono::milliseconds(20));
    
    EXPECT_EQ(jobManager.numFinishedJobs(), 1) << "A 20ms job should be done after 10+20 ms";

    std::this_thread::sleep_for(std::chrono::milliseconds(20));
    EXPECT_EQ(jobManager.numFinishedJobs(), 2) << "A 20ms job and a 20ms job should be done after 10+20+20 ms"; 


    auto finishedJob = jobManager.popFinishedJob();

    int product = *finishedJob->product();
    EXPECT_EQ(product, 1337) << "Expecting product to be 1337";
}

struct VerboseProduct {
    VerboseProduct(int v)
    : val(v){
        std::cout << "VerboseProduct constructor" << std::endl;
    }

    ~VerboseProduct() {
        std::cout << "VerboseProduct destructor" << std::endl;
    }

    int val;
};


struct VerboseJob : public openspace::Job<VerboseProduct>{
    VerboseJob(int jobExecutingTime)
        : _jobExecutingTime(jobExecutingTime) {
        std::cout << "VerboseTestJob constructor" << std::endl;
    }

    ~VerboseJob() {
        std::cout << "VerboseTestJob destructor" << std::endl;
    }

    virtual void execute() {
        std::cout << " ** Executing job ... " << std::endl;
        std::this_thread::sleep_for(std::chrono::milliseconds(_jobExecutingTime));
        _product = std::shared_ptr<VerboseProduct>(new VerboseProduct(1337));
        std::cout << " ** Finished job" << std::endl;
    }

    virtual std::shared_ptr<VerboseProduct> product() {
        return _product;
    }

    int _jobExecutingTime;
    std::shared_ptr<VerboseProduct> _product;

};

TEST_F(ConcurrentJobManagerTest, JobCreation) {
    using namespace openspace;
    
    std::this_thread::sleep_for(std::chrono::milliseconds(1000));

    ConcurrentJobManager<VerboseProduct> jobManager(ThreadPool(1));

    auto testJob1 = std::shared_ptr<VerboseJob>(new VerboseJob(20));

    jobManager.enqueueJob(testJob1);

    std::this_thread::sleep_for(std::chrono::milliseconds(10));
    EXPECT_EQ(jobManager.numFinishedJobs(), 0) << "A 20ms job should not be done after 10ms";

    std::this_thread::sleep_for(std::chrono::milliseconds(20));

    EXPECT_EQ(jobManager.numFinishedJobs(), 1) << "A 20ms job should be done after 10+20 ms";


    auto finishedJob = jobManager.popFinishedJob();
    {
        auto product = finishedJob->product();
    }
}
