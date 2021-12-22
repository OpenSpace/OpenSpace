/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include "catch2/catch.hpp"

#include <openspace/util/concurrentjobmanager.h>
#include <ghoul/misc/threadpool.h>
#include <glm/glm.hpp>

namespace {
    struct TestJob : public openspace::Job<int> {
        explicit TestJob(int jobExecutingTime)
            : _jobExecutingTime(jobExecutingTime)
        {}

        virtual void execute() override {
            std::this_thread::sleep_for(std::chrono::milliseconds(_jobExecutingTime));
            prod = 1337;
        }

        virtual int product() override {
            return prod;
        }

    private:
        int _jobExecutingTime;
        int prod = 0;
    };


    struct VerboseProduct {
        explicit VerboseProduct(int v) : val(v) {}

        ~VerboseProduct() {}

        int val;
    };


    struct VerboseJob : public openspace::Job<VerboseProduct> {
        explicit VerboseJob(int jobExecutingTime)
            : _jobExecutingTime(jobExecutingTime)
            , _product(-1)
        {}

        virtual void execute() override {
            std::this_thread::sleep_for(std::chrono::milliseconds(_jobExecutingTime));
            _product = VerboseProduct(1337);
        }

        virtual VerboseProduct product() override {
            return _product;
        }

        int _jobExecutingTime;
        VerboseProduct _product;
    };

} // namespace

TEST_CASE("ConcurrentJobmanager: Basic", "[concurrentjobmanager]") {
    using namespace openspace;

    ConcurrentJobManager<int> jobManager(ThreadPool(1));

    auto testJob1 = std::shared_ptr<TestJob>(new TestJob(20));
    auto testJob2 = std::shared_ptr<TestJob>(new TestJob(20));
    
    jobManager.enqueueJob(testJob1);
    jobManager.enqueueJob(testJob2);

    std::this_thread::sleep_for(std::chrono::milliseconds(10));
    REQUIRE(jobManager.numFinishedJobs() == 0);

    std::this_thread::sleep_for(std::chrono::milliseconds(20));

    REQUIRE(jobManager.numFinishedJobs() == 1);

    std::this_thread::sleep_for(std::chrono::milliseconds(20));
    REQUIRE(jobManager.numFinishedJobs() == 2); 

    auto finishedJob = jobManager.popFinishedJob();

    int product = finishedJob->product();
    REQUIRE(product == 1337);
}

TEST_CASE("ConcurrentJobmanager: Job Creation", "[concurrentjobmanager]") {
    using namespace openspace;

    std::this_thread::sleep_for(std::chrono::milliseconds(1000));

    ConcurrentJobManager<VerboseProduct> jobManager(ThreadPool(1));
    auto testJob1 = std::shared_ptr<VerboseJob>(new VerboseJob(20));

    jobManager.enqueueJob(testJob1);

    std::this_thread::sleep_for(std::chrono::milliseconds(10));
    REQUIRE(jobManager.numFinishedJobs() == 0);

    std::this_thread::sleep_for(std::chrono::milliseconds(20));
    REQUIRE(jobManager.numFinishedJobs() == 1);

    auto finishedJob = jobManager.popFinishedJob();
    {
        auto product = finishedJob->product();
    }
}
