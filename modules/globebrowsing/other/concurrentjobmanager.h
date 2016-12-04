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

#ifndef __CONCURRENT_JOB_MANAGER_H__
#define __CONCURRENT_JOB_MANAGER_H__

#include <glm/glm.hpp>
#include <memory>
#include <ostream>
#include <thread>
#include <queue>

#include <modules/globebrowsing/other/concurrentqueue.h>
#include <modules/globebrowsing/other/threadpool.h>
//#include <ghoul/misc/threadpool.h>

#include <ghoul/misc/assert.h>

namespace openspace {
namespace globebrowsing {

    // Templated abstract base class representing a job to be done.
    // Client code derive from this class and implement the virtual execute() method
    template<typename P>
    struct Job {

        Job() { }
        virtual ~Job() { }

        virtual void execute() = 0;
        virtual std::shared_ptr<P> product() const = 0;
    
    };

    /* 
     * Templated Concurrent Job Manager
     * This class is used execute specific jobs on one (1) parallell thread
     */
    template<typename P>
    class ConcurrentJobManager{
    public:
        ConcurrentJobManager(std::shared_ptr<ThreadPool> pool) : threadPool(pool)
        {

        }

        ~ConcurrentJobManager() {

        }

        void enqueueJob(std::shared_ptr<Job<P>> job) {
            //threadPool->queue([this, job]() {
            //    job->execute();
            //    _finishedJobs.push(job);
            //});
            threadPool->enqueue([this, job]() {
                job->execute();
                _finishedJobs.push(job);
            });
        }

        void clearEnqueuedJobs() {
            //threadPool->clearRemainingTasks();
            threadPool->clearTasks();
        }

        std::shared_ptr<Job<P>> popFinishedJob() {
            ghoul_assert(_finishedJobs.size() > 0, "There is no finished job to pop!");
            return _finishedJobs.pop();
        }

        size_t numFinishedJobs() const{
            return _finishedJobs.size();
        }

        void reset() {
            //threadPool->clearRemainingTasks();
            threadPool->clearTasks();
        }

    private:

        ConcurrentQueue<std::shared_ptr<Job<P>>> _finishedJobs;
        std::shared_ptr<ThreadPool> threadPool;
    };

} // namespace globebrowsing
} // namespace openspace

#endif // __CONCURRENT_JOB_MANAGER_H__