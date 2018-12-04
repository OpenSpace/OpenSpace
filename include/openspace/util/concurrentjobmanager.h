/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___CONCURRENT_JOB_MANAGER___H__
#define __OPENSPACE_CORE___CONCURRENT_JOB_MANAGER___H__

#include <openspace/util/concurrentqueue.h>
#include <openspace/util/threadpool.h>

#include <mutex>

namespace openspace {

template <typename T> struct Job;

/*
 * Templated Concurrent Job Manager
 * This class is used execute specific jobs on one (1) parallell thread
 */
template<typename P>
class ConcurrentJobManager {
public:
    ConcurrentJobManager(ThreadPool pool);

    void enqueueJob(std::shared_ptr<Job<P>> job);

    void clearEnqueuedJobs();

    std::shared_ptr<Job<P>> popFinishedJob();

    size_t numFinishedJobs() const;

private:
    ConcurrentQueue<std::shared_ptr<Job<P>>> _finishedJobs;
    std::mutex _finishedJobsMutex;
    ThreadPool threadPool;
};

} // namespace openspace

#include "concurrentjobmanager.inl"

#endif // __OPENSPACE_CORE___CONCURRENT_JOB_MANAGER___H__
