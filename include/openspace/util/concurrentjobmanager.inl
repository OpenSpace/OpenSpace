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

#include <openspace/util/job.h>
#include <ghoul/misc/assert.h>

namespace openspace {

template<typename P>
ConcurrentJobManager<P>::ConcurrentJobManager(ThreadPool pool)
    : threadPool(pool)
{}

template<typename P>
void ConcurrentJobManager<P>::enqueueJob(std::shared_ptr<Job<P>> job) {
    threadPool.enqueue([this, job]() {
        job->execute();
        std::lock_guard<std::mutex> lock(_finishedJobsMutex);
        _finishedJobs.push(job);
    });
}

template<typename P>
void ConcurrentJobManager<P>::clearEnqueuedJobs() {
    threadPool.clearTasks();
}

template<typename P>
std::shared_ptr<Job<P>> ConcurrentJobManager<P>::popFinishedJob() {
    ghoul_assert(!_finishedJobs.empty(), "There is no finished job to pop!");

    std::lock_guard<std::mutex> lock(_finishedJobsMutex);
    return _finishedJobs.pop();
}

template<typename P>
size_t ConcurrentJobManager<P>::numFinishedJobs() const {
    return _finishedJobs.size();
}

} // namespace openspace
