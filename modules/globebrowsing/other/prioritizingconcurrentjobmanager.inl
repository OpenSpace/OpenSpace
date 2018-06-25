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

#include <ghoul/misc/assert.h>

namespace openspace::globebrowsing {

template <typename P, typename KeyType>
PrioritizingConcurrentJobManager<P, KeyType>::PrioritizingConcurrentJobManager(
                                                              LRUThreadPool<KeyType> pool)
    : _threadPool(pool)
{}

template <typename P, typename KeyType>
void PrioritizingConcurrentJobManager<P, KeyType>::enqueueJob(std::shared_ptr<Job<P>> job,
                                                              KeyType key)
{
    _threadPool.enqueue([this, job]() {
        job->execute();
        std::lock_guard<std::mutex> lock(_finishedJobsMutex);
        _finishedJobs.push(job);
    }, key);
}

template <typename P, typename KeyType>
std::vector<KeyType>
PrioritizingConcurrentJobManager<P, KeyType>::keysToUnfinishedJobs() {
    return _threadPool.getUnqueuedTasksKeys();
}

template <typename P, typename KeyType>
std::vector<KeyType>
PrioritizingConcurrentJobManager<P, KeyType>::keysToEnqueuedJobs() {
    return _threadPool.getQueuedTasksKeys();
}

template <typename P, typename KeyType>
bool PrioritizingConcurrentJobManager<P, KeyType>::touch(KeyType key) {
    return _threadPool.touch(key);
}

template <typename P, typename KeyType>
void PrioritizingConcurrentJobManager<P, KeyType>::clearEnqueuedJobs() {
    _threadPool.clearEnqueuedTasks();
}

template <typename P, typename KeyType>
std::shared_ptr<Job<P>> PrioritizingConcurrentJobManager<P, KeyType>::popFinishedJob() {
    ghoul_assert(!_finishedJobs.empty(), "There is no finished job to pop!");

    std::lock_guard<std::mutex> lock(_finishedJobsMutex);
    std::shared_ptr<Job<P>> result = _finishedJobs.pop();
    return result;
}

template <typename P, typename KeyType>
size_t PrioritizingConcurrentJobManager<P, KeyType>::numFinishedJobs() const {
    return _finishedJobs.size();
}

} // namespace openspace::globebrowsing
