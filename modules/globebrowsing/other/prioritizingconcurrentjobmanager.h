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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___PRIORITIZING_CONCURRENT_JOB_MANAGER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___PRIORITIZING_CONCURRENT_JOB_MANAGER___H__

#include <modules/globebrowsing/other/lruthreadpool.h>

//#include <openspace/util/concurrentjobmanager.h>
#include <openspace/util/concurrentqueue.h>

#include <mutex>

namespace openspace { template <typename T> struct Job; }

namespace openspace::globebrowsing {

/**
 * Concurrent job manager which prioritizes which jobs to work on depending on which
 * ones were enqueued latest. The class is templated both on the job type and the key
 * type which is used to identify jobs. In case a job need to be explicitly ended
 * It can be identified using its key.
 */
template<typename P, typename KeyType>
class PrioritizingConcurrentJobManager {
public:
    PrioritizingConcurrentJobManager(LRUThreadPool<KeyType> pool);

    /**
     * Enqueues a job which is identified using a given key
     */
    void enqueueJob(std::shared_ptr<Job<P>> job, KeyType key);

    /**
     * The keys returned by this function have been popped from the queue and corresponds
     * to jobs that will not be executed and therefore marked as unfinished. Calling this
     * function will also clear the list of unfinished jobs so if the jobs need to be
     * explicitly ended, the user need to make sure to do so after calling this function.
     */
    std::vector<KeyType> keysToUnfinishedJobs();

    std::vector<KeyType> keysToEnqueuedJobs();

    /**
     * Bumps the job identified with <code>key</code> to the beginning of the queue.
     * In case the job was not already enqueued the function simply returns false and
     * no state is changed.
     * \param key is the identifier of the job to bump.
     * \returns true if the job was found, else returns false.
     */
    bool touch(KeyType key);

    /**
     * Clear all enqueued jobs. Can not end jobs that workers are currently handling.
     * Therefore it is not safe to assume that there will be no finished jobs after
     * calling this function.
     */
    void clearEnqueuedJobs();

    /**
     * \returns one finished job.
     */
    std::shared_ptr<Job<P>> popFinishedJob();

    size_t numFinishedJobs() const;

private:
    ConcurrentQueue<std::shared_ptr<Job<P>>> _finishedJobs;
    std::mutex _finishedJobsMutex;
    /// An LRU thread pool is used since the jobs can be bumped and hence prioritized.
    LRUThreadPool<KeyType> _threadPool;
};

} // namespace openspace::globebrowsing

#include "prioritizingconcurrentjobmanager.inl"

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___PRIORITIZING_CONCURRENT_JOB_MANAGER___H__
