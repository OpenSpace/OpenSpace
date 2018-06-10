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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LRU_THREAD_POOL___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LRU_THREAD_POOL___H__

#include <modules/globebrowsing/cache/lrucache.h>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <thread>
#include <vector>

// Implementation based on http://progsch.net/wordpress/?p=81

namespace openspace::globebrowsing {

template<typename KeyType> class LRUThreadPool;

template<typename KeyType>
class LRUThreadPoolWorker {
public:
    LRUThreadPoolWorker(LRUThreadPool<KeyType>& pool);
    void operator()();

private:
    LRUThreadPool<KeyType>& _pool;
};

/**
 * The <code>LRUThreadPool</code> will only enqueue a certain number of tasks. The most
 * recently enqueued task is the one that will be executed first. This class is templated
 * on a key type which used as an identifier to determine wheter or not a task with the
 * given key has been enqueued or not. This means that a task can be enqueued several
 * times. The user must ensure that an enqueued task with a given key should be
 * equal in outcome to a second enqueued task with the same key. This is because a second
 * enqueued task with the same key will simply be bumped and prioritised before other
 * enqueued tasks. The given task will be ignored.
 */
template<typename KeyType>
class LRUThreadPool {
public:
    LRUThreadPool(size_t numThreads, size_t queueSize);
    LRUThreadPool(const LRUThreadPool& toCopy);
    ~LRUThreadPool();

    void enqueue(std::function<void()> f, KeyType key);
    bool touch(KeyType key);
    std::vector<KeyType> getQueuedTasksKeys();
    std::vector<KeyType> getUnqueuedTasksKeys();
    void clearEnqueuedTasks();

private:
    struct DefaultHasher {
        unsigned long long operator()(const KeyType& key) const {
            return static_cast<unsigned long long>(key);
        }
    };
    friend class LRUThreadPoolWorker<KeyType>;

    std::vector<std::thread> _workers;
    cache::LRUCache<KeyType, std::function<void()>, DefaultHasher> _queuedTasks;
    std::vector<KeyType> _unqueuedTasks;
    std::mutex _queueMutex;
    std::condition_variable _condition;

    bool _stop = false;
};

} // namespace openspace::globebrowsing

#include "lruthreadpool.inl"

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LRU_THREAD_POOL___H__
