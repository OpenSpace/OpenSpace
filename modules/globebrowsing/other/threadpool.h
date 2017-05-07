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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___THREAD_POOL___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___THREAD_POOL___H__

#include <modules/globebrowsing/cache/lrucache.h>

#include <condition_variable>
#include <functional>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>

// Implementatin based on http://progsch.net/wordpress/?p=81

namespace openspace {
namespace globebrowsing {    

class ThreadPool;

class Worker {
public: 
    Worker(ThreadPool& pool);
    void operator()();
private:
    ThreadPool& pool;
};

class ThreadPool {
public:
    ThreadPool(size_t numThreads);
    ~ThreadPool();

    void enqueue(std::function<void()> f);
    void clearTasks();

private:
    friend class Worker;

    std::vector<std::thread> workers;

    std::deque<std::function<void()>> tasks;

    std::mutex queue_mutex;
    std::condition_variable condition;

    bool stop;
};


template<typename KeyType>
class LRUThreadPool;

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
    ~LRUThreadPool();

    void enqueue(std::function<void()> f, KeyType key);
    bool touch(KeyType key);
    std::vector<KeyType> getUnqueuedTasksKeys();
    //void clearTasks();

private:
    friend class LRUThreadPoolWorker<KeyType>;

    std::vector<std::thread> _workers;
    cache::LRUCache<KeyType, std::function<void()>> _queuedTasks;
    std::vector<KeyType> _unqueuedTasks;
    std::mutex _queue_mutex;
    std::condition_variable _condition;

    bool _stop;
};



template<typename KeyType>
LRUThreadPoolWorker<KeyType>::LRUThreadPoolWorker(LRUThreadPool<KeyType>& pool)
    : _pool(pool)
{}

template<typename KeyType>
void LRUThreadPoolWorker<KeyType>::operator()() {
    std::function<void()> task;
    while (true) {
        // acquire lock
        {
            std::unique_lock<std::mutex> lock(_pool._queue_mutex);

            // look for a work item
            while (!_pool._stop && _pool._queuedTasks.isEmpty()) {
                // if there are none wait for notification
                _pool._condition.wait(lock);
            }

            if (_pool._stop) { // exit if the pool is stopped
                return;
            }

            // get the task from the queue
            task = _pool._queuedTasks.popMRU();
            
        }// release lock

        // execute the task
        task();
    }
}

template<typename KeyType>
LRUThreadPool<KeyType>::LRUThreadPool(size_t numThreads, size_t queueSize)
    : _stop(false)
    , _queuedTasks(queueSize)
{
    for (size_t i = 0; i < numThreads; ++i) {
        _workers.push_back(std::thread(LRUThreadPoolWorker<KeyType>(*this)));
    }
}

// the destructor joins all threads
template<typename KeyType>
LRUThreadPool<KeyType>::~LRUThreadPool() {
    // Stop all threads
    _stop = true;
    _condition.notify_all();

    // join them
    for (size_t i = 0; i < _workers.size(); ++i) {
        _workers[i].join();
    }
}

// add new work item to the pool
template<typename KeyType>
void LRUThreadPool<KeyType>::enqueue(std::function<void()> f, KeyType key) {
    { // acquire lock
        std::unique_lock<std::mutex> lock(_queue_mutex);

        // add the task
        //_queuedTasks.put(key, f);
        std::vector<std::pair<KeyType, std::function<void()>>> unfinishedTasks =
            _queuedTasks.putAndFetchPopped(key, f);
        for (auto unfinishedTask : unfinishedTasks) {
            _unqueuedTasks.push_back(unfinishedTask.first);
        }
    } // release lock

    // wake up one thread
    _condition.notify_one();
}

template<typename KeyType>
bool LRUThreadPool<KeyType>::touch(KeyType key) {
    std::unique_lock<std::mutex> lock(_queue_mutex);
    return _queuedTasks.touch(key);
}


template<typename KeyType>
std::vector<KeyType> LRUThreadPool<KeyType>::getUnqueuedTasksKeys() {
    std::vector<KeyType> toReturn = _unqueuedTasks;
    {
        std::unique_lock<std::mutex> lock(_queue_mutex);
        _unqueuedTasks.clear();
    }
    return toReturn;
}

/*
template<typename KeyType>
void LRUThreadPool<KeyType>::clearTasks() {
    { // acquire lock
        std::unique_lock<std::mutex> lock(_queue_mutex);
        _queuedTasks.clear();
    } // release lock
}
*/

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___THREAD_POOL___H__
