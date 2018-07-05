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

namespace openspace::globebrowsing {

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
            std::unique_lock<std::mutex> lock(_pool._queueMutex);

            // look for a work item
            while (!_pool._stop && _pool._queuedTasks.isEmpty()) {
                // if there are none wait for notification
                _pool._condition.wait(lock);
            }

            if (_pool._stop) { // exit if the pool is stopped
                return;
            }

            // get the task from the queue
            task = _pool._queuedTasks.popMRU().second;

        }// release lock

        // execute the task
        task();
    }
}

template<typename KeyType>
LRUThreadPool<KeyType>::LRUThreadPool(size_t numThreads, size_t queueSize)
    : _queuedTasks(queueSize)
{
    for (size_t i = 0; i < numThreads; ++i) {
        _workers.push_back(std::thread(LRUThreadPoolWorker<KeyType>(*this)));
    }
}

template<typename KeyType>
LRUThreadPool<KeyType>::LRUThreadPool(const LRUThreadPool& toCopy)
    : LRUThreadPool(toCopy._workers.size(), toCopy._queuedTasks.maximumCacheSize())
{}

// the destructor joins all threads
template<typename KeyType>
LRUThreadPool<KeyType>::~LRUThreadPool() {
    {
        std::unique_lock<std::mutex> lock(_queueMutex);
        _stop = true;
    }
    _condition.notify_all();

    // join them
    for (size_t i = 0; i < _workers.size(); ++i) {
        _workers[i].join();
    }
}

// add new work item to the pool
template<typename KeyType>
void LRUThreadPool<KeyType>::enqueue(std::function<void()> f, KeyType key) {
    {
        std::unique_lock<std::mutex> lock(_queueMutex);

        // add the task
        //_queuedTasks.put(key, f);
        const std::vector<std::pair<KeyType, std::function<void()>>>& unfinishedTasks =
            _queuedTasks.putAndFetchPopped(key, f);
        for (const std::pair<KeyType, std::function<void()>>& unfinishedTask :
             unfinishedTasks)
        {
            _unqueuedTasks.push_back(unfinishedTask.first);
        }
    }

    // wake up one thread
    _condition.notify_one();
}

template<typename KeyType>
bool LRUThreadPool<KeyType>::touch(KeyType key) {
    std::unique_lock<std::mutex> lock(_queueMutex);
    return _queuedTasks.touch(key);
}

template<typename KeyType>
std::vector<KeyType> LRUThreadPool<KeyType>::getUnqueuedTasksKeys() {
    std::vector<KeyType> toReturn = _unqueuedTasks;
    {
        std::unique_lock<std::mutex> lock(_queueMutex);
        _unqueuedTasks.clear();
    }
    return toReturn;
}

template<typename KeyType>
std::vector<KeyType> LRUThreadPool<KeyType>::getQueuedTasksKeys() {
    std::vector<KeyType> queuedTasks;
    {
        std::unique_lock<std::mutex> lock(_queueMutex);
        while (!_queuedTasks.isEmpty()) {
            queuedTasks.push_back(_queuedTasks.popMRU().first);
        }
    }
    return queuedTasks;
}

template<typename KeyType>
void LRUThreadPool<KeyType>::clearEnqueuedTasks() {
    std::unique_lock<std::mutex> lock(_queueMutex);
    _queuedTasks.clear();
}

} // namespace openspace::globebrowsing
