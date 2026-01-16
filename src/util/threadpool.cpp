/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/util/threadpool.h>

#include <utility>

namespace openspace {

Worker::Worker(ThreadPool& p) : _pool(p) {}

void Worker::operator()() {
    std::function<void()> task;
    while (true) {
        {
            std::unique_lock lock(_pool._queueMutex);

            // look for a work item
            while (!_pool._shouldStop && _pool._tasks.empty()) {
                // if there are none wait for notification
                _pool._condition.wait(lock);
            }

            if (_pool._shouldStop) { // exit if the pool is stopped
                return;
            }

            // get the task from the queue
            task = _pool._tasks.front();
            _pool._tasks.pop_front();
        }

        // execute the task
        task();
    }
}

ThreadPool::ThreadPool(size_t numThreads) {
    for (size_t i = 0; i < numThreads; i++) {
        _workers.emplace_back(Worker(*this));
    }
}

ThreadPool::ThreadPool(const ThreadPool& toCopy) : ThreadPool(toCopy._workers.size()) {}

// the destructor joins all threads
ThreadPool::~ThreadPool() {
    // stop all threads
    {
        const std::unique_lock lock(_queueMutex);
        _shouldStop = true;
    }
    _condition.notify_all();

    // join them
    for (std::thread& w : _workers) {
        w.join();
    }
}

// add new work item to the pool
void ThreadPool::enqueue(std::function<void()> f) {
    {
        const std::unique_lock lock(_queueMutex);

        // add the task
        _tasks.push_back(std::move(f));
    }

    // wake up one thread
    _condition.notify_one();
}

void ThreadPool::clearTasks() {
    const std::unique_lock lock(_queueMutex);
    _tasks.clear();
}

bool ThreadPool::hasOutstandingTasks() const {
    return !_tasks.empty();
}

} // namespace openspace
