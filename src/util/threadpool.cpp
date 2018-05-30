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

#include <openspace/util/threadpool.h>

namespace openspace {

Worker::Worker(ThreadPool& p) : pool(p) {}

void Worker::operator()() {
    std::function<void()> task;
    while (true) {
        // acquire lock
        {
            std::unique_lock<std::mutex> lock(pool.queue_mutex);

            // look for a work item
            while (!pool.stop && pool.tasks.empty()) {
                // if there are none wait for notification
                pool.condition.wait(lock);
            }

            if (pool.stop) { // exit if the pool is stopped
                return;
            }

            // get the task from the queue
            task = pool.tasks.front();
            pool.tasks.pop_front();

        } // release lock

        // execute the task
        task();
    }
}

ThreadPool::ThreadPool(size_t numThreads) : stop(false) {
    for (size_t i = 0; i < numThreads; ++i) {
        workers.emplace_back(std::thread(Worker(*this)));
    }
}

ThreadPool::ThreadPool(const ThreadPool& toCopy) : ThreadPool(toCopy.workers.size()) {}

// the destructor joins all threads
ThreadPool::~ThreadPool() {
    // stop all threads
    {
        std::unique_lock<std::mutex> lock(queue_mutex);
        stop = true;
    }
    condition.notify_all();

    // join them
    for (std::thread& w : workers) {
        w.join();
    }
}

// add new work item to the pool
void ThreadPool::enqueue(std::function<void()> f) {
    { // acquire lock
        std::unique_lock<std::mutex> lock(queue_mutex);

        // add the task
        tasks.push_back(std::move(f));
    } // release lock

    // wake up one thread
    condition.notify_one();
}

void ThreadPool::clearTasks() {
    { // acquire lock
        std::unique_lock<std::mutex> lock(queue_mutex);
        tasks.clear();
    } // release lock
}

} // namespace openspace
