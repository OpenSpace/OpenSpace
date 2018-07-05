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

#ifndef __OPENSPACE_CORE___THREAD_POOL___H__
#define __OPENSPACE_CORE___THREAD_POOL___H__

#include <condition_variable>
#include <functional>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>

// Implementation based on http://progsch.net/wordpress/?p=81

namespace openspace {

class ThreadPool;

class Worker {
public:
    Worker(ThreadPool& p);
    void operator()();

private:
    ThreadPool& pool;
};

class ThreadPool {
public:
    ThreadPool(size_t numThreads);
    ThreadPool(const ThreadPool& toCopy);
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

} // namespace openspace

#endif // __OPENSPACE_CORE___THREAD_POOL___H__
