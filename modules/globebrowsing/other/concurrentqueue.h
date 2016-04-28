/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __CONCURRENT_QUEUE_H__
#define __CONCURRENT_QUEUE_H__

#include <glm/glm.hpp>
#include <memory>
#include <ostream>
#include <thread>
#include <queue>
#include <mutex>
#include <condition_variable>

namespace openspace {



/**
 * Templated thread-safe queue based on std::thread and std::queue
 */
template <typename T>
class ConcurrentQueue {
public:

    T pop() {
        std::unique_lock<std::mutex> mlock(_mutex);
        while (_queue.empty()) {
            _cond.wait(mlock);
        }
        auto item = _queue.front();
        _queue.pop();
        return item;
    }

    void pop(T& item) {
        std::unique_lock<std::mutex> mlock(_mutex);
        while (_queue.empty()) {
            _cond.wait(mlock);
        }
        item = _queue.front();
        _queue.pop();
    }

    void push(const T& item) {
        std::unique_lock<std::mutex> mlock(_mutex);
        _queue.push(item);
        mlock.unlock();
        _cond.notify_one();
    }

    void push(T&& item) {
        std::unique_lock<std::mutex> mlock(_mutex);
        _queue.push(std::move(item));
        mlock.unlock();
        _cond.notify_one();
    }

    size_t size() {
        std::unique_lock<std::mutex> mlock(_mutex);
        size_t s = _queue.size();
        mlock.unlock();
        _cond.notify_one();
        return s;
    }

private:
    std::queue<T> _queue;
    std::mutex _mutex;
    std::condition_variable _cond;
};

}

#endif // __CONCURRENT_QUEUE_H__
