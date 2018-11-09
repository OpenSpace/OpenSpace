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

#ifndef __OPENSPACE_CORE___CONCURRENT_QUEUE___H__
#define __OPENSPACE_CORE___CONCURRENT_QUEUE___H__

#include <condition_variable>
#include <mutex>
#include <queue>

namespace openspace {

/**
 * Templated thread-safe queue based on std::thread and std::queue
 */
template <typename T>
class ConcurrentQueue {
public:
    T pop();

    void pop(T& item);

    void push(const T& item);

    void push(T&& item);

    size_t size() const;

    bool empty() const;

private:
    std::queue<T> _queue;
    mutable std::mutex _mutex;
    mutable std::condition_variable _cond;
};

} // namespace openspace

#include "concurrentqueue.inl"

#endif // __OPENSPACE_CORE___CONCURRENT_QUEUE___H__
