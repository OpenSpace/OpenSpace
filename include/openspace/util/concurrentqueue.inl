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

namespace openspace {

template <typename T>
T ConcurrentQueue<T>::pop() {
    std::unique_lock<std::mutex> mlock(_mutex);
    while (_queue.empty()) {
        _cond.wait(mlock);
    }
    auto item = _queue.front();
    _queue.pop();
    return item;
}

template <typename T>
void ConcurrentQueue<T>::pop(T& item) {
    std::unique_lock<std::mutex> mlock(_mutex);
    while (_queue.empty()) {
        _cond.wait(mlock);
    }
    item = _queue.front();
    _queue.pop();
}

template <typename T>
void ConcurrentQueue<T>::push(const T& item) {
    std::unique_lock<std::mutex> mlock(_mutex);
    _queue.push(item);
    mlock.unlock();
    _cond.notify_one();
}

template <typename T>
void ConcurrentQueue<T>::push(T&& item) {
    std::unique_lock<std::mutex> mlock(_mutex);
    _queue.push(std::move(item));
    mlock.unlock();
    _cond.notify_one();
}

template <typename T>
size_t ConcurrentQueue<T>::size() const {
    std::unique_lock<std::mutex> mlock(_mutex);
    size_t s = _queue.size();
    mlock.unlock();
    _cond.notify_one();
    return s;
}

template <typename T>
bool ConcurrentQueue<T>::empty() const {
    return size() == 0;
}


} // namespace openspace
