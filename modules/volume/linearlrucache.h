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

#ifndef __OPENSPACE_MODULE_VOLUME___LINEARLRUCACHE___H__
#define __OPENSPACE_MODULE_VOLUME___LINEARLRUCACHE___H__

#include <ghoul/glm.h>
#include <list>
#include <iterator>

namespace openspace {
    
template <typename ValueType>
class LinearLruCache {
public:
    LinearLruCache(size_t capacity, size_t nIndices)
        : _tracker()
        , _cache(nIndices, std::make_pair(nullptr, _tracker.end()))
        , _capacity(capacity) {};

    bool has(size_t key) {
        return _cache[key].first != nullptr;
    };
    void set(size_t key, ValueType value) {
        auto prev = _cache[key];
        if (prev.first != nullptr) {
            prev.first = value;
            std::list<size_t>::iterator trackerIter = prev.second;
            _tracker.splice(_tracker.end(),
                _tracker,
                trackerIter);
        }
        else {
            insert(key, value);
        }
    };
    ValueType& use(size_t key) {
        auto pair = _cache[key];
        std::list<size_t>::iterator trackerIter = pair.second;
        _tracker.splice(_tracker.end(),
            _tracker,
            trackerIter);
        return pair.first;
    };
    ValueType& get(size_t key) {
        return _cache[key].first;
    };
    void evict() {
        _cache[_tracker.front()] = make_pair(nullptr, _tracker.end());
        _tracker.pop_front();
    };
    size_t capacity() {
        return _capacity;
    };
private:
    void insert(size_t key, const ValueType& value) {
        if (_tracker.size() == _capacity) {
            evict();
        }
        auto iter = _tracker.insert(_tracker.end(), key);
        _cache[key] = std::make_pair(value, iter);
    };
    std::list<size_t> _tracker;
    std::vector<std::pair<ValueType, typename std::list<size_t>::iterator>> _cache;
    size_t _capacity;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_VOLUME___LINEARLRUCACHE___H__
