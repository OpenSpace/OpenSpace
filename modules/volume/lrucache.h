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

#ifndef __OPENSPACE_MODULE_VOLUME___LRUCACHE___H__
#define __OPENSPACE_MODULE_VOLUME___LRUCACHE___H__

#include <ghoul/glm.h>
#include <list>
#include <iterator>

namespace openspace {
    
template <typename KeyType, typename ValueType, template<typename...> class ContainerType>
class LruCache {
    typedef KeyType K;
public:
    LruCache(size_t capacity) {
        _capacity = capacity;
    };
    bool has(const KeyType& key) {
        return (_cache.find(key) != _cache.end());
    };
    void set(const KeyType& key, ValueType value) {
        auto prev = _cache.find(key);
        if (prev != _cache.end()) {
            prev->second.first = value;
            std::list<KeyType>::iterator trackerIter = prev->second.second;
            _tracker.splice(_tracker.end(),
                _tracker,
                trackerIter);
        }
        else {
            insert(key, value);
        }
    };
    ValueType& use(const KeyType& key) {
        auto iter = _cache.find(key);
        std::list<KeyType>::iterator trackerIter = iter->second.second;
        _tracker.splice(_tracker.end(),
            _tracker,
            trackerIter);
        return iter->second.first;
    };
    ValueType& get(const KeyType& key) {
        auto iter = _cache.find(key);
        return iter->second.first;
    };
    void evict() {
        _cache.erase(_cache.find(_tracker.front()));
        _tracker.pop_front();
    };
    size_t capacity() {
        return _capacity;
    };
private:
    void insert(const KeyType& key, const ValueType& value) {
        if (_cache.size() == _capacity) {
            evict();
        }
        auto iter = _tracker.insert(_tracker.end(), key);
        _cache[key] = std::make_pair(value, iter);
    };
    ContainerType<KeyType, std::pair<ValueType, typename std::list<KeyType>::iterator>> _cache;
    std::list<KeyType> _tracker;
    size_t _capacity;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_VOLUME___LRUCACHE___H__
