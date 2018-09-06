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

namespace openspace::volume {

template <typename KeyType, typename ValueType, template<typename...> class ContainerType>
LruCache<KeyType, ValueType, ContainerType>::LruCache(size_t capacity) {
    _capacity = capacity;
}

template <typename KeyType, typename ValueType, template<typename...> class ContainerType>
bool LruCache<KeyType, ValueType, ContainerType>::has(const KeyType& key) {
    return (_cache.find(key) != _cache.end());
}

template <typename KeyType, typename ValueType, template<typename...> class ContainerType>
void LruCache<KeyType, ValueType, ContainerType>::set(const KeyType& key, ValueType value)
{
    auto prev = _cache.find(key);
    if (prev != _cache.end()) {
        prev->second.first = value;
        std::list<KeyType>::iterator trackerIter = prev->second.second;
        _tracker.splice(_tracker.end(), _tracker, trackerIter);
    }
    else {
        insert(key, value);
    }
}

template <typename KeyType, typename ValueType, template<typename...> class ContainerType>
ValueType& LruCache<KeyType, ValueType, ContainerType>::use(const KeyType& key) {
    auto iter = _cache.find(key);
    std::list<KeyType>::iterator trackerIter = iter->second.second;
    _tracker.splice(_tracker.end(), _tracker, trackerIter);
    return iter->second.first;
}

template <typename KeyType, typename ValueType, template<typename...> class ContainerType>
ValueType& LruCache<KeyType, ValueType, ContainerType>::get(const KeyType& key) {
    auto iter = _cache.find(key);
    return iter->second.first;
}

template <typename KeyType, typename ValueType, template<typename...> class ContainerType>
void LruCache<KeyType, ValueType, ContainerType>::evict() {
    _cache.erase(_cache.find(_tracker.front()));
    _tracker.pop_front();
}

template <typename KeyType, typename ValueType, template<typename...> class ContainerType>
size_t LruCache<KeyType, ValueType, ContainerType>::capacity() const {
    return _capacity;
}

template <typename KeyType, typename ValueType, template<typename...> class ContainerType>
void LruCache<KeyType, ValueType, ContainerType>::insert(const KeyType& key,
                                                         const ValueType& value)
{
    if (_cache.size() == _capacity) {
        evict();
    }
    auto iter = _tracker.insert(_tracker.end(), key);
    _cache[key] = std::make_pair(value, iter);
};

} // namespace openspace::volume
