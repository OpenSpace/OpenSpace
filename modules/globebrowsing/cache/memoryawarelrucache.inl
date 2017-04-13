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

#include <ghoul/misc/assert.h>

namespace openspace {
namespace globebrowsing {
namespace cache {
    
template<typename KeyType, typename ValueType, typename HasherType>
MemoryAwareLRUCache<KeyType, ValueType, HasherType>::MemoryAwareLRUCache(size_t maximumSize)
    : _maximumCacheSize(maximumSize)
    , _cacheSize(0)
{}

template<typename KeyType, typename ValueType, typename HasherType>
void MemoryAwareLRUCache<KeyType, ValueType, HasherType>::clear() {
    _itemList.clear();
    _itemMap.clear();
    _cacheSize = 0;
}

template<typename KeyType, typename ValueType, typename HasherType>
void MemoryAwareLRUCache<KeyType, ValueType, HasherType>::put(const KeyType& key, const ValueType& value) {
    auto it = _itemMap.find(key);
    if (it != _itemMap.end()) {
        _cacheSize -= it->second->second.memoryImpact();
        _itemList.erase(it->second);
        _itemMap.erase(it);
    }
    _itemList.emplace_front(key, value);
    _itemMap.emplace(key, _itemList.begin());
    _cacheSize += _itemList.begin()->second.memoryImpact();
    clean();
}

template<typename KeyType, typename ValueType, typename HasherType>
bool MemoryAwareLRUCache<KeyType, ValueType, HasherType>::exist(const KeyType& key) const {
    return _itemMap.count(key) > 0;
}

template<typename KeyType, typename ValueType, typename HasherType>
ValueType MemoryAwareLRUCache<KeyType, ValueType, HasherType>::get(const KeyType& key) {
    //ghoul_assert(exist(key), "Key " << key << " must exist");
    auto it = _itemMap.at(key);
    // Move list iterator pointing to value
    _itemList.splice(_itemList.begin(), _itemList, it);
    return it->second;
}

template<typename KeyType, typename ValueType, typename HasherType>
size_t MemoryAwareLRUCache<KeyType, ValueType, HasherType>::size() const {
    return _cacheSize;
}

template<typename KeyType, typename ValueType, typename HasherType>
size_t MemoryAwareLRUCache<KeyType, ValueType, HasherType>::maximumSize() const {
    return _maximumCacheSize;
}

template<typename KeyType, typename ValueType, typename HasherType>
void MemoryAwareLRUCache<KeyType, ValueType, HasherType>::setMaximumSize(size_t maximumSize) {
    _maximumCacheSize = maximumSize;
}

template<typename KeyType, typename ValueType, typename HasherType>
void MemoryAwareLRUCache<KeyType, ValueType, HasherType>::clean() {
    while (_cacheSize > _maximumCacheSize) {
        auto last_it = _itemList.end();
        last_it--;
        _itemMap.erase(last_it->first);
        _cacheSize -= last_it->second.memoryImpact();
        _itemList.pop_back();
    }
}

} // namespace cache
} // namespace globebrowsing
} // namespace openspace
