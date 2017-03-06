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
    
template<typename KeyType, typename ValueType>
MemoryAwareLRUCache<KeyType, ValueType>::MemoryAwareLRUCache(size_t maximumSize)
    : _maximumCacheSize(maximumSize)
    , _cacheSize(0)
{}

template<typename KeyType, typename ValueType>
void MemoryAwareLRUCache<KeyType, ValueType>::clear() {
    _itemList.erase(_itemList.begin(), _itemList.end());
    _itemMap.erase(_itemMap.begin(), _itemMap.end());
    _cacheSize = 0;
}

template<typename KeyType, typename ValueType>
void MemoryAwareLRUCache<KeyType, ValueType>::put(const KeyType& key, const ValueType& value) {
    auto it = _itemMap.find(key);
    if (it != _itemMap.end()) {
        _itemList.erase(it->second);
        _itemMap.erase(it);
        _cacheSize -= it->second->second.memoryImpact();
    }
    _itemList.push_front(std::make_pair(key, value));
    _itemMap.insert(std::make_pair(key, _itemList.begin()));
    _cacheSize += _itemList.begin()->second.memoryImpact();
    clean();
}

template<typename KeyType, typename ValueType>
bool MemoryAwareLRUCache<KeyType, ValueType>::exist(const KeyType& key) const {
    return _itemMap.count(key) > 0;
}

template<typename KeyType, typename ValueType>
ValueType MemoryAwareLRUCache<KeyType, ValueType>::get(const KeyType& key) {
    //ghoul_assert(exist(key), "Key " << key << " must exist");
    auto it = _itemMap.find(key);
    // Move list iterator pointing to value
    _itemList.splice(_itemList.begin(), _itemList, it->second);
    return it->second->second;
}

template<typename KeyType, typename ValueType>
size_t MemoryAwareLRUCache<KeyType, ValueType>::size() const {
    return _cacheSize;
}

template<typename KeyType, typename ValueType>
size_t MemoryAwareLRUCache<KeyType, ValueType>::maximumSize() const {
    return _maximumCacheSize;
}

template<typename KeyType, typename ValueType>
void MemoryAwareLRUCache<KeyType, ValueType>::clean(size_t extraMemorySize) {
    ghoul_assert(extraMemorySize < _maximumCacheSize, "Too big extra memory size.");
    while (_cacheSize + extraMemorySize > _maximumCacheSize) {
        auto last_it = _itemList.end(); last_it--;
        _itemMap.erase(last_it->first);
        _cacheSize -= last_it->second.memoryImpact();
        _itemList.pop_back();
    }
}

} // namespace cache
} // namespace globebrowsing
} // namespace openspace
