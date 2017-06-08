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
LRUCache<KeyType, ValueType, HasherType>::LRUCache(size_t size)
    : _maximumCacheSize(size)
{}

template<typename KeyType, typename ValueType, typename HasherType>
void LRUCache<KeyType, ValueType, HasherType>::clear() {
    _itemList.clear();
    _itemMap.clear();
}

template<typename KeyType, typename ValueType, typename HasherType>
void LRUCache<KeyType, ValueType, HasherType>::put(const KeyType& key,
                                                   const ValueType& value)
{
    putWithoutCleaning(key, value);
    clean();
}

template<typename KeyType, typename ValueType, typename HasherType>
std::vector<std::pair<KeyType, ValueType>>
LRUCache<KeyType, ValueType, HasherType>::putAndFetchPopped(const KeyType& key,
                                                            const ValueType& value)
{
    putWithoutCleaning(key, value);
    return cleanAndFetchPopped();
}

template<typename KeyType, typename ValueType, typename HasherType>
bool LRUCache<KeyType, ValueType, HasherType>::exist(const KeyType& key) const {
    return _itemMap.count(key) > 0;
}

template<typename KeyType, typename ValueType, typename HasherType>
bool LRUCache<KeyType, ValueType, HasherType>::touch(const KeyType& key) {
    auto it = _itemMap.find(key);
    if (it != _itemMap.end()) { // Found in cache
        ValueType value = it->second->second;
        // Remove from current position
        _itemList.erase(it->second);
        _itemMap.erase(it);
        // Bump to front
        _itemList.emplace_front(key, value);
        _itemMap.emplace(key, _itemList.begin());
        return true;
    } else {
        return false;
    }
}

template<typename KeyType, typename ValueType, typename HasherType>
bool LRUCache<KeyType, ValueType, HasherType>::isEmpty() const {
    return _itemMap.size() == 0;
}

template<typename KeyType, typename ValueType, typename HasherType>
ValueType LRUCache<KeyType, ValueType, HasherType>::get(const KeyType& key) {
    //ghoul_assert(exist(key), "Key " << key << " must exist");
    auto it = _itemMap.find(key);
    // Move list iterator pointing to value
    _itemList.splice(_itemList.begin(), _itemList, it->second);
    return it->second->second;
}

template<typename KeyType, typename ValueType, typename HasherType>
std::pair<KeyType, ValueType> LRUCache<KeyType, ValueType, HasherType>::popMRU() {
    ghoul_assert(_itemList.size() > 0,
        "Can not pop from LRU cache. Ensure cache is not empty.");
    auto first_it = _itemList.begin();
    _itemMap.erase(first_it->first);
    std::pair<KeyType, ValueType> toReturn = _itemList.front();
    _itemList.pop_front();
    return toReturn;
}

template<typename KeyType, typename ValueType, typename HasherType>
std::pair<KeyType, ValueType> LRUCache<KeyType, ValueType, HasherType>::popLRU() {
    ghoul_assert(_itemList.size() > 0,
        "Can not pop from LRU cache. Ensure cache is not empty.");
    auto lastIt = _itemList.end();
    lastIt--;
    _itemMap.erase(lastIt->first);
    std::pair<KeyType, ValueType> toReturn = _itemList.back();
    _itemList.pop_back();
    return toReturn;
}

template<typename KeyType, typename ValueType, typename HasherType>
size_t LRUCache<KeyType, ValueType, HasherType>::size() const {
    return _itemMap.size();
}

template<typename KeyType, typename ValueType, typename HasherType>
size_t LRUCache<KeyType, ValueType, HasherType>::maximumCacheSize() const {
    return _maximumCacheSize;
}

template<typename KeyType, typename ValueType, typename HasherType>
void LRUCache<KeyType, ValueType, HasherType>::putWithoutCleaning(const KeyType& key,
                                                                  const ValueType& value)
{
    auto it = _itemMap.find(key);
    if (it != _itemMap.end()) {
        _itemList.erase(it->second);
        _itemMap.erase(it);
    }
    _itemList.emplace_front(key, value);
    _itemMap.emplace(key, _itemList.begin());
}

template<typename KeyType, typename ValueType, typename HasherType>
void LRUCache<KeyType, ValueType, HasherType>::clean() {
    while (_itemMap.size() > _maximumCacheSize) {
        auto lastIt = _itemList.end();
        lastIt--;
        _itemMap.erase(lastIt->first);
        _itemList.pop_back();
    }
}

template<typename KeyType, typename ValueType, typename HasherType>
std::vector<std::pair<KeyType, ValueType>>
LRUCache<KeyType, ValueType, HasherType>::cleanAndFetchPopped()
{
    std::vector<std::pair<KeyType, ValueType>> toReturn;
    while (_itemMap.size() > _maximumCacheSize) {
        auto lastIt = _itemList.end();
        lastIt--;
        _itemMap.erase(lastIt->first);
        toReturn.push_back(_itemList.back());
        _itemList.pop_back();
    }
    return toReturn;
}

} // namespace cache
} // namespace globebrowsing
} // namespace openspace
