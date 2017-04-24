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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_LRU_CACHE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_LRU_CACHE___H__

#include <list>
#include <unordered_map>

namespace openspace {
namespace globebrowsing {
namespace cache {

/**
 * Least recently used cache that knows about its memory impact. This class is templated
 * and the second template argument <code>ValueType</code> needs to have a function
 * <code>void memoryImpact()</code> that returns the size of the object given in whatever
 * unit is used for size in the creation of the <code>MemoryAwareLRUCache</code>.
 * It can for example be given in kilobytes.
 * <code>KeyType</code> needs to be a size comparable type.
 */

template<typename KeyType, typename ValueType, typename HasherType>
class MemoryAwareLRUCache {
public:
    /**
     * \param maximumSize is the maximum size of the <code>MemoryAwareLRUCache</code>
     * Once the maximum size is reached, the cache will start removing objects that were
     * least recently used. The maximum size can for example be given in kilobytes. It
     * must be the same size unit as used by the cached object class
     * <code>ValueType</code>. 
     */
    MemoryAwareLRUCache(size_t maximumSize);

    void put(const KeyType& key, const ValueType& value);
    void clear();
    bool exist(const KeyType& key) const;
    ValueType get(const KeyType& key);
    size_t size() const;
    size_t maximumSize() const;

    void setMaximumSize(size_t maximumSize);

private:
    void clean();

    using Item = std::pair<KeyType, ValueType>;
    using Items = std::list<Item>;
    Items _itemList;
    std::unordered_map<KeyType, decltype(_itemList.begin()), HasherType> _itemMap;

    size_t _cacheSize;
    size_t _maximumCacheSize;
};

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

#include <modules/globebrowsing/cache/memoryawarelrucache.inl>

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_LRU_CACHE___H__
