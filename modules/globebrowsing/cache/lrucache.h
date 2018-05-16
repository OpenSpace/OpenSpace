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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LRU_CACHE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LRU_CACHE___H__

#include <list>
#include <unordered_map>
#include <vector>

namespace openspace::globebrowsing::cache {

/**
 * Templated class implementing a Least-Recently-Used Cache.
 * <code>KeyType</code> needs to be an enumerable type.
 */
template <typename KeyType, typename ValueType, typename HasherType>
class LRUCache {
public:
    using Item = std::pair<KeyType, ValueType>;
    using Items = std::list<Item>;

    /**
     * \param size is the maximum size of the cache given in number of cached items.
     */
    LRUCache(size_t size);

    void put(KeyType key, ValueType value);
    std::vector<Item> putAndFetchPopped(KeyType key, ValueType value);
    void clear();
    bool exist(const KeyType& key) const;

    /**
     * If value exists, the value is bumped to the front of the queue.
     * \returns true if value of this key exists.
     */
    bool touch(const KeyType& key);
    bool isEmpty() const;
    ValueType get(const KeyType& key);

    /**
     * Pops the front of the queue.
     */
    Item popMRU();

    /**
     * Pops the back of the queue.
     */
    Item popLRU();
    size_t size() const;
    size_t maximumCacheSize() const;

private:
    void putWithoutCleaning(KeyType key, ValueType value);
    void clean();

    std::vector<Item> cleanAndFetchPopped();

    Items _itemList;
    std::unordered_map<KeyType, typename Items::const_iterator, HasherType> _itemMap;

    size_t _maximumCacheSize;
};

} // namespace openspace::globebrowsing::cache

#include <modules/globebrowsing/cache/lrucache.inl>

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LRU_CACHE___H__
