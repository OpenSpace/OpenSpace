
#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LRU_MEMORY_CACHE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LRU_MEMORY_CACHE___H__

#include <modules/globebrowsing/cache/cacheable.h>

#include <list>
#include <unordered_map>

namespace openspace {
namespace globebrowsing {
namespace cache {

/**
 * This LRU cache needs a value type which implements the
 * <code>Cacheable</code> interface.
*/
template<typename KeyType, typename ValueType>
class LRUMemoryCache {
public:
    LRUMemoryCache(size_t size, DataSizeType dataSizeType);

    void put(const KeyType& key, const ValueType& value);
    void clear();
    bool exist(const KeyType& key) const;
    ValueType get(const KeyType& key);
    
    /**
     * \returns the number of <code>DataSizeType</code> currently in the cache.
     */
    size_t size() const;

    /**
     * \returns the maximum number of <code>DataSizeType</code> allowed in the cache.
     */
    size_t maximumSize() const;

private:
    void clean();

    std::list<std::pair<KeyType, ValueType>> _itemList;
    std::unordered_map<KeyType, decltype(_itemList.begin())> _itemMap;
    
    // Number of <code>DataSizeType</code> cached
    size_t _cacheSize;
    const size_t _maxCacheSize;
    const DataSizeType _dataSizeType;
};

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

#include <modules/globebrowsing/other/lrumemorycache.inl>

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LRU_MEMORY_CACHE___H__
