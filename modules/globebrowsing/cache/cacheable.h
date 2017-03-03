
#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_CACHE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_CACHE___H__

namespace openspace {
namespace globebrowsing {
namespace cache {

/**
 * Interface used for cacheable classes. A cacheable class needs
 * to implement the function <code>memoryImpact</code> that returns
 * memory impact given in kilobytes.
 */
class Cacheable {
public:
	Cacheable(size_t memoryImpact) : _memoryImpact(memoryImpact) {};
	~Cacheable() {};

	size_t memoryImpact() { return _memoryImpact; };
protected:
	const size_t _memoryImpact;
};

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_CACHE___H__
