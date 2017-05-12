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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_CACHEABLE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_CACHEABLE___H__

namespace openspace {
namespace globebrowsing {
namespace cache {

/**
 * Base class to be extended by classes that need to be cached and make use of the
 * memoryImpact interface. A class extending <code>MemoryAwareCacheable</code> needs to
 * know its memory impact at initialization and hence provide the memory impact in its
 * constructors. The memory impact can not change during the lifetime of an object that is
 * a <code>MemoryAwareCacheable</code>.
 */
class MemoryAwareCacheable {
public:
    /**
     * \param memoryImpact is the memory impact of the object. Can for example be given
     * in kilobytes.
     */
    MemoryAwareCacheable(size_t memoryImpact) : _memoryImpact(memoryImpact) {};
    ~MemoryAwareCacheable() {};

    size_t memoryImpact() { return _memoryImpact; };
    
protected:
    size_t _memoryImpact;
};

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_CACHEABLE___H__
