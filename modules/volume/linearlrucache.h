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

#ifndef __OPENSPACE_MODULE_VOLUME___LINEARLRUCACHE___H__
#define __OPENSPACE_MODULE_VOLUME___LINEARLRUCACHE___H__

//#include <modules/volume/lrucache.h>
#include <ghoul/glm.h>
#include <list>
#include <iterator>

namespace openspace::volume {

template <typename ValueType>
class LinearLruCache {
public:
    LinearLruCache(size_t capacity, size_t nIndices);

    bool has(size_t key) const;
    void set(size_t key, ValueType value);

    ValueType& use(size_t key);
    ValueType& get(size_t key);

    void evict();
    size_t capacity() const;

private:
    void insert(size_t key, const ValueType& value);

    std::list<size_t> _tracker;
    std::vector<std::pair<ValueType, typename std::list<size_t>::iterator>> _cache;
    size_t _capacity;
};

} // namespace openspace::volume

#include "linearlrucache.inl"

#endif // __OPENSPACE_MODULE_VOLUME___LINEARLRUCACHE___H__
