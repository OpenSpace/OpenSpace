/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___MANAGEDMEMORYUNIQUEPTR___H__
#define __GHOUL___MANAGEDMEMORYUNIQUEPTR___H__

#include <memory>

// The coding style in this file is a bit different to make the class in here more similar
// to the std

namespace ghoul {

namespace detail {
    template <typename T>
    struct placement_delete {
        void operator()(T* ptr) {
            if (ptr) {
                ptr->~T();
            }
        }
    };
} // namespace detail

template <typename T>
using managed_memory_unique_ptr = std::unique_ptr<T, detail::placement_delete<T>>;

template <typename T>
using mm_unique_ptr = managed_memory_unique_ptr<T>;

} // namespace ghoul

#endif // __GHOUL___MANAGEDMEMORYUNIQUEPTR___H__
