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

#ifndef __GHOUL___MAP___H__
#define __GHOUL___MAP___H__

#include <functional>
#include <string>
#include <string_view>

// The contents of this file define `transparent_string_hash` which is a type that can be
// used for unordered containers to make them capable of heterogeneous lookup. This
// follows the implementatoin of P0917 and P1690 that have been added into C++20.
//
// To use this, you would defined a container like such:
//
// std::unordered_map<std::string, int, transparent_string_hash, std::equal_to<>> map;
//
// and it allows the lookup of string_view and char* in the map without the memory
// allocation otherwise required

template <typename... Bases>
struct overload : Bases ... {
    using is_transparent = void;
    using Bases::operator() ...;
};

struct char_pointer_hash {
    auto operator()(const char* ptr) const noexcept {
        return std::hash<std::string_view>{}(ptr);
    }
};

using transparent_string_hash = overload<
    std::hash<std::string>,
    std::hash<std::string_view>,
    char_pointer_hash
>;

#endif // __GHOUL___MAP___H__
