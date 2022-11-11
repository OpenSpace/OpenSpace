/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <climits>

namespace openspace::softwareintegration::simp {

namespace {


bool hostIsBigEndian() {
    const int i = 1;
    // For big endian the most significant byte 
    // is placed at the byte with the lowest address
    // Example: 4 byte int with with value 1 is as bytes
    // is 0001 for big endian and 1000 for little endian
    return *(const char*)&i == 0;
}

template <typename T>
T swapEndian(T value) {
    union {
        T u;
        unsigned char u8[sizeof(T)];
    } source, dest;

    source.u = value;

    for (size_t k = 0; k < sizeof(T); k++) {
        dest.u8[k] = source.u8[sizeof(T) - k - 1];
    }

    return *reinterpret_cast<T*>(dest.u8);
}

} // namespace

template <typename T>
T networkToHostEndian(T value) {
    static_assert (CHAR_BIT == 8, "CHAR_BIT != 8");

    if (hostIsBigEndian()) return value;

    return swapEndian(value);
}

template <typename T>
T hostToNetworkEndian(T value) {
    static_assert (CHAR_BIT == 8, "CHAR_BIT != 8");
    
    if (!hostIsBigEndian()) return value;

    return swapEndian(value);
}

} // namespace openspace::softwareintegration::simp
