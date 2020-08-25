/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_CORE___SYNCBUFFER___H__
#define __OPENSPACE_CORE___SYNCBUFFER___H__

#include <memory>
#include <string>
#include <vector>

namespace openspace {

class SyncBuffer {
public:
    SyncBuffer(size_t n);

    ~SyncBuffer();

    void encode(const std::string& s);

    template <typename T>
    void encode(const T& v);

    std::string decode();

    template <typename T>
    T decode();

    void decode(std::string& s);

    template <typename T>
    void decode(T& value);

    void reset();

    //void write();
    //void read();

    void setData(std::vector<std::byte> data);
    std::vector<std::byte> data();

private:
    size_t _n;
    size_t _encodeOffset = 0;
    size_t _decodeOffset = 0;
    std::vector<std::byte> _dataStream;
};

} // namespace openspace

#include "syncbuffer.inl"

#endif // __OPENSPACE_CORE___SYNCBUFFER___H__
