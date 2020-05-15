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

#include <openspace/util/syncbuffer.h>

#include <ghoul/misc/profiling.h>

namespace openspace {

SyncBuffer::SyncBuffer(size_t n)
    : _n(n)
{
    _dataStream.resize(_n);
}

SyncBuffer::~SyncBuffer() {} // NOLINT

void SyncBuffer::encode(const std::string& s) {
    ZoneScoped

    int32_t anticpatedBufferSize = static_cast<int32_t>(
        _encodeOffset + (sizeof(char) * s.size()) + sizeof(int32_t)
    );
    if (anticpatedBufferSize >= _n) {
        _dataStream.resize(anticpatedBufferSize);
    }

    int32_t length = static_cast<int32_t>(s.size() * sizeof(char));
    memcpy(
        _dataStream.data() + _encodeOffset,
        reinterpret_cast<const char*>(&length),
        sizeof(int32_t)
    );
    _encodeOffset += sizeof(int32_t);
    memcpy(_dataStream.data() + _encodeOffset, s.c_str(), length);
    _encodeOffset += length;
}

std::string SyncBuffer::decode() {
    ZoneScoped

    int32_t length;
    memcpy(
        reinterpret_cast<char*>(&length),
        _dataStream.data() + _decodeOffset,
        sizeof(int32_t)
    );
    std::vector<char> tmp(length + 1);
    _decodeOffset += sizeof(int32_t);
    memcpy(tmp.data(), _dataStream.data() + _decodeOffset, length);
    _decodeOffset += length;
    tmp[length] = '\0';
    std::string ret(tmp.data());
    return ret;
}

void SyncBuffer::decode(std::string& s) {
    s = decode();
}

void SyncBuffer::setData(std::vector<std::byte> data) {
    _dataStream = std::move(data);
}

std::vector<std::byte> SyncBuffer::data() {
    _dataStream.resize(_encodeOffset);

    return _dataStream;
}

void SyncBuffer::reset() {
    _dataStream.resize(_n);
    _encodeOffset = 0;
    _decodeOffset = 0;
}

} // namespace openspace
