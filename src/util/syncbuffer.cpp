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

#include <openspace/util/syncbuffer.h>

#include <sgct.h>

namespace openspace {

SyncBuffer::SyncBuffer(size_t n)
    : _n(n)
    , _encodeOffset(0)
    , _decodeOffset(0)
    , _synchronizationBuffer(new sgct::SharedVector<char>())
{
    _dataStream.resize(_n);
}

SyncBuffer::~SyncBuffer() {
    // The destructor is defined here, so that the otherwise default inlined destructor is
    // not created (which would make it impossible to use a forward declaration with
    // unique_ptr
}

void SyncBuffer::write() {
    _dataStream.resize(_encodeOffset);
    _synchronizationBuffer->setVal(_dataStream);
    sgct::SharedData::instance()->writeVector(_synchronizationBuffer.get());
    _dataStream.resize(_n);
    _encodeOffset = 0;
    _decodeOffset = 0;
}

void SyncBuffer::read() {
    sgct::SharedData::instance()->readVector(_synchronizationBuffer.get());
    _dataStream = _synchronizationBuffer->getVal();
    _encodeOffset = 0;
    _decodeOffset = 0;
}

} // namespace openspace
