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
#include <modules/solarbrowsing/util/pixelbufferobject.h>
#include <iostream>

namespace openspace {

PixelBufferObject::PixelBufferObject(const int size) : _id(0), _size(size) {
    initialize();
}
PixelBufferObject::~PixelBufferObject() {
    glDeleteBuffers(1, &_id);
}

template<typename T>
T* PixelBufferObject::mapToClientMemory(bool shouldOrphanData, int size) {
    if (shouldOrphanData) {
        glBufferData(GL_PIXEL_UNPACK_BUFFER, size, NULL, GL_STREAM_DRAW);
    }
    T* data = static_cast<T*>(
          glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, NULL, size,
                           GL_MAP_WRITE_BIT | GL_MAP_INVALIDATE_BUFFER_BIT));

    return std::move(data);
}

const GLuint& PixelBufferObject::id() {
    return _id;
}

void PixelBufferObject::releaseMappedBuffer() {
    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
}

void PixelBufferObject::activate() {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _id);
}

void PixelBufferObject::deactivate() {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
}

void PixelBufferObject::initialize() {
    glGenBuffers(1, &_id);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _id);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, _size, 0, GL_STREAM_DRAW);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
}

template unsigned char* PixelBufferObject::mapToClientMemory(bool shouldOrphanData, int size);
template float* PixelBufferObject::mapToClientMemory(bool shouldOrphanData, int size);
template double* PixelBufferObject::mapToClientMemory(bool shouldOrphanData, int size);
template int* PixelBufferObject::mapToClientMemory(bool shouldOrphanData, int size);
template unsigned int* PixelBufferObject::mapToClientMemory(bool shouldOrphanData, int size);
template long* PixelBufferObject::mapToClientMemory(bool shouldOrphanData, int size);
template unsigned long* PixelBufferObject::mapToClientMemory(bool shouldOrphanData, int size);
} // namespace openspace
