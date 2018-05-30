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

#include <modules/globebrowsing/other/pixelbuffer.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "PixelBuffer";
} // namespace

namespace openspace::globebrowsing {

PixelBuffer::PixelBuffer(size_t numBytes, Usage usage)
    : _numBytes(numBytes)
    , _usage(usage)
{
    glGenBuffers(1, &_id);
    bind();
    glBufferData(GL_PIXEL_UNPACK_BUFFER, _numBytes, nullptr, static_cast<GLenum>(_usage));
    unbind();
}

PixelBuffer::~PixelBuffer() {
    glDeleteBuffers(1, &_id);
}

void* PixelBuffer::mapBuffer(Access access) {
    void* dataPtr = glMapBuffer(GL_PIXEL_UNPACK_BUFFER, static_cast<GLenum>(access));
    _isMapped = dataPtr != nullptr;
    return dataPtr;
}

void* PixelBuffer::mapBufferRange(GLintptr offset, GLsizeiptr length,
                                  BufferAccessMask access)
{
    void* dataPtr = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, offset, length, access);
    _isMapped = dataPtr;
    return dataPtr;
}

bool PixelBuffer::unMapBuffer() {
    GLboolean success = glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    if (!success) {
        LERROR("Unable to unmap pixel buffer, data may be corrupt!");
    }
    _isMapped = false;
    return success == GL_TRUE;
}

void PixelBuffer::bind() {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _id);
}

void PixelBuffer::unbind() {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
}

bool PixelBuffer::isMapped() const {
    return _isMapped;
}

PixelBuffer::operator GLuint() const {
    return _id;
}

} // namespace openspace::globebrowsing
