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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___PIXEL_BUFFER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___PIXEL_BUFFER___H__

#include <ghoul/opengl/ghoul_gl.h>

namespace openspace::globebrowsing {

/**
 * Handles an OpenGL pixel buffer which contains data allocated on the GPU. A simple
 * class that wraps the standard functionality of OpenGL pixel buffer objects. Once
 * the PixelBuffer is created, data is allocated on the GPU. When mapping data to a
 * address pointer, the user needs to ensure the data is unmapped before the data can
 * be used on the GPU / CPU depending on Usage.
 */
class PixelBuffer {
public:
    /**
     * All kinds of usage for pixel buffer objects as defined by the OpenGL standard.
     * See: https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferData.xhtml
     */
    enum class Usage : std::underlying_type_t<GLenum> {
        StreamDraw = static_cast<std::underlying_type_t<GLenum>>(GL_STREAM_DRAW),
        StreamRead = static_cast<std::underlying_type_t<GLenum>>(GL_STREAM_READ),
        StreamCopy = static_cast<std::underlying_type_t<GLenum>>(GL_STREAM_COPY),
        StaticDraw = static_cast<std::underlying_type_t<GLenum>>(GL_STATIC_DRAW),
        StaticRead = static_cast<std::underlying_type_t<GLenum>>(GL_STATIC_READ),
        StaticCopy = static_cast<std::underlying_type_t<GLenum>>(GL_STATIC_COPY),
        DynamicDraw = static_cast<std::underlying_type_t<GLenum>>(GL_DYNAMIC_DRAW),
        DynamicRead = static_cast<std::underlying_type_t<GLenum>>(GL_DYNAMIC_READ),
        DynamicCopy = static_cast<std::underlying_type_t<GLenum>>(GL_DYNAMIC_COPY)
    };

    /**
     * Access hints for OpenGL buffer mapping
     * See: https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMapBuffer.xml
     */
    enum class Access : std::underlying_type_t<GLenum> {
        ReadOnly = static_cast<std::underlying_type_t<GLenum>>(GL_READ_ONLY),
        WriteOnly = static_cast<std::underlying_type_t<GLenum>>(GL_WRITE_ONLY),
        ReadWrite = static_cast<std::underlying_type_t<GLenum>>(GL_READ_WRITE)
    };

    /**
     * Allocates <code>numBytes</code> bytes on the GPU and creates an ID for the pixel
     * buffer object.
     *
     * \param numBytes is the number of bytes to be allocated on GPU memory
     * \param usage is the Usage for the pixel buffer
     */
    PixelBuffer(size_t numBytes, Usage usage);

    /**
     * calls glDeleteBuffers().
     */
    ~PixelBuffer();

    /**
     * Maps an address pointer to GPU direct memory access. The user must make sure the
     * buffer is bound before calling this function.
     *
     * \param access is the access to which can be any of <code>GL_READ_ONLY</code>,
     * <code>GL_WRITE_ONLY</code>, or <code>GL_READ_WRITE</code>
     * \return The DMA address to the mapped buffer. Returns nullptr if the mapping
     *         failed
     */
    void* mapBuffer(Access access);

    /**
     * Maps an address pointer to GPU direct memory access. Gives access to a range of
     * the buffer. The user must make sure the buffer is bound before calling this
     * function.
     *
     * \param offet is the number of bytes to the first address to get in the buffer
     * \param length is the number of bytes to access in the buffer
     * \param access is a bitfield describing the access as described in:
     * https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMapBufferRange.xhtml
     * \return The DMA address to the mapped buffer. Returns nullptr if the mapping
     *         failed
     */
    void* mapBufferRange(GLintptr offset, GLsizeiptr length, BufferAccessMask access);

    /**
     * Maps the default buffer and makes the data available on the GPU
     */
    bool unMapBuffer();

    /**
     * Calls glBindBuffer()
     */
    void bind();

    /**
     * Calls glBindBuffer() with argument 0 to unmap any pixel buffer
     */
    void unbind();

    /**
     * \returns true of the buffer is mapped, otherwise false
     */
    bool isMapped() const;

    /**
     * \returns the OpenGL id of the pixel buffer object
     */
    operator GLuint() const;

private:
    GLuint _id = 0;
    const size_t _numBytes;
    const Usage _usage;
    bool _isMapped = false;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___PIXEL_BUFFER___H__
