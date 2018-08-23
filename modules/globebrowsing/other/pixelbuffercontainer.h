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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___PIXEL_BUFFER_CONTAINER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___PIXEL_BUFFER_CONTAINER___H__

#include <modules/globebrowsing/other/pixelbuffer.h>
#include <map>

namespace openspace::globebrowsing {

/**
 * Templated class which owns one or many PixelBuffer%s. The KeyType is used to map a
 * pixel buffer but only if it is not already mapped.
 */
template <class KeyType>
class PixelBufferContainer {
public:
    /**
     * Creates numPixelBuffers pixel buffer objects, each with numBytesPerBuffer bytes
     * allocated on the GPU.
     *
     * \param numBytesPerBuffer is the number of bytes per pixel buffer. All pixel
     *        buffers within a <code>PixelBufferContainer</code> have the same number of
     *        bytes
     * \param usage is the <code>Usage</code> as described by <code>PixelBuffer</code>
     * \param numPixelBuffers is the number of pixel buffers to create for this container.
     *        If numPixelBuffers is omitted, no pixel buffers are created.
     */
    PixelBufferContainer(size_t numBytesPerBuffer,
        globebrowsing::PixelBuffer::Usage usage, size_t numPixelBuffers = 0);
    ~PixelBufferContainer() = default;

    /**
     * Finds a Pixel buffer and maps it if it is available.
     *
     * \param key is the identifier for the pixel buffer which can be used later when
     *        unmapping the mapped pixel buffer.
     * \param access is the access as described by <code>PixelBuffer</code>
     * \return An address pointer to DMA if the mapping succeeded. Otherwise a \c nullptr
     *         is returned. The mapping can fail if the buffer identified with \p key is
     *         already mapped or if something else failed.
     */
    void* mapBuffer(KeyType key, PixelBuffer::Access access);

    /**
     * Finds a Pixel buffer and maps a range of it if it is available.
     *
     * \param key is the identifier for the pixel buffer which can be used later when
     *        unmapping the mapped pixel buffer.
     * \param offet is the number of bytes to the first address to get in the buffer
     * \param length is the number of bytes to access in the buffer
     * \param access is the access as described by <code>PixelBuffer</code>
     * \return An address pointer to DMA if the mapping succeeded. Otherwise a nullptr
     *         is returned. The mapping can fail if the buffer identified with \p key is
     *         already mapped or if something else failed.
     */
    void* mapBufferRange(KeyType key, GLintptr offset, GLsizeiptr length,
        BufferAccessMask access);

    /**
     * Unmaps all buffers in the PixelBufferContainer.
     *
     * \return \c true if the unmapping succeeded, otherwise \c false
     */
    bool resetMappedBuffers();

    /**
     * Unmaps a buffer that has previously been mapped. This buffer is identified using
     * \p key.
     *
     * \param key is the identifier of the mapped buffer.
     * \return \c true if the unmapping succeeded, otherwise \c false
     */
    bool unMapBuffer(KeyType key);

    /**
     * \return The \c GLuint id of a pixel buffer identified by \p key if it currently is
     * mapped.
     */
    GLuint idOfMappedBuffer(KeyType key);
private:
    const globebrowsing::PixelBuffer::Usage _usage;

    std::vector<std::unique_ptr<PixelBuffer>> _pixelBuffers;

    // Maps from KeyType to index of mapped buffers
    std::map<KeyType, int> _indexMap;
};

} // namespace openspace::globebrowsing

#include "pixelbuffercontainer.inl"

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___PIXEL_BUFFER_CONTAINER___H__
