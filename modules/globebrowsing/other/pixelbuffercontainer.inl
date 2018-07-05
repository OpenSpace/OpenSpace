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

namespace openspace::globebrowsing {

template <class KeyType>
PixelBufferContainer<KeyType>::PixelBufferContainer(size_t numBytesPerBuffer,
    PixelBuffer::Usage usage, size_t numPixelBuffers)
    : _usage(usage)
{
    for (size_t i = 0; i < numPixelBuffers; ++i) {
        _pixelBuffers.push_back(std::make_unique<PixelBuffer>(numBytesPerBuffer, _usage));
    }
}

template <class KeyType>
void* PixelBufferContainer<KeyType>::mapBuffer(KeyType key, PixelBuffer::Access access) {
    const typename std::map<KeyType, int>::const_iterator iter = _indexMap.find(key);
    const bool notFoundAmongMappedBuffers = (iter == _indexMap.end());

    if (!notFoundAmongMappedBuffers) { // This PBO is already mapped
        ghoul_assert(_pixelBuffers[iter->second], "Incorrect index map");
        return nullptr;
    }

    // Find a pixel buffer that is unmapped
    for (size_t i = 0; i < _pixelBuffers.size(); ++i) {
        if (!_pixelBuffers[i]->isMapped()) {
            _pixelBuffers[i]->bind();
            void* dataPtr = _pixelBuffers[i]->mapBuffer(access);
            _pixelBuffers[i]->unbind();
            if (dataPtr) { // Success in mapping
                // Add this index to the map of mapped pixel buffers
                _indexMap.emplace(std::move(key), static_cast<int>(i));
                return dataPtr;
            }
        }
    }
    return nullptr;
}

template <class KeyType>
void* PixelBufferContainer<KeyType>::mapBufferRange(KeyType key, GLintptr offset,
                                                    GLsizeiptr length,
                                                    BufferAccessMask access)
{
    const typename std::map<KeyType, int>::const_iterator iter = _indexMap.find(key);
    const bool notFoundAmongMappedBuffers = (iter == _indexMap.end());

    if (!notFoundAmongMappedBuffers) { // This PBO is already mapped
        ghoul_assert(_pixelBuffers[iter->second], "Incorrect index map");
        return nullptr;
    }

    // Find a pixel buffer that is unmapped
    for (int i = 0; i < _pixelBuffers.size(); ++i) {
        const bool bufferIsMapped = _pixelBuffers[i]->isMapped();
        if (!bufferIsMapped) {
            _pixelBuffers[i]->bind();
            void* dataPtr = _pixelBuffers[i]->mapBufferRange(offset, length, access);
            _pixelBuffers[i]->unbind();
            if (dataPtr) { // Success in mapping
                _indexMap.emplace(std::move(key), i);
                return dataPtr;
            }
        }
    }
    return nullptr;
}

template <class KeyType>
bool PixelBufferContainer<KeyType>::resetMappedBuffers() {
    bool success = true;
    for (auto iter = _indexMap.begin(); iter != _indexMap.end(); iter++) {
        const int index = iter->second; // Index where the mapped buffer is stored
        _pixelBuffers[index]->bind();
        success &= _pixelBuffers[index]->unMapBuffer();
        _pixelBuffers[index]->unbind();
        _indexMap.erase(iter); // This key should no longer be among the mapped buffers
    }
    return success;
}

template <class KeyType>
bool PixelBufferContainer<KeyType>::unMapBuffer(KeyType key) {
    bool success = false;
    const typename std::map<KeyType, int>::const_iterator iter = _indexMap.find(key);
    if (iter != _indexMap.end()) { // Found a mapped pixel buffer
        const int index = iter->second; // Index where the mapped buffer is stored
        _pixelBuffers[index]->bind();
        success = _pixelBuffers[index]->unMapBuffer();
        _pixelBuffers[index]->unbind();
        _indexMap.erase(iter); // This key should no longer be among the mapped buffers
    }
    return success;
}

template <class KeyType>
GLuint PixelBufferContainer<KeyType>::idOfMappedBuffer(KeyType key) {
    const typename std::map<KeyType, int>::const_iterator iter = _indexMap.find(key);
    if (iter != _indexMap.end()) { // Found a mapped pixel buffer
        int index = iter->second; // Index where the mapped buffer is stored
        return *_pixelBuffers[index];
    }
    return 0;
}

} // namespace openspace::globebrowsing
