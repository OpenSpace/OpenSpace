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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_READER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_READER___H__

#include <modules/globebrowsing/other/concurrentjobmanager.h>
#include <modules/globebrowsing/tile/tileindex.h>

#include <ghoul/opengl/ghoul_gl.h>

#include <unordered_map>
#include <map>
#include <set>

namespace openspace {


class PixelBuffer
{
public:
	enum class Usage {
		STREAM_DRAW = GL_STREAM_DRAW,
		STREAM_READ = GL_STREAM_READ,
		STREAM_COPY = GL_STREAM_COPY,
		STATIC_DRAW = GL_STATIC_DRAW,
		STATIC_READ = GL_STATIC_READ,
		STATIC_COPY = GL_STATIC_COPY,
		DYNAMIC_DRAW = GL_DYNAMIC_DRAW,
		DYNAMIC_READ = GL_DYNAMIC_READ,
		DYNAMIC_COPY = GL_DYNAMIC_COPY
	};

    PixelBuffer(size_t numBytes, Usage usage);
    ~PixelBuffer();
    
    void* mapBuffer(GLenum access);
    void* mapBufferRange(GLintptr offset, GLsizeiptr length, GLbitfield access);
    bool unMapBuffer();

    void bind();
    void unbind();

    bool isMapped();

    operator GLuint();

private:
    GLuint _id;
    const size_t _numBytes;
    const Usage _usage;
    bool _isMapped;
};





template <class KeyType>
class PixelBufferContainer
{
public:
    PixelBufferContainer(size_t numBytesPerBuffer, PixelBuffer::Usage usage,
        size_t numPixelBuffers = 0);
    ~PixelBufferContainer() = default;

    void* mapBuffer(KeyType key, GLenum  access);
    void* mapBufferRange(KeyType key, GLintptr offset, GLsizeiptr length, GLbitfield access);

    bool resetMappedBuffers();

    bool unMapBuffer(KeyType key);
    GLuint idOfMappedBuffer(KeyType key);
private:
    const PixelBuffer::Usage _usage;

    std::vector<std::unique_ptr<PixelBuffer>> _pixelBuffers;
    // Maps from KeyType to index of mapped buffers
    std::map<KeyType, int> _indexMap;
};









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
void* PixelBufferContainer<KeyType>::mapBuffer(KeyType key, GLenum access) {
	typename std::map<KeyType, int>::iterator iter = _indexMap.find(key);
	bool notFoundAmongMappedBuffers = iter == _indexMap.end();

	if (!notFoundAmongMappedBuffers) { // This PBO is already mapped
		ghoul_assert(_pixelBuffers[iter->second], "Incorrect index map");
		return nullptr;
	}

    // Find a pixel buffer that is unmapped
	for (int i = 0; i < _pixelBuffers.size(); ++i) {
		bool bufferIsMapped = _pixelBuffers[i]->isMapped();
        if (!_pixelBuffers[i]->isMapped()) {
            _pixelBuffers[i]->bind();
            void* dataPtr = _pixelBuffers[i]->mapBuffer(access);
            _pixelBuffers[i]->unbind();
            if (dataPtr) { // Success in mapping
                // Add this index to the map of mapped pixel buffers
                _indexMap.emplace(key, i);
                return dataPtr;
            }
        }
    }
	return nullptr;
}

template <class KeyType>
void* PixelBufferContainer<KeyType>::mapBufferRange(KeyType key, GLintptr offset,
    GLsizeiptr length, GLbitfield access)
{
    typename std::map<KeyType, int>::iterator iter = _indexMap.find(key);
    bool notFoundAmongMappedBuffers = iter == _indexMap.end();
	
	if (!notFoundAmongMappedBuffers) { // This PBO is already mapped
		ghoul_assert(_pixelBuffers[iter->second], "Incorrect index map");
		return nullptr;
	}
  
    // Find a pixel buffer that is unmapped
    for (int i = 0; i < _pixelBuffers.size(); ++i) {
        bool bufferIsMapped = _pixelBuffers[i]->isMapped();
        if (!bufferIsMapped) {
            _pixelBuffers[i]->bind();
            void* dataPtr = _pixelBuffers[i]->mapBufferRange(offset, length, access);
            _pixelBuffers[i]->unbind();
            if (dataPtr) { // Success in mapping
                _indexMap.emplace(key, i);
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
        int index = iter->second; // Index where the mapped buffer is stored
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
   typename  std::map<KeyType, int>::iterator iter = _indexMap.find(key);
    if (iter != _indexMap.end()) { // Found a mapped pixel buffer
        int index = iter->second; // Index where the mapped buffer is stored
        _pixelBuffers[index]->bind();
        success = _pixelBuffers[index]->unMapBuffer();
        _pixelBuffers[index]->unbind();
        _indexMap.erase(iter); // This key should no longer be among the mapped buffers
    }
    return success;
}

template <class KeyType>
GLuint PixelBufferContainer<KeyType>::idOfMappedBuffer(KeyType key) {
    typename std::map<KeyType, int>::iterator iter = _indexMap.find(key);
    if (iter != _indexMap.end()) { // Found a mapped pixel buffer
        int index = iter->second; // Index where the mapped buffer is stored
        return *_pixelBuffers[index];
    }
    return 0;
}















namespace globebrowsing {
    
struct RawTile;
class RawTileDataReader;

class AsyncTileDataProvider {
public:
    enum class UsePBO {
        Yes,
        No
    };

    AsyncTileDataProvider(std::shared_ptr<RawTileDataReader> textureDataProvider,
        std::shared_ptr<LRUThreadPool<TileIndex::TileHashKey>> pool,
        UsePBO usePbo = UsePBO::No);

    bool enqueueTileIO(const TileIndex& tileIndex);        
    std::vector<std::shared_ptr<RawTile>> getRawTiles();
    std::shared_ptr<RawTile> popFinishedRawTile();

    void update();
    void reset();

    std::shared_ptr<RawTileDataReader> getRawTileDataReader() const;
    float noDataValueAsFloat() const;

protected:
    virtual bool satisfiesEnqueueCriteria(const TileIndex&);
    void endUnfinishedJobs();

private:
    std::shared_ptr<RawTileDataReader> _rawTileDataReader;
    PrioritizingConcurrentJobManager<RawTile, TileIndex::TileHashKey> _concurrentJobManager;

    std::unique_ptr<PixelBufferContainer<TileIndex::TileHashKey>> _pboContainer;
    std::set<TileIndex::TileHashKey> _enqueuedTileRequests;

    bool _shouldReset;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_READER___H__
