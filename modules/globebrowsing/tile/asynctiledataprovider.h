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

#include <ghoul\opengl\ghoul_gl.h>

#include <unordered_map>

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
    
    void* mapBuffer(GLbitfield access);
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

namespace globebrowsing {
    
struct RawTile;
class RawTileDataReader;

class AsyncTileDataProvider {
public:
    AsyncTileDataProvider(std::shared_ptr<RawTileDataReader> textureDataProvider,
        std::shared_ptr<ThreadPool> pool);

    bool enqueueTileIO(const TileIndex& tileIndex);        
    std::vector<std::shared_ptr<RawTile>> getRawTiles();
    std::shared_ptr<RawTile> popFinishedRawTile();

    void reset();
    void clearRequestQueue();

    std::shared_ptr<RawTileDataReader> getRawTileDataReader() const;
    float noDataValueAsFloat() const;

    GLuint pbo() const { return *_pbo; };

protected:
    virtual bool satisfiesEnqueueCriteria(const TileIndex&) const;

private:
    std::shared_ptr<RawTileDataReader> _rawTileDataReader;
    ConcurrentJobManager<RawTile> _concurrentJobManager;
    std::unordered_map<TileIndex::TileHashKey, TileIndex> _enqueuedTileRequests;

    std::unique_ptr<PixelBuffer> _pbo;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_READER___H__
