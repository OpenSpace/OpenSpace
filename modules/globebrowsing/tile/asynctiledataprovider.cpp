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

#include <modules/globebrowsing/tile/asynctiledataprovider.h>

#include <modules/globebrowsing/tile/loadjob/tileloadjob.h>
#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>
#include <modules/globebrowsing/tile/tilediskcache.h>

#include <ghoul/logging/logmanager.h>

namespace openspace {

namespace {
    const char* _loggerCat = "PixelBuffer";
}

PixelBuffer::PixelBuffer(size_t numBytes, Usage usage)
    : _numBytes(numBytes)
    , _usage(usage)
	, _isMapped(false)
{
    glGenBuffers(1, &_id);
    bind();
    glBufferData(GL_PIXEL_UNPACK_BUFFER, _numBytes, 0, static_cast<GLenum>(_usage));
    unbind();
}

PixelBuffer::~PixelBuffer() {
    glDeleteBuffers(1, &_id);
}

void* PixelBuffer::mapBuffer(GLenum access) {
    void* dataPtr = glMapBuffer(GL_PIXEL_UNPACK_BUFFER, access);
    _isMapped = dataPtr ? true : false;
    return dataPtr;
}

void* PixelBuffer::mapBufferRange(GLintptr offset, GLsizeiptr length, GLbitfield access) {
    void* dataPtr = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, offset, length, access);
    _isMapped = dataPtr ? true : false;
    return dataPtr;
}

bool PixelBuffer::unMapBuffer() {
    bool success = glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    if (!success) {
        LERROR("Unable to unmap pixel buffer, data may be corrupt!");
    }
    _isMapped = false;
    return success;
}

void PixelBuffer::bind() {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _id);
}

void PixelBuffer::unbind() {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
}

bool PixelBuffer::isMapped() {
    return _isMapped;
}

PixelBuffer::operator GLuint() {
    return _id;
}





namespace globebrowsing {

AsyncTileDataProvider::AsyncTileDataProvider(
    const std::shared_ptr<RawTileDataReader> rawTileDataReader,
    std::shared_ptr<LRUThreadPool<TileIndex::TileHashKey>> pool,
    UsePBO usePbo)
    : _rawTileDataReader(rawTileDataReader)
    , _concurrentJobManager(pool)
    , _pboContainer(nullptr)
    , _shouldReset(false)
{
    if (usePbo == UsePBO::Yes) {
        size_t pboNumBytes = rawTileDataReader->tileTextureInitData().totalNumBytes();
        _pboContainer = std::make_unique<PixelBufferContainer<TileIndex::TileHashKey>>(
            pboNumBytes, PixelBuffer::Usage::STREAM_DRAW, 10
        );
    }
}

std::shared_ptr<RawTileDataReader> AsyncTileDataProvider::getRawTileDataReader() const {
    return _rawTileDataReader;
}

bool AsyncTileDataProvider::enqueueTileIO(const TileIndex& tileIndex) {
    if (!_shouldReset && satisfiesEnqueueCriteria(tileIndex)) {
		if (_pboContainer) {
            char* dataPtr = static_cast<char*>(_pboContainer->mapBuffer(
                tileIndex.hashKey(), GL_WRITE_ONLY));
            if (dataPtr) {
                auto job = std::make_shared<TileLoadJob>(_rawTileDataReader, tileIndex,
                    dataPtr);
                _concurrentJobManager.enqueueJob(job, tileIndex.hashKey());
                _enqueuedTileRequests.insert(tileIndex.hashKey());
            }
            else {
                return false;
            }
        }
        else {
            size_t numBytes = _rawTileDataReader->tileTextureInitData().totalNumBytes();
            auto job = std::make_shared<TileLoadJob>(_rawTileDataReader, tileIndex,
                numBytes);
            _concurrentJobManager.enqueueJob(job, tileIndex.hashKey());
            _enqueuedTileRequests.insert(tileIndex.hashKey());
        }
        return true;
    }
    return false;
}

std::vector<std::shared_ptr<RawTile>> AsyncTileDataProvider::getRawTiles() {
    std::vector<std::shared_ptr<RawTile>> readyResults;
    std::shared_ptr<RawTile> finishedJob = popFinishedRawTile();
    while (finishedJob) {
        readyResults.push_back(finishedJob);
        finishedJob = popFinishedRawTile();
    }
    return readyResults;
}   

std::shared_ptr<RawTile> AsyncTileDataProvider::popFinishedRawTile() {
    if (_concurrentJobManager.numFinishedJobs() > 0) {

        // Now the tile load job looses ownerwhip of the data pointer
        std::shared_ptr<RawTile> product =
            _concurrentJobManager.popFinishedJob()->product();
      
        TileIndex::TileHashKey key = product->tileIndex.hashKey();
        // Pbo is still mapped. Set the id for the raw tile
        if (_pboContainer) {
            product->pbo = _pboContainer->idOfMappedBuffer(key);
            // Now we are finished with the mapping of this pbo
            _pboContainer->unMapBuffer(key);
        }
        else {
            product->pbo = 0;
        }
        
        // No longer enqueued. Remove from set of enqueued tiles
        _enqueuedTileRequests.erase(key);

        return product;
    }
    else
        return nullptr;
}   

bool AsyncTileDataProvider::satisfiesEnqueueCriteria(const TileIndex& tileIndex) {
    // Only satisfies if it is not already enqueued. Also bumps the request to the top.
    bool alreadyEnqueued = _concurrentJobManager.touch(tileIndex.hashKey());
    bool notFoundAmongEnqueued =
        _enqueuedTileRequests.find(tileIndex.hashKey()) ==_enqueuedTileRequests.end();

    return !alreadyEnqueued && notFoundAmongEnqueued;
}

void AsyncTileDataProvider::endUnfinishedJobs() {
    std::vector<TileIndex::TileHashKey> unfinishedJobs =
        _concurrentJobManager.getKeysToUnfinishedJobs();
    for (auto unfinishedJob : unfinishedJobs) {
        // Unmap unfinished jobs
        if (_pboContainer) {
            _pboContainer->unMapBuffer(unfinishedJob);
        }
        _enqueuedTileRequests.erase(unfinishedJob);
    }
}

void AsyncTileDataProvider::update() {
    endUnfinishedJobs();
    // Only reset if there are no threads executing any jobs i.e.
    // _enqueuedTileRequests is empty
    if (_shouldReset && _enqueuedTileRequests.size() == 0) {
        while (_concurrentJobManager.numFinishedJobs() > 0) {
            // When calling product, the job releases ownership of the data and it must
            // be deleted if not using PBO
            std::shared_ptr<RawTile> product =
                _concurrentJobManager.popFinishedJob()->product();
            if (_pboContainer) {
                TileIndex::TileHashKey key = product->tileIndex.hashKey();
                _pboContainer->unMapBuffer(key);
            }
            else {
                delete [] product->imageData;
            }
        }
        _rawTileDataReader->reset();
        _shouldReset = false;
    }
}

void AsyncTileDataProvider::reset() {
    // Can not clear concurrent job manager in case there are threads running. therefore
    // we need to wait until _enqueuedTileRequests is empty before finishing up.
    _shouldReset = true;
    _concurrentJobManager.clearEnqueuedJobs();
}

float AsyncTileDataProvider::noDataValueAsFloat() const {
    return _rawTileDataReader->noDataValueAsFloat();
}

} // namespace globebrowsing
} // namespace openspace
