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
{
    glGenBuffers(1, &_id);
    bind();
    glBufferData(GL_PIXEL_UNPACK_BUFFER, _numBytes, 0, static_cast<GLenum>(_usage));
    unbind();
}

PixelBuffer::~PixelBuffer() {
    glDeleteBuffers(1, &_id);
}

void* PixelBuffer::mapBuffer(GLbitfield access) {
    void* dataPtr = glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, 0, _numBytes, access);
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
    std::shared_ptr<ThreadPool> pool)
    : _rawTileDataReader(rawTileDataReader)
    , _concurrentJobManager(pool)
{
    // PBO
    size_t pboNumBytes = rawTileDataReader->getWriteDataDescription().totalNumBytes;

    //_pbo = std::make_unique<PixelBuffer>(pboNumBytes, PixelBuffer::Usage::STREAM_DRAW);
}

std::shared_ptr<RawTileDataReader> AsyncTileDataProvider::getRawTileDataReader() const {
    return _rawTileDataReader;
}

bool AsyncTileDataProvider::enqueueTileIO(const TileIndex& tileIndex) {
    if (satisfiesEnqueueCriteria(tileIndex)) {
		//_pbo->bind();
        char* pboBufferData = new char[_rawTileDataReader->getWriteDataDescription().totalNumBytes];// static_cast<char*>(_pbo->mapBuffer(GL_MAP_WRITE_BIT));
        //_pbo->unbind();
        
		if (pboBufferData)
		{
			//char* dataDestination = new char[_pboNumBytes];
			auto job = std::make_shared<TileLoadJob>(_rawTileDataReader, tileIndex, pboBufferData);
			//auto job = std::make_shared<DiskCachedTileLoadJob>(_tileDataset, tileIndex, tileDiskCache, "ReadAndWrite");
			_concurrentJobManager.enqueueJob(job);
			_enqueuedTileRequests[tileIndex.hashKey()] = tileIndex;

			return true;
		}
    }
    return false;
}

std::vector<std::shared_ptr<RawTile>> AsyncTileDataProvider::getRawTiles() {
    std::vector<std::shared_ptr<RawTile>> readyResults;
    while (_concurrentJobManager.numFinishedJobs() > 0) {
        readyResults.push_back(_concurrentJobManager.popFinishedJob()->product());
    }
    return readyResults;
}   

std::shared_ptr<RawTile> AsyncTileDataProvider::popFinishedRawTile() {
    if (_concurrentJobManager.numFinishedJobs() > 0) {

        //_pbo->bind();
        //_pbo->unMapBuffer();
        //_pbo->unbind();

        return _concurrentJobManager.popFinishedJob()->product();
	}
    else
        return nullptr;
}   

bool AsyncTileDataProvider::satisfiesEnqueueCriteria(const TileIndex& tileIndex) const {
    // only allow tile to be enqueued if it's not already enqueued
    return _enqueuedTileRequests.find(tileIndex.hashKey()) == _enqueuedTileRequests.end();
}

void AsyncTileDataProvider::reset() {
    //_futureTileIOResults.clear();
    //_threadPool->stop(ghoul::ThreadPool::RunRemainingTasks::No);
    //_threadPool->start();
    _enqueuedTileRequests.clear();
    _concurrentJobManager.reset();
    while (_concurrentJobManager.numFinishedJobs() > 0) {
        _concurrentJobManager.popFinishedJob();
    }
    _rawTileDataReader->reset();
}

void AsyncTileDataProvider::clearRequestQueue() {
    //_threadPool->clearRemainingTasks();
    //_futureTileIOResults.clear();
    _concurrentJobManager.clearEnqueuedJobs();
    _enqueuedTileRequests.clear();
}

float AsyncTileDataProvider::noDataValueAsFloat() const {
    return _rawTileDataReader->noDataValueAsFloat();
}

} // namespace globebrowsing
} // namespace openspace
