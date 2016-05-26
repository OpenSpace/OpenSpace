  /*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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


#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/misc/assert.h>

#include <modules/globebrowsing/other/asynctexturedataprovider.h>
#include <modules/globebrowsing/other/tileprovider.h>
#include <modules/globebrowsing/geodetics/angle.h>

namespace {
    const std::string _loggerCat = "AsyncTextureDataProvider";
}



namespace openspace {


    AsyncTileDataProvider::AsyncTileDataProvider(const std::string& filename, int minNumPixels, ThreadPool& pool)
        : _textureDataProvider(std::shared_ptr<TileDataset>(new TileDataset(filename, minNumPixels)))
        , _concurrentJobManager(pool)
    {

    }

    AsyncTileDataProvider::AsyncTileDataProvider(
        std::shared_ptr<TileDataset> textureDataProvider,
        ThreadPool& pool)
        : _textureDataProvider(textureDataProvider)
        , _concurrentJobManager(pool)
    {

    }

    AsyncTileDataProvider::~AsyncTileDataProvider() {
        
    }


    std::shared_ptr<TileDataset> AsyncTileDataProvider::getTextureDataProvider() const {
        return _textureDataProvider;
    }

    bool AsyncTileDataProvider::enqueueTextureData(const ChunkIndex& chunkIndex) {
        if (satisfiesEnqueueCriteria(chunkIndex)) {
            std::shared_ptr<TileLoadJob> job = std::shared_ptr<TileLoadJob>(
                new TileLoadJob(_textureDataProvider, chunkIndex));

            _concurrentJobManager.enqueueJob(job);
            _enqueuedTileRequests[chunkIndex.hashKey()] = chunkIndex;
            return true;
        }
        return false;
    }

    bool AsyncTileDataProvider::hasLoadedTextureData() const{
        return _concurrentJobManager.numFinishedJobs() > 0;
    }
    
    std::shared_ptr<TileIOResult> AsyncTileDataProvider::nextTileIOResult() {
        auto tileIOResult = _concurrentJobManager.popFinishedJob()->product();
        HashKey key = tileIOResult->rawTileData->chunkIndex.hashKey();
        if (_enqueuedTileRequests.find(key) != _enqueuedTileRequests.end()) {
            _enqueuedTileRequests.erase(key);
        }
        return tileIOResult;
    }


    bool AsyncTileDataProvider::satisfiesEnqueueCriteria(const ChunkIndex& chunkIndex) const {
        auto it = _enqueuedTileRequests.begin();
        auto end = _enqueuedTileRequests.end();
        for (; it != end; it++) {
            const ChunkIndex& otherChunk = it->second;
            if (chunkIndex.level == otherChunk.level &&
                chunkIndex.manhattan(otherChunk) < 1) {
                return false;
            }
        }
        return true;
    }


    void AsyncTileDataProvider::clearRequestQueue() {
        _concurrentJobManager.clearEnqueuedJobs();
        _enqueuedTileRequests.clear();
    }

}  // namespace openspace
