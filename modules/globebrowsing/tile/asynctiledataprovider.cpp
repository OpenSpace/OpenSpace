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

namespace openspace {
namespace globebrowsing {

AsyncTileDataProvider::AsyncTileDataProvider(
    const std::shared_ptr<RawTileDataReader> rawTileDataReader,
    std::shared_ptr<ThreadPool> pool)
    : _rawTileDataReader(rawTileDataReader)
    , _concurrentJobManager(pool)
{}

std::shared_ptr<RawTileDataReader> AsyncTileDataProvider::getRawTileDataReader() const {
    return _rawTileDataReader;
}

bool AsyncTileDataProvider::enqueueTileIO(const TileIndex& tileIndex) {
    if (satisfiesEnqueueCriteria(tileIndex)) {
        auto job = std::make_shared<TileLoadJob>(_rawTileDataReader, tileIndex);
        //auto job = std::make_shared<DiskCachedTileLoadJob>(_tileDataset, tileIndex, tileDiskCache, "ReadAndWrite");
        _concurrentJobManager.enqueueJob(job);
        _enqueuedTileRequests[tileIndex.hashKey()] = tileIndex;

        return true;
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
    if (_concurrentJobManager.numFinishedJobs() > 0)
        return _concurrentJobManager.popFinishedJob()->product();
    else
        return nullptr;
}   

bool AsyncTileDataProvider::satisfiesEnqueueCriteria(const TileIndex& tileIndex) const {
    // only allow tile to be enqueued if it's not already enqueued
    //return _futureTileIOResults.find(tileIndex.hashKey()) == _futureTileIOResults.end();
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
