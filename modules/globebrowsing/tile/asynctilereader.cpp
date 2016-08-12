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

#include <modules/globebrowsing/tile/asynctilereader.h>
#include <modules/globebrowsing/tile/tileprovider.h>
#include <modules/globebrowsing/tile/tilediskcache.h>


#include <modules/globebrowsing/geometry/angle.h>

namespace {
    const std::string _loggerCat = "AsyncTextureDataProvider";
}



namespace openspace {

    void TileLoadJob::execute() {
        _tileIOResult = _tileDataset->readTileData(_chunkIndex);
    }




    DiskCachedTileLoadJob::DiskCachedTileLoadJob(std::shared_ptr<TileDataset> textureDataProvider, 
        const ChunkIndex& chunkIndex, std::shared_ptr<TileDiskCache> tdc, CacheMode m)
        : TileLoadJob(textureDataProvider, chunkIndex)
        , _tileDiskCache(tdc)
        , _mode(m)
    {

    }

    DiskCachedTileLoadJob::DiskCachedTileLoadJob(std::shared_ptr<TileDataset> textureDataProvider, 
        const ChunkIndex& chunkIndex, std::shared_ptr<TileDiskCache> tdc, const std::string cacheMode)
        : TileLoadJob(textureDataProvider, chunkIndex)
        , _tileDiskCache(tdc)
    {
        if (cacheMode == "Disabled") _mode = CacheMode::Disabled;
        else if (cacheMode == "ReadOnly") _mode = CacheMode::ReadOnly;
        else if (cacheMode == "ReadAndWrite") _mode = CacheMode::ReadAndWrite;
        else if (cacheMode == "WriteOnly") _mode = CacheMode::WriteOnly;
        else if (cacheMode == "CacheHitsOnly") _mode = CacheMode::CacheHitsOnly;
    }



    void DiskCachedTileLoadJob::execute() {
        _tileIOResult = nullptr;

        switch (_mode) {
        case CacheMode::Disabled: 
            _tileIOResult = _tileDataset->readTileData(_chunkIndex); 
            break;

        case CacheMode::ReadOnly:
            _tileIOResult = _tileDiskCache->get(_chunkIndex);
            if (_tileIOResult == nullptr) {
                _tileIOResult = _tileDataset->readTileData(_chunkIndex);
            }
            break;

        case CacheMode::ReadAndWrite:
            _tileIOResult = _tileDiskCache->get(_chunkIndex);
            if (_tileIOResult == nullptr) {
                _tileIOResult = _tileDataset->readTileData(_chunkIndex);
                _tileDiskCache->put(_chunkIndex, _tileIOResult);
            }
            break;

        case CacheMode::WriteOnly:
            _tileIOResult = _tileDataset->readTileData(_chunkIndex);
            _tileDiskCache->put(_chunkIndex, _tileIOResult);
            break;

        case CacheMode::CacheHitsOnly:
            _tileIOResult = _tileDiskCache->get(_chunkIndex);
            if (_tileIOResult == nullptr) {
                TileIOResult res = TileIOResult::createDefaultRes();
                res.chunkIndex = _chunkIndex;
                _tileIOResult = std::make_shared<TileIOResult>(res);
            }
            break;
        }

        
    }



    AsyncTileDataProvider::AsyncTileDataProvider(
        std::shared_ptr<TileDataset> tileDataset,
        std::shared_ptr<ghoul::ThreadPool> pool)
        : _tileDataset(tileDataset)
        , _threadPool(pool)
    {

    }

    AsyncTileDataProvider::~AsyncTileDataProvider() {
        
    }


    std::shared_ptr<TileDataset> AsyncTileDataProvider::getTextureDataProvider() const {
        return _tileDataset;
    }

    bool AsyncTileDataProvider::enqueueTileIO(const ChunkIndex& chunkIndex) {
        if (satisfiesEnqueueCriteria(chunkIndex)) {
            const static auto job = [](const ChunkIndex& chunkIndex, std::shared_ptr<TileDataset> tileDataset) {
                return tileDataset->readTileData(chunkIndex);
            };

            FutureResult futureResult = _threadPool->queue(job, chunkIndex, _tileDataset);
            _futureTileIOResults[chunkIndex.hashKey()] = std::move(futureResult);
            return true;
        }
        return false;
    }

    std::vector<std::shared_ptr<TileIOResult>> AsyncTileDataProvider::getTileIOResults() {
        std::vector<std::shared_ptr<TileIOResult>> readyResults;
        auto it = _futureTileIOResults.begin();
        while(it != _futureTileIOResults.end()) {
            std::future_status status = it->second.wait_for(std::chrono::seconds(0));
            if (status == std::future_status::ready) {
                readyResults.push_back(it->second.get());
                it = _futureTileIOResults.erase(it);
            }
            else {
                it++;
            }
        }
        return readyResults;
    }
   

    bool AsyncTileDataProvider::satisfiesEnqueueCriteria(const ChunkIndex& chunkIndex) const {
        // only allow tile to be enqueued if it's not already enqueued
        return _futureTileIOResults.find(chunkIndex.hashKey()) == _futureTileIOResults.end();
    }

    void AsyncTileDataProvider::reset() {
        _futureTileIOResults.clear();
        _threadPool->stop(ghoul::ThreadPool::RunRemainingTasks::No);
        _threadPool->start();
        getTextureDataProvider()->reset();
    }

    void AsyncTileDataProvider::clearRequestQueue() {
        _threadPool->clearRemainingTasks();
        _futureTileIOResults.clear();
    }

}  // namespace openspace
