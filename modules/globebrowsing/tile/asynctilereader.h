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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATA_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATA_PROVIDER___H__

#include <modules/globebrowsing/other/concurrentjobmanager.h>
#include <modules/globebrowsing/tile/tile.h>

#include <unordered_map>

namespace openspace {
namespace globebrowsing {

class TileDataset;

struct LoadJob : public Job<RawTile> {
    virtual void execute() = 0;
    virtual std::shared_ptr<RawTile> product() const = 0;
};

struct TileLoadJob : LoadJob {
    TileLoadJob(std::shared_ptr<TileDataset> textureDataProvider, 
        const TileIndex& tileIndex)
        : _tileDataset(textureDataProvider)
        , _chunkIndex(tileIndex) 
    {}

    virtual ~TileLoadJob() = default;

    virtual void execute() override;

    virtual std::shared_ptr<RawTile> product() const override;

protected:
    TileIndex _chunkIndex;
    std::shared_ptr<TileDataset> _tileDataset;
    std::shared_ptr<RawTile> _rawTile;
};

class TileDiskCache;

struct DiskCachedTileLoadJob : public TileLoadJob {
    enum CacheMode {
        Disabled,
        ReadOnly,
        ReadAndWrite,
        WriteOnly,
        CacheHitsOnly,
    };
        
    DiskCachedTileLoadJob(std::shared_ptr<TileDataset> textureDataProvider, 
        const TileIndex& tileIndex, std::shared_ptr<TileDiskCache> tdc, 
        CacheMode cacheMode = CacheMode::ReadOnly);

    void execute() override;

protected:
    std::shared_ptr<TileDiskCache> _tileDiskCache;
    CacheMode _mode;
};

class AsyncTileDataProvider {
public:
    AsyncTileDataProvider(std::shared_ptr<TileDataset> textureDataProvider, 
        std::shared_ptr<ThreadPool> pool);

    bool enqueueTileIO(const TileIndex& tileIndex);        
    std::vector<std::shared_ptr<RawTile>> getRawTiles();
        
    void reset();
    void clearRequestQueue();

    std::shared_ptr<TileDataset> getTextureDataProvider() const;
    float noDataValueAsFloat();

protected:
    virtual bool satisfiesEnqueueCriteria(const TileIndex&) const;

private:
    std::shared_ptr<TileDataset> _tileDataset;
    ConcurrentJobManager<RawTile> _concurrentJobManager;
    std::unordered_map<TileHashKey, TileIndex> _enqueuedTileRequests;
};

} // namespace globebrowsing
} // namespace openspace

#endif  // __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATA_PROVIDER___H__
