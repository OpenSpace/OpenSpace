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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATAPROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATAPROVIDER___H__

#include <modules/globebrowsing/other/prioritizingconcurrentjobmanager.h>
#include <modules/globebrowsing/other/pixelbuffercontainer.h>
#include <modules/globebrowsing/tile/tileindex.h>

#include <ghoul/opengl/ghoul_gl.h>

#include <map>
#include <set>
#include <unordered_map>

namespace openspace {
namespace globebrowsing {
    
struct RawTile;
class RawTileDataReader;

/**
 * The responsibility of this class is to enqueue tile requests and fetching finished
 * <code>RawTile</code>s that has been asynchronously loaded.
 */
class AsyncTileDataProvider {
public:
    enum class UsePBO {
        Yes,
        No
    };

    /**
     * \param textureDataProvider is the reader that will be used for the asynchronos
     * tile loading.
     */
    AsyncTileDataProvider(std::shared_ptr<RawTileDataReader> textureDataProvider,
        UsePBO usePbo = UsePBO::No);

    /**
     * Creates a job which asynchronously loads a raw tile. This job is enqueued.
     */
    bool enqueueTileIO(const TileIndex& tileIndex);

    /**
     * Get all finished jobs.
     */
    std::vector<std::shared_ptr<RawTile>> getRawTiles();

    /**
     * Get one finished job.
     */
    std::shared_ptr<RawTile> popFinishedRawTile();

    void update();
    void reset();

    std::shared_ptr<RawTileDataReader> getRawTileDataReader() const;
    float noDataValueAsFloat() const;

protected:
    /**
     * \returns true if tile of index <code>tileIndex</code> is not already enqueued.
     */
    bool satisfiesEnqueueCriteria(const TileIndex& tileIndex);

    /**
     * An unfinished job is a load tile job that has been popped from the thread pool due
     * to its low priority. Once it has been popped, it is marked as unfinished and needs
     * to be explicitly ended.
     */
    void endUnfinishedJobs();

private:
    /// The reader used for asynchronous reading
    std::shared_ptr<RawTileDataReader> _rawTileDataReader;
    
    PrioritizingConcurrentJobManager<RawTile, TileIndex::TileHashKey>
        _concurrentJobManager;

    /// nullptr if pbo is not used for texture uploading. Otherwise initialized.
    std::unique_ptr<PixelBufferContainer<TileIndex::TileHashKey>> _pboContainer;
    std::set<TileIndex::TileHashKey> _enqueuedTileRequests;

    bool _shouldReset;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATAPROVIDER___H__
