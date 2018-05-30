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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATAPROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATAPROVIDER___H__

#include <modules/globebrowsing/other/prioritizingconcurrentjobmanager.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <ghoul/misc/boolean.h>
#include <map>
#include <set>

namespace openspace { class GlobeBrowsingModule; }

namespace openspace::globebrowsing {

template <typename T> class PixelBufferContainer;

struct RawTile;
class RawTileDataReader;

/**
 * The responsibility of this class is to enqueue tile requests and fetching finished
 * <code>RawTile</code>s that has been asynchronously loaded.
 */
class AsyncTileDataProvider {
public:
    /**
     * \param rawTileDataReader is the reader that will be used for the asynchronous
     * tile loading.
     */
    AsyncTileDataProvider(std::string name,
        std::shared_ptr<RawTileDataReader> rawTileDataReader);

    ~AsyncTileDataProvider();

    /**
     * Creates a job which asynchronously loads a raw tile. This job is enqueued.
     */
    bool enqueueTileIO(const TileIndex& tileIndex);

    /**
     * Get all finished jobs.
     */
    std::vector<std::shared_ptr<RawTile>> rawTiles();

    /**
     * Get one finished job.
     */
    std::shared_ptr<RawTile> popFinishedRawTile();

    void update();
    void reset();
    void prepairToBeDeleted();

    bool shouldBeDeleted();

    std::shared_ptr<RawTileDataReader> rawTileDataReader() const;
    float noDataValueAsFloat() const;

protected:
    BooleanType(ResetRawTileDataReader);

    enum class ResetMode {
        ShouldResetAll,
        ShouldResetAllButRawTileDataReader,
        ShouldBeDeleted,
        ShouldNotReset
    };

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

    void endEnqueuedJobs();

    void updatePboUsage();

    void performReset(ResetRawTileDataReader resetRawTileDataReader);

private:
    const std::string _name;
    GlobeBrowsingModule* _globeBrowsingModule;
    /// The reader used for asynchronous reading
    std::shared_ptr<RawTileDataReader> _rawTileDataReader;

    PrioritizingConcurrentJobManager<RawTile, TileIndex::TileHashKey>
        _concurrentJobManager;

    /// nullptr if pbo is not used for texture uploading. Otherwise initialized.
    std::unique_ptr<PixelBufferContainer<TileIndex::TileHashKey>> _pboContainer;
    std::set<TileIndex::TileHashKey> _enqueuedTileRequests;

    ResetMode _resetMode = ResetMode::ShouldResetAllButRawTileDataReader;
    bool _shouldBeDeleted = false;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATAPROVIDER___H__
