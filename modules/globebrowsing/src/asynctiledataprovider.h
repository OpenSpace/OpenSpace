/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/globebrowsing/src/prioritizingconcurrentjobmanager.h>
#include <modules/globebrowsing/src/rawtiledatareader.h>
#include <modules/globebrowsing/src/tileindex.h>
#include <ghoul/misc/boolean.h>
#include <map>
#include <optional>
#include <set>

namespace openspace::globebrowsing {

struct RawTile;

/**
 * The responsibility of this class is to enqueue tile requests and fetching finished
 * `RawTile`s that has been asynchronously loaded.
 */
class AsyncTileDataProvider {
public:
    /**
     * \param name is the name for this provider
     * \param rawTileDataReader is the reader that will be used for the asynchronous tile
     *        loading
     */
    AsyncTileDataProvider(std::string name,
        std::unique_ptr<RawTileDataReader> rawTileDataReader);

    /**
     * Creates a job which asynchronously loads a raw tile. This job is enqueued.
     */
    bool enqueueTileIO(const TileIndex& tileIndex);

    /**
     * Get one finished job.
     */
    std::optional<RawTile> popFinishedRawTile();

    void update();
    void reset();
    void prepareToBeDeleted();

    bool shouldBeDeleted() const;

    const RawTileDataReader& rawTileDataReader() const;
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
     * \return `true` if tile of index \p tileIndex is not already enqueued
     */
    bool satisfiesEnqueueCriteria(const TileIndex& tileIndex);

    /**
     * An unfinished job is a load tile job that has been popped from the thread pool due
     * to its low priority. Once it has been popped, it is marked as unfinished and needs
     * to be explicitly ended.
     */
    void endUnfinishedJobs();

    void clearTiles();

    void endEnqueuedJobs();

    void performReset(ResetRawTileDataReader resetRawTileDataReader);

private:
    const std::string _name;
    /// The reader used for asynchronous reading
    std::unique_ptr<RawTileDataReader> _rawTileDataReader;

    PrioritizingConcurrentJobManager<RawTile, TileIndex::TileHashKey>
        _concurrentJobManager;

    std::set<TileIndex::TileHashKey> _enqueuedTileRequests;

    ResetMode _resetMode = ResetMode::ShouldResetAllButRawTileDataReader;
    bool _shouldBeDeleted = false;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_TILE_DATAPROVIDER___H__
