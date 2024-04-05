/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/globebrowsing/src/asynctiledataprovider.h>

#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <modules/globebrowsing/src/rawtiledatareader.h>
#include <modules/globebrowsing/src/tileloadjob.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace::globebrowsing {

namespace {
    constexpr std::string_view _loggerCat = "AsyncTileDataProvider";
} // namespace

AsyncTileDataProvider::AsyncTileDataProvider(std::string name,
                                    std::unique_ptr<RawTileDataReader> rawTileDataReader)
    : _name(std::move(name))
    , _rawTileDataReader(std::move(rawTileDataReader))
    , _concurrentJobManager(LRUThreadPool<TileIndex::TileHashKey>(1, 10))
{
    ZoneScoped;

    performReset(ResetRawTileDataReader::No);
}

const RawTileDataReader& AsyncTileDataProvider::rawTileDataReader() const {
    return *_rawTileDataReader;
}

bool AsyncTileDataProvider::enqueueTileIO(const TileIndex& tileIndex) {
    ZoneScoped;

    if (_resetMode == ResetMode::ShouldNotReset && satisfiesEnqueueCriteria(tileIndex)) {
        auto job = std::make_unique<TileLoadJob>(*_rawTileDataReader, tileIndex);
        _concurrentJobManager.enqueueJob(std::move(job), tileIndex.hashKey());
        _enqueuedTileRequests.insert(tileIndex.hashKey());
        return true;
    }
    return false;
}

void AsyncTileDataProvider::clearTiles() {
    std::optional<RawTile> finishedJob = popFinishedRawTile();
    while (finishedJob) {
        finishedJob = popFinishedRawTile();
    }
}

std::optional<RawTile> AsyncTileDataProvider::popFinishedRawTile() {
    if (_concurrentJobManager.numFinishedJobs() > 0) {
        // Now the tile load job looses ownerwhip of the data pointer
        RawTile product = _concurrentJobManager.popFinishedJob()->product();

        const TileIndex::TileHashKey key = product.tileIndex.hashKey();
        // No longer enqueued. Remove from set of enqueued tiles
        _enqueuedTileRequests.erase(key);
        // Pbo is still mapped. Set the id for the raw tile
        if (product.error != RawTile::ReadError::None) {
            product.imageData = nullptr;
            return std::nullopt;
        }

        return product;
    }
    else {
        return std::nullopt;
    }
}

bool AsyncTileDataProvider::satisfiesEnqueueCriteria(const TileIndex& tileIndex) {
    ZoneScoped;

    // Only satisfies if it is not already enqueued. Also bumps the request to the top.
    const bool alreadyEnqueued = _concurrentJobManager.touch(tileIndex.hashKey());
    // Early out so we don't need to check the already enqueued requests
    if (alreadyEnqueued) {
        return false;
    }

    // Concurrent job manager can start jobs which will pop them from enqueued, however
    // they are still in _enqueuedTileRequests until finished
    const auto it = _enqueuedTileRequests.find(tileIndex.hashKey());
    const bool notFoundAmongEnqueued = it == _enqueuedTileRequests.end();

    return !alreadyEnqueued && notFoundAmongEnqueued;
}

void AsyncTileDataProvider::endUnfinishedJobs() {
    const std::vector<TileIndex::TileHashKey> unfinishedJobs =
        _concurrentJobManager.keysToUnfinishedJobs();
    for (const TileIndex::TileHashKey& unfinishedJob : unfinishedJobs) {
        // When erasing the job before
        _enqueuedTileRequests.erase(unfinishedJob);
    }
}

void AsyncTileDataProvider::endEnqueuedJobs() {
    const std::vector<TileIndex::TileHashKey> enqueuedJobs =
        _concurrentJobManager.keysToEnqueuedJobs();
    for (const TileIndex::TileHashKey& enqueuedJob : enqueuedJobs) {
        // When erasing the job before
        _enqueuedTileRequests.erase(enqueuedJob);
    }
}

void AsyncTileDataProvider::update() {
    endUnfinishedJobs();

    // May reset
    switch (_resetMode) {
        case ResetMode::ShouldResetAll:
            // Clean all finished jobs
            clearTiles();
            // Only allow resetting if there are no jobs currently running
            if (_enqueuedTileRequests.empty()) {
                performReset(ResetRawTileDataReader::Yes);
                LINFO(std::format("Tile data reader '{}' reset successfully", _name));
            }
            break;
        case ResetMode::ShouldResetAllButRawTileDataReader:
            // Clean all finished jobs
            clearTiles();
            // Only allow resetting if there are no jobs currently running
            if (_enqueuedTileRequests.empty()) {
                performReset(ResetRawTileDataReader::No);
                LINFO(std::format("Tile data reader '{}' reset successfully", _name));
            }
            break;
        case ResetMode::ShouldBeDeleted:
            // Clean all finished jobs
            clearTiles();
            // Only allow resetting if there are no jobs currently running
            if (_enqueuedTileRequests.empty()) {
                _shouldBeDeleted = true;
            }
            break;
        case ResetMode::ShouldNotReset:
        default:
            break;
    }
}

void AsyncTileDataProvider::reset() {
    // Can not clear concurrent job manager in case there are threads running. therefore
    // we need to wait until _enqueuedTileRequests is empty before finishing up.
    _resetMode = ResetMode::ShouldResetAll;
    endEnqueuedJobs();
    LINFO(std::format("Prepairing for resetting of tile reader '{}'", _name));
}

void AsyncTileDataProvider::prepareToBeDeleted() {
    _resetMode = ResetMode::ShouldBeDeleted;
    endEnqueuedJobs();
}

bool AsyncTileDataProvider::shouldBeDeleted() const {
    return _shouldBeDeleted;
}

void AsyncTileDataProvider::performReset(ResetRawTileDataReader resetRawTileDataReader) {
    ZoneScoped;

    ghoul_assert(_enqueuedTileRequests.empty(), "No enqueued requests left");

    // Reset raw tile data reader
    if (resetRawTileDataReader == ResetRawTileDataReader::Yes) {
        _rawTileDataReader->reset();
    }

    // Finished resetting
    _resetMode = ResetMode::ShouldNotReset;
}

float AsyncTileDataProvider::noDataValueAsFloat() const {
    return _rawTileDataReader->noDataValueAsFloat();
}

} // namespace openspace::globebrowsing
