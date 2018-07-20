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

#include <modules/globebrowsing/tile/asynctiledataprovider.h>

#include <modules/globebrowsing/cache/memoryawaretilecache.h>
#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/other/pixelbuffercontainer.h>
#include <modules/globebrowsing/tile/tileloadjob.h>
#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace::globebrowsing {

namespace {
    constexpr const char* _loggerCat = "AsyncTileDataProvider";
} // namespace

AsyncTileDataProvider::AsyncTileDataProvider(std::string name,
                               const std::shared_ptr<RawTileDataReader> rawTileDataReader)
    : _name(std::move(name))
    , _rawTileDataReader(std::move(rawTileDataReader))
    , _concurrentJobManager(LRUThreadPool<TileIndex::TileHashKey>(1, 10))
{
    _globeBrowsingModule = global::moduleEngine.module<GlobeBrowsingModule>();
    performReset(ResetRawTileDataReader::No);
}

AsyncTileDataProvider::~AsyncTileDataProvider() {} // NOLINT

std::shared_ptr<RawTileDataReader> AsyncTileDataProvider::rawTileDataReader() const {
    return _rawTileDataReader;
}

bool AsyncTileDataProvider::enqueueTileIO(const TileIndex& tileIndex) {
    if (_resetMode == ResetMode::ShouldNotReset && satisfiesEnqueueCriteria(tileIndex)) {
        if (_pboContainer) {
            char* dataPtr = static_cast<char*>(_pboContainer->mapBuffer(
                tileIndex.hashKey(), PixelBuffer::Access::WriteOnly));
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
            auto job = std::make_shared<TileLoadJob>(_rawTileDataReader, tileIndex);
            _concurrentJobManager.enqueueJob(job, tileIndex.hashKey());
            _enqueuedTileRequests.insert(tileIndex.hashKey());
        }
        return true;
    }
    return false;
}

std::vector<std::shared_ptr<RawTile>> AsyncTileDataProvider::rawTiles() {
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

        const TileIndex::TileHashKey key = product->tileIndex.hashKey();
        // No longer enqueued. Remove from set of enqueued tiles
        _enqueuedTileRequests.erase(key);
        // Pbo is still mapped. Set the id for the raw tile
        if (_pboContainer) {
            product->pbo = _pboContainer->idOfMappedBuffer(key);
            // Now we are finished with the mapping of this pbo
            _pboContainer->unMapBuffer(key);
        }
        else {
            product->pbo = 0;
            if (product->error != RawTile::ReadError::None) {
                delete[] product->imageData;
                return nullptr;
            }
        }

        return product;
    }
    else {
        return nullptr;
    }
}

bool AsyncTileDataProvider::satisfiesEnqueueCriteria(const TileIndex& tileIndex) {
    // Only satisfies if it is not already enqueued. Also bumps the request to the top.
    const bool alreadyEnqueued = _concurrentJobManager.touch(tileIndex.hashKey());

    // Concurrent job manager can start jobs which will pop them from enqueued, however
    // they are still in _enqueuedTileRequests until finished
    const auto it = _enqueuedTileRequests.find(tileIndex.hashKey());
    const bool notFoundAmongEnqueued = it == _enqueuedTileRequests.end();

    return !alreadyEnqueued && notFoundAmongEnqueued;
}

void AsyncTileDataProvider::endUnfinishedJobs() {
    std::vector<TileIndex::TileHashKey> unfinishedJobs =
        _concurrentJobManager.keysToUnfinishedJobs();
    for (const TileIndex::TileHashKey& unfinishedJob : unfinishedJobs) {
        // Unmap unfinished jobs
        if (_pboContainer) {
            _pboContainer->unMapBuffer(unfinishedJob);
        }
        // When erasing the job before
        _enqueuedTileRequests.erase(unfinishedJob);
    }
}

void AsyncTileDataProvider::endEnqueuedJobs() {
    std::vector<TileIndex::TileHashKey> enqueuedJobs =
        _concurrentJobManager.keysToEnqueuedJobs();
    for (const TileIndex::TileHashKey& enqueuedJob : enqueuedJobs) {
        // Unmap unfinished jobs
        if (_pboContainer) {
            _pboContainer->unMapBuffer(enqueuedJob);
        }
        // When erasing the job before
        _enqueuedTileRequests.erase(enqueuedJob);
    }
}

void AsyncTileDataProvider::updatePboUsage() {
    const bool usingPbo = _pboContainer != nullptr;
    const bool shouldUsePbo = _globeBrowsingModule->tileCache()->shouldUsePbo();

    // If changed, we need to reset the async tile data provider.
    // No need to reset the raw tile data reader when changing PBO usage.
    if (usingPbo != shouldUsePbo &&
        _resetMode != ResetMode::ShouldResetAllButRawTileDataReader)
    {
        _resetMode = ResetMode::ShouldResetAllButRawTileDataReader;
        LINFO(fmt::format(
            "PBO usage updated, prepairing for resetting of tile reader '{}'", _name
        ));
    }
}

void AsyncTileDataProvider::update() {
    endUnfinishedJobs();

    // May reset
    switch (_resetMode) {
        case ResetMode::ShouldResetAll: {
            // Clean all finished jobs
            rawTiles();
            // Only allow resetting if there are no jobs currently running
            if (_enqueuedTileRequests.empty()) {
                performReset(ResetRawTileDataReader::Yes);
                LINFO(fmt::format("Tile data reader '{}' reset successfully", _name));
            }
            break;
        }
        case ResetMode::ShouldResetAllButRawTileDataReader: {
            // Clean all finished jobs
            rawTiles();
            // Only allow resetting if there are no jobs currently running
            if (_enqueuedTileRequests.empty()) {
                performReset(ResetRawTileDataReader::No);
                LINFO(fmt::format("Tile data reader '{}' reset successfully", _name));
            }
            break;
        }
        case ResetMode::ShouldBeDeleted: {
            // Clean all finished jobs
            rawTiles();
            // Only allow resetting if there are no jobs currently running
            if (_enqueuedTileRequests.empty()) {
                _shouldBeDeleted = true;
            }
            break;
        }
        case ResetMode::ShouldNotReset: {
            updatePboUsage();
            break;
        }
        default:
            break;
    }
}

void AsyncTileDataProvider::reset() {
    // Can not clear concurrent job manager in case there are threads running. therefore
    // we need to wait until _enqueuedTileRequests is empty before finishing up.
    _resetMode = ResetMode::ShouldResetAll;
    endEnqueuedJobs();
    LINFO(fmt::format("Prepairing for resetting of tile reader '{}'", _name));
}

void AsyncTileDataProvider::prepairToBeDeleted() {
    _resetMode = ResetMode::ShouldBeDeleted;
    endEnqueuedJobs();
}

bool AsyncTileDataProvider::shouldBeDeleted() {
    return _shouldBeDeleted;
}

void AsyncTileDataProvider::performReset(ResetRawTileDataReader resetRawTileDataReader) {
    ghoul_assert(_enqueuedTileRequests.empty(), "No enqueued requests left");

    // Re-initialize PBO container
    if (_globeBrowsingModule->tileCache()->shouldUsePbo()) {
        size_t pboNumBytes = _rawTileDataReader->tileTextureInitData().totalNumBytes();
        _pboContainer = std::make_unique<PixelBufferContainer<TileIndex::TileHashKey>>(
            pboNumBytes,
            PixelBuffer::Usage::StreamDraw,
            10
        );
    }
    else {
        _pboContainer = nullptr;
    }

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
