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

#include <modules/globebrowsing/tile/tileloadjob.h>

#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>

namespace openspace::globebrowsing {

TileLoadJob::TileLoadJob(std::shared_ptr<RawTileDataReader> rawTileDataReader,
                         const TileIndex& tileIndex)
    : _rawTileDataReader(std::move(rawTileDataReader))
    , _chunkIndex(tileIndex)
{}


TileLoadJob::TileLoadJob(std::shared_ptr<RawTileDataReader> rawTileDataReader,
                         const TileIndex& tileIndex, char* pboDataPtr)
    : _rawTileDataReader(std::move(rawTileDataReader))
    , _chunkIndex(tileIndex)
    , _pboMappedDataDestination(pboDataPtr)
{}

TileLoadJob::~TileLoadJob() {
    if (_hasOwnershipOfData) {
        ghoul_assert(_rawTile->imageData, "Image data must exist");

        delete[] _rawTile->imageData;
    }
}

void TileLoadJob::execute() {
    size_t numBytes = _rawTileDataReader->tileTextureInitData().totalNumBytes();
    char* dataPtr = nullptr;
    if (_rawTileDataReader->tileTextureInitData().shouldAllocateDataOnCPU() ||
        !_pboMappedDataDestination)
    {
        dataPtr = new char[numBytes];
        _hasOwnershipOfData = true;
    }
    _rawTile = _rawTileDataReader->readTileData(
        _chunkIndex,
        dataPtr,
        _pboMappedDataDestination
    );
}

std::shared_ptr<RawTile> TileLoadJob::product() {
    _hasOwnershipOfData = false;
    return _rawTile;
}

bool TileLoadJob::hasOwnershipOfData() const {
    return _hasOwnershipOfData;
}

} // namespace openspace::globebrowsing
