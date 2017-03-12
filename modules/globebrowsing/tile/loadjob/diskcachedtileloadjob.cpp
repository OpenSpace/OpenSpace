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

#include <modules/globebrowsing/tile/loadjob/diskcachedtileloadjob.h>

#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>
#include <modules/globebrowsing/tile/tilediskcache.h>

namespace openspace {
namespace globebrowsing {

DiskCachedTileLoadJob::DiskCachedTileLoadJob(
    std::shared_ptr<RawTileDataReader> textureDataProvider,
    const TileIndex& tileIndex, std::shared_ptr<TileDiskCache> tdc,
    CacheMode m)
    : TileLoadJob(textureDataProvider, tileIndex)
    , _tileDiskCache(tdc)
    , _mode(m)
{}

void DiskCachedTileLoadJob::execute() {
    _rawTile = nullptr;

    switch (_mode) {
        case CacheMode::Disabled: 
            _rawTile = _rawTileDataReader->readTileData(_chunkIndex);
            break;

        case CacheMode::ReadOnly:
            _rawTile = _tileDiskCache->get(_chunkIndex);
            if (_rawTile == nullptr) {
                _rawTile = _rawTileDataReader->readTileData(_chunkIndex);
            }
            break;

        case CacheMode::ReadAndWrite:
            _rawTile = _tileDiskCache->get(_chunkIndex);
            if (_rawTile == nullptr) {
                _rawTile = _rawTileDataReader->readTileData(_chunkIndex);
                _tileDiskCache->put(_chunkIndex, _rawTile);
            }
            break;

        case CacheMode::WriteOnly:
            _rawTile = _rawTileDataReader->readTileData(_chunkIndex);
            _tileDiskCache->put(_chunkIndex, _rawTile);
            break;

        case CacheMode::CacheHitsOnly:
            _rawTile = _tileDiskCache->get(_chunkIndex);
            if (_rawTile == nullptr) {
                RawTile res = RawTile::createDefaultRes();
                res.tileIndex = _chunkIndex;
                _rawTile = std::make_shared<RawTile>(res);
            }
            break;
    }
}

} // namespace globebrowsing
} // namespace openspace
