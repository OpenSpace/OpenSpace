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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILELOADJOB___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILELOADJOB___H__

#include <openspace/util/job.h>

#include <modules/globebrowsing/src/rawtile.h>
#include <modules/globebrowsing/src/tileindex.h>

namespace openspace::globebrowsing {

class RawTileDataReader;

struct TileLoadJob : public Job<RawTile> {
    /**
     * Allocates enough data for one tile. When calling #product, the ownership of this
     * data will be released. If `product()` has not been called before the TileLoadJob is
     * finished, the data will be deleted as it has not been exposed outside of this
     * object.
     */
    TileLoadJob(RawTileDataReader& rawTileDataReader, TileIndex tileIndex);

    /**
     * Destroys the allocated data pointer if it has been allocated and the TileLoadJob
     * has ownership of it.
     */
    ~TileLoadJob() override;

    /**
     * If the TileLoadJob has been created using PBO, this is the address that the
     * RawTileDataReader will read to. In case specified so in the TileTextureInitData
     * of RawTileDataReader, the data will also be written to CPU memory.
     */
    void execute() override;

    /**
     * Marks the job as finised and releases ownership of the data. Unless the job is
     * marked as finished, the pixel data will be deallocated when the job is deleted.
     */
    RawTile product() override;

protected:
    RawTileDataReader& _rawTileDataReader;
    RawTile _rawTile;
    const TileIndex _chunkIndex;
    bool _hasTile = false;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILELOADJOB___H__
