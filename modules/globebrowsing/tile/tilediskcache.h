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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING_TILE_DISK_CACHE_H__
#define __OPENSPACE_MODULE_GLOBEBROWSING_TILE_DISK_CACHE_H__

#include <ghoul/filesystem/directory.h>
#include <ghoul/filesystem/file.h>

#include <memory>

namespace openspace {
namespace globebrowsing {

struct TileIndex;
struct RawTile;

class TileDiskCache {
public:
    TileDiskCache(const std::string& name);
        
    std::shared_ptr<RawTile> get(const TileIndex& tileIndex);
    bool has(const TileIndex& tileIndex) const;
    bool put(const TileIndex& tileIndex, std::shared_ptr<RawTile> rawTile);
        
    static const std::string CACHE_ROOT;
    
private:
    const std::string _name;
        
    ghoul::filesystem::Directory _cacheDir;
        
    std::string getFilePath(const TileIndex& tileIndex) const;
    ghoul::filesystem::File getMetaDataFile(const TileIndex& tileIndex) const;
    ghoul::filesystem::File getDataFile(const TileIndex& tileIndex) const;

};

} // namespace globebrowsing
} // namespace openspace

#endif  // __OPENSPACE_MODULE_GLOBEBROWSING_TILE_DISK_CACHE_H__
