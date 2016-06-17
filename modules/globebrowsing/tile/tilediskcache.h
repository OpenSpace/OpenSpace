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

#ifndef __TILE_DISK_CACHE_H__
#define __TILE_DISK_CACHE_H__


#include <modules/globebrowsing/chunk/chunkindex.h>
#include <modules/globebrowsing/tile/tileprovider.h>

#include <ghoul/filesystem/filesystem>


namespace openspace {

    using namespace ghoul::filesystem;

    class TileDiskCache {
    public:
        TileDiskCache(const std::string& name);
        
        std::shared_ptr<TileIOResult> get(const ChunkIndex& chunkIndex);
        bool has(const ChunkIndex& chunkIndex) const;
        bool TileDiskCache::put(const ChunkIndex& chunkIndex, std::shared_ptr<TileIOResult> tileIOResult);

        
        static const std::string CACHE_ROOT;
    
    private:
        const std::string _name;
        
        Directory _cacheDir;
        
        std::string getFilePath(const ChunkIndex& chunkIndex) const;
        File getMetaFile(const ChunkIndex& chunkIndex) const;
        File getDataFile(const ChunkIndex& chunkIndex) const;

    };

}  // namespace openspace


#endif  // __TILE_DISK_CACHE_H__