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

#include <modules/globebrowsing/tile/tilediskcache.h>

#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/tile.h>

#include <ghoul/filesystem/filesystem.h>

#include <fstream>

using namespace ghoul::filesystem;

namespace openspace {
namespace globebrowsing {

const std::string TileDiskCache::CACHE_ROOT = "tilecache";

TileDiskCache::TileDiskCache(const std::string& name)
    : _name(name)
{
    std::string pathToCacheDir = FileSys.pathByAppendingComponent(CACHE_ROOT, name);
    Directory cacheDir(pathToCacheDir, Directory::RawPath::No);
    if (!FileSys.directoryExists(cacheDir)) {
        FileSys.createDirectory(pathToCacheDir, FileSystem::Recursive::Yes);
    }
    _cacheDir = cacheDir;
}

bool TileDiskCache::has(const TileIndex& tileIndex) const {
    File metaFile = getMetaDataFile(tileIndex);
    return FileSys.fileExists(metaFile);
}

std::shared_ptr<RawTile> TileDiskCache::get(const TileIndex& tileIndex) {
    File metaDataFile = getMetaDataFile(tileIndex);
    File dataFile = getDataFile(tileIndex);
    if (FileSys.fileExists(metaDataFile) && FileSys.fileExists(dataFile)) {
        // read meta
        std::ifstream ifsMeta;
        ifsMeta.open(metaDataFile.path(), std::ifstream::in);
        RawTile res = RawTile::deserializeMetaData(ifsMeta);
        ifsMeta.close();

        // read data
        std::ifstream ifsData;
        ifsData.open(dataFile.path(), std::ifstream::binary);
        char * buffer = new char[res.nBytesImageData];
        ifsData.read(buffer, res.nBytesImageData);
        res.imageData = buffer;

        return std::make_shared<RawTile>(res);
    }
    return nullptr;
}

bool TileDiskCache::put(const TileIndex& tileIndex, std::shared_ptr<RawTile> rawTile) {
    File metaDataFile = getMetaDataFile(tileIndex);
    if (!FileSys.fileExists(metaDataFile)) {
        std::ofstream ofsMeta;
        ofsMeta.open(metaDataFile.path());
        rawTile->serializeMetaData(ofsMeta);
        ofsMeta.close();

        std::ofstream ofsData;
        File dataFile = getDataFile(tileIndex);
        ofsData.open(dataFile.path(), std::ofstream::binary);
        char* data = (char*)rawTile->imageData;
        ofsData.write(data, rawTile->nBytesImageData);
        ofsData.close();
        return true;
    }
    return false;
}

std::string TileDiskCache::getFilePath(const TileIndex& tileIndex) const {
    std::stringstream ss;
    ss << tileIndex.level;
    ss << "_" << tileIndex.x;
    ss << "_" << tileIndex.y;
    std::string filePath = FileSys.pathByAppendingComponent(_cacheDir.path(), ss.str());
    return filePath;
}

File TileDiskCache::getMetaDataFile(const TileIndex& tileIndex) const {
    return File(getFilePath(tileIndex) + ".meta");
}

File TileDiskCache::getDataFile(const TileIndex& tileIndex) const {
    return File(getFilePath(tileIndex) + ".data");
}

} // namespace globebrowsing
}  // namespace openspace
