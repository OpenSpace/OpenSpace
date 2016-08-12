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

#include <modules/globebrowsing/geometry/geodetic2.h>

#include <modules/globebrowsing/tile/tileprovider/singleimageprovider.h>
#include <modules/globebrowsing/chunk/chunkindex.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>


#include <openspace/engine/openspaceengine.h>



namespace {
    const std::string _loggerCat = "SingleImageProvider";
}


namespace openspace {



    SingleImagePrivoder::SingleImagePrivoder(const std::string& imagePath)
        : _imagePath(imagePath)
    {
        reset();
    }

    Tile SingleImagePrivoder::getTile(const ChunkIndex& chunkIndex) {
        return _tile;
    }

    Tile SingleImagePrivoder::getDefaultTile() {
        return _tile;
    }


    Tile::Status SingleImagePrivoder::getTileStatus(const ChunkIndex& index) {
        return _tile.status;
    }

    TileDepthTransform SingleImagePrivoder::depthTransform() {
        TileDepthTransform transform;
        transform.depthOffset = 0.0f;
        transform.depthScale = 1.0f;
        return transform;
    }

    void SingleImagePrivoder::update() {
        // nothing to be done
    }

    void SingleImagePrivoder::reset() {
        _tile = Tile();
        _tile.texture = std::shared_ptr<Texture>(ghoul::io::TextureReader::ref().loadTexture(_imagePath).release());
        _tile.status = _tile.texture != nullptr ? Tile::Status::OK : Tile::Status::IOError;
        _tile.preprocessData = nullptr;

        _tile.texture->uploadTexture();
        _tile.texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    }

    int SingleImagePrivoder::maxLevel() {
        return 1337; // unlimited
    }

}  // namespace openspace
