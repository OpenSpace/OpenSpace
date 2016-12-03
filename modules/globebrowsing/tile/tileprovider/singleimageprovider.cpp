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
#include <modules/globebrowsing/tile/tileindex.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <openspace/engine/openspaceengine.h>

namespace {
    const std::string _loggerCat = "SingleImageProvider";

    const char* KeyFilePath = "FilePath";
}

namespace openspace {
namespace globebrowsing {

    SingleImageProvider::SingleImageProvider(const ghoul::Dictionary& dictionary) {
        // Required input
        if (!dictionary.getValue<std::string>(KeyFilePath, _imagePath)) {
            throw std::runtime_error(std::string("Must define key '") + KeyFilePath + "'");
        }

        reset();
    }

    SingleImageProvider::SingleImageProvider(const std::string& imagePath)
        : _imagePath(imagePath)
    {
        reset();
    }

    Tile SingleImageProvider::getTile(const TileIndex& tileIndex) {
        return _tile;
    }

    Tile SingleImageProvider::getDefaultTile() {
        return _tile;
    }

    Tile::Status SingleImageProvider::getTileStatus(const TileIndex& index) {
        return _tile.status;
    }

    TileDepthTransform SingleImageProvider::depthTransform() {
        TileDepthTransform transform;
        transform.depthOffset = 0.0f;
        transform.depthScale = 1.0f;
        return transform;
    }

    void SingleImageProvider::update() {
        // nothing to be done
    }

    void SingleImageProvider::reset() {
        _tile = Tile();
        _tile.texture = std::shared_ptr<Texture>(ghoul::io::TextureReader::ref().loadTexture(_imagePath).release());
        _tile.status = _tile.texture != nullptr ? Tile::Status::OK : Tile::Status::IOError;
        _tile.metaData = nullptr;

        _tile.texture->uploadTexture();
        _tile.texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    }

    int SingleImageProvider::maxLevel() {
        return 1337; // unlimited
    }

} // namespace globebrowsing
} // namespace openspace
