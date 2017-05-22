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

#include <modules/globebrowsing/tile/tileprovider/singleimageprovider.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/texture.h>

namespace {
    const char* KeyFilePath = "FilePath";
}

namespace openspace {
namespace globebrowsing {
namespace tileprovider {
    
SingleImageProvider::SingleImageProvider(const ghoul::Dictionary& dictionary)
    : _tile(nullptr, nullptr, Tile::Status::Unavailable)
{
    // Required input
    if (!dictionary.getValue<std::string>(KeyFilePath, _imagePath)) {
        throw std::runtime_error(std::string("Must define key '") + KeyFilePath + "'");
    }

    reset();
}

SingleImageProvider::SingleImageProvider(const std::string& imagePath)
    : _imagePath(imagePath)
    , _tile(nullptr, nullptr, Tile::Status::Unavailable)
{
    reset();
}

Tile SingleImageProvider::getTile(const TileIndex& tileIndex) {
    return _tile;
}

Tile::Status SingleImageProvider::getTileStatus(const TileIndex& index) {
    return _tile.status();
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
    _tileTexture = ghoul::io::TextureReader::ref().loadTexture(_imagePath);
    Tile::Status tileStatus = _tileTexture ? Tile::Status::OK : Tile::Status::IOError;

    if (!_tileTexture) {
        throw std::runtime_error(std::string("Unable to load texture '")
            + _imagePath + "'");
    }
 
    _tileTexture->uploadTexture();
    _tileTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

    _tile = Tile(_tileTexture.get(), nullptr, tileStatus);
}

int SingleImageProvider::maxLevel() {
    return 1337; // unlimited
}

} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace
