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
} // namespace

namespace openspace::globebrowsing::tileprovider {
    
SingleImageProvider::SingleImageProvider(const ghoul::Dictionary& dictionary)
    : _tile(nullptr, nullptr, Tile::Status::Unavailable)
    , _filePath("filePath", "File Path", "")
{
    // Required input
    std::string filePath;
    dictionary.getValue<std::string>(KeyFilePath, filePath);
    _filePath.setValue(filePath);

    addProperty(_filePath);

    reset();
}

SingleImageProvider::SingleImageProvider(const std::string& imagePath)
    : _tile(nullptr, nullptr, Tile::Status::Unavailable)
    , _filePath("filePath", "File Path", imagePath)
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
    if (_filePath.value().empty()) {
        return;
    }
    _tileTexture = ghoul::io::TextureReader::ref().loadTexture(_filePath);
    Tile::Status tileStatus = _tileTexture ? Tile::Status::OK : Tile::Status::IOError;

    if (!_tileTexture) {
        throw std::runtime_error(std::string("Unable to load texture '")
            + _filePath.value() + "'");
    }
 
    _tileTexture->uploadTexture();
    _tileTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

    _tile = Tile(_tileTexture.get(), nullptr, tileStatus);
}

int SingleImageProvider::maxLevel() {
    return 1337; // unlimited
}

} // namespace openspace::globebrowsing::tileprovider
