/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/globebrowsing/src/tileprovider/singleimagetileprovider.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/io/texture/texturereader.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "The file path that is used for this image provider. The file must point to an "
        "image that is then loaded and used for all tiles",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(SingleImageProvider)]] Parameters {
        // [[codegen::verbatim(FilePathInfo.description)]]
        std::string filePath;
    };
#include "singleimagetileprovider_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation SingleImageProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_singleimageprovider");
}

SingleImageProvider::SingleImageProvider(const ghoul::Dictionary& dictionary)
    : _filePath(FilePathInfo)
{
    ZoneScoped;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _filePath = p.filePath;
    addProperty(_filePath);

    reset();
}

Tile SingleImageProvider::tile(const TileIndex&) {
    ZoneScoped;
    return _tile;
}

Tile::Status SingleImageProvider::tileStatus(const TileIndex&) {
    return _tile.status;
}

TileDepthTransform SingleImageProvider::depthTransform() {
    return { 0.f, 1.f };
}

void SingleImageProvider::update() {}

void SingleImageProvider::reset() {
    if (_filePath.value().empty()) {
        return;
    }

    _tileTexture = ghoul::io::TextureReader::ref().loadTexture(_filePath, 2);
    if (!_tileTexture) {
        throw ghoul::RuntimeError(
            std::format("Unable to load texture '{}'", _filePath.value())
        );
    }

    _tileTexture->uploadTexture();
    _tileTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
    _tile = Tile{ _tileTexture.get(), std::nullopt, Tile::Status::OK };
}

int SingleImageProvider::minLevel() {
    return 1;
}

int SingleImageProvider::maxLevel() {
    return 1337; // unlimited
}

float SingleImageProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
