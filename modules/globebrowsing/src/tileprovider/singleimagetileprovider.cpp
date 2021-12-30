/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <ghoul/io/texture/texturereader.h>

namespace {
    constexpr const char* KeyFilePath = "FilePath";
    
    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "The file path that is used for this image provider. The file must point to an "
        "image that is then loaded and used for all tiles."
    };
} // namespace

namespace openspace::globebrowsing {

SingleImageProvider::SingleImageProvider(const ghoul::Dictionary& dictionary)
    : filePath(FilePathInfo)
{
    ZoneScoped

    filePath = dictionary.value<std::string>(KeyFilePath);
    addProperty(filePath);

    reset();
}

Tile SingleImageProvider::tile(const TileIndex&) {
    ZoneScoped
    return ttile;
}

Tile::Status SingleImageProvider::tileStatus(const TileIndex&) {
    return ttile.status;
}

TileDepthTransform SingleImageProvider::depthTransform() {
    return { 0.f, 1.f };
}

void SingleImageProvider::update() {}

void SingleImageProvider::reset() {
    if (filePath.value().empty()) {
        return;
    }
    tileTexture = ghoul::io::TextureReader::ref().loadTexture(filePath, 2);
    if (!tileTexture) {
        throw ghoul::RuntimeError(
            fmt::format("Unable to load texture '{}'", filePath.value())
        );
    }
    Tile::Status tileStatus = Tile::Status::OK;

    tileTexture->uploadTexture();
    tileTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
    ttile = Tile{ tileTexture.get(), std::nullopt, tileStatus };
}

int SingleImageProvider::maxLevel() {
    return 1337; // unlimited
}

float SingleImageProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
