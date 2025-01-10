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

#include <modules/iswa/rendering/texturecygnet.h>

#include <modules/iswa/util/iswamanager.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr std::string_view _loggerCat = "TextureCygnet";
} // namespace

namespace openspace {

TextureCygnet::TextureCygnet(const ghoul::Dictionary& dictionary)
    : IswaCygnet(dictionary)
{
    registerProperties();
}

bool TextureCygnet::updateTexture() {
    using namespace ghoul;

    std::unique_ptr<opengl::Texture> texture = io::TextureReader::ref().loadTexture(
        reinterpret_cast<void*>(_imageFile.buffer),
        _imageFile.size,
        2,
        _imageFile.format
    );

    if (texture) {
        LDEBUG(std::format(
            "Loaded texture from image iswa cygnet with id '{}'", _data.id
        ));
        texture->uploadTexture();
        // Textures of planets looks much smoother with AnisotropicMipMap
        texture->setFilter(opengl::Texture::FilterMode::LinearMipMap);
        _textures[0] = std::move(texture);
    }

    return false;
}

bool TextureCygnet::downloadTextureResource(double timestamp) {
    if (_futureObject.valid()) {
        return false;
    }

    if (_textures.empty()) {
        _textures.push_back(nullptr);
    }

    std::future<DownloadManager::MemoryFile> future = IswaManager::ref().fetchImageCygnet(
        _data.id,
        timestamp
    );

    if (future.valid()) {
        _futureObject = std::move(future);
        return true;
    }

    return false;
}

bool TextureCygnet::updateTextureResource() {
    // if The future is done then get the new imageFile
    DownloadManager::MemoryFile imageFile;
    if (_futureObject.valid() && DownloadManager::futureReady(_futureObject)) {
        imageFile = _futureObject.get();

        if (imageFile.corrupted) {
            delete[] imageFile.buffer;
            return false;
        }
        else {
            _imageFile = imageFile;
        }
    }
    else {
        return false;
    }

    return true;
}

bool TextureCygnet::readyToRender() const {
    return isReady() && ((!_textures.empty()) && _textures[0]);
}

} //namespace openspace
