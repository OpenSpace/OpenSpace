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

#include <openspace/rendering/texturecomponent.h>

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char _loggerCat[] = "TextureComponent";
} // namespace

namespace openspace {

const ghoul::opengl::Texture* TextureComponent::texture() const {
    return _texture.get();
}

ghoul::opengl::Texture* TextureComponent::texture() {
    return _texture.get();
}

void TextureComponent::setFilterMode(Texture::FilterMode filterMode) {
    _filterMode = filterMode;
}

void TextureComponent::setWrapping(Texture::WrappingMode wrapping) {
    _wrappingMode = wrapping;
}

void TextureComponent::setShouldWatchFileForChanges(bool value) {
    _shouldWatchFile = value;
}

void TextureComponent::setShouldPurgeFromRAM(bool value) {
    _shouldPurgeFromRAM = value;
}

void TextureComponent::bind() {
    ghoul_assert(_texture, "Texture must be loaded before binding");
    _texture->bind();
}

void TextureComponent::uploadToGpu() {
    if (!_texture) {
        LERROR("Could not upload texture to GPU. Texture not loaded");
        return;
    }
    _texture->uploadTexture();
    _texture->setFilter(_filterMode);
    _texture->setWrapping(_wrappingMode);
    if (_shouldPurgeFromRAM) {
        _texture->purgeFromRAM();
    }
}

void TextureComponent::loadFromFile(const std::string& path) {
    if (!path.empty()) {
        using namespace ghoul::io;
        using namespace ghoul::opengl;
        std::unique_ptr<Texture> texture = TextureReader::ref().loadTexture(
            absPath(path)
        );

        if (texture) {
            LDEBUG(fmt::format("Loaded texture from '{}'", absPath(path)));
            _texture = std::move(texture);

            _textureFile = std::make_unique<ghoul::filesystem::File>(path);
            if (_shouldWatchFile) {
                _textureFile->setCallback(
                    [&](const ghoul::filesystem::File&) { _fileIsDirty = true; }
                );
            }

            _fileIsDirty = false;
            _textureIsDirty = true;
        }
    }
}

void TextureComponent::update() {
    if (_fileIsDirty) {
        loadFromFile(_textureFile->path());
    }

    if (_textureIsDirty) {
        uploadToGpu();
        _textureIsDirty = false;
    }
}
} // namespace openspace
