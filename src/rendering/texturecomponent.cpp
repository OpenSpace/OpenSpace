/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
    constexpr std::string_view _loggerCat = "TextureComponent";
} // namespace

namespace openspace {

TextureComponent::TextureComponent(int nDimensions)
    : _nDimensions(nDimensions)
{
    ghoul_assert(
        _nDimensions >= 1 && _nDimensions <= 4,
        "nDimensions must be 1, 2, or 3"
    );
}

const ghoul::opengl::Texture* TextureComponent::texture() const {
    return _texture.get();
}

ghoul::opengl::Texture* TextureComponent::texture() {
    return _texture.get();
}

void TextureComponent::setFilterMode(ghoul::opengl::Texture::FilterMode filterMode) {
    _filterMode = filterMode;
}

void TextureComponent::setWrapping(ghoul::opengl::Texture::WrappingMode wrapping) {
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

void TextureComponent::loadFromFile(const std::filesystem::path& path) {
    if (path.empty()) {
        return;
    }

    using namespace ghoul::io;
    using namespace ghoul::opengl;

    std::unique_ptr<Texture> tex = TextureReader::ref().loadTexture(path, _nDimensions);
    if (tex) {
        LDEBUG(std::format("Loaded texture from '{}'", path));
        _texture = std::move(tex);

        _textureFile = std::make_unique<ghoul::filesystem::File>(path);
        if (_shouldWatchFile) {
            _textureFile->setCallback([this]() { _fileIsDirty = true; });
        }

        _fileIsDirty = false;
        _textureIsDirty = true;
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
