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

#include <modules/base/rendering/screenspaceimage.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem>

namespace {
    const char* KeyName = "Name";
    const char* KeyTexturePath = "TexturePath";
    const char* KeyUrl = "URL";
}

namespace openspace {

ScreenSpaceImage::ScreenSpaceImage(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _downloadImage(false)
    , _textureIsDirty(false)
    , _texturePath("texturePath", "Texture path", "")
{
    std::string name;
    if (dictionary.getValue(KeyName, name)) {
        setName(name);
    } else {
        static int id = 0;
        setName("ScreenSpaceImage " + std::to_string(id));
        ++id;
    }

    addProperty(_texturePath);

    std::string texturePath;
    if (dictionary.getValue(KeyTexturePath, texturePath)) {
        _texturePath = texturePath;
        _texturePath.onChange([this]() { _textureIsDirty = true; });
    }

    if (dictionary.getValue(KeyUrl, _url)) {
        _downloadImage = true;
    }
}

ScreenSpaceImage::~ScreenSpaceImage() {}

bool ScreenSpaceImage::initialize() {
    _originalViewportSize = OsEng.windowWrapper().currentWindowResolution();

    createPlane();
    createShaders();
    updateTexture();

    return isReady();
}

bool ScreenSpaceImage::deinitialize() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    _texturePath = "";
    _texture = nullptr;

    if (_shader) {
        OsEng.renderEngine().removeRenderProgram(_shader);
        _shader = nullptr;
    }

    return true;
}

void ScreenSpaceImage::render() {
    draw(rotationMatrix() * translationMatrix() * scaleMatrix());
}

void ScreenSpaceImage::update() {
    bool download = _downloadImage ? (_futureImage.valid() && DownloadManager::futureReady(_futureImage)) : true;
    if (download && _textureIsDirty) {
        loadTexture();
    }
}

bool ScreenSpaceImage::isReady() const {
    return _shader && _texture;
}

void ScreenSpaceImage::loadTexture() {
    std::unique_ptr<ghoul::opengl::Texture> texture = nullptr;
    if (!_downloadImage)
        texture = loadFromDisk();
    else
        texture = loadFromMemory();

    if (texture) {
        // LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");
        texture->uploadTexture();

        // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
        texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

        _texture = std::move(texture);
        _textureIsDirty = false;
    }
}

void ScreenSpaceImage::updateTexture() {
    if (!_downloadImage) {
        loadTexture();
    } else {
        if (_futureImage.valid())
            return;

        std::future<DownloadManager::MemoryFile> future = downloadImageToMemory(_url);
        if (future.valid()) {
            _futureImage = std::move(future);
        }
    }
}

 std::unique_ptr<ghoul::opengl::Texture> ScreenSpaceImage::loadFromDisk() {
    if (_texturePath.value() != "")
        return (ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath.value())));
    return nullptr;
 }

 std::unique_ptr<ghoul::opengl::Texture> ScreenSpaceImage::loadFromMemory() {
    if (_futureImage.valid() && DownloadManager::futureReady(_futureImage)) {
        DownloadManager::MemoryFile imageFile = _futureImage.get();

        if (imageFile.corrupted) {
            return nullptr;
        }

        return (ghoul::io::TextureReader::ref().loadTexture(
            reinterpret_cast<void*>(imageFile.buffer), 
            imageFile.size, 
            imageFile.format)
        );
    }
    else {
        return nullptr;
    }
 }

std::future<DownloadManager::MemoryFile> ScreenSpaceImage::downloadImageToMemory(
    std::string url)
{
    return OsEng.downloadManager().fetchFile(
        url,
        [url](const DownloadManager::MemoryFile&) {
            LDEBUGC(
                "ScreenSpaceImage",
                "Download to memory finished for screen space image"
            );
        },
        [url](const std::string& err) {
            LDEBUGC(
                "ScreenSpaceImage",
                "Download to memory failer for screen space image: " + err
            );
        }
    );
}

} // namespace openspace
