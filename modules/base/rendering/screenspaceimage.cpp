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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

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

    static const openspace::properties::Property::PropertyInfo TexturePathInfo = {
        "TexturePath",
        "Texture path",
        "Sets the path of the texture that is displayed on this screen space plane. If "
        "this value is changed, the image at the new path will automatically be loaded "
        "and displayed. The size of the image will also automatically set the default "
        "size of this plane."
    };
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceImage::Documentation() {
    using namespace openspace::documentation;
    return {
        "ScreenSpace Image",
        "base_screenspace_image",
        {
            {
                KeyName,
                new StringVerifier,
                "Specifies the GUI name of the ScreenspaceImage",
                Optional::Yes
            },
            {
                KeyTexturePath,
                new StringVerifier,
                "Specifies the image that is shown on the screenspace-aligned plane. If "
                "this value is set and the URL is not, the disk image is used.",
                Optional::Yes
            }
        }
    };
}

ScreenSpaceImage::ScreenSpaceImage(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _downloadImage(false)
    , _textureIsDirty(false)
    , _texturePath(TexturePathInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ScreenSpaceImage"
    );

    if (dictionary.hasKey(KeyName)) {
        setName(dictionary.value<std::string>(KeyName));
    }
    else {
        static int id = 0;
        setName("ScreenSpaceImage " + std::to_string(id));
        ++id;
    }

    std::string texturePath;
    if (dictionary.getValue(KeyTexturePath, texturePath)) {
        _texturePath = dictionary.value<std::string>(KeyTexturePath);
    }

    if (dictionary.getValue(KeyUrl, _url)) {
        _downloadImage = true;
    }

    _texturePath.onChange([this]() { _textureIsDirty = true; });
    addProperty(_texturePath);
}

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
