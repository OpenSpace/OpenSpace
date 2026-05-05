/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/base/rendering/renderableplaneimageonline.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <cstdlib>
#include <limits>
#include <utility>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "Sets the path of the texture that is displayed on this plane. The path can "
        "either be specified as a filepath to a local image on disc, or a web URL to an "
        "online image. If this value is changed, the image at the new path will "
        "automatically be loaded and displayed.",
        Property::Visibility::User
    };

    // Creates a textured 3D plane, where the texture image is loaded from the internet
    // through a web URL.
    struct [[codegen::Dictionary(RenderablePlaneImageOnline)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::string texture;

        // Whether the provided path should be interpreted as a URL or a local file path.
        std::optional<bool> isUrl;
    };
} // namespace
#include "renderableplaneimageonline_codegen.cpp"

namespace openspace {

Documentation RenderablePlaneImageOnline::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_planeimageonline",
        RenderablePlane::Documentation()
    );
}

RenderablePlaneImageOnline::RenderablePlaneImageOnline(
                                                      const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _texturePath(TextureInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _isUrl = p.isUrl.value_or(_isUrl);

    if (_isUrl) {
        _texturePath = p.texture;
    }
    else {
        _texturePath = absPath(p.texture).string();
        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath.value());
        _textureFile->setCallback([this]() { _textureIsDirty = true; });
    }

    _textureIsDirty = true;
    _texturePath.onChange([this]() { _textureIsDirty = true; });
    addProperty(_texturePath);

    _autoScale.onChange([this]() {
        if (!_autoScale) {
            return;
        }

        // Shape the plane based on the aspect ration of the image
        const glm::vec2 textureDim = glm::vec2(_texture->dimensions());
        if (_textureDimensions != textureDim) {
            const float aspectRatio = textureDim.x / textureDim.y;
            const float planeAspectRatio = _size.value().x / _size.value().y;

            if (std::abs(planeAspectRatio - aspectRatio) >
                std::numeric_limits<float>::epsilon())
            {
                _size =
                    aspectRatio > 0.f ?
                    glm::vec2(_size.value().x * aspectRatio, _size.value().y) :
                    glm::vec2(_size.value().x, _size.value().y * aspectRatio);
            }

            _textureDimensions = textureDim;
        }
    });
}

void RenderablePlaneImageOnline::initializeGL() {
    RenderablePlane::initializeGL();
}

void RenderablePlaneImageOnline::deinitializeGL() {
    _texture = nullptr;
    _textureFile = nullptr;

    RenderablePlane::deinitializeGL();
}

void RenderablePlaneImageOnline::bindTexture(ghoul::opengl::TextureUnit& unit) {
    if (_texture) {
        unit.bind(*_texture);
    }
}

void RenderablePlaneImageOnline::update(const UpdateData& data) {
    RenderablePlane::update(data);

    if (_textureIsDirty) {
        loadTexture();
    }
}

std::future<DownloadManager::MemoryFile>
RenderablePlaneImageOnline::downloadImageToMemory(const std::string& url)
{
    return global::downloadManager->fetchFile(
        url,
        [](const DownloadManager::MemoryFile&) {
            LDEBUGC(
                "RenderablePlaneImage",
                "Download to memory finished"
            );
        },
        [](const std::string& err) {
            LDEBUGC(
                "RenderablePlaneImage",
                "Download to memory failed: " + err
            );
        }
    );
}

void RenderablePlaneImageOnline::loadTexture() {
    if (_isUrl) {
        if (!_imageFuture.valid()) {
            std::future<DownloadManager::MemoryFile> future = downloadImageToMemory(
                _texturePath
            );
            if (future.valid()) {
                _imageFuture = std::move(future);
            }
        }

        if (_imageFuture.valid() && DownloadManager::futureReady(_imageFuture)) {
            const DownloadManager::MemoryFile imageFile = _imageFuture.get();

            if (imageFile.corrupted) {
                LERRORC(
                    "RenderablePlaneImage",
                    std::format("Error loading image from URL '{}'", _texturePath.value())
                );
                return;
            }

            try {
                _texture = ghoul::io::texture::loadTexture(
                    reinterpret_cast<void*>(imageFile.buffer),
                    imageFile.size,
                    2,
                    ghoul::opengl::Texture::SamplerInit{
                        .filter = ghoul::opengl::Texture::FilterMode::LinearMipMap
                    },
                    imageFile.format
                );

                _textureIsDirty = false;
            }
            catch (const ghoul::io::texture::InvalidLoadException& e) {
                _textureIsDirty = false;
                LERRORC(e.component, e.message);
            }
        }
    }
    else {
        if (_texturePath.value().empty()) {
            return;
        }

        const unsigned int hash = ghoul::hashCRC32File(_texturePath);

        ghoul::opengl::Texture* texture = BaseModule::TextureManager.request(
            std::to_string(hash),
            [path = _texturePath.value()]() -> std::unique_ptr<ghoul::opengl::Texture> {
                std::unique_ptr<ghoul::opengl::Texture> texture =
                    ghoul::io::texture::loadTexture(
                        absPath(path),
                        2,
                        ghoul::opengl::Texture::SamplerInit{
                            .filter = ghoul::opengl::Texture::FilterMode::LinearMipMap
                        }
                    );

                LDEBUGC(
                    "RenderablePlaneImage",
                    std::format("Loaded texture from '{}'", absPath(path))
                );
                return texture;
            }
        );
        _texture = std::make_unique<ghoul::opengl::Texture>(*texture);
        _textureIsDirty = false;

        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath.value());
        _textureFile->setCallback([this]() { _textureIsDirty = true; });
    }

    if (!_autoScale) {
        return;
    }

    // Shape the plane based on the aspect ration of the image
    const glm::vec2 textureDim = glm::vec2(_texture->dimensions());
    if (_textureDimensions != textureDim) {
        const float aspectRatio = textureDim.x / textureDim.y;
        const float planeAspectRatio = _size.value().x / _size.value().y;

        if (std::abs(planeAspectRatio - aspectRatio) >
            std::numeric_limits<float>::epsilon())
        {
            _size =
                aspectRatio > 0.f ?
                glm::vec2(_size.value().x * aspectRatio, _size.value().y) :
                glm::vec2(_size.value().x, _size.value().y * aspectRatio);
        }

        _textureDimensions = textureDim;
    }
}

} // namespace openspace
