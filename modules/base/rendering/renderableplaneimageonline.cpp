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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/format.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <cstdlib>
#include <limits>
#include <utility>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo TextureInfo = {
        "URL",
        "Image URL",
        "Sets the URL of the texture that is displayed on this screen space plane. If "
        "this value is changed, the image at the new path will automatically be loaded "
        "and displayed. During stereo rendering, this texture is considered the left eye.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo RightTextureInfo = {
        "RightTexture",
        "Right Eye Image URL",
        "Sets the URL of the texture that is displayed on this plane for the right eye "
        "during stereo rendering. If this value is changed, the image at the new path "
        "will automatically be loaded and displayed.",
        Property::Visibility::User
    };

    // Creates a textured 3D plane, where the texture image is loaded from the internet
    // through a web URL. To use a stereo image set, a second image can be specified with
    // the `RightURL` property that will be used for the right eye during stereo
    // rendering.
    struct [[codegen::Dictionary(RenderablePlaneImageOnline)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::string url [[codegen::key("URL")]];

        // [[codegen::verbatim(RightTextureInfo.description)]]
        std::optional<std::string> rightUrl [[codegen::key("RightURL")]];
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
    , _rightTexturePath(RightTextureInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _texturePath.onChange([this]() { _textureIsDirty = true; });
    _texturePath = p.url;
    addProperty(_texturePath);

    if (p.rightUrl.has_value()) {
        _useStereo = true;

        _rightTexturePath.onChange([this]() { _textureIsDirty = true; });
        _rightTexturePath = *p.rightUrl;
        addProperty(_rightTexturePath);
    }

    _autoScale.onChange([this]() {
        if (!_autoScale) {
            return;
        }

        // Shape the plane based on the aspect ratio of the image
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

void RenderablePlaneImageOnline::deinitializeGL() {
    _texture = nullptr;
    _rightTexture = nullptr;

    RenderablePlane::deinitializeGL();
}

void RenderablePlaneImageOnline::bindTexture(ghoul::opengl::TextureUnit& unit) {
    switch (global::windowDelegate->frustumMode()) {
        case WindowDelegate::Frustum::Mono:
        case WindowDelegate::Frustum::LeftEye:
            if (_texture) {
                unit.bind(*_texture);
            }
            break;
        case WindowDelegate::Frustum::RightEye:
            if (_useStereo && _rightTexture) {
                unit.bind(*_rightTexture);
            }
            else if (!_useStereo && _texture) {
                unit.bind(*_texture);
            }
            break;
    }
}

void RenderablePlaneImageOnline::update(const UpdateData& data) {
    RenderablePlane::update(data);

    if (!_textureIsDirty) [[unlikely]] {
        return;
    }

    if (!_texture && !_imageFuture.valid()) {
        std::future<DownloadManager::MemoryFile> future = downloadImageToMemory(
            _texturePath
        );
        if (future.valid()) {
            _imageFuture = std::move(future);
        }
    }

    if (!_texture && _imageFuture.valid() && DownloadManager::futureReady(_imageFuture)) {
        const DownloadManager::MemoryFile imageFile = _imageFuture.get();

        if (imageFile.corrupted) {
            LERRORC(
                "RenderablePlaneImageOnline",
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
        }
        catch (const ghoul::io::texture::InvalidLoadException& e) {
            _textureIsDirty = false;
            LERRORC(e.component, e.message);
        }
    }

    if (_useStereo) {
        if (!_rightTexture && !_rightImageFuture.valid()) {
            std::future<DownloadManager::MemoryFile> future = downloadImageToMemory(
                _rightTexturePath
            );
            if (future.valid()) {
                _rightImageFuture = std::move(future);
            }
        }

        if (!_rightTexture && _rightImageFuture.valid() &&
            DownloadManager::futureReady(_rightImageFuture))
        {
            const DownloadManager::MemoryFile imageFile = _rightImageFuture.get();

            if (imageFile.corrupted) {
                LERRORC(
                    "RenderablePlaneImageOnline",
                    std::format("Error loading right image from URL '{}'",
                        _rightTexturePath.value()
                    )
                );
                return;
            }

            try {
                _rightTexture = ghoul::io::texture::loadTexture(
                    reinterpret_cast<void*>(imageFile.buffer),
                    imageFile.size,
                    2,
                    ghoul::opengl::Texture::SamplerInit{
                        .filter = ghoul::opengl::Texture::FilterMode::LinearMipMap
                    },
                    imageFile.format
                );
            }
            catch (const ghoul::io::texture::InvalidLoadException& e) {
                _textureIsDirty = false;
                LERRORC(e.component, e.message);
            }
        }
    }

    if (_texture && (!_useStereo || _rightTexture)) {
        _textureIsDirty = false;
    }

    if (!_autoScale) {
        return;
    }

    // Shape the plane based on the aspect ratio of the image
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

std::future<DownloadManager::MemoryFile>
RenderablePlaneImageOnline::downloadImageToMemory(const std::string& url)
{
    return global::downloadManager->fetchFile(
        url,
        [](const DownloadManager::MemoryFile&) {
            LDEBUGC(
                "RenderablePlaneImageOnline",
                "Download to memory finished"
            );
        },
        [](const std::string& err) {
            LDEBUGC(
                "RenderablePlaneImageOnline",
                "Download to memory failed: " + err
            );
        }
    );
}

} // namespace openspace
