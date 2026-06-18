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

#include <modules/base/rendering/renderableplaneimagelocal.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <cstdlib>
#include <limits>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "A path to an image file to use as a texture for the plane. During stereo "
        "rendering, this texture is considered the left eye.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo RightTextureInfo = {
        "RightTexture",
        "Right Eye Texture",
        "Sets the path of the texture that is displayed on this plane for the right eye "
        "during stereo rendering.",
        Property::Visibility::User
    };

    // Creates a textured 3D plane, where the texture is provided by a local file on disk.
    // To use a stereo image set, a second image can be specified with the `RightTexture`
    // property that will be used for the right eye during stereo rendering.
    struct [[codegen::Dictionary(RenderablePlaneImageLocal)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::filesystem::path texture;

        // [[codegen::verbatim(RightTextureInfo.description)]]
        std::optional<std::filesystem::path> rightTexture;

        // If this value is set to true, the image for this plane will not be loaded at
        // startup but rather when plane is shown for the first time. Additionally, if the
        // plane is hidden, the image will automatically be unloaded.
        std::optional<bool> lazyLoading;
    };
} // namespace
#include "renderableplaneimagelocal_codegen.cpp"

namespace openspace {

Documentation RenderablePlaneImageLocal::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_planeimagelocal",
        RenderablePlane::Documentation()
    );
}

RenderablePlaneImageLocal::RenderablePlaneImageLocal(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary, { .shouldUpdateIfDisabled = true })
    , _texturePath(TextureInfo)
    , _rightTexturePath(RightTextureInfo)
    , _textureIsDirty(_enabled)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _texturePath = p.texture.string();
    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath.value());
    _textureFile->setCallback([this]() { _textureIsDirty = true; });

    _texturePath.onChange([this]() { loadTexture(); });
    addProperty(_texturePath);

    if (p.rightTexture.has_value()) {
        _isStereo = true;
        _rightTexturePath = (*p.rightTexture).string();
        _rightTextureFile =
            std::make_unique<ghoul::filesystem::File>(_rightTexturePath.value());
        _rightTextureFile->setCallback([this]() { _textureIsDirty = true; });

        _rightTexturePath.onChange([this]() { loadTexture(); });
        addProperty(_rightTexturePath);
    }

    _isLoadingLazily = p.lazyLoading.value_or(_isLoadingLazily);
    if (_isLoadingLazily) {
        _enabled.onChange([this]() {
            if (_enabled) {
                _textureIsDirty = true;
            }
            else {
                _shouldUnloadTexture = true;
            }
        });
    }

    if (_autoScale) {
        scaleToAspectRatio();
    }
}

void RenderablePlaneImageLocal::initializeGL() {
    RenderablePlane::initializeGL();

    if (!_isLoadingLazily) {
        loadTexture();
    }
}

void RenderablePlaneImageLocal::deinitializeGL() {
    _textureFile = nullptr;
    _rightTextureFile = nullptr;

    BaseModule::TextureManager.release(_texture);
    _texture = nullptr;

    if (_isStereo) {
        BaseModule::TextureManager.release(_rightTexture);
        _rightTexture = nullptr;
    }

    RenderablePlane::deinitializeGL();
}

void RenderablePlaneImageLocal::bindTexture(ghoul::opengl::TextureUnit& unit) {
    switch (global::windowDelegate->frustumMode()) {
        case WindowDelegate::Frustum::Mono:
        case WindowDelegate::Frustum::LeftEye:
            if (_texture) {
                unit.bind(*_texture);
            }
            break;
        case WindowDelegate::Frustum::RightEye:
            if (_isStereo && _rightTexture) {
                unit.bind(*_rightTexture);
            }
            else if (!_isStereo && _texture) {
                unit.bind(*_texture);
            }
            break;
    }
}

void RenderablePlaneImageLocal::update(const UpdateData& data) {
    ZoneScoped;

    RenderablePlane::update(data);

    if (_shouldUnloadTexture) [[unlikely]] {
        BaseModule::TextureManager.release(_texture);
        _texture = nullptr;

        if (_isStereo) {
            BaseModule::TextureManager.release(_rightTexture);
            _rightTexture = nullptr;
        }

        _shouldUnloadTexture = false;
        _textureIsDirty = false;
    }

    if (_textureIsDirty) [[unlikely]] {
        loadTexture();
    }
}

void RenderablePlaneImageLocal::loadTexture() {
    ZoneScoped;

    if (_texturePath.value().empty()) [[unlikely]] {
        return;
    }
    else if (_isStereo && _rightTexturePath.value().empty()) [[unlikely]] {
        _isStereo = false;
    }
    else if (!_isStereo && !_rightTexturePath.value().empty()) [[unlikely]] {
        _isStereo = true;
    }

    ghoul::opengl::Texture* t = _texture;

    const unsigned int hash = ghoul::hashCRC32File(_texturePath);

    _texture = BaseModule::TextureManager.request(
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
                "RenderablePlaneImageLocal",
                std::format("Loaded texture from '{}'", absPath(path))
            );
            return texture;
        }
    );

    BaseModule::TextureManager.release(t);

    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath.value());
    _textureFile->setCallback([this]() { _textureIsDirty = true; });

    if (_isStereo) {
        ghoul::opengl::Texture* t = _rightTexture;

        const unsigned int hash = ghoul::hashCRC32File(_rightTexturePath);

        _rightTexture = BaseModule::TextureManager.request(
            std::to_string(hash),
            [path = _rightTexturePath.value()]() ->
                std::unique_ptr<ghoul::opengl::Texture>
            {
                std::unique_ptr<ghoul::opengl::Texture> texture =
                    ghoul::io::texture::loadTexture(
                        path,
                        2,
                        ghoul::opengl::Texture::SamplerInit{
                            .filter = ghoul::opengl::Texture::FilterMode::LinearMipMap
                        }
                    );

                LDEBUGC(
                    "RenderablePlaneImageLocal",
                    std::format("Loaded right texture from '{}'", absPath(path))
                );
                return texture;
            }
        );

        BaseModule::TextureManager.release(t);

        _rightTextureFile =
            std::make_unique<ghoul::filesystem::File>(_rightTexturePath.value());
        _rightTextureFile->setCallback([this]() { _textureIsDirty = true; });
    }

    if (_texture && (!_isStereo || _rightTexture)) {
        _textureIsDirty = false;
    }

    if (_autoScale) {
        scaleToAspectRatio();
    }
}

void RenderablePlaneImageLocal::scaleToAspectRatio() {
    if (!_texture) {
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

} // namespace openspace
