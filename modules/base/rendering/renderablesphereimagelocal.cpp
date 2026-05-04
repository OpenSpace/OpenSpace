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

#include <modules/base/rendering/renderablesphereimagelocal.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <filesystem>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "The path to an image on disk to use as a texture for this sphere. The image is "
        "expected to be an equirectangular projection.",
        Property::Visibility::User
    };

    // Shows a sphere with an image provided by a local file on disk. To show a sphere
    // with an image from an online source, see
    // [RenderableSphereImageOnline](#base_screenspace_imageonline).
    //
    // Per default, the sphere uses an equirectangular projection for the image mapping
    // and hence expects an equirectangular image. However, it can also be used to show
    // fisheye images by changing the `TextureProjection`.
    struct [[codegen::Dictionary(RenderableSphereImageLocal)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::filesystem::path texture;

        // If this value is set to true, the image for this sphere will not be loaded at
        // startup but rather when sphere is shown for the first time. Additionally, if
        // the sphere is hidden, the image will automatically be unloaded.
        std::optional<bool> lazyLoading;
    };
} // namespace
#include "renderablesphereimagelocal_codegen.cpp"

namespace openspace {

Documentation RenderableSphereImageLocal::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_sphereimagelocal",
        RenderableSphere::Documentation()
    );
}

RenderableSphereImageLocal::RenderableSphereImageLocal(
                                                      const ghoul::Dictionary& dictionary)
    : RenderableSphere(dictionary)
    , _texturePath(TextureInfo)
    , _textureIsDirty(_enabled)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _texturePath = p.texture.string();
    _texturePath.onChange([this]() { _texture->loadFromFile(_texturePath.value()); });
    addProperty(_texturePath);

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
}

void RenderableSphereImageLocal::initialize() {
    _texture = std::make_unique<TextureComponent>(2);
    _texture->setFilterMode(ghoul::opengl::Texture::FilterMode::LinearMipMap);
    _texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
}

void RenderableSphereImageLocal::initializeGL() {
    RenderableSphere::initializeGL();

    if (!_isLoadingLazily) {
        _texture->loadFromFile(_texturePath.value());
        _textureIsDirty = false;
    }
}

void RenderableSphereImageLocal::deinitializeGL() {
    _texture = nullptr;

    RenderableSphere::deinitializeGL();
}

void RenderableSphereImageLocal::update(const UpdateData& data) {
    RenderableSphere::update(data);

    if (_shouldUnloadTexture) [[unlikely]] {
        _texture->unloadTexture();
        _shouldUnloadTexture = false;
    }

    if (_textureIsDirty) [[unlikely]] {
        _texture->loadFromFile(_texturePath.value());
        _textureIsDirty = false;
    }

    _texture->update();
}

void RenderableSphereImageLocal::bindTexture(ghoul::opengl::TextureUnit& unit) {
    unit.bind(*_texture->texture());
}

} // namespace openspace
