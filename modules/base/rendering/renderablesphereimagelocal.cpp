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

#include <modules/base/rendering/renderablesphereimagelocal.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/sphere.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "The path to an image on disk to use as a texture for this sphere. The image is "
        "expected to be an equirectangular projection.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableSphereImageLocal)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::filesystem::path texture;

        // If true, the image for this sphere will not be loaded at startup but rather
        // when the image is shown for the first time. Additionally, if the sphere is
        // disabled, the image will automatically be unloaded.
        std::optional<bool> lazyLoading;
    };
#include "renderablesphereimagelocal_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSphereImageLocal::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_sphere_image_local",
        RenderableSphere::Documentation()
    );
}

RenderableSphereImageLocal::RenderableSphereImageLocal(
                                                      const ghoul::Dictionary& dictionary)
    : RenderableSphere(dictionary)
    , _texturePath(TextureInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _texturePath = p.texture.string();
    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath.value());
    _texturePath.onChange([this]() {
        loadTexture();
    });
    addProperty(_texturePath);
    _textureFile->setCallback([this]() { _textureIsDirty = true; });
}

bool RenderableSphereImageLocal::isReady() const {
    return RenderableSphere::isReady() && _texture;
}

void RenderableSphereImageLocal::initializeGL() {
    RenderableSphere::initializeGL();

    if (!_isLoadingLazily) {
        loadTexture();
    }
}

void RenderableSphereImageLocal::deinitializeGL() {
    _texture = nullptr;

    RenderableSphere::deinitializeGL();
}

void RenderableSphereImageLocal::update(const UpdateData& data) {
    RenderableSphere::update(data);

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderableSphereImageLocal::bindTexture() {
    _texture->bind();
}

void RenderableSphereImageLocal::loadTexture() {
    if (_texturePath.value().empty()) {
        return;
    }

    std::unique_ptr<ghoul::opengl::Texture> texture =
        ghoul::io::TextureReader::ref().loadTexture(_texturePath.value(), 2);

    if (!texture) {
        LWARNINGC(
            "RenderableSphereImageLocal",
            std::format("Could not load texture from '{}'", absPath(_texturePath))
        );
        return;
    }

    LDEBUGC(
        "RenderableSphereImageLocal",
        std::format("Loaded texture from '{}'", absPath(_texturePath))
    );
    texture->uploadTexture();
    texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
    texture->purgeFromRAM();
    _texture = std::move(texture);
}

} // namespace openspace
