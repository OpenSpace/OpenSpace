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

    // This `Renderable` shows a sphere with an image provided by a local file on disk. To
    // show a sphere with an image from an online source, see
    // [RenderableSphereImageOnline](#base_screenspace_image_online).
    struct [[codegen::Dictionary(RenderableSphereImageLocal)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::filesystem::path texture;
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
    _texturePath.onChange([this]() { _texture->loadFromFile(_texturePath.value()); });
    addProperty(_texturePath);
}

bool RenderableSphereImageLocal::isReady() const {
    return RenderableSphere::isReady() && _texture;
}

void RenderableSphereImageLocal::initialize() {
    _texture = std::make_unique<TextureComponent>(2);
    _texture->setFilterMode(ghoul::opengl::Texture::FilterMode::LinearMipMap);
    _texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
}

void RenderableSphereImageLocal::initializeGL() {
    RenderableSphere::initializeGL();
    _texture->loadFromFile(_texturePath.value());
    _texture->uploadToGpu();
}

void RenderableSphereImageLocal::deinitializeGL() {
    _texture = nullptr;

    RenderableSphere::deinitializeGL();
}

void RenderableSphereImageLocal::update(const UpdateData& data) {
    RenderableSphere::update(data);
    _texture->update();
}

void RenderableSphereImageLocal::bindTexture() {
    _texture->bind();
}

} // namespace openspace
