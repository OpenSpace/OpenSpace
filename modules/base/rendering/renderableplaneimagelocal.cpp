/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>

namespace {
    static const openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this plane. This image has to be square."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderablePlaneImageLocal::Documentation() {
    using namespace documentation;
    return {
        "Renderable Plane Image Local",
        "base_renderable_plane_image_local",
        {
            {
                TextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                TextureInfo.description,
            }
        }
    };
}

RenderablePlaneImageLocal::RenderablePlaneImageLocal(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _texturePath(TextureInfo)
    , _texture(nullptr)
    , _textureIsDirty(false)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderablePlaneImageLocal"
    );

    _texturePath = absPath(dictionary.value<std::string>(TextureInfo.identifier));
    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);

    addProperty(_texturePath);
    _texturePath.onChange([this]() {loadTexture(); });
    _textureFile->setCallback(
        [this](const ghoul::filesystem::File&) { _textureIsDirty = true; }
    );
}

bool RenderablePlaneImageLocal::isReady() const {
    return RenderablePlane::isReady() && _texture != nullptr;
}

void RenderablePlaneImageLocal::initializeGL() {
    RenderablePlane::initializeGL();

    loadTexture();
}

void RenderablePlaneImageLocal::deinitializeGL() {
    _textureFile = nullptr;
    _texture = nullptr;
    RenderablePlane::deinitializeGL();
}


void RenderablePlaneImageLocal::bindTexture() {
    _texture->bind();
}

void RenderablePlaneImageLocal::update(const UpdateData& data) {
    RenderablePlane::update(data);

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderablePlaneImageLocal::loadTexture() {
    if (!_texturePath.value().empty()) {
        std::unique_ptr<ghoul::opengl::Texture> texture =
            ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));

        if (texture) {
            LDEBUGC(
                "RenderablePlane",
                "Loaded texture from '" << absPath(_texturePath) << "'"
            );
            texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than
            // linear
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

            _texture = std::move(texture);

            _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
            _textureFile->setCallback(
                [&](const ghoul::filesystem::File&) { _textureIsDirty = true; }
            );
        }
    }
}

} // namespace openspace
