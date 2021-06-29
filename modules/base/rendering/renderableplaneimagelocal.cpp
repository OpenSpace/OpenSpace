/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>
#include <fstream>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this plane. This image has to be square."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderableTypeInfo = {
        "RenderableType",
        "RenderableType",
        "This value specifies if the plane should be rendered in the Background,"
        "Opaque, Transparent, or Overlay rendering step."
    };

    struct [[codegen::Dictionary(RenderablePlaneImageLocal)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::string texture;

        enum class RenderType {
            Background,
            Opaque,
            PreDeferredTransparency,
            PostDeferredTransparency,
            Overlay
        };

        // [[codegen::verbatim(RenderableTypeInfo.description)]]
        std::optional<RenderType> renderType [[codegen::key("RenderableType")]];

        // If this value is set to 'true', the image for this plane will not be loaded at
        // startup but rather when image is shown for the first time. Additionally, if the
        // plane is hidden, the image will automatically be unloaded
        std::optional<bool> lazyLoading;
    };
#include "renderableplaneimagelocal_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderablePlaneImageLocal::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>(
        "base_renderable_plane_image_local"
    );

    // @TODO cleanup
    // Insert the parents documentation entries until we have a verifier that can deal
    // with class hierarchy
    documentation::Documentation parentDoc = RenderablePlane::Documentation();
    doc.entries.insert(
        doc.entries.end(),
        parentDoc.entries.begin(),
        parentDoc.entries.end()
    );
    return doc;
}

RenderablePlaneImageLocal::RenderablePlaneImageLocal(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _texturePath(TextureInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_blendMode);

    _texturePath = absPath(p.texture).string();
    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath.value());

    addProperty(_texturePath);
    _texturePath.onChange([this]() { loadTexture(); });
    _textureFile->setCallback([this]() { _textureIsDirty = true; });

    if (p.renderType.has_value()) {
        switch (*p.renderType) {
            case Parameters::RenderType::Background:
                setRenderBin(Renderable::RenderBin::Background);
                break;
            case Parameters::RenderType::Opaque:
                setRenderBin(Renderable::RenderBin::Opaque);
                break;
            case Parameters::RenderType::PreDeferredTransparency:
                setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
                break;
            case Parameters::RenderType::PostDeferredTransparency:
                setRenderBin(Renderable::RenderBin::PostDeferredTransparent);
                break;
            case Parameters::RenderType::Overlay:
                setRenderBin(Renderable::RenderBin::Overlay);
                break;
        }
    }
    else {
        setRenderBin(Renderable::RenderBin::Opaque);
    }

    _isLoadingLazily = p.lazyLoading.value_or(_isLoadingLazily);
    if (_isLoadingLazily) {
        _enabled.onChange([this]() {
            if (!_enabled) {
                BaseModule::TextureManager.release(_texture);
                _texture = nullptr;
            }
            if (_enabled) {
                _textureIsDirty = true;
            }
        });
    }
}

bool RenderablePlaneImageLocal::isReady() const {
    return RenderablePlane::isReady();
}

void RenderablePlaneImageLocal::initializeGL() {
    RenderablePlane::initializeGL();

    if (!_isLoadingLazily) {
        loadTexture();
    }
}

void RenderablePlaneImageLocal::deinitializeGL() {
    _textureFile = nullptr;

    BaseModule::TextureManager.release(_texture);
    RenderablePlane::deinitializeGL();
}

void RenderablePlaneImageLocal::bindTexture() {
    _texture->bind();
}

void RenderablePlaneImageLocal::update(const UpdateData& data) {
    ZoneScoped

    RenderablePlane::update(data);

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderablePlaneImageLocal::loadTexture() {
    ZoneScoped

    if (!_texturePath.value().empty()) {
        ghoul::opengl::Texture* t = _texture;

        unsigned int hash = ghoul::hashCRC32File(_texturePath);

        _texture = BaseModule::TextureManager.request(
            std::to_string(hash),
            [path = _texturePath]() -> std::unique_ptr<ghoul::opengl::Texture> {
                std::unique_ptr<ghoul::opengl::Texture> texture =
                    ghoul::io::TextureReader::ref().loadTexture(absPath(path).string());

                LDEBUGC(
                    "RenderablePlaneImageLocal",
                    fmt::format("Loaded texture from {}", absPath(path))
                );
                texture->uploadTexture();
                texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
                texture->purgeFromRAM();

                return texture;
            }
        );

        BaseModule::TextureManager.release(t);

        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath.value());
        _textureFile->setCallback([this]() { _textureIsDirty = true; });
    }
}

} // namespace openspace
