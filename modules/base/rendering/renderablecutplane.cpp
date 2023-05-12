/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/base/rendering/renderablecutplane.h>

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
        "hej",
        "Hejhejs",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this plane. This image has to be square",
        // @VISIBILITY(2.25)
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableCutPlane)]] Parameters {
//         [[codegen::verbatim(TextureInfo.description)]]
        std::string texture;

    };
#include "renderablecutplane_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableCutPlane::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_cut_plane",
        RenderablePlane::Documentation()
    );
}

RenderableCutPlane::RenderableCutPlane(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _filePath(TextureInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_blendMode);

    _filePath = absPath(p.texture).string();
    _sourceFile = std::make_unique<ghoul::filesystem::File>(_filePath.value());

    addProperty(_filePath);
    _filePath.onChange([this]() { loadTexture(); });
    _sourceFile->setCallback([this]() { _textureIsDirty = true; });

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

    _autoScale.onChange([this]() {
        if (!_autoScale) {
            return;
        }

        // Shape the plane based on the aspect ration of the image
        glm::vec2 textureDim = glm::vec2(_texture->dimensions());
        if (_textureDimensions != textureDim) {
            float aspectRatio = textureDim.x / textureDim.y;
            float planeAspectRatio = _size.value().x / _size.value().y;

            if (std::abs(planeAspectRatio - aspectRatio) >
                std::numeric_limits<float>::epsilon())
            {
                glm::vec2 newSize =
                    aspectRatio > 0.f ?
                    glm::vec2(_size.value().x * aspectRatio, _size.value().y) :
                    glm::vec2(_size.value().x, _size.value().y * aspectRatio);
                _size = newSize;
            }

            _textureDimensions = textureDim;
        }
    });
}

bool RenderableCutPlane::isReady() const {
    return RenderablePlane::isReady();
}

void RenderableCutPlane::initializeGL() {
    RenderablePlane::initializeGL();

    if (!_isLoadingLazily) {
        loadTexture();
    }
}

void RenderableCutPlane::deinitializeGL() {
    _sourceFile = nullptr;

    BaseModule::TextureManager.release(_texture);
    RenderablePlane::deinitializeGL();
}

void RenderableCutPlane::bindTexture() {
    _texture->bind();
}

void RenderableCutPlane::update(const UpdateData& data) {
    ZoneScoped;

    RenderablePlane::update(data);

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderableCutPlane::loadTexture() {
    ZoneScoped;

    if (!_filePath.value().empty()) {
        ghoul::opengl::Texture* t = _texture;

        unsigned int hash = ghoul::hashCRC32File(_filePath);

        _texture = BaseModule::TextureManager.request(
            std::to_string(hash),
            [path = _filePath]() -> std::unique_ptr<ghoul::opengl::Texture> {
                std::unique_ptr<ghoul::opengl::Texture> texture =
                    ghoul::io::TextureReader::ref().loadTexture(
                        absPath(path).string(),
                        2
                    );

                LDEBUGC(
                    "RenderableCutPlane",
                    fmt::format("Loaded texture from {}", absPath(path))
                );
                texture->uploadTexture();
                texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
                texture->purgeFromRAM();

                return texture;
            }
        );

        BaseModule::TextureManager.release(t);

        _sourceFile = std::make_unique<ghoul::filesystem::File>(_filePath.value());
        _sourceFile->setCallback([this]() { _textureIsDirty = true; });

        if (!_autoScale) {
            return;
        }

        // Shape the plane based on the aspect ration of the image
        glm::vec2 textureDim = glm::vec2(_texture->dimensions());
        if (_textureDimensions != textureDim) {
            float aspectRatio = textureDim.x / textureDim.y;
            float planeAspectRatio = _size.value().x / _size.value().y;

            if (std::abs(planeAspectRatio - aspectRatio) >
                std::numeric_limits<float>::epsilon())
            {
                glm::vec2 newSize =
                    aspectRatio > 0.f ?
                    glm::vec2(_size.value().x * aspectRatio, _size.value().y) :
                    glm::vec2(_size.value().x, _size.value().y * aspectRatio);
                _size = newSize;
            }

            _textureDimensions = textureDim;
        }
    }
}

} // namespace openspace
