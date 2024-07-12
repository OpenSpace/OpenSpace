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

#include <modules/space/rendering/renderablerings.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <optional>

namespace {
    constexpr std::array<const char*, 6> UniformNames = {
        "modelViewProjectionTransform", "textureOffset", "colorFilterValue",
        "_nightFactor", "sunPosition", "texture1"
    };

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "The path to a texture on disk that contains a one-dimensional texture to use "
        "for these rings.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "The radius of the rings in meters.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OffsetInfo = {
        "Offset",
        "Offset",
        "A value that is used to limit the width of the rings. Each of the two values is "
        "a value between 0 and 1, where 0 is the center of the ring and 1 is the "
        "maximum extent at the radius. For example, if the value is {0.5, 1.0}, the "
        "ring is only shown between radius/2 and radius. It defaults to {0.0, 1.0}.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo NightFactorInfo = {
        "NightFactor",
        "Night Factor",
        "A multiplicative factor that is applied to the side of the rings that is facing "
        "away from the Sun. If it is 1, no darkening of the night side occurs.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorFilterInfo = {
        "ColorFilter",
        "Color Filter",
        "A value that affects the filtering out of part of the rings depending on the "
        "color values of the texture. The higher value, the more rings are filtered out.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableRings)]] Parameters {
        // [[codegen::verbatim(TextureInfo.description)]]
        std::filesystem::path texture;

        // [[codegen::verbatim(SizeInfo.description)]]
        float size;

        // [[codegen::verbatim(OffsetInfo.description)]]
        std::optional<glm::vec2> offset;

        // [[codegen::verbatim(NightFactorInfo.description)]]
        std::optional<float> nightFactor;

        // [[codegen::verbatim(ColorFilterInfo.description)]]
        std::optional<float> colorFilter;
    };
#include "renderablerings_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableRings::Documentation() {
    return codegen::doc<Parameters>("space_renderable_rings");
}

RenderableRings::RenderableRings(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath(TextureInfo)
    , _size(SizeInfo, 1.f, 0.f, 1e25f)
    , _offset(OffsetInfo, glm::vec2(0.f, 1.f), glm::vec2(0.f), glm::vec2(1.f))
    , _nightFactor(NightFactorInfo, 0.33f, 0.f, 1.f)
    , _colorFilter(ColorFilterInfo, 0.15f, 0.f, 1.f)
{
    using ghoul::filesystem::File;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _size = p.size;
    setBoundingSphere(_size);
    _size.onChange([this]() { _planeIsDirty = true; });
    addProperty(_size);

    _texturePath = p.texture.string();
    _textureFile = std::make_unique<File>(_texturePath.value());

    _offset = p.offset.value_or(_offset);
    _offset.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_offset);

    _texturePath.onChange([this]() { loadTexture(); });
    addProperty(_texturePath);

    _textureFile->setCallback([this]() { _textureIsDirty = true; });

    _nightFactor = p.nightFactor.value_or(_nightFactor);
    addProperty(_nightFactor);

    _colorFilter = p.colorFilter.value_or(_colorFilter);
    addProperty(_colorFilter);
}

bool RenderableRings::isReady() const {
    return _shader && _texture;
}

void RenderableRings::initializeGL() {
    _shader = global::renderEngine->buildRenderProgram(
        "RingProgram",
        absPath("${MODULE_SPACE}/shaders/rings_vs.glsl"),
        absPath("${MODULE_SPACE}/shaders/rings_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);

    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);

    createPlane();
    loadTexture();
}

void RenderableRings::deinitializeGL() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    _textureFile = nullptr;
    _texture = nullptr;

    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;
}

void RenderableRings::render(const RenderData& data, RendererTasks&) {
    _shader->activate();

    _shader->setUniform(
        _uniformCache.modelViewProjection,
        glm::mat4(calcModelViewProjectionTransform(data))
    );
    _shader->setUniform(_uniformCache.textureOffset, _offset);
    _shader->setUniform(_uniformCache.colorFilterValue, _colorFilter);

    _shader->setUniform(_uniformCache.nightFactor, _nightFactor);
    _shader->setUniform(_uniformCache.sunPosition, _sunPosition);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform(_uniformCache.texture, unit);

    glDisable(GL_CULL_FACE);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    glEnable(GL_CULL_FACE);
    _shader->deactivate();
}

void RenderableRings::update(const UpdateData& data) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    }

    if (_planeIsDirty) {
        createPlane();
        _planeIsDirty = false;
    }

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }

    SceneGraphNode* sun = global::renderEngine->scene()->sceneGraphNode("Sun");
    if (sun) {
        _sunPosition = sun->worldPosition() - data.modelTransform.translation;
    }
    else {
        // If the Sun node does not exist, we assume the light source to be in the origin
        _sunPosition = -data.modelTransform.translation;
    }
}

void RenderableRings::loadTexture() {
    if (!_texturePath.value().empty()) {
        using namespace ghoul::io;
        using namespace ghoul::opengl;
        std::unique_ptr<Texture> texture = TextureReader::ref().loadTexture(
            absPath(_texturePath),
            1
        );

        if (texture) {
            LDEBUGC(
                "RenderableRings",
                std::format("Loaded texture from '{}'", absPath(_texturePath))
            );
            _texture = std::move(texture);

            _texture->uploadTexture();
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

            _textureFile = std::make_unique<ghoul::filesystem::File>(
                _texturePath.value()
            );
            _textureFile->setCallback([this]() { _textureIsDirty = true; });
        }
    }
}

void RenderableRings::createPlane() {
    const GLfloat size = _size;

    struct VertexData {
        GLfloat x;
        GLfloat y;
        GLfloat s;
        GLfloat t;
    };

    const std::array<VertexData, 6> Data = {
        VertexData{ -size, -size, 0.f, 0.f },
        VertexData{  size,  size, 1.f, 1.f },
        VertexData{ -size,  size, 0.f, 1.f },
        VertexData{ -size, -size, 0.f, 0.f },
        VertexData{  size, -size, 1.f, 0.f },
        VertexData{  size,  size, 1.f, 1.f },
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, Data.size(), Data.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(VertexData),
        nullptr
    );
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(VertexData),
        reinterpret_cast<void*>(offsetof(VertexData, s))
    );
}

} // namespace openspace
