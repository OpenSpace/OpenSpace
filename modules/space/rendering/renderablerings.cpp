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

#include <modules/space/rendering/renderablerings.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <cstddef>
#include <filesystem>
#include <utility>

namespace {
    using namespace openspace;

    struct VertexData {
        GLfloat x;
        GLfloat y;
        GLfloat s;
        GLfloat t;
    };

    constexpr Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "The path to a texture on disk that contains a one-dimensional texture to use "
        "for these rings.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "The radius of the rings in meters.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo OffsetInfo = {
        "Offset",
        "Offset",
        "A value that is used to limit the width of the rings. Each of the two values is "
        "a value between 0 and 1, where 0 is the center of the ring and 1 is the "
        "maximum extent at the radius. For example, if the value is {0.5, 1.0}, the "
        "ring is only shown between radius/2 and radius. It defaults to {0.0, 1.0}.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo NightFactorInfo = {
        "NightFactor",
        "Night factor",
        "A multiplicative factor that is applied to the side of the rings that is facing "
        "away from the Sun. If it is 1, no darkening of the night side occurs.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo ColorFilterInfo = {
        "ColorFilter",
        "Color filter",
        "A value that affects the filtering out of part of the rings depending on the "
        "color values of the texture. The higher value, the more rings are filtered out.",
        Property::Visibility::AdvancedUser
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
} // namespace
#include "renderablerings_codegen.cpp"

namespace openspace {

Documentation RenderableRings::Documentation() {
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
    _offset.setViewOption(Property::ViewOptions::MinMaxRange);
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

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);

    glCreateBuffers(1, &_vbo);
    glCreateVertexArrays(1, &_vao);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, sizeof(VertexData));

    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 2, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);

    glEnableVertexArrayAttrib(_vao, 1);
    glVertexArrayAttribFormat(_vao, 1, 2, GL_FLOAT, GL_FALSE, offsetof(VertexData, s));
    glVertexArrayAttribBinding(_vao, 1, 0);

    createPlane();
    loadTexture();
}

void RenderableRings::deinitializeGL() {
    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);

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
    unit.bind(*_texture);
    _shader->setUniform(_uniformCache.texture, unit);

    glDisable(GL_CULL_FACE);

    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    glEnable(GL_CULL_FACE);
    _shader->deactivate();
}

void RenderableRings::update(const UpdateData& data) {
    if (_shader->isDirty()) [[unlikely]] {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
    }

    if (_planeIsDirty) [[unlikely]] {
        createPlane();
        _planeIsDirty = false;
    }

    if (_textureIsDirty) [[unlikely]] {
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
    using namespace ghoul::io;
    using namespace ghoul::opengl;
    _texture = TextureReader::ref().loadTexture(
        _texturePath.value(),
        1,
        { .filter = ghoul::opengl::Texture::FilterMode::AnisotropicMipMap }
    );

    LDEBUGC(
        "RenderableRings",
        std::format("Loaded texture from '{}'", _texturePath.value())
    );

    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath.value());
    _textureFile->setCallback([this]() { _textureIsDirty = true; });
}

void RenderableRings::createPlane() {
    const GLfloat size = _size;

    const std::array<VertexData, 6> Data = {
        VertexData{ -size, -size, 0.f, 0.f },
        VertexData{  size,  size, 1.f, 1.f },
        VertexData{ -size,  size, 0.f, 1.f },
        VertexData{ -size, -size, 0.f, 0.f },
        VertexData{  size, -size, 1.f, 0.f },
        VertexData{  size,  size, 1.f, 1.f },
    };

    glNamedBufferData(_vbo, Data.size(), Data.data(), GL_STATIC_DRAW);
}

} // namespace openspace
