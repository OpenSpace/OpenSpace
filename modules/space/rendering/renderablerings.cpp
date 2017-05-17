/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    const char* KeyTexture = "Texture";
    const char* KeySize = "Size";
    const char* KeyOffset = "Offset";
}

namespace openspace {

documentation::Documentation RenderableRings::Documentation() {
    using namespace documentation;
    return {
        "Renderable Rings",
        "space_renderable_rings",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableRings"),
                "",
                Optional::No
            },
            {
                KeyTexture,
                new StringVerifier,
                "The one dimensional texture that is used for both sides of the ring.",
                Optional::No
            },
            {
                KeySize,
                new DoubleVerifier,
                "The radius of the rings in meters.",
                Optional::No
            },
            {
                KeyOffset,
                new DoubleVector2Verifier,
                "The offset that is used to limit the width of the rings. Each of the "
                "two values is a value between 0 and 1, where 0 is the center of the "
                "ring and 1 is the maximum extent at the radius. If this value is, for "
                "example {0.5, 1.0}, the ring is only shown between radius/2 and radius. "
                "It defaults to {0.0, 1.0}.",
                Optional::Yes
            }
        }
    };
}

RenderableRings::RenderableRings(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath("texture", "Texture")
    , _size("size", "Size", 1.f, 0.f, std::pow(1.f, 25.f))
    , _offset("offset", "Offset", glm::vec2(0, 1.f), glm::vec2(0.f), glm::vec2(1.f))
    , _nightFactor("nightFactor", "Night Factor", 0.33f, 0.f, 1.f)
    , _transparency("transparency", "Transparency", 0.15f, 0.f, 1.f)
    , _shader(nullptr)
    , _texture(nullptr)
    , _textureFile(nullptr)
    , _textureIsDirty(false)
    , _quad(0)
    , _vertexPositionBuffer(0)
    , _planeIsDirty(false)
{
    using ghoul::filesystem::File;

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableRings"
    );

    _size = static_cast<float>(dictionary.value<double>(KeySize));
    setBoundingSphere(_size);
    addProperty(_size);
    _size.onChange([&]() { _planeIsDirty = true; });

    _texturePath = absPath(dictionary.value<std::string>(KeyTexture));
    _textureFile = std::make_unique<File>(_texturePath);

    if (dictionary.hasKeyAndValue<glm::vec2>(KeyOffset)) {
        _offset = dictionary.value<glm::vec2>(KeyOffset);
    }
    addProperty(_offset);
    

    addProperty(_texturePath);
    _texturePath.onChange([&](){ loadTexture(); });

    _textureFile->setCallback([&](const File&) { _textureIsDirty = true; });
    
    addProperty(_nightFactor);
    addProperty(_transparency);
}

bool RenderableRings::isReady() const {
    return _shader && _texture;
}

bool RenderableRings::initialize() {
    if (!_shader) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("RingProgram",
            "${MODULE_SPACE}/shaders/rings_vs.glsl",
            "${MODULE_SPACE}/shaders/rings_fs.glsl"
            );
        _shader->setIgnoreUniformLocationError(
            ghoul::opengl::ProgramObject::IgnoreError::Yes
        );
    }

    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);
    createPlane();
    loadTexture();

    return isReady();
}

bool RenderableRings::deinitialize() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    _textureFile = nullptr;
    _texture = nullptr;

    OsEng.renderEngine().removeRenderProgram(_shader);
    _shader = nullptr;

    return true;
}

void RenderableRings::render(const RenderData& data) {
    _shader->activate();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));
    
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform(
        "modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );
    _shader->setUniform("textureOffset", _offset);
    _shader->setUniform("transparency", _transparency);
    
    _shader->setUniform("_nightFactor", _nightFactor);
    _shader->setUniform(
        "sunPosition",
        _sunPosition
    );
    
    setPscUniforms(*_shader, data.camera, data.position);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform("texture1", unit);
    
    glDisable(GL_CULL_FACE);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    glEnable(GL_CULL_FACE);
    _shader->deactivate();
}

void RenderableRings::update(const UpdateData& data) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }

    if (_planeIsDirty) {
        createPlane();
        _planeIsDirty = false;
    }

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }

    _sunPosition = OsEng.renderEngine().scene()->sceneGraphNode("Sun")->worldPosition() -
        data.modelTransform.translation;
}

void RenderableRings::loadTexture() {
    if (_texturePath.value() != "") {
        std::unique_ptr<ghoul::opengl::Texture> texture =
            ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));

        if (texture) {
            LDEBUGC(
                "RenderableRings",
                "Loaded texture from '" << absPath(_texturePath) << "'"
            );
            _texture = std::move(texture);

            _texture->uploadTexture();
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

            _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
            _textureFile->setCallback(
                [&](const ghoul::filesystem::File&) { _textureIsDirty = true; }
            );
        }
    }
}

void RenderableRings::createPlane() {
    const GLfloat size = _size.value();

    struct VertexData {
        GLfloat x;
        GLfloat y;
        GLfloat s;
        GLfloat t;
    };
    
    VertexData data[] = {
        { -size, -size, 0.f, 0.f },
        {  size,  size, 1.f, 1.f },
        { -size,  size, 0.f, 1.f },
        { -size, -size, 0.f, 0.f },
        {  size, -size, 1.f, 0.f },
        {  size,  size, 1.f, 1.f },
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(data), data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(VertexData),
        reinterpret_cast<void*>(0)
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
