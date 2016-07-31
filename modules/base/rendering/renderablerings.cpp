/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/base/rendering/renderablerings.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    const std::string _loggerCat = "RenderableRings";
    
    const std::string KeySize = "Size";
    const std::string KeyTexture = "Texture";
    const std::string KeyBody = "Body";
    const std::string KeyFrame = "Frame";
    const std::string KeyOrientation = "Orientation";
    const std::string KeyOffset = "Offset";
    const std::string KeyNightFactor = "NightFactor";
    const std::string KeyTransparency = "Transparency";
}

namespace openspace {

RenderableRings::RenderableRings(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath("texture", "Texture")
    , _size("size", "Size", glm::vec2(1.f, 1.f), glm::vec2(0.f), glm::vec2(1.f, 25.f))
    , _offset("offset", "Texture Offset", glm::vec2(0.f, 1.f), glm::vec2(0.f), glm::vec2(1.f))
    , _nightFactor("nightFactor", "Night Factor", 0.33f, 0.f, 1.f)
    , _transparency("transparency", "Transparency", 0.15f, 0.f, 1.f)
    , _shader(nullptr)
    , _texture(nullptr)
    , _textureFile(nullptr)
    , _textureIsDirty(false)
    , _quad(0)
    , _vertexPositionBuffer(0)
    , _planeIsDirty(false)
    , _hasSunPosition(false)
{
    glm::vec2 size;
    dictionary.getValue(KeySize, size);
    _size = size;
    
    if (dictionary.hasKeyAndValue<std::string>(KeyBody)) {
        _body = dictionary.value<std::string>(KeyBody);
        _hasSunPosition = true;
    }
    
    if (dictionary.hasKeyAndValue<std::string>(KeyFrame)) {
        _frame = dictionary.value<std::string>(KeyFrame);
    }
    
    if (dictionary.hasKeyAndValue<glm::mat3>(KeyOrientation)) {
        _orientation = dictionary.value<glm::mat3>(KeyOrientation);
    }

    if (dictionary.hasKeyAndValue<std::string>(KeyTexture)) {
        _texturePath = absPath(dictionary.value<std::string>(KeyTexture));
        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);
    }
    
    if (dictionary.hasKeyAndValue<glm::vec2>(KeyOffset)) {
        glm::vec2 off = dictionary.value<glm::vec2>(KeyOffset);
        _offset = off;
    }
    
    if (dictionary.hasKeyAndValue<float>(KeyNightFactor)) {
        float v = dictionary.value<float>(KeyNightFactor);
        _nightFactor = v;
    }
    
    if (dictionary.hasKeyAndValue<float>(KeyTransparency)) {
        float v = dictionary.value<float>(KeyTransparency);
        _transparency = v;
    }
    
    addProperty(_offset);
    
    addProperty(_size);
    _size.onChange([&](){ _planeIsDirty = true; });

    addProperty(_texturePath);
    _texturePath.onChange([&](){ loadTexture(); });

    _textureFile->setCallback(
        [&](const ghoul::filesystem::File&) { _textureIsDirty = true; }
    );
    
    addProperty(_nightFactor);
    
    addProperty(_transparency);

    setBoundingSphere(_size.value());
}

bool RenderableRings::isReady() const {
    return _shader && _texture;
}

bool RenderableRings::initialize() {
    if (!_shader) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("RingProgram",
            "${MODULE_BASE}/shaders/rings_vs.glsl",
            "${MODULE_BASE}/shaders/rings_fs.glsl"
            );
        if (!_shader)
            return false;

        _shader->setIgnoreUniformLocationError(
            ghoul::opengl::ProgramObject::IgnoreError::Yes
        );
    }

    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
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

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

    return true;
}

void RenderableRings::render(const RenderData& data) {
    _shader->activate();

    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", glm::mat4(_orientation * _state));
    _shader->setUniform("textureOffset", _offset);
    _shader->setUniform("transparency", _transparency);
    
    _shader->setUniform("hasSunPosition", _hasSunPosition);
    if (_hasSunPosition) {
        _shader->setUniform("_nightFactor", _nightFactor);
        _shader->setUniform("sunPosition", _sunPosition);
    }
    
    setPscUniforms(*_shader.get(), data.camera, data.position);

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
    
    if (!_frame.empty()) {
        _state = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
    }
    
    if (!_body.empty()) {
        double lt;
        _sunPosition = SpiceManager::ref().targetPosition(
            "SUN",
            _body,
            "GALACTIC",
            {},
            data.time,
            lt
        );
    }
}

void RenderableRings::loadTexture() {
    if (_texturePath.value() != "") {
        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));
        if (texture) {
            LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");
            _texture = std::move(texture);

            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
//            _texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
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
    // ============================
    //         GEOMETRY (quad)
    // ============================
    const GLfloat size = _size.value()[0];
    const GLfloat w = _size.value()[1];
    const GLfloat vertex_data[] = {
        //      x      y     z     w     s     t
        -size, -size, 0.f, w, 0.f, 0.f,
        size, size, 0.f, w, 1.f, 1.f,
        -size, size, 0.f, w, 0.f, 1.f,
        -size, -size, 0.f, w, 0.f, 0.f,
        size, -size, 0.f, w, 1.f, 0.f,
        size, size, 0.f, w, 1.f, 1.f,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

} // namespace openspace
