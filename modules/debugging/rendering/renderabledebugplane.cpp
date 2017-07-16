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

// @TODO: Clean up this class and make it not copy much code from RenderablePlane ---abock

#include <modules/debugging/rendering/renderabledebugplane.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/rendering/renderengine.h>
#include <modules/newhorizons/rendering/renderableplanetprojection.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    const char* _loggerCat = "RenderablePlaneTexture";
} // namespace

namespace openspace {

RenderableDebugPlane::RenderableDebugPlane(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texture("texture", "Texture", -1, -1, 255)
    , _billboard("billboard", "Billboard", false)
    , _size("size", "Size", 10.f, 0.f, std::pow(10.f, 25.f))
    , _origin(Origin::Center)
    , _shader(nullptr)
    , _quad(0)
    , _vertexPositionBuffer(0)
{
    dictionary.getValue("Size", _size);

    if (dictionary.hasKey("Name")){
        dictionary.getValue("Name", _nodeName);
    }

    if (dictionary.hasKey("Texture")) {
        int t;
        dictionary.getValue("Texture", t);
        _texture = t;
    }

    std::string origin;
    if (dictionary.getValue("Origin", origin)) {
        if (origin == "LowerLeft") {
            _origin = Origin::LowerLeft;
        }
        else if (origin == "LowerRight") {
            _origin = Origin::LowerRight;
        }
        else if (origin == "UpperLeft") {
            _origin = Origin::UpperLeft;
        }
        else if (origin == "UpperRight") {
            _origin = Origin::UpperRight;
        }
        else if (origin == "Center") {
            _origin = Origin::Center;
        }
    }

    // Attempt to get the billboard value
    bool billboard = false;
    if (dictionary.getValue("Billboard", billboard)) {
        _billboard = billboard;
    }

    int texture;
    if (dictionary.getValue("Texture", texture))
        _texture = texture;
    addProperty(_texture);

    addProperty(_billboard);

    addProperty(_size);
    _size.onChange([this](){ _planeIsDirty = true; });

    setBoundingSphere(_size);
}

RenderableDebugPlane::~RenderableDebugPlane() {
}

bool RenderableDebugPlane::isReady() const {
    bool ready = true;
    if (!_shader)
        ready &= false;
    return ready;
}

bool RenderableDebugPlane::initialize() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    if (_shader == nullptr) {
        // Plane Program

        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("PlaneProgram",
            "${MODULE_BASE}/shaders/plane_vs.glsl",
            "${MODULE_BASE}/shaders/plane_fs.glsl"
            );
        if (!_shader)
            return false;
    }


    return isReady();
}

bool RenderableDebugPlane::deinitialize() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

    return true;
}

void RenderableDebugPlane::render(const RenderData& data) {
    glm::mat4 transform = glm::mat4(1.0);
    if (_billboard)
        transform = glm::inverse(glm::mat4(data.camera.viewRotationMatrix()));

    // Activate shader
    _shader->activate();

    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", transform);
    setPscUniforms(*_shader.get(), data.camera, data.position);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_2D, _texture);
    _shader->setUniform("texture1", unit);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    _shader->deactivate();
}

void RenderableDebugPlane::update(const UpdateData& data) {
    if (_shader->isDirty())
        _shader->rebuildFromFile();

    if (_planeIsDirty)
        createPlane();
}

void RenderableDebugPlane::createPlane() {
    // ============================
    //         GEOMETRY (quad)
    // ============================
    const GLfloat size = _size;

    const GLfloat vertex_data[] = {
        //      x      y     z     w     s     t
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
        -size, size, 0.f, 0.f, 0.f, 1.f,
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, -size, 0.f, 0.f, 1.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
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
