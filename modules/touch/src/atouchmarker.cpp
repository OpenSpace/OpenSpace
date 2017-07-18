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

#include <modules/touch/include/TouchMarker.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

#include <ghoul/logging/logmanager.h>

namespace {
    const std::string _loggerCat = "TouchMarker";
    const int MAX_FINGERS = 20;
}
namespace openspace {

TouchMarker::TouchMarker()
    : properties::PropertyOwner("TouchMarker")
    , _visible("TouchMarkers visible", "Toggle visibility of markers", true)
    , _radiusSize("Marker size", "Marker radius", 30.f, 0.f, 100.f)
    , _transparency("Transparency of marker", "Marker transparency", 0.8f, 0.f, 1.f)
    , _thickness("Thickness of marker", "Marker thickness", 2.f, 0.f, 4.f)
    , _color(
        "MarkerColor",
        "Marker color",
        glm::vec3(204.f / 255.f, 51.f / 255.f, 51.f / 255.f),
        glm::vec3(0.f),
        glm::vec3(1.f)
    )
    , _shader(nullptr)
    , _numFingers(0)
{
    addProperty(_visible);
    addProperty(_radiusSize);
    addProperty(_transparency);
    addProperty(_thickness);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);
    
}

bool TouchMarker::initialize() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer

    try {
        _shader = OsEng.renderEngine().buildRenderProgram("MarkerProgram",
            "${MODULE_TOUCH}/shaders/marker_vs.glsl",
            "${MODULE_TOUCH}/shaders/marker_fs.glsl"
        );
    }
    catch (const ghoul::opengl::ShaderObject::ShaderCompileError& e) {
        LERRORC(e.component, e.what());
    }
    return (_shader != nullptr);
}

bool TouchMarker::deinitialize() {
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

void TouchMarker::render(const std::vector<TUIO::TuioCursor>& list) {
    if (_visible && !list.empty()) {
        createVertexList(list);
        _shader->activate();

        _shader->setUniform("radius", _radiusSize);
        _shader->setUniform("transparency", _transparency);
        _shader->setUniform("thickness", _thickness);
        _shader->setUniform("color", _color.value());

        glEnable(GL_BLEND);
        glBlendEquation(GL_FUNC_ADD);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_PROGRAM_POINT_SIZE); // Enable gl_PointSize in vertex shader
        glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
        glBindVertexArray(_quad);
        glDrawArrays(GL_POINTS, 0, _numFingers);

        _shader->deactivate();
    }
}

void TouchMarker::createVertexList(const std::vector<TUIO::TuioCursor>& list) {
    _numFingers = static_cast<int>(list.size());
    GLfloat vertexData[MAX_FINGERS];
    int i = 0;
    for (const TUIO::TuioCursor& c : list) {
        vertexData[i] = 2 * (c.getX() - 0.5f);
        vertexData[i + 1] = -2 * (c.getY() - 0.5f);
        i += 2;
    }

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        2,
        GL_FLOAT,
        GL_FALSE,
        0,
        0
    );
}

} // openspace namespace