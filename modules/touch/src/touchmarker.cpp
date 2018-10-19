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

#include <modules/touch/include/touchmarker.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const std::array<const char*, 4> UniformNames = {
        "radius", "transparency", "thickness", "color"
    };

    constexpr openspace::properties::Property::PropertyInfo VisibilityInfo = {
        "Visibility",
        "Toggle visibility of markers",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusInfo = {
        "Size",
        "Marker radius",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Marker transparency",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ThicknessInfo = {
        "Thickness",
        "Marker thickness",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "MarkerColor", "Marker color", "" // @TODO Missing documentation
    };
} // namespace

namespace openspace {

TouchMarker::TouchMarker()
    : properties::PropertyOwner({ "TouchMarker" })
    , _visible(VisibilityInfo, true)
    , _radiusSize(RadiusInfo, 30.f, 0.f, 100.f)
    , _transparency(TransparencyInfo, 0.8f, 0.f, 1.f)
    , _thickness(ThicknessInfo, 2.f, 0.f, 4.f )
    , _color(
        ColorInfo,
        glm::vec3(204.f / 255.f, 51.f / 255.f, 51.f / 255.f),
        glm::vec3(0.f),
        glm::vec3(1.f)
    )
    , _shader(nullptr)
{
    addProperty(_visible);
    addProperty(_radiusSize);
    addProperty(_transparency);
    addProperty(_thickness);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);
}

TouchMarker::~TouchMarker() {} // NOLINT

void TouchMarker::initialize() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer

    _shader = global::renderEngine.buildRenderProgram(
        "MarkerProgram",
        absPath("${MODULE_TOUCH}/shaders/marker_vs.glsl"),
        absPath("${MODULE_TOUCH}/shaders/marker_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
}

void TouchMarker::deinitialize() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    if (_shader) {
        global::renderEngine.removeRenderProgram(_shader.get());
        _shader = nullptr;
    }
}

void TouchMarker::render(const std::vector<TUIO::TuioCursor>& list) {
    if (_visible && !list.empty()) {
        createVertexList(list);
        _shader->activate();

        _shader->setUniform(_uniformCache.radius, _radiusSize);
        _shader->setUniform(_uniformCache.transparency, _transparency);
        _shader->setUniform(_uniformCache.thickness, _thickness);
        _shader->setUniform(_uniformCache.color, _color.value());

        glEnable(GL_BLEND);
        glBlendEquation(GL_FUNC_ADD);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_PROGRAM_POINT_SIZE); // Enable gl_PointSize in vertex shader
        glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
        glBindVertexArray(_quad);
        glDrawArrays(GL_POINTS, 0, static_cast<int>(_vertexData.size() / 2));

        _shader->deactivate();
    }
}

void TouchMarker::createVertexList(const std::vector<TUIO::TuioCursor>& list) {
    _vertexData.resize(list.size() * 2);

    int i = 0;
    for (const TUIO::TuioCursor& c : list) {
        _vertexData[i] = 2 * (c.getX() - 0.5f);
        _vertexData[i + 1] = -2 * (c.getY() - 0.5f);
        i += 2;
    }

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexData.size() * sizeof(GLfloat),
        _vertexData.data(),
        GL_STATIC_DRAW
    );
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        2,
        GL_FLOAT,
        GL_FALSE,
        0,
        nullptr
    );
}

} // openspace namespace
