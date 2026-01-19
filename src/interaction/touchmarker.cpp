/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/interaction/touchmarker.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo VisibilityInfo = {
        "Enabled",
        "Show touch markers",
        "Decides whether to show circular markers on touch interaction.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusInfo = {
        "Size",
        "Marker size",
        "Controls the size of the touch input markers.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Marker opacity",
        "The opacity of the touch markers.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "MarkerColor",
        "Marker color",
        "The color of the touch markers.",
        openspace::properties::Property::Visibility::User
    };
} // namespace

namespace openspace {

TouchMarker::TouchMarker()
    : properties::PropertyOwner({ "TouchMarkers", "Touch Markers" })
    , _enabled(VisibilityInfo, true)
    , _radiusSize(RadiusInfo, 30.f, 0.f, 100.f)
    , _opacity(OpacityInfo, 0.6f, 0.f, 1.f)
    , _color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
{
    addProperty(_enabled);
    addProperty(_radiusSize);
    addProperty(_opacity);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);
}

TouchMarker::~TouchMarker() {}

void TouchMarker::initializeGL() {
    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);

    _shader = global::renderEngine->buildRenderProgram(
        "TouchMarkerProgram",
        absPath("${SHADERS}/core/touchmarker_vs.glsl"),
        absPath("${SHADERS}/core/touchmarker_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
}

void TouchMarker::deinitializeGL() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    if (_shader) {
        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
    }
}

void TouchMarker::render(const std::vector<openspace::TouchInputHolder>& touchPoints) {
    if (!_enabled || touchPoints.empty()) {
        return;
    }

    createVertexList(touchPoints);

    _shader->activate();
    _shader->setUniform(_uniformCache.radius, _radiusSize);
    _shader->setUniform(_uniformCache.opacity, _opacity);
    _shader->setUniform(_uniformCache.color, _color.value());

    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_PROGRAM_POINT_SIZE); // Enable gl_PointSize in vertex shader
    glBindVertexArray(_quad);
    glDrawArrays(GL_POINTS, 0, static_cast<int>(_vertexData.size() / 2));

    _shader->deactivate();
}

void TouchMarker::createVertexList(
                              const std::vector<openspace::TouchInputHolder>& touchPoints)
{
    _vertexData.resize(touchPoints.size() * 2);

    int i = 0;
    for (const openspace::TouchInputHolder& touchPoint : touchPoints) {
        _vertexData[i] = 2.f * (touchPoint.latestInput().x - 0.5f);
        _vertexData[i + 1] = -2.f * (touchPoint.latestInput().y - 0.5f);
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
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, nullptr);
}

} // openspace namespace
