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

#include <openspace/interaction/touchmarker.h>

#include <openspace/engine/globals.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/opengl/programobject.h>
#include <openspace/rendering/renderengine.h>

namespace {
    using namespace openspace;

    constexpr BoolPropertyInfo VisibilityInfo = {
        .identifier = "Enabled",
        .name = "Show touch markers",
        .description = "Decides whether to show circular markers on touch interaction.",
        .visibility = Visibility::NoviceUser,
        .defaults = {
            .value = true
        }
    };

    constexpr FloatPropertyInfo RadiusInfo = {
        .identifier = "Size",
        .name = "Marker size",
        .description = "Controls the size of the touch input markers.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 30.f,
            .minMax = { 0.f, 100.f }
        }
    };

    constexpr FloatPropertyInfo OpacityInfo = {
        .identifier = "Opacity",
        .name = "Marker opacity",
        .description = "The opacity of the touch markers.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 0.6f,
            .minMax = { 0.f, 1.f }
        }
    };

    constexpr Vec3PropertyInfo ColorInfo = {
        .identifier = "MarkerColor",
        .name = "Marker color",
        .description = "The color of the touch markers.",
        .visibility = Visibility::User,
        .defaults = {
            .value = glm::vec3(1.f),
            .minMax = { glm::vec3(0.f), glm::vec3(1.f) }
        }
    };
} // namespace

namespace openspace {

TouchMarker::TouchMarker()
    : PropertyOwner({ "TouchMarkers", "Touch Markers" })
    , _enabled(VisibilityInfo)
    , _radiusSize(RadiusInfo)
    , _opacity(OpacityInfo)
    , _color(ColorInfo)
{
    addProperty(_enabled);
    addProperty(_radiusSize);
    addProperty(_opacity);
    _color.setViewOption(Property::ViewOptions::Color);
    addProperty(_color);
}

TouchMarker::~TouchMarker() {}

void TouchMarker::initializeGL() {
    glCreateBuffers(1, &_vbo);
    glCreateVertexArrays(1, &_vao);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, 2 * sizeof(float));

    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 2, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);

    _shader = global::renderEngine->buildRenderProgram(
        "TouchMarkerProgram",
        absPath("${SHADERS}/core/touchmarker_vs.glsl"),
        absPath("${SHADERS}/core/touchmarker_fs.glsl")
    );

    opengl::updateUniformLocations(*_shader, _uniformCache);
}

void TouchMarker::deinitializeGL() {
    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);

    if (_shader) {
        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
    }
}

void TouchMarker::render(const std::vector<TouchInputHolder>& touchPoints) {
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
    glBindVertexArray(_vao);
    glDrawArrays(GL_POINTS, 0, _count);

    _shader->deactivate();
}

void TouchMarker::createVertexList(
                              const std::vector<openspace::TouchInputHolder>& touchPoints)
{
    std::vector<GLfloat> vertexData;
    vertexData.resize(touchPoints.size() * 2);

    int i = 0;
    for (const TouchInputHolder& touchPoint : touchPoints) {
        vertexData[i] = 2.f * (touchPoint.latestInput().pos.x - 0.5f);
        vertexData[i + 1] = -2.f * (touchPoint.latestInput().pos.y - 0.5f);
        i += 2;
    }

    glNamedBufferData(
        _vbo,
        vertexData.size() * sizeof(GLfloat),
        vertexData.data(),
        GL_STATIC_DRAW
    );
    _count = static_cast<GLsizei>(touchPoints.size());
}

} // namespace openspace
