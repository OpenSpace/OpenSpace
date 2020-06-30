
/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/base/rendering/grids/renderableradialgrid.h>

#include <modules/base/basemodule.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const char* ProgramName = "GridProgram";

    constexpr openspace::properties::Property::PropertyInfo GridColorInfo = {
        "GridColor",
        "Grid Color",
        "This value determines the color of the grid lines that are rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo GridSegmentsInfo = {
        "GridSegments",
        "Number of Grid Segments",
        "Specifies the number of segments for the grid, in the radial and angular "
        " direction respectively"
    };

    constexpr openspace::properties::Property::PropertyInfo CircleSegmentsInfo = {
        "CircleSegments",
        "Number of Circle Segments",
        "This value specifies the number of segments that is used to render each circle "
        "in the grid"
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the spherical grid."
    };

    constexpr openspace::properties::Property::PropertyInfo OuterRadiusInfo = {
        "OuterRadius",
        "Outer Radius",
        "The outer radius of the circular grid, i.e. its size."
    };

    constexpr openspace::properties::Property::PropertyInfo InnerRadiusInfo = {
        "InnerRadius",
        "Inner Radius",
        "The inner radius of the circular grid, that is the radius of the inmost ring. "
        "Must be smaller than the outer radius."
    };

} // namespace

namespace openspace {

documentation::Documentation RenderableRadialGrid::Documentation() {
    using namespace documentation;
    return {
        "RenderableRadialGrid",
        "base_renderable_radialgrid",
        {
            {
                GridColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                GridColorInfo.description
            },
            {
                GridSegmentsInfo.identifier,
                new DoubleVector2Verifier,
                Optional::Yes,
                GridSegmentsInfo.description
            },
            {
                CircleSegmentsInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                CircleSegmentsInfo.description
            },
            {
                LineWidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LineWidthInfo.description
            },
            {
                OuterRadiusInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                OuterRadiusInfo.description
            },
            {
                InnerRadiusInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                InnerRadiusInfo.description
            }
        }
    };
}


RenderableRadialGrid::RenderableRadialGrid(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _gridProgram(nullptr)
    , _gridColor(
        GridColorInfo,
        glm::vec3(0.5f, 0.5, 0.5f),
        glm::vec3(0.f),
        glm::vec3(1.f)
    )
    , _gridSegments(
        GridSegmentsInfo, 
        glm::ivec2(1, 1),  
        glm::ivec2(1),
        glm::ivec2(200)
    )
    , _circleSegments(CircleSegmentsInfo, 36, 4, 200)
    , _lineWidth(LineWidthInfo, 0.5f, 0.f, 20.f)
    , _maxRadius(OuterRadiusInfo, 1.f, 0.f, 20.f)
    , _minRadius(InnerRadiusInfo, 0.f, 0.f, 20.f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableRadialGrid"
    );

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    if (dictionary.hasKey(GridColorInfo.identifier)) {
        _gridColor = dictionary.value<glm::vec3>(GridColorInfo.identifier);
    }
    _gridColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_gridColor);

    if (dictionary.hasKey(GridSegmentsInfo.identifier)) {
        _gridSegments = static_cast<glm::ivec2>(
            dictionary.value<glm::vec2>(GridSegmentsInfo.identifier)
        );
    }
    _gridSegments.onChange([&]() { _gridIsDirty = true; });
    addProperty(_gridSegments);

    if (dictionary.hasKey(CircleSegmentsInfo.identifier)) {
        _circleSegments = static_cast<int>(
            dictionary.value<double>(CircleSegmentsInfo.identifier)
        );
    }
    _circleSegments.onChange([&]() {
        if (_circleSegments.value() % 2 == 1) {
            _circleSegments = _circleSegments - 1;
        }
        _gridIsDirty = true;
    });
    addProperty(_circleSegments);

    if (dictionary.hasKey(LineWidthInfo.identifier)) {
        _lineWidth = static_cast<float>(
            dictionary.value<double>(LineWidthInfo.identifier)
        );
    }
    addProperty(_lineWidth);

    if (dictionary.hasKey(OuterRadiusInfo.identifier)) {
        _maxRadius = static_cast<float>(
            dictionary.value<double>(OuterRadiusInfo.identifier)
        );
    }

    if (dictionary.hasKey(InnerRadiusInfo.identifier)) {
        _minRadius = static_cast<float>(
            dictionary.value<double>(InnerRadiusInfo.identifier)
        );
    }

    _maxRadius.setMinValue(_minRadius);
    _minRadius.setMaxValue(_maxRadius);

    _maxRadius.onChange([&]() {
        _gridIsDirty = true;
        _minRadius.setMaxValue(_maxRadius);
    });

    _minRadius.onChange([&]() {
        _gridIsDirty = true;
        _maxRadius.setMinValue(_minRadius);
    });

    addProperty(_maxRadius);
    addProperty(_minRadius);
}

bool RenderableRadialGrid::isReady() const {
    return _gridProgram != nullptr;
}

void RenderableRadialGrid::initializeGL() {
    _gridProgram = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_BASE}/shaders/grid_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/grid_fs.glsl")
            );
        }
    );
}

void RenderableRadialGrid::deinitializeGL() {
    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _gridProgram = nullptr;
}

void RenderableRadialGrid::render(const RenderData& data, RendererTasks&){
    _gridProgram->activate();

    _gridProgram->setUniform("opacity", _opacity);

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() *
                                          modelTransform;

    _gridProgram->setUniform("modelViewTransform", modelViewTransform);
    _gridProgram->setUniform(
        "MVPTransform",
        glm::dmat4(data.camera.projectionMatrix()) * modelViewTransform
    );
    
    _gridProgram->setUniform("gridColor", _gridColor);

    float adjustedLineWidth = 1.f;

#ifndef __APPLE__
    adjustedLineWidth = _lineWidth;
#endif

    // Saves current state:
    GLboolean isBlendEnabled = glIsEnabledi(GL_BLEND, 0);
    GLfloat currentLineWidth;
    glGetFloatv(GL_LINE_WIDTH, &currentLineWidth);
    GLboolean isLineSmoothEnabled = glIsEnabled(GL_LINE_SMOOTH);

    GLenum blendEquationRGB;
    GLenum blendEquationAlpha;
    GLenum blendDestAlpha;
    GLenum blendDestRGB;
    GLenum blendSrcAlpha;
    GLenum blendSrcRGB;
    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    // Changes GL state:
    glLineWidth(adjustedLineWidth);
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_LINE_SMOOTH);
    
    for (GeometryData &c : _circles) {
        c.render();
    }

    _lines.render();

    _gridProgram->deactivate();

    // Restores GL State
    glLineWidth(currentLineWidth);
    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);

    if (!isBlendEnabled) {
        glDisablei(GL_BLEND, 0);
    }

    if (!isLineSmoothEnabled) {
        glDisable(GL_LINE_SMOOTH);
    }
}

void RenderableRadialGrid::update(const UpdateData&) {
    if (_gridIsDirty) {

        // Circles
        const int nRadialSegments = _gridSegments.value()[0];
        const float fnCircles = static_cast<float>(nRadialSegments);
        const float deltaRadius = (_maxRadius - _minRadius) / fnCircles;

        const bool hasInnerRadius = _minRadius > 0;
        const int nCircles = hasInnerRadius ? nRadialSegments : nRadialSegments + 1;

        _circles.clear();
        _circles.reserve(nCircles);

        auto addRing = [&](int nSegments, float radius) {
            std::vector<rendering::helper::Vertex> vertices =
                rendering::helper::createRing(nSegments, radius);

            _circles.push_back(GeometryData(GL_LINE_STRIP));
            _circles.back().varray = rendering::helper::convert(vertices);
            _circles.back().update();
        };

        // add an extra inmost circle
        if (hasInnerRadius) {
            addRing(_circleSegments, _minRadius);
        }

        for (int i = 0; i < nRadialSegments; ++i) {
            float ri = static_cast<float>(i + 1) * deltaRadius;
            ri += _minRadius;
            addRing(_circleSegments, ri);
        }

        // Lines
        const int nLines = _gridSegments.value()[1];
        const int nVertices = 2 * nLines;
        const float fsegments = static_cast<float>(nLines);

        _lines.varray.clear();
        _lines.varray.reserve(nVertices);

        if (nLines > 1) {
            for (int i = 0; i < nLines; ++i) {
                const float fi = static_cast<float>(i);

                const float theta = fi * glm::pi<float>() * 2.0f / fsegments;  // 0 -> 2*PI

                float x = _maxRadius * cos(theta);
                float y = _maxRadius * sin(theta);
                float z = 0.0f;

                _lines.varray.push_back({ x, y, z });

                x = _minRadius * cos(theta);
                y = _minRadius * sin(theta);

                _lines.varray.push_back({ x, y, z });
            }
        }
        _lines.update();

        _gridIsDirty = false;
    }
}

RenderableRadialGrid::GeometryData::GeometryData(GLenum renderMode) 
    : mode(renderMode)
{
    glGenVertexArrays(1, &vao);
    glGenBuffers(1, &vbo);

    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glEnableVertexAttribArray(0);
    glBindVertexArray(0);
}

RenderableRadialGrid::GeometryData::GeometryData(GeometryData&& other) noexcept {
    if (this == &other) return;
    vao = other.vao;
    vbo = other.vbo;
    varray = std::move(other.varray);
    mode = other.mode;

    other.vao = 0;
    other.vbo = 0;
}

RenderableRadialGrid::GeometryData& 
RenderableRadialGrid::GeometryData::operator=(GeometryData&& other) noexcept {
    if (this != &other) {
        vao = other.vao;
        vbo = other.vbo;
        varray = std::move(other.varray);
        mode = other.mode;

        other.vao = 0;
        other.vbo = 0;
    }
    return *this;
}

RenderableRadialGrid::GeometryData::~GeometryData() {
    glDeleteVertexArrays(1, &vao);
    vao = 0;

    glDeleteBuffers(1, &vbo);
    vbo = 0;
}

void RenderableRadialGrid::GeometryData::update() {
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        varray.size() * sizeof(rendering::helper::VertexXYZ),
        varray.data(),
        GL_STATIC_DRAW
    );

    glVertexAttribPointer(
        0, 
        3, 
        GL_FLOAT, 
        GL_FALSE, 
        sizeof(rendering::helper::VertexXYZ), 
        nullptr
    );
}

void RenderableRadialGrid::GeometryData::render() {
    glBindVertexArray(vao);
    glDrawArrays(mode, 0, static_cast<GLsizei>(varray.size()));
    glBindVertexArray(0);
}

} // namespace openspace
