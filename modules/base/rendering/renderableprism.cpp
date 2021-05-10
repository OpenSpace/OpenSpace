/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#define _USE_MATH_DEFINES

#include <modules/base/rendering/renderableprism.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <math.h>
#include <optional>

namespace {
    constexpr const char _loggerCat[] = "RenderablePrism";

    constexpr const std::array<const char*, 2> UniformNames = {
        "modelViewProjectionTransform", "vs_color"
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Segments",
        "The number of segments the shape of the prism should have."
    };

    static const openspace::properties::Property::PropertyInfo RadiusInfo = {
        "Radius",
        "Radius",
        "The radius of the prism's shape in meters."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width."
    };

    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB color for the line."
    };

    constexpr openspace::properties::Property::PropertyInfo LengthInfo = {
        "Length",
        "Length",
        "The length of the prism in meters."
    };

    // Generate a vertices around the unit circle on the XY-plane
    void getUnitCircleVertices(std::vector<float>& vertices, int sectorCount) {
        float sectorStep = 2 * M_PI / sectorCount;
        float sectorAngle;  // in radians

        for (int i = 0; i < sectorCount; ++i) {
            sectorAngle = i * sectorStep;
            vertices.push_back(cos(sectorAngle)); // x
            vertices.push_back(sin(sectorAngle)); // y
        }
    }

    struct [[codegen::Dictionary(RenderablePrism)]] Parameters {
        // [[codegen::verbatim(SegmentsInfo.description)]]
        int segments;

        // [[codegen::verbatim(RadiusInfo.description)]]
        std::optional<float> radius;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(LineColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(LengthInfo.description)]]
        std::optional<float> length;
    };
#include "renderableprism_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderablePrism::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "base_renderable_prism";
    return doc;
}

RenderablePrism::RenderablePrism(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _nShapeSegments(SegmentsInfo, 6, 3, 32)
    , _radius(RadiusInfo, 10.f, 0.f, 3.0e12f)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
    , _lineColor(LineColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _length(LengthInfo, 20.f, 1.f, 3.0e12f)

{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _radius.setViewOption(properties::Property::ViewOptions::Logarithmic);
    _lineColor.setViewOption(properties::Property::ViewOptions::Color);
    _length.setViewOption(properties::Property::ViewOptions::Logarithmic);

    _nShapeSegments.onChange([&]() { _prismIsDirty = true; });
    _radius.onChange([&]() { _prismIsDirty = true; });
    _length.onChange([&]() { _prismIsDirty = true; });

    _nShapeSegments = p.segments;
    _radius = p.radius.value_or(_radius);
    _lineWidth = p.lineWidth.value_or(_lineWidth);
    _lineColor = p.color.value_or(_lineColor);
    _length = p.length.value_or(_length);

    addProperty(_nShapeSegments);
    addProperty(_radius);
    addProperty(_lineWidth);
    addProperty(_lineColor);
    addProperty(_length);
    addProperty(_opacity);
}

bool RenderablePrism::isReady() const {
    return _shader != nullptr;
}

void RenderablePrism::bindGL() {
    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vboId);
}

void RenderablePrism::unbindGL() {
    glBindVertexArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void RenderablePrism::initialize() {
    updateVertexData();
}

void RenderablePrism::initializeGL() {
    initializeShader();
    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);

    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vboId);
}

void RenderablePrism::deinitializeGL() {
    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;

    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vboId);
    _vboId = 0;
}

void RenderablePrism::updateVertexData() {
    _baseVertexArray.clear();
    _topVertexArray.clear();
    _linesVertexArray.clear();

    // Get unit circle vertices on the XY-plane
    std::vector<float> unitVertices;
    unitVertices.reserve(2 * _nShapeSegments);
    getUnitCircleVertices(unitVertices, _nShapeSegments);

    for (int j = 0, k = 0; j < _nShapeSegments && k < unitVertices.size(); ++j) {
        float ux = unitVertices[k++];
        float uy = unitVertices[k++];

        // Base
        _baseVertexArray.push_back(ux * _radius); // x
        _baseVertexArray.push_back(uy * _radius); // y
        _baseVertexArray.push_back(0.f);          // z

        // Top
        _topVertexArray.push_back(ux * _radius); // x
        _topVertexArray.push_back(uy * _radius); // y
        _topVertexArray.push_back(_length);      // z

        // Lines
        _linesVertexArray.push_back(ux * _radius); // x
        _linesVertexArray.push_back(uy * _radius); // y
        _linesVertexArray.push_back(0.f);          // z

        _linesVertexArray.push_back(ux * _radius); // x
        _linesVertexArray.push_back(uy * _radius); // y
        _linesVertexArray.push_back(_length);      // z
    }
}

void RenderablePrism::render(const RenderData& data, RendererTasks&) {
    _shader->activate();

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::mat4 modelViewProjectionTransform =
        data.camera.projectionMatrix() *
        glm::mat4(data.camera.combinedViewMatrix() * modelTransform);

    // Uniforms
    _shader->setUniform(_uniformCache.modelViewProjection, modelViewProjectionTransform);
    _shader->setUniform(_uniformCache.color, glm::vec4(_lineColor.value(), _opacity));

    // Render
    glLineWidth(_lineWidth);
    bindGL();

    // Base
    glBufferData(
        GL_ARRAY_BUFFER,
        _baseVertexArray.size() * sizeof(float),
        _baseVertexArray.data(),
        GL_STREAM_DRAW
    );

    glEnableVertexAttribArray(_locVertex);
    glVertexAttribPointer(_locVertex, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);

    glDrawArrays(GL_LINE_LOOP, 0, static_cast<int>(_nShapeSegments));

    // Top
    glBufferData(
        GL_ARRAY_BUFFER,
        _topVertexArray.size() * sizeof(float),
        _topVertexArray.data(),
        GL_STREAM_DRAW
    );

    glDrawArrays(GL_LINE_LOOP, 0, static_cast<int>(_nShapeSegments));

    // Lines
    glBufferData(
        GL_ARRAY_BUFFER,
        _linesVertexArray.size() * sizeof(float),
        _linesVertexArray.data(),
        GL_STREAM_DRAW
    );

    glDrawArrays(GL_LINES, 0, static_cast<int>(2 * _nShapeSegments));

    unbindGL();
    global::renderEngine->openglStateCache().resetLineState();

    _shader->deactivate();
}

void RenderablePrism::update(const UpdateData&) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        updateUniformLocations();
    }
    if (_prismIsDirty) {
        updateVertexData();
        _prismIsDirty = false;
    }
}

void RenderablePrism::initializeShader() {
    _shader = global::renderEngine->buildRenderProgram(
        "PrismProgram",
        absPath("${MODULE_BASE}/shaders/prism_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/prism_fs.glsl")
    );
    updateUniformLocations();
}

void RenderablePrism::updateUniformLocations() {
   ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
}

} // namespace openspace
