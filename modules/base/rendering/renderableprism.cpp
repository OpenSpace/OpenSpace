/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/base/rendering/renderableprism.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Segments",
        "The number of segments the shape of the prism should have.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LinesInfo = {
        "NumLines",
        "Number of Lines",
        "The number of lines connecting the two shapes of the prism. They will be evenly "
        "distributed around the bounding circle that makes up the shape of the prism.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusInfo = {
        "Radius",
        "Radius",
        "The radius of the prism's shape in meters.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo BaseRadiusInfo = {
        "BaseRadius",
        "Base Radius",
        "The radius of the base of the prism's shape, in meters. By default it is given "
        "the same radius as the outer shape.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The width of the lines. The larger number, the thicker the lines.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "The RGB color of the line.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo LengthInfo = {
        "Length",
        "Length",
        "The length of the prism in meters.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderablePrism)]] Parameters {
        // [[codegen::verbatim(SegmentsInfo.description)]]
        int segments;

        // [[codegen::verbatim(LinesInfo.description)]]
        std::optional<int> lines;

        // [[codegen::verbatim(RadiusInfo.description)]]
        std::optional<float> radius;

        // [[codegen::verbatim(BaseRadiusInfo.description)]]
        std::optional<float> baseRadius;

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
    return codegen::doc<Parameters>("base_renderable_prism");
}

RenderablePrism::RenderablePrism(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _nShapeSegments(SegmentsInfo, 6, 3, 32)
    , _nLines(LinesInfo, 6, 0, 32)
    , _radius(RadiusInfo, 10.f, 0.f, 3e20f)
    , _baseRadius(BaseRadiusInfo, 10.f, 0.f, 3e20f)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
    , _lineColor(LineColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _length(LengthInfo, 20.f, 1.f, 3e20f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _nShapeSegments.onChange([this]() { _prismIsDirty = true; });
    _nShapeSegments = p.segments;
    addProperty(_nShapeSegments);

    _nLines.onChange([this]() { _prismIsDirty = true; });
    _nLines = p.lines.value_or(_nShapeSegments);
    addProperty(_nLines);

    _radius.setExponent(12.f);
    _radius.onChange([this]() { _prismIsDirty = true; });
    _radius = p.radius.value_or(_radius);
    addProperty(_radius);

    _baseRadius.setExponent(12.f);
    _baseRadius.onChange([this]() { _prismIsDirty = true; });
    // Use the "regular" radius as default if no value was provided
    _baseRadius = p.baseRadius.value_or(_radius);
    addProperty(_baseRadius);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _lineColor.setViewOption(properties::Property::ViewOptions::Color);
    _lineColor = p.color.value_or(_lineColor);
    addProperty(_lineColor);

    _length.setExponent(12.f);
    _length.onChange([this]() { _prismIsDirty = true; });
    _length = p.length.value_or(_length);
    addProperty(_length);

    addProperty(Fadeable::_opacity);
}

bool RenderablePrism::isReady() const {
    return _shader != nullptr;
}

void RenderablePrism::initialize() {
    updateVertexData();
}

void RenderablePrism::initializeGL() {
    _shader = global::renderEngine->buildRenderProgram(
        "PrismProgram",
        absPath("${MODULE_BASE}/shaders/prism_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/prism_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);

    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vboId);
    glGenBuffers(1, &_iboId);

    glBindVertexArray(_vaoId);

    updateBufferData();

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);
    glBindVertexArray(0);
}

void RenderablePrism::deinitializeGL() {
    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;

    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vboId);
    _vboId = 0;

    glDeleteBuffers(1, &_iboId);
    _iboId = 0;
}

void RenderablePrism::updateVertexData() {
    _vertexArray.clear();
    _indexArray.clear();

    using namespace rendering::helper;

    // Get unit circle vertices on the XY-plane
    std::vector<VertexXYZ> unitVertices = createRingXYZ(_nShapeSegments.value(), 1.f);
    std::vector<VertexXYZ> unitVerticesLines = createRingXYZ(_nLines.value(), 1.f);

    // Put base vertices into array
    for (int j = 0; j < _nShapeSegments; j++) {
        const float ux = unitVertices[j].xyz[0];
        const float uy = unitVertices[j].xyz[1];

        _vertexArray.push_back(ux * _baseRadius); // x
        _vertexArray.push_back(uy * _baseRadius); // y
        _vertexArray.push_back(0.f);              // z
    }

    // Put top shape vertices into array
    for (int j = 0; j < _nShapeSegments; j++) {
        const float ux = unitVertices[j].xyz[0];
        const float uy = unitVertices[j].xyz[1];

        _vertexArray.push_back(ux * _radius); // x
        _vertexArray.push_back(uy * _radius); // y
        _vertexArray.push_back(_length);      // z
    }

    // Put the vertices for the connecting lines into array
    if (_nLines == 1) {
        // In the case of just one line then connect the center points instead
        // Center for base shape
        _vertexArray.push_back(0.f);
        _vertexArray.push_back(0.f);
        _vertexArray.push_back(0.f);

        // Center for top shape
        _vertexArray.push_back(0.f);
        _vertexArray.push_back(0.f);
        _vertexArray.push_back(_length);
    }
    else {
        for (int j = 0; j < _nLines; j++) {
            const float ux = unitVerticesLines[j].xyz[0];
            const float uy = unitVerticesLines[j].xyz[1];

            // Base
            _vertexArray.push_back(ux * _baseRadius); // x
            _vertexArray.push_back(uy * _baseRadius); // y
            _vertexArray.push_back(0.f);              // z

            // Top
            _vertexArray.push_back(ux * _radius); // x
            _vertexArray.push_back(uy * _radius); // y
            _vertexArray.push_back(_length);      // z
        }
    }

    // Indices for Base shape
    ghoul_assert(
        _nShapeSegments.value() <= std::numeric_limits<uint8_t>::max(),
        "Too many shape segments"
    );
    for (uint8_t i = 0; i < _nShapeSegments; i++) {
        _indexArray.push_back(i);
    }

    // Reset
    _indexArray.push_back(255);

    // Indices for Top shape
    for (int i = _nShapeSegments; i < 2 * _nShapeSegments; i++) {
        _indexArray.push_back(static_cast<uint8_t>(i));
    }

    // Indices for connecting lines
    for (int i = 0, k = 0; i < _nLines; i++, k += 2) {
        // Reset
        _indexArray.push_back(255);

        _indexArray.push_back(static_cast<uint8_t>(2 * _nShapeSegments + k));
        _indexArray.push_back(static_cast<uint8_t>(2 * _nShapeSegments + k + 1));
    }
}

void RenderablePrism::updateBufferData() {
    glBindBuffer(GL_ARRAY_BUFFER, _vboId);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexArray.size() * sizeof(float),
        _vertexArray.data(),
        GL_STREAM_DRAW
    );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboId);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        _indexArray.size() * sizeof(uint8_t),
        _indexArray.data(),
        GL_STREAM_DRAW
    );
}

void RenderablePrism::render(const RenderData& data, RendererTasks&) {
    _shader->activate();

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 modelViewProjectionTransform =
        calcModelViewProjectionTransform(data);

    // Uniforms
    _shader->setUniform(
        _uniformCache.modelViewProjectionTransform,
        glm::mat4(modelViewProjectionTransform)
    );
    _shader->setUniform(_uniformCache.color, glm::vec4(_lineColor.value(), opacity()));

    // Render
    glEnable(GL_PRIMITIVE_RESTART);
    glPrimitiveRestartIndex(255);
    glLineWidth(_lineWidth);
    glBindVertexArray(_vaoId);

    glDrawElements(
        GL_LINE_LOOP,
        static_cast<GLsizei>(_indexArray.size()),
        GL_UNSIGNED_BYTE,
        nullptr
    );

    glBindVertexArray(0);
    global::renderEngine->openglStateCache().resetLineState();
    glDisable(GL_PRIMITIVE_RESTART);

    _shader->deactivate();
}

void RenderablePrism::update(const UpdateData& data) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
    }
    if (_prismIsDirty) {
        updateVertexData();
        updateBufferData();
        setBoundingSphere(_length * glm::compMax(data.modelTransform.scale));
        _prismIsDirty = false;
    }
}

} // namespace openspace
