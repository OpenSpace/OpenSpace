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

#include <modules/base/rendering/renderableprism.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <limits>
#include <optional>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Segments",
        "The number of segments the shape of the prism should have.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo LinesInfo = {
        "NumLines",
        "Number of lines",
        "The number of lines connecting the two shapes of the prism. They will be evenly "
        "distributed around the bounding circle that makes up the shape of the prism.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo RadiusInfo = {
        "Radius",
        "Radius",
        "The radius of the prism's shape in meters.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo BaseRadiusInfo = {
        "BaseRadius",
        "Base radius",
        "The radius of the base of the prism's shape, in meters. By default it is given "
        "the same radius as the outer shape.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line width",
        "The width of the lines. The larger number, the thicker the lines.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "The RGB color of the line.",
        Property::Visibility::NoviceUser
    };

    constexpr Property::PropertyInfo LengthInfo = {
        "Length",
        "Length",
        "The length of the prism in meters.",
        Property::Visibility::User
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
} // namespace
#include "renderableprism_codegen.cpp"

namespace openspace {

Documentation RenderablePrism::Documentation() {
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

    _lineColor.setViewOption(Property::ViewOptions::Color);
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

void RenderablePrism::initializeGL() {
    _shader = global::renderEngine->buildRenderProgram(
        "PrismProgram",
        absPath("${MODULE_BASE}/shaders/prism_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/prism_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);

    glCreateVertexArrays(1, &_vao);
    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);

    updateVertexData();
}

void RenderablePrism::deinitializeGL() {
    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;

    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);
    glDeleteBuffers(1, &_ibo);
}

void RenderablePrism::updateVertexData() {
    using namespace rendering;

    std::vector<float> vertexArray;

    // Get unit circle vertices on the XY-plane
    std::vector<VertexXYZ> unitVertices = createRingXYZ(_nShapeSegments, 1.f);
    std::vector<VertexXYZ> unitVerticesLines = createRingXYZ(_nLines, 1.f);

    // Put base vertices into array
    for (int j = 0; j < _nShapeSegments; j++) {
        const float ux = unitVertices[j].position.x;
        const float uy = unitVertices[j].position.y;

        vertexArray.push_back(ux * _baseRadius); // x
        vertexArray.push_back(uy * _baseRadius); // y
        vertexArray.push_back(0.f);              // z
    }

    // Put top shape vertices into array
    for (int j = 0; j < _nShapeSegments; j++) {
        const float ux = unitVertices[j].position.x;
        const float uy = unitVertices[j].position.y;

        vertexArray.push_back(ux * _radius); // x
        vertexArray.push_back(uy * _radius); // y
        vertexArray.push_back(_length);      // z
    }

    // Put the vertices for the connecting lines into array
    if (_nLines == 1) {
        // In the case of just one line then connect the center points instead
        // Center for base shape
        vertexArray.push_back(0.f);
        vertexArray.push_back(0.f);
        vertexArray.push_back(0.f);

        // Center for top shape
        vertexArray.push_back(0.f);
        vertexArray.push_back(0.f);
        vertexArray.push_back(_length);
    }
    else {
        for (int j = 0; j < _nLines; j++) {
            const float ux = unitVerticesLines[j].position.x;
            const float uy = unitVerticesLines[j].position.y;

            // Base
            vertexArray.push_back(ux * _baseRadius); // x
            vertexArray.push_back(uy * _baseRadius); // y
            vertexArray.push_back(0.f);              // z

            // Top
            vertexArray.push_back(ux * _radius); // x
            vertexArray.push_back(uy * _radius); // y
            vertexArray.push_back(_length);      // z
        }
    }

    // Indices for Base shape
    ghoul_assert(
        _nShapeSegments.value() <= std::numeric_limits<uint8_t>::max(),
        "Too many shape segments"
    );
    std::vector<uint8_t> indexArray;

    for (uint8_t i = 0; i < _nShapeSegments; i++) {
        indexArray.push_back(i);
    }

    // Reset
    indexArray.push_back(255);

    // Indices for Top shape
    for (int i = _nShapeSegments; i < 2 * _nShapeSegments; i++) {
        indexArray.push_back(static_cast<uint8_t>(i));
    }

    // Indices for connecting lines
    for (int i = 0, k = 0; i < _nLines; i++, k += 2) {
        // Reset
        indexArray.push_back(255);

        indexArray.push_back(static_cast<uint8_t>(2 * _nShapeSegments + k));
        indexArray.push_back(static_cast<uint8_t>(2 * _nShapeSegments + k + 1));
    }

    glDeleteBuffers(1, &_vbo);
    glCreateBuffers(1, &_vbo);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, 3 * sizeof(float));
    glNamedBufferStorage(
        _vbo,
        vertexArray.size() * sizeof(float),
        vertexArray.data(),
        GL_NONE_BIT
    );

    glDeleteBuffers(1, &_ibo);
    glCreateBuffers(1, &_ibo);
    glVertexArrayElementBuffer(_vao, _ibo);
    glNamedBufferStorage(
        _ibo,
        indexArray.size() * sizeof(uint8_t),
        indexArray.data(),
        GL_NONE_BIT
    );

    _count = static_cast<GLsizei>(indexArray.size());
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
    glBindVertexArray(_vao);

    glDrawElements(GL_LINE_LOOP, _count, GL_UNSIGNED_BYTE, nullptr);

    glBindVertexArray(0);
    global::renderEngine->openglStateCache().resetLineState();
    glDisable(GL_PRIMITIVE_RESTART);

    _shader->deactivate();
}

void RenderablePrism::update(const UpdateData& data) {
    if (_shader->isDirty()) [[unlikely]] {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
    }

    if (_prismIsDirty) [[unlikely]] {
        updateVertexData();
        setBoundingSphere(_length * glm::compMax(data.modelTransform.scale));
        _prismIsDirty = false;
    }
}

} // namespace openspace
