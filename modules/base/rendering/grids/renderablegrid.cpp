/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/base/rendering/grids/renderablegrid.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value determines the color of the grid lines that are rendered"
    };

    constexpr openspace::properties::Property::PropertyInfo HighlightColorInfo = {
        "HighlightColor",
        "Highlight Color",
        "This value determines the color of the highlighted lines in the grid"
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "This value specifies the number of segments that are used to render the "
        "grid in each direction"
    };

    constexpr openspace::properties::Property::PropertyInfo HighlightRateInfo = {
        "HighlightRate",
        "Highlight Rate",
        "The rate that the columns and rows are highlighted, counted with respect to the "
        "center of the grid. If the number of segments in the grid is odd, the "
        "highlighting might be offset from the center."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the grid"
    };

    constexpr openspace::properties::Property::PropertyInfo HighlightLineWidthInfo = {
        "HighlightLineWidth",
        "Highlight Line Width",
        "This value specifies the line width of the highlighted lines in the grid"
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Grid Size",
        "This value species the size of each dimensions of the grid"
    };

    constexpr openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden"
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo LabelsInfo =
    {
        "Labels",
        "Labels",
        "The labels for the grid"
    };

    struct [[codegen::Dictionary(RenderableGrid)]] Parameters {
        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(HighlightColorInfo.description)]]
        std::optional<glm::vec3> highlightColor [[codegen::color()]];

        // [[codegen::verbatim(SegmentsInfo.description)]]
        std::optional<glm::ivec2> segments;

        // [[codegen::verbatim(HighlightRateInfo.description)]]
        std::optional<glm::ivec2> highlightRate;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(HighlightLineWidthInfo.description)]]
        std::optional<float> highlightLineWidth;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<glm::vec2> size;

        // [[codegen::verbatim(DrawLabelInfo.description)]]
        std::optional<bool> drawLabels;

        // [[codegen::verbatim(LabelsInfo.description)]]
        std::optional<ghoul::Dictionary> labels
            [[codegen::reference("space_labelscomponent")]];
    };
#include "renderablegrid_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableGrid::Documentation() {
    return codegen::doc<Parameters>("base_renderable_grid");
}

RenderableGrid::RenderableGrid(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _color(ColorInfo, glm::vec3(0.5f), glm::vec3(0.f), glm::vec3(1.f))
    , _highlightColor(HighlightColorInfo, glm::vec3(0.8f), glm::vec3(0.f), glm::vec3(1.f))
    , _segments(SegmentsInfo, glm::uvec2(10), glm::uvec2(1), glm::uvec2(200))
    , _highlightRate(HighlightRateInfo, glm::uvec2(0), glm::uvec2(0), glm::uvec2(200))
    , _lineWidth(LineWidthInfo, 0.5f, 1.f, 20.f)
    , _highlightLineWidth(HighlightLineWidthInfo, 0.5f, 1.f, 20.f)
    , _size(SizeInfo, glm::vec2(1.f), glm::vec2(1.f), glm::vec2(1e11f))
    , _drawLabels(DrawLabelInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    // If no highlight color is specified then use the base color
    _highlightColor = p.highlightColor.value_or(_color);
    _highlightColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_highlightColor);

    _segments = p.segments.value_or(_segments);
    _segments.onChange([&]() { _gridIsDirty = true; });
    addProperty(_segments);

    _highlightRate = p.highlightRate.value_or(_highlightRate);
    _highlightRate.onChange([&]() { _gridIsDirty = true; });
    addProperty(_highlightRate);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    // If no highlight line width is specified then use the base line width
    _highlightLineWidth = p.highlightLineWidth.value_or(_lineWidth);
    addProperty(_highlightLineWidth);

    _size.setExponent(10.f);
    _size = p.size.value_or(_size);
    _size.onChange([&]() { _gridIsDirty = true; });
    addProperty(_size);

    if (p.labels.has_value()) {
        _drawLabels = p.drawLabels.value_or(_drawLabels);
        addProperty(_drawLabels);

        _labels = std::make_unique<LabelsComponent>(*p.labels);
        _hasLabels = true;
        addPropertySubOwner(_labels.get());
    }
}

bool RenderableGrid::isReady() const {
    return _hasLabels ? _gridProgram && _labels->isReady() : _gridProgram != nullptr;
}

void RenderableGrid::initialize() {
    if (_hasLabels) {
        _labels->initialize();
        _labels->loadLabels();
    }
}

void RenderableGrid::initializeGL() {
    _gridProgram = BaseModule::ProgramObjectManager.request(
        "GridProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "GridProgram",
                absPath("${MODULE_BASE}/shaders/grid_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/grid_fs.glsl")
            );
        }
    );

    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vBufferID);
    glGenVertexArrays(1, &_highlightVaoID);
    glGenBuffers(1, &_highlightVBufferID);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glBindVertexArray(_highlightVaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _highlightVBufferID);

    glEnableVertexAttribArray(0);
    glBindVertexArray(0);
}

void RenderableGrid::deinitializeGL() {
    glDeleteVertexArrays(1, &_vaoID);
    _vaoID = 0;
    glDeleteVertexArrays(1, &_highlightVaoID);
    _highlightVaoID = 0;

    glDeleteBuffers(1, &_vBufferID);
    _vBufferID = 0;
    glDeleteBuffers(1, &_highlightVBufferID);
    _highlightVBufferID = 0;

    BaseModule::ProgramObjectManager.release(
        "GridProgram",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _gridProgram = nullptr;
}

void RenderableGrid::render(const RenderData& data, RendererTasks&){
    _gridProgram->activate();

    const glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelMatrix;
    const glm::dmat4 projectionMatrix = data.camera.projectionMatrix();

    const glm::dmat4 modelViewProjectionMatrix = projectionMatrix * modelViewTransform;

    const glm::vec3 lookup = data.camera.lookUpVectorWorldSpace();
    const glm::vec3 viewDirection = data.camera.viewDirectionWorldSpace();
    glm::vec3 right = glm::cross(viewDirection, lookup);
    const glm::vec3 up = glm::cross(right, viewDirection);

    const glm::dmat4 worldToModelTransform = glm::inverse(modelMatrix);
    glm::vec3 orthoRight = glm::normalize(
        glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
    );

    if (orthoRight == glm::vec3(0.0)) {
        glm::vec3 otherVector = glm::vec3(lookup.y, lookup.x, lookup.z);
        right = glm::cross(viewDirection, otherVector);
        orthoRight = glm::normalize(
            glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
        );
    }

    _gridProgram->setUniform("modelViewTransform", modelViewTransform);
    _gridProgram->setUniform("MVPTransform", modelViewProjectionMatrix);
    _gridProgram->setUniform("opacity", opacity());
    _gridProgram->setUniform("gridColor", _color);

    // Change GL state:
#ifndef __APPLE__
    glLineWidth(_lineWidth);
#else
    glLineWidth(1.f);
#endif
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_LINE_SMOOTH);
    glEnable(GL_DEPTH_TEST);

    // Render minor grid
    glBindVertexArray(_vaoID);
    glDrawArrays(_mode, 0, static_cast<GLsizei>(_varray.size()));

    // Render major grid
#ifndef __APPLE__
    glLineWidth(_highlightLineWidth);
#else
    glLineWidth(1.f);
#endif
    _gridProgram->setUniform("gridColor", _highlightColor);

    glBindVertexArray(_highlightVaoID);
    glDrawArrays(_mode, 0, static_cast<GLsizei>(_highlightArray.size()));

    // Restore GL State
    glBindVertexArray(0);
    _gridProgram->deactivate();
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetLineState();
    global::renderEngine->openglStateCache().resetDepthState();

    // Draw labels
    if (_drawLabels && _hasLabels) {
        const glm::vec3 orthoUp = glm::normalize(
            glm::vec3(worldToModelTransform * glm::dvec4(up, 0.0))
        );
         _labels->render(data, modelViewProjectionMatrix, orthoRight, orthoUp);
    }
}

void RenderableGrid::update(const UpdateData&) {
    if (!_gridIsDirty) {
        return;
    }

    const glm::dvec2 halfSize = static_cast<glm::dvec2>(_size.value()) / 2.0;
    const glm::uvec2 nSegments = _segments.value();
    const glm::dvec2 step =
        static_cast<glm::dvec2>(_size.value()) / static_cast<glm::dvec2>(nSegments);

    const int nLines = (2 * nSegments.x * nSegments.y) + nSegments.x + nSegments.y;
    const int nVertices = 2 * nLines;
    _varray.clear();
    _varray.reserve(nVertices);
    _highlightArray.clear();
    _highlightArray.reserve(nVertices);
    // OBS! Could be optimized further by removing duplicate vertices

    // If the number of segments are uneven the center won't be completly centered
    const glm::uvec2 center = glm::uvec2(nSegments.x / 2.f, nSegments.y / 2.f);
    for (unsigned int i = 0; i < nSegments.x; ++i) {
        for (unsigned int j = 0; j < nSegments.y; ++j) {
            const double y0 = -halfSize.y + j * step.y;
            const double y1 = y0 + step.y;

            const double x0 = -halfSize.x + i * step.x;
            const double x1 = x0 + step.x;

            // Line in y direction
            bool shouldHighlight = false;
            if (_highlightRate.value().x != 0) {
                int dist = abs(static_cast<int>(i) - static_cast<int>(center.x));
                int rest = dist % _highlightRate.value().x;
                shouldHighlight = rest == 0;
            }

            if (shouldHighlight) {
                _highlightArray.push_back({ x0, y0, 0.0 });
                _highlightArray.push_back({ x0, y1, 0.0 });
            }
            else {
                _varray.push_back({ x0, y0, 0.0 });
                _varray.push_back({ x0, y1, 0.0 });
            }

            // Line in x direction
            shouldHighlight = false;
            if (_highlightRate.value().y != 0) {
                int dist = abs(static_cast<int>(j) - static_cast<int>(center.y));
                int rest = dist % _highlightRate.value().y;
                shouldHighlight = abs(rest) == 0;
            }

            if (shouldHighlight) {
                _highlightArray.push_back({ x0, y0, 0.0 });
                _highlightArray.push_back({ x1, y0, 0.0 });
            }
            else {
                _varray.push_back({ x0, y0, 0.0 });
                _varray.push_back({ x1, y0, 0.0 });
            }
        }
    }

    // last x row
    for (unsigned int i = 0; i < nSegments.x; ++i) {
        const double x0 = -halfSize.x + i * step.x;
        const double x1 = x0 + step.x;

        bool shouldHighlight = false;
        if (_highlightRate.value().y != 0) {
            int dist = abs(static_cast<int>(nSegments.y) - static_cast<int>(center.y));
            int rest = dist % _highlightRate.value().y;
            shouldHighlight = abs(rest) == 0;
        }

        if (shouldHighlight) {
            _highlightArray.push_back({ x0, halfSize.y, 0.0 });
            _highlightArray.push_back({ x1, halfSize.y, 0.0 });
        }
        else {
            _varray.push_back({ x0, halfSize.y, 0.0 });
            _varray.push_back({ x1, halfSize.y, 0.0 });
        }
    }

    // last y col
    for (unsigned int j = 0; j < nSegments.y; ++j) {
        const double y0 = -halfSize.y + j * step.y;
        const double y1 = y0 + step.y;

        bool shouldHighlight = false;
        if (_highlightRate.value().x != 0) {
            int dist = abs(static_cast<int>(nSegments.x) - static_cast<int>(center.x));
            int rest = dist % _highlightRate.value().x;
            shouldHighlight = abs(rest) == 0;
        }
        if (shouldHighlight) {
            _highlightArray.push_back({ halfSize.x, y0, 0.0 });
            _highlightArray.push_back({ halfSize.x, y1, 0.0 });
        }
        else {
            _varray.push_back({ halfSize.x, y0, 0.0 });
            _varray.push_back({ halfSize.x, y1, 0.0 });
        }
    }

    setBoundingSphere(glm::length(glm::dvec2(halfSize)));

    // Minor grid
    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        _varray.size() * sizeof(Vertex),
        _varray.data(),
        GL_STATIC_DRAW
    );
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_DOUBLE, GL_FALSE, sizeof(Vertex), nullptr);

    // Major grid
    glBindVertexArray(_highlightVaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _highlightVBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        _highlightArray.size() * sizeof(Vertex),
        _highlightArray.data(),
        GL_STATIC_DRAW
    );
    glVertexAttribPointer(0, 3, GL_DOUBLE, GL_FALSE, sizeof(Vertex), nullptr);

    glBindVertexArray(0);

    _gridIsDirty = false;
}

} // namespace openspace
