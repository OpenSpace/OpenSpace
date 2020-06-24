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

#include <modules/base/rendering/grids/renderablegrid.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const char* ProgramName = "GridProgram";

    constexpr openspace::properties::Property::PropertyInfo GridColorInfo = {
        "GridColor",
        "Grid Color",
        "This value determines the color of the grid lines that are rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "This value specifies the number of segments that are used to render the "
        "grid in each direction."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the grid."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Grid Size",
        "This value species the size of each dimensions of the grid"
    };

} // namespace

namespace openspace {

documentation::Documentation RenderableGrid::Documentation() {
    using namespace documentation;
    return {
        "RenderableGrid",
        "base_renderable_grid",
        {
            {
                GridColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                GridColorInfo.description
            },
            {
                SegmentsInfo.identifier,
                new DoubleVector2Verifier, // TODO: Should be Int, but specification test fails...
                Optional::Yes,
                SegmentsInfo.description
            },
            {
                LineWidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LineWidthInfo.description
            },
            {
                SizeInfo.identifier,
                new DoubleVector2Verifier,
                Optional::Yes,
                SizeInfo.description
            }
        }
    };
}

RenderableGrid::RenderableGrid(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _gridColor(
        GridColorInfo,
        glm::vec3(0.5f, 0.5, 0.5f),
        glm::vec3(0.f),
        glm::vec3(1.f)
    )
    , _segments(SegmentsInfo, glm::uvec2(10), glm::uvec2(1), glm::uvec2(200)) // TODO: review range
    , _lineWidth(LineWidthInfo, 0.5f, 0.f, 20.f)
    , _size(SizeInfo, glm::vec2(1e20f), glm::vec2(1.f), glm::vec2(1e35f))
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableGrid"
    );

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    if (dictionary.hasKey(GridColorInfo.identifier)) {
        _gridColor = dictionary.value<glm::vec3>(GridColorInfo.identifier);
    }
    _gridColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_gridColor);

    if (dictionary.hasKey(SegmentsInfo.identifier)) {
        _segments = static_cast<glm::uvec2>(
            dictionary.value<glm::vec2>(SegmentsInfo.identifier)
        );
    }
    _segments.onChange([&]() { _gridIsDirty = true; });
    addProperty(_segments);

    if (dictionary.hasKey(LineWidthInfo.identifier)) {
        _lineWidth = static_cast<float>(
            dictionary.value<double>(LineWidthInfo.identifier)
        );
    }
    addProperty(_lineWidth);

    if (dictionary.hasKey(SizeInfo.identifier)) {
        _size = dictionary.value<glm::vec2>(SizeInfo.identifier);
    }
    _size.onChange([&]() { _gridIsDirty = true; });
    addProperty(_size);
}

bool RenderableGrid::isReady() const {
    return _gridProgram != nullptr;
}

void RenderableGrid::initializeGL() {
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

    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vBufferID);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glEnableVertexAttribArray(0);
    glBindVertexArray(0);
}

void RenderableGrid::deinitializeGL() {
    glDeleteVertexArrays(1, &_vaoID);
    _vaoID = 0;

    glDeleteBuffers(1, &_vBufferID);
    _vBufferID = 0;

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _gridProgram = nullptr;
}

void RenderableGrid::render(const RenderData& data, RendererTasks&){
    _gridProgram->activate();

    _gridProgram->setUniform("opacity", _opacity);

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _gridProgram->setUniform("modelViewTransform", modelViewTransform);
    _gridProgram->setUniform(
        "MVPTransform",
        glm::dmat4(data.camera.projectionMatrix()) * modelViewTransform
    );

    _gridProgram->setUniform("gridColor", _gridColor);

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
    glLineWidth(_lineWidth);
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_LINE_SMOOTH);

    glBindVertexArray(_vaoID);
    glDrawArrays(_mode, 0, static_cast<GLsizei>(_varray.size()));
    glBindVertexArray(0);

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

void RenderableGrid::update(const UpdateData&) {
    if (_gridIsDirty) {
        const glm::vec2 halfSize = _size.value() / 2.f;
        const glm::uvec2 nSegments = _segments.value();
        const glm::vec2 step = _size.value() / static_cast<glm::vec2>(nSegments); 

        const int nLines = (2 * nSegments.x * nSegments.y) + nSegments.x + nSegments.y;
        const int nVertices = 2 * nLines;
        _varray.resize(nVertices);
        // OBS! Could be optimized further by removing duplicate vertices
       
        int nr = 0;
        for (unsigned int i = 0; i < nSegments.x; ++i) {
            for (unsigned int j = 0; j < nSegments.y; ++j) {
                float y0 = -halfSize.y + j * step.y;
                float y1 = y0 + step.y;

                float x0 = -halfSize.x + i * step.x;
                float x1 = x0 + step.x;

                _varray[nr++] = { x0, y0, 0.f };
                _varray[nr++] = { x0, y1, 0.f };

                _varray[nr++] = { x0, y0, 0.f };
                _varray[nr++] = { x1, y0, 0.f };
            }
        }

        // last x row
        for (unsigned int i = 0; i < nSegments.x; ++i) {
            float x0 = -halfSize.x + i * step.x;
            float x1 = x0 + step.x;
            _varray[nr++] = { x0, halfSize.y, 0.f };
            _varray[nr++] = { x1, halfSize.y, 0.f };
        }

        // last y col
        for (unsigned int i = 0; i < nSegments.y; ++i) {
            float y0 = -halfSize.y + i * step.y;
            float y1 = y0 + step.y;
            _varray[nr++] = { halfSize.x, y0, 0.f };
            _varray[nr++] = { halfSize.x, y1, 0.f };
        }

        glBindVertexArray(_vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _varray.size() * sizeof(Vertex),
            _varray.data(),
            GL_STATIC_DRAW
        );

        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), nullptr);

        glBindVertexArray(0);

        _gridIsDirty = false;
    }
}

} // namespace openspace
