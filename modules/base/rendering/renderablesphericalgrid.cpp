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

#include <modules/base/rendering/renderablesphericalgrid.h>

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

    constexpr openspace::properties::Property::PropertyInfo GridMatrixInfo = {
        "GridMatrix",
        "Grid Matrix",
        "This value specifies the local transformation matrix that defines the "
        "orientation of this grid relative to the parent's rotation."
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "This value specifies the number of segments that are used to render the "
        "surrounding sphere."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the spherical grid."
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusInfo = {
        "Radius",
        "Radius",
        "This value specifies the radius of the grid."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderableSphericalGrid::Documentation() {
    using namespace documentation;
    return {
        "RenderableSphericalGrid",
        "base_renderable_sphericalgrid",
        {
            {
                GridMatrixInfo.identifier,
                new DoubleMatrix4x4Verifier,
                Optional::Yes,
                GridMatrixInfo.description
            },
            {
                GridColorInfo.identifier,
                new DoubleVector4Verifier,
                Optional::Yes,
                GridColorInfo.description
            },
            {
                SegmentsInfo.identifier,
                new IntVerifier,
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
                RadiusInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                RadiusInfo.description
            }
        }
    };
}


RenderableSphericalGrid::RenderableSphericalGrid(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _gridProgram(nullptr)
    , _gridMatrix(GridMatrixInfo, glm::mat4(1.f))
    , _gridColor(
        GridColorInfo,
        glm::vec4(0.5f, 0.5, 0.5f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _segments(SegmentsInfo, 36, 4, 200)
    , _lineWidth(LineWidthInfo, 0.5f, 0.f, 20.f)
    , _radius(RadiusInfo, 1e20f, 1.f, 1e35f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableSphericalGrid"
    );

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    if (dictionary.hasKey(GridMatrixInfo.identifier)) {
        _gridMatrix = dictionary.value<glm::dmat4>(GridMatrixInfo.identifier);
    }
    addProperty(_gridMatrix);

    if (dictionary.hasKey(GridColorInfo.identifier)) {
        _gridColor = dictionary.value<glm::vec4>(GridColorInfo.identifier);
    }
    _gridColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_gridColor);

    if (dictionary.hasKey(SegmentsInfo.identifier)) {
        _segments = static_cast<int>(dictionary.value<double>(SegmentsInfo.identifier));
    }
    _segments.onChange([&]() { _gridIsDirty = true; });
    addProperty(_segments);

    if (dictionary.hasKey(LineWidthInfo.identifier)) {
        _lineWidth = static_cast<float>(
            dictionary.value<double>(LineWidthInfo.identifier)
        );
    }
    addProperty(_lineWidth);

    if (dictionary.hasKey(RadiusInfo.identifier)) {
        _radius = static_cast<float>(
            dictionary.value<double>(RadiusInfo.identifier)
        );
    }
    _radius.onChange([&]() { _gridIsDirty = true; });
    addProperty(_radius);
}

bool RenderableSphericalGrid::isReady() const {
    bool ready = true;
    ready &= (_gridProgram != nullptr);
    return ready;
}

void RenderableSphericalGrid::initializeGL() {
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
    glGenBuffers(1, &_iBufferID);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
    glEnableVertexAttribArray(0);
    glBindVertexArray(0);
}

void RenderableSphericalGrid::deinitializeGL() {
    glDeleteVertexArrays(1, &_vaoID);
    _vaoID = 0;

    glDeleteBuffers(1, &_vBufferID);
    _vBufferID = 0;

    glDeleteBuffers(1, &_iBufferID);
    _iBufferID = 0;

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _gridProgram = nullptr;
}

void RenderableSphericalGrid::render(const RenderData& data, RendererTasks&){
    _gridProgram->activate();

    _gridProgram->setUniform("opacity", _opacity);

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() *
                                          modelTransform;

    _gridProgram->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _gridProgram->setUniform("projectionTransform", data.camera.projectionMatrix());

    _gridProgram->setUniform("gridColor", _gridColor);

    glLineWidth(_lineWidth);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glEnable(GL_LINE_SMOOTH);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
    glDrawElements(_mode, _isize, GL_UNSIGNED_INT, nullptr);
    glBindVertexArray(0);

    _gridProgram->deactivate();
}

void RenderableSphericalGrid::update(const UpdateData&) {
    if (_gridIsDirty) {
        _isize = 6 * _segments * _segments;
        _vsize = (_segments + 1) * (_segments + 1);
        _varray.resize(_vsize);
        _iarray.resize(_isize);

        int nr = 0;
        const float fsegments = static_cast<float>(_segments);
        const float r = _radius;

        for (int nSegment = 0; nSegment <= _segments; ++nSegment) {
            // define an extra vertex around the y-axis due to texture mapping
            for (int j = 0; j <= _segments; j++) {
                const float fi = static_cast<float>(nSegment);
                const float fj = static_cast<float>(j);

                // inclination angle (north to south)
                const float theta = fi * glm::pi<float>() / fsegments * 2.f;  // 0 -> PI

                // azimuth angle (east to west)
                const float phi = fj * glm::pi<float>() * 2.0f / fsegments;  // 0 -> 2*PI

                const float x = r * sin(phi) * sin(theta);  //
                const float y = r * cos(theta);             // up
                const float z = r * cos(phi) * sin(theta);  //

                glm::vec3 normal = glm::vec3(x, y, z);
                if (!(x == 0.f && y == 0.f && z == 0.f)) {
                    normal = glm::normalize(normal);
                }

                glm::vec4 tmp(x, y, z, 1);
                glm::mat4 rot = glm::rotate(
                    glm::mat4(1),
                    glm::half_pi<float>(),
                    glm::vec3(1, 0, 0)
                );
                tmp = glm::vec4(_gridMatrix.value() * glm::dmat4(rot) * glm::dvec4(tmp));

                for (int i = 0; i < 3; i++) {
                    _varray[nr].location[i] = tmp[i];
                }
                ++nr;
            }
        }
        nr = 0;
        // define indices for all triangles
        for (int i = 1; i <= _segments; ++i) {
            for (int j = 0; j < _segments; ++j) {
                const int t = _segments + 1;
                _iarray[nr] = t * (i - 1) + j + 0; ++nr;
                _iarray[nr] = t * (i + 0) + j + 0; ++nr;
                _iarray[nr] = t * (i + 0) + j + 1; ++nr;
                _iarray[nr] = t * (i - 1) + j + 1; ++nr;
                _iarray[nr] = t * (i - 1) + j + 0; ++nr;
            }
        }

        glBindVertexArray(_vaoID);
        glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
        glBufferData(
            GL_ARRAY_BUFFER,
            _vsize * sizeof(Vertex),
            _varray.data(),
            GL_STATIC_DRAW
        );

        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), nullptr);

        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
        glBufferData(
            GL_ELEMENT_ARRAY_BUFFER,
            _isize * sizeof(int),
            _iarray.data(),
            GL_STATIC_DRAW
        );

        _gridIsDirty = false;
    }
}

} // namespace openspace
