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

#include <modules/base/rendering/renderablecartesianaxes.h>

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
    constexpr const char* ProgramName = "CartesianAxesProgram";
    const int NVertexIndices = 6;

    constexpr openspace::properties::Property::PropertyInfo XColorInfo = {
        "XColor",
        "X Color",
        "This value determines the color of the x axis."
    };

    constexpr openspace::properties::Property::PropertyInfo YColorInfo = {
        "YColor",
        "Y Color",
        "This value determines the color of the y axis."
    };

    constexpr openspace::properties::Property::PropertyInfo ZColorInfo = {
        "ZColor",
        "Z Color",
        "This value determines the color of the z axis."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderableCartesianAxes::Documentation() {
    using namespace documentation;
    return {
        "CartesianAxesProgram",
        "base_renderable_cartesianaxes",
        {
            {
                XColorInfo.identifier,
                new DoubleVector4Verifier,
                Optional::Yes,
                XColorInfo.description
            },
            {
                YColorInfo.identifier,
                new DoubleVector4Verifier,
                Optional::Yes,
                YColorInfo.description
            },
            {
                ZColorInfo.identifier,
                new DoubleVector4Verifier,
                Optional::Yes,
                ZColorInfo.description
            }
        }
    };
}


RenderableCartesianAxes::RenderableCartesianAxes(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _program(nullptr)
    , _xColor(
        XColorInfo,
        glm::vec4(0.f, 0.f, 0.f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _yColor(
        YColorInfo,
        glm::vec4(0.f, 1.f, 0.f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _zColor(
        ZColorInfo,
        glm::vec4(0.f, 0.f, 1.f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableCartesianAxes"
    );

    if (dictionary.hasKey(XColorInfo.identifier)) {
        _xColor = dictionary.value<glm::vec4>(XColorInfo.identifier);
    }
    _xColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_xColor);

    if (dictionary.hasKey(XColorInfo.identifier)) {
        _yColor = dictionary.value<glm::vec4>(YColorInfo.identifier);
    }
    _yColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_yColor);

    if (dictionary.hasKey(ZColorInfo.identifier)) {
        _zColor = dictionary.value<glm::vec4>(ZColorInfo.identifier);
    }
    _zColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_zColor);
}

bool RenderableCartesianAxes::isReady() const {
    bool ready = true;
    ready &= (_program != nullptr);
    return ready;
}

void RenderableCartesianAxes::initializeGL() {
    _program = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_BASE}/shaders/axes_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/axes_fs.glsl")
            );
        }
    );

    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vBufferId);
    glGenBuffers(1, &_iBufferId);

    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferId);
    glEnableVertexAttribArray(0);
    glBindVertexArray(0);

    std::vector<Vertex> vertices({
        Vertex{0.f, 0.f, 0.f},
        Vertex{1.f, 0.f, 0.f},
        Vertex{0.f, 1.f, 0.f},
        Vertex{0.f, 0.f, 1.f}
    });

    std::vector<int> indices = {
        0, 1,
        0, 2,
        0, 3
    };

    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glBufferData(
        GL_ARRAY_BUFFER,
        vertices.size() * sizeof(Vertex),
        vertices.data(),
        GL_STATIC_DRAW
    );

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), nullptr);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferId);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        indices.size() * sizeof(int),
        indices.data(),
        GL_STATIC_DRAW
    );
}

void RenderableCartesianAxes::deinitializeGL() {
    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vBufferId);
    _vBufferId = 0;

    glDeleteBuffers(1, &_iBufferId);
    _iBufferId = 0;

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _program = nullptr;
}

void RenderableCartesianAxes::render(const RenderData& data, RendererTasks&){
    _program->activate();

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() *
                                          modelTransform;

    _program->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _program->setUniform("projectionTransform", data.camera.projectionMatrix());

    _program->setUniform("xColor", _xColor);
    _program->setUniform("yColor", _yColor);
    _program->setUniform("zColor", _zColor);

    // Saves current state:
    GLboolean isBlendEnabled = glIsEnabledi(GL_BLEND, 0);
    GLboolean isLineSmoothEnabled = glIsEnabled(GL_LINE_SMOOTH);
    GLfloat currentLineWidth;
    glGetFloatv(GL_LINE_WIDTH, &currentLineWidth);

    GLenum blendEquationRGB, blendEquationAlpha, blendDestAlpha,
        blendDestRGB, blendSrcAlpha, blendSrcRGB;
    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    // Changes GL state:
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnablei(GL_BLEND, 0);
    glEnable(GL_LINE_SMOOTH);

    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferId);
    glDrawElements(GL_LINES, NVertexIndices, GL_UNSIGNED_INT, nullptr);
    glBindVertexArray(0);

    _program->deactivate();

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

} // namespace openspace
