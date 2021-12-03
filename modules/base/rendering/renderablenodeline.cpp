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

#include <modules/base/rendering/renderablenodeline.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/translation.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const char* _loggerCat = "RenderableNodeLine";
    constexpr const char* ProgramName = "NodeLineProgram";
    constexpr const char* Root = "Root";

    constexpr openspace::properties::Property::PropertyInfo StartNodeInfo = {
        "StartNode",
        "Start Node",
        "The identifier of the node the line starts from. "
        "Defaults to 'Root' if not specified. "
    };

    constexpr openspace::properties::Property::PropertyInfo EndNodeInfo = {
        "EndNode",
        "End Node",
        "The identifier of the node the line ends at. "
        "Defaults to 'Root' if not specified. "
    };

    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB color for the line."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width."
    };

    // Returns a position that is relative to the current anchor node. This is a method to
    // handle precision problems that occur when approaching a line end point
    glm::dvec3 coordinatePosFromAnchorNode(const glm::dvec3& worldPos) {
        using namespace openspace;
        glm::dvec3 anchorNodePos(0.0);

        const interaction::OrbitalNavigator& nav =
            global::navigationHandler->orbitalNavigator();

        if (nav.anchorNode()) {
            anchorNodePos = nav.anchorNode()->worldPosition();
        }
        glm::dvec3 diffPos = worldPos - anchorNodePos;
        return diffPos;
    }

    struct [[codegen::Dictionary(RenderableNodeLine)]] Parameters {
        // [[codegen::verbatim(StartNodeInfo.description)]]
        std::optional<std::string> startNode;

        // [[codegen::verbatim(EndNodeInfo.description)]]
        std::optional<std::string> endNode;

        // [[codegen::verbatim(LineColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;
    };
#include "renderablenodeline_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableNodeLine::Documentation() {
    return codegen::doc<Parameters>("base_renderable_renderablenodeline");
}

RenderableNodeLine::RenderableNodeLine(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _start(StartNodeInfo, Root)
    , _end(EndNodeInfo, Root)
    , _lineColor(LineColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 20.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _start = p.startNode.value_or(_start);
    _start.onChange([&]() { validateNodes(); });
    addProperty(_start);

    _end = p.endNode.value_or(_end);
    _end.onChange([&]() { validateNodes(); });
    addProperty(_end);

    _lineColor = p.color.value_or(_lineColor);
    _lineColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_lineColor);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    addProperty(_opacity);
}

double RenderableNodeLine::distance() const {
    return glm::distance(_startPos, _endPos);
}

std::string RenderableNodeLine::start() const {
    return _start;
}

std::string RenderableNodeLine::end() const {
    return _end;
}

void RenderableNodeLine::initializeGL() {
    _program = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                ProgramName,
                absPath("${MODULE_BASE}/shaders/line_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/line_fs.glsl")
            );
        }
    );

    // Generate
    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vBufferId);

    bindGL();

    glVertexAttribPointer(_locVertex, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);
    glEnableVertexAttribArray(_locVertex);

    unbindGL();
}

void RenderableNodeLine::deinitializeGL() {
    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vBufferId);
    _vBufferId = 0;

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _program = nullptr;
}

bool RenderableNodeLine::isReady() const {
    bool ready = true;
    ready &= (_program != nullptr);
    return ready;
}

void RenderableNodeLine::unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableNodeLine::bindGL() {
    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
}

void RenderableNodeLine::updateVertexData() {
    _vertexArray.clear();

    // Update the positions of the nodes
    _startPos = coordinatePosFromAnchorNode(
        global::renderEngine->scene()->sceneGraphNode(_start)->worldPosition()
    );
    _endPos = coordinatePosFromAnchorNode(
        global::renderEngine->scene()->sceneGraphNode(_end)->worldPosition()
    );

    _vertexArray.push_back(static_cast<float>(_startPos.x));
    _vertexArray.push_back(static_cast<float>(_startPos.y));
    _vertexArray.push_back(static_cast<float>(_startPos.z));

    _vertexArray.push_back(static_cast<float>(_endPos.x));
    _vertexArray.push_back(static_cast<float>(_endPos.y));
    _vertexArray.push_back(static_cast<float>(_endPos.z));

    bindGL();
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexArray.size() * sizeof(float),
        _vertexArray.data(),
        GL_DYNAMIC_DRAW
    );

    // update vertex attributes
    glVertexAttribPointer(_locVertex, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);

    unbindGL();
}

void RenderableNodeLine::render(const RenderData& data, RendererTasks&) {
    updateVertexData();

    _program->activate();

    glm::dmat4 anchorTranslation(1.0);
    // Update anchor node information, used to counter precision problems
    if (global::navigationHandler->orbitalNavigator().anchorNode()) {
        anchorTranslation = glm::translate(
            glm::dmat4(1.0),
            global::navigationHandler->orbitalNavigator().anchorNode()->worldPosition()
        );
    }

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() *
        modelTransform * anchorTranslation;

    _program->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _program->setUniform("projectionTransform", data.camera.projectionMatrix());
    _program->setUniform("color", glm::vec4(_lineColor.value(), _opacity));

    // Change GL state:
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnablei(GL_BLEND, 0);
    glEnable(GL_LINE_SMOOTH);
    glLineWidth(_lineWidth);

    // Bind and draw
    bindGL();
    glDrawArrays(GL_LINES, 0, 2);

    // Restore GL State
    unbindGL();
    _program->deactivate();
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetLineState();
}

void RenderableNodeLine::validateNodes() {
    if (!global::renderEngine->scene()->sceneGraphNode(_start)) {
        LERROR(fmt::format(
            "There is no scenegraph node with id {}, defaults to 'Root'", _start
        ));
        _start = Root;
    }
    if (!global::renderEngine->scene()->sceneGraphNode(_end)) {
        LERROR(fmt::format(
            "There is no scenegraph node with id {}, defaults to 'Root'", _end
        ));
        _end = Root;
    }
}

} // namespace openspace
