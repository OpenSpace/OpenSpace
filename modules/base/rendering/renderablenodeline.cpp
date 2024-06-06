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
    constexpr std::string_view _loggerCat = "RenderableNodeLine";

    constexpr openspace::properties::Property::PropertyInfo StartNodeInfo = {
        "StartNode",
        "Start Node",
        "The identifier of the node the line starts from. Defaults to 'Root' if not "
        "specified.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EndNodeInfo = {
        "EndNode",
        "End Node",
        "The identifier of the node the line ends at. Defaults to 'Root' if not "
        "specified.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "The RGB color for the line.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The width of the line. The larger number, the thicker the line.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo StartOffsetInfo = {
        "StartOffset",
        "Offset to Start Node",
        "A distance from the start node at which the rendered line should begin. "
        "By default it takes a value in meters, but if 'UseRelativeOffsets' is set "
        "to true it is read as a multiplier times the bounding sphere of the node.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EndOffsetInfo = {
        "EndOffset",
        "Offset to End Node",
        "A distance to the end node at which the rendered line should end. "
        "By default it takes a value in meters, but if 'UseRelativeOffsets' is set "
        "to true it is read as a multiplier times the bounding sphere of the node.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RelativeOffsetsInfo = {
        "UseRelativeOffsets",
        "Use Relative Offsets",
        "If true, the offset values are interpreted as relative values to be multiplied "
        "with the bounding sphere of the start/end node. If false, the value is "
        "interpreted as a distance in meters.",
        openspace::properties::Property::Visibility::AdvancedUser
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
        const glm::dvec3 diffPos = worldPos - anchorNodePos;
        return diffPos;
    }

    struct [[codegen::Dictionary(RenderableNodeLine)]] Parameters {
        // [[codegen::verbatim(StartNodeInfo.description)]]
        std::optional<std::string> startNode [[codegen::identifier()]];

        // [[codegen::verbatim(EndNodeInfo.description)]]
        std::optional<std::string> endNode [[codegen::identifier()]];

        // [[codegen::verbatim(LineColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(StartOffsetInfo.description)]]
        std::optional<float> startOffset;

        // [[codegen::verbatim(EndOffsetInfo.description)]]
        std::optional<float> endOffset;

        // [[codegen::verbatim(RelativeOffsetsInfo.description)]]
        std::optional<bool> useRelativeOffsets;
    };
#include "renderablenodeline_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableNodeLine::Documentation() {
    return codegen::doc<Parameters>("base_renderable_renderablenodeline");
}

RenderableNodeLine::RenderableNodeLine(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _start(StartNodeInfo, "Root")
    , _end(EndNodeInfo, "Root")
    , _lineColor(LineColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 20.f)
    , _startOffset(StartOffsetInfo, 0.f, 0.f, 1e13f)
    , _endOffset(EndOffsetInfo, 0.f, 0.f, 1e13f)
    , _useRelativeOffsets(RelativeOffsetsInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _lineColor = p.color.value_or(_lineColor);
    _lineColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_lineColor);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _start = p.startNode.value_or(_start);
    addProperty(_start);

    _end = p.endNode.value_or(_end);
    addProperty(_end);

    addProperty(_startOffset);
    _startOffset.setExponent(7.f);
    _startOffset.onChange([&] {
        if (_useRelativeOffsets) {
            SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(_start);
            if (!node || node->boundingSphere() > 0.0) {
                return;
            }
            LWARNING(std::format(
                "Setting StartOffset for node line '{}': Trying to use relative offsets "
                "for start node '{}' that has no bounding sphere. This will result in no "
                "offset. Use direct values by setting UseRelativeOffsets to false",
                parent()->identifier(), _start.value()
            ));
        }
    });

    addProperty(_endOffset);
    _endOffset.setExponent(7.f);
    _endOffset.onChange([&] {
        if (_useRelativeOffsets) {
            SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(_end);
            if (!node || node->boundingSphere() > 0.0) {
                return;
            }
            LWARNING(std::format(
                "Setting EndOffset for node line '{}': Trying to use relative offsets "
                "for end node '{}' that has no bounding sphere. This will result in no "
                "offset. Use direct values by setting UseRelativeOffsets to false",
                parent()->identifier(), _end.value()
            ));
        }
     });

    addProperty(_useRelativeOffsets);
    _useRelativeOffsets.onChange([this]() {
        SceneGraphNode* startNode = global::renderEngine->scene()->sceneGraphNode(_start);
        SceneGraphNode* endNode = global::renderEngine->scene()->sceneGraphNode(_end);

        if (!startNode) {
            LERROR(std::format(
                "Error when recomputing node line offsets for scene graph node '{}'. "
                "Could not find start node '{}'", parent()->identifier(), _start.value()
            ));
            return;
        }

        if (!endNode) {
            LERROR(std::format(
                "Error when recomputing node line offsets for scene graph node '{}'. "
                "Could not find end node '{}'", parent()->identifier(), _end.value()
            ));
            return;
        }

        if (_useRelativeOffsets) {
            // Recompute previous offsets to relative values
            const double startBs = startNode->boundingSphere();
            const double endBs = endNode->boundingSphere();
            _startOffset =
                static_cast<float>(startBs > 0.0 ? _startOffset / startBs : 0.0);
            _endOffset =
                static_cast<float>(endBs > 0.0 ? _endOffset / startBs : 0.0);
        }
        else {
            // Recompute relative values to meters
            _startOffset = static_cast<float>(_startOffset * startNode->boundingSphere());
            _endOffset = static_cast<float>(_endOffset * endNode->boundingSphere());
        }
    });
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
        "NodeLineProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "NodeLineProgram",
                absPath("${MODULE_BASE}/shaders/line_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/line_fs.glsl")
            );
        }
    );

    // Generate
    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vBufferId);

    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);

    glVertexAttribPointer(_locVertex, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);
    glEnableVertexAttribArray(_locVertex);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableNodeLine::deinitializeGL() {
    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vBufferId);
    _vBufferId = 0;

    BaseModule::ProgramObjectManager.release(
        "NodeLineProgram",
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

void RenderableNodeLine::updateVertexData() {
    SceneGraphNode* startNode = global::renderEngine->scene()->sceneGraphNode(_start);
    SceneGraphNode* endNode = global::renderEngine->scene()->sceneGraphNode(_end);

    if (!startNode) {
        LERROR(std::format("Could not find start node '{}'", _start.value()));
        return;
    }

    if (!endNode) {
        LERROR(std::format("Could not find end node '{}'", _end.value()));
        return;
    }

    _vertexArray.clear();

    // Update the positions of the nodes
    _startPos = coordinatePosFromAnchorNode(startNode->worldPosition());
    _endPos = coordinatePosFromAnchorNode(endNode->worldPosition());

    // Handle relative values
    double startOffset = static_cast<double>(_startOffset);
    double endOffset = static_cast<double>(_endOffset);
    if (_useRelativeOffsets) {
        startOffset *= startNode->boundingSphere();
        endOffset *= endNode->boundingSphere();
    }

    // Compute line positions
    const glm::dvec3 dir = glm::normalize(_endPos - _startPos);
    const glm::dvec3 startPos = _startPos + startOffset * dir;
    const glm::dvec3 endPos = _endPos - endOffset * dir;

    _vertexArray.push_back(static_cast<float>(startPos.x));
    _vertexArray.push_back(static_cast<float>(startPos.y));
    _vertexArray.push_back(static_cast<float>(startPos.z));

    _vertexArray.push_back(static_cast<float>(endPos.x));
    _vertexArray.push_back(static_cast<float>(endPos.y));
    _vertexArray.push_back(static_cast<float>(endPos.z));

    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexArray.size() * sizeof(float),
        _vertexArray.data(),
        GL_DYNAMIC_DRAW
    );

    // update vertex attributes
    glVertexAttribPointer(_locVertex, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableNodeLine::update(const UpdateData&) {
    updateVertexData();
}

void RenderableNodeLine::render(const RenderData& data, RendererTasks&) {
    _program->activate();

    glm::dmat4 anchorTranslation(1.0);
    // Update anchor node information, used to counter precision problems
    if (global::navigationHandler->orbitalNavigator().anchorNode()) {
        anchorTranslation = glm::translate(
            glm::dmat4(1.0),
            global::navigationHandler->orbitalNavigator().anchorNode()->worldPosition()
        );
    }

    const glm::dmat4 modelTransform = calcModelTransform(data);
    const glm::dmat4 modelViewTransform =
        calcModelViewTransform(data, modelTransform) * anchorTranslation;

    _program->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _program->setUniform("projectionTransform", data.camera.projectionMatrix());
    _program->setUniform("color", glm::vec4(_lineColor.value(), opacity()));

    // Change GL state:
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnablei(GL_BLEND, 0);
    glEnable(GL_LINE_SMOOTH);
    glLineWidth(_lineWidth);

    // Bind and draw
    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glDrawArrays(GL_LINES, 0, 2);

    // Restore GL State
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    _program->deactivate();
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetLineState();
}

} // namespace openspace
