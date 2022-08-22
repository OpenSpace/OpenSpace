/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/base/rendering/renderablenodedirectionhint.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/translation.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr std::string_view _loggerCat = "RenderableNodeDirectionHint";

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
        "This value determines the RGB color for the line"
    };

    constexpr openspace::properties::Property::PropertyInfo OffsetDistanceInfo = {
        "Offset",
        "Offset Distance",
        "The distance from the center of the start node where the arrow starts. "
        "If 'UseRelativeOffset' is true, the value should be given as a factor to "
        "multiply with the boudning sphere of the node. Otherwise, the value is "
        "specified in meters"
    };

    constexpr openspace::properties::Property::PropertyInfo RelativeOffsetInfo = {
        "UseRelativeOffset",
        "Use Relative Offset Distance",
        "Decide whether to use relative distances (in units of start node bounding "
        "sphere) for the offset distance. If false, meters is used"
    };

    constexpr openspace::properties::Property::PropertyInfo LengthInfo = {
        "Length",
        "Length",
        "The length of the arrow, given either in meters or as a factor to be "
        "multiplied with the bounding sphere of the start node (if "
        "'UseRelativeLength' is true)"
    };

    constexpr openspace::properties::Property::PropertyInfo RelativeLengthInfo = {
        "UseRelativeLength",
        "Use Relative Length",
        "Decide whether to use relative size (in units of start node bounding "
        "sphere) for the length of the arrow. If false, meters is used"
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the arrow shape"
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

    struct [[codegen::Dictionary(RenderableNodeDirectionHint)]] Parameters {
        // [[codegen::verbatim(StartNodeInfo.description)]]
        std::optional<std::string> startNode;

        // [[codegen::verbatim(EndNodeInfo.description)]]
        std::optional<std::string> endNode;

        // [[codegen::verbatim(LineColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(OffsetDistanceInfo.description)]]
        std::optional<float> offset;

        // [[codegen::verbatim(RelativeOffsetInfo.description)]]
        std::optional<bool> useRelativeOffset;

        // [[codegen::verbatim(LengthInfo.description)]]
        std::optional<float> length;

        // [[codegen::verbatim(RelativeLengthInfo.description)]]
        std::optional<bool> useRelativeLength;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;
    };
#include "renderablenodedirectionhint_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableNodeDirectionHint::Documentation() {
    return codegen::doc<Parameters>("base_renderable_renderablenodedirectionhint");
}

RenderableNodeDirectionHint::RenderableNodeDirectionHint(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _start(StartNodeInfo, "Root")
    , _end(EndNodeInfo, "Root")
    , _color(LineColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _offsetDistance(OffsetDistanceInfo, 0.f, 0.f, 1e11f)
    , _useRelativeOffset(RelativeOffsetInfo, false)
    , _length(LengthInfo, 100.f, 0.f, 1e11f)
    , _useRelativeLength(RelativeLengthInfo, false)
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 20.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _start = p.startNode.value_or(_start);
    addProperty(_start);

    _end = p.endNode.value_or(_end);
    addProperty(_end);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    addProperty(_opacity);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _useRelativeOffset.onChange([this]() {
        SceneGraphNode* startNode = sceneGraphNode(_start);
        if (!startNode) {
            LERROR(fmt::format("Could not find start node '{}'", _start.value()));
            return;
        }
        const double boundingSphere = startNode->boundingSphere();

        if (!_useRelativeOffset) {
            // Recompute distance (previous value was relative)
            _offsetDistance = _offsetDistance * startNode->boundingSphere();
            _offsetDistance.setExponent(10.f);
            _offsetDistance.setMaxValue(1e11f);
        }
        else {
            // Recompute distance (previous value was in meters)
            if (boundingSphere < std::numeric_limits<double>::epsilon()) {
                LERROR(fmt::format(
                    "Start node '{}' has invalid bounding sphere", _start.value()
                ));
                return;
            }
            _offsetDistance = _offsetDistance / startNode->boundingSphere();
            _offsetDistance.setExponent(3.f);
            _offsetDistance.setMaxValue(1000.f);
        }
    });
    // @TODO (emmbr, 2022-08-22): make GUI update when min/max value is updated

    _useRelativeOffset = p.useRelativeOffset.value_or(_useRelativeOffset);
    addProperty(_useRelativeOffset);

    _offsetDistance = p.offset.value_or(_offsetDistance);
    addProperty(_offsetDistance);

    _useRelativeLength.onChange([this]() {
        SceneGraphNode* startNode = sceneGraphNode(_start);
        if (!startNode) {
            LERROR(fmt::format("Could not find start node '{}'", _start.value()));
            return;
        }
        const double boundingSphere = startNode->boundingSphere();

        if (!_useRelativeLength) {
            // Recompute distance (previous value was relative)
            _length = _length * startNode->boundingSphere();
            _length.setExponent(10.f);
            _length.setMaxValue(1e11f);
        }
        else {
            // Recompute distance (previous value was in meters)
            if (boundingSphere < std::numeric_limits<double>::epsilon()) {
                LERROR(fmt::format(
                    "Start node '{}' has invalid bounding sphere", _start.value()
                ));
                return;
            }
            _length = _length / startNode->boundingSphere();
            _length.setExponent(3.f);
            _length.setMaxValue(1000.f);
        }
    });
    // @TODO (emmbr, 2022-08-22): make GUI update when min/max value is updated

    _useRelativeLength = p.useRelativeLength.value_or(_useRelativeLength);
    addProperty(_useRelativeLength);

    _length = p.length.value_or(_length);
    addProperty(_length);

    if (!_useRelativeLength) {
        _length.setExponent(10.f);
    }
}

std::string RenderableNodeDirectionHint::start() const {
    return _start;
}

std::string RenderableNodeDirectionHint::end() const {
    return _end;
}

void RenderableNodeDirectionHint::initializeGL() {
    _shaderProgram = BaseModule::ProgramObjectManager.request(
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

    bindGL();

    glVertexAttribPointer(_locVertex, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);
    glEnableVertexAttribArray(_locVertex);

    unbindGL();
}

void RenderableNodeDirectionHint::deinitializeGL() {
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
    _shaderProgram = nullptr;
}

bool RenderableNodeDirectionHint::isReady() const {
    bool ready = true;
    ready &= (_shaderProgram != nullptr);
    return ready;
}

void RenderableNodeDirectionHint::unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableNodeDirectionHint::bindGL() {
    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
}

void RenderableNodeDirectionHint::updateVertexData() {
    SceneGraphNode* startNode = sceneGraphNode(_start);
    SceneGraphNode* endNode = sceneGraphNode(_end);

    if (!startNode) {
        LERROR(fmt::format("Could not find start node '{}'", _start.value()));
        return;
    }

    if (!endNode) {
        LERROR(fmt::format("Could not find end node '{}'", _end.value()));
        return;
    }

    _vertexArray.clear();

    const double boundingSphere = startNode->boundingSphere();
    bool hasNoBoundingSphere = boundingSphere < std::numeric_limits<double>::epsilon();

    if (hasNoBoundingSphere && (_useRelativeLength || _useRelativeOffset)) {
        LERROR(fmt::format(
            "Node '{}' has no valid bounding sphere. Can not use relative values", 
            _end.value()
        ));
        return;
    }

    double offset = static_cast<double>(_offsetDistance);
    if (_useRelativeOffset) {
        offset *= boundingSphere;
    }

    double length = static_cast<double>(_length);
    if (_useRelativeLength) {
        length *= boundingSphere;
    }

    // Update the position based on the arrowDirection of the nodes
    const glm::dvec3 startNodePos = coordinatePosFromAnchorNode(startNode->worldPosition());
    const glm::dvec3 endNodePos = coordinatePosFromAnchorNode(endNode->worldPosition());

    const glm::dvec3 arrowDirection = glm::normalize(endNodePos - startNodePos);
    const glm::dvec3 startPos = startNodePos + offset * arrowDirection;
    const glm::dvec3 endPos = startPos + length * arrowDirection;

    _vertexArray.push_back(static_cast<float>(startPos.x));
    _vertexArray.push_back(static_cast<float>(startPos.y));
    _vertexArray.push_back(static_cast<float>(startPos.z));

    _vertexArray.push_back(static_cast<float>(endPos.x));
    _vertexArray.push_back(static_cast<float>(endPos.y));
    _vertexArray.push_back(static_cast<float>(endPos.z));

    // Arrow head (@TODO: generate in geometry shader instead)

    // Angle and vector lengths
    const float arrowAngle = 45.f; // degrees
    double adjacent = (0.1f * length);
    double hypotenuse = adjacent / std::cos(glm::radians(arrowAngle));
    double opposite = glm::sqrt(hypotenuse * hypotenuse - adjacent * adjacent);

    // TODO: should probably compute arrow head size in view space size instead of physical..
    // TODO: Make editable propeies to control arrow size

    const glm::dvec3 arrowHeadStartPos = startPos + (length - adjacent) * arrowDirection;

    const Camera* camera = global::renderEngine->scene()->camera();
    const glm::dvec3 cameraPos = coordinatePosFromAnchorNode(camera->positionVec3());
    const glm::dvec3 cameraToLineDirection = glm::normalize(cameraPos - arrowHeadStartPos);

    // Compute orthogonal arrow arrowDirection
    const glm::dvec3 stepDirection = glm::cross(cameraToLineDirection, arrowDirection);

    const glm::dvec3 leftArrowPos = arrowHeadStartPos + opposite * stepDirection;
    const glm::dvec3 rightArrowPos = arrowHeadStartPos - opposite * stepDirection;

    _vertexArray.push_back(static_cast<float>(endPos.x));
    _vertexArray.push_back(static_cast<float>(endPos.y));
    _vertexArray.push_back(static_cast<float>(endPos.z));
    _vertexArray.push_back(static_cast<float>(leftArrowPos.x));
    _vertexArray.push_back(static_cast<float>(leftArrowPos.y));
    _vertexArray.push_back(static_cast<float>(leftArrowPos.z));

    _vertexArray.push_back(static_cast<float>(endPos.x));
    _vertexArray.push_back(static_cast<float>(endPos.y));
    _vertexArray.push_back(static_cast<float>(endPos.z));
    _vertexArray.push_back(static_cast<float>(rightArrowPos.x));
    _vertexArray.push_back(static_cast<float>(rightArrowPos.y));
    _vertexArray.push_back(static_cast<float>(rightArrowPos.z));


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

void RenderableNodeDirectionHint::update(const UpdateData&) {
    updateVertexData();
}

void RenderableNodeDirectionHint::render(const RenderData& data, RendererTasks&) {
    _shaderProgram->activate();

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

    _shaderProgram->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _shaderProgram->setUniform("projectionTransform", data.camera.projectionMatrix());
    _shaderProgram->setUniform("color", glm::vec4(_color.value(), opacity()));

    // Change GL state:
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnablei(GL_BLEND, 0);
    glEnable(GL_LINE_SMOOTH);
    glLineWidth(_lineWidth);

    // Bind and draw
    bindGL();
    glDrawArrays(GL_LINES, 0, _vertexArray.size() / 3);

    // Restore GL State
    unbindGL();
    _shaderProgram->deactivate();
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetLineState();
}

} // namespace openspace
