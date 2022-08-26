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
        "The identifier of the node the arrow starts from. "
        "Defaults to 'Root' if not specified. "
    };

    constexpr openspace::properties::Property::PropertyInfo EndNodeInfo = {
        "EndNode",
        "End Node",
        "The identifier of the node the arrow should point towards. "
        "Defaults to 'Root' if not specified. "
    };

    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB color for the arrow"
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "This value pecifies the number of segments that the shapes for the arrow are "
        "separated in. A higher number leads to a higher resolution"
    };

    constexpr openspace::properties::Property::PropertyInfo InvertInfo = {
        "Invert",
        "Invert Direction",
        "If set to true, the arrow direction is inverted so that it points to the "
        "strat node instead of the end node"
    };

    constexpr openspace::properties::Property::PropertyInfo ArrowHeadAngleInfo = {
        "ArrowHeadAngle",
        "Arrow Head Angle",
        "This value is used to set the angle of the arrow head, to make it etiher "
        "wider or pointier"
    };

    constexpr openspace::properties::Property::PropertyInfo ArrowHeadWidthInfo = {
        "ArrowHeadWidthFactor",
        "Arrow Head Width Factor",
        "A factor that is multiplied with the width or the arrow itself, to determine "
        "the width of the base of the arrow head"
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

    constexpr openspace::properties::Property::PropertyInfo WidthInfo = {
        "Width",
        "Width",
        "This value specifies the width of the arrow shape"
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

    // @TODO (emmbr) put in some helper file?
    // Generate vertices around a circle with the given radius, in a plane 
    // with with the given center point and normal
    std::vector<glm::vec3> circleVertices(unsigned int count, float radius,
                                          glm::vec3 center, glm::vec3 normal)
    {
        normal = glm::normalize(normal); // should be normalized, but want to be sure

        std::vector<glm::vec3> vertices;
        vertices.reserve(count);
        float angleStep = glm::two_pi<float>() / count;

        // Find the quaternion that represents a rotation from positive Z to normal. 
        // (this approach is the shortest arc)
        // https://www.xarg.org/proof/quaternion-from-two-vectors/
        constexpr const glm::vec3 Z = glm::vec3(0.f, 0.f, 1.f);
        float w = 1 + glm::dot(Z, normal);
        glm::quat zToNormal = glm::quat(w, glm::cross(Z, normal));
        zToNormal = glm::normalize(zToNormal);
        // TODO: handle parallel case

        for (int i = 0; i < count; ++i) {
            const float angle = i * angleStep;

            // Start with xy plane
            glm::vec3 v(0.f);
            v.x = cos(angle) * radius;
            v.y = sin(angle) * radius;

            // Tranform to match input plane
            v = glm::rotate(zToNormal, v);
            v += center; // translate

            vertices.push_back(v);
        }
        return vertices;
    }

    struct [[codegen::Dictionary(RenderableNodeDirectionHint)]] Parameters {
        // [[codegen::verbatim(StartNodeInfo.description)]]
        std::optional<std::string> startNode;

        // [[codegen::verbatim(EndNodeInfo.description)]]
        std::optional<std::string> endNode;

        // [[codegen::verbatim(LineColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(SegmentsInfo.description)]]
        std::optional<int> segments;

        // [[codegen::verbatim(InvertInfo.description)]]
        std::optional<bool> invert;

        // [[codegen::verbatim(ArrowHeadAngleInfo.description)]]
        std::optional<float> arrowHeadAngle;

        // [[codegen::verbatim(ArrowHeadWidthInfo.description)]]
        std::optional<float> arrowHeadWidthFactor;

        // [[codegen::verbatim(OffsetDistanceInfo.description)]]
        std::optional<float> offset;

        // [[codegen::verbatim(RelativeOffsetInfo.description)]]
        std::optional<bool> useRelativeOffset;

        // [[codegen::verbatim(LengthInfo.description)]]
        std::optional<float> length;

        // [[codegen::verbatim(RelativeLengthInfo.description)]]
        std::optional<bool> useRelativeLength;

        // [[codegen::verbatim(WidthInfo.description)]]
        std::optional<float> width;
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
    , _segments(SegmentsInfo, 10, 3, 100)
    , _invertArrowDirection(InvertInfo, false)
    , _arrowHeadAngle(ArrowHeadAngleInfo, 30.f, 5.f, 45.f)
    , _arrowHeadWidthFactor(ArrowHeadWidthInfo, 2.f, 1.f, 10.f)
    , _offsetDistance(OffsetDistanceInfo, 0.f, 0.f, 1e20f)
    , _useRelativeOffset(RelativeOffsetInfo, false)
    , _length(LengthInfo, 100.f, 0.f, 1e20f)
    , _useRelativeLength(RelativeLengthInfo, false)
    , _width(WidthInfo, 10.f, 0.f, 1e11f)
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

    _segments = p.segments.value_or(_segments);
    _segments.onChange([this]() { _shapeIsDirty = true; });
    addProperty(_segments);

    _invertArrowDirection = p.invert.value_or(_invertArrowDirection);
    _invertArrowDirection.onChange([this]() { _shapeIsDirty = true; });
    addProperty(_invertArrowDirection);

    _arrowHeadAngle = p.arrowHeadAngle.value_or(_arrowHeadAngle);
    _arrowHeadAngle.onChange([this]() { _shapeIsDirty = true; });
    addProperty(_arrowHeadAngle);

    _arrowHeadWidthFactor = p.arrowHeadWidthFactor.value_or(_arrowHeadWidthFactor);
    _arrowHeadWidthFactor.onChange([this]() { _shapeIsDirty = true; });
    addProperty(_arrowHeadWidthFactor);

    _width = p.width.value_or(_width);
    _width.setExponent(10.f);
    addProperty(_width);

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
            _length.setExponent(11.f);
            _length.setMaxValue(1e20f);
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
        _length.setExponent(11.f);
    }

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
            _offsetDistance.setExponent(11.f);
            _offsetDistance.setMaxValue(1e20f);
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

    if (!_useRelativeOffset) {
        _offsetDistance.setExponent(11.f);
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
        "3DArrowProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "3DArrowProgram",
                absPath("${MODULE_BASE}/shaders/arrow_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/arrow_fs.glsl")
            );
        }
    );

    // Generate
    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vBufferId);
    glGenBuffers(1, &_iboId);

    glBindVertexArray(_vaoId);

    updateBufferData();

    glEnableVertexAttribArray(_locVertex);
    glVertexAttribPointer(_locVertex, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), nullptr);

    glBindVertexArray(0);
}

void RenderableNodeDirectionHint::deinitializeGL() {
    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vBufferId);
    _vBufferId = 0;

    glDeleteBuffers(1, &_iboId);
    _iboId = 0;

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

void RenderableNodeDirectionHint::updateBufferData() {
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexArray.size() * sizeof(float),
        _vertexArray.data(),
        GL_STREAM_DRAW
    );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboId);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        _indexArray.size() * sizeof(unsigned int),
        _indexArray.data(),
        GL_STREAM_DRAW
    );
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

    double opposite = _arrowHeadWidthFactor * _width;
    double hypotenuse = opposite / std::tan(glm::radians(_arrowHeadAngle.value()));
    double adjacent = glm::sqrt(hypotenuse * hypotenuse - opposite * opposite);
    const glm::dvec3 arrowHeadStartPos = startPos + (length - adjacent) * arrowDirection;

    _vertexArray.clear();
    _indexArray.clear();

    // Get unit circle vertices on the XY-plane
    const std::vector<glm::vec3> bottomVertices = circleVertices(
        _segments,
        _width,
        startPos,
        arrowDirection
    );

    const std::vector<glm::vec3> topVertices = circleVertices(
        _segments,
        _width,
        arrowHeadStartPos,
        arrowDirection
    );

    const std::vector<glm::vec3> arrowHeadVertices = circleVertices(
        _segments,
        _width * _arrowHeadWidthFactor,
        arrowHeadStartPos,
        arrowDirection
    );

    // Center bot vertex
    _vertexArray.push_back(static_cast<float>(startPos.x));
    _vertexArray.push_back(static_cast<float>(startPos.y));
    _vertexArray.push_back(static_cast<float>(startPos.z));

    // All bottom vertices (same number as number of segments)
    for (const glm::vec3& v : bottomVertices) {
        _vertexArray.push_back(v.x);
        _vertexArray.push_back(v.y);
        _vertexArray.push_back(v.z);
    }

    auto botRingIndex = [](unsigned int i) {
        return i + 1;
    };

    // Then all top vertices
    for (const glm::vec3& v : topVertices) {
        _vertexArray.push_back(v.x);
        _vertexArray.push_back(v.y);
        _vertexArray.push_back(v.z);
    }

    const unsigned int nSegments = _segments;

    auto topRingIndex = [&nSegments](unsigned int i) {
        return 1 + nSegments + i;
    };

    // Then all arrow head bottom vertices
    for (const glm::vec3& v : arrowHeadVertices) {
        _vertexArray.push_back(v.x);
        _vertexArray.push_back(v.y);
        _vertexArray.push_back(v.z);
    }

    auto arrowHeadRingIndex = [&nSegments](unsigned int i) {
        return 1 + 2 * nSegments + i;
    };

    // Center top vertex (arrow head point)
    _vertexArray.push_back(static_cast<float>(endPos.x));
    _vertexArray.push_back(static_cast<float>(endPos.y));
    _vertexArray.push_back(static_cast<float>(endPos.z));

    unsigned int botCenterIndex = 0;
    unsigned int topCenterIndex = _vertexArray.size() / 3 - 1;

    // Build triangle list from the given vertices
    for (unsigned int i = 0; i < _segments; ++i) {
        bool isLast = (i == _segments - 1);

        // Bottom vertex indices
        unsigned int v0 = botRingIndex(i);
        unsigned int v1 = botRingIndex(isLast ? 0 : i + 1);

        // Top vertex indices
        unsigned int v2 = topRingIndex(i);
        unsigned int v3 = topRingIndex(isLast ? 0 : i + 1);

        // Side of cylinder
        _indexArray.push_back(v0);
        _indexArray.push_back(v1);
        _indexArray.push_back(v2);

        _indexArray.push_back(v1);
        _indexArray.push_back(v3);
        _indexArray.push_back(v2);

        // Bot triangle
        _indexArray.push_back(botCenterIndex);
        _indexArray.push_back(v1);
        _indexArray.push_back(v0);
        
        // Arrow head
        v0 = arrowHeadRingIndex(i);
        v1 = arrowHeadRingIndex(isLast ? 0 : i + 1);

        _indexArray.push_back(v0);
        _indexArray.push_back(v1);
        _indexArray.push_back(topCenterIndex);

        // TODO: arrow head bottom
    }

    _shapeIsDirty = false;
}

void RenderableNodeDirectionHint::update(const UpdateData&) {
    SceneGraphNode* startNode = sceneGraphNode(_start);
    SceneGraphNode* endNode = sceneGraphNode(_end);

    bool shouldUpdate = _shapeIsDirty;
    if (startNode && endNode) {
        constexpr float Epsilon = std::numeric_limits<float>::epsilon();
        float combinedDistanceChange =
            glm::distance(startNode->worldPosition(), _prevStartNodePosition) +
            glm::distance(endNode->worldPosition(), _prevEndNodePosition);
        if (combinedDistanceChange > Epsilon) {
            shouldUpdate = true;
        }

        _prevStartNodePosition = startNode->worldPosition();
        _prevEndNodePosition = endNode->worldPosition();
    }

    if (shouldUpdate) {
        updateVertexData();
        updateBufferData();
    }
}

void RenderableNodeDirectionHint::render(const RenderData& data, RendererTasks&) {
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

    _shaderProgram->activate();

    _shaderProgram->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _shaderProgram->setUniform("projectionTransform", data.camera.projectionMatrix());
    _shaderProgram->setUniform("color", glm::vec4(_color.value(), opacity()));

    // Change GL state:
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnablei(GL_BLEND, 0);

    // Bind and draw shape
    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboId);

    glDrawElements(
        GL_TRIANGLES,
        static_cast<GLsizei>(_indexArray.size()),
        GL_UNSIGNED_INT,
        nullptr
    );

    // Restore GL State
    glBindVertexArray(0);
    global::renderEngine->openglStateCache().resetBlendState();

    _shaderProgram->deactivate();
}

} // namespace openspace
