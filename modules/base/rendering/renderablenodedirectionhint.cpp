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
#include <glm/gtx/projection.hpp>

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

    constexpr openspace::properties::Property::PropertyInfo ArrowHeadSizeInfo = {
        "ArrowHeadSize",
        "Arrow Head Size",
        "This size of the arrow head, given in relative value of the entire length of "
        "the arrow. For example, 0.1 makes the arrow head length be 10% of the full "
        "arrow length."
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

    constexpr openspace::properties::Property::PropertyInfo AmbientIntensityInfo = {
        "AmbientIntensity",
        "Ambient Intensity",
        "A multiplier for ambient lighting"
    };

    constexpr openspace::properties::Property::PropertyInfo DiffuseIntensityInfo = {
        "DiffuseIntensity",
        "Diffuse Intensity",
        "A multiplier for diffuse lighting"
    };

    constexpr openspace::properties::Property::PropertyInfo SpecularIntensityInfo = {
        "SpecularIntensity",
        "Specular Intensity",
        "A multiplier for specular lighting"
    };

    constexpr openspace::properties::Property::PropertyInfo ShadingEnabledInfo = {
        "PerformShading",
        "Perform Shading",
        "This value determines whether the arrow model should be shaded by using the "
        "position of the Sun"
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

        // [[codegen::verbatim(ArrowHeadSizeInfo.description)]]
        std::optional<float> arrowHeadSize;

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

        // [[codegen::verbatim(ShadingEnabledInfo.description)]]
        std::optional<float> performShading;

        // [[codegen::verbatim(AmbientIntensityInfo.description)]]
        std::optional<float> ambientIntensity;

        // [[codegen::verbatim(DiffuseIntensityInfo.description)]]
        std::optional<float> diffuseIntensity;

        // [[codegen::verbatim(SpecularIntensityInfo.description)]]
        std::optional<float> specularIntensity;
    };
#include "renderablenodedirectionhint_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableNodeDirectionHint::Documentation() {
    return codegen::doc<Parameters>("base_renderable_renderablenodedirectionhint");
}

RenderableNodeDirectionHint::Shading::Shading() 
    : properties::PropertyOwner({ "Shading" }) 
    , enabled(ShadingEnabledInfo, true)
    , ambientIntensity(AmbientIntensityInfo, 0.2f, 0.f, 1.f)
    , diffuseIntensity(DiffuseIntensityInfo, 0.7f, 0.f, 1.f)
    , specularIntensity(SpecularIntensityInfo, 0.f, 0.f, 1.f)
{
    addProperty(enabled);
    addProperty(ambientIntensity);
    addProperty(diffuseIntensity);
    addProperty(specularIntensity);
}

RenderableNodeDirectionHint::RenderableNodeDirectionHint(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _start(StartNodeInfo, "Root")
    , _end(EndNodeInfo, "Root")
    , _color(LineColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _segments(SegmentsInfo, 10, 3, 100)
    , _invertArrowDirection(InvertInfo, false)
    , _arrowHeadSize(ArrowHeadSizeInfo, 0.1f, 0.f, 1.f)
    , _arrowHeadWidthFactor(ArrowHeadWidthInfo, 2.f, 1.f, 100.f)
    , _offsetDistance(OffsetDistanceInfo, 0.f, 0.f, 1e20f)
    , _useRelativeOffset(RelativeOffsetInfo, false)
    , _length(LengthInfo, 100.f, 0.f, 1e20f)
    , _useRelativeLength(RelativeLengthInfo, false)
    , _width(WidthInfo, 10.f, 0.f, 1e11f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addPropertySubOwner(_shading);
    _shading.enabled = p.performShading.value_or(_shading.enabled);
    _shading.ambientIntensity = p.ambientIntensity.value_or(_shading.ambientIntensity);
    _shading.diffuseIntensity = p.diffuseIntensity.value_or(_shading.diffuseIntensity);
    _shading.specularIntensity = p.specularIntensity.value_or(_shading.specularIntensity);

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

    _arrowHeadSize = p.arrowHeadSize.value_or(_arrowHeadSize);
    _arrowHeadSize.onChange([this]() { _shapeIsDirty = true; });
    addProperty(_arrowHeadSize);

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

    _offsetDistance = p.offset.value_or(_offsetDistance);
    addProperty(_offsetDistance);

    if (!_useRelativeOffset) {
        _offsetDistance.setExponent(11.f);
    }

    addProperty(_useRelativeLength);
    addProperty(_useRelativeOffset);

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

    // Vertices
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), nullptr);

    // Normals
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        3,
        GL_FLOAT,
        GL_FALSE,
        6 * sizeof(float),
        reinterpret_cast<void*>(3 * sizeof(float))
    );

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
    glm::dvec3 startNodePos = coordinatePosFromAnchorNode(startNode->worldPosition());
    glm::dvec3 endNodePos = coordinatePosFromAnchorNode(endNode->worldPosition());

    glm::dvec3 arrowDirection = glm::normalize(endNodePos - startNodePos);
    glm::dvec3 startPos = glm::vec3(startNodePos + offset * arrowDirection);
    glm::dvec3 endPos = glm::vec3(startPos + length * arrowDirection);

    if (_invertArrowDirection) {
        std::swap(startPos, endPos);
        arrowDirection *= -1;
    }

    double adjacent = _arrowHeadSize * length;
    glm::dvec3 arrowHeadStartPos = startPos + (length - adjacent) * arrowDirection;

    _vertexArray.clear();
    _indexArray.clear();

    // TODO: Simplify. Just generate a simple cone and a cylinder or correctl size then
    // Transform it to be located in teh same position

    // Vertice positions for arrow bottom
    const std::vector<glm::vec3> bottomVertices = circleVertices(
        _segments,
        _width,
        startPos,
        arrowDirection
    );

    // Vertice positions for arrow top (close to head)
    const std::vector<glm::vec3> topVertices = circleVertices(
        _segments,
        _width,
        arrowHeadStartPos,
        arrowDirection
    );

    // Vertice positions for arrow head
    const std::vector<glm::vec3> arrowHeadVertices = circleVertices(
        _segments,
        _width * _arrowHeadWidthFactor,
        arrowHeadStartPos,
        arrowDirection
    );

    // Center bot vertex and normal
    _vertexArray.push_back(static_cast<float>(startPos.x));
    _vertexArray.push_back(static_cast<float>(startPos.y));
    _vertexArray.push_back(static_cast<float>(startPos.z));

    _vertexArray.push_back(static_cast<float>(-arrowDirection.x));
    _vertexArray.push_back(static_cast<float>(-arrowDirection.y));
    _vertexArray.push_back(static_cast<float>(-arrowDirection.z));

    // Ring 0
    // Vertices of bottom circle, with normals pointing down
    for (const glm::vec3& v : bottomVertices) {
        _vertexArray.push_back(v.x);
        _vertexArray.push_back(v.y);
        _vertexArray.push_back(v.z);

        _vertexArray.push_back(static_cast<float>(-arrowDirection.x));
        _vertexArray.push_back(static_cast<float>(-arrowDirection.y));
        _vertexArray.push_back(static_cast<float>(-arrowDirection.z));
    }

    // Ring 1
    // Bottom vertices of cylider sides with normals pointing outwards
    for (const glm::vec3& v : bottomVertices) {
        _vertexArray.push_back(v.x);
        _vertexArray.push_back(v.y);
        _vertexArray.push_back(v.z);

        glm::vec3 normal = glm::normalize(glm::dvec3(v) - startPos);
        _vertexArray.push_back(normal.x);
        _vertexArray.push_back(normal.y);
        _vertexArray.push_back(normal.z);
    }

    const unsigned int nSegments = _segments;

    auto ringVerticeIndex = [&nSegments](unsigned int ringIndex, unsigned int i) {
        return 1 + ringIndex * nSegments + i;
    };

    // Ring 2
    // Then all top vertices of cylinder sides
    for (const glm::vec3& v : topVertices) {
        _vertexArray.push_back(v.x);
        _vertexArray.push_back(v.y);
        _vertexArray.push_back(v.z);

        glm::vec3 normal = glm::normalize(glm::dvec3(v) - arrowHeadStartPos);
        _vertexArray.push_back(normal.x);
        _vertexArray.push_back(normal.y);
        _vertexArray.push_back(normal.z);
    }

    // TODO: when generalizing, add another circle on top

    // Ring 3
    // Then all arrow head bottom vertices. Sore normal for top position use
    std::vector<glm::vec3> coneNormals;
    coneNormals.reserve(nSegments);
    for (const glm::vec3& v : arrowHeadVertices) {
        _vertexArray.push_back(v.x);
        _vertexArray.push_back(v.y);
        _vertexArray.push_back(v.z);

        // Project vector from end to center on cone side to find normal
        glm::vec3 c = glm::vec3(arrowHeadStartPos);
        glm::vec3 side = glm::normalize(endPos - glm::dvec3(v));
        glm::vec3 edgeToCenter = glm::normalize(c - v);
        glm::vec3 projected = glm::proj(edgeToCenter, side);
        glm::vec3 normal = glm::normalize(-edgeToCenter + projected);
        coneNormals.push_back(normal);

        _vertexArray.push_back(normal.x);
        _vertexArray.push_back(normal.y);
        _vertexArray.push_back(normal.z);
    }

    // Ring 4
    // Center top vertex (arrow head point). Note that we need multiple vertices
    // to get correct normal
    int normalCounter = 0;
    for (const glm::vec3& v : arrowHeadVertices) {
        _vertexArray.push_back(static_cast<float>(endPos.x));
        _vertexArray.push_back(static_cast<float>(endPos.y));
        _vertexArray.push_back(static_cast<float>(endPos.z));

        glm::vec3 normal = coneNormals[normalCounter++];
        _vertexArray.push_back(normal.x);
        _vertexArray.push_back(normal.y);
        _vertexArray.push_back(normal.z);
    }

    unsigned int botCenterIndex = 0;

    // Build triangle list from the given vertices
    for (unsigned int i = 0; i < _segments; ++i) {
        bool isLast = (i == _segments - 1);
        unsigned int v0, v1, v2, v3;

        // Bot triangle
        v0 = ringVerticeIndex(0, i);
        v1 = ringVerticeIndex(0, isLast ? 0 : i + 1);
        _indexArray.push_back(botCenterIndex);
        _indexArray.push_back(v1);
        _indexArray.push_back(v0);

        // Side of cylinder

        // Bottom ring
        v0 = ringVerticeIndex(1, i);
        v1 = ringVerticeIndex(1, isLast ? 0 : i + 1);
        // Top ring
        v2 = ringVerticeIndex(2, i);
        v3 = ringVerticeIndex(2, isLast ? 0 : i + 1);
        _indexArray.push_back(v0);
        _indexArray.push_back(v1);
        _indexArray.push_back(v2);

        _indexArray.push_back(v1);
        _indexArray.push_back(v3);
        _indexArray.push_back(v2);
        
        // Arrow head

        // Bottom vertices
        v0 = ringVerticeIndex(3, i);
        v1 = ringVerticeIndex(3, isLast ? 0 : i + 1);
        // Top vertex
        v3 = ringVerticeIndex(4, i);

        _indexArray.push_back(v0);
        _indexArray.push_back(v1);
        _indexArray.push_back(v3);

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

    glm::dmat4 normalTransform = glm::transpose(glm::inverse(modelViewTransform));

    _shaderProgram->activate();

    _shaderProgram->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _shaderProgram->setUniform("projectionTransform", data.camera.projectionMatrix());
    _shaderProgram->setUniform("normalTransform", glm::mat3(normalTransform));
    _shaderProgram->setUniform("color", _color);
    _shaderProgram->setUniform("opacity", opacity());

    _shaderProgram->setUniform("ambientIntensity", _shading.ambientIntensity);
    _shaderProgram->setUniform("diffuseIntensity", _shading.diffuseIntensity);
    _shaderProgram->setUniform("specularIntensity", _shading.specularIntensity);
    _shaderProgram->setUniform("performShading", _shading.enabled);

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
