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

#include <modules/base/rendering/renderablenodedirectionhint.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/query/query.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/translation.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <glm/gtx/projection.hpp>
#include <glm/gtx/transform.hpp>

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

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
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

    struct [[codegen::Dictionary(RenderableNodeDirectionHint)]] Parameters {
        // [[codegen::verbatim(StartNodeInfo.description)]]
        std::optional<std::string> startNode;

        // [[codegen::verbatim(EndNodeInfo.description)]]
        std::optional<std::string> endNode;

        // [[codegen::verbatim(ColorInfo.description)]]
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
    , _color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
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
    _width.onChange([this]() { _shapeIsDirty = true; });
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
            _length = static_cast<float>(_length * startNode->boundingSphere());
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
            _length = static_cast<float>(_length / startNode->boundingSphere());
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
            _offsetDistance = static_cast<float>(_offsetDistance * boundingSphere);
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
            _offsetDistance = static_cast<float>(_offsetDistance / boundingSphere);
            _offsetDistance.setExponent(3.f);
            _offsetDistance.setMaxValue(1000.f);
        }
    });
    // @TODO (emmbr, 2022-08-22): make GUI update when min/max value is updated

    _useRelativeOffset = p.useRelativeOffset.value_or(_useRelativeOffset);
    _useRelativeOffset.onChange([this]() { _shapeIsDirty = true; });

    _offsetDistance = p.offset.value_or(_offsetDistance);
    _offsetDistance.onChange([this]() { _shapeIsDirty = true; });
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
        "NodeDirectionLineProgram",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "NodeDirectionLineProgram",
                absPath("${MODULE_BASE}/shaders/arrow_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/arrow_fs.glsl")
            );
        }
    );
}

void RenderableNodeDirectionHint::deinitializeGL() {
    BaseModule::ProgramObjectManager.release(
        "NodeDirectionLineProgram",
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

void RenderableNodeDirectionHint::updateShapeTransforms() {
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
    glm::dvec3 startNodePos = startNode->worldPosition();
    glm::dvec3 endNodePos = endNode->worldPosition();

    // TODO: check that distance is larger than arrow length?

    glm::dvec3 arrowDirection = glm::normalize(endNodePos - startNodePos);
    glm::dvec3 startPos = glm::dvec3(startNodePos + offset * arrowDirection);
    glm::dvec3 endPos = glm::dvec3(startPos + length * arrowDirection);

    if (_invertArrowDirection) {
        std::swap(startPos, endPos);
        arrowDirection *= -1;
    }

    double coneLength = _arrowHeadSize * length;
    double cylinderLength = length - coneLength;
    glm::dvec3 arrowHeadStartPos = startPos + cylinderLength * arrowDirection;
    double arrowHeadWidth = _width * _arrowHeadWidthFactor;

    // Create transformation matrices to reshape to size and position
    _cylinderTranslation = glm::translate(glm::dmat4(1.0), startPos);
    glm::dvec3 scale = glm::dvec3(_width, _width, cylinderLength);
    _cylinderScale = glm::scale(glm::dmat4(1.0), scale);

    _coneTranslation = glm::translate(glm::dmat4(1.0), arrowHeadStartPos);
    scale = glm::dvec3(arrowHeadWidth, arrowHeadWidth, coneLength);
    _coneScale = glm::scale(glm::dmat4(1.0), scale);

    // Rotation to point at the end node
    glm::quat rotQuat = glm::rotation(glm::dvec3(0.0, 0.0, 1.0), arrowDirection);
    _pointDirectionRotation = glm::toMat4(rotQuat);

    _shapeIsDirty = false;
}

void RenderableNodeDirectionHint::update(const UpdateData&) {
    SceneGraphNode* startNode = sceneGraphNode(_start);
    SceneGraphNode* endNode = sceneGraphNode(_end);

    // Check if any of the targetted nodes moved
    bool shouldUpdate = _shapeIsDirty;
    if (startNode && endNode) {
        constexpr float Epsilon = std::numeric_limits<float>::epsilon();
        const float combinedDistanceChange = static_cast<float>(
            glm::distance(startNode->worldPosition(), _prevStartNodePosition) +
            glm::distance(endNode->worldPosition(), _prevEndNodePosition)
        );
        if (combinedDistanceChange > Epsilon) {
            shouldUpdate = true;
        }

        _prevStartNodePosition = startNode->worldPosition();
        _prevEndNodePosition = endNode->worldPosition();
    }

    if (shouldUpdate) {
        updateShapeTransforms();
    }
}

void RenderableNodeDirectionHint::render(const RenderData& data, RendererTasks&) {
    const glm::dmat4 T = glm::translate(glm::dmat4(1.0), data.modelTransform.translation);
    const glm::dmat4 R = glm::dmat4(data.modelTransform.rotation);
    const glm::dmat4 S = glm::scale(
        glm::dmat4(1.0),
        glm::dvec3(data.modelTransform.scale)
    );

    // Cylinder transforms
    glm::dmat4 modelTransform = _cylinderTranslation * T * _pointDirectionRotation * R *
        _cylinderScale * S;

    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
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

    // Draw cylinder
    glBindVertexArray(rendering::helper::vertexObjects.cylinder.vao);
    glDrawElements(
        GL_TRIANGLES,
        rendering::helper::vertexObjects.cylinder.nElements,
        GL_UNSIGNED_SHORT,
        nullptr
    );

    // Update transforms and render cone
    modelTransform = _coneTranslation * T * _pointDirectionRotation * R * _coneScale * S;
    modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
    normalTransform = glm::transpose(glm::inverse(modelViewTransform));

    _shaderProgram->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _shaderProgram->setUniform("normalTransform", glm::mat3(normalTransform));

    glBindVertexArray(rendering::helper::vertexObjects.cone.vao);
    glDrawElements(
        GL_TRIANGLES,
        rendering::helper::vertexObjects.cone.nElements,
        GL_UNSIGNED_SHORT,
        nullptr
    );

    // Restore GL State
    glBindVertexArray(0);
    global::renderEngine->openglStateCache().resetBlendState();

    _shaderProgram->deactivate();
}

} // namespace openspace
