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

#include <modules/base/rendering/renderablenodearrow.h>

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
    constexpr std::string_view _loggerCat = "RenderableNodeArrow";

    constexpr openspace::properties::Property::PropertyInfo StartNodeInfo = {
        "StartNode",
        "Start Node",
        "The identifier of the node the arrow starts from.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo EndNodeInfo = {
        "EndNode",
        "End Node",
        "The identifier of the node the arrow should point towards.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The RGB color for the arrow.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "The number of segments that the shapes of the arrow are divided into. A higher "
        "number leads to a higher resolution and smoother shape.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo InvertInfo = {
        "Invert",
        "Invert Direction",
        "If true, the arrow direction is inverted so that it points to the start node "
        "instead of the end node.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ArrowHeadSizeInfo = {
        "ArrowHeadSize",
        "Arrow Head Size",
        "The length of the arrow head, given in relative value of the entire length of "
        "the arrow. For example, 0.1 makes the arrow head length be 10% of the full "
        "arrow length.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ArrowHeadWidthInfo = {
        "ArrowHeadWidthFactor",
        "Arrow Head Width Factor",
        "A factor that is multiplied with the width, or the arrow itself, to determine "
        "the width of the base of the arrow head.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OffsetDistanceInfo = {
        "Offset",
        "Offset Distance",
        "The distance from the center of the start node where the arrow starts. "
        "If 'UseRelativeOffset' is true, the value should be given as a factor to "
        "multiply with the bounding sphere of the node. Otherwise, the value is "
        "specified in meters.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RelativeOffsetInfo = {
        "UseRelativeOffset",
        "Use Relative Offset Distance",
        "Decides whether to use relative distances for the offset distance. This means "
        "that the offset distance will be computed as the provided 'Offset' value times "
        "the bounding sphere of the start node. If false, meters is used.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LengthInfo = {
        "Length",
        "Length",
        "The length of the arrow, given either in meters or as a factor to be "
        "multiplied with the bounding sphere of the start node (if "
        "'UseRelativeLength' is true).",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RelativeLengthInfo = {
        "UseRelativeLength",
        "Use Relative Length",
        "Decides whether to use relative size for the length of the arrow. This means "
        "that the arrow length will be computed as the provided 'Length' value times "
        "the bounding sphere of the start node. If false, meters is used.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo WidthInfo = {
        "Width",
        "Width",
        "The width of the arrow, in meters.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AmbientIntensityInfo = {
        "AmbientIntensity",
        "Ambient Intensity",
        "A multiplier for ambient lighting for the shading of the arrow.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DiffuseIntensityInfo = {
        "DiffuseIntensity",
        "Diffuse Intensity",
        "A multiplier for diffuse lighting for the shading of the arrow.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SpecularIntensityInfo = {
        "SpecularIntensity",
        "Specular Intensity",
        "A multiplier for specular lighting for the shading of the arrow.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ShadingEnabledInfo = {
        "PerformShading",
        "Perform Shading",
        "Determines whether shading should be applied to the arrow model.",
        openspace::properties::Property::Visibility::User
    };

    // A RenderableNodeArrow can be used to create a 3D arrow pointing in the direction
    // of one scene graph node to another.
    //
    // The arrow will be placed at the `StartNode` at a distance of the provided
    // `Offset` value. Per default, the `Length` and `Offset` of the arrow is specified
    // in meters, but they may also be specified as a multiplier of the bounding sphere
    // of the `StartNode`. The look of the arrow can be customized to change the width
    // and length of both the arrow body and head.
    struct [[codegen::Dictionary(RenderableNodeArrow)]] Parameters {
        // [[codegen::verbatim(StartNodeInfo.description)]]
        std::string startNode [[codegen::identifier()]];

        // [[codegen::verbatim(EndNodeInfo.description)]]
        std::string endNode [[codegen::identifier()]];

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(SegmentsInfo.description)]]
        std::optional<int> segments [[codegen::greaterequal(3)]];

        // [[codegen::verbatim(InvertInfo.description)]]
        std::optional<bool> invert;

        // [[codegen::verbatim(ArrowHeadSizeInfo.description)]]
        std::optional<float> arrowHeadSize [[codegen::greaterequal(0.f)]];

        // [[codegen::verbatim(ArrowHeadWidthInfo.description)]]
        std::optional<float> arrowHeadWidthFactor [[codegen::greaterequal(0.f)]];

        // [[codegen::verbatim(OffsetDistanceInfo.description)]]
        std::optional<float> offset;

        // [[codegen::verbatim(RelativeOffsetInfo.description)]]
        std::optional<bool> useRelativeOffset;

        // [[codegen::verbatim(LengthInfo.description)]]
        std::optional<float> length [[codegen::greaterequal(0.f)]];

        // [[codegen::verbatim(RelativeLengthInfo.description)]]
        std::optional<bool> useRelativeLength;

        // [[codegen::verbatim(WidthInfo.description)]]
        std::optional<float> width [[codegen::greaterequal(0.f)]];

        // [[codegen::verbatim(ShadingEnabledInfo.description)]]
        std::optional<float> performShading;

        // [[codegen::verbatim(AmbientIntensityInfo.description)]]
        std::optional<float> ambientIntensity [[codegen::greaterequal(0.f)]];

        // [[codegen::verbatim(DiffuseIntensityInfo.description)]]
        std::optional<float> diffuseIntensity [[codegen::greaterequal(0.f)]];

        // [[codegen::verbatim(SpecularIntensityInfo.description)]]
        std::optional<float> specularIntensity [[codegen::greaterequal(0.f)]];
    };
#include "renderablenodearrow_codegen.cpp"

    void updateDistanceBasedOnRelativeValues(const std::string& nodeName,
                                             bool useRelative,
                                             openspace::properties::FloatProperty& prop)
    {
        using namespace::openspace;

        SceneGraphNode* startNode = sceneGraphNode(nodeName);
        if (!startNode) {
            LERROR(std::format("Could not find start node '{}'", nodeName));
            return;
        }
        const double boundingSphere = startNode->boundingSphere();

        if (!useRelative) {
            // Recompute distance (previous value was relative)
            prop = static_cast<float>(prop * boundingSphere);
            prop.setExponent(11.f);
            prop.setMaxValue(1e20f);
        }
        else {
            // Recompute distance (previous value was in meters)
            if (boundingSphere < std::numeric_limits<double>::epsilon()) {
                LERROR(std::format(
                    "Start node '{}' has invalid bounding sphere", nodeName
                ));
                return;
            }
            prop = static_cast<float>(prop / boundingSphere);
            prop.setExponent(3.f);
            prop.setMaxValue(1000.f);
        }
        // @TODO (emmbr, 2022-08-22): make GUI update when min/max value is updated
    }
} // namespace

namespace openspace {

documentation::Documentation RenderableNodeArrow::Documentation() {
    return codegen::doc<Parameters>("base_renderable_renderablenodearrow");
}

RenderableNodeArrow::Shading::Shading()
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

RenderableNodeArrow::RenderableNodeArrow(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _start(StartNodeInfo)
    , _end(EndNodeInfo)
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

    _shading.enabled = p.performShading.value_or(_shading.enabled);
    _shading.ambientIntensity = p.ambientIntensity.value_or(_shading.ambientIntensity);
    _shading.diffuseIntensity = p.diffuseIntensity.value_or(_shading.diffuseIntensity);
    _shading.specularIntensity = p.specularIntensity.value_or(_shading.specularIntensity);
    addPropertySubOwner(_shading);

    addProperty(Fadeable::_opacity);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    _start = p.startNode;
    addProperty(_start);

    _end = p.endNode;
    addProperty(_end);

    _segments = p.segments.value_or(_segments);
    addProperty(_segments);

    _invertArrowDirection = p.invert.value_or(_invertArrowDirection);
    addProperty(_invertArrowDirection);

    _arrowHeadSize = p.arrowHeadSize.value_or(_arrowHeadSize);
    addProperty(_arrowHeadSize);

    _arrowHeadWidthFactor = p.arrowHeadWidthFactor.value_or(_arrowHeadWidthFactor);
    addProperty(_arrowHeadWidthFactor);

    _width = p.width.value_or(_width);
    _width.setExponent(10.f);
    addProperty(_width);

    _useRelativeLength.onChange([this]() {
        updateDistanceBasedOnRelativeValues(_start, _useRelativeLength, _length);
    });
    _useRelativeLength = p.useRelativeLength.value_or(_useRelativeLength);

    _length = p.length.value_or(_length);
    if (!_useRelativeLength) {
        _length.setExponent(11.f);
    }
    addProperty(_length);

    _useRelativeOffset.onChange([this]() {
        updateDistanceBasedOnRelativeValues(_start, _useRelativeOffset, _offsetDistance);
    });
    _useRelativeOffset = p.useRelativeOffset.value_or(_useRelativeOffset);

    _offsetDistance = p.offset.value_or(_offsetDistance);
    if (!_useRelativeOffset) {
        _offsetDistance.setExponent(11.f);
    }
    addProperty(_offsetDistance);

    addProperty(_useRelativeLength);
    addProperty(_useRelativeOffset);
}

void RenderableNodeArrow::initializeGL() {
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

void RenderableNodeArrow::deinitializeGL() {
    BaseModule::ProgramObjectManager.release(
        "NodeDirectionLineProgram",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _shaderProgram = nullptr;
}

bool RenderableNodeArrow::isReady() const {
    return _shaderProgram;
}

void RenderableNodeArrow::updateShapeTransforms(const RenderData& data) {
    SceneGraphNode* startNode = sceneGraphNode(_start);
    SceneGraphNode* endNode = sceneGraphNode(_end);

    if (!startNode) {
        LERROR(std::format("Could not find start node '{}'", _start.value()));
        return;
    }

    if (!endNode) {
        LERROR(std::format("Could not find end node '{}'", _end.value()));
        return;
    }

    const double boundingSphere = startNode->boundingSphere();
    const bool hasNoBoundingSphere =
        boundingSphere < std::numeric_limits<double>::epsilon();

    if (hasNoBoundingSphere && (_useRelativeLength || _useRelativeOffset)) {
        LERROR(std::format(
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

    // Take additional transformation scale into account
    const glm::dmat4 s = glm::scale(
        glm::dmat4(1.0),
        glm::dvec3(data.modelTransform.scale)
    );

    // Update the position based on the arrowDirection of the nodes
    const glm::dvec3 startNodePos = startNode->worldPosition();
    const glm::dvec3 endNodePos = endNode->worldPosition();

    glm::dvec3 arrowDirection = glm::normalize(endNodePos - startNodePos);
    glm::dvec3 startPos = glm::dvec3(startNodePos + offset * arrowDirection);
    glm::dvec3 endPos = glm::dvec3(startPos + length * arrowDirection);

    if (_invertArrowDirection) {
        std::swap(startPos, endPos);
        arrowDirection *= -1.0;
    }

    const double coneLength = _arrowHeadSize * length;
    const double cylinderLength = length - coneLength;
    const double arrowHeadWidth = _width * _arrowHeadWidthFactor;

    // Create transformation matrices to reshape to size and position
    _cylinderTranslation = glm::translate(glm::dmat4(1.0), startPos);
    const glm::dvec3 cylinderScale = glm::dvec3(
        s * glm::dvec4(_width, _width, cylinderLength, 0.0)
    );
    _cylinderScale = glm::scale(glm::dmat4(1.0), cylinderScale);

    // Adapt arrow head start to scaled size
    const glm::dvec3 arrowHeadStartPos = startPos + cylinderScale.z * arrowDirection;

    _coneTranslation = glm::translate(glm::dmat4(1.0), arrowHeadStartPos);
    const glm::dvec3 coneScale = glm::dvec3(arrowHeadWidth, arrowHeadWidth, coneLength);
    _coneScale = s * glm::scale(glm::dmat4(1.0), coneScale);

    // Rotation to point at the end node
    const glm::quat rotQuat = glm::rotation(glm::dvec3(0.0, 0.0, 1.0), arrowDirection);
    _pointDirectionRotation = glm::dmat4(glm::toMat4(rotQuat));

    setBoundingSphere(length + offset);
}

void RenderableNodeArrow::render(const RenderData& data, RendererTasks&) {
    updateShapeTransforms(data);

    // Cylinder transforms
    glm::dmat4 modelTransform =
        _cylinderTranslation * _pointDirectionRotation * _cylinderScale;
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
    modelTransform = _coneTranslation * _pointDirectionRotation * _coneScale;
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
