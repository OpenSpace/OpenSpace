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

#include <modules/skybrowser/include/renderableskytarget.h>

#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/base/basemodule.h>
#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    enum BlendMode {
        Normal = 0,
        Additive
    };

    constexpr openspace::properties::Property::PropertyInfo crossHairSizeInfo = {
        "CrosshairSize",
        "Crosshair Size",
        "The size of the crosshair given as a field of view (in degrees).",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RectangleThresholdInfo = {
        "RectangleThreshold",
        "Rectangle Threshold",
        "A threshold value for the vertical field of view, in degrees, that decides when "
        "a rectangle will be used to visualize the target in addition to the crosshair. "
        "When the field of view is smaller than this value, only the crosshair will be "
        "shown.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The thickness of the line of the target. The larger number, the thicker line.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo VerticalFovInfo = {
        "VerticalFov",
        "Vertical Field Of View",
        "The vertical field of view of the target.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyRollInfo = {
       "ApplyRoll",
       "Apply Roll",
       "If true, always rotate the target to have its up direction aligned with the up "
       "direction of the camera.",
       openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableSkyTarget)]] Parameters {
        // [[codegen::verbatim(crossHairSizeInfo.description)]]
        std::optional<float> crossHairSize;

        // [[codegen::verbatim(RectangleThresholdInfo.description)]]
        std::optional<float> rectangleThreshold;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(VerticalFovInfo.description)]]
        std::optional<double> verticalFov;

        // [[codegen::verbatim(ApplyRollInfo.description)]]
        std::optional<bool> applyRoll;
    };

#include "renderableskytarget_codegen.cpp"
} //namespace

namespace openspace {

documentation::Documentation RenderableSkyTarget::Documentation() {
    return codegen::doc<Parameters>("skybrowser_renderableskytarget");
}

RenderableSkyTarget::RenderableSkyTarget(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _crossHairSize(crossHairSizeInfo, 2.f, 1.f, 10.f)
    , _showRectangleThreshold(RectangleThresholdInfo, 5.f, 0.1f, 70.f)
    , _lineWidth(LineWidthInfo, 13.f, 1.f, 100.f)
    , _verticalFov(VerticalFovInfo, 10.0, 0.00000000001, 70.0)
    , _applyRoll(ApplyRollInfo, true)
    , _borderColor(220, 220, 220)
{
    // Handle target dimension property
    _autoScale = false;
    _autoScale.setReadOnly(true);
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _crossHairSize = p.crossHairSize.value_or(_crossHairSize);
    addProperty(_crossHairSize);

    _showRectangleThreshold = p.rectangleThreshold.value_or(_showRectangleThreshold);
    addProperty(_showRectangleThreshold);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _verticalFov= p.verticalFov.value_or(_verticalFov);
    _verticalFov.setReadOnly(true);
    addProperty(_verticalFov);

    addProperty(_applyRoll);
}

void RenderableSkyTarget::bindTexture() {}

void RenderableSkyTarget::initializeGL() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    std::string ProgramName = identifier() + "Shader";

    _shader = BaseModule::ProgramObjectManager.request(
        ProgramName,
        [&ProgramName]() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                ProgramName,
                absPath("${MODULE_SKYBROWSER}/shaders/target_vs.glsl"),
                absPath("${MODULE_SKYBROWSER}/shaders/target_fs.glsl")
            );
        }
    );
}

void RenderableSkyTarget::setColor(glm::ivec3 color) {
    _borderColor = std::move(color);
}

glm::ivec3 RenderableSkyTarget::borderColor() const {
    return _borderColor;
}

glm::dvec3 RenderableSkyTarget::rightVector() const {
    const double scaling =
        (_verticalFov / 70.0) * static_cast<double>(glm::compMax(_size.value()));
    return scaling * _rightVector;
}

glm::dvec3 RenderableSkyTarget::upVector() const {
    const double scaling =
        (_verticalFov / 70.0) * static_cast<double>(glm::compMax(_size.value()));
    return scaling * _upVector;
}

void RenderableSkyTarget::applyRoll() {
    Camera* camera = global::navigationHandler->camera();
    const glm::dvec3 normal = glm::normalize(camera->positionVec3() - _worldPosition);

    _rightVector = glm::normalize(
        glm::cross(camera->lookUpVectorWorldSpace(), normal)
    );
    _upVector = glm::cross(normal, _rightVector);
}

void RenderableSkyTarget::render(const RenderData& data, RendererTasks&) {
    ZoneScoped;
    const bool showRectangle = _verticalFov > _showRectangleThreshold;

    const glm::vec4 color = glm::vec4(glm::vec3(_borderColor) / 255.f, 1.0);

    _shader->activate();
    _shader->setUniform("opacity", opacity());
    _shader->setUniform("crossHairSize", _crossHairSize);
    _shader->setUniform("showRectangle", showRectangle);
    _shader->setUniform("lineWidth", _lineWidth * 0.0001f);
    _shader->setUniform("ratio", _ratio);
    _shader->setUniform("lineColor", color);
    _shader->setUniform("fov", static_cast<float>(_verticalFov));
    _shader->setUniform("borderRadius", static_cast<float>(_borderRadius));

    _worldPosition = glm::dvec3(
        glm::translate(
            glm::dmat4(1.0),
            data.modelTransform.translation) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );

    const glm::dvec3 normal = glm::normalize(data.camera.positionVec3() - _worldPosition);
    // There are two modes - 1) target rolls to have its up vector parallel to the
    // cameras up vector or 2) it is decoupled from the camera, in which case it needs to
    // be initialized once
    if (!_isInitialized || _applyRoll) {
        applyRoll();
        _isInitialized = true;
    }
    else {
        // Use last frames vector for right and don't apply any roll
        _upVector = glm::cross(normal, _rightVector);
        _rightVector = glm::normalize(
            glm::cross(_upVector, normal)
        );
    }

    glm::dmat4 cameraOrientedRotation = glm::dmat4(1.0);
    cameraOrientedRotation[0] = glm::dvec4(_rightVector, 0.0);
    cameraOrientedRotation[1] = glm::dvec4(_upVector, 0.0);
    cameraOrientedRotation[2] = glm::dvec4(normal, 0.0);

    const glm::dmat4 rotationTransform = _billboard ?
        cameraOrientedRotation :
        glm::dmat4(data.modelTransform.rotation);

    auto [modelTransform, modelViewTransform, modelViewProjectionTransform] =
        calcAllTransforms(data, { .rotation = rotationTransform });

    _shader->setUniform(
        "modelViewProjectionTransform",
        glm::mat4(modelViewProjectionTransform)
    );

    _shader->setUniform("modelViewTransform", glm::mat4(modelViewTransform));

    _shader->setUniform("multiplyColor", _multiplyColor);

    const bool additiveBlending = _blendMode == static_cast<int>(BlendMode::Additive);
    if (additiveBlending) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    glBindVertexArray(_quad);
    glEnable(GL_LINE_SMOOTH);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glDisable(GL_LINE_SMOOTH);
    glBindVertexArray(0);

    if (additiveBlending) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }

    _shader->deactivate();
}

void RenderableSkyTarget::setRatio(float ratio) {
    // To avoid flooring of the size of the target, multiply by factor of 100
    // Object size is really the pixel size so this calculation is not exact
    _ratio = ratio;
}

void RenderableSkyTarget::highlight(const glm::ivec3& addition) {
    _borderColor += addition;
}

void RenderableSkyTarget::removeHighlight(const glm::ivec3& removal) {
    _borderColor -= removal;
}

void RenderableSkyTarget::setVerticalFov(double fov) {
    _verticalFov = fov;
}

void RenderableSkyTarget::setBorderRadius(double radius) {
    _borderRadius = radius;
}

} // namespace openspace
