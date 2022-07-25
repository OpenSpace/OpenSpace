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
    constexpr std::string_view _loggerCat = "RenderableSkyTarget";

    enum BlendMode {
        Normal = 0,
        Additive
    };

    constexpr openspace::properties::Property::PropertyInfo crossHairSizeInfo = {
        "CrosshairSize",
        "Crosshair Size",
        "Determines the size of the crosshair. The size is determined in fov (degrees)."
    };

    constexpr openspace::properties::Property::PropertyInfo RectangleThresholdInfo = {
        "RectangleThreshold",
        "Rectangle Threshold",
        "When the field of view is larger than the rectangle threshold, a rectangle will"
        "be rendered in the target."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The thickness of the line of the target. The larger number, the thicker line."
    };

    struct [[codegen::Dictionary(RenderableSkyTarget)]] Parameters {
        // [[codegen::verbatim(crossHairSizeInfo.description)]]
        std::optional<float> crossHairSize;

        // [[codegen::verbatim(RectangleThresholdInfo.description)]]
        std::optional<float> rectangleThreshold;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;
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
    , _borderColor(220, 220, 220)
{
    // Handle target dimension property
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _crossHairSize = p.crossHairSize.value_or(_crossHairSize);
    addProperty(_crossHairSize);

    _showRectangleThreshold = p.rectangleThreshold.value_or(_showRectangleThreshold);
    addProperty(_showRectangleThreshold);

    addProperty(_lineWidth);
}

void RenderableSkyTarget::bindTexture() {}

void RenderableSkyTarget::initializeGL() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    std::string ProgramName = identifier() + "Shader";

    _shader = BaseModule::ProgramObjectManager.request(
        ProgramName,
        [&]() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
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

void RenderableSkyTarget::render(const RenderData& data, RendererTasks&) {
    ZoneScoped
    const bool showRectangle = _verticalFov > _showRectangleThreshold;

    glm::vec4 color = { glm::vec3(_borderColor) / 255.f, 1.0 };

    _shader->activate();
    _shader->setUniform("opacity", opacity());

    _shader->setUniform("crossHairSize", _crossHairSize);
    _shader->setUniform("showRectangle", showRectangle);
    _shader->setUniform("lineWidth", _lineWidth * 0.0001f);
    _shader->setUniform("ratio", _ratio);
    _shader->setUniform("lineColor", color);
    _shader->setUniform("fov", static_cast<float>(_verticalFov));

    glm::dvec3 objectPositionWorld = glm::dvec3(
        glm::translate(
            glm::dmat4(1.0),
            data.modelTransform.translation) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );

    glm::dvec3 normal = glm::normalize(data.camera.positionVec3() - objectPositionWorld);
    glm::dvec3 newRight = glm::normalize(
        glm::cross(data.camera.lookUpVectorWorldSpace(), normal)
    );
    glm::dvec3 newUp = glm::cross(normal, newRight);

    glm::dmat4 cameraOrientedRotation = glm::dmat4(1.0);
    cameraOrientedRotation[0] = glm::dvec4(newRight, 0.0);
    cameraOrientedRotation[1] = glm::dvec4(newUp, 0.0);
    cameraOrientedRotation[2] = glm::dvec4(normal, 0.0);

    const glm::dmat4 rotationTransform = _billboard ?
        cameraOrientedRotation :
        glm::dmat4(data.modelTransform.rotation);

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        rotationTransform *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)) *
        glm::dmat4(1.0);
    const glm::dmat4 modelViewTransform =
        data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform(
        "modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );

    _shader->setUniform(
        "modelViewTransform",
        glm::mat4(data.camera.combinedViewMatrix() * glm::dmat4(modelViewTransform))
    );

    _shader->setUniform("multiplyColor", _multiplyColor);

    bool additiveBlending = (_blendMode == static_cast<int>(BlendMode::Additive));
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

} // namespace openspace
