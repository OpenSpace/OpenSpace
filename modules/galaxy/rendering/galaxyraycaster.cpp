/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/galaxy/rendering/galaxyraycaster.h>

#include <openspace/rendering/renderable.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr const char* GlslRaycastPath =
        "${MODULES}/galaxy/shaders/galaxyraycast.glsl";
    constexpr const char* GlslBoundsVsPath =
        "${MODULES}/galaxy/shaders/raycasterbounds.vs";
    constexpr const char* GlslBoundsFsPath =
        "${MODULES}/galaxy/shaders/raycasterbounds.fs";
} // namespace

namespace openspace {

GalaxyRaycaster::GalaxyRaycaster(ghoul::opengl::Texture& texture)
    : _boundingBox(glm::vec3(1.0))
    , _texture(texture)
    , _textureUnit(nullptr)
{}

GalaxyRaycaster::~GalaxyRaycaster() {}

void GalaxyRaycaster::initialize() {
    _boundingBox.initialize();
}

void GalaxyRaycaster::renderEntryPoints(const RenderData& data,
                                        ghoul::opengl::ProgramObject& program)
{
    program.setUniform("modelTransform", _modelTransform);
    program.setUniform("viewProjection", data.camera.viewProjectionMatrix());
    program.setUniform("blendMode", static_cast<unsigned int>(1));

    Renderable::setPscUniforms(program, data.camera, data.position);

    // Cull back face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    // Render bounding geometry
    _boundingBox.render();
}

void GalaxyRaycaster::renderExitPoints(const RenderData& data,
                                       ghoul::opengl::ProgramObject& program)
{
    // Uniforms
    program.setUniform("modelTransform", _modelTransform);
    program.setUniform("viewProjection", data.camera.viewProjectionMatrix());
    program.setUniform("blendMode", static_cast<unsigned int>(1));
    Renderable::setPscUniforms(program, data.camera, data.position);

    // Cull front face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);

    // Render bounding geometry
    _boundingBox.render();

    // Restore defaults
    glCullFace(GL_BACK);
}

void GalaxyRaycaster::preRaycast(const RaycastData& data,
                                 ghoul::opengl::ProgramObject& program)
{
    const std::string colorUniformName = "color" + std::to_string(data.id);
    const std::string stepSizeUniformName = "maxStepSize" + std::to_string(data.id);
    const std::string galaxyTextureUniformName = "galaxyTexture" +
                                                 std::to_string(data.id);
    const std::string volumeAspectUniformName = "aspect" + std::to_string(data.id);
    const std::string opacityCoefficientUniformName = "opacityCoefficient" +
                                                      std::to_string(data.id);

    program.setUniform(volumeAspectUniformName, _aspect);
    program.setUniform(stepSizeUniformName, _stepSize);
    program.setUniform(opacityCoefficientUniformName, _opacityCoefficient);

    _textureUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _textureUnit->activate();
    _texture.bind();
    program.setUniform(galaxyTextureUniformName, *_textureUnit);
}

void GalaxyRaycaster::postRaycast(const RaycastData&, ghoul::opengl::ProgramObject&) {
    _textureUnit = nullptr; // release texture unit.
}

bool GalaxyRaycaster::isCameraInside(const RenderData& data, glm::vec3& localPosition) {
    // Camera rig position in world coordinates.
    const glm::vec4 rigWorldPos = glm::vec4(data.camera.position().vec3(), 1.0);
    //rigWorldPos /= data.camera.scaling().x * pow(10.0, data.camera.scaling().y);
    //glm::mat4 invSgctMatrix = glm::inverse(data.camera.viewMatrix());

    // Camera position in world coordinates.
    const glm::vec4 camWorldPos = rigWorldPos;
    const glm::vec3 objPos = data.position.vec3();

    const glm::mat4 modelTransform = glm::translate(_modelTransform, objPos);

    float divisor = 1.f;
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            if (abs(modelTransform[i][j] > divisor)) {
                divisor = modelTransform[i][j];
            }
        }
    }

    const glm::mat4 scaledModelTransform = modelTransform / divisor;

    const glm::vec4 modelPos = (glm::inverse(scaledModelTransform) / divisor) *
                               camWorldPos;

    localPosition = (glm::vec3(modelPos) + glm::vec3(0.5));
    return (localPosition.x > 0 && localPosition.y > 0 &&
            localPosition.z > 0 && localPosition.x < 1 &&
            localPosition.y < 1 && localPosition.z < 1);
}

std::string GalaxyRaycaster::boundsVertexShaderPath() const {
    return GlslBoundsVsPath;
}

std::string GalaxyRaycaster::boundsFragmentShaderPath() const {
    return GlslBoundsFsPath;
}

std::string GalaxyRaycaster::raycasterPath() const {
    return GlslRaycastPath;
}

std::string GalaxyRaycaster::helperPath() const {
    return ""; // no helper file
}

void GalaxyRaycaster::setAspect(const glm::vec3& aspect) {
    _aspect = aspect / std::max(std::max(aspect.x, aspect.y), aspect.z);
}

void GalaxyRaycaster::setModelTransform(glm::mat4 transform) {
    _modelTransform = transform;
}

void GalaxyRaycaster::setOpacityCoefficient(float opacityCoefficient) {
    _opacityCoefficient = opacityCoefficient;
}

void GalaxyRaycaster::setTime(double time) {
    _time = time;
}

void GalaxyRaycaster::setStepSize(float stepSize) {
    _stepSize = stepSize;
}

} // namespace openspace
