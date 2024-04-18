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

#include <modules/galaxy/rendering/galaxyraycaster.h>

#include <openspace/rendering/renderable.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr std::string_view GlslRaycastPath =
        "${MODULES}/galaxy/shaders/galaxyraycast.glsl";
    constexpr std::string_view GlslBoundsVsPath =
        "${MODULES}/galaxy/shaders/raycasterbounds_vs.glsl";
    constexpr std::string_view GlslBoundsFsPath =
        "${MODULES}/galaxy/shaders/raycasterbounds_fs.glsl";
} // namespace

namespace openspace {

GalaxyRaycaster::GalaxyRaycaster(ghoul::opengl::Texture& texture,
                             const std::optional<std::filesystem::path>& raycastingShader)
    : _boundingBox(glm::vec3(1.f))
    , _texture(texture)
    , _textureUnit(nullptr)
    , _raycastingShader(raycastingShader.value_or(GlslRaycastPath))
{}

void GalaxyRaycaster::initialize() {
    ZoneScoped;

    _boundingBox.initialize();
}

void GalaxyRaycaster::renderEntryPoints(const RenderData& data,
                                        ghoul::opengl::ProgramObject& program)
{
    program.setUniform("modelViewTransform", modelViewTransform(data));
    program.setUniform("projectionTransform", data.camera.projectionMatrix());

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
    program.setUniform("modelViewTransform", modelViewTransform(data));
    program.setUniform("projectionTransform", data.camera.projectionMatrix());

    // Cull front face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);

    // Render bounding geometry
    _boundingBox.render();

    // Restore defaults
    glCullFace(GL_BACK);
}

glm::dmat4 GalaxyRaycaster::modelViewTransform(const RenderData& data) {
    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)) *
        glm::dmat4(_modelTransform);

    return data.camera.combinedViewMatrix() * modelTransform;
}

void GalaxyRaycaster::preRaycast(const RaycastData& data,
                                 ghoul::opengl::ProgramObject& program)
{
    const std::string stepSizeUniformName = "maxStepSize" + std::to_string(data.id);
    const std::string galaxyTextureUniformName = "galaxyTexture" +
                                                 std::to_string(data.id);
    const std::string volumeAspectUniformName = "aspect" + std::to_string(data.id);
    const std::string opacityCoefficientUniformName = "opacityCoefficient" +
                                                      std::to_string(data.id);

    const std::string absorptionMultiplyUniformName = "absorptionMultiply" +
                                                  std::to_string(data.id);

    const std::string emissionMultiplyUniformName = "emissionMultiply" +
                                                    std::to_string(data.id);

    program.setUniform(volumeAspectUniformName, _aspect);
    program.setUniform(stepSizeUniformName, _stepSize);
    program.setUniform(opacityCoefficientUniformName, _opacityCoefficient);
    program.setUniform(absorptionMultiplyUniformName, _absorptionMultiply);
    program.setUniform(emissionMultiplyUniformName, _emissionMultiply);

    _textureUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _textureUnit->activate();
    _texture.bind();
    program.setUniform(galaxyTextureUniformName, *_textureUnit);
}

void GalaxyRaycaster::postRaycast(const RaycastData&, ghoul::opengl::ProgramObject&) {
    _textureUnit = nullptr; // release texture unit.
}

bool GalaxyRaycaster::isCameraInside(const RenderData& data, glm::vec3& localPosition) {
    const glm::vec4 modelPos = glm::inverse(modelViewTransform(data)) *
        glm::vec4(0.f, 0.f, 0.f, 1.f);

    localPosition = (glm::vec3(modelPos) + glm::vec3(0.5f));

    return (localPosition.x > 0 && localPosition.x < 1 &&
        localPosition.y > 0 && localPosition.y < 1 &&
        localPosition.z > 0 && localPosition.z < 1);
}

std::filesystem::path GalaxyRaycaster::boundsVertexShaderPath() const {
    return absPath(GlslBoundsVsPath);
}

std::filesystem::path GalaxyRaycaster::boundsFragmentShaderPath() const {
    return absPath(GlslBoundsFsPath);
}

std::filesystem::path GalaxyRaycaster::raycasterPath() const {
    return _raycastingShader;
}

std::filesystem::path GalaxyRaycaster::helperPath() const {
    return ""; // no helper file
}

void GalaxyRaycaster::setAspect(const glm::vec3& aspect) {
    _aspect = aspect / std::max(std::max(aspect.x, aspect.y), aspect.z);
}

void GalaxyRaycaster::setModelTransform(glm::mat4 transform) {
    _modelTransform = std::move(transform);
}

void GalaxyRaycaster::setOpacityCoefficient(float opacityCoefficient) {
    _opacityCoefficient = opacityCoefficient;
}

void GalaxyRaycaster::setAbsorptionMultiplier(float absorptionMultiply) {
    _absorptionMultiply = absorptionMultiply;
}

void GalaxyRaycaster::setEmissionMultiplier(float emissionMultiply) {
    _emissionMultiply = emissionMultiply;
}

void GalaxyRaycaster::setTime(double time) {
    _time = time;
}

void GalaxyRaycaster::setStepSize(float stepSize) {
    _stepSize = stepSize;
}

} // namespace openspace
