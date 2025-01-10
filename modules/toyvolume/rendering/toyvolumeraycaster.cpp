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

#include <modules/toyvolume/rendering/toyvolumeraycaster.h>

#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>
#include <vector>
#include <openspace/util/blockplaneintersectiongeometry.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/filesystem/filesystem.h>

#include <sstream>

namespace {
    constexpr std::string_view GlslRaycastPath =
        "${MODULE_TOYVOLUME}/shaders/raycast.glsl";
    constexpr std::string_view GlslBoundsVsPath =
        "${MODULE_TOYVOLUME}/shaders/bounds_vs.glsl";
    constexpr std::string_view GlslBoundsFsPath =
        "${MODULE_TOYVOLUME}/shaders/bounds_fs.glsl";
} // namespace

namespace openspace {

ToyVolumeRaycaster::ToyVolumeRaycaster(glm::vec4 color)
    : _boundingBox(glm::vec3(1.f))
    , _color(std::move(color))
{}

void ToyVolumeRaycaster::initialize() {
    _boundingBox.initialize();
}

void ToyVolumeRaycaster::deinitialize() {}

void ToyVolumeRaycaster::renderEntryPoints(const RenderData& data,
                                           ghoul::opengl::ProgramObject& program)
{
    program.setUniform("modelViewTransform", glm::mat4(modelViewTransform(data)));
    program.setUniform("projectionTransform", data.camera.projectionMatrix());

    // Cull back face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    // Render bounding geometry
    _boundingBox.render();
}

void ToyVolumeRaycaster::renderExitPoints(const RenderData& data,
                                          ghoul::opengl::ProgramObject& program)
{
    // Uniforms
    program.setUniform("modelViewTransform", glm::mat4(modelViewTransform(data)));
    program.setUniform("projectionTransform", data.camera.projectionMatrix());

    // Cull front face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);

    // Render bounding geometry
    _boundingBox.render();

    // Restore defaults
    glCullFace(GL_BACK);
}

glm::dmat4 ToyVolumeRaycaster::modelViewTransform(const RenderData& data) {
    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)) *
        glm::dmat4(_modelTransform);

    return data.camera.combinedViewMatrix() * modelTransform;
}

void ToyVolumeRaycaster::preRaycast(const RaycastData& data,
                                    ghoul::opengl::ProgramObject& program)
{
    const std::string& colorUniformName = "color" + std::to_string(data.id);
    const std::string& timeUniformName = "time" + std::to_string(data.id);
    const std::string& stepSizeUniformName = "maxStepSize" + std::to_string(data.id);
    program.setUniform(colorUniformName, _color);
    program.setUniform(stepSizeUniformName, _stepSize);
    program.setUniform(timeUniformName, static_cast<float>(std::fmod(_time, 3600.0)));
}

void ToyVolumeRaycaster::postRaycast(const RaycastData&, ghoul::opengl::ProgramObject&) {}

bool ToyVolumeRaycaster::isCameraInside(const RenderData& data, glm::vec3& localPosition)
{
    const glm::vec4 modelPos =
        glm::inverse(modelViewTransform(data)) * glm::vec4(0.f, 0.f, 0.f, 1.f);

    localPosition = (glm::vec3(modelPos) + glm::vec3(0.5f));

    return (localPosition.x > 0.f && localPosition.x < 1.f &&
            localPosition.y > 0.f && localPosition.y < 1.f &&
            localPosition.z > 0.f && localPosition.z < 1.f);
}

std::filesystem::path ToyVolumeRaycaster::boundsVertexShaderPath() const {
    return absPath(GlslBoundsVsPath);
}

std::filesystem::path ToyVolumeRaycaster::boundsFragmentShaderPath() const {
    return absPath(GlslBoundsFsPath);
}

std::filesystem::path ToyVolumeRaycaster::raycasterPath() const {
    return absPath(GlslRaycastPath);
}

std::filesystem::path ToyVolumeRaycaster::helperPath() const {
    return ""; // no helper file
}

void ToyVolumeRaycaster::setColor(glm::vec4 color) {
    _color = std::move(color);
}

void ToyVolumeRaycaster::setModelTransform(glm::mat4 transform) {
    _modelTransform = std::move(transform);
}

void ToyVolumeRaycaster::setTime(double time) {
    _time = time;
}

void ToyVolumeRaycaster::setStepSize(float stepSize) {
    _stepSize = stepSize;
}

} // namespace openspace
