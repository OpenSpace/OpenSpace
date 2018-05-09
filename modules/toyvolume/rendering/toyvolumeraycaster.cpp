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

#include <modules/toyvolume/rendering/toyvolumeraycaster.h>

#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/filesystem/filesystem.h>

#include <sstream>

namespace {
    const char* GlslRaycastPath = "${MODULE_TOYVOLUME}/shaders/raycast.glsl";
    const char* GlslBoundsVsPath = "${MODULE_TOYVOLUME}/shaders/boundsvs.glsl";
    const char* GlslBoundsFsPath = "${MODULE_TOYVOLUME}/shaders/boundsfs.glsl";
} // namespace

namespace openspace {

ToyVolumeRaycaster::ToyVolumeRaycaster(glm::vec4 color)
    : _boundingBox(glm::vec3(1.0))
    , _color(color)
{}

ToyVolumeRaycaster::~ToyVolumeRaycaster() {}

void ToyVolumeRaycaster::initialize() {
    _boundingBox.initialize();
}

void ToyVolumeRaycaster::deinitialize() {
}

void ToyVolumeRaycaster::renderEntryPoints(const RenderData& data,
                                           ghoul::opengl::ProgramObject& program)
{
    program.setUniform("modelViewTransform", glm::mat4(modelViewTransform(data)));
    program.setUniform("viewProjection", data.camera.viewProjectionMatrix());

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
    program.setUniform("viewProjection", data.camera.viewProjectionMatrix());

    // Cull front face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);

    // Render bounding geometry
    _boundingBox.render();

    // Restore defaults
    glCullFace(GL_BACK);
}

glm::dmat4 ToyVolumeRaycaster::modelViewTransform(const RenderData& data) {
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)) *
        glm::dmat4(_modelTransform);

    return data.camera.combinedViewMatrix() * modelTransform;
}

void ToyVolumeRaycaster::preRaycast(const RaycastData& data,
                                    ghoul::opengl::ProgramObject& program)
{
    std::string colorUniformName = "color" + std::to_string(data.id);
    std::string timeUniformName = "time" + std::to_string(data.id);
    std::string stepSizeUniformName = "maxStepSize" + std::to_string(data.id);
    program.setUniform(colorUniformName, _color);
    program.setUniform(stepSizeUniformName, _stepSize);
    program.setUniform(timeUniformName, static_cast<float>(std::fmod(_time, 3600.0)));
}

void ToyVolumeRaycaster::postRaycast(const RaycastData&, ghoul::opengl::ProgramObject&) {
    // For example: release texture units
}

bool ToyVolumeRaycaster::cameraIsInside(const RenderData& data,
    glm::vec3& localPosition)
{
    glm::vec4 modelPos =
        glm::inverse(modelViewTransform(data)) * glm::vec4(0.0, 0.0, 0.0, 1.0);

    localPosition = (glm::vec3(modelPos) + glm::vec3(0.5));

    return (localPosition.x > 0 && localPosition.x < 1 &&
        localPosition.y > 0 && localPosition.y < 1 &&
        localPosition.z > 0 && localPosition.z < 1);
}

std::string ToyVolumeRaycaster::getBoundsVsPath() const {
    return absPath(GlslBoundsVsPath);
}

std::string ToyVolumeRaycaster::getBoundsFsPath() const {
    return absPath(GlslBoundsFsPath);
}

std::string ToyVolumeRaycaster::getRaycastPath() const {
    return absPath(GlslRaycastPath);
}

std::string ToyVolumeRaycaster::getHelperPath() const {
    return ""; // no helper file
}

void ToyVolumeRaycaster::setColor(glm::vec4 color) {
    _color = color;
}

void ToyVolumeRaycaster::setModelTransform(glm::mat4 transform) {
    _modelTransform = transform;
}

void ToyVolumeRaycaster::setTime(double time) {
    _time = time;
}

void ToyVolumeRaycaster::setStepSize(float stepSize) {
    _stepSize = stepSize;
}

} // namespace openspace
