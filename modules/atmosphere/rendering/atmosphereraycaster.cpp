/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/atmosphere/rendering/atmosphereraycaster.h>

#include <glm/glm.hpp>
#include <ghoul/opengl/ghoul_gl.h>
#include <sstream>
#include <ghoul/opengl/programobject.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>

namespace {
    const std::string GlslRaycastPath = "${MODULES}/atmosphere/shaders/raycast.glsl";
    const std::string GlslBoundsVsPath = "${MODULES}/atmosphere/shaders/boundsvs.glsl";
    const std::string GlslBoundsFsPath = "${MODULES}/atmosphere/shaders/boundsfs.glsl";
}

namespace openspace {

AtmosphereRaycaster::AtmosphereRaycaster(glm::vec4 color)
    : _boundingBox(glm::vec3(1.0))
    , _color(color) {}
    
AtmosphereRaycaster::~AtmosphereRaycaster() {}

void AtmosphereRaycaster::initialize() {
    _boundingBox.initialize();
}
    
void AtmosphereRaycaster::deinitialize() {
}
    
void AtmosphereRaycaster::renderEntryPoints(const RenderData& data, ghoul::opengl::ProgramObject& program) {
    program.setUniform("modelTransform", _modelTransform);
    program.setUniform("viewProjection", data.camera.viewProjectionMatrix());
    Renderable::setPscUniforms(program, data.camera, data.position);
    
    // Cull back face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    
    // Render bounding geometry
    _boundingBox.render();
}
    
void AtmosphereRaycaster::renderExitPoints(const RenderData& data, ghoul::opengl::ProgramObject& program) {   
    // Uniforms
    program.setUniform("modelTransform", _modelTransform);
    program.setUniform("viewProjection", data.camera.viewProjectionMatrix());
    Renderable::setPscUniforms(program, data.camera, data.position);

    // Cull front face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);
    
    // Render bounding geometry
    _boundingBox.render();
    
    // Restore defaults
    glCullFace(GL_BACK);
}
    
void AtmosphereRaycaster::preRaycast(const RaycastData& data, ghoul::opengl::ProgramObject& program) {
    std::string colorUniformName = "color" + std::to_string(data.id);
    std::string timeUniformName = "time" + std::to_string(data.id);
    std::string stepSizeUniformName = "maxStepSize" + std::to_string(data.id);
    program.setUniform(colorUniformName, _color);
    program.setUniform(stepSizeUniformName, _stepSize);
    program.setUniform(timeUniformName, static_cast<float>(std::fmod(_time, 3600.0)));
}
    
void AtmosphereRaycaster::postRaycast(const RaycastData& data, ghoul::opengl::ProgramObject& program) {
    // For example: release texture units
}
    
std::string AtmosphereRaycaster::getBoundsVsPath() const {
    return GlslBoundsVsPath;
}
    
std::string AtmosphereRaycaster::getBoundsFsPath() const {
    return GlslBoundsFsPath;
}

std::string AtmosphereRaycaster::getRaycastPath() const {
    return GlslRaycastPath;
}

std::string AtmosphereRaycaster::getHelperPath() const {
    return ""; // no helper file
}

void AtmosphereRaycaster::setColor(glm::vec4 color) {
    _color = color;
}

void AtmosphereRaycaster::setModelTransform(glm::mat4 transform) {
    _modelTransform = transform;
}

void AtmosphereRaycaster::setTime(double time) {
    _time = time;
}
    
void AtmosphereRaycaster::setStepSize(float stepSize) {
    _stepSize = stepSize;
}
    
}
