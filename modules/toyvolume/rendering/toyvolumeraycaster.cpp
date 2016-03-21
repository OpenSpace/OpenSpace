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

#include <modules/toyvolume/rendering/toyvolumeraycaster.h>

#include <glm/glm.hpp>
#include <ghoul/opengl/ghoul_gl.h>
#include <sstream>
#include <ghoul/opengl/programobject.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>

namespace {
    const std::string GlslRaycastPath = "${MODULES}/toyvolume/shaders/raycast.glsl";
    const std::string GlslBoundsVsPath = "${MODULES}/toyvolume/shaders/boundsVs.glsl";
    const std::string GlslBoundsFsPath = "${MODULES}/toyvolume/shaders/boundsFs.glsl";
}

namespace openspace {

ToyVolumeRaycaster::ToyVolumeRaycaster(glm::vec4 color)
    : _boundingBox(glm::vec3(1.0))
    , _color(color) {}
    
ToyVolumeRaycaster::~ToyVolumeRaycaster() {}

void ToyVolumeRaycaster::initialize() {
    _boundingBox.initialize();
}
    
void ToyVolumeRaycaster::deinitialize() {
}
    
void ToyVolumeRaycaster::renderEntryPoints(const RenderData& data, ghoul::opengl::ProgramObject* program) {
    program->setUniform("modelTransform", _modelTransform);
    program->setUniform("viewProjection", data.camera.viewProjectionMatrix());
    Renderable::setPscUniforms(program, &data.camera, data.position);
    
    // Cull back face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    
    // Render bounding geometry
    _boundingBox.render();
}
    
void ToyVolumeRaycaster::renderExitPoints(const RenderData& data, ghoul::opengl::ProgramObject* program) {   
    // Uniforms
    program->setUniform("modelTransform", _modelTransform);
    program->setUniform("viewProjection", data.camera.viewProjectionMatrix());
    Renderable::setPscUniforms(program, &data.camera, data.position);

    // Cull front face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);
    
    // Render bounding geometry
    _boundingBox.render();
    
    // Restore defaults
    glCullFace(GL_BACK);
}
    
void ToyVolumeRaycaster::preRaycast(const RaycastData& data, ghoul::opengl::ProgramObject* program) {
    std::string colorUniformName = "color" + std::to_string(data.id);
    std::string timeUniformName = "time" + std::to_string(data.id);
    std::string stepSizeUniformName = "maxStepSize" + std::to_string(data.id);
    program->setUniform(colorUniformName, _color);
    program->setUniform(stepSizeUniformName, _stepSize);
    program->setUniform(timeUniformName, static_cast<float>(std::fmod(_time, 3600.0)));
}
    
void ToyVolumeRaycaster::postRaycast(const RaycastData& data, ghoul::opengl::ProgramObject* program) {
    // For example: release texture units
}
    
std::string ToyVolumeRaycaster::getBoundsVsPath() const {
    return GlslBoundsVsPath;
}
    
std::string ToyVolumeRaycaster::getBoundsFsPath() const {
    return GlslBoundsFsPath;
}

std::string ToyVolumeRaycaster::getRaycastPath() const {
    return GlslRaycastPath;
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
    
}
