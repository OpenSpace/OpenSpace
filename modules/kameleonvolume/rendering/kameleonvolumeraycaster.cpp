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

#include <modules/kameleonvolume/rendering/kameleonvolumeraycaster.h>

#include <glm/glm.hpp>
#include <ghoul/opengl/ghoul_gl.h>
#include <sstream>
#include <ghoul/opengl/programobject.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>
#include <modules/kameleonvolume/rendering/renderablekameleonvolume.h>


namespace {
    const std::string GlslRaycastPath = "${MODULES}/kameleonvolume/shaders/raycast.glsl";
    const std::string GlslHelperPath = "${MODULES}/kameleonvolume/shaders/helper.glsl";
    const std::string GlslBoundsVsPath = "${MODULES}/kameleonvolume/shaders/boundsvs.glsl";
    const std::string GlslBoundsFsPath = "${MODULES}/kameleonvolume/shaders/boundsfs.glsl";
}

namespace openspace {

KameleonVolumeRaycaster::KameleonVolumeRaycaster(
    std::shared_ptr<ghoul::opengl::Texture> texture,
    std::shared_ptr<TransferFunction> transferFunction,
    std::shared_ptr<VolumeClipPlanes> clipPlanes)
    : _volumeTexture(texture)
    , _transferFunction(transferFunction)
    , _clipPlanes(clipPlanes)
    , _boundingBox(glm::vec3(1.0)) {}
    
KameleonVolumeRaycaster::~KameleonVolumeRaycaster() {}

void KameleonVolumeRaycaster::initialize() {
    _boundingBox.initialize();
}
    
void KameleonVolumeRaycaster::deinitialize() {
}
    
void KameleonVolumeRaycaster::renderEntryPoints(const RenderData& data, ghoul::opengl::ProgramObject& program) {
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(_modelTransform), glm::dvec3(data.modelTransform.scale)));
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    program.setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    program.setUniform("projectionTransform", data.camera.projectionMatrix());
    
    // Cull back face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    
    // Render bounding geometry
    _boundingBox.render();
}
    
void KameleonVolumeRaycaster::renderExitPoints(const RenderData& data, ghoul::opengl::ProgramObject& program) {
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::dmat4(glm::scale(glm::dmat4(_modelTransform), glm::dvec3(data.modelTransform.scale)));
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    program.setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    program.setUniform("projectionTransform", data.camera.projectionMatrix());

    // Cull front face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);
    
    // Render bounding geometry
    _boundingBox.render();
    
    // Restore defaults
    glCullFace(GL_BACK);
}
    
void KameleonVolumeRaycaster::preRaycast(const RaycastData& data, ghoul::opengl::ProgramObject& program) {
    std::string stepSizeUniformName = "maxStepSize" + std::to_string(data.id);
    program.setUniform(stepSizeUniformName, _stepSize);

    std::string id = std::to_string(data.id);

    _tfUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _tfUnit->activate();
    _transferFunction->getTexture().bind();
    program.setUniform("transferFunction_" + id, _tfUnit->unitNumber());

    _textureUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _textureUnit->activate();
    _volumeTexture->bind();
    program.setUniform("volumeTexture_" + id, _textureUnit->unitNumber());

    program.setUniform("gridType_" + id, static_cast<int>(_gridType));

    std::vector<glm::vec3> clipNormals = _clipPlanes->normals();
    std::vector<glm::vec2> clipOffsets = _clipPlanes->offsets();
    int nClips = clipNormals.size();

    program.setUniform("nClips_" + id, nClips);
    program.setUniform("clipNormals_" + id, clipNormals.data(), nClips);
    program.setUniform("clipOffsets_" + id, clipOffsets.data(), nClips);
}
    
void KameleonVolumeRaycaster::postRaycast(const RaycastData& data, ghoul::opengl::ProgramObject& program) {
    // For example: release texture units
    _textureUnit = nullptr;
    _tfUnit = nullptr;
}

bool KameleonVolumeRaycaster::cameraIsInside(const RenderData & data, glm::vec3 & localPosition)
{
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::dmat4(glm::scale(glm::dmat4(_modelTransform), glm::dvec3(data.modelTransform.scale)));
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    glm::vec4 modelPos = glm::inverse(modelViewTransform) * glm::vec4(0.0, 0.0, 0.0, 1.0);

    localPosition = (glm::vec3(modelPos) + glm::vec3(0.5));
    return (localPosition.x > 0 && localPosition.y > 0 && localPosition.z > 0 && localPosition.x < 1 && localPosition.y < 1 && localPosition.z < 1);
}
    
std::string KameleonVolumeRaycaster::getBoundsVsPath() const {
    return GlslBoundsVsPath;
}
    
std::string KameleonVolumeRaycaster::getBoundsFsPath() const {
    return GlslBoundsFsPath;
}

std::string KameleonVolumeRaycaster::getRaycastPath() const {
    return GlslRaycastPath;
}

std::string KameleonVolumeRaycaster::getHelperPath() const {
    return GlslHelperPath; // no helper file
}
    
void KameleonVolumeRaycaster::setStepSize(float stepSize) {
    _stepSize = stepSize;
}

void KameleonVolumeRaycaster::setGridType(VolumeGridType gridType)
{
    _gridType = gridType;
}

void KameleonVolumeRaycaster::setModelTransform(const glm::mat4 & transform) {
    _modelTransform = transform;
}
    
}
