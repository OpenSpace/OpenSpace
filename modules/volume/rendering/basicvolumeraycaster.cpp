/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/volume/rendering/basicvolumeraycaster.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <sstream>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>

namespace {
    const char* GlslRaycastPath = "${MODULES}/volume/shaders/raycast.glsl";
    const char* GlslHelperPath = "${MODULES}/volume/shaders/helper.glsl";
    const char* GlslBoundsVsPath = "${MODULES}/volume/shaders/boundsvs.glsl";
    const char* GlslBoundsFsPath = "${MODULES}/volume/shaders/boundsfs.glsl";
}

namespace openspace {
namespace volume {

BasicVolumeRaycaster::BasicVolumeRaycaster(
    std::shared_ptr<ghoul::opengl::Texture> volumeTexture,
    std::shared_ptr<TransferFunction> transferFunction,
    std::shared_ptr<VolumeClipPlanes> clipPlanes)
    : _volumeTexture(volumeTexture)
    , _transferFunction(transferFunction)
    , _clipPlanes(clipPlanes)
    , _boundingBox(glm::vec3(1.0))
    , _opacity(20.0)
    , _rNormalization(0.0)
    {}
    
BasicVolumeRaycaster::~BasicVolumeRaycaster() {}

void BasicVolumeRaycaster::initialize() {
    _boundingBox.initialize();
}
    
void BasicVolumeRaycaster::deinitialize() {}
    
void BasicVolumeRaycaster::renderEntryPoints(
    const RenderData& data,
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

glm::dmat4 BasicVolumeRaycaster::modelViewTransform(const RenderData& data) {
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)) *
        glm::dmat4(_modelTransform);

    return data.camera.combinedViewMatrix() * modelTransform;
}

    
void BasicVolumeRaycaster::renderExitPoints(
    const RenderData& data,
    ghoul::opengl::ProgramObject& program)
{
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
    
void BasicVolumeRaycaster::preRaycast(
    const RaycastData& data,
    ghoul::opengl::ProgramObject& program)
{
    if (!_volumeTexture || !_transferFunction) {
        return;
    }

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
    program.setUniform("opacity_" + id, _opacity);
    program.setUniform("rNormalization_" + id, _rNormalization);
}
    
void BasicVolumeRaycaster::postRaycast(
    const RaycastData& data,
    ghoul::opengl::ProgramObject& program)
{
    // For example: release texture units
    _textureUnit = nullptr;
    _tfUnit = nullptr;
}



bool BasicVolumeRaycaster::cameraIsInside(
    const RenderData & data,
    glm::vec3 & localPosition)
{
    glm::vec4 modelPos =
        glm::inverse(modelViewTransform(data)) * glm::vec4(0.0, 0.0, 0.0, 1.0);

    localPosition = (glm::vec3(modelPos) + glm::vec3(0.5));

    return (localPosition.x > 0 && localPosition.x < 1 &&
            localPosition.y > 0 && localPosition.y < 1 &&
            localPosition.z > 0 && localPosition.z < 1);
}
    
std::string BasicVolumeRaycaster::getBoundsVsPath() const {
    return GlslBoundsVsPath;
}
    
std::string BasicVolumeRaycaster::getBoundsFsPath() const {
    return GlslBoundsFsPath;
}

std::string BasicVolumeRaycaster::getRaycastPath() const {
    return GlslRaycastPath;
}

std::string BasicVolumeRaycaster::getHelperPath() const {
    return GlslHelperPath;
}

void BasicVolumeRaycaster::setTransferFunction(
    std::shared_ptr<TransferFunction> transferFunction)
{
    _transferFunction = transferFunction;
}

void BasicVolumeRaycaster::setVolumeTexture(
    std::shared_ptr<ghoul::opengl::Texture> volumeTexture)
{
    _volumeTexture = volumeTexture;
}

std::shared_ptr<ghoul::opengl::Texture> BasicVolumeRaycaster::volumeTexture() const {
    return _volumeTexture;
}

void BasicVolumeRaycaster::setStepSize(float stepSize) {
    _stepSize = stepSize;
}

void BasicVolumeRaycaster::setOpacity(float opacity) {
    _opacity = opacity;
}

float BasicVolumeRaycaster::opacity() const {
    return _opacity;
}

void BasicVolumeRaycaster::setRNormalization(float rNormalization) {
    _rNormalization = rNormalization;
}

float BasicVolumeRaycaster::rNormalization() const {
    return _rNormalization;
}

VolumeGridType BasicVolumeRaycaster::gridType() const {
    return _gridType;
}

void BasicVolumeRaycaster::setGridType(VolumeGridType gridType) {
    _gridType = gridType;
}

void BasicVolumeRaycaster::setModelTransform(const glm::mat4 & transform) {
    _modelTransform = transform;
}

} // namespace volume
} // namespace openspace
