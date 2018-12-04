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

#include <modules/volume/rendering/basicvolumeraycaster.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/filesystem/filesystem.h>
#include <sstream>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderable.h>
#include <modules/volume/transferfunctionhandler.h>
#include <modules/volume/rendering/volumeclipplanes.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr const char* GlslRaycastPath = "${MODULE_VOLUME}/shaders/raycast.glsl";
    constexpr const char* GlslHelperPath = "${MODULE_VOLUME}/shaders/helper.glsl";
    constexpr const char* GlslBoundsVsPath = "${MODULE_VOLUME}/shaders/boundsvs.glsl";
    constexpr const char* GlslBoundsFsPath = "${MODULE_VOLUME}/shaders/boundsfs.glsl";
} // namespace

namespace openspace::volume {

BasicVolumeRaycaster::BasicVolumeRaycaster(
                                    std::shared_ptr<ghoul::opengl::Texture> volumeTexture,
                            std::shared_ptr<openspace::TransferFunction> transferFunction,
                                             std::shared_ptr<VolumeClipPlanes> clipPlanes)
    : _clipPlanes(clipPlanes)
    , _volumeTexture(volumeTexture)
    , _transferFunction(transferFunction)
    , _boundingBox(glm::vec3(1.0))
{}

BasicVolumeRaycaster::~BasicVolumeRaycaster() {}

void BasicVolumeRaycaster::initialize() {
    _boundingBox.initialize();
}

void BasicVolumeRaycaster::deinitialize() {}

void BasicVolumeRaycaster::renderEntryPoints(const RenderData& data,
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

void BasicVolumeRaycaster::renderExitPoints(const RenderData& data,
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

void BasicVolumeRaycaster::preRaycast(const RaycastData& data,
                                      ghoul::opengl::ProgramObject& program)
{
    if (!_volumeTexture || !_transferFunction) {
        return;
    }

    std::string stepSizeUniformName = "maxStepSize" + std::to_string(data.id);
    program.setUniform(stepSizeUniformName, _stepSize);

    std::string id = std::to_string(data.id);

    _transferFunction->update();
    _tfUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _tfUnit->activate();
    _transferFunction->texture().bind();
    program.setUniform("transferFunction_" + id, _tfUnit->unitNumber());

    _textureUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _textureUnit->activate();
    _volumeTexture->bind();
    program.setUniform("volumeTexture_" + id, _textureUnit->unitNumber());

    program.setUniform("gridType_" + id, static_cast<int>(_gridType));

    std::vector<glm::vec3> clipNormals = _clipPlanes->normals();
    std::vector<glm::vec2> clipOffsets = _clipPlanes->offsets();
    int nClips = static_cast<int>(clipNormals.size());

    program.setUniform("nClips_" + id, nClips);
    program.setUniform("clipNormals_" + id, clipNormals.data(), nClips);
    program.setUniform("clipOffsets_" + id, clipOffsets.data(), nClips);
    program.setUniform("opacity_" + id, _opacity);
    program.setUniform("rNormalization_" + id, _rNormalization);
    program.setUniform("rUpperBound_" + id, _rUpperBound);
}

void BasicVolumeRaycaster::postRaycast(const RaycastData&, ghoul::opengl::ProgramObject&)
{
    _textureUnit = nullptr;
    _tfUnit = nullptr;
}

bool BasicVolumeRaycaster::isCameraInside(const RenderData& data,
                                          glm::vec3& localPosition)
{
    glm::vec4 modelPos = glm::inverse(modelViewTransform(data)) *
                         glm::vec4(0.f, 0.f, 0.f, 1.f);

    localPosition = (glm::vec3(modelPos) + glm::vec3(0.5));

    return (localPosition.x > 0 && localPosition.x < 1 &&
            localPosition.y > 0 && localPosition.y < 1 &&
            localPosition.z > 0 && localPosition.z < 1);
}

std::string BasicVolumeRaycaster::boundsVertexShaderPath() const {
    return absPath(GlslBoundsVsPath);
}

std::string BasicVolumeRaycaster::boundsFragmentShaderPath() const {
    return absPath(GlslBoundsFsPath);
}

std::string BasicVolumeRaycaster::raycasterPath() const {
    return absPath(GlslRaycastPath);
}

std::string BasicVolumeRaycaster::helperPath() const {
    return absPath(GlslHelperPath);
}


void BasicVolumeRaycaster::setTransferFunction(
                            std::shared_ptr<openspace::TransferFunction> transferFunction)
{
    _transferFunction = std::move(transferFunction);
}

void BasicVolumeRaycaster::setVolumeTexture(
                                    std::shared_ptr<ghoul::opengl::Texture> volumeTexture)
{
    _volumeTexture = std::move(volumeTexture);
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

void BasicVolumeRaycaster::setRUpperBound(float rUpperBound) {
    _rUpperBound = rUpperBound;
}

float BasicVolumeRaycaster::rUpperBound() const {
    return _rUpperBound;
}

VolumeGridType BasicVolumeRaycaster::gridType() const {
    return _gridType;
}

void BasicVolumeRaycaster::setGridType(VolumeGridType gridType) {
    _gridType = gridType;
}

void BasicVolumeRaycaster::setModelTransform(glm::mat4 transform) {
    _modelTransform = std::move(transform);
}

} // namespace openspace::volume
