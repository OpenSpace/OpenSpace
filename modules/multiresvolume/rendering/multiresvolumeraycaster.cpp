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

#include <modules/multiresvolume/rendering/multiresvolumeraycaster.h>

#include <modules/multiresvolume/rendering/atlasmanager.h>
#include <modules/multiresvolume/rendering/tsp.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr std::string_view GlslRaycastPath =
        "${MODULES}/multiresvolume/shaders/raycast.glsl";
    constexpr std::string_view GlslHelperPath =
        "${MODULES}/multiresvolume/shaders/helper.glsl";
    constexpr std::string_view GlslBoundsVsPath =
        "${MODULES}/multiresvolume/shaders/bounds_vs.glsl";
    constexpr std::string_view GlslBoundsFsPath =
        "${MODULES}/multiresvolume/shaders/bounds_fs.glsl";
} // namespace

namespace openspace {

MultiresVolumeRaycaster::MultiresVolumeRaycaster(std::shared_ptr<TSP> tsp,
                                               std::shared_ptr<AtlasManager> atlasManager,
                                       std::shared_ptr<TransferFunction> transferFunction)
    : _boundingBox(glm::vec3(1.f))
    , _tsp(tsp)
    , _atlasManager(atlasManager)
    , _transferFunction(transferFunction)
{}

MultiresVolumeRaycaster::~MultiresVolumeRaycaster() {}

void MultiresVolumeRaycaster::initialize() {
    _boundingBox.initialize();
}

void MultiresVolumeRaycaster::deinitialize() {}

void MultiresVolumeRaycaster::renderEntryPoints(const RenderData& data,
                                                ghoul::opengl::ProgramObject& program)
{
    program.setUniform("modelTransform", _modelTransform);
    program.setUniform("viewProjection", data.camera.viewProjectionMatrix());
    program.setUniform("campos", glm::vec4(data.camera.positionVec3(), 1.f));
    program.setUniform("objpos", glm::vec4(data.modelTransform.translation, 0.f));
    program.setUniform("camrot", glm::mat4(data.camera.viewRotationMatrix()));
    program.setUniform("scaling", glm::vec2(1.f, 0.f));

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    _boundingBox.render();
}

void MultiresVolumeRaycaster::renderExitPoints(const RenderData& data,
                                               ghoul::opengl::ProgramObject& program)
{
    program.setUniform("modelTransform", _modelTransform);
    program.setUniform("viewProjection", data.camera.viewProjectionMatrix());
    program.setUniform("campos", glm::vec4(data.camera.positionVec3(), 1.f));
    program.setUniform("objpos", glm::vec4(data.modelTransform.translation, 0.f));
    program.setUniform("camrot", glm::mat4(data.camera.viewRotationMatrix()));
    program.setUniform("scaling", glm::vec2(1.f, 0.f));

    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);

    _boundingBox.render();

    glCullFace(GL_BACK);
}

void MultiresVolumeRaycaster::preRaycast(const RaycastData& data,
                                         ghoul::opengl::ProgramObject& program)
{
    std::string id = std::to_string(data.id);
    //program.setUniform("opacity_" + std::to_string(id), visible ? 1.f : 0.f);
    program.setUniform("stepSizeCoefficient_" + id, _stepSizeCoefficient);

    _tfUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _tfUnit->activate();
    _transferFunction->texture().bind();
    program.setUniform("transferFunction_" + id, _tfUnit->unitNumber());

    _atlasUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _atlasUnit->activate();
    _atlasManager->textureAtlas().bind();
    program.setUniform("textureAtlas_" + id, _atlasUnit->unitNumber());

    _atlasMapBinding = std::make_unique<ghoul::opengl::BufferBinding<
        ghoul::opengl::bufferbinding::Buffer::ShaderStorage
    >>();
    glBindBufferBase(
        GL_SHADER_STORAGE_BUFFER,
        _atlasMapBinding->bindingNumber(),
        _atlasManager->atlasMapBuffer()
    );
    program.setSsboBinding("atlasMapBlock_" + id, _atlasMapBinding->bindingNumber());

    program.setUniform("gridType_" + id, static_cast<int>(_tsp->header().gridType));
    program.setUniform(
        "maxNumBricksPerAxis_" + id,
        static_cast<unsigned int>(_tsp->header().xNumBricks)
    );
    program.setUniform("paddedBrickDim_" + id, _tsp->paddedBrickDim());

    glm::size3_t size = _atlasManager->textureSize();
    glm::ivec3 atlasSize(size.x, size.y, size.z);
    program.setUniform("atlasSize_" + id, atlasSize);
}

bool MultiresVolumeRaycaster::isCameraInside(const RenderData& data,
                                             glm::vec3& localPosition)
{
    // Camera rig position in world coordinates.
    glm::vec4 rigWorldPos = glm::vec4(data.camera.positionVec3(), 1.0);
    //rigWorldPos /= data.camera.scaling().x * pow(10.0, data.camera.scaling().y);
    //glm::mat4 invSgctMatrix = glm::inverse(data.camera.viewMatrix());

    // Camera position in world coordinates.
    glm::vec4 camWorldPos = rigWorldPos;
    glm::vec3 objPos = static_cast<glm::vec3>(data.modelTransform.translation);

    glm::mat4 modelTransform = glm::translate(_modelTransform, objPos);

    float divisor = 1.0;
    for (int i = 0; i < 4; i++) for (int j = 0; j < 4; j++) {
        if (abs(modelTransform[i][j]) > divisor) {
            divisor = modelTransform[i][j];
        }
    }

    glm::mat4 scaledModelTransform = modelTransform / divisor;

    glm::vec4 modelPos = (glm::inverse(scaledModelTransform) / divisor) * camWorldPos;


    localPosition = (glm::vec3(modelPos) + glm::vec3(0.5f));
    return (localPosition.x > 0 && localPosition.y > 0 && localPosition.z > 0 &&
            localPosition.x < 1 && localPosition.y < 1 && localPosition.z < 1);
}

void MultiresVolumeRaycaster::postRaycast(const RaycastData&,
                                          ghoul::opengl::ProgramObject&)
{
    _atlasUnit = nullptr;
    _tfUnit = nullptr;
}

std::filesystem::path MultiresVolumeRaycaster::boundsVertexShaderPath() const {
    return absPath(GlslBoundsVsPath);
}

std::filesystem::path MultiresVolumeRaycaster::boundsFragmentShaderPath() const {
    return absPath(GlslBoundsFsPath);
}

std::filesystem::path MultiresVolumeRaycaster::raycasterPath() const {
    return absPath(GlslRaycastPath);
}

std::filesystem::path MultiresVolumeRaycaster::helperPath() const {
    return absPath(GlslHelperPath); // no helper file
}

void MultiresVolumeRaycaster::setModelTransform(glm::mat4 transform) {
    _modelTransform = transform;
}

void MultiresVolumeRaycaster::setStepSizeCoefficient(float stepSizeCoefficient) {
    _stepSizeCoefficient = stepSizeCoefficient;
}

} // namespace openspace
