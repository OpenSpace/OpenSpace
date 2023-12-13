/*********************************************************************************
 *
 * Inviwo - Interactive Visualization Workshop
 *
 * Copyright (c) 2023 Inviwo Foundation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *********************************************************************************/

#include <inviwo/tetramesh/processors/tetrameshvolumeraycaster.h>
#include <inviwo/tetramesh/datastructures/tetramesh.h>
#include <inviwo/tetramesh/util/tetrameshutils.h>

#include <inviwo/core/datastructures/geometry/mesh.h>
#include <inviwo/core/datastructures/buffer/bufferram.h>
#include <inviwo/core/util/exception.h>
#include <modules/opengl/image/layergl.h>
#include <modules/opengl/texture/textureutils.h>
#include <modules/opengl/texture/textureunit.h>
#include <modules/opengl/rendering/meshdrawergl.h>
#include <modules/opengl/shader/shaderutils.h>
#include <modules/opengl/openglutils.h>
#include <modules/opengl/openglcapabilities.h>
#include <modules/opengl/sharedopenglresources.h>


#include <modules/tetramesh/include/processors/tetrameshvolumeraycaster.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    // TODO rewrite these so that the VS shader fit with the new glsl frag shader
    // bounding frag must also accomodate changes in bounds_vs
    constexpr std::string_view GlslBoundsVs = "${MODULE_TETRAMESH}/glsl/bounds_vs.glsl";
    constexpr std::string_view GlslBoundsFs = "${MODULE_TETRAMESH}/glsl/bounds_fs.glsl";
}
namespace openspace {
TetraMeshVolumeRaycaster::TetraMeshVolumeRaycaster(
    std::shared_ptr<openspace::TransferFunction> transferFunction,
    const std::string& fragmentShaderRaycastPath)
    : _transferFunction(transferFunction)
    , _glslRaycast{ fragmentShaderRaycastPath }
{
}

TetraMeshVolumeRaycaster::~TetraMeshVolumeRaycaster()
{
}

void TetraMeshVolumeRaycaster::renderEntryPoints(const RenderData& data,
    ghoul::opengl::ProgramObject& program)
{
    // TODO: figure out modelviewTransform
    // TODO: check if we need dmat precision here or if should cast to mat4
    program.setUniform("modelViewTransform", glm::mat4(modelViewTransform(data)));
    program.setUniform("projectionTransform", data.camera.projectionMatrix());

    // Cull back face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    // Render bounding geometry
    glBindVertexArray(_buffers.boundaryMeshVAO);
    glDrawElements(
        GL_TRIANGLES,
        static_cast<GLsizei>(_numIndices),
        GL_UNSIGNED_INT,
        nullptr
    );
    glBindVertexArray(0);
}

void TetraMeshVolumeRaycaster::renderExitPoints(const RenderData& data,
    ghoul::opengl::ProgramObject& program)
{
    // TODO: figure out modelviewTransform matrix
    // renderablegaiavolume and timevarying for that creates a modeltransform that is
    // based on the metadata domain -- Don't think this should be necessary as
    // we already scale the data to [0,1] in the tetramesh.get function.
    program.setUniform("modelViewTransform", glm::mat4(modelViewTransform(data)));
    // TODO change to renderable  calcModelViewProjection transform?
    program.setUniform("projectionTransform", data.camera.projectionMatrix());

    // Cull front face
    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);

    // Render bounding geometry
    glBindVertexArray(_buffers.boundaryMeshVAO);
    glDrawElements(
        GL_TRIANGLES,
        static_cast<GLsizei>(_numIndices),
        GL_UNSIGNED_INT,
        nullptr
    );
    glBindVertexArray(0);

    // Restore defaults
    glCullFace(GL_BACK);
}

void TetraMeshVolumeRaycaster::preRaycast(const RaycastData& data,
    ghoul::opengl::ProgramObject& program)
{
    if (program.isDirty()) {
        program.rebuildFromFile();
    }
    const std::string id = std::to_string(data.id); //std::string_view?

    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, _buffers.nodesBuffer);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, _buffers.nodeIdsBuffer);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, _buffers.opposingFaceIdsBuffer);

    //program.setUniform(
    //    program.uniformLocation("dataToWorld"),
    //    modelTransform
    //);
    //// TODO: make sure we have correct matrice (inviwo does inverse(world * model) -> what is our world?
    //program.setUniform(
    //    program.uniformLocation("worldToData"),
    //    glm::inverse(modelTransform)
    //);
    ////_program->setUniform(
    ////    _program->uniformLocation("dataToWorldNormalMatrix"),
    ////    glm::mat3(glm::transpose(glm::inverse(modelTransform)))
    ////);
    //program.setUniform(
    //    program.uniformLocation("worldToClip"), // Check
    //    cameraVP
    //);
    //// TODO make sure this is correct way to inverse viewprojection matrix
    ////_program->setUniform(
    ////    _program->uniformLocation("clipToWorld"),
    ////    glm::inverse(cameraVP)
    ////);
    //program.setUniform(
    //    program.uniformLocation("position"),
    //    cameraPosition
    //);

    //const glm::vec2 dataRange = glm::vec2{ _metadata.minValue, _metadata.maxValue };
    const float scalingFactor = 1.f / (_dataRange.y - _dataRange.x);
    const float offset = -_dataRange.x;
    program.setUniform(program.uniformLocation("tfValueScaling_" + id), scalingFactor);
    program.setUniform(program.uniformLocation("tfValueOffset_" + id), offset);

    _transferFunction->update();
    _tfUnit = std::make_unique<ghoul::opengl::TextureUnit>();
    _tfUnit->activate();
    _transferFunction->bind();
    program.setUniform("transferFunction_" + id, _tfUnit->unitNumber());
}

void TetraMeshVolumeRaycaster::postRaycast(const RaycastData&,
    ghoul::opengl::ProgramObject& program)
{
    _tfUnit = nullptr;
}

bool TetraMeshVolumeRaycaster::isCameraInside(const RenderData& data,
    glm::vec3& localPosition)
{
    return false;
}

std::string TetraMeshVolumeRaycaster::boundsVertexShaderPath() const
{
    return absPath(GlslBoundsVs).string(); // TODO check if this shader will work
}

std::string TetraMeshVolumeRaycaster::boundsFragmentShaderPath() const
{
    return absPath(GlslBoundsFs).string(); // TODO check if this shader will work
}

std::string TetraMeshVolumeRaycaster::raycasterPath() const
{
    return absPath(_glslRaycast).string();
}

std::string TetraMeshVolumeRaycaster::helperPath() const
{
    return std::string();
}

void TetraMeshVolumeRaycaster::setBuffers(utiltetra::TetraBufferIds buffers)
{
    _buffers = buffers;
}

void TetraMeshVolumeRaycaster::setBoundaryDrawCalls(unsigned amount)
{
    _numIndices = amount;
}

void TetraMeshVolumeRaycaster::setDataRange(float min, float max)
{
    _dataRange = glm::vec2(min, max);
}

std::string TetraMeshVolumeRaycaster::foo()
{
    return R"(in int in_tetraFaceId;

    out Fragment_tetra {
        smooth vec4 worldPosition;
        smooth vec3 position; //seems to be equivalent to Fragment.color in bounds_fs.glsl
        flat vec4 color;
        flat int tetraFaceId;

        flat vec3 camPosData;
    } out_vert;)";
}
std::string TetraMeshVolumeRaycaster::foo2() {
    return R"(  out_vert.tetraFaceId = in_tetraFaceId;)";
}

//void openspace::TetraMeshVolumeRaycaster::setModelTransform(glm::dmat4 transform)
//{
//    _modelTransform = std::move(transform);
//}

glm::dmat4 TetraMeshVolumeRaycaster::modelViewTransform(const RenderData& data)
{
    glm::dvec3 translation = glm::dvec3(-0.5) + data.modelTransform.translation;
    glm::dmat3 rotation = data.modelTransform.rotation;
    glm::dvec3 scale = data.modelTransform.scale;

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), translation) *
        glm::dmat4(rotation) *
        glm::scale(glm::dmat4(1.0), scale);

    return data.camera.combinedViewMatrix() * modelTransform;
}

} // namespace OpenSpace
