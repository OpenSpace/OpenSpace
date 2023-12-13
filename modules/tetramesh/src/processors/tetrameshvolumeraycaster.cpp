/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
