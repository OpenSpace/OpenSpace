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

#include <modules/tetramesh/include/renderabletetrameshvolume.h>
#include <modules/gaia/tasks/generateGaiaVolumeTask.h>
#include <modules/volume/rawvolumereader.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/csvreader.h>

#include <glm/glm.hpp>
#include <vector>

namespace {
    constexpr std::string_view _loggerCat = "RenderableTetraMeshVolume";

    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "Specifies the path to load the volume from",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "TransferFunctionPath",
        "Transfer Function Path",
        "Specifies the transfer function file path",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SamplingIntervalInfo = {
        "SamplingInterval",
        "Sampling Interval",
        "Specifies the sampling interval",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityScalingInfo{
        "OpacityScaling",
        "Opacity Scaling",
        "Specifies the opacity scaling facotr used to scale the extinction to account for"
        " differently sized datasets",
        openspace::properties::Property::Visibility::AdvancedUser
    };


    struct [[codegen::Dictionary(RenderableTetraMeshVolume)]] Parameters {
        // [[codegen::verbatim(FilePathInfo.description)]]
        std::string filePath;

        // [[codegen::verbatim(TransferFunctionInfo.description)]]
        std::string transferFunction;

        // Specifies the number of grid cells in each dimension
        glm::ivec3 dimensions [[codegen::greater(glm::ivec3(0))]];
    };

#include "renderabletetrameshvolume_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableTetraMeshVolume::Documentation() {
    return codegen::doc<Parameters>("tetramesh_RenderableTetraMeshVolume");
}


RenderableTetraMeshVolume::RenderableTetraMeshVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _samplingInterval(SamplingIntervalInfo, 150, 0, 1000)
    , _opacityScaling(OpacityScalingInfo, 1, 0, 10)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _filePath = p.filePath;
    _dimensions = p.dimensions;

    std::string transferFunctionPath = absPath(p.transferFunction).string();
    _transferFunction = std::make_shared<openspace::TransferFunction>(
        transferFunctionPath, [](const openspace::TransferFunction&) {}, 2u
    );

    _samplingInterval.onChange([this]() {
        float value = _samplingInterval.value();
        _raycaster->setSamplingInterval(value);
    });

    _opacityScaling.onChange([this]() {
        float value = _opacityScaling.value();
        _raycaster->setOpacityScaling(value);
    });

    addProperty(_samplingInterval);
    addProperty(_opacityScaling);
}

void RenderableTetraMeshVolume::initialize() {
    namespace fs = std::filesystem;

    std::string baseName;

    std::vector<std::vector<std::string>> csvData = ghoul::loadCSVFile(_filePath, false);

    if (csvData.empty()) {
        throw ghoul::RuntimeError(fmt::format("{}: CSV file {} must not be empty",
            _loggerCat, _filePath
        ));
    }

    std::vector<utiltetra::VoxelData> voxelData(csvData.size());
    volume::RawVolume<utiltetra::VoxelData> volume(static_cast<glm::uvec3>(_dimensions));

    if (volume.nCells() != csvData.size()) {
        throw ghoul::RuntimeError(fmt::format(
            "{}: Specified volume dimensions ({},{},{}) does not match data points read",
            _loggerCat, _dimensions.x, _dimensions.y, _dimensions.z
        ));
    }

    float minValue = std::numeric_limits<float>::max();
    float maxValue = std::numeric_limits<float>::min();

    int index = 0;
    for (const std::vector<std::string> &row : csvData) {
        // TODO: handle NaN values
        // Data format of ecah voxel:  x, y, z, value
        float x = std::stof(row[0]);
        float y = std::stof(row[1]);
        float z = std::stof(row[2]);
        float v = std::stof(row[3]);
        utiltetra::VoxelData voxel(x, y, z, v);
        voxelData[index] = voxel;

        minValue = std::min(v, minValue);
        maxValue = std::max(v, maxValue);

        ++index;
    }
    _dataRange = glm::vec2(minValue, maxValue);

    for (size_t i = 0; i < volume.nCells(); i++) {
        volume.set(i, voxelData[i]);
    }
    _volume = std::make_shared<const volume::RawVolume<utiltetra::VoxelData>>(volume);

    _tetraMesh.setData(_volume);    
}

void RenderableTetraMeshVolume::initializeGL()
{
    _raycaster = std::make_unique<TetraMeshVolumeRaycaster>(
        _transferFunction,
        "${MODULE_TETRAMESH}/glsl/tetramesh_raycast.glsl"
    );

    global::raycasterManager->attachRaycaster(*_raycaster.get());
    onEnabledChange([this](bool enabled) {
        if (enabled) {
            global::raycasterManager->attachRaycaster(*_raycaster.get());
        }
        else
        {
            global::raycasterManager->detachRaycaster(*_raycaster.get());
        }
    });

    // TODO check if we need to bindbufferbase here or not. BufferBase may be overwritten
    // During other render calls (?)
    // Generate buffers
    glGenBuffers(1, &_buffers.nodesBuffer);
    //glBindBuffer(GL_SHADER_STORAGE_BUFFER, _nodesBuffer);
    //glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, _nodesBuffer);
    // Node ids
    glGenBuffers(1, &_buffers.nodeIdsBuffer);
    //glBindBuffer(GL_SHADER_STORAGE_BUFFER, _nodeIdsBuffer);
    //glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, _nodeIdsBuffer);
    // Opposing faces
    glGenBuffers(1, &_buffers.opposingFaceIdsBuffer);
    //glBindBuffer(GL_SHADER_STORAGE_BUFFER, _opposingFaceIdsBuffer);
    //glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, _opposingFaceIdsBuffer);

    // Generate buffers for the boundary mesh
    glGenVertexArrays(1, &_buffers.boundaryMeshVAO);
    glGenBuffers(1, &_buffers.indicesEBO);
    glGenBuffers(1, &_buffers.vertsVBO);
    glGenBuffers(1, &_buffers.faceIdVBO);

    _raycaster->setBuffers(_buffers);
    _raycaster->setDataRange(_dataRange.x, _dataRange.y);
}

void RenderableTetraMeshVolume::deinitializeGL()
{
    glDeleteBuffers(1, &_buffers.nodesBuffer);
    glDeleteBuffers(1, &_buffers.nodeIdsBuffer);
    glDeleteBuffers(1, &_buffers.opposingFaceIdsBuffer);
    glDeleteBuffers(1, &_buffers.indicesEBO);
    glDeleteBuffers(1, &_buffers.vertsVBO);
    glDeleteBuffers(1, &_buffers.faceIdVBO);
    glDeleteVertexArrays(1, &_buffers.boundaryMeshVAO);

    _buffers.deinitialize();
}
bool RenderableTetraMeshVolume::isReady() const
{
    // TODO: make sure data is ready over just program - for now ok since we fill
    // volume on init
    return _tetraMesh.getNumberOfCells() != 0;

}
void RenderableTetraMeshVolume::render(const RenderData& data, RendererTasks& tasks)
{
    if (_raycaster) {
        tasks.raycasterTasks.push_back({ _raycaster.get(), data });
    }
    return;

    //_program->activate();
    // Check if the buffer needs to be bound first, inviwo does not seem to bind it before 
    // calling bind buffer base
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, _buffers.nodesBuffer);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, _buffers.nodeIdsBuffer);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, _buffers.opposingFaceIdsBuffer);

    // TODO set uniforms
    
    //const glm::mat4 modelTransform = _tetraMesh.tetraBoundingBox();
    //const glm::mat4 cameraVP = data.camera.sgctInternal.viewProjectionMatrix();
    const glm::vec3 cameraPosition = data.camera.positionVec3();

    glm::dvec3 translation =  glm::dvec3(-0.5) + data.modelTransform.translation;
    glm::dmat4 modelTransform = calcModelTransform(data, {.translation = translation});


    const glm::dmat4 cameraVP = calcModelViewProjectionTransform(data, modelTransform);

    //_program->setUniform(
    //    _program->uniformLocation("dataToWorld"),
    //    modelTransform
    //);
    // TODO: make sure we have correct matrice (inviwo does inverse(world * model) -> what is our world?
    //_program->setUniform(
    //    _program->uniformLocation("worldToData"),
    //    glm::inverse(modelTransform)
    //);
    //_program->setUniform(
    //    _program->uniformLocation("dataToWorldNormalMatrix"),
    //    glm::mat3(glm::transpose(glm::inverse(modelTransform)))
    //);
    //_program->setUniform(
    //    _program->uniformLocation("worldToClip"), // Check
    //    cameraVP
    //);
    // TODO make sure this is correct way to inverse viewprojection matrix
    //_program->setUniform(
    //    _program->uniformLocation("clipToWorld"),
    //    glm::inverse(cameraVP)
    //);
    //_program->setUniform(
    //    _program->uniformLocation("position"),
    //    cameraPosition
    //);
}
void RenderableTetraMeshVolume::update(const UpdateData& data)
{
    // TODO: only do if the data has changed
    if (!_boundaryMesh) {
        std::vector<glm::vec4> nodes;
        std::vector<glm::ivec4> nodeIds;

        _tetraMesh.get(nodes, nodeIds);
        std::vector<glm::ivec4> opposingFaces = utiltetra::getOpposingFaces(nodeIds);

        const GLsizeiptr nodesSizeInBytes = static_cast<GLsizeiptr>(
            sizeof(glm::vec4) * nodes.size()
        );
        const GLsizeiptr nodeIdsSizeInBytes = static_cast<GLsizeiptr>(
            sizeof(glm::ivec4) * nodeIds.size()
        );
        const GLsizeiptr opposingFacesSizeInBytes = static_cast<GLsizeiptr>(
            sizeof(glm::ivec4) * opposingFaces.size()
        );

        // Upload data 
        // TODO: keep track of size and use glSubData if we can? e.g.,
        // glBufferSubData(GL_SHADER_STORAGE_BUFFER, offset, sizeof(newData), newData);
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _buffers.nodesBuffer);
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            nodesSizeInBytes,
            nodes.data(),
            GL_DYNAMIC_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _buffers.nodeIdsBuffer);
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            nodeIdsSizeInBytes,
            nodeIds.data(),
            GL_DYNAMIC_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _buffers.opposingFaceIdsBuffer);
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            opposingFacesSizeInBytes,
            opposingFaces.data(),
            GL_DYNAMIC_DRAW
        );

        // Create boundary mesh
        _boundaryMesh = utiltetra::createBoundaryMesh(
            nodes, nodeIds, utiltetra::getBoundaryFaces(opposingFaces)
        );

        _raycaster->setBoundaryDrawCalls(_boundaryMesh->indices.size());
        _raycaster->setDataRange(_dataRange.x, _dataRange.y);

        const GLsizeiptr verticesSizeInBytes = static_cast<GLsizeiptr>(
            _boundaryMesh->vertices.size() * sizeof(glm::vec3)
        );
        const GLsizeiptr faceIdsSizeInBytes = static_cast<GLsizeiptr>(
            _boundaryMesh->faceIds.size() * sizeof(int)
        );
        const GLsizeiptr indicesSizeInBytes = static_cast<GLsizeiptr>(
            _boundaryMesh->indices.size() * sizeof(uint32_t)
        );

        // Upload boundary mesh data
        glBindVertexArray(_buffers.boundaryMeshVAO);

        glBindBuffer(GL_ARRAY_BUFFER, _buffers.vertsVBO);
        glBufferData(
            GL_ARRAY_BUFFER,
            verticesSizeInBytes,
            _boundaryMesh.get()->vertices.data(),
            GL_DYNAMIC_DRAW
        );

        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GL_FLOAT), (void*)0);
        glEnableVertexAttribArray(0);

        glBindBuffer(GL_ARRAY_BUFFER, _buffers.faceIdVBO);
        glBufferData(
            GL_ARRAY_BUFFER,
            faceIdsSizeInBytes,
            _boundaryMesh.get()->faceIds.data(),
            GL_DYNAMIC_DRAW
        );

        glVertexAttribIPointer(1, 1, GL_INT, sizeof(GL_INT), (void*)0);
        glEnableVertexAttribArray(1);

        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _buffers.indicesEBO);
        glBufferData(GL_ELEMENT_ARRAY_BUFFER,
            indicesSizeInBytes,
            _boundaryMesh.get()->indices.data(),
            GL_DYNAMIC_DRAW
        );

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);
    }
}

} // namespace OpenSpace
