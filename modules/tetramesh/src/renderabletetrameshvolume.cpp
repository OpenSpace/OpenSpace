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

#include <glm/glm.hpp>
#include <vector>

namespace {
    constexpr std::string_view _loggerCat = "RenderableTetraMeshVolume";

    constexpr openspace::properties::Property::PropertyInfo SourceDirectoryInfo = {
        "SourceDirectory",
        "Source Directory",
        "Specifies the path to load the volume from",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
    "StepSize",
    "Step Size",
    "Specifies how often to sample on the raycaster. Lower step -> higher resolution",
    // @VISIBILITY(3.5)
    openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
    "TransferFunctionPath",
    "Transfer Function Path",
    "Specifies the transfer function file path",
    openspace::properties::Property::Visibility::AdvancedUser
    };


    struct [[codegen::Dictionary(RenderableTetraMeshVolume)]] Parameters {
        // [[codegen::verbatim(SourceDirectoryInfo.description)]]
        std::string sourceDirectory;

        // [[codegen::verbatim(TransferFunctionInfo.description)]]
        std::string transferFunction;

        //// [[codegen::verbatim(StepSizeInfo.description)]]
        //std::optional<float> stepSize;
    };

#include "renderabletetrameshvolume_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableTetraMeshVolume::Documentation() {
    return codegen::doc<Parameters>("tetramesh_RenderableTetraMeshVolume");
}


RenderableTetraMeshVolume::RenderableTetraMeshVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _srcDirectory = absPath(p.sourceDirectory);

    //_transferFunctionPath = absPath(p.transferFunction);
    std::string transferFunctionPath = absPath(p.transferFunction).string();
    _transferFunction = std::make_shared<openspace::TransferFunction>(
        transferFunctionPath, [](const openspace::TransferFunction&) {}, 2u
    );
}

void RenderableTetraMeshVolume::initialize() {
    namespace fs = std::filesystem;

    std::string baseName;
    for (const fs::directory_entry& e : fs::recursive_directory_iterator(_srcDirectory)) {
        if (e.is_regular_file() && e.path().extension() == ".dictionary") {
            const std::string path = e.path().string();
            loadVolumeMetadata(path);
            baseName = std::filesystem::path(path).stem().string();
            break;
        }
    }
    // TODO: check that we have metafile -> report error

    std::string volumePath = fmt::format("{}/{}.rawvolume", _srcDirectory, baseName);
    volume::RawVolumeReader<gaiavolume::GaiaVolumeDataLayout> reader(
        volumePath, _metadata.dimensions
    );

    _tetraMesh.setData(reader.read(false, _metadata.fileheaders.size()));
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


    // TODO: move this to raycaster class

    // TODO remove
    //std::unique_ptr<ghoul::opengl::ProgramObject> program =
    //    ghoul::opengl::ProgramObject::Build(
    //        "TetraMeshVolume",
    //        absPath("${MODULE_TETRAMESH}/glsl/tetramesh_traversal.vert"),
    //        absPath("${MODULE_TETRAMESH}/glsl/tetramesh_traversal.frag")
    //    );

    //_program = std::move(program);


    // Generate and bind buffers
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
    _raycaster->setDataRange(_metadata.minValue, _metadata.maxValue);
    //_raycaster->setBoundaryMeshVAO(_boundaryMeshVAO);
    //_nodesBuffer = std::make_unique<ghoul::opengl::BufferBinding<
    //    ghoul::opengl::bufferbinding::Buffer::ShaderStorage>>();
 
    //_program->setSsboBinding("nodeBuffer", _nodesBuffer);

    //_nodeIdsBuffer = std::make_unique<ghoul::opengl::BufferBinding<
    //    ghoul::opengl::bufferbinding::Buffer::ShaderStorage>>();

    //_program->setSsboBinding("NodeIdsBuffer", _nodeIdsBuffer);

    //_opposingFaceIdsBuffer = std::make_unique<ghoul::opengl::BufferBinding<
    //    ghoul::opengl::bufferbinding::Buffer::ShaderStorage>>();

    //_program->setSsboBinding("opposingFaceIdsBuffer", _opposingFaceIdsBuffer);
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

    global::renderEngine->removeRenderProgram(_program.get());
    _program = nullptr;
}
bool RenderableTetraMeshVolume::isReady() const
{
    // TODO: make sure data is ready over just program - for now ok since we fill
    // volume on init
    //return _program != nullptr;
    return _tetraMesh.getNumberOfCells() != 0;

}
void RenderableTetraMeshVolume::render(const RenderData& data, RendererTasks& tasks)
{
    if (_raycaster) {
        tasks.raycasterTasks.push_back({ _raycaster.get(), data });
        //return;
    }
    return;

    _program->activate();
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

    _program->setUniform(
        _program->uniformLocation("dataToWorld"),
        modelTransform
    );
    // TODO: make sure we have correct matrice (inviwo does inverse(world * model) -> what is our world?
    _program->setUniform(
        _program->uniformLocation("worldToData"),
        glm::inverse(modelTransform)
    );
    //_program->setUniform(
    //    _program->uniformLocation("dataToWorldNormalMatrix"),
    //    glm::mat3(glm::transpose(glm::inverse(modelTransform)))
    //);
    _program->setUniform(
        _program->uniformLocation("worldToClip"), // Check
        cameraVP
    );
    // TODO make sure this is correct way to inverse viewprojection matrix
    //_program->setUniform(
    //    _program->uniformLocation("clipToWorld"),
    //    glm::inverse(cameraVP)
    //);
    _program->setUniform(
        _program->uniformLocation("position"),
        cameraPosition
    );

    const glm::vec2 dataRange = glm::vec2{ _metadata.minValue, _metadata.maxValue };
    const float scalingFactor = 1.f / (dataRange.y - dataRange.x);
    const float offset = -dataRange.x;
    _program->setUniform(_program->uniformLocation("tfValueScaling"), scalingFactor);
    _program->setUniform(_program->uniformLocation("tfValueOffset"), offset);

    _transferFunction->update();
    auto tfunit = std::make_unique<ghoul::opengl::TextureUnit>();
    tfunit->activate();
    _transferFunction->bind();
    _program->setUniform("transferFunction", tfunit->unitNumber());
    //// set texture uniform

    glCullFace(GL_BACK);
    glEnable(GL_CULL_FACE);
    glDisable(GL_BLEND);
    

    glBindVertexArray(_buffers.boundaryMeshVAO);

    const unsigned numIndices = static_cast<unsigned>(_boundaryMesh.get()->indices.size());
    glDrawElements(GL_TRIANGLES, static_cast<GLsizei>(numIndices), GL_UNSIGNED_INT, nullptr);

    glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
    _program->deactivate();
    
}
void RenderableTetraMeshVolume::update(const UpdateData& data)
{
    // TODO: only do if the data has changed
    if (!_boundaryMesh) {
        std::vector<glm::vec4> nodes;
        std::vector<glm::ivec4> nodeIds;

        _tetraMesh.get(nodes, nodeIds);
        std::vector<glm::ivec4> opposingFaces = utiltetra::getOpposingFaces(nodeIds);

        // Upload data
        // TODO: keep track of size and use glSubData if we can?
        // e.g., glBufferSubData(GL_SHADER_STORAGE_BUFFER, offset, sizeof(newData), newData);
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _buffers.nodesBuffer);
        GLsizeiptr sizeInBytes = static_cast<GLsizeiptr>(sizeof(GL_FLOAT) * 4 * nodes.size());
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            sizeInBytes,
            nodes.data(),
            GL_DYNAMIC_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _buffers.nodeIdsBuffer);
        sizeInBytes = static_cast<GLsizeiptr>(sizeof(GL_INT) * 4 * nodeIds.size());
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            sizeInBytes,
            nodeIds.data(),
            GL_DYNAMIC_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _buffers.opposingFaceIdsBuffer);
        sizeInBytes = static_cast<GLsizeiptr>(sizeof(GL_INT) * 4 * opposingFaces.size());
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            sizeInBytes,
            opposingFaces.data(),
            GL_DYNAMIC_DRAW
        );

        // Create boundary mesh
        _boundaryMesh = utiltetra::createBoundaryMesh(
            nodes, nodeIds, utiltetra::getBoundaryFaces(opposingFaces)
        );

        _raycaster->setBoundaryDrawCalls(_boundaryMesh->indices.size());
        _raycaster->setDataRange(_metadata.minValue, _metadata.maxValue);


        // Setup boundary mesh
        glBindVertexArray(_buffers.boundaryMeshVAO);
        auto verticesSize = _boundaryMesh.get()->vertices.size() * sizeof(glm::vec3);
        auto faceIdsSize = _boundaryMesh.get()->faceIds.size() * sizeof(int);
        auto indicesSize = _boundaryMesh.get()->indices.size() * sizeof(uint32_t);

        glBindBuffer(GL_ARRAY_BUFFER, _buffers.vertsVBO);
        glBufferData(
            GL_ARRAY_BUFFER,
            verticesSize,
            _boundaryMesh.get()->vertices.data(),
            GL_DYNAMIC_DRAW
        );

        // TODO: get index (first parameter) by
        //GLint attributeIndex = _program->attributeLocation("in_position");
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GL_FLOAT), (void*)0);
        glEnableVertexAttribArray(0);

        glBindBuffer(GL_ARRAY_BUFFER, _buffers.faceIdVBO);
        glBufferData(
            GL_ARRAY_BUFFER,
            faceIdsSize,
            _boundaryMesh.get()->faceIds.data(),
            GL_DYNAMIC_DRAW
        );

        glVertexAttribIPointer(1, 1, GL_INT, sizeof(GL_INT), (void*)0);
        glEnableVertexAttribArray(1);

        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _buffers.indicesEBO);
        glBufferData(GL_ELEMENT_ARRAY_BUFFER,
            indicesSize,
            _boundaryMesh.get()->indices.data(),
            GL_DYNAMIC_DRAW
        );

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);
    }
}

void RenderableTetraMeshVolume::loadVolumeMetadata(const std::string& path) {
    try {
        ghoul::Dictionary dictionary = ghoul::lua::loadDictionaryFromFile(path);
        _metadata = volume::RawVolumeMetadata::createFromDictionary(dictionary);
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
        return;
    }
}

} // namespace OpenSpace
