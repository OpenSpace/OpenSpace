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

#ifndef __OPENSPACE_MODULE_TETRAMESH___RENDERABLETETRAMESH___H__
#define __OPENSPACE_MODULE_TETRAMESH___RENDERABLETETRAMESH___H__

#include <openspace/rendering/renderable.h>

#include <modules/tetramesh/include/datastructures/volumetetramesh.h>
#include <modules/tetramesh/include/processors/tetrameshvolumeraycaster.h>
#include <modules/tetramesh/include/util/tetrameshutils.h>
#include <modules/volume/rawvolumemetadata.h>
#include <openspace/rendering/transferfunction.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/bufferbinding.h>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace{

namespace documentation { struct Documentation; }

class RenderableTetraMeshVolume : public Renderable {
public:
    explicit RenderableTetraMeshVolume(const ghoul::Dictionary& dictionary);
    ~RenderableTetraMeshVolume() override = default;

    void initialize() override;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    void loadVolumeMetadata(const std::string& path);

    std::unique_ptr<TetraMeshVolumeRaycaster> _raycaster;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    std::shared_ptr<openspace::TransferFunction> _transferFunction;

    VolumeTetraMesh _tetraMesh;
    //volume::RawVolumeMetadata _metadata;
    std::shared_ptr<utiltetra::Mesh> _boundaryMesh;

    // TODO: use raw OpenGL commands for buffer storages since these buffers objs are not 100% tested
    //std::unique_ptr<ghoul::opengl::BufferBinding<
    //    ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _nodesBuffer;
    //std::unique_ptr<ghoul::opengl::BufferBinding<
    //    ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _nodeIdsBuffer;
    //std::unique_ptr<ghoul::opengl::BufferBinding<
    //    ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _opposingFaceIdsBuffer;

    // TODO: depending on above solution, these might be redundant
    utiltetra::TetraBufferIds _buffers;

    //GLuint _nodesBuffer = 0;
    //GLuint _nodeIdsBuffer = 0;
    //GLuint _opposingFaceIdsBuffer = 0;
    //GLuint _boundaryMeshVAO = 0;

    //GLuint _indicesEBO = 0;
    //GLuint _vertsVBO = 0;
    //GLuint _faceIdVBO = 0;

    std::string _filePath;
    std::shared_ptr<const volume::RawVolume<utiltetra::VoxelData>> _volume;
    glm::vec2 _dataRange = glm::vec2(0.f);

    //std::filesystem::path _srcDirectory;
    //std::filesystem::path _transferFunctionPath;
    
    properties::FloatProperty _samplingInterval;
    properties::FloatProperty _opacityScaling;
};

} // namespace OpenSpace

#endif // !__OPENSPACE_MODULE_TETRAMESH___RENDERABLETETRAMESH___H__
