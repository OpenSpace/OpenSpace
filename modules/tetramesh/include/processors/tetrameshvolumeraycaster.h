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

#ifndef __OPENSPACE_MODULE_TETRAMESH___TETRAMESHVOLUMERAYCASTER___H__
#define __OPENSPACE_MODULE_TETRAMESH___TETRAMESHVOLUMERAYCASTER___H__

#include <openspace/rendering/volumeraycaster.h>
#include <modules/tetramesh/include/util/tetrameshutils.h>

//#include <openspace/util/updatestructures.h>
namespace ghoul::opengl {
    class TextureUnit;
}

namespace openspace {
    //struct RenderData;
    class TransferFunction;
}

namespace openspace {

class TetraMeshVolumeRaycaster : public VolumeRaycaster {
public:
    TetraMeshVolumeRaycaster(
        std::shared_ptr<openspace::TransferFunction> transferFunction,
        const std::string& fragmentShaderRaycastPath);
    ~TetraMeshVolumeRaycaster() override;

    void renderEntryPoints(const RenderData& data,
        ghoul::opengl::ProgramObject& program) override;
    void renderExitPoints(const RenderData& data,
        ghoul::opengl::ProgramObject& program) override;
    void preRaycast(const RaycastData& data,
        ghoul::opengl::ProgramObject& program) override;
    void postRaycast(const RaycastData& data,
        ghoul::opengl::ProgramObject& program) override;
    bool isCameraInside(const RenderData& data, glm::vec3& localPosition) override;

    std::string boundsVertexShaderPath() const override;
    std::string boundsFragmentShaderPath() const override;
    std::string raycasterPath() const override;
    std::string helperPath() const override;

    void setBuffers(utiltetra::TetraBufferIds buffers);
    void setBoundaryDrawCalls(unsigned amount);
    //void setModelTransform(glm::dmat4 transform);
    void setDataRange(float min, float max);

    std::string foo() override;
    std::string foo2() override;
private:
    glm::dmat4 modelViewTransform(const RenderData& data);

    std::string _glslRaycast;
    std::shared_ptr<openspace::TransferFunction> _transferFunction;
    std::unique_ptr<ghoul::opengl::TextureUnit> _tfUnit;
    // Same as renderable buffer ids to make drawcalls
    utiltetra::TetraBufferIds _buffers;
    unsigned _numIndices = 0;

    glm::vec2 _dataRange;
    
    //glm::dmat4 _modelTransform;
};
}

#endif // !__OPENSPACE_MODULE_TETRAMESH___TETRAMESHVOLUMERAYCASTER___H__
