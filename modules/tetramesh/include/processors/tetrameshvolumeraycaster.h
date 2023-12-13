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
