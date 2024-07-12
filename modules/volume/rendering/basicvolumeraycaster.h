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

#ifndef __OPENSPACE_MODULE_VOLUME___BASICVOLUMERAYCASTER___H__
#define __OPENSPACE_MODULE_VOLUME___BASICVOLUMERAYCASTER___H__

#include <openspace/rendering/volumeraycaster.h>

#include <openspace/util/boxgeometry.h>
#include <modules/volume/volumegridtype.h>

namespace ghoul::opengl {
    class Texture;
    class ProgramObject;
    class TextureUnit;
} // namespace ghoul::opengl

namespace openspace {
    struct RenderData;
    struct RaycastData;
    class TransferFunction;
} // namespace openspace

namespace openspace::volume {

class TransferFunctionHandler;
class VolumeClipPlanes;

class BasicVolumeRaycaster : public VolumeRaycaster {
public:
    BasicVolumeRaycaster(
        std::shared_ptr<ghoul::opengl::Texture> texture,
        std::shared_ptr<openspace::TransferFunction> transferFunction,
        std::shared_ptr<VolumeClipPlanes> clipPlanes);
    ~BasicVolumeRaycaster() override;

    void initialize();
    void deinitialize();

    void renderEntryPoints(const RenderData& data,
        ghoul::opengl::ProgramObject& program) override;
    void renderExitPoints(const RenderData& data,
        ghoul::opengl::ProgramObject& program) override;
    void preRaycast(const RaycastData& data,
        ghoul::opengl::ProgramObject& program) override;
    void postRaycast(const RaycastData& data,
        ghoul::opengl::ProgramObject& program) override;
    bool isCameraInside(const RenderData& data, glm::vec3& localPosition) override;

    std::filesystem::path boundsVertexShaderPath() const override;
    std::filesystem::path boundsFragmentShaderPath() const override;
    std::filesystem::path raycasterPath() const override;
    std::filesystem::path helperPath() const override;


    void setVolumeTexture(std::shared_ptr<ghoul::opengl::Texture> texture);
    std::shared_ptr<ghoul::opengl::Texture> volumeTexture() const;
    void setTransferFunction(std::shared_ptr<openspace::TransferFunction>
        transferFunction);

    void setStepSize(float stepSize);
    float brightness() const;
    void setBrightness(float brightness);
    float rNormalization() const;
    void setRNormalization(float rNormalization);
    float rUpperBound() const;
    void setRUpperBound(float rUpperBound);
    VolumeGridType gridType() const;
    void setGridType(VolumeGridType gridType);
    void setModelTransform(glm::mat4 transform);

private:
    glm::dmat4 modelViewTransform(const RenderData& data);

    std::shared_ptr<VolumeClipPlanes> _clipPlanes;
    std::shared_ptr<ghoul::opengl::Texture> _volumeTexture;
    std::shared_ptr<openspace::TransferFunction> _transferFunction;
    BoxGeometry _boundingBox;
    VolumeGridType _gridType;
    glm::mat4 _modelTransform = glm::mat4(1.f);
    float _brightness = 1.f;
    float _rNormalization = 0.f;
    float _rUpperBound = 1.f;

    std::unique_ptr<ghoul::opengl::TextureUnit> _tfUnit;
    std::unique_ptr<ghoul::opengl::TextureUnit> _textureUnit;
    float _stepSize = 0.f;
};

} // namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME___BASICVOLUMERAYCASTER___H__
