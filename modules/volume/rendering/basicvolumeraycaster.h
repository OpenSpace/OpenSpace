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

#ifndef __OPENSPACE_MODULE_VOLUME___BASICVOLUMERAYCASTER___H__
#define __OPENSPACE_MODULE_VOLUME___BASICVOLUMERAYCASTER___H__

#include <string>
#include <vector>
#include <memory>

#include <ghoul/glm.h>
#include <ghoul/opengl/texture.h>

#include <openspace/rendering/volumeraycaster.h>
#include <openspace/util/boxgeometry.h>
#include <modules/volume/transferfunctionhandler.h>
#include <modules/volume/rendering/volumeclipplanes.h>

#include <modules/volume/volumegridtype.h>

namespace ghoul::opengl {
    class Texture;
    class ProgramObject;
    class TextureUnit;
}

namespace openspace {

struct RenderData;
struct RaycastData;

namespace volume {

class BasicVolumeRaycaster : public VolumeRaycaster {
public:
    BasicVolumeRaycaster(
        std::shared_ptr<ghoul::opengl::Texture> texture,
        std::shared_ptr<TransferFunctionHandler> transferFunctionHandler,
        std::shared_ptr<VolumeClipPlanes> clipPlanes);
    virtual ~BasicVolumeRaycaster();
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
    bool cameraIsInside(const RenderData& data, glm::vec3& localPosition) override;

    std::string getBoundsVsPath() const override;
    std::string getBoundsFsPath() const override;
    std::string getRaycastPath() const override;
    std::string getHelperPath() const override;


    void setVolumeTexture(std::shared_ptr<ghoul::opengl::Texture> texture);
    std::shared_ptr<ghoul::opengl::Texture> volumeTexture() const;
    void setTransferFunctionHandler(std::shared_ptr<TransferFunctionHandler> transferFunctionHandler);

    void setStepSize(float stepSize);
    float opacity() const;
    void setOpacity(float opacity);
    float rNormalization() const;
    void setRNormalization(float rNormalization);
    float rUpperBound() const;
    void setRUpperBound(float rNormalization);
    VolumeGridType gridType() const;
    void setGridType(VolumeGridType gridType);
    void setModelTransform(const glm::mat4& transform);

private:
    glm::dmat4 modelViewTransform(const RenderData& data);

    std::shared_ptr<VolumeClipPlanes> _clipPlanes;
    std::shared_ptr<ghoul::opengl::Texture> _volumeTexture;
    std::shared_ptr<TransferFunctionHandler> _transferFunctionHandler;
    BoxGeometry _boundingBox;
    VolumeGridType _gridType;
    glm::mat4 _modelTransform;
    float _opacity;
    float _rNormalization;
    float _rUpperBound;

    std::unique_ptr<ghoul::opengl::TextureUnit> _tfUnit;
    std::unique_ptr<ghoul::opengl::TextureUnit> _textureUnit;
    float _stepSize;
};

} // namespace volume
} // namespace openspace
#endif // __OPENSPACE_MODULE_VOLUME___BASICVOLUMERAYCASTER___H__
