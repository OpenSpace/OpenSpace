/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                             *
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

#ifndef __OPENSPACE_MODULE_KAMELEONVOLUME___KAMELEONVOLUMERAYCASTER___H__
#define __OPENSPACE_MODULE_KAMELEONVOLUME___KAMELEONVOLUMERAYCASTER___H__

#include <string>
#include <vector>
#include <memory>

#include <ghoul/glm.h>
#include <ghoul/opengl/texture.h>

#include <openspace/rendering/volumeraycaster.h>
#include <openspace/util/boxgeometry.h>
#include <openspace/rendering/transferfunction.h>
#include <modules/volume/rendering/volumeclipplanes.h>

#include <modules/volume/volumegridtype.h>

namespace ghoul {
    namespace opengl {
        class Texture;
        class ProgramObject;
        class TextureUnit;
    }
}

namespace openspace {

struct RenderData;
struct RaycastData;

class KameleonVolumeRaycaster : public VolumeRaycaster {
public:

    KameleonVolumeRaycaster(
        std::shared_ptr<ghoul::opengl::Texture> texture,
        std::shared_ptr<TransferFunction> transferFunction, 
        std::shared_ptr<VolumeClipPlanes> clipPlanes);

    virtual ~KameleonVolumeRaycaster();
    void initialize();
    void deinitialize();
    void renderEntryPoints(const RenderData& data, ghoul::opengl::ProgramObject& program) override;
    void renderExitPoints(const RenderData& data, ghoul::opengl::ProgramObject& program) override;
    void preRaycast(const RaycastData& data, ghoul::opengl::ProgramObject& program) override;
    void postRaycast(const RaycastData& data, ghoul::opengl::ProgramObject& program) override;
    bool cameraIsInside(const RenderData& data, glm::vec3& localPosition) override;

    std::string getBoundsVsPath() const override;
    std::string getBoundsFsPath() const override;
    std::string getRaycastPath() const override;
    std::string getHelperPath() const override;

    void setStepSize(float stepSize);
    void setGridType(VolumeGridType gridType);
    void setModelTransform(const glm::mat4& transform);
private:
    std::shared_ptr<VolumeClipPlanes> _clipPlanes;
    std::shared_ptr<ghoul::opengl::Texture> _volumeTexture;
    std::shared_ptr<TransferFunction> _transferFunction;
    BoxGeometry _boundingBox;
    VolumeGridType _gridType;
    glm::mat4 _modelTransform;

    std::unique_ptr<ghoul::opengl::TextureUnit> _tfUnit;
    std::unique_ptr<ghoul::opengl::TextureUnit> _textureUnit;
    float _stepSize;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_KAMELEONVOLUME___KAMELEONVOLUMERAYCASTER___H__ 
