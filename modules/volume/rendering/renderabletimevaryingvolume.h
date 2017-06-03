/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_VOLUME___RENDERABLEKAMELEONVOLUME___H__
#define __OPENSPACE_MODULE_VOLUME___RENDERABLEKAMELEONVOLUME___H__

#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/util/boxgeometry.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/transferfunction.h>

#include <modules/volume/rawvolume.h>
#include <modules/volume/rendering/basicvolumeraycaster.h>

#include <modules/volume/rendering/volumeclipplanes.h>

namespace openspace {

struct RenderData;
    
class RenderableTimeVaryingVolume : public Renderable {
public:
    RenderableTimeVaryingVolume(const ghoul::Dictionary& dictionary);
    ~RenderableTimeVaryingVolume();
    
    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& tasks) override;
    void update(const UpdateData& data) override;
    bool cachingEnabled();

private:
    void load();
    void loadFromPath(const std::string& path);
    void loadRaw(const std::string& path);

    void updateTextureFromVolume();
    void updateRaycasterModelTransform();

    properties::Vec3Property _lowerDomainBound;
    properties::Vec3Property _upperDomainBound;
    properties::Vec3Property _domainScale;

    properties::FloatProperty _lowerValueBound;
    properties::FloatProperty _upperValueBound;
    
    properties::OptionProperty _gridType;
    
    std::shared_ptr<VolumeClipPlanes> _clipPlanes;

    properties::FloatProperty _stepSize;
    properties::StringProperty _sourcePath;
    properties::StringProperty _transferFunctionPath;
    
    std::vector<RawVolume<float>> _volumeTimesteps;
    std::unique_ptr<BasicVolumeRaycaster> _raycaster;

    std::shared_ptr<ghoul::opengl::Texture> _volumeTexture;
    std::shared_ptr<TransferFunction> _transferFunction;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_KAMELEONVOLUME___RENDERABLEKAMELEONVOLUME___H__
