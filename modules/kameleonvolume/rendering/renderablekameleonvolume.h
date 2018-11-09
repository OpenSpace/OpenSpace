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

#ifndef __OPENSPACE_MODULE_KAMELEONVOLUME___RENDERABLEKAMELEONVOLUME___H__
#define __OPENSPACE_MODULE_KAMELEONVOLUME___RENDERABLEKAMELEONVOLUME___H__

#include <openspace/rendering/renderable.h>
#include <openspace/rendering/transferfunction.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <modules/volume/rawvolume.h>
#include <modules/volume/rendering/basicvolumeraycaster.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/uvec3property.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace { struct RenderData; }

namespace openspace::volume {
    class BasicVolumeRaycaster;
    template <typename T> class RawVolume;
    class TransferFunctionHandler;
    class VolumeClipPlanes;
} // openspace::volume

namespace openspace::kameleonvolume {

class RenderableKameleonVolume : public Renderable {
public:
    RenderableKameleonVolume(const ghoul::Dictionary& dictionary);
    ~RenderableKameleonVolume();

    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& tasks) override;
    void update(const UpdateData& data) override;
    bool isCachingEnabled() const;

private:
    void load();
    void loadFromPath(const std::string& path);
    void loadRaw(const std::string& path);
    void loadCdf(const std::string& path);
    void storeRaw(const std::string& path);

    std::string cacheSuffix() const;
    void updateTextureFromVolume();
    void updateRaycasterModelTransform();

    properties::UVec3Property _dimensions;
    properties::StringProperty _variable;
    properties::Vec3Property _lowerDomainBound;
    properties::Vec3Property _upperDomainBound;
    properties::Vec3Property _domainScale;
    bool _autoDomainBounds = false;

    properties::FloatProperty _lowerValueBound;
    properties::FloatProperty _upperValueBound;
    bool _autoValueBounds = false;

    properties::OptionProperty _gridType;
    bool _autoGridType = false;

    std::shared_ptr<volume::VolumeClipPlanes> _clipPlanes;

    properties::FloatProperty _stepSize;
    properties::StringProperty _sourcePath;
    properties::StringProperty _transferFunctionPath;
    properties::BoolProperty _cache;


    std::unique_ptr<volume::RawVolume<float>> _rawVolume;
    std::unique_ptr<volume::RawVolume<GLfloat>> _normalizedVolume;
    std::unique_ptr<volume::BasicVolumeRaycaster> _raycaster;

    std::shared_ptr<ghoul::opengl::Texture> _volumeTexture;
    std::shared_ptr<openspace::TransferFunction> _transferFunction;
};

} // namespace openspace::kameleonvolume

#endif // __OPENSPACE_MODULE_KAMELEONVOLUME___RENDERABLEKAMELEONVOLUME___H__
