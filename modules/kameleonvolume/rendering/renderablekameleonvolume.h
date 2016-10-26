/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __RENDERABLEKAMELEONVOLUME_H__
#define __RENDERABLEKAMELEONVOLUME_H__

#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/util/boxgeometry.h>
#include <openspace/util/blockplaneintersectiongeometry.h>

#include <openspace/rendering/renderable.h>

#include <openspace/rendering/transferfunction.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <modules/volume/rawvolume.h>
#include <modules/kameleonvolume/rendering/kameleonvolumeraycaster.h>


namespace openspace {

struct RenderData;
    
class RenderableKameleonVolume : public Renderable {
public:
    RenderableKameleonVolume(const ghoul::Dictionary& dictionary);
    ~RenderableKameleonVolume();
    
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
    void loadCdf(const std::string& path);
    void storeRaw(const std::string& path);

    std::string cacheSuffix();
    void updateTextureFromVolume();
    void updateRaycasterModelTransform();

    properties::UVec3Property _dimensions;
    properties::StringProperty _variable;
    properties::Vec3Property _lowerDomainBound;
    properties::Vec3Property _upperDomainBound;
    properties::Vec3Property _domainScale;
    bool _autoDomainBounds;

    properties::FloatProperty _lowerValueBound;
    properties::FloatProperty _upperValueBound;
    bool _autoValueBounds;

    properties::OptionProperty _gridType;
    bool _autoGridType;

    properties::FloatProperty _stepSize;
    properties::StringProperty _sourcePath;
    properties::StringProperty _transferFunctionPath;
    properties::BoolProperty _cache;


    std::unique_ptr<RawVolume<float>> _rawVolume;
    std::unique_ptr<RawVolume<GLfloat>> _normalizedVolume;
    std::unique_ptr<KameleonVolumeRaycaster> _raycaster;

    std::shared_ptr<ghoul::opengl::Texture> _volumeTexture;
    std::shared_ptr<TransferFunction> _transferFunction;
};
}

#endif // __RENDERABLETOYVOLUME_H__
