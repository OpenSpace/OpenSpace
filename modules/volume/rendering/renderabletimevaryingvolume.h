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

#ifndef __OPENSPACE_MODULE_VOLUME___RENDERABLETIMEVARYINGVOLUME___H__
#define __OPENSPACE_MODULE_VOLUME___RENDERABLETIMEVARYINGVOLUME___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
// #include <modules/volume/rawvolume.h>
 #include <modules/volume/rawvolumemetadata.h>
// #include <modules/volume/rendering/basicvolumeraycaster.h>
// #include <modules/volume/rendering/volumeclipplanes.h>

// #include <openspace/properties/vectorproperty.h>
// #include <openspace/properties/optionproperty.h>
// #include <openspace/properties/stringproperty.h>
// #include <openspace/util/boxgeometry.h>
// #include <openspace/util/histogram.h>
// #include <openspace/rendering/transferfunction.h>

namespace openspace {
    class Histogram;
    struct RenderData;
    class TransferFunction;
} // namespace openspace

namespace openspace::volume {

class BasicVolumeRaycaster;
template <typename T> class RawVolume;
class VolumeClipPlanes;

class RenderableTimeVaryingVolume : public Renderable {
public:
    RenderableTimeVaryingVolume(const ghoul::Dictionary& dictionary);
    ~RenderableTimeVaryingVolume();

    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& tasks) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    struct Timestep {
        std::string baseName;
        bool inRam;
        bool onGpu;
        RawVolumeMetadata metadata;
        std::shared_ptr<RawVolume<float>> rawVolume;
        std::shared_ptr<ghoul::opengl::Texture> texture;
        std::shared_ptr<Histogram> histogram;
    };

    Timestep* currentTimestep();
    int timestepIndex(const Timestep* t) const;
    Timestep* timestepFromIndex(int index);
    void jumpToTimestep(int i);

    void loadTimestepMetadata(const std::string& path);

    properties::OptionProperty _gridType;
    std::shared_ptr<VolumeClipPlanes> _clipPlanes;

    properties::FloatProperty _stepSize;
    properties::FloatProperty _opacity;
    properties::FloatProperty _rNormalization;
    properties::FloatProperty _rUpperBound;
    properties::FloatProperty _secondsBefore;
    properties::FloatProperty _secondsAfter;
    properties::StringProperty _sourceDirectory;
    properties::StringProperty _transferFunctionPath;

    properties::TriggerProperty _triggerTimeJump;
    properties::IntProperty _jumpToTimestep;
    properties::IntProperty _currentTimestep;

    std::map<double, Timestep> _volumeTimesteps;
    std::unique_ptr<BasicVolumeRaycaster> _raycaster;

    std::shared_ptr<openspace::TransferFunction> _transferFunction;
};

} // namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME___RENDERABLETIMEVARYINGVOLUME___H__
