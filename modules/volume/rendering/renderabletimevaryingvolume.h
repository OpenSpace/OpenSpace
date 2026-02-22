/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/volume/rawvolumemetadata.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/misc/triggerproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/rendering/transferfunction.h>
#include <filesystem>
#include <memory>

namespace openspace {

class BasicVolumeRaycaster;
class Histogram;
template <typename T> class RawVolume;
class VolumeClipPlanes;

class RenderableTimeVaryingVolume : public Renderable {
public:
    explicit RenderableTimeVaryingVolume(const ghoul::Dictionary& dictionary);
    ~RenderableTimeVaryingVolume() override;

    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& tasks) override;
    void update(const UpdateData& data) override;

    static openspace::Documentation Documentation();

private:
    struct Timestep {
        std::filesystem::path baseName;
        bool inRam;
        bool onGpu;
        RawVolumeMetadata metadata;
        std::shared_ptr<RawVolume<float>> rawVolume;
        std::shared_ptr<ghoul::opengl::Texture> texture;
        std::shared_ptr<Histogram> histogram;
    };

    Timestep* currentTimestep();
    int timestepIndex(const Timestep* t) const;
    Timestep* timestepFromIndex(int target);
    void jumpToTimestep(int target);

    void loadTimestepMetadata(const std::filesystem::path& path);

    OptionProperty _gridType;
    std::shared_ptr<VolumeClipPlanes> _clipPlanes;

    FloatProperty _stepSize;
    FloatProperty _brightness;
    FloatProperty _rNormalization;
    FloatProperty _rUpperBound;
    FloatProperty _secondsBefore;
    FloatProperty _secondsAfter;
    StringProperty _sourceDirectory;
    StringProperty _transferFunctionPath;

    TriggerProperty _triggerTimeJump;
    IntProperty _jumpToTimestep;

    std::map<double, Timestep> _volumeTimesteps;
    std::unique_ptr<BasicVolumeRaycaster> _raycaster;
    bool _invertDataAtZ;

    std::shared_ptr<TransferFunction> _transferFunction;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_VOLUME___RENDERABLETIMEVARYINGVOLUME___H__
