/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/rendering/transferfunction.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul::opengl {
    class ProgramObject;
    class TextureUnit;
} // namespace ghoul::opengl

namespace openspace {
    class Histogram;
    struct RenderData;
} // namespace openspace

namespace openspace::volume {

class BasicVolumeRaycaster;
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

    static documentation::Documentation Documentation();

private:
    struct Timestep {
        std::filesystem::path baseName;
        bool inRam;
        bool onGpu;
        RawVolumeMetadata metadata;
        std::shared_ptr<RawVolume<float>> rawVolume;
        std::shared_ptr<ghoul::opengl::Texture> texture;
        std::shared_ptr<Histogram> histogram;
        std::vector<float> rawData;
    };

    Timestep* currentTimestep();
    int timestepIndex(const Timestep* t) const;
    Timestep* timestepFromIndex(int target);
    void jumpToTimestep(int target);

    void loadTimestepMetadata(const std::filesystem::path& path, float& globalMin,
        float& globalMax);

    glm::mat4 calculateModelTransform();
    void createPlane() const;
    void renderVolumeSlice(const RenderData& data);

    void loadVtiFromCache(const std::filesystem::path& cached, double timestep,
        RawVolumeMetadata& metadata, Timestep& t, float& globalMin, float& globalMax);
    void loadVti(const std::filesystem::path& path, double timestep, Timestep& t,
        float& globalMin, float& globalMax, const RawVolumeMetadata& metadata);
    void saveVtiCache(const std::filesystem::path& cached,
        const std::vector<float>& scalars);

    properties::OptionProperty _gridType;
    std::shared_ptr<VolumeClipPlanes> _clipPlanes;


    properties::FloatProperty _stepSize;
    properties::FloatProperty _brightness;
    properties::FloatProperty _rNormalization;
    properties::FloatProperty _rUpperBound;
    properties::FloatProperty _secondsBefore;
    properties::FloatProperty _secondsAfter;
    properties::StringProperty _sourceDirectory;
    properties::StringProperty _transferFunctionPath;

    properties::TriggerProperty _triggerTimeJump;
    properties::IntProperty _jumpToTimestep;
    properties::Vec2Property _valueRange;
    properties::BoolProperty _hideOutsideRange;

    std::map<double, Timestep> _volumeTimesteps;
    std::unique_ptr<BasicVolumeRaycaster> _raycaster;
    bool _invertDataAtZ;
    bool _useCaching = true;

    struct VolumeSliceSettings : properties::PropertyOwner {
        VolumeSliceSettings();

        properties::Vec3Property normal;
        properties::FloatProperty offset;
        properties::BoolProperty shouldRenderSlice;
    };

    VolumeSliceSettings _volumeSlice;

    bool _slicePlaneIsDirty = true;
    glm::mat3 _basisTransform = glm::mat3(1.0f);

    GLuint _quad = 0;
    GLuint _vertexPositionBuffer = 0;
    ghoul::opengl::ProgramObject* _shader = nullptr;
    UniformCache(normal, offset, basis, volumeTexture, transferFunction, modelTransform,
        modelViewTransform, modelViewProjection, volumeResolution, valueRange)
        _uniformCache;

    std::shared_ptr<openspace::TransferFunction> _transferFunction;
};

} // namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME___RENDERABLETIMEVARYINGVOLUME___H__
