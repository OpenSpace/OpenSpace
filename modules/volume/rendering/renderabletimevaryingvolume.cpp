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

#include <modules/volume/rendering/renderabletimevaryingvolume.h>

#include <modules/volume/rendering/basicvolumeraycaster.h>
#include <modules/volume/rendering/volumeclipplanes.h>
#include <modules/volume/transferfunctionhandler.h>
#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumereader.h>
#include <modules/volume/volumegridtype.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/histogram.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <filesystem>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "RenderableTimeVaryingVolume";

    const float SecondsInOneDay = 60 * 60 * 24;

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Step Size",
        "Specifies how often to sample on the raycaster. Lower step -> higher resolution",
        // @VISIBILITY(3.5)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo GridTypeInfo = {
        "GridType",
        "Grid Type",
        "Spherical or Cartesian grid",
        // @VISIBILITY(3.5)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SecondsBeforeInfo = {
        "SecondsBefore",
        "Seconds before",
        "Specifies the number of seconds to show the first timestep before its "
        "actual time. The default value is 0",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SecondsAfterInfo = {
        "SecondsAfter",
        "Seconds after",
        "Specifies the number of seconds to show the the last timestep after its "
        "actual time",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SourceDirectoryInfo = {
        "SourceDirectory",
        "Source Directory",
        "Specifies the path to load timesteps from",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "TransferFunctionPath",
        "Transfer Function Path",
        "Specifies the transfer function file path",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TriggerTimeJumpInfo = {
        "TriggerTimeJump",
        "Jump",
        "Sets the time to be the first time of the volume sequence",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo JumpToTimestepInfo = {
        "JumpToTimestep",
        "Jump to timestep",
        "Lets you scrub through the sequence's time steps",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo BrightnessInfo = {
        "Brightness",
        "Brightness",
        "The volume renderer's general brightness",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo rNormalizationInfo = {
        "RNormalization",
        "Radius normalization",
        "", // @TODO Missing documentation
        // @VISIBILITY(3.5)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo rUpperBoundInfo = {
        "RUpperBound",
        "Radius upper bound",
        "Limit the volume's radius",
        // @VISIBILITY(3.5)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableTimeVaryingVolume)]] Parameters {
        // [[codegen::verbatim(SourceDirectoryInfo.description)]]
        std::string sourceDirectory;

        // [[codegen::verbatim(TransferFunctionInfo.description)]]
        std::string transferFunction;

        // [[codegen::verbatim(SecondsBeforeInfo.description)]]
        std::optional<float> secondsBefore;

        // [[codegen::verbatim(SecondsAfterInfo.description)]]
        float secondsAfter;

        // Specifies if you want to invert the volume data at it z-axis.
        std::optional<bool> invertDataAtZ;

        // [[codegen::verbatim(BrightnessInfo.description)]]
        std::optional<float> brightness;

        // [[codegen::verbatim(StepSizeInfo.description)]]
        std::optional<float> stepSize;

        // [[codegen::verbatim(GridTypeInfo.description)]]
        std::optional<std::string> gridType;

        // @TODO Missing documentation
        std::optional<ghoul::Dictionary> clipPlanes;
    };
#include "renderabletimevaryingvolume_codegen.cpp"
} // namespace

namespace openspace::volume {

documentation::Documentation RenderableTimeVaryingVolume::Documentation() {
    return codegen::doc<Parameters>("volume_renderable_timevaryingvolume");
}

RenderableTimeVaryingVolume::RenderableTimeVaryingVolume(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _gridType(GridTypeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _stepSize(StepSizeInfo, 0.02f, 0.001f, 0.1f)
    , _brightness(BrightnessInfo, 0.33f, 0.f, 1.f)
    , _rNormalization(rNormalizationInfo, 0.f, 0.f, 2.f)
    , _rUpperBound(rUpperBoundInfo, 1.f, 0.f, 2.f)
    , _secondsBefore(SecondsBeforeInfo, 0.f, 0.01f, SecondsInOneDay)
    , _secondsAfter(SecondsAfterInfo, 0.f, 0.01f, SecondsInOneDay)
    , _sourceDirectory(SourceDirectoryInfo)
    , _transferFunctionPath(TransferFunctionInfo)
    , _triggerTimeJump(TriggerTimeJumpInfo)
    , _jumpToTimestep(JumpToTimestepInfo, 0, 0, 256)
    , _invertDataAtZ(false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sourceDirectory = absPath(p.sourceDirectory).string();
    _transferFunctionPath = absPath(p.transferFunction).string();
    _transferFunction = std::make_shared<openspace::TransferFunction>(
        _transferFunctionPath.value(),
        [](const openspace::TransferFunction&) {}
    );

    _invertDataAtZ = p.invertDataAtZ.value_or(_invertDataAtZ);

    _gridType.addOptions({
        { static_cast<int>(volume::VolumeGridType::Cartesian), "Cartesian grid" },
        { static_cast<int>(volume::VolumeGridType::Spherical), "Spherical grid" }
    });
    _gridType = static_cast<int>(volume::VolumeGridType::Cartesian);

    _stepSize = p.stepSize.value_or(_stepSize);

    _brightness = p.brightness.value_or(_brightness);
    _secondsBefore = p.secondsBefore.value_or(_secondsBefore);
    _secondsAfter = p.secondsAfter;

    const ghoul::Dictionary clipPlanesDict = p.clipPlanes.value_or(ghoul::Dictionary());
    _clipPlanes = std::make_shared<volume::VolumeClipPlanes>(clipPlanesDict);
    _clipPlanes->setIdentifier("clipPlanes");
    _clipPlanes->setGuiName("Clip Planes");

    if (p.gridType.has_value()) {
        const VolumeGridType gridType = volume::parseGridType(*p.gridType);
        _gridType = static_cast<std::underlying_type_t<VolumeGridType>>(gridType);
    }

    addProperty(_brightness);
    addProperty(Fadeable::_opacity);
}

RenderableTimeVaryingVolume::~RenderableTimeVaryingVolume() {}

void RenderableTimeVaryingVolume::initializeGL() {
    std::filesystem::path sequenceDir = absPath(_sourceDirectory);

    if (!std::filesystem::is_directory(sequenceDir)) {
        LERROR(std::format("Could not load sequence directory '{}'", sequenceDir));
        return;
    }

    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::recursive_directory_iterator(sequenceDir)) {
        if (e.is_regular_file() && e.path().extension() == ".dictionary") {
            loadTimestepMetadata(e.path());
        }
    }

    // TODO: defer loading of data to later (separate thread or at least not when loading)
    for (std::pair<const double, Timestep>& p : _volumeTimesteps) {
        Timestep& t = p.second;
        const std::string path = std::format(
            "{}/{}.rawvolume", _sourceDirectory.value(), t.baseName
        );
        RawVolumeReader<float> reader(path, t.metadata.dimensions);
        t.rawVolume = reader.read(_invertDataAtZ);

        const float min = t.metadata.minValue;
        const float diff = t.metadata.maxValue - t.metadata.minValue;
        float* data = t.rawVolume->data();
        for (size_t i = 0; i < t.rawVolume->nCells(); i++) {
            data[i] = glm::clamp((data[i] - min) / diff, 0.f, 1.f);
        }

        t.histogram = std::make_shared<Histogram>(0.f, 1.f, 100);
        for (size_t i = 0; i < t.rawVolume->nCells(); i++) {
            t.histogram->add(data[i]);
        }
        // TODO: handle normalization properly for different timesteps + transfer function

        t.texture = std::make_shared<ghoul::opengl::Texture>(
            t.metadata.dimensions,
            GL_TEXTURE_3D,
            ghoul::opengl::Texture::Format::Red,
            GL_RED,
            GL_FLOAT,
            ghoul::opengl::Texture::FilterMode::Linear,
            ghoul::opengl::Texture::WrappingMode::Clamp
        );

        t.texture->setPixelData(
            reinterpret_cast<void*>(data),
            ghoul::opengl::Texture::TakeOwnership::No
        );
        t.texture->uploadTexture();
    }

    _clipPlanes->initialize();

    _raycaster = std::make_unique<volume::BasicVolumeRaycaster>(
        nullptr,
        _transferFunction,
        _clipPlanes
    );

    _raycaster->initialize();
    global::raycasterManager->attachRaycaster(*_raycaster);
    onEnabledChange([this](bool enabled) {
        if (enabled) {
            global::raycasterManager->attachRaycaster(*_raycaster);
        }
        else {
            global::raycasterManager->detachRaycaster(*_raycaster);
        }
    });

    _triggerTimeJump.onChange([this]() { jumpToTimestep(_jumpToTimestep); });

    _jumpToTimestep.onChange([this]() { jumpToTimestep(_jumpToTimestep); });

    const int lastTimestep = !_volumeTimesteps.empty() ?
        static_cast<int>(_volumeTimesteps.size() - 1) :
        0;
    _jumpToTimestep.setMaxValue(lastTimestep);

    addProperty(_stepSize);
    addProperty(_transferFunctionPath);
    addProperty(_sourceDirectory);
    addPropertySubOwner(_clipPlanes.get());

    addProperty(_triggerTimeJump);
    addProperty(_jumpToTimestep);
    addProperty(_rNormalization);
    addProperty(_rUpperBound);
    addProperty(_gridType);

    _raycaster->setGridType(static_cast<VolumeGridType>(_gridType.value()));
    _gridType.onChange([this] {
        _raycaster->setGridType(static_cast<VolumeGridType>(_gridType.value()));
    });

    _transferFunctionPath.onChange([this] {
        _transferFunction = std::make_shared<openspace::TransferFunction>(
            _transferFunctionPath.value()
        );
        _raycaster->setTransferFunction(_transferFunction);
    });
}

void RenderableTimeVaryingVolume::loadTimestepMetadata(const std::filesystem::path& path)
{
    RawVolumeMetadata metadata;

    try {
        const ghoul::Dictionary dictionary = ghoul::lua::loadDictionaryFromFile(path);
        metadata = RawVolumeMetadata::createFromDictionary(dictionary);
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
        return;
    }
    catch (...) {
        return;
    }

    Timestep t;
    t.metadata = metadata;
    t.baseName = path.stem();
    t.inRam = false;
    t.onGpu = false;

    _volumeTimesteps[t.metadata.time] = std::move(t);
}

RenderableTimeVaryingVolume::Timestep* RenderableTimeVaryingVolume::currentTimestep() {
    if (_volumeTimesteps.empty()) {
        return nullptr;
    }
    const double currentTime = global::timeManager->time().j2000Seconds();

    // Get the first item with time > currentTime
    auto currentTimestepIt = _volumeTimesteps.upper_bound(currentTime);
    if (currentTimestepIt == _volumeTimesteps.end()) {
        // No such timestep was found: show last timestep if it is within the time margin.
        Timestep* lastTimestep = &(_volumeTimesteps.rbegin()->second);
        const double threshold =
            lastTimestep->metadata.time + static_cast<double>(_secondsAfter);
        return currentTime < threshold ? lastTimestep : nullptr;
    }

    if (currentTimestepIt == _volumeTimesteps.begin()) {
        // No such timestep was found: show first timestep if it is within the time margin
        Timestep* firstTimestep = &(_volumeTimesteps.begin()->second);
        const double threshold =
            firstTimestep->metadata.time - static_cast<double>(_secondsBefore);
        return currentTime >= threshold ? firstTimestep : nullptr;
    }

    // Get the last item with time <= currentTime
    currentTimestepIt--;
    return &(currentTimestepIt->second);
}

int RenderableTimeVaryingVolume::timestepIndex(
                                     const RenderableTimeVaryingVolume::Timestep* t) const
{
    if (!t) {
        return -1;
    }
    int index = 0;
    for (const std::pair<const double, Timestep>& it : _volumeTimesteps) {
        if (&(it.second) == t) {
            return index;
        }
        index++;
    }
    return -1;
}

// @TODO Can this be turned into a const ref?
RenderableTimeVaryingVolume::Timestep* RenderableTimeVaryingVolume::timestepFromIndex(
                                                                               int target)
{
    if (target < 0) {
        target = 0;
    }
    int index = 0;
    for (std::pair<const double, Timestep>& it : _volumeTimesteps) {
        if (index == target) {
            return &(it.second);
        }
        index++;
    }
    return nullptr;
}

void RenderableTimeVaryingVolume::jumpToTimestep(int target) {
    Timestep* t = timestepFromIndex(target);
    if (t) {
        global::timeManager->setTimeNextFrame(Time(t->metadata.time));
    }
}

void RenderableTimeVaryingVolume::update(const UpdateData&) {
    _transferFunction->update();

    if (_raycaster) {
        Timestep* t = currentTimestep();

        // Set scale and translation matrices:
        // The original data cube is a unit cube centered in 0
        // ie with lower bound from (-0.5, -0.5, -0.5) and upper bound (0.5, 0.5, 0.5)
        if (t && t->texture) {
            if (_raycaster->gridType() == volume::VolumeGridType::Cartesian) {
                const glm::dvec3 scale =
                    t->metadata.upperDomainBound - t->metadata.lowerDomainBound;
                const glm::dvec3 translation =
                    (t->metadata.lowerDomainBound + t->metadata.upperDomainBound) * 0.5f;

                glm::dmat4 modelTransform = glm::translate(glm::dmat4(1.0), translation);
                const glm::dmat4 scaleMatrix = glm::scale(glm::dmat4(1.0), scale);
                modelTransform = modelTransform * scaleMatrix;
                _raycaster->setModelTransform(glm::mat4(modelTransform));
            }
            else {
                // The diameter is two times the maximum radius.
                // No translation: the sphere is always centered in (0, 0, 0)
                _raycaster->setModelTransform(
                    glm::scale(
                        glm::dmat4(1.0),
                        glm::dvec3(2.0 * t->metadata.upperDomainBound[0])
                    )
                );
            }
            _raycaster->setVolumeTexture(t->texture);
        }
        else {
            _raycaster->setVolumeTexture(nullptr);
        }
        _raycaster->setStepSize(_stepSize);
        _raycaster->setBrightness(_brightness * opacity());
        _raycaster->setRNormalization(_rNormalization);
        _raycaster->setRUpperBound(_rUpperBound);
    }
}

void RenderableTimeVaryingVolume::render(const RenderData& data, RendererTasks& tasks) {
    if (_raycaster && _raycaster->volumeTexture()) {
        tasks.raycasterTasks.push_back({ _raycaster.get(), data });
    }
}

bool RenderableTimeVaryingVolume::isReady() const {
    return true;
}

void RenderableTimeVaryingVolume::deinitializeGL() {
    if (_raycaster) {
        global::raycasterManager->detachRaycaster(*_raycaster);
        _raycaster = nullptr;
    }
}

} // namespace openspace::volume
