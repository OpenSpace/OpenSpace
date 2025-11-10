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

#include <modules/volume/rendering/renderabletimevaryingvolume.h>

#include <modules/base/basemodule.h>
#include <modules/volume/rendering/basicvolumeraycaster.h>
#include <modules/volume/rendering/volumeclipplanes.h>
#include <modules/volume/transferfunctionhandler.h>
#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumereader.h>
#include <modules/volume/rawvolumewriter.h>
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
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <filesystem>
#include <optional>

#include <modules/volume/xmlreader.h>

namespace {
    constexpr std::string_view _loggerCat = "RenderableTimeVaryingVolume";

    const float SecondsInOneDay = 60 * 60 * 24;

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Step size",
        "Specifies how often to sample during raycasting. Lower step size leads to "
        "higher resolution.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo GridTypeInfo = {
        "GridType",
        "Grid type",
        "The grid type to use for the volume.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SecondsBeforeInfo = {
        "SecondsBefore",
        "Seconds before",
        "The number of seconds to show the first timestep before its actual time.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SecondsAfterInfo = {
        "SecondsAfter",
        "Seconds after",
        "The number of seconds to show the the last timestep after its actual time.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SourceDirectoryInfo = {
        "SourceDirectory",
        "Source directory",
        "A directory from where to load the data files for the different time steps.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "TransferFunctionPath",
        "Transfer function path",
        "The path to the transfer function file.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TriggerTimeJumpInfo = {
        "TriggerTimeJump",
        "Jump",
        "Sets the time to be the first time of the volume sequence.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo JumpToTimestepInfo = {
        "JumpToTimestep",
        "Jump to timestep",
        "Setting this value lets you scrub through the sequence's time steps.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo BrightnessInfo = {
        "Brightness",
        "Brightness",
        "A value that affects the general brightness of the volume rendering.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo rNormalizationInfo = {
        "RNormalization",
        "Radius normalization",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo rUpperBoundInfo = {
        "RUpperBound",
        "Radius upper bound",
        "Limit the volume's radius.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo VolumeSliceNormalInfo = {
        "VolumeSliceNormal",
        "Slice Plane Normal",
        "Normal of the volume slice plane.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo VolumeSliceOffsetInfo = {
        "VolumeSliceOffset",
        "Slice Plane Offset",
        "Offset of the volume slice plane in the volume.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShowVolumeSliceInfo = {
        "Enabled",
        "Show Volume Slice",
        "Determine whether the volume slice plane should be visualized or not.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RangeInfo = {
        "ValueRange",
        "Value Range",
        "The range of values to use in the color mapping. The lowest value will be "
        "mapped to the first color in the color map.",
        openspace::properties::Property::Visibility::AdvancedUser

    };

    constexpr openspace::properties::Property::PropertyInfo HideOutsideInfo = {
        "HideValuesOutsideRange",
        "Hide values outside range",
        "If ture, density values outside the provided range will be hidden, i.e., not "
        "rendered at all.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableTimeVaryingVolume)]] Parameters {
        // [[codegen::verbatim(SourceDirectoryInfo.description)]]
        std::filesystem::path sourceDirectory [[codegen::directory()]];

        // [[codegen::verbatim(TransferFunctionInfo.description)]]
        std::filesystem::path transferFunction;

        // [[codegen::verbatim(SecondsBeforeInfo.description)]]
        std::optional<float> secondsBefore;

        // [[codegen::verbatim(SecondsAfterInfo.description)]]
        float secondsAfter;

        // If true, the volume data will be inverted at its z-axis.
        std::optional<bool> invertDataAtZ;

        // [[codegen::verbatim(BrightnessInfo.description)]]
        std::optional<float> brightness;

        // [[codegen::verbatim(StepSizeInfo.description)]]
        std::optional<float> stepSize;

        // [[codegen::verbatim(GridTypeInfo.description)]]
        std::optional<std::string> gridType [[codegen::inlist("Spherical", "Cartesian")]];

        std::optional<std::vector<ghoul::Dictionary>> clipPlanes [[codegen::reference("volume_volumeclipplane")]];

        // [[codegen::verbatim(VolumeSliceNormalInfo.description)]]
        std::optional<glm::vec3> planeSliceNormal
            [[codegen::inrange(glm::vec3(- 1.f),glm::vec3(1.f))]];

        // [[codegen::verbatim(VolumeSliceOffsetInfo.description)]]
        std::optional<float> planeSliceOffset [[codegen::inrange(-0.5f,0.5f)]];

        // [[codegen::verbatim(ShowVolumeSliceInfo.description)]]
        std::optional<bool> showVolumeSlice;

        // [[codegen::verbatim(RangeInfo.description)]]
        std::optional<glm::vec2> valueRange;

        // [[codegen::verbatim(HideOutsideInfo.description)]]
        std::optional<bool> hideValuesOutsideRange;

        // If true (default), the loaded dataset and color map will be cached so that they
        // can be loaded faster at a later time. This does however mean that any updates
        // to the values in the dataset will not lead to changes in the rendering without
        // first removing the cached file. Set it to false to disable caching. This can be
        // useful for example when working on importing a new dataset or when making
        // changes to the color map.
        std::optional<bool> useCaching;
    };
#include "renderabletimevaryingvolume_codegen.cpp"
} // namespace

namespace openspace::volume {

documentation::Documentation RenderableTimeVaryingVolume::Documentation() {
    return codegen::doc<Parameters>("volume_renderable_timevaryingvolume");
}

RenderableTimeVaryingVolume::VolumeSliceSettings::VolumeSliceSettings()
    : properties::PropertyOwner({
        "SliceSettings",
        "Slice Settings",
        "Settings for the volume slice plane."
        })
    , normal(VolumeSliceNormalInfo, glm::vec3{ 1.f, 0.f, 0.f })
    , offset(VolumeSliceOffsetInfo, 0, -0.5f, 0.5f)
    , shouldRenderSlice(ShowVolumeSliceInfo, false)
{
    addProperty(shouldRenderSlice);
    addProperty(normal);
    addProperty(offset);
}



RenderableTimeVaryingVolume::RenderableTimeVaryingVolume(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _gridType(GridTypeInfo)
    , _stepSize(StepSizeInfo, 0.01f, 0.001f, 0.1f, 0.001f)
    , _brightness(BrightnessInfo, 0.33f, 0.f, 1.f)
    , _rNormalization(rNormalizationInfo, 0.f, 0.f, 2.f)
    , _rUpperBound(rUpperBoundInfo, 1.f, 0.f, 2.f)
    , _secondsBefore(SecondsBeforeInfo, 0.f, 0.01f, SecondsInOneDay)
    , _secondsAfter(SecondsAfterInfo, 0.f, 0.01f, SecondsInOneDay)
    , _sourceDirectory(SourceDirectoryInfo)
    , _transferFunctionPath(TransferFunctionInfo)
    , _triggerTimeJump(TriggerTimeJumpInfo)
    , _jumpToTimestep(JumpToTimestepInfo, 0, 0, 256)
    , _valueRange(RangeInfo, glm::vec2(
        std::numeric_limits<float>::lowest(),
        std::numeric_limits<float>::max())
    )
    , _hideOutsideRange(HideOutsideInfo, false)
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
        { static_cast<int>(volume::VolumeGridType::Cartesian), "Cartesian" },
        { static_cast<int>(volume::VolumeGridType::Spherical), "Spherical" }
    });
    _gridType = static_cast<int>(volume::VolumeGridType::Cartesian);

    _stepSize = p.stepSize.value_or(_stepSize);

    _brightness = p.brightness.value_or(_brightness);
    _secondsBefore = p.secondsBefore.value_or(_secondsBefore);
    _secondsAfter = p.secondsAfter;

    const std::vector<ghoul::Dictionary> clipPlanes = p.clipPlanes.value_or(
                                                          std::vector<ghoul::Dictionary>()
    );
    _clipPlanes = std::make_shared<volume::VolumeClipPlanes>(clipPlanes);

    if (p.gridType.has_value()) {
        const VolumeGridType gridType = volume::parseGridType(*p.gridType);
        _gridType = static_cast<std::underlying_type_t<VolumeGridType>>(gridType);
    }

    addProperty(_brightness);
    addProperty(Fadeable::_opacity);

    _volumeSlice.normal = p.planeSliceNormal.value_or(_volumeSlice.normal);
    _volumeSlice.offset = p.planeSliceOffset.value_or(_volumeSlice.offset);
    _volumeSlice.shouldRenderSlice = p.showVolumeSlice.value_or(
                                                            _volumeSlice.shouldRenderSlice
    );

    _volumeSlice.normal.onChange([this]() {
        _slicePlaneIsDirty = true;
    });
    _volumeSlice.offset.onChange([this]() {
        _slicePlaneIsDirty = true;
    });

    addPropertySubOwner(_volumeSlice);

    _valueRange = p.valueRange.value_or(_valueRange);
    _hideOutsideRange = p.hideValuesOutsideRange.value_or(_hideOutsideRange);
    _useCaching = p.useCaching.value_or(_useCaching);

    addProperty(_valueRange);
    addProperty(_hideOutsideRange);
}

RenderableTimeVaryingVolume::~RenderableTimeVaryingVolume() {}

void RenderableTimeVaryingVolume::initializeGL() {
    std::filesystem::path sequenceDir = absPath(_sourceDirectory);

    if (!std::filesystem::is_directory(sequenceDir)) {
        LERROR(std::format("Could not load sequence directory '{}'", sequenceDir));
        return;
    }

    namespace fs = std::filesystem;

    // TODO refactor into datareader
    int step = 0;
    double deltaTimeStep = 0.0;
    double startTime = 0.0;
    volume::RawVolumeMetadata metadata;


    // VTI specific metadata
    std::filesystem::path metaDataPath = sequenceDir / ".metadata";
    if (std::filesystem::is_regular_file(metaDataPath)) {
        const ghoul::Dictionary dictionary = ghoul::lua::loadDictionaryFromFile(metaDataPath);
        const double nVtiSamples = dictionary.value<double>("TimeSamples");
        const std::string start = dictionary.value<std::string>("Start");
        const std::string end = dictionary.value<std::string>("End");

        startTime = Time::convertTime(start);
        const double endTime = Time::convertTime(end);
        const double simulationTime = endTime - startTime;

        deltaTimeStep = simulationTime / nVtiSamples;

        glm::ivec3 minExtent = dictionary.value<glm::dvec3>("MinExtent");
        glm::ivec3 maxExtent = dictionary.value<glm::dvec3>("MaxExtent");
        metadata.dimensions = 1 + static_cast<glm::ivec3>(maxExtent - minExtent);

        metadata.hasDomainBounds = true;
        metadata.lowerDomainBound =
            static_cast<glm::vec3>(dictionary.value<glm::dvec3>("MinDomain"));
        metadata.upperDomainBound =
            static_cast<glm::vec3>(dictionary.value<glm::dvec3>("MaxDomain"));

        metadata.hasValueRange = true;
        metadata.minValue = static_cast<float>(dictionary.value<double>("MinValue"));
        metadata.maxValue = static_cast<float>(dictionary.value<double>("MaxValue"));
        metadata.hasTime = true;
    }

    float globalMin = std::numeric_limits<float>::max();
    float globalMax = std::numeric_limits<float>::lowest();

    const std::vector<std::filesystem::path> paths = ghoul::filesystem::walkDirectory(
        sequenceDir,
        ghoul::filesystem::Recursive::Yes,
        ghoul::filesystem::Sorted::Yes,
        [](const fs::path& p) { return fs::is_regular_file(p); }
    );

    for (const std::filesystem::path& path : paths) {
        if (path.extension() == ".dictionary") {
            loadTimestepMetadata(path, globalMin, globalMax);
        }
        if (path.extension() == ".vti") {
            Timestep t;
            double timestep = startTime + deltaTimeStep * step++;

            std::filesystem::path cached =
                FileSys.cacheManager()->cachedFilename(path);
            if (_useCaching) {

                // Load caching data if it exists, otherwise we'll load normally and store a
                // cache for next time
                if (std::filesystem::exists(cached)) {
                    LINFO(std::format("Cached file {} used for file {}", cached, path));
                    loadVtiFromCache(cached, timestep, metadata, t, globalMin, globalMax);
                }
                else {
                    loadVti(path, timestep, t, globalMin, globalMax, metadata);
                    saveVtiCache(cached, t.rawData);
                }
            }
            else {
                loadVti(path, timestep, t, globalMin, globalMax, metadata);
                saveVtiCache(cached, t.rawData);
            }

            t.baseName = "";
            t.inRam = false;
            t.onGpu = false;
            _volumeTimesteps[t.metadata.time] = std::move(t);
        }
    }

    // TODO: defer loading of data to later (separate thread or at least not when loading)
    for (std::pair<const double, Timestep>& p : _volumeTimesteps) {
        Timestep& t = p.second;

        // Read volume from file if it exists
        if (t.baseName != "") {
            const std::string path = std::format(
                "{}/{}.rawvolume", _sourceDirectory.value(), t.baseName
            );
            RawVolumeReader<float> reader(path, t.metadata.dimensions);
            t.rawVolume = reader.read(_invertDataAtZ);
            const unsigned length = t.metadata.dimensions.x * t.metadata.dimensions.y *
                t.metadata.dimensions.z;
            for (size_t i = 0; i < length; i++) {
                float d = t.rawVolume->data()[i];
                if (!std::isinf(d) && !std::isnan(d)) {
                    globalMin = std::min(globalMin, d);
                    globalMax = std::max(globalMax, d);
                }
            }
        }

        float* data;
        // We've read data from binary file
        if (t.rawVolume) {
            data = t.rawVolume->data();
        }
        // Data came from xml file
        else {
            data = t.rawData.data();
        }

        t.texture = std::make_shared<ghoul::opengl::Texture>(
            t.metadata.dimensions,
            GL_TEXTURE_3D,
            ghoul::opengl::Texture::Format::Red,
            GL_R32F,
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

    // Data range was not set by user,  set it based on min and max values of the data
    if (_valueRange.value().x == std::numeric_limits<float>::lowest()) {
        _valueRange = glm::vec2(globalMin, _valueRange.value().y);
    }
    if (_valueRange.value().y == std::numeric_limits<float>::max()) {
        _valueRange = glm::vec2(_valueRange.value().x, globalMax);
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

    // Slice plane
    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);
    createPlane();
    _shader = BaseModule::ProgramObjectManager.request(
        "SlicePlane",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "SlicePlane",
                absPath("${MODULE_VOLUME}/shaders/plane_vs.glsl"),
                absPath("${MODULE_VOLUME}/shaders/plane_fs.glsl")
            );
        }
    );
    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
}

void RenderableTimeVaryingVolume::loadTimestepMetadata(const std::filesystem::path& path,
                                                       float& globalMin, float& globalMax)
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

    globalMin = std::min(t.metadata.minValue, globalMin);
    globalMax = std::max(t.metadata.maxValue, globalMax);

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

        // Set scale matrix: The original data cube is a unit cube centered in 0, i.e.,
        // with lower bound from (-0.5, -0.5, -0.5) and upper bound (0.5, 0.5, 0.5)
        if (t && t->texture) {
            if (_raycaster->gridType() == volume::VolumeGridType::Cartesian) {
                _raycaster->setModelTransform(calculateModelTransform());
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
        _raycaster->setValueRange(_valueRange);
        _raycaster->setHideOutsideRange(_hideOutsideRange);
    }
}

void RenderableTimeVaryingVolume::render(const RenderData& data, RendererTasks& tasks) {
    if (_raycaster && _raycaster->volumeTexture()) {
        tasks.raycasterTasks.push_back({ _raycaster.get(), data });
    }

    if (_volumeSlice.shouldRenderSlice) {
        renderVolumeSlice(data);
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

    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    BaseModule::ProgramObjectManager.release(
        "PlaneSlice",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _shader = nullptr;
}

glm::mat4 RenderableTimeVaryingVolume::calculateModelTransform() {
    Timestep* t = currentTimestep();

    if (!t) {
        return glm::mat4(1.0f);
    }

    const glm::dvec3 scale =
        t->metadata.upperDomainBound - t->metadata.lowerDomainBound;
    const glm::dvec3 translation =
        (t->metadata.lowerDomainBound + t->metadata.upperDomainBound) * 0.5f;

    glm::dmat4 translationMatrix = glm::translate(glm::dmat4(1.0), translation);
    const glm::dmat4 scaleMatrix = glm::scale(glm::dmat4(1.0), scale);
    glm::dmat4 modelTransform = translationMatrix * scaleMatrix;
    return glm::mat4(modelTransform);
}

void RenderableTimeVaryingVolume::createPlane() const {
    const std::array<GLfloat, 36> vertexData = {
    //   x     y
        -1.f, -1.f,
         1.f,  1.f,
        -1.f,  1.f,
        -1.f, -1.f,
         1.f, -1.f,
         1.f,  1.f,
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GL_FLOAT) * 2, nullptr);

    glBindVertexArray(0);
}

void RenderableTimeVaryingVolume::renderVolumeSlice(const RenderData& data) {
    Timestep* t = currentTimestep();

    if (!t || !t->texture) {
        return;
    }

    _shader->activate();
    _shader->setUniform(_uniformCache.offset, _volumeSlice.offset);
    _shader->setUniform(_uniformCache.normal, _volumeSlice.normal);

    _shader->setUniform(_uniformCache.modelTransform,
        glm::mat4(calculateModelTransform())
    );

    auto [modelTransform, modelViewTransform, modelViewProjectionTransform] =
        calcAllTransforms(data);

    _shader->setUniform(_uniformCache.modelViewProjection,
        glm::mat4(modelViewProjectionTransform)
    );
    _shader->setUniform(_uniformCache.modelViewTransform, glm::mat4(modelViewTransform));

    _shader->setUniform(_uniformCache.volumeResolution,
        glm::vec3(t->metadata.dimensions)
    );

    // Upload volume texture
    ghoul::opengl::TextureUnit textureUnit;
    textureUnit.activate();
    t->texture->bind();
    _shader->setUniform(_uniformCache.volumeTexture, textureUnit);
    // Upload transfer function texture
    ghoul::opengl::TextureUnit transferFunctionUnit;
    transferFunctionUnit.activate();
    _transferFunction->texture().bind();
    _shader->setUniform(_uniformCache.transferFunction, transferFunctionUnit);

    if (_slicePlaneIsDirty) {
        // Create basis to convert points on the 2D plane into volume 3D coordinates
        glm::vec3 n = glm::normalize(_volumeSlice.normal.value());

        glm::vec3 up = glm::abs(n.z) < 0.999f ? glm::vec3(0.f, 0.f, 1.f)
            : glm::vec3(1.f, 0.f, 0.f);

        glm::vec3 tangent = glm::normalize(glm::cross(up, n));
        glm::vec3 bitangent = glm::normalize(glm::cross(n, tangent));

        _basisTransform = glm::mat3(tangent, bitangent, n);
    }

    _shader->setUniform(_uniformCache.basis, _basisTransform);

    glDisable(GL_CULL_FACE);
    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);
    glEnable(GL_CULL_FACE);
    _shader->deactivate();
}

void RenderableTimeVaryingVolume::loadVtiFromCache(const std::filesystem::path& cached,
                                                   double timestep,
                                                   RawVolumeMetadata& metadata,
                                                   Timestep& t, float& globalMin,
                                                   float& globalMax)
{
    metadata.time = timestep;
    t.metadata = metadata;

    RawVolumeReader<float> reader(cached, metadata.dimensions);
    t.rawVolume = reader.read(_invertDataAtZ);
    const unsigned length = metadata.dimensions.x * metadata.dimensions.y *
        metadata.dimensions.z;
    // If the metadata contain the min max this is unnecessary
    for (size_t i = 0; i < length; i++) {
        float d = t.rawVolume->data()[i];
        if (!std::isinf(d) && !std::isnan(d)) {
            globalMin = std::min(globalMin, d);
            globalMax = std::max(globalMax, d);
        }
    }
}

void RenderableTimeVaryingVolume::loadVti(const std::filesystem::path& path,
                                          double timestep, Timestep& t, float& globalMin,
                                          float& globalMax,
                                          const RawVolumeMetadata& metadata)
{
    const auto [m, scalars] = readVTIFile(path, timestep);
    // TODO: this will now ignore any metadata in the VTI file and only use the 1 .metadata
    // file supplied. The time is set specifically since the metadata file determines the
    // shown time for each timestep.
    t.metadata = metadata;
    t.metadata.time = timestep;
    t.rawData = scalars;

    globalMin = std::min(globalMin, m.minValue);
    globalMax = std::max(globalMax, m.maxValue);
}

void RenderableTimeVaryingVolume::saveVtiCache(const std::filesystem::path& cached,
                                               const std::vector<float>& scalars)
{
    RawVolumeWriter<float> writer(cached);
    writer.write(scalars);
}

} // namespace openspace::volume
