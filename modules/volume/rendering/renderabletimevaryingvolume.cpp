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
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr const char* _loggerCat = "RenderableTimeVaryingVolume";
} // namespace

namespace {
    constexpr const char* KeyDimensions = "Dimensions";
    constexpr const char* KeyStepSize = "StepSize";
    constexpr const char* KeyTransferFunction = "TransferFunction";
    constexpr const char* KeySourceDirectory = "SourceDirectory";
    constexpr const char* KeyLowerDomainBound = "LowerDomainBound";
    constexpr const char* KeyUpperDomainBound = "UpperDomainBound";
    constexpr const char* KeyClipPlanes = "ClipPlanes";
    constexpr const char* KeySecondsBefore = "SecondsBefore";
    constexpr const char* KeySecondsAfter = "SecondsAfter";
    constexpr const char* KeyGridType = "GridType";
    constexpr const char* KeyMinValue = "MinValue";
    constexpr const char* KeyMaxValue = "MaxValue";
    constexpr const char* KeyTime = "Time";
    constexpr const char* KeyUnit = "VisUnit";
    constexpr const float SecondsInOneDay = 60 * 60 * 24;

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "stepSize",
        "Step Size",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo GridTypeInfo = {
        "gridType",
        "Grid Type",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo SecondsBeforeInfo = {
        "secondsBefore",
        "Seconds before",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo SecondsAfterInfo = {
        "secondsAfter",
        "Seconds after",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo SourceDirectoryInfo = {
        "sourceDirectory",
        "Source Directory",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "transferFunctionPath",
        "Transfer Function Path",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo TriggerTimeJumpInfo = {
        "triggerTimeJump",
        "Jump",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo JumpToTimestepInfo = {
        "jumpToTimestep",
        "Jump to timestep",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo CurrentTimeStepInfo = {
        "currentTimestep",
        "Current timestep",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "opacity",
        "Opacity",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo rNormalizationInfo = {
        "rNormalization",
        "Radius normalization",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo rUpperBoundInfo = {
        "rUpperBound",
        "Radius upper bound",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace::volume {

    documentation::Documentation RenderableTimeVaryingVolume::Documentation() {
    using namespace documentation;
    return {
        "RenderableTimevaryingVolume",
        "volume_renderable_timevaryingvolume",
        {
            {
                KeySourceDirectory,
                new StringVerifier,
                Optional::No,
                "Specifies the path to load timesteps from"
            },
            {
                KeyTransferFunction,
                new StringVerifier,
                Optional::No,
                "Specifies the transfer function file path"
            },
            {
                KeySecondsBefore,
                new DoubleVerifier,
                Optional::Yes,
                "Specifies the number of seconds to show the the first timestep before "
                "its actual time. The default value is 0."
            },
            {
                KeySecondsAfter,
                new DoubleVerifier,
                Optional::No,
                "Specifies the number of seconds to show the the last timestep after its "
                "actual time"
            }
        }
    };
}


//documentation::Documentation RenderableTimeVaryingVolume::TimestepDocumentation() {
//    using namespace documentation;
//    return {
//        "TimevaryingVolumeTimestep",
//        "volume_timevaryingvolumetimestep",
//        {
//            {
//                KeyLowerDomainBound,
//                new Vector3Verifier<float>,
//                Optional::No,
//                "Specifies the lower domain bounds in the model coordinate system",
//            },
//            {
//                KeyUpperDomainBound,
//                new Vector3Verifier<float>,
//                Optional::No,
//                "Specifies the upper domain bounds in the model coordinate system",
//            },
//            {
//                KeyDimensions,
//                new Vector3Verifier<float>,
//                Optional::No,
//                "Specifies the number of grid cells in each dimension",
//            },
//            {
//                KeyTime,
//                new StringVerifier,
//                Optional::No,
//                "Specifies the time on the format YYYY-MM-DDTHH:MM:SS.000Z",
//            },
//            {
//                KeyMinValue,
//                new DoubleVerifier,
//                Optional::No,
//                "Specifies the minimum value stored in the volume"
//            },
//            {
//                KeyMaxValue,
//                new DoubleVerifier,
//                Optional::No,
//                "Specifies the maximum value stored in the volume"
//            }
//        }
//    };
//}

RenderableTimeVaryingVolume::RenderableTimeVaryingVolume(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _gridType(GridTypeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _stepSize(StepSizeInfo, 0.02f, 0.001f, 1.f)
    , _opacity(OpacityInfo, 10.f, 0.f, 500.f)
    , _rNormalization(rNormalizationInfo, 0.f, 0.f, 2.f)
    , _rUpperBound(rUpperBoundInfo, 1.f, 0.f, 2.f)
    , _secondsBefore(SecondsBeforeInfo, 0.f, 0.01f, SecondsInOneDay)
    , _secondsAfter(SecondsAfterInfo, 0.f, 0.01f, SecondsInOneDay)
    , _sourceDirectory(SourceDirectoryInfo)
    , _transferFunctionPath(TransferFunctionInfo)
    , _triggerTimeJump(TriggerTimeJumpInfo)
    , _jumpToTimestep(JumpToTimestepInfo, 0, 0, 256)
    , _currentTimestep(CurrentTimeStepInfo, 0, 0, 256)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableTimeVaryingVolume"
    );

    _sourceDirectory = absPath(dictionary.value<std::string>(KeySourceDirectory));
    _transferFunctionPath = absPath(dictionary.value<std::string>(KeyTransferFunction));
    _transferFunction = std::make_shared<openspace::TransferFunction>(
        _transferFunctionPath,
        [](const openspace::TransferFunction&) {}
    );

    _gridType.addOptions({
        { static_cast<int>(volume::VolumeGridType::Cartesian), "Cartesian grid" },
        { static_cast<int>(volume::VolumeGridType::Spherical), "Spherical grid" }
    });
    _gridType = static_cast<int>(volume::VolumeGridType::Cartesian);

    if (dictionary.hasKeyAndValue<float>(KeyStepSize)) {
        _stepSize = dictionary.value<float>(KeyStepSize);
    }

    if (dictionary.hasKeyAndValue<float>(KeySecondsBefore)) {
        _secondsBefore = dictionary.value<float>(KeySecondsBefore);
    }
    _secondsAfter = dictionary.value<float>(KeySecondsAfter);

    ghoul::Dictionary clipPlanesDictionary;
    dictionary.getValue(KeyClipPlanes, clipPlanesDictionary);
    _clipPlanes = std::make_shared<volume::VolumeClipPlanes>(clipPlanesDictionary);
    _clipPlanes->setIdentifier("clipPlanes");
    _clipPlanes->setGuiName("Clip Planes");

    if (dictionary.hasKeyAndValue<std::string>(KeyGridType)) {
        VolumeGridType gridType = volume::parseGridType(
           dictionary.value<std::string>(KeyGridType)
        );
        _gridType = static_cast<std::underlying_type_t<VolumeGridType>>(gridType);
    }
}

RenderableTimeVaryingVolume::~RenderableTimeVaryingVolume() {}

void RenderableTimeVaryingVolume::initializeGL() {
    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(_sourceDirectory, RawPath::Yes);

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR(fmt::format("Could not load sequence directory '{}'", sequenceDir.path()));
        return;
    }

    using Recursive = ghoul::filesystem::Directory::Recursive;
    using Sort = ghoul::filesystem::Directory::Sort;

    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::No);
    for (const std::string& path : sequencePaths) {
        ghoul::filesystem::File currentFile(path);
        std::string extension = currentFile.fileExtension();
        if (extension == "dictionary") {
            loadTimestepMetadata(path);
        }
    }

    // TODO: defer loading of data to later (separate thread or at least not when loading)
    for (std::pair<const double, Timestep>& p : _volumeTimesteps) {
        Timestep& t = p.second;
        std::string path = FileSys.pathByAppendingComponent(
            _sourceDirectory, t.baseName
        ) + ".rawvolume";
        RawVolumeReader<float> reader(path, t.metadata.dimensions);
        t.rawVolume = reader.read();

        float min = t.metadata.minValue;
        float diff = t.metadata.maxValue - t.metadata.minValue;
        float* data = t.rawVolume->data();
        for (size_t i = 0; i < t.rawVolume->nCells(); ++i) {
            data[i] = glm::clamp((data[i] - min) / diff, 0.f, 1.f);
        }

        t.histogram = std::make_shared<Histogram>(0.f, 1.f, 100);
        for (size_t i = 0; i < t.rawVolume->nCells(); ++i) {
            t.histogram->add(data[i]);
        }

        // TODO: handle normalization properly for different timesteps + transfer function

        t.texture = std::make_shared<ghoul::opengl::Texture>(
            t.metadata.dimensions,
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

    //_transferFunction->initialize();
    _clipPlanes->initialize();

    _raycaster = std::make_unique<volume::BasicVolumeRaycaster>(
        nullptr,
        _transferFunction,
        _clipPlanes
    );

    _raycaster->initialize();
    global::raycasterManager.attachRaycaster(*_raycaster.get());
    onEnabledChange([&](bool enabled) {
        if (enabled) {
            global::raycasterManager.attachRaycaster(*_raycaster.get());
        }
        else {
            global::raycasterManager.detachRaycaster(*_raycaster.get());
        }
    });

    _triggerTimeJump.onChange([this] () { jumpToTimestep(_jumpToTimestep); });

    _jumpToTimestep.onChange([this] () { jumpToTimestep(_jumpToTimestep); });

    const int lastTimestep = !_volumeTimesteps.empty() ?
        static_cast<int>(_volumeTimesteps.size() - 1) :
        0;
    _currentTimestep.setMaxValue(lastTimestep);
    _jumpToTimestep.setMaxValue(lastTimestep);

    addProperty(_stepSize);
    addProperty(_transferFunctionPath);
    addProperty(_sourceDirectory);
    addPropertySubOwner(_clipPlanes.get());

    addProperty(_triggerTimeJump);
    addProperty(_jumpToTimestep);
    addProperty(_currentTimestep);
    addProperty(_opacity);
    addProperty(_rNormalization);
    addProperty(_rUpperBound);
    addProperty(_gridType);

    _raycaster->setGridType(static_cast<VolumeGridType>(_gridType.value()));
    _gridType.onChange([this] {
        _raycaster->setGridType(static_cast<VolumeGridType>(_gridType.value()));
    });

    _transferFunctionPath.onChange([this] {
        _transferFunction = std::make_shared<openspace::TransferFunction>(
            _transferFunctionPath
        );
        _raycaster->setTransferFunction(_transferFunction);
    });
}

void RenderableTimeVaryingVolume::loadTimestepMetadata(const std::string& path) {
    RawVolumeMetadata metadata;

    try {
        ghoul::Dictionary dictionary = ghoul::lua::loadDictionaryFromFile(path);
        metadata = RawVolumeMetadata::createFromDictionary(dictionary);
    } catch (...) {
        return;
    }

    Timestep t;
    t.metadata = metadata;
    t.baseName = ghoul::filesystem::File(path).baseName();
    t.inRam = false;
    t.onGpu = false;

    _volumeTimesteps[t.metadata.time] = std::move(t);
}

RenderableTimeVaryingVolume::Timestep* RenderableTimeVaryingVolume::currentTimestep() {
    if (_volumeTimesteps.empty()) {
        return nullptr;
    }
    double currentTime = global::timeManager.time().j2000Seconds();

    // Get the first item with time > currentTime
    auto currentTimestepIt = _volumeTimesteps.upper_bound(currentTime);
    if (currentTimestepIt == _volumeTimesteps.end()) {
        // No such timestep was found: show last timestep if it is within the time margin.
        Timestep* lastTimestep = &(_volumeTimesteps.rbegin()->second);
        double threshold = lastTimestep->metadata.time +
            static_cast<double>(_secondsAfter);
        return currentTime < threshold ? lastTimestep : nullptr;
    }

    if (currentTimestepIt == _volumeTimesteps.begin()) {
        // No such timestep was found: show first timestep if it is within the time margin
        Timestep* firstTimestep = &(_volumeTimesteps.begin()->second);
        double threshold = firstTimestep->metadata.time -
            static_cast<double>(_secondsBefore);
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
        ++index;
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
        ++index;
    }
    return nullptr;
}

void RenderableTimeVaryingVolume::jumpToTimestep(int target) {
    Timestep* t = timestepFromIndex(target);
    if (t) {
        global::timeManager.setTimeNextFrame(t->metadata.time);
    }
}

void RenderableTimeVaryingVolume::update(const UpdateData&) {
    if (_raycaster) {
        Timestep* t = currentTimestep();
        _currentTimestep = timestepIndex(t);

        // Set scale and translation matrices:
        // The original data cube is a unit cube centered in 0
        // ie with lower bound from (-0.5, -0.5, -0.5) and upper bound (0.5, 0.5, 0.5)
        if (t && t->texture) {
            if (_raycaster->gridType() == volume::VolumeGridType::Cartesian) {
                glm::dvec3 scale = t->metadata.upperDomainBound -
                    t->metadata.lowerDomainBound;
                glm::dvec3 translation =
                    (t->metadata.lowerDomainBound + t->metadata.upperDomainBound) * 0.5f;

                glm::dmat4 modelTransform = glm::translate(glm::dmat4(1.0), translation);
                glm::dmat4 scaleMatrix = glm::scale(glm::dmat4(1.0), scale);
                modelTransform = modelTransform * scaleMatrix;
                _raycaster->setModelTransform(glm::mat4(modelTransform));
            } else {
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
            //_transferFunctionHandler->setUnit(t->metadata.valueUnit);
            //_transferFunctionHandler->setMinAndMaxValue(
            //    t->metadata.minValue, t->metadata.maxValue);

            //_transferFunctionHandler->setHistogramProperty(t->histogram);
        } else {
            _raycaster->setVolumeTexture(nullptr);
        }
        _raycaster->setStepSize(_stepSize);
        _raycaster->setOpacity(_opacity);
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
        global::raycasterManager.detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
}

} // namespace openspace::volume
