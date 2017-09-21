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

#include <modules/volume/rendering/renderabletimevaryingvolume.h>
#include <modules/volume/rawvolumereader.h>
#include <modules/volume/rawvolume.h>

#include <openspace/rendering/renderable.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/time.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>

#include <glm/gtc/matrix_transform.hpp>

namespace {
    const char* _loggerCat = "RenderableTimeVaryingVolume";
}

namespace {
    const char* KeyDimensions = "Dimensions";
    const char* KeyStepSize = "StepSize";
    const char* KeyTransferFunction = "TransferFunction";
    const char* KeySourceDirectory = "SourceDirectory";
    const char* KeyLowerDomainBound = "LowerDomainBound";
    const char* KeyUpperDomainBound = "UpperDomainBound";
    const char* KeyLowerValueBound = "LowerValueBound";
    const char* KeyUpperValueBound = "UpperValueBound";
    const char* KeyClipPlanes = "ClipPlanes";
    const char* KeySecondsBefore = "SecondsBefore";
    const char* KeySecondsAfter = "SecondsAfter";
    const char* KeyGridType = "GridType";
    const char* KeyMinValue = "MinValue";
    const char* KeyMaxValue = "MaxValue";
    const char* KeyTime = "Time";
    const float SecondsInOneDay = 60 * 60 * 24;
}

namespace openspace {
namespace volume {

RenderableTimeVaryingVolume::RenderableTimeVaryingVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _clipPlanes(nullptr)
    , _stepSize({ "stepSize", "Step Size", "" }, 0.02, 0.01, 1)
    , _gridType({ "gridType", "Grid Type", "" }, properties::OptionProperty::DisplayType::Dropdown)
    , _secondsBefore({ "secondsBefore", "Seconds before", "" }, 0.0, 0.01, SecondsInOneDay)
    , _secondsAfter({ "secondsAfter", "Seconds after", "" }, 0.0, 0.01, SecondsInOneDay)
    , _sourceDirectory({ "sourceDirectory", "Source Directory", "" })
    , _transferFunctionPath({"transferFunctionPath", "Transfer Function Path", "" })
    , _triggerTimeJump({"triggerTimeJump", "Jump", "" })
    , _jumpToTimestep({"jumpToTimestep", "Jump to timestep", "" }, 0, 0, 256)
    , _currentTimestep({"currentTimestep", "Current timestep", "" }, 0, 0, 256)
    , _opacity({"opacity", "Opacity", "" }, 10.0f, 0.0f, 50.0f)
    , _rNormalization({"rNormalization", "Radius normalization", "" }, 0.0f, 0.0f, 2.0f)
    , _rUpperBound({"rUpperBound", "Radius upper bound", "" }, 1.0f, 0.0f, 2.0f)
    , _lowerValueBound({"lowerValueBound", "Lower value bound", "" }, 0.0f, 0.0f, 1000000.0f)
    , _upperValueBound({"upperValueBound", "Upper value bound", "" }, 0.0f, 0.0f, 1000000.0f)
    , _raycaster(nullptr)
    , _transferFunction(nullptr)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableTimeVaryingVolume"
    );

    _sourceDirectory = absPath(dictionary.value<std::string>(KeySourceDirectory));
    _transferFunctionPath = absPath(dictionary.value<std::string>(KeyTransferFunction));
    _lowerValueBound = dictionary.value<float>(KeyLowerValueBound);
    _upperValueBound = dictionary.value<float>(KeyUpperValueBound);
    _transferFunction = std::make_shared<TransferFunction>(_transferFunctionPath);

    _gridType.addOption(static_cast<int>(volume::VolumeGridType::Cartesian), "Cartesian grid");
    _gridType.addOption(static_cast<int>(volume::VolumeGridType::Spherical), "Spherical grid");
    _gridType.setValue(static_cast<int>(volume::VolumeGridType::Cartesian));


    if (dictionary.hasValue<float>(KeySecondsBefore)) {
        _secondsBefore = dictionary.value<float>(KeySecondsBefore);
    }
    _secondsAfter = dictionary.value<float>(KeySecondsAfter);

    ghoul::Dictionary clipPlanesDictionary;
    dictionary.getValue(KeyClipPlanes, clipPlanesDictionary);
    _clipPlanes = std::make_shared<volume::VolumeClipPlanes>(clipPlanesDictionary);
    _clipPlanes->setName("clipPlanes");

    if (dictionary.hasValue<std::string>(KeyGridType)) {
        VolumeGridType gridType = volume::parseGridType(dictionary.value<std::string>(KeyGridType));
        _gridType = (gridType == VolumeGridType::Spherical) ? 1 : 0;
    }
}
    
RenderableTimeVaryingVolume::~RenderableTimeVaryingVolume() {}

void RenderableTimeVaryingVolume::initialize() {

    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(_sourceDirectory, RawPath::Yes);

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load sequence directory '" << sequenceDir.path() << "'");
        return;
    }

    using Recursive = ghoul::filesystem::Directory::Recursive;
    using Sort = ghoul::filesystem::Directory::Sort;

    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::No);
    for (auto path : sequencePaths) {
        ghoul::filesystem::File currentFile(path);
        std::string extension = currentFile.fileExtension();
        if (extension == "dictionary") {
            loadTimestepMetadata(path);
        }
    }


    // TODO: defer loading of data to later. (separate thread or at least not when loading)
    for (auto& p : _volumeTimesteps) {
        Timestep& t = p.second;
        std::string path = FileSys.pathByAppendingComponent(_sourceDirectory, t.baseName) + ".rawvolume";
        RawVolumeReader<float> reader(path, t.dimensions);
        t.rawVolume = reader.read();

        float min = t.minValue;
        float diff = t.maxValue - t.minValue;

        float *data = t.rawVolume->data();
        for (size_t i = 0; i < t.rawVolume->nCells(); ++i) {
            data[i] = glm::clamp((data[i] - min) / diff, 0.0f, 1.0f);
        }
        // TODO: handle normalization properly for different timesteps + transfer function

        t.texture = std::make_shared<ghoul::opengl::Texture>(
            t.dimensions,
            ghoul::opengl::Texture::Format::Red,
            GL_RED,
            GL_FLOAT,
            ghoul::opengl::Texture::FilterMode::Linear,
            ghoul::opengl::Texture::WrappingMode::Clamp
        );

        t.texture->setPixelData(reinterpret_cast<void*>(data), ghoul::opengl::Texture::TakeOwnership::No);
        t.texture->uploadTexture();
    }

    _clipPlanes->initialize();
    _transferFunction->update();
    _raycaster = std::make_unique<volume::BasicVolumeRaycaster>(nullptr, _transferFunction, _clipPlanes);
    _raycaster->initialize();
    OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());
    auto onChange = [&](bool enabled) {
        if (enabled) {
            OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());
        } else {
            OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        }
    };
    onEnabledChange(onChange);

    _triggerTimeJump.onChange([this] () {
        jumpToTimestep(_jumpToTimestep);
    });

    _jumpToTimestep.onChange([this] () {
        jumpToTimestep(_jumpToTimestep);
    });

    const int lastTimestep = (_volumeTimesteps.size() > 0) ? (_volumeTimesteps.size() - 1) : 0;
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
    addProperty(_lowerValueBound);
    addProperty(_upperValueBound);

    _raycaster->setGridType((_gridType.value() == 1) ? VolumeGridType::Spherical : VolumeGridType::Cartesian);
    _gridType.onChange([this] {
        _raycaster->setGridType((_gridType.value() == 1) ? VolumeGridType::Spherical : VolumeGridType::Cartesian);
    });
}

void RenderableTimeVaryingVolume::loadTimestepMetadata(const std::string& path) {
    ghoul::Dictionary dictionary = ghoul::lua::loadDictionaryFromFile(path);
    try {
        documentation::testSpecificationAndThrow(TimestepDocumentation(), dictionary, "TimeVaryingVolumeTimestep");
    } catch (const documentation::SpecificationError& e) {
        LERROR(e.message << e.component);
        return;
    }

    Timestep t;
    t.baseName = ghoul::filesystem::File(path).baseName();
    t.dimensions = dictionary.value<glm::vec3>(KeyDimensions);
    t.lowerDomainBound = dictionary.value<glm::vec3>(KeyLowerDomainBound);
    t.upperDomainBound = dictionary.value<glm::vec3>(KeyUpperDomainBound);
    t.minValue = dictionary.value<float>(KeyMinValue);
    t.maxValue = dictionary.value<float>(KeyMaxValue);

    std::string timeString = dictionary.value<std::string>(KeyTime);
    t.time = Time::convertTime(timeString);
    t.inRam = false;
    t.onGpu = false;
    
    _volumeTimesteps[t.time] = std::move(t);
}

RenderableTimeVaryingVolume::Timestep* RenderableTimeVaryingVolume::currentTimestep() {
    if (_volumeTimesteps.size() == 0) {
        return nullptr;
    }
    double currentTime = OsEng.timeManager().time().j2000Seconds();

    // Get the first item with time > currentTime
    auto currentTimestepIt = _volumeTimesteps.upper_bound(currentTime);
    if (currentTimestepIt == _volumeTimesteps.end()) {
        // No such timestep was found: show last timestep if it is within the time margin.
        RenderableTimeVaryingVolume::Timestep* lastTimestep = &(_volumeTimesteps.rbegin()->second);
        double threshold = lastTimestep->time + static_cast<double>(_secondsAfter);
        return currentTime < threshold ? lastTimestep : nullptr;
    }

    if (currentTimestepIt == _volumeTimesteps.begin()) {
        // No such timestep was found: show first timestep if it is within the time margin.
        RenderableTimeVaryingVolume::Timestep* firstTimestep = &(_volumeTimesteps.begin()->second);
        double threshold = firstTimestep->time - static_cast<double>(_secondsBefore);
        return currentTime >= threshold ? firstTimestep : nullptr;
    }

    // Get the last item with time <= currentTime
    currentTimestepIt--;
    return &(currentTimestepIt->second);
}

int RenderableTimeVaryingVolume::timestepIndex(const RenderableTimeVaryingVolume::Timestep* t) const {
    if (!t) {
        return -1;
    }
    int index = 0;
    for (auto& it : _volumeTimesteps) {
        if (&(it.second) == t) {
            return index;
        }
        ++index;
    }
    return -1;
}

RenderableTimeVaryingVolume::Timestep* RenderableTimeVaryingVolume::timestepFromIndex(int target) {
    if (target < 0) target = 0;
    int index = 0;
    for (auto& it : _volumeTimesteps) {
        if (index == target) {
            return &(it.second);
        }
        ++index;
    }
    return nullptr;
}

void RenderableTimeVaryingVolume::jumpToTimestep(int target) {
    Timestep* t = timestepFromIndex(target);
    if (!t) {
        return;
    }
    OsEng.timeManager().setTimeNextFrame(t->time);
}

void RenderableTimeVaryingVolume::update(const UpdateData& data) {
    if (_raycaster) {
        Timestep* t = currentTimestep();
        _currentTimestep = timestepIndex(t);
        if (t && t->texture) {
            if (_raycaster->gridType() == volume::VolumeGridType::Cartesian) {
                glm::dvec3 scale = t->upperDomainBound - t->lowerDomainBound;
                glm::dvec3 translation = (t->lowerDomainBound + t->upperDomainBound) * 0.5f;

                glm::dmat4 modelTransform = glm::translate(glm::dmat4(1.0), translation);
                glm::dmat4 scaleMatrix = glm::scale(glm::dmat4(1.0), scale);
                modelTransform = modelTransform * scaleMatrix;
                _raycaster->setModelTransform(glm::mat4(modelTransform));
            } else {
                _raycaster->setModelTransform(
                    glm::scale(
                        glm::dmat4(1.0),
                        glm::dvec3(t->upperDomainBound[0])
                    )
                );
            }
            _raycaster->setVolumeTexture(t->texture);

            // Remap volume value to that TF value 0 is sampled for lowerValueBound, and 1 is sampled for upperLowerBound.
            // This means that volume values = 0 need to be remapped to how localMin relates to the global range.
            float zeroMap = (t->minValue - _lowerValueBound) / (_upperValueBound - _lowerValueBound);

            // Volume values = 1 are mapped to how localMax relates to the global range.
            float oneMap = (t->maxValue - _lowerValueBound) / (_upperValueBound - _lowerValueBound);
            _raycaster->setValueRemapping(zeroMap, oneMap);
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


void RenderableTimeVaryingVolume::deinitialize() {
    if (_raycaster) {
        OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
}

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
                "Specifies the transfer function file path",
            },
            {
                KeyLowerValueBound,
                new DoubleVerifier,
                Optional::No,
                "Specifies the lower value bound."
                "This number will be mapped to 0 before uploadin to the GPU.",
            },
            {
                KeyUpperValueBound,
                new DoubleVerifier,
                Optional::No,
                "Specifies the lower value bound."
                "This number will be mapped to 0 before uploadin to the GPU."
            },
            {
                KeyGridType,
                new StringInListVerifier({"Cartesian", "Spherical"}),
                Optional::Yes,
                "Specifies the grid type"
            },
            {
                KeySecondsBefore,
                new DoubleVerifier,
                Optional::Yes,
                "Specifies the number of seconds to show the the first timestep before its actual time."
                "The default value is 0.",
            },
            {
                KeySecondsAfter,
                new DoubleVerifier,
                Optional::No,
                "Specifies the number of seconds to show the the last timestep after its actual time",
            },
        }
    };
}


documentation::Documentation RenderableTimeVaryingVolume::TimestepDocumentation() {
    using namespace documentation;
    return {
        "TimevaryingVolumeTimestep",
        "volume_timevaryingvolumetimestep",
        {
            {
                KeyLowerDomainBound,
                new Vector3Verifier<float>,
                Optional::No,
                "Specifies the lower domain bounds in the model coordinate system",
            },
            {
                KeyUpperDomainBound,
                new Vector3Verifier<float>,
                Optional::No,
                "Specifies the upper domain bounds in the model coordinate system",
            },
            {
                KeyDimensions,
                new Vector3Verifier<float>,
                Optional::No,
                "Specifies the number of grid cells in each dimension",
            },
            {
                KeyTime,
                new StringVerifier,
                Optional::No,
                "Specifies the time on the format YYYY-MM-DDTHH:MM:SS.000Z",
            },
            {
                KeyMinValue,
                new DoubleVerifier,
                Optional::No,
                "Specifies the minimum value stored in the volume"
            },
            {
                KeyMaxValue,
                new DoubleVerifier,
                Optional::No,
                "Specifies the maximum value stored in the volume"
            }
        }
    };
}

} // namespace volume
} // namespace openspace
