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
    const char* KeyVariable = "Variable";
    const char* KeyLowerDomainBound = "LowerDomainBound";
    const char* KeyUpperDomainBound = "UpperDomainBound";
    const char* KeyLowerValueBound = "LowerValueBound";
    const char* KeyUpperValueBound = "UpperValueBound";
    const char* KeyClipPlanes = "ClipPlanes";
    const char* KeyCache = "Cache";
    const char* KeyGridType = "GridType";
    const char* ValueSphericalGridType = "Spherical";
    const char* KeyMinValue = "MinValue";
    const char* KeyMaxValue = "MaxValue";
    const char* KeyTime = "Time";
}

namespace openspace {
namespace volume {

RenderableTimeVaryingVolume::RenderableTimeVaryingVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _clipPlanes(nullptr)
    , _stepSize("stepSize", "Step Size", 0.02, 0.01, 1)
    , _sourceDirectory("sourceDirectory", "Source Directory")
    , _transferFunctionPath("transferFunctionPath", "Transfer Function Path")
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
    _gridType = VolumeGridType::Cartesian;

    if (dictionary.hasValue<std::string>(KeyGridType)) {
        _gridType = volume::parseGridType(dictionary.value<std::string>(KeyGridType));
    }
}
    
RenderableTimeVaryingVolume::~RenderableTimeVaryingVolume() {}

bool RenderableTimeVaryingVolume::initialize() {

    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(_sourceDirectory, RawPath::Yes);

    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load sequence directory '" << sequenceDir.path() << "'");
        return false;
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

        // TODO: remap values from [lowerValueBounds, upperDomainBounds[ to [0, 1[

        t.texture = std::make_shared<ghoul::opengl::Texture>(
            t.dimensions,
            ghoul::opengl::Texture::Format::Red,
            GL_RED,
            GL_FLOAT,
            ghoul::opengl::Texture::FilterMode::Linear,
            ghoul::opengl::Texture::WrappingMode::Clamp
        );

        void* data = reinterpret_cast<void*>(t.rawVolume->data());
        t.texture->setPixelData(data, ghoul::opengl::Texture::TakeOwnership::No);
        t.texture->uploadTexture();
    }

    
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

    return true;
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
    t.upperDomainBound = dictionary.value<glm::vec3>(KeyLowerDomainBound);
    t.minValue = dictionary.value<float>(KeyMinValue);
    t.maxValue = dictionary.value<float>(KeyMaxValue);
    t.time = dictionary.value<double>(KeyTime);
    t.inRam = false;
    t.onGpu = false;
    
    _volumeTimesteps[t.time] = std::move(t);
}

RenderableTimeVaryingVolume::Timestep* RenderableTimeVaryingVolume::currentTimestep() {
    return &(_volumeTimesteps.begin()->second);
}

void RenderableTimeVaryingVolume::update(const UpdateData& data) {
    if (_raycaster) {
        Timestep* t = currentTimestep();
        if (t && t->texture) {
            _raycaster->setVolumeTexture(t->texture);
        }
        _raycaster->setStepSize(_stepSize);
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


bool RenderableTimeVaryingVolume::deinitialize() {
    if (_raycaster) {
        OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
    return true;
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
                "Specifies the path to load timesteps from",
                Optional::No
            },
            {
                KeyTransferFunction,
                new StringVerifier,
                "Specifies the transfer function file path",
                Optional::No
            },
            {
                KeyLowerValueBound,
                new DoubleVerifier,
                "Specifies the lower value bound."
                "This number will be mapped to 0 before uploadin to the GPU.",
                Optional::No
            },
            {
                KeyUpperValueBound,
                new DoubleVerifier,
                "Specifies the lower value bound."
                "This number will be mapped to 0 before uploadin to the GPU.",
                Optional::No
            },
            {
                KeyGridType,
                new StringInListVerifier({"Cartesian", "Spherical"}),
                "Specifies the grid type",
                Optional::Yes
            }
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
                "Specifies the lower domain bounds in the model coordinate system",
                Optional::No
            },
            {
                KeyUpperDomainBound,
                new Vector3Verifier<float>,
                "Specifies the upper domain bounds in the model coordinate system",
                Optional::No
            },
            {
                KeyDimensions,
                new Vector3Verifier<float>,
                "Specifies the number of grid cells in each dimension",
                Optional::No
            },
            {
                KeyTime,
                new DoubleVerifier,
                "Specifies the time (seconds since epoch)",
                Optional::No
            },
            {
                KeyMinValue,
                new DoubleVerifier,
                "Specifies the minimum value stored in the volume"
            },
            {
                KeyMaxValue,
                new DoubleVerifier,
                "Specifies the maximum value stored in the volume"
            }
        }
    };
}

} // namespace volume
} // namespace openspace
