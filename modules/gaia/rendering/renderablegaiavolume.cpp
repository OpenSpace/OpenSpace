/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/gaia/rendering/renderablegaiavolume.h>

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
#include <modules/gaia/tasks/generateGaiaVolumeTask.h>
#include <ghoul/fmt.h>


namespace {
    constexpr std::string_view _loggerCat = "RenderableGaiaVolume";

    const float SecondsInOneDay = 60 * 60 * 24;

    constexpr openspace::properties::Property::PropertyInfo XAxisInfo = {
        "XAxis",
        "X-axis",
        "Specify column data to use for the transfer function x-axis."
    };

    constexpr openspace::properties::Property::PropertyInfo YAxisInfo = {
        "YAxis",
        "Y-axis",
        "Specify column data to use for the transfer function y-axis."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderValueByInfo = {
        "RenderValueBy",
        "Render value by",
        "Specifiy which value to render by, e.g., average, minimum, maximum."
    };

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

    struct [[codegen::Dictionary(RenderableGaiaVolume)]] Parameters {
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

        std::optional<std::string> xAxis;

        std::optional<std::string> yAxis;

        std::optional<int> textureDimension;
    };
#include "renderablegaiavolume_codegen.cpp"

} // namespace

namespace openspace::volume {

documentation::Documentation RenderableGaiaVolume::Documentation() {
    return codegen::doc<Parameters>("volume_renderable_gaiavolume");
}

RenderableGaiaVolume::RenderableGaiaVolume(
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
    , _xAxis(XAxisInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _yAxis(YAxisInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _renderValueBy(RenderValueByInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    
    _sourceDirectory = absPath(p.sourceDirectory).string();
    _transferFunctionPath = absPath(p.transferFunction).string();
    const unsigned textureDim = static_cast<unsigned>(p.textureDimension.value_or(1));
    _transferFunction = std::make_shared<openspace::TransferFunction>(
        _transferFunctionPath,
        [](const openspace::TransferFunction&) {},
        textureDim
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

    ghoul::Dictionary clipPlanesDictionary = p.clipPlanes.value_or(ghoul::Dictionary());
    _clipPlanes = std::make_shared<volume::VolumeClipPlanes>(clipPlanesDictionary);
    _clipPlanes->setIdentifier("clipPlanes");
    _clipPlanes->setGuiName("Clip Planes");

    if (p.gridType.has_value()) {
        VolumeGridType gridType = volume::parseGridType(*p.gridType);
        _gridType = static_cast<std::underlying_type_t<VolumeGridType>>(gridType);
    }

    _renderValueBy.addOptions({
        {static_cast<int>(gaiavolume::VolumeRenderMode::Average), "Average"},
        {static_cast<int>(gaiavolume::VolumeRenderMode::Minimum), "Minimum"},
        {static_cast<int>(gaiavolume::VolumeRenderMode::Maximum), "Maximum"}
        });
    _renderValueBy = static_cast<int>(gaiavolume::VolumeRenderMode::Average);
    _renderValueBy.onChange([this]() { updateRenderVoxelData(); });


    _xAxisStartValue = p.xAxis;
    _yAxisStartValue = p.yAxis;

    addProperty(_brightness);
    addProperty(Fadeable::_opacity);
    addProperty(_renderValueBy);
}

RenderableGaiaVolume::~RenderableGaiaVolume() {}

void RenderableGaiaVolume::initializeGL() {
    std::filesystem::path sequenceDir = absPath(_sourceDirectory);

    if (!std::filesystem::is_directory(sequenceDir)) {
        LERROR(fmt::format("Could not load sequence directory {}", sequenceDir));
        return;
    }

    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::recursive_directory_iterator(sequenceDir)) {
        if (e.is_regular_file() && e.path().extension() == ".dictionary") {
            loadTimestepMetadata(e.path().string());
        }
    }

    // TODO: defer loading of data to later (separate thread or at least not when loading)
    for (std::pair<const double, Timestep>& p : _volumeTimesteps) {
        Timestep& t = p.second;
        std::string path = fmt::format(
            "{}/{}.rawvolume", _sourceDirectory.value(), t.baseName
        );
        //Create reader and read raw volume
        RawVolumeReader<gaiavolume::GaiaVolumeDataLayout> reader(path, t.metadata.dimensions);
        t.rawVolume = reader.read(_invertDataAtZ, t.metadata.fileheaders.size());

        gaiavolume::GaiaVolumeDataLayout* data = t.rawVolume->data();

        std::map<std::string, int> const &columns = t.metadata.fileheaders;

        //Add all the options in the same order as they appear in the file. Kind of fugly but
        //it makes it more understandable in the gui if they follow the same order(?)..
        for (size_t index{ 0 }; index < t.metadata.fileheaders.size(); index++) {
            //Return the item with matching index.
            auto header = std::find_if(t.metadata.fileheaders.begin(), t.metadata.fileheaders.end(),
                [&index](std::pair<std::string, int> const& p) {
                    return index == p.second;
                });
            _xAxis.addOption(index, header->first);
            _yAxis.addOption(index, header->first);
        }

        //TODO: this is kind of fugly that we set the x and y axis in each iteration
        //but it works for now because gaia volume will probably only use 1 timestep.
        if (_xAxisStartValue.has_value()) {
            _xAxis.set(columns.at(_xAxisStartValue.value()));
        }
        else {
            _xAxis.set(0);
        }
        if (_yAxisStartValue.has_value()) {
            _yAxis.set(columns.at(_yAxisStartValue.value()));
        }
        else {
            _yAxis.set(0);
        }
        
        updateRenderVoxelData(&t);
    }

    _clipPlanes->initialize();


    // Temporary to make the 1D texture volumes work while doing the InfraViz project
    bool useCustomRayCaster = false;
    if (useCustomRayCaster) {
        _raycaster = std::make_unique<volume::BasicVolumeRaycaster>(
            nullptr,
            _transferFunction,
            _clipPlanes,
            "${MODULE_GAIA}/shaders/gaiaraycast.glsl"
        );
    }
    else {
        _raycaster = std::make_unique<volume::BasicVolumeRaycaster>(
            nullptr,
            _transferFunction,
            _clipPlanes
        );
    }

    _raycaster->initialize();
    global::raycasterManager->attachRaycaster(*_raycaster.get());
    onEnabledChange([this](bool enabled) {
        if (enabled) {
            global::raycasterManager->attachRaycaster(*_raycaster.get());
        }
        else {
            global::raycasterManager->detachRaycaster(*_raycaster.get());
        }
    });

    _triggerTimeJump.onChange([this]() { jumpToTimestep(_jumpToTimestep); });

    _jumpToTimestep.onChange([this]() { jumpToTimestep(_jumpToTimestep); });

    const int lastTimestep = !_volumeTimesteps.empty() ?
        static_cast<int>(_volumeTimesteps.size() - 1) :
        0;
    _jumpToTimestep.setMaxValue(lastTimestep);


    _xAxis.onChange([this]() { updateRenderVoxelData(); });
    _yAxis.onChange([this]() { updateRenderVoxelData(); });

    addProperty(_xAxis);
    addProperty(_yAxis);

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
            _transferFunctionPath
        );
        _raycaster->setTransferFunction(_transferFunction);
    });
}

void RenderableGaiaVolume::updateRenderVoxelData(Timestep* t) {
    //LDEBUG("X axis value changed to: " + std::to_string(_xAxis.value()));
    //LDEBUG("Y axis value changed to: " + std::to_string(_yAxis.value()));
    if (!t) {
        t = currentTimestep();
    }

    gaiavolume::GaiaVolumeDataLayout* data = t->rawVolume->data();
    int nanvoxels{ 0 };
    int filledvoxels{ 0 };

    //Create a vector contining the data for the transfer function
    std::vector<float> newdata;
    newdata.reserve(t->rawVolume->nCells());
    for (size_t i = 0; i < t->rawVolume->nCells(); i++) {
        if (data[i].containData()) {
            gaiavolume::VoxelDataLayout const& xVoxelData = data[i].data[_xAxis.value()];
            gaiavolume::VoxelDataLayout const& yVoxelData = data[i].data[_yAxis.value()];

            //If either of the values are nan we 'ignore' it in the same way as if it was empty.
            if (xVoxelData.isNaNData || yVoxelData.isNaNData) {
                newdata.push_back(0.0f);
                newdata.push_back(0.0f);
                ++nanvoxels;
            }
            else {
                //Get the data depending on what render mode used.
                float xValue = getVoxelData(xVoxelData);
                float yValue = getVoxelData(yVoxelData);

                newdata.push_back(xValue);
                newdata.push_back(yValue);
            }
            ++filledvoxels;
        }
        //Voxel does not contain any data, for now use 0,0 as coordinates. TODO: See if there is another
        //That does not involve putting 'fake' data, (0,0) has a meaning in the transfer function.. 
        else {
            newdata.push_back(0.0f);
            newdata.push_back(0.0f);
        }
    }
    LDEBUG(fmt::format(
        "Number of nan values {} out of {}: {}%", nanvoxels, filledvoxels, static_cast<double>(nanvoxels) / filledvoxels * 100.0
    ));
    //Create texture if it does not exist
    if (!t->texture) {
        t->texture = std::make_shared<ghoul::opengl::Texture>(
            t->metadata.dimensions,
            GL_TEXTURE_3D,
            ghoul::opengl::Texture::Format::RG,
            GL_RG,
            GL_FLOAT,
            ghoul::opengl::Texture::FilterMode::Linear,
            ghoul::opengl::Texture::WrappingMode::Clamp
        );
    }

    t->texture->setPixelData(
        reinterpret_cast<void*>(newdata.data()),
        ghoul::opengl::Texture::TakeOwnership::No
    );
    t->texture->uploadTexture();
}

float RenderableGaiaVolume::getVoxelData(gaiavolume::VoxelDataLayout const& data) const {
    switch (_renderValueBy)
    {
    case gaiavolume::VolumeRenderMode::Average:
        return data.avgData;
    case gaiavolume::VolumeRenderMode::Minimum:
        return data.minData;
    case gaiavolume::VolumeRenderMode::Maximum:
        return data.maxData;
    }
}

void RenderableGaiaVolume::loadTimestepMetadata(const std::string& path) {
    RawVolumeMetadata metadata;

    try {
        ghoul::Dictionary dictionary = ghoul::lua::loadDictionaryFromFile(path);
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
    t.baseName = std::filesystem::path(path).stem().string();
    t.inRam = false;
    t.onGpu = false;

    _volumeTimesteps[t.metadata.time] = std::move(t);
}

RenderableGaiaVolume::Timestep* RenderableGaiaVolume::currentTimestep() {
    if (_volumeTimesteps.empty()) {
        return nullptr;
    }
    double currentTime = global::timeManager->time().j2000Seconds();

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

int RenderableGaiaVolume::timestepIndex(
                                     const RenderableGaiaVolume::Timestep* t) const
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
RenderableGaiaVolume::Timestep* RenderableGaiaVolume::timestepFromIndex(
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

void RenderableGaiaVolume::jumpToTimestep(int target) {
    Timestep* t = timestepFromIndex(target);
    if (t) {
        global::timeManager->setTimeNextFrame(Time(t->metadata.time));
    }
}

void RenderableGaiaVolume::update(const UpdateData&) {
    _transferFunction->update();

    if (_raycaster) {
        Timestep* t = currentTimestep();

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

void RenderableGaiaVolume::render(const RenderData& data, RendererTasks& tasks) {
    if (_raycaster && _raycaster->volumeTexture()) {
        tasks.raycasterTasks.push_back({ _raycaster.get(), data });
    }
}

bool RenderableGaiaVolume::isReady() const {
    return true;
}

void RenderableGaiaVolume::deinitializeGL() {
    if (_raycaster) {
        global::raycasterManager->detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
}

} // namespace openspace::volume
