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

#include <modules/multiresvolume/rendering/renderablemultiresvolume.h>

#include <modules/kameleon/include/kameleonwrapper.h>
#include <modules/multiresvolume/rendering/atlasmanager.h>
#include <modules/multiresvolume/rendering/errorhistogrammanager.h>
#include <modules/multiresvolume/rendering/histogrammanager.h>
#include <modules/multiresvolume/rendering/localerrorhistogrammanager.h>
#include <modules/multiresvolume/rendering/localtfbrickselector.h>
#include <modules/multiresvolume/rendering/multiresvolumeraycaster.h>
#include <modules/multiresvolume/rendering/shenbrickselector.h>
#include <modules/multiresvolume/rendering/simpletfbrickselector.h>
#include <modules/multiresvolume/rendering/tfbrickselector.h>
#include <modules/multiresvolume/rendering/tsp.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <algorithm>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iterator>

namespace {
    constexpr std::string_view _loggerCat = "RenderableMultiresVolume";

    constexpr openspace::properties::Property::PropertyInfo StepSizeCoefficientInfo = {
        "StepSizeCoefficient",
        "Stepsize Coefficient",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo CurrentTimeInfo = {
        "CurrentTime",
        "Current Time",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MemoryBudgetInfo = {
        "MemoryBudget",
        "Memory Budget",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StreamingBudgetInfo = {
        "StreamingBudget",
        "Streaming Budget",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseGlobalTimeInfo = {
        "UseGlobalTime",
        "Global Time",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LoopInfo = {
        "Loop",
        "Loop",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SelectorNameInfo = {
        "Selector",
        "Brick Selector",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StatsToFileInfo = {
        "PrintStats",
        "Print Stats",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo StatsToFileNameInfo = {
        "PrintStatsFileName",
        "Stats Filename",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo ScalingExponentInfo = {
        "ScalingExponent",
        "Scaling Exponent",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ScalingInfo = {
        "Scaling",
        "Scaling",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TranslationInfo = {
        "Translation",
        "Translation",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo RotationInfo = {
        "Rotation",
        "Euler rotation",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };

    struct [[codegen::Dictionary(RenderableMultiresVolume)]] Parameters {
        std::filesystem::path source;
        std::optional<std::filesystem::path> errorHistogramsSource;
        std::optional<int> scalingExponent;
        std::optional<float> stepSizeCoefficient;
        std::optional<glm::vec3> scaling;
        std::optional<glm::vec3> translation;
        std::optional<glm::vec3> rotation;

        std::optional<std::string> startTime;
        std::optional<std::string> endTime;

        std::filesystem::path transferFunction;

        std::optional<std::string> brickSelector;
    };
#include "renderablemultiresvolume_codegen.cpp"
} // namespace

namespace openspace {

RenderableMultiresVolume::RenderableMultiresVolume(const ghoul::Dictionary& dictionary)
    :  Renderable(dictionary)
    , _useGlobalTime(UseGlobalTimeInfo, false)
    , _loop(LoopInfo, false)
    , _currentTime(CurrentTimeInfo, 0, 0, 0)
    , _memoryBudget(MemoryBudgetInfo, 0, 0, 0)
    , _streamingBudget(StreamingBudgetInfo, 0, 0, 0)
    , _stepSizeCoefficient(StepSizeCoefficientInfo, 1.f, 0.01f, 10.f)
    , _selectorName(SelectorNameInfo, "tf")
    , _statsToFile(StatsToFileInfo, false)
    , _statsToFileName(StatsToFileNameInfo)
    , _scalingExponent(ScalingExponentInfo, 1, -10, 20)
    , _translation(TranslationInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(10.f))
    , _rotation(
        RotationInfo,
        glm::vec3(0.f),
        glm::vec3(0.f),
        glm::vec3(glm::two_pi<float>())
    )
    , _scaling(ScalingInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(10.f))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _filename = p.source;
    _errorHistogramsPath = p.errorHistogramsSource.value_or(_errorHistogramsPath);
    _scalingExponent = p.scalingExponent.value_or(_scalingExponent);
    _stepSizeCoefficient = p.stepSizeCoefficient.value_or(_stepSizeCoefficient);
    _scaling = p.scaling.value_or(_scaling);
    _translation = p.translation.value_or(_translation);
    _rotation = p.rotation.value_or(_rotation);

    if (p.startTime.has_value() && p.endTime.has_value()) {
        _startTime = SpiceManager::ref().ephemerisTimeFromDate(*p.startTime);
        _endTime = SpiceManager::ref().ephemerisTimeFromDate(*p.endTime);
        _loop = false;
    }
    else {
        _loop = true;
        LWARNING("Node does not provide time information. Viewing one image / frame");
    }

    _transferFunctionPath = p.transferFunction;
    _transferFunction = std::make_shared<TransferFunction>(_transferFunctionPath);

    _tsp = std::make_shared<TSP>(_filename);
    _atlasManager = std::make_shared<AtlasManager>(_tsp.get());

    _selectorName = p.brickSelector.value_or(_selectorName);

    std::string selectorName = _selectorName;
    if (selectorName == "simple") {
        _selector = Selector::SIMPLE;
    }
    else if (selectorName == "local") {
        _selector = Selector::LOCAL;
    }
    else {
        _selector = Selector::TF;
    }

    addProperty(_selectorName);
    _selectorName.onChange([this]() {
        Selector s;
        std::string newSelectorName = _selectorName;
        if (newSelectorName == "simple") {
            s = Selector::SIMPLE;
        }
        else if (newSelectorName == "local") {
            s = Selector::LOCAL;
        }
        else if (newSelectorName == "tf") {
            s = Selector::TF;
        }
        else {
            return;
        }
        setSelectorType(s);
    });

    addProperty(_stepSizeCoefficient);
    addProperty(_useGlobalTime);
    addProperty(_loop);
    addProperty(_statsToFile);
    addProperty(_statsToFileName);
    addProperty(_scaling);
    addProperty(_scalingExponent);
    addProperty(_translation);
    addProperty(_rotation);
}

RenderableMultiresVolume::~RenderableMultiresVolume() {}

void RenderableMultiresVolume::setSelectorType(Selector selector) {
    // @TODO(abock): Can these if statements be simplified by checking if
    //               selector == _selector before and bailing out early?
    _selector = selector;
    switch (_selector) {
        case Selector::TF:
            if (!_tfBrickSelector) {
                _errorHistogramManager = std::make_unique<ErrorHistogramManager>(
                    _tsp.get()
                );
                _tfBrickSelector = std::make_unique<TfBrickSelector>(
                    _tsp.get(),
                    _errorHistogramManager.get(),
                    _transferFunction.get(),
                    _memoryBudget,
                    _streamingBudget
                );
                _transferFunction->setCallback([this](const TransferFunction&) {
                    _tfBrickSelector->calculateBrickErrors();
                });
                if (initializeSelector()) {
                    _tfBrickSelector->calculateBrickErrors();
                    return;
                }
            }
            break;

        case Selector::SIMPLE:
            if (!_simpleTfBrickSelector) {
                _histogramManager = std::make_unique<HistogramManager>();
                _simpleTfBrickSelector = std::make_unique<SimpleTfBrickSelector>(
                    _tsp.get(),
                    _histogramManager.get(),
                    _transferFunction.get(),
                    _memoryBudget,
                    _streamingBudget
                );
                _transferFunction->setCallback([this](const TransferFunction&) {
                    _simpleTfBrickSelector->calculateBrickImportances();
                });
                if (initializeSelector()) {
                    _simpleTfBrickSelector->calculateBrickImportances();
                    return;
                }
            }
            break;

        case Selector::LOCAL:
            if (!_localTfBrickSelector) {
                _localErrorHistogramManager =
                    std::make_unique<LocalErrorHistogramManager>(_tsp.get());
                _localTfBrickSelector = std::make_unique<LocalTfBrickSelector>(
                    _tsp.get(),
                    _localErrorHistogramManager.get(),
                    _transferFunction.get(),
                    _memoryBudget,
                    _streamingBudget
                );
                _transferFunction->setCallback([this](const TransferFunction&) {
                    _localTfBrickSelector->calculateBrickErrors();
                });
                if (initializeSelector()) {
                    _localTfBrickSelector->calculateBrickErrors();
                    return;
                }
            }
            break;
    }
}

void RenderableMultiresVolume::initializeGL() {
    bool success = _tsp && _tsp->load();

    unsigned int maxNumBricks = _tsp->header().xNumBricks * _tsp->header().yNumBricks *
                                _tsp->header().zNumBricks;

    constexpr unsigned int MaxInitialBudget = 2048;
    int initialBudget = std::min(MaxInitialBudget, maxNumBricks);

    _currentTime = properties::IntProperty(
        CurrentTimeInfo,
        0,
        0,
        _tsp->header().numTimesteps - 1
    );
    _memoryBudget = properties::IntProperty(
        MemoryBudgetInfo,
        initialBudget,
        0,
        maxNumBricks
    );
    _streamingBudget = properties::IntProperty(
        StreamingBudgetInfo,
        initialBudget,
        0,
        maxNumBricks
    );
    addProperty(_currentTime);
    addProperty(_memoryBudget);
    addProperty(_streamingBudget);

    if (success) {
        _brickIndices.resize(maxNumBricks, 0);
        setSelectorType(_selector);
    }

    success &= _atlasManager && _atlasManager->initialize();

    _transferFunction->update();

    success &= isReady();

    _raycaster = std::make_unique<MultiresVolumeRaycaster>(
        _tsp,
        _atlasManager,
        _transferFunction
    );
    _raycaster->initialize();

    global::raycasterManager->attachRaycaster(*_raycaster);

    auto onChange = [this](bool enabled) {
        if (enabled) {
            global::raycasterManager->attachRaycaster(*_raycaster);
        }
        else {
            global::raycasterManager->detachRaycaster(*_raycaster);
        }
    };

    onEnabledChange(onChange);

    if (!success) {
        throw ghoul::RuntimeError("Error during initialization");
    }
}

void RenderableMultiresVolume::deinitializeGL() {
    _tsp = nullptr;
    _transferFunction = nullptr;
}

bool RenderableMultiresVolume::isReady() const {
    return true;
}

bool RenderableMultiresVolume::initializeSelector() {
    int nHistograms = 50;
    bool success = true;

    switch (_selector) {
        case Selector::TF:
            if (_errorHistogramManager) {
                 std::filesystem::path cached = FileSys.cacheManager()->cachedFilename(
                     std::format("{}_{}_errorHistograms", _filename.stem(), nHistograms),
                     ""
                );
                std::ifstream cacheFile(cached, std::ios::in | std::ios::binary);
                if (cacheFile.is_open()) {
                    // Read histograms from cache
                    cacheFile.close();
                    LINFO(
                        std::format("Loading histograms from cache '{}'", cached)
                    );
                    success &= _errorHistogramManager->loadFromFile(cached);
                }
                else if (!_errorHistogramsPath.empty()) {
                    // Read histograms from scene data
                    LINFO(std::format(
                        "Loading histograms from scene data '{}'", _errorHistogramsPath
                    ));
                    success &= _errorHistogramManager->loadFromFile(_errorHistogramsPath);
                }
                else {
                    // Build histograms from tsp file
                    LWARNING(std::format("Failed to open '{}'", cached));
                    success &= _errorHistogramManager->buildHistograms(nHistograms);
                    if (success) {
                        LINFO(std::format("Writing cache to '{}'", cached));
                        _errorHistogramManager->saveToFile(cached);
                    }
                }
                success &= _tfBrickSelector && _tfBrickSelector->initialize();
            }
            break;

        case Selector::SIMPLE:
            if (_histogramManager) {
                std::filesystem::path cached = FileSys.cacheManager()->cachedFilename(
                    std::format("{}_{}_histogram", _filename.stem(), nHistograms),
                    ""
                );
                std::ifstream cacheFile(cached, std::ios::in | std::ios::binary);
                if (cacheFile.is_open()) {
                    // Read histograms from cache.
                    cacheFile.close();
                    LINFO(std::format("Loading histograms from '{}'", cached));
                    success &= _histogramManager->loadFromFile(cached);
                }
                else {
                    // Build histograms from tsp file.
                    LWARNING(std::format("Failed to open '{}'", cached));
                    success &= _histogramManager->buildHistograms(
                        _tsp.get(),
                        nHistograms
                    );
                    if (success) {
                        LINFO(std::format("Writing cache to '{}'", cached));
                        _histogramManager->saveToFile(cached);
                    }
                }
                success &= _simpleTfBrickSelector && _simpleTfBrickSelector->initialize();
            }
            break;

        case Selector::LOCAL:
            if (_localErrorHistogramManager) {
                 std::filesystem::path cached = FileSys.cacheManager()->cachedFilename(
                    std::format(
                        "{}_{}_localErrorHistograms", _filename.stem(), nHistograms
                    ),
                    ""
                );
                std::ifstream cacheFile(cached, std::ios::in | std::ios::binary);
                if (cacheFile.is_open()) {
                    // Read histograms from cache.
                    cacheFile.close();
                    LINFO(std::format("Loading histograms from '{}'", cached));
                    success &= _localErrorHistogramManager->loadFromFile(cached);
                }
                else {
                    // Build histograms from tsp file.
                    LWARNING(std::format("Failed to open '{}'", cached));
                    success &= _localErrorHistogramManager->buildHistograms(nHistograms);
                    if (success) {
                        LINFO(std::format("Writing cache to '{}'", cached));
                        _localErrorHistogramManager->saveToFile(cached);
                    }
                }
                success &= _localTfBrickSelector && _localTfBrickSelector->initialize();
            }
            break;
    }

    return success;
}
/*
void RenderableMultiresVolume::preResolve(ghoul::opengl::ProgramObject* program) {
    RenderableVolume::preResolve(program);


    std::stringstream ss;
    ss << "opacity_" << getId();
    program->setUniform(ss.str(), visible ? 1.f : 0.f);

    ss.str(std::string());
    ss << "stepSizeCoefficient_" << getId();
    program->setUniform(ss.str(), _stepSizeCoefficient);

    ss.str(std::string());
    ss << "transferFunction_" << getId();
    program->setUniform(ss.str(), getTextureUnit(_transferFunction->texture()));

    ss.str(std::string());
    ss << "textureAtlas_" << getId();
    program->setUniform(ss.str(), getTextureUnit(_atlasManager->textureAtlas()));

    ss.str(std::string());
    ss << "atlasMapBlock_" << getId();
    program->setSsboBinding(ss.str(), getSsboBinding(_atlasManager->atlasMapBuffer()));

    ss.str(std::string());
    ss << "gridType" << getId();
    program->setUniform(ss.str(), static_cast<int>(_tsp->header().gridType));

    ss.str(std::string());
    ss << "maxNumBricksPerAxis_" << getId();
    program->setUniform(ss.str(), static_cast<unsigned int>(_tsp->header().xNumBricks));

    ss.str(std::string());
    ss << "paddedBrickDim_" << getId();
    program->setUniform(ss.str(), static_cast<unsigned int>(_tsp->paddedBrickDim()));

    ss.str(std::string());
    ss << "atlasSize_" << getId();
    glm::size3_t size = _atlasManager->textureSize();
    glm::ivec3 atlasSize(size.x, size.y, size.z);
    program->setUniform(ss.str(), atlasSize);

    _timestep++;
}
*/
/*
std::vector<ghoul::opengl::Texture*> RenderableMultiresVolume::getTextures() {
    std::vector<ghoul::opengl::Texture*> textures{
        _transferFunction->texture(),
        _atlasManager->textureAtlas()
    };
    return textures;
}

std::vector<unsigned int> RenderableMultiresVolume::getBuffers() {
    std::vector<unsigned int> buffers{_atlasManager->atlasMapBuffer()};
    return buffers;
}*/

void RenderableMultiresVolume::update(const UpdateData& data) {
    _timestep++;
    _time = data.time.j2000Seconds();

    if (_gatheringStats) {
        std::chrono::system_clock::time_point frameEnd = std::chrono::system_clock::now();
        std::chrono::duration<double> frameDuration = frameEnd - _frameStart;

        // Make sure that the directory exists
        ghoul::filesystem::File file(_statsFileName);
        std::filesystem::path directory =
            std::filesystem::path(_statsFileName).parent_path();
        std::filesystem::create_directories(directory);

        std::ofstream ofs(_statsFileName, std::ofstream::out);

        ofs << frameDuration.count() << " "
            << _selectionDuration.count() << " "
            << _uploadDuration.count() << " "
            << _nUsedBricks << " "
            << _nStreamedBricks << " "
            << _nDiskReads;

        ofs.close();

        _gatheringStats = false;
    }
    if (_statsToFile) {
        // Start frame timer
        _frameStart = std::chrono::system_clock::now();
        _statsFileName = _statsToFileName;

        _gatheringStats = true;
        _statsToFile = false;
    }

    int numTimesteps = _tsp->header().numTimesteps;
    int currentTimestep;
    bool visible = true;
    if (_loop) {
        currentTimestep = _timestep % numTimesteps;
    }
    else if (_useGlobalTime) {
        double t = (_time - _startTime) / (_endTime - _startTime);
        currentTimestep = static_cast<int>(t * numTimesteps);
        visible = currentTimestep >= 0 && currentTimestep < numTimesteps;
    }
    else {
        currentTimestep = _currentTime;
    }

    if (visible) {
        std::chrono::system_clock::time_point selectionStart;
        if (_gatheringStats) {
            selectionStart = std::chrono::system_clock::now();
        }

        switch (_selector) {
            case Selector::TF:
                if (_tfBrickSelector) {
                    _tfBrickSelector->setMemoryBudget(_memoryBudget);
                    _tfBrickSelector->setStreamingBudget(_streamingBudget);
                    _tfBrickSelector->selectBricks(currentTimestep, _brickIndices);
                }
                break;
            case Selector::SIMPLE:
                if (_simpleTfBrickSelector) {
                    _simpleTfBrickSelector->setMemoryBudget(_memoryBudget);
                    _simpleTfBrickSelector->setStreamingBudget(_streamingBudget);
                    _simpleTfBrickSelector->selectBricks(currentTimestep, _brickIndices);
                }
                break;
            case Selector::LOCAL:
                if (_localTfBrickSelector) {
                    _localTfBrickSelector->setMemoryBudget(_memoryBudget);
                    _localTfBrickSelector->setStreamingBudget(_streamingBudget);
                    _localTfBrickSelector->selectBricks(currentTimestep, _brickIndices);
                }
                break;
        }

        std::chrono::system_clock::time_point uploadStart;
        if (_gatheringStats) {
            std::chrono::system_clock::time_point selectionEnd =
                std::chrono::system_clock::now();
            _selectionDuration = selectionEnd - selectionStart;
            uploadStart = selectionEnd;
        }

        _atlasManager->updateAtlas(AtlasManager::EVEN, _brickIndices);

        if (_gatheringStats) {
            std::chrono::system_clock::time_point uploadEnd =
                std::chrono::system_clock::now();
            _uploadDuration = uploadEnd - uploadStart;
            _nDiskReads = _atlasManager->numDiskReads();
            _nUsedBricks = _atlasManager->numUsedBricks();
            _nStreamedBricks = _atlasManager->numStreamedBricks();
        }
    }

    if (_raycaster) {
        glm::mat4 transform = glm::translate(
            glm::mat4(1.f),
            static_cast<glm::vec3>(_translation) *
                std::pow(10.f, static_cast<float>(_scalingExponent))
        );
        glm::vec3 eulerRotation = static_cast<glm::vec3>(_rotation);
        transform = glm::rotate(transform, eulerRotation.x, glm::vec3(1.f, 0.f, 0.f));
        transform = glm::rotate(transform, eulerRotation.y, glm::vec3(0.f, 1.f, 0.f));
        transform = glm::rotate(transform, eulerRotation.z, glm::vec3(0.f, 0.f, 1.f));
        transform = glm::scale(
            transform,
            static_cast<glm::vec3>(_scaling) *
                std::pow(10.f, static_cast<float>(_scalingExponent))
        );

        _raycaster->setStepSizeCoefficient(_stepSizeCoefficient);
        _raycaster->setModelTransform(transform);
        //_raycaster->setTime(data.time);
    }
}

void RenderableMultiresVolume::render(const RenderData& data, RendererTasks& tasks) {
    RaycasterTask task { _raycaster.get(), data };
    tasks.raycasterTasks.push_back(task);
}

} // namespace openspace
