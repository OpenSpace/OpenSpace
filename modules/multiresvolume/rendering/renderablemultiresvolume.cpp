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

#include <modules/multiresvolume/rendering/renderablemultiresvolume.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>

#include <ghoul/glm.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/cachemanager.h>

#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>

#include <modules/multiresvolume/rendering/tsp.h>
#include <modules/multiresvolume/rendering/sandtsp.h>
#include <modules/multiresvolume/rendering/shentsp.h>
#include <modules/multiresvolume/rendering/atlasmanager.h>
#include <modules/multiresvolume/rendering/tfbrickselector.h>
#include <modules/multiresvolume/rendering/simpletfbrickselector.h>
#include <modules/multiresvolume/rendering/localtfbrickselector.h>
#include <modules/multiresvolume/rendering/shenbrickselector.h>
#include <modules/multiresvolume/rendering/timebrickselector.h>

#include <modules/multiresvolume/rendering/histogrammanager.h>
#include <modules/multiresvolume/rendering/errorhistogrammanager.h>
#include <modules/multiresvolume/rendering/localerrorhistogrammanager.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/vectorproperty.h>

#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>

#include <algorithm>
#include <iterator>
#include <fstream>
#include <algorithm>
#include <chrono>



namespace {
    const std::string _loggerCat = "RenderableMultiresVolume";
    const std::string KeyDataSource = "Source";
    const std::string KeyErrorHistogramsSource = "ErrorHistogramsSource";
    const std::string KeyHints = "Hints";
    const std::string KeyTransferFunction = "TransferFunction";

    const std::string KeyVolumeName = "VolumeName";
    const std::string KeyBrickSelector = "BrickSelector";
    const std::string KeyStartTime = "StartTime";
    const std::string KeyEndTime = "EndTime";
    const std::string GlslHelpersPath = "${MODULES}/multiresvolume/shaders/helpers_fs.glsl";
    const std::string GlslHelperPath = "${MODULES}/multiresvolume/shaders/helper.glsl";
    const std::string GlslHeaderPath = "${MODULES}/multiresvolume/shaders/header.glsl";
    bool registeredGlslHelpers = false;
}

namespace openspace {

    const char* RenderableMultiresVolume::TYPE_SIMPLE = "simple";
    const char* RenderableMultiresVolume::TYPE_TIME = "time";
    const char* RenderableMultiresVolume::TYPE_TF = "tf";
    const char* RenderableMultiresVolume::TYPE_LOCAL = "local";
    const char* RenderableMultiresVolume::TYPE_SHEN = "shen";



    const std::unordered_map<const char *, RenderableMultiresVolume::Selector> RenderableMultiresVolume::SelectorValues = {
        {RenderableMultiresVolume::TYPE_SIMPLE , RenderableMultiresVolume::Selector::SIMPLE},
        {RenderableMultiresVolume::TYPE_TF     , RenderableMultiresVolume::Selector::TF},
        {RenderableMultiresVolume::TYPE_LOCAL  , RenderableMultiresVolume::Selector::LOCAL},
        { RenderableMultiresVolume::TYPE_SHEN  , RenderableMultiresVolume::Selector::SHEN },
        {RenderableMultiresVolume::TYPE_TIME   , RenderableMultiresVolume::Selector::TIME}
    };

RenderableMultiresVolume::RenderableMultiresVolume (const ghoul::Dictionary& dictionary)
    :  Renderable(dictionary)
    , _transferFunction(nullptr)
    , _timestep(0)
    , _atlasMapSize(0)
    , _tfBrickSelector(nullptr)
    , _simpleTfBrickSelector(nullptr)
    , _localTfBrickSelector(nullptr)
    , _timeBrickSelector(nullptr)
    , _errorHistogramManager(nullptr)
    , _histogramManager(nullptr)
    , _localErrorHistogramManager(nullptr)
    , _stepSizeCoefficient("stepSizeCoefficient", "Stepsize Coefficient", 1.f, 0.01f, 10.f)
    , _currentTime("currentTime", "Current Time", 0, 0, 0)
    , _memoryBudget("memoryBudget", "Memory Budget", 0, 0, 0)
    , _streamingBudget("streamingBudget", "Streaming Budget", 0, 0, 0)
    , _histogramBins("histogramBins", "Histogram Bins", 0, 0, 0, 1)
    , _useGlobalTime("useGlobalTime", "Global Time", false)
    , _loop("loop", "Loop", false)
    , _selectorName("selector", "Brick Selector")
    , _gatheringStats(false)
    , _statsToFile("printStats", "Print Stats", false)
    , _statsToFileName("printStatsFileName", "Stats Filename")
    , _scalingExponent("scalingExponent", "Scaling Exponent", 1, -10, 20)
    , _scaling("scaling", "Scaling", glm::vec3(1.0, 1.0, 1.0), glm::vec3(0.0), glm::vec3(10.0))
    , _translation("translation", "Translation", glm::vec3(0.0, 0.0, 0.0), glm::vec3(0.0), glm::vec3(10.0))
    , _rotation("rotation", "Euler rotation", glm::vec3(0.0, 0.0, 0.0), glm::vec3(0), glm::vec3(6.28))
    , _toleranceSpatial("spatialTolerance", "Spatial Tolerance", 1.f, 0.00f, 2.f)
    , _toleranceTemporal("temporalTolerance", "Temporal Tolerance", 1.f, 0.00f, 2.f)
{
    std::string name;

    _filename = "";
    bool success = dictionary.getValue(KeyDataSource, _filename);
    if (!success) {
        LERROR("Node '" << name << "' did not contain a valid '" <<  KeyDataSource << "'");
        return;
    }
    _filename = absPath(_filename);
    if (_filename == "") {
        return;
    }

    _errorHistogramsPath = "";
    if (dictionary.getValue(KeyErrorHistogramsSource, _errorHistogramsPath)) {
        _errorHistogramsPath = absPath(_errorHistogramsPath);
    }



    float scalingExponent, stepSizeCoefficient;
    glm::vec3 scaling, translation, rotation;

    if (dictionary.getValue("ScalingExponent", scalingExponent)) {
        _scalingExponent = scalingExponent;
    }
    if (dictionary.getValue("Scaling", scaling)) {
        _scaling = scaling;
    }
    if (dictionary.getValue("Translation", translation)) {
        _translation = translation;
    }
    if (dictionary.getValue("Rotation", rotation)) {
        _rotation = rotation;
    }
    if (dictionary.getValue("StepSizeCoefficient", stepSizeCoefficient)) {
        _stepSizeCoefficient = stepSizeCoefficient;
    }


    std::string startTimeString, endTimeString;
    bool hasTimeData = true;
    hasTimeData &= dictionary.getValue(KeyStartTime, startTimeString);
    hasTimeData &= dictionary.getValue(KeyEndTime, endTimeString);
    if (hasTimeData) {
        _startTime = SpiceManager::ref().ephemerisTimeFromDate(startTimeString);
        _endTime = SpiceManager::ref().ephemerisTimeFromDate(endTimeString);
    }
    if (hasTimeData) {
        _loop = false;
    } else {
    _loop = true;
        LWARNING("Node " << name << " does not provide valid time information. Viewing one image per frame.");
    }


    _transferFunction = nullptr;
    _transferFunctionPath = "";
    success = dictionary.getValue(KeyTransferFunction, _transferFunctionPath);
    if (!success) {
        LERROR("Node '" << name << "' did not contain a valid '" <<
            KeyTransferFunction << "'");
        return;
    }
    _transferFunctionPath = absPath(_transferFunctionPath);
    _transferFunction = std::make_shared<TransferFunction>(_transferFunctionPath);

    _tsp = std::make_shared<SandTSP>(_filename);
    _atlasManager = std::make_shared<AtlasManager>(_tsp.get());

    _selectorName = TYPE_TF;
    std::string brickSelectorType;
    if (dictionary.hasKey(KeyBrickSelector)) {
        success = dictionary.getValue(KeyBrickSelector, brickSelectorType);
        if (success) {
            _selectorName = brickSelectorType;
        }
    }

    _selector = getSelector();

    addProperty(_selectorName);
    _selectorName.onChange([&] {
        setSelectorType(getSelector());
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
    addProperty(_toleranceSpatial);
    addProperty(_toleranceTemporal);

    _toleranceSpatial.onChange([&] {
        _shenBrickSelector->setSpatialTolerance(_toleranceSpatial.value());
    });

    _toleranceTemporal.onChange([&] {
        _shenBrickSelector->setTemporalTolerance(_toleranceTemporal.value());
    });
}

RenderableMultiresVolume::~RenderableMultiresVolume() {

    if (_tfBrickSelector)
        delete _tfBrickSelector;
    if (_simpleTfBrickSelector)
        delete _simpleTfBrickSelector;
    if (_localTfBrickSelector)
        delete _localTfBrickSelector;
    if (_shenBrickSelector)
        delete _shenBrickSelector;
    if (_timeBrickSelector)
        delete _timeBrickSelector;

    if (_errorHistogramManager)
        delete _errorHistogramManager;
    if (_histogramManager)
        delete _histogramManager;
    if (_localErrorHistogramManager)
        delete _localErrorHistogramManager;
}

bool RenderableMultiresVolume::setSelectorType(Selector selector) {
    _selector = selector;
    switch (_selector) {
        case Selector::TF:
            if (!_tfBrickSelector) {
                TfBrickSelector* tbs;
                _errorHistogramManager = new ErrorHistogramManager(_tsp.get());
                _tfBrickSelector = tbs = new TfBrickSelector(_tsp.get(), _errorHistogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _transferFunction->setCallback([tbs](const TransferFunction &tf) {
                    tbs->initialize();
                });
                return initializeSelector();
            }
            break;
        case Selector::TIME:
            if (!_timeBrickSelector) {
                TimeBrickSelector* tbs;
                _errorHistogramManager = new ErrorHistogramManager(_tsp.get());
                tbs = new TimeBrickSelector(_tsp.get(), _errorHistogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _timeBrickSelector = tbs;
                _transferFunction->setCallback([tbs](const TransferFunction &tf) {
                    tbs->initialize();
                });
                return initializeSelector();
            }
            break;
        case Selector::SIMPLE:
            if (!_simpleTfBrickSelector) {
                SimpleTfBrickSelector *stbs;
                _histogramManager = new HistogramManager(_tsp.get());
                _simpleTfBrickSelector = stbs = new SimpleTfBrickSelector(_tsp.get(), _histogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _transferFunction->setCallback([stbs](const TransferFunction &tf) {
                    stbs->initialize();
                });
                return initializeSelector();
            }
            break;

        case Selector::LOCAL:
            if (!_localTfBrickSelector) {
                LocalTfBrickSelector* ltbs;
                _localErrorHistogramManager = new LocalErrorHistogramManager(_tsp.get());
                _localTfBrickSelector = ltbs = new LocalTfBrickSelector(_tsp.get(), _localErrorHistogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _transferFunction->setCallback([ltbs](const TransferFunction &tf) {
                    ltbs->initialize();
                });
                return initializeSelector();
            }
            break;
        case Selector::SHEN:
            if (!_shenBrickSelector) {
                ShenBrickSelector* sbs;
                _shenBrickSelector = sbs = new ShenBrickSelector(_tsp.get(), _tsp->getMaxError(TSP::NodeType::SPATIAL), _tsp->getMaxError(TSP::NodeType::TEMPORAL));
                _transferFunction->setCallback([sbs](const TransferFunction &tf) {
                    sbs->initialize();
                });
                return initializeSelector();
            }
            break;
    }
    return true;
}

bool RenderableMultiresVolume::initialize() {

    bool success = _tsp && _tsp->load();

    unsigned int maxNumBricks = _tsp->header().xNumBricks_ * _tsp->header().yNumBricks_ * _tsp->header().zNumBricks_;

    unsigned int maxInitialBudget = 2048;
    int initialBudget = std::min(maxInitialBudget, maxNumBricks);

    unsigned int histoBins = 50,
        histoBinsMin = 2,
        histoBinsMax = maxInitialBudget,
        histoBinsStep = 1;

    _currentTime = properties::IntProperty("currentTime", "Current Time", 0, 0, _tsp->header().numTimesteps_ - 1);
    _memoryBudget = properties::IntProperty("memoryBudget", "Memory Budget", initialBudget, 0, maxNumBricks);
    _streamingBudget = properties::IntProperty("streamingBudget", "Streaming Budget", initialBudget, 0, maxNumBricks);
    _histogramBins = properties::IntProperty("histogramBins", "Histogram Bins", histoBins, histoBinsMin, histoBinsMax, histoBinsStep);

    addProperty(_currentTime);
    addProperty(_memoryBudget);
    addProperty(_streamingBudget);
    addProperty(_histogramBins);

    if (success) {
        _brickIndices.resize(maxNumBricks, 0);
        success &= setSelectorType(_selector);
    }

    success &= _atlasManager && _atlasManager->initialize();

    _transferFunction->update();

    success &= isReady();

    _raycaster = std::make_unique<MultiresVolumeRaycaster>(_tsp, _atlasManager, _transferFunction);
    _raycaster->initialize();

    OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());

    std::function<void(bool)> onChange = [&](bool enabled) {
        if (enabled) {
            OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());
        }
        else {
            OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        }
    };

    onEnabledChange(onChange);

    return success;
}

bool RenderableMultiresVolume::deinitialize() {
    _tsp = nullptr;
    _transferFunction = nullptr;
    return true;
}

bool RenderableMultiresVolume::isReady() const {
    return true;
}


bool RenderableMultiresVolume::initializeShenSelector() {
    bool success = true;
    BrickSelector * selector = _shenBrickSelector;
    success &= selector && selector->initialize();
    _toleranceSpatial.setMaxValue(_tsp->getMaxError(TSP::NodeType::SPATIAL));
    _toleranceSpatial.setMinValue(_tsp->getMinError(TSP::NodeType::SPATIAL));
    _toleranceTemporal.setMaxValue(_tsp->getMaxError(TSP::NodeType::TEMPORAL));
    _toleranceTemporal.setMinValue(_tsp->getMinError(TSP::NodeType::TEMPORAL));
    return success;
}

bool RenderableMultiresVolume::initializeSelector() {
    int nHistograms = _histogramBins;
    bool success = true;

    BrickSelector * selector;
    HistogramManager * manager;
    std::string scenePath = "";

    switch (_selector) {
    case Selector::TF:      selector = _tfBrickSelector;        manager = _errorHistogramManager;        scenePath = _errorHistogramsPath;  break;
    case Selector::TIME:    selector = _timeBrickSelector;      manager = _errorHistogramManager;        scenePath = _errorHistogramsPath;  break;
    case Selector::LOCAL:   selector = _localTfBrickSelector;   manager = _localErrorHistogramManager;                                      break;
    case Selector::SIMPLE:  selector = _simpleTfBrickSelector;  manager = _histogramManager;                                                break;
    case Selector::SHEN:    selector = _shenBrickSelector;      return initializeShenSelector();
    }

    if (manager) {
        LINFO("Histogram Manager: " << manager->getName());
        std::stringstream cacheName;
        ghoul::filesystem::File f = _filename;
        cacheName << f.baseName() << "_" << nHistograms << "_" << manager->getName();
        std::string cacheFilename;
        cacheFilename = FileSys.cacheManager()->cachedFilename(
            cacheName.str(), "", ghoul::filesystem::CacheManager::Persistent::Yes);
        LINFO("Trying to open cache: " << cacheFilename);
        std::ifstream cacheFile(cacheFilename, std::ios::in | std::ios::binary);
        if (cacheFile.is_open()) {
            // Read histograms from cache.
            cacheFile.close();
            LINFO("Loading histograms from cache: " << cacheFilename);
            success &= manager->loadFromFile(cacheFilename);
        }
        else {
            if (scenePath != "") {
                // Read histograms from scene data.
                LINFO("Loading histograms from scene data: " << scenePath);
                success &= manager->loadFromFile(scenePath);
            }
            else {
                // Build histograms from tsp file.
                LWARNING("Failed to open " << cacheFilename);
                success &= manager->buildHistograms(nHistograms);
            }
            if (success) {
                LINFO("Writing cache to " << cacheFilename);
                manager->saveToFile(cacheFilename);
            }
        }
        success &= selector && selector->initialize();
    }





    return success;
}

void RenderableMultiresVolume::update(const UpdateData& data) {
    _timestep++;
    _time = data.time.j2000Seconds();

    if (_gatheringStats) {
        std::chrono::system_clock::time_point frameEnd = std::chrono::system_clock::now();
        std::chrono::duration<double> frameDuration = frameEnd - _frameStart;

        // Make sure that the directory exists
        ghoul::filesystem::File file(_statsFileName);
        ghoul::filesystem::Directory directory(file.directoryName());
        FileSys.createDirectory(directory, ghoul::filesystem::FileSystem::Recursive::Yes);

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


    int numTimesteps = _tsp->header().numTimesteps_;
    int currentTimestep;
    bool visible = true;
    if (_loop) {
        currentTimestep = _timestep % numTimesteps;
    }
    else if (_useGlobalTime) {
        double t = (_time - _startTime) / (_endTime - _startTime);
        currentTimestep = t * numTimesteps;
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

        BrickSelector * s;
        switch (_selector) {
        case Selector::TF:      s = _tfBrickSelector; break;
        case Selector::SIMPLE:  s = _simpleTfBrickSelector; break;
        case Selector::LOCAL:   s = _localTfBrickSelector; break;
        case Selector::SHEN:    s = _shenBrickSelector; break;
        case Selector::TIME:    s = _timeBrickSelector; break;
        }

        if (s) {
            if (_selector != Selector::SHEN) {
                s->setMemoryBudget(_memoryBudget);
                s->setStreamingBudget(_streamingBudget);
            }
            s->selectBricks(currentTimestep, _brickIndices);
        }

        std::chrono::system_clock::time_point uploadStart;
        if (_gatheringStats) {
            std::chrono::system_clock::time_point selectionEnd = std::chrono::system_clock::now();
            _selectionDuration = selectionEnd - selectionStart;
            uploadStart = selectionEnd;
        }

        _atlasManager->updateAtlas(AtlasManager::EVEN, _brickIndices);

        if (_gatheringStats) {
            std::chrono::system_clock::time_point uploadEnd = std::chrono::system_clock::now();
            _uploadDuration = uploadEnd - uploadStart;
            _nDiskReads = _atlasManager->getNumDiskReads();
            _nUsedBricks = _atlasManager->getNumUsedBricks();
            _nStreamedBricks = _atlasManager->getNumStreamedBricks();
        }
    }

    if (_raycaster) {

        glm::mat4 transform = glm::translate(glm::mat4(1.0), static_cast<glm::vec3>(_translation) * std::pow(10.0f, static_cast<float>(_scalingExponent)));
        glm::vec3 eulerRotation = static_cast<glm::vec3>(_rotation);
        transform = glm::rotate(transform, eulerRotation.x, glm::vec3(1, 0, 0));
        transform = glm::rotate(transform, eulerRotation.y, glm::vec3(0, 1, 0));
        transform = glm::rotate(transform, eulerRotation.z, glm::vec3(0, 0, 1));
        transform = glm::scale(transform, static_cast<glm::vec3>(_scaling) * std::pow(10.0f, static_cast<float>(_scalingExponent)));

    
        _raycaster->setStepSizeCoefficient(_stepSizeCoefficient);
        _raycaster->setModelTransform(transform);
    }

}

void RenderableMultiresVolume::render(const RenderData& data, RendererTasks& tasks) {
    RaycasterTask task{ _raycaster.get(), data };
    tasks.raycasterTasks.push_back(task);
}

RenderableMultiresVolume::Selector RenderableMultiresVolume::getSelector() {
    std::string s;
    _selectorName.getStringValue(s);
    s = s.substr(1, s.length() - 2);

    if (s == TYPE_TF)       return SelectorValues.at(TYPE_TF);
    if (s == TYPE_SIMPLE)   return SelectorValues.at(TYPE_SIMPLE);
    if (s == TYPE_LOCAL)    return SelectorValues.at(TYPE_LOCAL);
    if (s == TYPE_SHEN)    return SelectorValues.at(TYPE_SHEN);
    if (s == TYPE_TIME)     return SelectorValues.at(TYPE_TIME);

    return Selector::SIMPLE;
}

} // namespace openspace
