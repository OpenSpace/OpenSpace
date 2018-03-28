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
#include <modules/multiresvolume/rendering/allatlasmanager.h>
#include <modules/multiresvolume/rendering/tfbrickselector.h>
#include <modules/multiresvolume/rendering/simpletfbrickselector.h>
#include <modules/multiresvolume/rendering/localtfbrickselector.h>
#include <modules/multiresvolume/rendering/shenbrickselector.h>
#include <modules/multiresvolume/rendering/timebrickselector.h>
#include <modules/multiresvolume/rendering/leafbrickselector.h>

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
    constexpr const char* _loggerCat = "RenderableMultiresVolume";
    constexpr const char* KeyDataSource = "Source";
    constexpr const char* KeyErrorHistogramsSource = "ErrorHistogramsSource";
    constexpr const char* KeyHints = "Hints";
    constexpr const char* KeyTransferFunction = "TransferFunction";
    constexpr const char* KeyTspType = "TspType";
    constexpr const char* KeyAtlasType = "AtlasType";
    constexpr const char* KeyHistogramBins = "HistogramBins";
    constexpr const char* KeyScalingExponent = "ScalingExponent";
    constexpr const char* KeyScaling = "Scaling";
    constexpr const char* KeyTranslation = "Translation";
    constexpr const char* KeyRotation = "Rotation";
    constexpr const char* KeyStepSizeCoefficient = "StepSizeCoefficient";

    constexpr const char* KeyVolumeName = "VolumeName";
    constexpr const char* KeyBrickSelector = "BrickSelector";
    constexpr const char* KeyStartTime = "StartTime";
    constexpr const char* KeyEndTime = "EndTime";
    constexpr const char* GlslHelpersPath =
        "${MODULES}/multiresvolume/shaders/helpers_fs.glsl";
    constexpr const char* GlslHelperPath =
        "${MODULES}/multiresvolume/shaders/helper.glsl";
    constexpr const char* GlslHeaderPath =
        "${MODULES}/multiresvolume/shaders/header.glsl";
    bool registeredGlslHelpers = false;

    static const openspace::properties::Property::PropertyInfo StepSizeCoefficientInfo = {
        KeyStepSizeCoefficient,
        "Stepsize Coefficient",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo CurrentTimeInfo = {
        "CurrentTime",
        "Current Time",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo MemoryBudgetInfo = {
        "MemoryBudget",
        "Memory Budget",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo StreamingBudgetInfo = {
        "StreamingBudget",
        "Streaming Budget",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo UseGlobalTimeInfo = {
        "UseGlobalTime",
        "Global Time",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo LoopInfo = {
        "Loop",
        "Loop",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo SelectorNameInfo = {
        "Selector",
        "Brick Selector",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo AtlasTypeInfo = {
        KeyAtlasType,
        "Atlas Type",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo StatsToFileInfo = {
        "PrintStats",
        "Print Stats",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo StatsToFileNameInfo = {
        "PrintStatsFileName",
        "Stats Filename",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ScalingExponentInfo = {
        KeyScalingExponent,
        "Scaling Exponent",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ScalingInfo = {
        KeyScaling,
        KeyScaling,
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TranslationInfo = {
        KeyTranslation,
        KeyTranslation,
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo RotationInfo = {
        KeyRotation,
        "Euler rotation",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ToleranceSpatialInfo = {
        "spatialTolerance",
        "Spatial Tolerance",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ToleranceTemporalInfo = {
        "temporalTolerance",
        "Temporal Tolerance",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo HistogramBinsInfo = {
        KeyHistogramBins,
        "Histogram Bins",
        "" // @TODO Missing documentation
    };

} // namespace

namespace openspace {

    const char* RenderableMultiresVolume::TYPE_SIMPLE = "simple";
    const char* RenderableMultiresVolume::TYPE_TIME   = "time";
    const char* RenderableMultiresVolume::TYPE_TF     = "tf";
    const char* RenderableMultiresVolume::TYPE_LOCAL  = "local";
    const char* RenderableMultiresVolume::TYPE_SHEN   = "shen";
    const char* RenderableMultiresVolume::TYPE_LEAF = "leaf";

    const char* RenderableMultiresVolume::TSP_DEFAULT = "default";
    const char* RenderableMultiresVolume::TSP_SAND    = "sand";
    const char* RenderableMultiresVolume::TSP_SHEN    = "shen";

    const char* RenderableMultiresVolume::ATLAS_DEFAULT = "default";
    const char* RenderableMultiresVolume::ATLAS_ALL = "all";

    const std::unordered_map<const char *, RenderableMultiresVolume::Selector> RenderableMultiresVolume::SelectorValues = {
        { RenderableMultiresVolume::TYPE_SIMPLE , RenderableMultiresVolume::Selector::SIMPLE},
        { RenderableMultiresVolume::TYPE_TF     , RenderableMultiresVolume::Selector::TF},
        { RenderableMultiresVolume::TYPE_LOCAL  , RenderableMultiresVolume::Selector::LOCAL},
        { RenderableMultiresVolume::TYPE_SHEN  , RenderableMultiresVolume::Selector::SHEN },
        { RenderableMultiresVolume::TYPE_TIME   , RenderableMultiresVolume::Selector::TIME},
        { RenderableMultiresVolume::TYPE_LEAF   , RenderableMultiresVolume::Selector::LEAF }
    };

    const std::unordered_map<const char *, RenderableMultiresVolume::TspType> RenderableMultiresVolume::TspTypes = {
        { RenderableMultiresVolume::TSP_DEFAULT, RenderableMultiresVolume::TspType::DEFAULT },
        { RenderableMultiresVolume::TSP_SAND   , RenderableMultiresVolume::TspType::SAND },
        { RenderableMultiresVolume::TSP_SHEN   , RenderableMultiresVolume::TspType::SHEN }
    };

    const std::unordered_map<const char *, RenderableMultiresVolume::AtlasType> RenderableMultiresVolume::AtlasTypes = {
        { RenderableMultiresVolume::ATLAS_DEFAULT, RenderableMultiresVolume::AtlasType::DEFAULT },
        { RenderableMultiresVolume::ATLAS_ALL    , RenderableMultiresVolume::AtlasType::ALL }
    };
RenderableMultiresVolume::RenderableMultiresVolume(const ghoul::Dictionary& dictionary)
    :  Renderable(dictionary)
    , _transferFunction(nullptr)
    , _timestep(0)
    , _atlasMapSize(0)
    , _tfBrickSelector(nullptr)
    , _simpleTfBrickSelector(nullptr)
    , _localTfBrickSelector(nullptr)
    , _timeBrickSelector(nullptr)
    , _shenBrickSelector(nullptr)
    , _leafBrickSelector(nullptr)
    , _errorHistogramManager(nullptr)
    , _histogramManager(nullptr)
    , _localErrorHistogramManager(nullptr)
    , _stepSizeCoefficient(StepSizeCoefficientInfo, 1.f, 0.01f, 10.f)
    , _currentTime(CurrentTimeInfo, 0, 0, 0)
    , _memoryBudget(MemoryBudgetInfo, 0, 0, 0)
    , _streamingBudget(StreamingBudgetInfo, 0, 0, 0)
    , _histogramBins(HistogramBinsInfo, 0, 0, 0, 1)
    , _useGlobalTime(UseGlobalTimeInfo, false)
    , _loop(LoopInfo, false)
    , _selectorName(SelectorNameInfo)
    , _gatheringStats(false)
    , _statsToFile(StatsToFileInfo, false)
    , _statsToFileName(StatsToFileNameInfo)
    , _scalingExponent(ScalingExponentInfo, 1, -10, 20)
    , _scaling(ScalingInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(10.f))
    , _translation(TranslationInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(10.f))
    , _rotation(RotationInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(6.28f))
    , _toleranceSpatial(ToleranceSpatialInfo, 1.f, 0.f, 2.f)
    , _toleranceTemporal(ToleranceTemporalInfo, 1.f, 0.f, 2.f)
    , _tspType(RenderableMultiresVolume::TSP_DEFAULT)
    , _nBins(50)
    , _atlasType(AtlasTypeInfo, RenderableMultiresVolume::ATLAS_DEFAULT)
{
    std::string name;

    _filename = "";
    bool success = dictionary.getValue(KeyDataSource, _filename);
    if (!success) {
        LERROR(fmt::format(
            "Node '{}' did not contain a valid '{}'",
            name,
            KeyDataSource
        ));
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

    if (dictionary.getValue(KeyTspType, _tspType)) {
        std::transform(_tspType.begin(), _tspType.end(), _tspType.begin(), ::tolower);
    }

    std::string atlasType;
    if (dictionary.getValue(KeyAtlasType, atlasType)) {
        LWARNING(atlasType);
        std::transform(atlasType.begin(), atlasType.end(), atlasType.begin(), ::tolower);
        LWARNING(atlasType);
        _atlasType = atlasType;
    }

    float scalingExponent, stepSizeCoefficient, bins = static_cast<float>(_nBins);
    glm::vec3 scaling, translation, rotation;

    if (dictionary.getValue(KeyHistogramBins, bins)) {
        _nBins = static_cast<unsigned int>(bins);
    }

    if (dictionary.getValue(KeyScalingExponent, scalingExponent)) {
        _scalingExponent = static_cast<int>(scalingExponent);
    }
    if (dictionary.getValue(KeyScaling, scaling)) {
        _scaling = scaling;
    }
    if (dictionary.getValue(KeyTranslation, translation)) {
        _translation = translation;
    }
    if (dictionary.getValue(KeyRotation, rotation)) {
        _rotation = rotation;
    }
    if (dictionary.getValue(KeyStepSizeCoefficient, stepSizeCoefficient)) {
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
        LWARNING(fmt::format(
            "Node '{}' does not provide valid time information. Viewing one image per "
            "frame.",
            name
        ));
    }


    _transferFunction = nullptr;
    _transferFunctionPath = "";
    success = dictionary.getValue(KeyTransferFunction, _transferFunctionPath);
    if (!success) {
        LERROR(fmt::format(
            "Node '{}' did not contain a valid '{}'",
            name,
            KeyTransferFunction
        ));
        return;
    }
    _transferFunctionPath = absPath(_transferFunctionPath);
    _transferFunction = std::make_shared<TransferFunction>(_transferFunctionPath);
    
    // Create a TSP type based on the scene file.
    switch (getTspType()) {
    case TspType::SHEN:    _tsp = std::make_shared<ShenTSP>(_filename);    break;
    case TspType::SAND:    _tsp = std::make_shared<SandTSP>(_filename);    break;
    case TspType::DEFAULT: _tsp = std::make_shared<TSP>(_filename);        break;
    default:               _tsp = std::make_shared<TSP>(_filename);        break;
    }
/*
    switch (getAtlasType()) {
    case AtlasType::ALL:     _atlasManager = std::make_shared<AllAtlasManager>(_tsp.get()); break;
    case AtlasType::DEFAULT: _atlasManager = std::make_shared<AtlasManager>(_tsp.get());    break;
    default:                 _atlasManager = std::make_shared<AtlasManager>(_tsp.get());    break;
    }
*/
    // Get Selector from asset file if exists
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

    addProperty(_atlasType);
    _atlasType.onChange([&] {
        setAtlasType(getAtlasType());
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
                std::shared_ptr<TfBrickSelector> tfbs;
                _errorHistogramManager = new ErrorHistogramManager(_tsp.get());
                _tfBrickSelector = tfbs = std::make_shared<TfBrickSelector>(_tsp, _errorHistogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _transferFunction->setCallback([tfbs](const TransferFunction&) {
                    tfbs->initialize();
                });
                return initializeSelector();
            }
            break;
        case Selector::TIME:
            if (!_timeBrickSelector) {
                std::shared_ptr<TimeBrickSelector> tbs;
                _errorHistogramManager = new ErrorHistogramManager(_tsp.get());
                tbs = std::make_shared< TimeBrickSelector>(_tsp, _errorHistogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _timeBrickSelector = tbs;
                _transferFunction->setCallback([tbs](const TransferFunction&) {
                    tbs->initialize();
                });
                return initializeSelector();
            }
            break;
        case Selector::SIMPLE:
            if (!_simpleTfBrickSelector) {
                std::shared_ptr<SimpleTfBrickSelector> stbs;
                _histogramManager = new HistogramManager(_tsp.get());
                _simpleTfBrickSelector = stbs = std::make_shared<SimpleTfBrickSelector>(_tsp, _histogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _transferFunction->setCallback([stbs](const TransferFunction&) {
                    stbs->initialize();
                });
                return initializeSelector();
            }
            break;

        case Selector::LOCAL:
            if (!_localTfBrickSelector) {
                std::shared_ptr< LocalTfBrickSelector> ltbs;
                _localErrorHistogramManager = new LocalErrorHistogramManager(_tsp.get());
                _localTfBrickSelector = ltbs = std::make_shared<LocalTfBrickSelector>(_tsp, _localErrorHistogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _transferFunction->setCallback([ltbs](const TransferFunction&) {
                    ltbs->initialize();
                });
                return initializeSelector();
            }
            break;
        case Selector::SHEN:
            if (!_shenBrickSelector) {
                std::shared_ptr<ShenBrickSelector> sbs;
                _shenBrickSelector = sbs = std::make_shared<ShenBrickSelector>(_tsp, _tsp->getMaxError(TSP::NodeType::SPATIAL), _tsp->getMaxError(TSP::NodeType::TEMPORAL));
                _transferFunction->setCallback([sbs](const TransferFunction&) {
                    sbs->initialize();
                });
                return initializeSelector();
            }
            break;
        case Selector::LEAF:
            if (!_leafBrickSelector) {
                std::shared_ptr<LeafBrickSelector> lbs;
                _leafBrickSelector = lbs = std::make_shared<LeafBrickSelector>(_tsp);
                _transferFunction->setCallback([lbs](const TransferFunction&) {
                    lbs->initialize();
                });
                return initializeSelector();
            }
            break;
    }
    return true;
}

bool RenderableMultiresVolume::setAtlasType(RenderableMultiresVolume::AtlasType type) {
    switch (type) {
    case AtlasType::ALL:     _atlasManager = std::make_unique<AllAtlasManager>(_tsp.get()); break;
    case AtlasType::DEFAULT: _atlasManager = std::make_unique<AtlasManager>(_tsp.get());    break;
    default:                 _atlasManager = std::make_unique<AtlasManager>(_tsp.get());    break;
    }
    
    return _atlasManager->initialize();
}

void RenderableMultiresVolume::initializeGL() {
    bool success = _tsp && _tsp->load();

    unsigned int maxNumBricks = _tsp->header().xNumBricks_ * _tsp->header().yNumBricks_ * _tsp->header().zNumBricks_;

    unsigned int maxInitialBudget = 2048;
    int initialBudget = std::min(maxInitialBudget, maxNumBricks);

    unsigned int histoBins = _nBins,
        histoBinsMin = 2,
        histoBinsMax = maxInitialBudget,
        histoBinsStep = 1;

    _histogramBins = properties::IntProperty(
        HistogramBinsInfo,
        histoBins,
        histoBinsMin,
        histoBinsMax,
        histoBinsStep
    );
    _currentTime = properties::IntProperty(
        CurrentTimeInfo,
        0,
        0,
        _tsp->header().numTimesteps_ - 1
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
    addProperty(_histogramBins);

    if (success) {
        _brickIndices.resize(maxNumBricks, 0);
        success &= setSelectorType(_selector);
        success &= setAtlasType(getAtlasType());
    }

    //success &= _atlasManager && _atlasManager->initialize();

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

bool RenderableMultiresVolume::initializeShenSelector() {
    bool success = true;
    BrickSelector* selector = _shenBrickSelector.get();
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

    BrickSelector* selector;
    HistogramManager * manager;
    std::string scenePath = "";

    switch (_selector) {
    case Selector::TF:      selector = _tfBrickSelector.get();          manager = _errorHistogramManager;        scenePath = _errorHistogramsPath;  break;
    case Selector::TIME:    selector = _timeBrickSelector.get();        manager = _errorHistogramManager;        scenePath = _errorHistogramsPath;  break;
    case Selector::LOCAL:   selector = _localTfBrickSelector.get();     manager = _localErrorHistogramManager;   scenePath = _errorHistogramsPath;  break;
    case Selector::SIMPLE:  selector = _simpleTfBrickSelector.get();    manager = _histogramManager;                                                break;
    case Selector::SHEN:    selector = _shenBrickSelector.get();        return initializeShenSelector();
    case Selector::LEAF:    selector = _leafBrickSelector.get();        return true;
    default:                LERROR(fmt::format("No selector {}", _selector));        return false;
    }

    if (!manager) {
        LERROR("Couldn't properly initialize HistogramManager");
        return false;
    }

    LINFO(fmt::format("Histogram Manager: {}", manager->getName()));

    // Generate cache file name
    std::stringstream cacheName;
    ghoul::filesystem::File f = _filename;
    std::string cacheFilename;


    cacheName << f.baseName() << "_" << nHistograms << "_" << manager->getName();
    cacheFilename = FileSys.cacheManager()->cachedFilename(
        cacheName.str(), "", ghoul::filesystem::CacheManager::Persistent::Yes);



    // Try to load from scene
    std::string histogramFile = scenePath;
    if (!histogramFile.empty()) {
        // Read histograms from scene data.
        LINFO(fmt::format("Loading histograms from scene data: {}", histogramFile));
        success = manager->loadFromFile(histogramFile);
        if (!success) {
            LWARNING(fmt::format("Couldn't load histograms from scene: {}", histogramFile));
            histogramFile = "";
        }
    }

    // If can't, load from cache
    if (histogramFile.empty()) {
        histogramFile = cacheFilename;
        LINFO(fmt::format("Trying to open cache: {}", histogramFile));
        success = manager->loadFromFile(histogramFile);
        if (!success) {
            LINFO(fmt::format("Couldn't load histograms from cache: {}", histogramFile));
            histogramFile = "";
        }
    }
    
    //If can't, regenerate from file
    if (histogramFile.empty()) {
        // Build histograms from tsp file.
        LINFO(fmt::format("Generating from TSP file and caching in: {}", cacheFilename));
        success = manager->buildHistograms(nHistograms);
    }

    if (success) {
        // Caching histograms
        LINFO(fmt::format("Writing cache to {}", cacheFilename));
        manager->saveToFile(cacheFilename);
    }

    success &= selector && selector->initialize();
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

        BrickSelector* s;
        switch (_selector) {
        case Selector::TF:      s = _tfBrickSelector.get();         break;
        case Selector::SIMPLE:  s = _simpleTfBrickSelector.get();   break;
        case Selector::LOCAL:   s = _localTfBrickSelector.get();    break;
        case Selector::SHEN:    s = _shenBrickSelector.get();       break;
        case Selector::TIME:    s = _timeBrickSelector.get();       break;
        case Selector::LEAF:    s = _leafBrickSelector.get();       break;
        default:                LERROR(fmt::format("No selector {}", _selector)); return;
        }

        if (s) {
            s->setMemoryBudget(_memoryBudget);
            s->setStreamingBudget(_streamingBudget);
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
        
        /*LDEBUG(fmt::format("{}, {}, {}", 
            _atlasManager->getNumUsedBricks(),
            _atlasManager->getNumStreamedBricks(),
            _atlasManager->getNumDiskReads()
        ));
        */
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
    if (s == TYPE_SHEN)     return SelectorValues.at(TYPE_SHEN);
    if (s == TYPE_TIME)     return SelectorValues.at(TYPE_TIME);
    if (s == TYPE_LEAF)     return SelectorValues.at(TYPE_LEAF);

    return Selector::SIMPLE;
}

RenderableMultiresVolume::TspType RenderableMultiresVolume::getTspType() {
    if (_tspType == TSP_SAND)       return TspTypes.at(TSP_SAND);
    if (_tspType == TSP_SHEN)       return TspTypes.at(TSP_SHEN);
    if (_tspType == TSP_DEFAULT)    return TspTypes.at(TSP_DEFAULT);

    return TspType::DEFAULT;
}

RenderableMultiresVolume::AtlasType RenderableMultiresVolume::getAtlasType() {
    std::string val;
    if ( _atlasType.getStringValue(val) ) {
        val = val.substr(1, val.length() - 2);
        if (val == ATLAS_ALL)       return AtlasTypes.at(ATLAS_ALL);
        if (val == ATLAS_DEFAULT)   return AtlasTypes.at(ATLAS_DEFAULT);
    }
    LWARNING(fmt::format("Could not get atlas type {} -- setting default", val ) );
    return AtlasType::DEFAULT;
}

} // namespace openspace
