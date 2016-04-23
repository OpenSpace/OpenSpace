/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <glm/glm.hpp>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/cachemanager.h>

#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>

#include <modules/multiresvolume/rendering/tsp.h>
#include <modules/multiresvolume/rendering/atlasmanager.h>
#include <modules/multiresvolume/rendering/shenbrickselector.h>
#include <modules/multiresvolume/rendering/tfbrickselector.h>
#include <modules/multiresvolume/rendering/simpletfbrickselector.h>
#include <modules/multiresvolume/rendering/localtfbrickselector.h>

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

RenderableMultiresVolume::RenderableMultiresVolume (const ghoul::Dictionary& dictionary)
    :  Renderable(dictionary)
    , _transferFunction(nullptr)
    , _timestep(0)
    , _atlasMapSize(0)
    , _tfBrickSelector(nullptr)
    , _simpleTfBrickSelector(nullptr)
    , _localTfBrickSelector(nullptr)
    , _errorHistogramManager(nullptr)
    , _histogramManager(nullptr)
    , _localErrorHistogramManager(nullptr)
    , _stepSizeCoefficient("stepSizeCoefficient", "Stepsize Coefficient", 1.f, 0.01f, 10.f)
    , _currentTime("currentTime", "Current Time", 0, 0, 0)
    , _memoryBudget("memoryBudget", "Memory Budget", 0, 0, 0)
    , _streamingBudget("streamingBudget", "Streaming Budget", 0, 0, 0)
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
{
    std::string name;
    //bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
    //assert(success);

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

    //_pscOffset = psc(glm::vec4(0.0));
    //_boxScaling = glm::vec3(1.0);


    /*if (dictionary.hasKey(KeyBoxScaling)) {
        glm::vec4 scalingVec4(_boxScaling, _w);
        success = dictionary.getValue(KeyBoxScaling, scalingVec4);
        if (success) {
            _boxScaling = scalingVec4.xyz;
            _w = scalingVec4.w;
        }
        else {
            success = dictionary.getValue(KeyBoxScaling, _boxScaling);
            if (!success) {
                LERROR("Node '" << name << "' did not contain a valid '" <<
                    KeyBoxScaling << "'");
                return;
            }
        }
    }*/

    //setBoundingSphere(PowerScaledScalar::CreatePSS(glm::length(_boxScaling)*pow(10,_w)));

    _tsp = std::make_shared<TSP>(_filename);
    _atlasManager = std::make_shared<AtlasManager>(_tsp.get());

    _selectorName = "tf";
    std::string brickSelectorType;
    if (dictionary.hasKey(KeyBrickSelector)) {
        success = dictionary.getValue(KeyBrickSelector, brickSelectorType);
        if (success) {
            _selectorName = brickSelectorType;
        }
    }

    std::string selectorName = _selectorName;
    if (selectorName == "simple") {
        _selector = Selector::SIMPLE;
    } else if (selectorName == "local") {
        _selector = Selector::LOCAL;
    } else {
        _selector = Selector::TF;
    }

    addProperty(_selectorName);
    _selectorName.onChange([&] {
        Selector s;
        std::string newSelectorName = _selectorName;
        if (newSelectorName == "simple") {
            s = Selector::SIMPLE;
        } else if (newSelectorName == "local") {
            s = Selector::LOCAL;
        } else if (newSelectorName == "tf") {
            s = Selector::TF;
        } else {
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


    //_brickSelector = new ShenBrickSelector(_tsp, -1, -1);
}

RenderableMultiresVolume::~RenderableMultiresVolume() {
    //OsEng.renderEngine()->aBuffer()->removeVolume(this);

    if (_tfBrickSelector)
        delete _tfBrickSelector;
    if (_simpleTfBrickSelector)
        delete _simpleTfBrickSelector;
    if (_localTfBrickSelector)
        delete _localTfBrickSelector;

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
                    tbs->calculateBrickErrors();
                });
                if (initializeSelector()) {
                    tbs->calculateBrickErrors();
                    return true;
                }
            }
            break;

        case Selector::SIMPLE:
            if (!_simpleTfBrickSelector) {
                SimpleTfBrickSelector *stbs;
                _histogramManager = new HistogramManager();
                _simpleTfBrickSelector = stbs = new SimpleTfBrickSelector(_tsp.get(), _histogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _transferFunction->setCallback([stbs](const TransferFunction &tf) {
                    stbs->calculateBrickImportances();
                });
                if (initializeSelector()) {
                    stbs->calculateBrickImportances();
                    return true;
                }
            }
            break;

        case Selector::LOCAL:
            if (!_localTfBrickSelector) {
                LocalTfBrickSelector* ltbs;
                _localErrorHistogramManager = new LocalErrorHistogramManager(_tsp.get());
                _localTfBrickSelector = ltbs = new LocalTfBrickSelector(_tsp.get(), _localErrorHistogramManager, _transferFunction.get(), _memoryBudget, _streamingBudget);
                _transferFunction->setCallback([ltbs](const TransferFunction &tf) {
                    ltbs->calculateBrickErrors();
                });
                if (initializeSelector()) {
                    ltbs->calculateBrickErrors();
                    return true;
                }
            }
            break;
    }
    return false;
}

bool RenderableMultiresVolume::initialize() {

    bool success = _tsp && _tsp->load();

    unsigned int maxNumBricks = _tsp->header().xNumBricks_ * _tsp->header().yNumBricks_ * _tsp->header().zNumBricks_;

    unsigned int maxInitialBudget = 2048;
    int initialBudget = std::min(maxInitialBudget, maxNumBricks);

    _currentTime = properties::IntProperty("currentTime", "Current Time", 0, 0, _tsp->header().numTimesteps_ - 1);
    _memoryBudget = properties::IntProperty("memoryBudget", "Memory Budget", initialBudget, 0, maxNumBricks);
    _streamingBudget = properties::IntProperty("streamingBudget", "Streaming Budget", initialBudget, 0, maxNumBricks);
    addProperty(_currentTime);
    addProperty(_memoryBudget);
    addProperty(_streamingBudget);

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


bool RenderableMultiresVolume::initializeSelector() {
    int nHistograms = 50;
    bool success = true;

    switch (_selector) {
        case Selector::TF:
            if (_errorHistogramManager) {
                std::stringstream cacheName;
                ghoul::filesystem::File f = _filename;
                cacheName << f.baseName() << "_" << nHistograms << "_errorHistograms";
                std::string cacheFilename;
                cacheFilename = FileSys.cacheManager()->cachedFilename(
                    cacheName.str(), "", ghoul::filesystem::CacheManager::Persistent::Yes);
                std::ifstream cacheFile(cacheFilename, std::ios::in | std::ios::binary);
                if (cacheFile.is_open()) {
                    // Read histograms from cache.
                    cacheFile.close();
                    LINFO("Loading histograms from " << cacheFilename);
                    success &= _errorHistogramManager->loadFromFile(cacheFilename);
                } else {
                    // Build histograms from tsp file.
                    LWARNING("Failed to open " << cacheFilename);
                    if (success &= _errorHistogramManager->buildHistograms(nHistograms)) {
                        LINFO("Writing cache to " << cacheFilename);
                        _errorHistogramManager->saveToFile(cacheFilename);
                    }
                }
                success &= _tfBrickSelector && _tfBrickSelector->initialize();
            }
            break;

        case Selector::SIMPLE:
            if (_histogramManager) {
                std::stringstream cacheName;
                ghoul::filesystem::File f = _filename;
                cacheName << f.baseName() << "_" << nHistograms << "_histograms";
                std::string cacheFilename;
                cacheFilename = FileSys.cacheManager()->cachedFilename(
                    cacheName.str(), "", ghoul::filesystem::CacheManager::Persistent::Yes);
                std::ifstream cacheFile(cacheFilename, std::ios::in | std::ios::binary);
                if (cacheFile.is_open()) {
                    // Read histograms from cache.
                    cacheFile.close();
                    LINFO("Loading histograms from " << cacheFilename);
                    success &= _histogramManager->loadFromFile(cacheFilename);
                } else {
                    // Build histograms from tsp file.
                    LWARNING("Failed to open " << cacheFilename);
                    if (success &= _histogramManager->buildHistograms(_tsp.get(), nHistograms)) {
                        LINFO("Writing cache to " << cacheFilename);
                        _histogramManager->saveToFile(cacheFilename);
                    }
                }
                success &= _simpleTfBrickSelector && _simpleTfBrickSelector->initialize();
            }
            break;

        case Selector::LOCAL:
            if (_localErrorHistogramManager) {
                std::stringstream cacheName;
                ghoul::filesystem::File f = _filename;
                cacheName << f.baseName() << "_" << nHistograms << "_localErrorHistograms";
                std::string cacheFilename;
                cacheFilename = FileSys.cacheManager()->cachedFilename(
                    cacheName.str(), "", ghoul::filesystem::CacheManager::Persistent::Yes);
                std::ifstream cacheFile(cacheFilename, std::ios::in | std::ios::binary);
                if (cacheFile.is_open()) {
                    // Read histograms from cache.
                    cacheFile.close();
                    LINFO("Loading histograms from " << cacheFilename);
                    success &= _localErrorHistogramManager->loadFromFile(cacheFilename);
                } else {
                    // Build histograms from tsp file.
                    LWARNING("Failed to open " << cacheFilename);
                    if (success &= _localErrorHistogramManager->buildHistograms(nHistograms)) {
                        LINFO("Writing cache to " << cacheFilename);
                        _localErrorHistogramManager->saveToFile(cacheFilename);
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
    program->setUniform(ss.str(), visible ? 1.0f : 0.0f);

    ss.str(std::string());
    ss << "stepSizeCoefficient_" << getId();
    program->setUniform(ss.str(), _stepSizeCoefficient);

    ss.str(std::string());
    ss << "transferFunction_" << getId();
    program->setUniform(ss.str(), getTextureUnit(_transferFunction->getTexture()));

    ss.str(std::string());
    ss << "textureAtlas_" << getId();
    program->setUniform(ss.str(), getTextureUnit(_atlasManager->textureAtlas()));

    ss.str(std::string());
    ss << "atlasMapBlock_" << getId();
    program->setSsboBinding(ss.str(), getSsboBinding(_atlasManager->atlasMapBuffer()));

    ss.str(std::string());
    ss << "gridType_" << getId();
    program->setUniform(ss.str(), static_cast<int>(_tsp->header().gridType_));

    ss.str(std::string());
    ss << "maxNumBricksPerAxis_" << getId();
    program->setUniform(ss.str(), static_cast<unsigned int>(_tsp->header().xNumBricks_));

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
    std::vector<ghoul::opengl::Texture*> textures{_transferFunction->getTexture(), _atlasManager->textureAtlas()};
    return textures;
}

std::vector<unsigned int> RenderableMultiresVolume::getBuffers() {
    std::vector<unsigned int> buffers{_atlasManager->atlasMapBuffer()};
    return buffers;
}*/

void RenderableMultiresVolume::update(const UpdateData& data) {
        _timestep++;
    _time = data.time;

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
        //_raycaster->setTime(data.time);
    }

}

void RenderableMultiresVolume::render(const RenderData& data, RendererTasks& tasks) {
    RaycasterTask task{ _raycaster.get(), data };
    tasks.raycasterTasks.push_back(task);
}

} // namespace openspace
