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

#include <modules/solarbrowsing/rendering/renderablesolarimagery.h>

#include <ghoul/logging/logmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/rendering/renderengine.h>

// TODO(mnoven) CLEAN REDUNDANT STUFF
#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/scene/scene.h>

#include <openspace/util/spicemanager.h>

#include <fstream>

#include <chrono>
#include <math.h>

#define BUFFER_SIZE 5

using namespace ghoul::opengl;
using namespace std::chrono;

typedef std::chrono::high_resolution_clock Clock;

namespace {
    static const std::string _loggerCat = "RenderableSolarImagery";
    //double HALF_SUN_RADIUS = (1391600000.0 * 0.50); // Half sun radius divided by magic factor
    const double EPSILON = std::numeric_limits<double>::epsilon();
}

namespace openspace {

RenderableSolarImagery::RenderableSolarImagery(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _sharpenValue("sharpenValue", "Sharpen", 0.0, 0.0, 1.0)
    , _contrastValue("contrastValue", "Contrast", 0.0, -15.0, 15.0)
    , _planeOpacity("planeOpacity", "Plane Opacity", 1.0, 0.0, 1.0)
    , _enableFrustum("enableFrustum", "Enable Frustum", false)
    , _enableBorder("enableBorder", "Enable Border", false)
    , _gammaValue("gammaValue", "Gamma", 0.9, 0.1, 10.0)
    , _asyncUploadPBO("asyncUploadPBO", "Upload to PBO Async", true)
    , _activeInstruments("activeInstrument", "Active Instrument", properties::OptionProperty::DisplayType::Radio)
    , _bufferSize("bufferSize", "Buffer Size", 5, 1, 150)
    , _displayTimers("displayTimers", "Display Timers", false)
    , _lazyBuffering("lazyBuffering", "Lazy Buffering", true)
    , _minRealTimeUpdateInterval("minRealTimeUpdateInterval", "Min Update Interval", 65, 0, 300)
    , _moveFactor("moveFactor", "Move Factor" , 1.0, 0.0, 1.0)
    , _resolutionLevel("resolutionlevel", "Level of detail", 3, 0, 5)
    , _useBuffering("useBuffering", "Use Buffering", true)
    , _usePBO("usePBO", "Use PBO", true)
    , _planeSize("planeSize", "Plane Size", 50.0, 0.0, 1.0)
    , _verboseMode("verboseMode", "Verbose Mode", false)
{
    // std::string target;
    // if (dictionary.getValue("Target", target)) {
    //     _target = target;
    // }

    _enabled = false;
    bool startEnabled;
    if (dictionary.getValue("Enabled", startEnabled)) {
        _enabled = startEnabled;
    }

    float gamma;
    if (dictionary.getValue("StartGamma", gamma)) {
        _gammaValue = gamma;
    }

    if (!dictionary.getValue("Name", _name)) {
        throw ghoul::RuntimeError("Nodename has to be specified");
    }

    // glm::dvec2 magicPlaneOffset;
    // if (dictionary.getValue("MagicOffsetFromCenter", magicPlaneOffset)) {
    //     _magicPlaneOffset = magicPlaneOffset;
    // }

    float tmpStartResolutionLevel;
    if (!dictionary.getValue("StartResolutionLevel", tmpStartResolutionLevel)) {
        throw ghoul::RuntimeError("Plane must have a starting resolution level");
    }
    _resolutionLevel = static_cast<int>(tmpStartResolutionLevel);

    // if (!dictionary.getValue("MagicPlaneFactor", _magicPlaneFactor)) {
    //     throw ghoul::RuntimeError("Plane must at the moment have a magic factor");
    // }

    float offset;
    if (dictionary.getValue("Offset", offset)) {
        _offset = offset;
    }

    std::string rootPath;
    if (!dictionary.getValue("RootPath", rootPath)) {
        throw ghoul::RuntimeError("RootPath has to be specified");
    }

    //TODO(mnoven): Can't pass in an int to dictionary?
    // float tmpResolution;
    // if (!dictionary.getValue("Resolution", tmpResolution)){
    //     throw ghoul::RuntimeError("Resolution has to be specified");
    // }
    //_fullResolution = static_cast<unsigned int>(tmpResolution);

    if (dictionary.hasKeyAndValue<ghoul::Dictionary>("Instruments")) {
        ghoul::Dictionary instrumentDic = dictionary.value<ghoul::Dictionary>("Instruments");
        for (size_t i = 1; i <= instrumentDic.size(); ++i) {
            if (!instrumentDic.hasKeyAndValue<std::string>(std::to_string(i))) {
                throw ghoul::RuntimeError("Instruments has to be an array-style table");
            }
            std::string instrument = instrumentDic.value<std::string>(std::to_string(i));
            std::transform(instrument.begin(), instrument.end(), instrument.begin(),
                           ::tolower);
            _instrumentFilter.insert(instrument);
        }
    }


    //SpacecraftImageryManager::ref().loadImageMetadata(rootPath, _imageMetadataMap2, _instrumentFilter);
    //saveMetadata(rootPath);
    loadMetadata(rootPath);

    // Add GUI names
    unsigned int i = 0;
    for (auto& el : _imageMetadataMap2) {
       // if (el.first.find("304") != std::string::npos) {
         //   _activeInstruments.addOption(0, el.first);
        //} else {
            _activeInstruments.addOption(i++, el.first);
       // }
    }

    std::string startInstrument;
    if (dictionary.getValue("StartInstrument", startInstrument)) {
        _currentActiveInstrument = startInstrument;

    } else {
        _currentActiveInstrument
            = _activeInstruments.getDescriptionByValue(_activeInstruments.value());
    }

    SpacecraftImageryManager::ref().loadTransferFunctions(rootPath + "/colortables", _tfMap, _instrumentFilter);
    //std::string tfRootPath;
    //if (dictionary.getValue("TransferfunctionPath", tfRootPath)) {
        //throw ghoul::RuntimeError("TransferfunctionPath has to be specified");
    //}

    // Some sanity checks
    if (_imageMetadataMap2.size() == 0) {
        if (_instrumentFilter.size() > 0) {
            LERROR("Could not find any instruments that match specified filter");
            for (auto& filter : _instrumentFilter) {
                LERROR(filter);
            }
        } else {
            LERROR("Images map is empty! Check your path");
        }
    }

    // Get image size
    auto& stateSequenceStart = _imageMetadataMap2[_currentActiveInstrument];
    auto& stateStart = stateSequenceStart.getState(OsEng.timeManager().time().j2000Seconds());
    std::shared_ptr<ImageMetadata> imStart = stateStart.contents();
    //_imageSize = _imageMetadataMap2[_currentActiveInstrument]. //_fullResolution / (pow(2, _resolutionLevel));
    _imageSize = imStart->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)));

    // Always give PBO maximum size
    //_pbo = std::make_unique<PixelBufferObject>(4096 * 4096 * sizeof(IMG_PRECISION));

    for (int i = 0; i < BUFFER_SIZE; ++i) {
        _pbos[i] = std::make_unique<PixelBufferObject>(4096 * 4096 * sizeof(IMG_PRECISION));
    }

    // TODO(mnoven): Faster to send GL_RGBA32F as internal format - GPU Pads anyways?
    _texture =  std::make_unique<Texture>(
                    nullptr,
                    glm::size3_t(_imageSize, _imageSize, 1),
                    ghoul::opengl::Texture::Red, // Format of the pixeldata
                    GL_R8, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
                    GL_UNSIGNED_BYTE, // Type of data
                    Texture::FilterMode::Linear,
                    Texture::WrappingMode::ClampToEdge
                );

    _texture->setDataOwnership(ghoul::Boolean::No);
    _texture->uploadTexture();

    // Initialize time
    _realTime = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;

     _enableFrustum.onChange([this]() {
        if (_enableFrustum) {
            _enableBorder = true;
        } else {
            _enableBorder = false;
        }
     });

    _activeInstruments.onChange([this]() {
        _updatingCurrentActiveChannel = true;
        _pboIsDirty = false;
        _currentActiveInstrument
               = _activeInstruments.getDescriptionByValue(_activeInstruments.value());

        // Update image size
        auto& stateSequence = _imageMetadataMap2[_currentActiveInstrument];
        auto& state = stateSequence.getState(OsEng.timeManager().time().j2000Seconds());
       // const double& timeObserved = state.timeObserved();
        std::shared_ptr<ImageMetadata> im = state.contents();
        //_imageSize = _imageMetadataMap2[_currentActiveInstrument]. //_fullResolution / (pow(2, _resolutionLevel));
        _imageSize = im->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)));
        //_pbo->setSize(_imageSize * _imageSize * sizeof(IMG_PRECISION));

        // Upload asap
        updateTextureGPU(/*asyncUpload=*/false);
        if (_useBuffering) {
            //double oktime = OsEng.timeManager().time().deltaTime();
            //TimeManager& lel = OsEng.timeManager();
            // _currentActiveImageTime
            //           = getDecodeDataFromOsTime(OsEng.timeManager().time().j2000Seconds())
            //                   .timeObserved;
            clearBuffer();
            // _streamBuffer.clear();
            // _frameSkipCount = 0;
            // _bufferCountOffset = 1;
            //fillBuffer(OsEng.timeManager().time().deltaTime());
        } /*else {
            uploadImageDataToPBO();
        }*/

        _updatingCurrentActiveChannel = false;
    });

    _minRealTimeUpdateInterval.onChange([this]() {
        // _streamBuffer.clear();
        // _frameSkipCount = 0;
        // _bufferCountOffset = 1;
        clearBuffer();
    });

    _resolutionLevel.onChange([this]() {
        _updatingCurrentLevelOfResolution = true;
        _pboIsDirty = false;

        auto& stateSequence = _imageMetadataMap2[_currentActiveInstrument];
        auto& state = stateSequence.getState(OsEng.timeManager().time().j2000Seconds());
       // const double& timeObserved = state.timeObserved();
        std::shared_ptr<ImageMetadata> im = state.contents();
        //_imageSize = _imageMetadataMap2[_currentActiveInstrument]. //_fullResolution / (pow(2, _resolutionLevel));
        _imageSize = im->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)));

        //_imageSize = _fullResolution / (pow(2, _resolutionLevel));

        //_pbo->setSize(_imageSize * _imageSize * sizeof(IMG_PRECISION));
        updateTextureGPU(/*asyncUpload=*/false, /*resChanged=*/true);
        if (_useBuffering) {

           // LDEBUG("image size" << _imageSize);

            // _currentActiveImageTime
            //           = getDecodeDataFromOsTime(OsEng.timeManager().time().j2000Seconds())
            //                   .timeObserved;
            clearBuffer();
            // _streamBuffer.clear();
            // _frameSkipCount = 0;
            // _bufferCountOffset = 1;
            //fillBuffer(OsEng.timeManager().time().deltaTime());
        } /*else {
            uploadImageDataToPBO();
        }*/
       _updatingCurrentLevelOfResolution = false;
    });

    _moveFactor.onChange([this]() {
        //updatePlane();
        _spacecraftCameraPlane->createPlaneAndFrustum(_moveFactor);
    });

    _deltaTimeLast = 1.0;

    // if (_useBuffering) {
    //     fillBuffer(OsEng.timeManager().time().deltaTime());
    // }




    // _currentActiveImageTime
    //       = getDecodeDataFromOsTime(OsEng.timeManager().time().j2000Seconds())
    //               .timeObserved;

   // LDEBUG("Init current active image time " <<  SpiceManager::ref().dateFromEphemerisTime(OsEng.timeManager().time().j2000Seconds()));
    //LDEBUG("Init current active image time " << SpiceManager::ref().dateFromEphemerisTime(_currentActiveImageTime));

    // If no buffer is used this is needed
    _initializePBO = true;

    // Initialize PBO - not needed since buffer is filled async anyways
    // if (_usePBO) {
    //     LDEBUG("Initializing PBO with image " << _currentActiveImage);
    //     uploadImageDataToPBO(_currentActiveImage); // Begin fill PBO 1
    // }

    performImageTimestep(OsEng.timeManager().time().j2000Seconds());

    addProperty(_planeOpacity);
    addProperty(_enableBorder);
    addProperty(_enableFrustum);
    addProperty(_activeInstruments);
    addProperty(_sharpenValue);
    addProperty(_gammaValue);
    addProperty(_contrastValue);
    //addProperty(_bufferSize);
    addProperty(_displayTimers);
    addProperty(_planeSize);
    addProperty(_resolutionLevel);
    addProperty(_lazyBuffering);
    addProperty(_minRealTimeUpdateInterval);
    //addProperty(_asyncUploadPBO);
    addProperty(_useBuffering);
    addProperty(_usePBO);
    addProperty(_moveFactor);
    addProperty(_verboseMode);
}

// MUST do this conversion before passing in the spice manager again - WTF.
std::string RenderableSolarImagery::ISO8601(std::string& datetime) {
    std::string month = datetime.substr(5, 3);

    std::string MM = "";
    if (month == "JAN") MM = "01";
    else if (month == "FEB") MM = "02";
    else if (month == "MAR") MM = "03";
    else if (month == "APR") MM = "04";
    else if (month == "MAY") MM = "05";
    else if (month == "JUN") MM = "06";
    else if (month == "JUL") MM = "07";
    else if (month == "AUG") MM = "08";
    else if (month == "SEP") MM = "09";
    else if (month == "OCT") MM = "10";
    else if (month == "NOV") MM = "11";
    else if (month == "DEC") MM = "12";
    else ghoul_assert(false, "Bad month");

    datetime.replace(4, 5, "-" + MM + "-");
    return datetime;
}

void RenderableSolarImagery::clearBuffer() {
    _pboIsDirty = false;
    _streamBuffer.clear();
    _frameSkipCount = 0;
    _bufferCountOffset = 1;
    _busyPbos.clear();

    while (!_pboQueue.empty()) {
        _pboQueue.pop();
    }
}

void RenderableSolarImagery::loadMetadata(const std::string& rootPath) {

    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(rootPath, RawPath::Yes);
    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load directory '" << sequenceDir.path() << "'");
    }

    using Recursive = ghoul::filesystem::Directory::RawPath;
    using Sort = ghoul::filesystem::Directory::Sort;

    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::No, Sort::No);

    LDEBUG("Begin reading from files");
    for (auto& seqPath : sequencePaths) {
        if (size_t position = seqPath.find_last_of(".") + 1) {
            if (position != std::string::npos) {
                ghoul::filesystem::File currentFile(seqPath);
                std::string extension = currentFile.fileExtension();
                if (extension == "txt") {
                    LDEBUG("Loading instrument: " << currentFile.baseName());
                    std::ifstream myfile(currentFile.path());
                    if (!myfile.is_open()) {
                        LERROR("Failed to open metadata file");
                    }
                  //  std::string ca;
                    int numStates;
                    myfile >> numStates;

                    for (int i = 0; i < numStates; i++) {
                        ImageMetadata im;

                        myfile >> std::ws; // skip the rest of the line
                        std::string date;
                        std::getline(myfile, date);
                        double timeObserved = SpiceManager::ref().ephemerisTimeFromDate(ISO8601(date));

                        std::string relPath;
                        myfile >> relPath;
                        im.filename = rootPath + relPath;

                        myfile >> im.fullResolution;
                        myfile >> im.scale;

                        float x, y;
                        myfile >> x >> y;
                        im.centerPixel = glm::vec2(x,y);
                        myfile >> im.isCoronaGraph;
                        std::shared_ptr<ImageMetadata> data = std::make_shared<ImageMetadata>(im);
                        TimedependentState<ImageMetadata> timeState(
                                      std::move(data), timeObserved, im.filename);
                        _imageMetadataMap2[currentFile.baseName()].addState(std::move(timeState));
                    }
                    myfile.close();
                }
            }
        }
    }
}

void RenderableSolarImagery::saveMetadata(const std::string& rootPath) {
    for (auto& instrument : _imageMetadataMap2) {
        std::ofstream ofs(rootPath + instrument.first + ".txt");
        if (!ofs.is_open()) {
            LERROR("Failed to open file");
        }
        auto &sequence = instrument.second;
       // ofs << instrument.first << "\n";
        ofs << sequence.getNumStates() << "\n";
        for (const auto& metadata : sequence.getStates()) {
                ofs << SpiceManager::ref().dateFromEphemerisTime(metadata.timeObserved()) << "\n";
                auto im = metadata.contents();

                size_t filenamePos = im->filename.find("imagedata");
                std::string fname = im->filename.substr(filenamePos);
                ofs << fname << "\n";
                ofs << im->fullResolution << "\n";
                ofs << im->scale << "\n";
                ofs << im->centerPixel.x << "\n";
                ofs << im->centerPixel.y << "\n";
                ofs << im->isCoronaGraph << "\n";
        }
        ofs.close();
    }
}

DecodeData RenderableSolarImagery::getDecodeDataFromOsTime(const int& osTime) {
    auto& stateSequence = _imageMetadataMap2[_currentActiveInstrument];
    auto& state = stateSequence.getState(osTime);
    const double& timeObserved = state.timeObserved();
    std::shared_ptr<ImageMetadata> im = state.contents();
    //const std::string stateFilename = state.contents()->filename;
    //DecodeData decodeData {_imageSize * _imageSize, stateFilename, _resolutionLevel, _verboseMode, timeObserved};

    DecodeData decodeData {std::move(im), static_cast<unsigned int>(_resolutionLevel), timeObserved, _verboseMode};
    return decodeData;
}

// void RenderableSolarImagery::fillBuffer(const double& dt) {
//     _streamBuffer.clear();
//     if (_verboseMode) {
//         LDEBUG("Refilling Buffer. dt: " << dt);
//     }
//     const double& osTime = OsEng.timeManager().time().j2000Seconds();
//     const double& startTime = getDecodeDataFromOsTime(osTime).timeObserved;

//     for (int i = 1; i < _bufferSize; i++) {
//         const double& time = startTime + (dt * i) * (static_cast<double>(_minRealTimeUpdateInterval) / 1000.0);
//         DecodeData decodeData = getDecodeDataFromOsTime(time);
//         auto job = std::make_shared<DecodeJob>(decodeData, decodeData.im->filename + std::to_string(_imageSize));
//         _streamBuffer.enqueueJob(job);
//         if (_verboseMode) {
//             LDEBUG("Enqueueing " << decodeData.im->filename);
//         }
//     }
//     //_initializePBO = true;
// }

bool RenderableSolarImagery::isReady() const {
    return _spacecraftCameraPlane->isReady() && _texture != nullptr;
}

bool RenderableSolarImagery::initialize() {
    _spacecraftCameraPlane = std::make_unique<SpacecraftCameraPlane>(
          /*_magicPlaneOffset, _magicPlaneFactor,*/ _moveFactor);
    return isReady();
}

bool RenderableSolarImagery::deinitialize() {
    return _spacecraftCameraPlane->destroy();
}

void RenderableSolarImagery::uploadImageDataToPBO() {
  //  _pbo->activate();
  //  IMG_PRECISION* _pboBufferData = _pbo->mapToClientMemory<IMG_PRECISION>(/*shouldOrphanData=*/true, _imageSize * _imageSize * sizeof(IMG_PRECISION));

    if (!_useBuffering) {
        //const std::string filename = getDecodeDataFromOsTime(OsEng.timeManager().time().j2000Seconds()).im->filename;
        //decode(_pboBufferData, filename);
        //_pboIsDirty = true;
    } else {
        // WARNING - this can be an old job - but looks smoother - bug or feature?
        // const double& osTime = OsEng.timeManager().time().j2000Seconds();
        // std::shared_ptr<SolarImageData> _solarImageData = nullptr;
        // while (_streamBuffer._concurrentJobManager.numFinishedJobs() > 0) {
        //     _solarImageData = _streamBuffer.popFinishedJob();
        //     if (osTime <= _solarImageData->timeObserved) {
        //         break;
        //     }
        // }

        std::shared_ptr<SolarImageData> _solarImageData = _streamBuffer.popFinishedJob();
        //int pboId = _streamBuffer.numJobs();

        //LDEBUG("Popping job from PBO " << pboId);

        if (_solarImageData) {
            _currentActiveImageTime = _solarImageData->timeObserved;
            _currentScale = _solarImageData->im->scale;
            _currentCenterPixel = _solarImageData->im->centerPixel;
            _isCoronaGraph = _solarImageData->im->isCoronaGraph;

           // auto t1 = Clock::now();
           // std::memcpy(_pboBufferData, _solarImageData->data, _imageSize * _imageSize * sizeof(unsigned char));
           // auto t2 = Clock::now();

           // _uploadData = _solarImageData->data;
            _currentPbo = _pboQueue.front(); //_pbos[pboId].get();
            //LDEBUG("Popping job to pbo" << _currentPbo->id());


            // if (_displayTimers) {
            //     LDEBUG("Memcpy time "
            //            << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1)
            //                     .count()
            //            << " ms" << std::endl);
            // }

            _initializePBO = false;
            _pboIsDirty = true;

            if (_verboseMode)  {
                LDEBUG("Popped image" << _solarImageData->im->filename);
                //LDEBUG("Adding work from PBO " << decodeData.im->filename);
            }
        } else {
            if (_verboseMode) {
                LWARNING(_name << " -> Nothing to update, buffer is not ready, missing frames " << _frameSkipCount);
                _frameSkipCount++;
            }
        }
    }
    //_pbo->releaseMappedBuffer();
    //_pbo->deactivate();
}

void RenderableSolarImagery::updateTextureGPU(bool asyncUpload, bool resChanged) {
    if (_usePBO && asyncUpload) {
        _currentPbo->activate();
        _texture->bind();
        // Send async to GPU by coping from PBO to texture objects
        glTexSubImage2D(_texture->type(), 0, 0, 0, _imageSize, _imageSize,
                        GLint(_texture->format()), _texture->dataType(), nullptr);
        _currentPbo->deactivate();

        _busyPbos.erase(_currentPbo->id());
        _pboQueue.pop();
        _pboIsDirty = false;
    } else {
        unsigned char* data
              = new unsigned char[_imageSize * _imageSize * sizeof(IMG_PRECISION)];

        const double& osTime = OsEng.timeManager().time().j2000Seconds();
        const auto& decodeData = getDecodeDataFromOsTime(osTime);
        //const auto& decodeData = getDecodeDataFromOsTime(_currentActiveImageTime);
        decode(data, decodeData.im->filename);

        _currentScale = decodeData.im->scale;
        _currentCenterPixel = decodeData.im->centerPixel;

        _texture->bind();
        // if (!resChanged) {
        //     glTexSubImage2D(_texture->type(), 0, 0, 0, _imageSize, _imageSize,
        //                     GLint(_texture->format()), _texture->dataType(), data);
        // } else {
            glTexImage2D(_texture->type(), 0, _texture->internalFormat(), _imageSize,
                         _imageSize, 0, GLint(_texture->format()), _texture->dataType(),
                         data);
       // }
        delete[] data;
    }
}

void RenderableSolarImagery::decode(unsigned char* buffer, const std::string& filename)
{
    SimpleJ2kCodec j2c(_verboseMode);
    j2c.DecodeIntoBuffer(filename, buffer, _resolutionLevel);
    //KakaduWrapper w(_verboseMode);
    //w.DecodeIntoBuffer(filename, buffer, _resolutionLevel);
}

void RenderableSolarImagery::performImageTimestep(const double& osTime) {
    if (_pboIsDirty || !_usePBO) {
        updateTextureGPU();
    }

    bool stateChanged = _imageMetadataMap2[_currentActiveInstrument].hasStateChanged(osTime);
    // Time to pop from buffer !!! - And refill with buffer offset
    if (stateChanged || _initializePBO) {
        // Refill PBO
        // if (_verboseMode) {
        //     LDEBUG("Time to update image! ");
        // }
        if (_usePBO /*&& !_initializePBO*/) {
            uploadImageDataToPBO();
        }
    }
}

bool RenderableSolarImagery::checkBoundaries(const RenderData& data) {
    const glm::dvec3& normal = _spacecraftCameraPlane->normal();
    const glm::dvec3& cameraPosition = data.camera.positionVec3();
    const glm::dvec3& planePosition = _spacecraftCameraPlane->worldPosition();

    const glm::dvec3 toCamera = glm::normalize(cameraPosition - planePosition);
    if (glm::dot(toCamera, normal) < 0) {
        return false;
    }
    return true;
}

void RenderableSolarImagery::update(const UpdateData& data) {
    if (!isReady() || !isEnabled()) {
        return;
    }

    _realTime = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
    _realTimeDiff = _realTime.count() - _lastUpdateRealTime.count();
    if (_useBuffering) {
        const double& dt = OsEng.timeManager().time().deltaTime();
        // Delta time changed, need to refill buffer
        if ((std::abs(_deltaTimeLast - dt)) > EPSILON) {
            _pboIsDirty = false;
           // fillBuffer(dt);
            clearBuffer();
            // _frameSkipCount = 0;
            // _streamBuffer.clear();
            // _bufferCountOffset = 1;
            //LDEBUG("Clearing ... dt : " << dt);
            // _currentActiveImageTime
            //           = getDecodeDataFromOsTime(OsEng.timeManager().time().j2000Seconds())
            //                   .timeObserved;
        }
        _deltaTimeLast = dt;
    }

    if (/*_streamBuffer.numJobs() */_pboQueue.size() < /*_bufferSize*/ BUFFER_SIZE && (_isWithinFrustum || _initializePBO)) {
        // Always add to buffer faster than pop ..
        const double& osTime = OsEng.timeManager().time().j2000Seconds();
        // The min real time update interval doesnt make any sense
        DecodeData decodeData = getDecodeDataFromOsTime(osTime + (_bufferCountOffset /** _streamBuffer.numJobs()*/) * (OsEng.timeManager().time().deltaTime() * static_cast<double>(_minRealTimeUpdateInterval)/1000.0));
        //LDEBUG("Current active time " << SpiceManager::ref().dateFromEphemerisTime(_currentActiveImageTime));
        //LDEBUG("dt" << (_streamBuffer.numJobs()) * (OsEng.timeManager().time().deltaTime()));
        const std::string hash = decodeData.im->filename + std::to_string(_imageSize);

        // If job does not exist already and last popped time is not the same as the job trying to be enqueued
        if (!_streamBuffer.hasJob(hash) && _currentActiveImageTime != decodeData.timeObserved) {
            //LINFO("Pushing hash  " << hash);
            //LINFO("Pushing delta time  " << SpiceManager::ref().dateFromEphemerisTime(decodeData.timeObserved));

            //_pboQueue.push(_stream)
            // Get a free PBO, and add to Queue
            PixelBufferObject* pboToPush = getAvailablePbo(); //_pbos[_streamBuffer.numJobs()].get();
            pboToPush->activate();
            IMG_PRECISION* _pboBufferData = _pbo->mapToClientMemory<IMG_PRECISION>(/*shouldOrphanData=*/true, _imageSize * _imageSize * sizeof(IMG_PRECISION));

            // Give it a ready PBO
            auto job = std::make_shared<DecodeJob>(_pboBufferData, decodeData, decodeData.im->filename + std::to_string(_imageSize));
            _streamBuffer.enqueueJob(job);

            _pboQueue.push(pboToPush);
            pboToPush->releaseMappedBuffer();
            pboToPush->deactivate();
            //_bufferCountOffset = 0;
        } else {
            _bufferCountOffset++;
        }
    }

    _timeToUpdateTexture = _realTimeDiff > _minRealTimeUpdateInterval;

    // Update lookup table, TODO: No need to do this every update
    _lut = _tfMap[_currentActiveInstrument].get();
    _spacecraftCameraPlane->update();
}

PixelBufferObject* RenderableSolarImagery::getAvailablePbo() {
    for (int i = 0; i < BUFFER_SIZE; ++i) {
        if (_busyPbos.count(_pbos[i]->id()) == 0) {
            _busyPbos.insert(_pbos[i]->id());
            return _pbos[i].get();
        }
    }
    return nullptr;
}

void RenderableSolarImagery::render(const RenderData& data) {
    if (!isReady() || !isEnabled()) {
        return;
    }

    // if (!_isWithinFrustum) {
    //     _shouldRenderPlane = false;
    // } else {
    //     _shouldRenderPlane = true;
    // }

     _isWithinFrustum = checkBoundaries(data);
    if (_isWithinFrustumLast != _isWithinFrustum) {
        //_pboIsDirty = false;
        //fillBuffer(OsEng.timeManager().time().j2000Seconds());
        // _currentActiveImageTime
        //               = getDecodeDataFromOsTime(OsEng.timeManager().time().j2000Seconds())
        //                       .timeObserved;
        clearBuffer();
        // _streamBuffer.clear();
        // _bufferCountOffset = 1;
        // _frameSkipCount = 0;
        // if (!_isWithinFrustum) {
        //     _enableBorder = true;
        //     _enableFrustum = true;
        // } else {
        //     _enableBorder = false;
        //     _enableFrustum = false;
        // }
    }
    _isWithinFrustumLast = _isWithinFrustum;

    // Update texture
    // The bool blockers might probably not be needed now
    if (_timeToUpdateTexture && !_updatingCurrentLevelOfResolution
        && !_updatingCurrentActiveChannel && (_isWithinFrustum || _initializePBO || _pboIsDirty)) {
        performImageTimestep(OsEng.timeManager().time().j2000Seconds());
        _lastUpdateRealTime = _realTime;
    }

    // TODO: We want to create sun imagery node from within the module
    const glm::dvec3& sunPositionWorld
          = OsEng.renderEngine().scene()->sceneGraphNode("Sun")->worldPosition();

    _spacecraftCameraPlane->render(data, *_texture, _lut, sunPositionWorld, _planeOpacity,
                                   _contrastValue, _gammaValue, _enableBorder,
                                   _enableFrustum, _currentCenterPixel, _currentScale, _offset);
}

} // namespace openspace
