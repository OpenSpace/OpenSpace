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
#include <openspace/util/timemanager.h>
#include <openspace/scene/scene.h>
#include <modules/solarbrowsing/util/j2kcodec.h>

#include <chrono>
#include <math.h>

using namespace ghoul::opengl;
using namespace std::chrono;

typedef std::chrono::high_resolution_clock Clock;

namespace {
    static const std::string _loggerCat = "RenderableSolarImagery";
    const double EPSILON = std::numeric_limits<double>::epsilon();
    const unsigned int MAX_IMAGE_RESOLUTION = 4096;
}

namespace openspace {

RenderableSolarImagery::RenderableSolarImagery(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _activeInstruments("activeInstrument", "Active Instrument", properties::OptionProperty::DisplayType::Radio)
    , _contrastValue("contrastValue", "Contrast", 0.0, -15.0, 15.0)
    , _enableBorder("enableBorder", "Enable Border", false)
    , _enableFrustum("enableFrustum", "Enable Frustum", false)
    , _gammaValue("gammaValue", "Gamma", 0.9, 0.1, 10.0)
    , _minRealTimeUpdateInterval("minRealTimeUpdateInterval", "Min Update Interval", 65, 0, 300)
    , _moveFactor("moveFactor", "Move Factor" , 1.0, 0.0, 1.0)
    , _planeOpacity("planeOpacity", "Plane Opacity", 1.0, 0.0, 1.0)
    , _resolutionLevel("resolutionlevel", "Level of detail", 2, 0, 5)
    , _usePBO("usePBO", "Use PBO", true)
    , _verboseMode("verboseMode", "Verbose Mode", false)
{

    if (!dictionary.getValue("Name", _nodeName)) {
        throw ghoul::RuntimeError("Nodename has to be specified");
    }

    std::string rootPath;
    if (!dictionary.getValue("RootPath", rootPath)) {
        throw ghoul::RuntimeError("RootPath has to be specified");
    }

    float imagePlaneOffset;
    if (dictionary.getValue("imagePlaneOffset", imagePlaneOffset)) {
        _imagePlaneOffset = imagePlaneOffset;
    }

    SpacecraftImageryManager::ref().loadImageMetadata(rootPath, _imageMetadataMap);
    SpacecraftImageryManager::ref().loadTransferFunctions(rootPath + "/colortables", _tfMap);

    // Add GUI names
    unsigned int i = 0;
    for (auto& el : _imageMetadataMap) {
        _activeInstruments.addOption(i++, el.first);
    }

    _currentActiveInstrument
        = _activeInstruments.getDescriptionByValue(_activeInstruments.value());

    // Some sanity checks
    if (_imageMetadataMap.size() == 0) {
        LERROR("Images map is empty! Check your path");
    }

    // Initialize PBO's
    for (int i = 0; i < SOLAR_BUFFER_SIZE; ++i) {
        _pbos[i] = std::make_unique<PixelBufferObject>(MAX_IMAGE_RESOLUTION * MAX_IMAGE_RESOLUTION * sizeof(IMG_PRECISION));
    }

    // Get first image size
    auto& stateSequenceStart = _imageMetadataMap[_currentActiveInstrument];
    auto& stateStart = stateSequenceStart.getState(OsEng.timeManager().time().j2000Seconds());
    std::shared_ptr<ImageMetadata> imStart = stateStart.contents();
    _imageSize = imStart->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)));

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

    performImageTimestep(OsEng.timeManager().time().j2000Seconds());
    listen();

    addProperty(_planeOpacity);
    addProperty(_enableBorder);
    addProperty(_enableFrustum);
    addProperty(_activeInstruments);
    addProperty(_gammaValue);
    addProperty(_contrastValue);
    addProperty(_resolutionLevel);
    addProperty(_minRealTimeUpdateInterval);
    addProperty(_usePBO);
    addProperty(_moveFactor);
    addProperty(_verboseMode);
}

void RenderableSolarImagery::listen() {
    _enableFrustum.onChange([this]() {
        if (_enableFrustum) {
            _enableBorder = true;
        } else {
            _enableBorder = false;
        }
    });

    _activeInstruments.onChange([this]() {
        _pboIsDirty = false;
        _currentActiveInstrument
               = _activeInstruments.getDescriptionByValue(_activeInstruments.value());
        // Update image size
        auto& stateSequence = _imageMetadataMap[_currentActiveInstrument];
        auto& state = stateSequence.getState(OsEng.timeManager().time().j2000Seconds());
        std::shared_ptr<ImageMetadata> im = state.contents();
        _imageSize = im->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)));

        // Upload asap
        updateTextureGPU(/*asyncUpload=*/false);
        clearBuffer();
    });

    _minRealTimeUpdateInterval.onChange([this]() {
        clearBuffer();
    });

    _resolutionLevel.onChange([this]() {
        _pboIsDirty = false;
        // Update image size
        auto& stateSequence = _imageMetadataMap[_currentActiveInstrument];
        auto& state = stateSequence.getState(OsEng.timeManager().time().j2000Seconds());
        std::shared_ptr<ImageMetadata> im = state.contents();
        _imageSize = im->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)));

        // Upload asap
        updateTextureGPU(/*asyncUpload=*/false, /*resChanged=*/true);
        clearBuffer();
    });

    _moveFactor.onChange([this]() {
        _spacecraftCameraPlane->createPlaneAndFrustum(_moveFactor);
    });

    _deltaTimeLast = 1.0;
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

DecodeData RenderableSolarImagery::getDecodeDataFromOsTime(const int& osTime) {
    auto& stateSequence = _imageMetadataMap[_currentActiveInstrument];
    auto& state = stateSequence.getState(osTime);
    const double& timeObserved = state.timeObserved();
    std::shared_ptr<ImageMetadata> im = state.contents();

    DecodeData decodeData {std::move(im), static_cast<unsigned int>(_resolutionLevel), timeObserved, _verboseMode};
    return decodeData;
}

bool RenderableSolarImagery::isReady() const {
    return _spacecraftCameraPlane->isReady() && _texture != nullptr;
}

bool RenderableSolarImagery::initialize() {
    _spacecraftCameraPlane = std::make_unique<SpacecraftCameraPlane>(_moveFactor);
    return isReady();
}

bool RenderableSolarImagery::deinitialize() {
    return _spacecraftCameraPlane->destroy();
}

void RenderableSolarImagery::uploadImageDataToPBO() {
    std::shared_ptr<SolarImageData> _solarImageData = _streamBuffer.popFinishedJob();

    if (_solarImageData) {
        _currentActiveImageTime = _solarImageData->timeObserved;
        _currentScale = _solarImageData->im->scale;
        _currentCenterPixel = _solarImageData->im->centerPixel;
        _isCoronaGraph = _solarImageData->im->isCoronaGraph;

        _currentPbo = _pboQueue.front();
        _initializePBO = false;
        _pboIsDirty = true;

        if (_verboseMode)  {
            LDEBUG("Popped image" << _solarImageData->im->filename);
        }
    } else {
        if (_verboseMode) {
            LWARNING(_nodeName << " -> Nothing to update, buffer is not ready, missing frames " << _frameSkipCount);
            _frameSkipCount++;
        }
    }
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
    } else { // Synchronous "normal" texture upload
        unsigned char* data
              = new unsigned char[_imageSize * _imageSize * sizeof(IMG_PRECISION)];
        const double& osTime = OsEng.timeManager().time().j2000Seconds();
        const auto& decodeData = getDecodeDataFromOsTime(osTime);
        decode(data, decodeData.im->filename);

        _currentScale = decodeData.im->scale;
        _currentCenterPixel = decodeData.im->centerPixel;

        _texture->bind();
        glTexImage2D(_texture->type(), 0, _texture->internalFormat(), _imageSize,
                     _imageSize, 0, GLint(_texture->format()), _texture->dataType(),
                     data);
        delete[] data;
    }
     _pboIsDirty = false;
}

void RenderableSolarImagery::decode(unsigned char* buffer, const std::string& filename) {
    J2kCodec j2c(_verboseMode);
    j2c.decodeIntoBuffer(filename, buffer, _resolutionLevel);
}

void RenderableSolarImagery::performImageTimestep(const double& osTime) {
    if (_pboIsDirty) {
        updateTextureGPU();
    }
    const bool stateChanged = _imageMetadataMap[_currentActiveInstrument].hasStateChanged(osTime);

    // Time to pop from buffer!
    if (stateChanged || _initializePBO) {
        if (!_usePBO) {
            updateTextureGPU();
        } else { // Refill PBO
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
    const double& dt = OsEng.timeManager().time().deltaTime();

    // Delta time changed, need to refill buffer
    if ((std::abs(_deltaTimeLast - dt)) > EPSILON) {
        _pboIsDirty = false;
        clearBuffer();
        _deltaTimeLast = dt;
    }

    // Continuously fill buffer
    if (_usePBO && _pboQueue.size() < SOLAR_BUFFER_SIZE && (_isWithinFrustum || _initializePBO)) {
        // Always add to buffer faster than pop ..
        const double& osTime = OsEng.timeManager().time().j2000Seconds();
        DecodeData decodeData = getDecodeDataFromOsTime(osTime + _bufferCountOffset * (OsEng.timeManager().time().deltaTime() * static_cast<double>(_minRealTimeUpdateInterval)/1000.0));
        const std::string hash = decodeData.im->filename + std::to_string(_imageSize);

        // If job does not exist already and last popped time is not the same as the job trying to be enqueued
        if (!_streamBuffer.hasJob(hash) && _currentActiveImageTime != decodeData.timeObserved) {
            // Get an available PBO, and add to Queue
            PixelBufferObject* pboToPush = getAvailablePbo();
            pboToPush->activate();
            IMG_PRECISION* _pboBufferData = pboToPush->mapToClientMemory<IMG_PRECISION>(/*shouldOrphanData=*/true, _imageSize * _imageSize * sizeof(IMG_PRECISION));

            auto job = std::make_shared<DecodeJob>(_pboBufferData, decodeData, decodeData.im->filename + std::to_string(_imageSize));
            _streamBuffer.enqueueJob(job);

            _pboQueue.push(pboToPush);
            pboToPush->releaseMappedBuffer();
            pboToPush->deactivate();
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
    for (int i = 0; i < SOLAR_BUFFER_SIZE; ++i) {
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

     _isWithinFrustum = checkBoundaries(data);
    if (_isWithinFrustumLast != _isWithinFrustum) {
        clearBuffer();
    }
    _isWithinFrustumLast = _isWithinFrustum;

    // Update texture
    if (_timeToUpdateTexture /*&& !_updatingCurrentLevelOfResolution
        && !_updatingCurrentActiveChannel */ && (_isWithinFrustum || _initializePBO || _pboIsDirty)) {
        performImageTimestep(OsEng.timeManager().time().j2000Seconds());
        _lastUpdateRealTime = _realTime;
    }

    const glm::dvec3& sunPositionWorld
          = OsEng.renderEngine().scene()->sceneGraphNode("Sun")->worldPosition();
    _spacecraftCameraPlane->render(data, *_texture, _lut, sunPositionWorld, _planeOpacity,
                                   _contrastValue, _gammaValue, _enableBorder,
                                   _enableFrustum, _currentCenterPixel, _currentScale, _imagePlaneOffset, _isCoronaGraph);
}

} // namespace openspace
