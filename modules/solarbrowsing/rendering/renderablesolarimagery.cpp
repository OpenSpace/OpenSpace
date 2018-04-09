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

#include <modules/solarbrowsing/rendering/renderablesolarimagery.h>

#include <modules/solarbrowsing/rendering/spacecraftcameraplane.h>
#include <modules/solarbrowsing/util/pixelbufferobject.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/timemanager.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/defer.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <chrono>

namespace {
    constexpr const char* _loggerCat = "RenderableSolarImagery";
    constexpr const char* KeyStartInstrument = "StartInstrument";


    constexpr const unsigned int MaxImageResolution = 4096;

    static const openspace::properties::Property::PropertyInfo ActiveInstrumentsInfo = {
        "activeInstrument",
        "Active instrument",
        "The active instrument of the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo ContrastValueInfo = {
        "contrastValue",
        "Contrast",
        "Contrast of the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo EnableBorderInfo = {
        "enableBorder",
        "Enable Border",
        "Enables border around the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo EnableFrustumInfo = {
        "enableFrustum",
        "Enable frustum",
        "Enables frustum around the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo GammaValueInfo = {
        "gammaValue",
        "Gamma",
        "Gamma of the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo MinRealTimeUpdateIntervalInfo = {
        "minRealTimeUpdateInterval",
        "Min Update Interval",
        "@TODO"
    };
    static const openspace::properties::Property::PropertyInfo MoveFactorInfo = {
        "moveFactor",
        "Move Factor",
        "@TODO"
    };
    static const openspace::properties::Property::PropertyInfo PlaneOpacityInfo = {
        "planeOpacity",
        "Plane Opacity",
        "@TODO"
    };
    static const openspace::properties::Property::PropertyInfo ResolutionLevelInfo = {
        "resolutionLevel",
        "Resolution Level",
        "@TODO"
    };
    static const openspace::properties::Property::PropertyInfo UsePBOInfo = {
        "usePBO",
        "Use PBO",
        "@TODO"
    };
    static const openspace::properties::Property::PropertyInfo VerboseModeInfo = {
        "verboseMode",
        "Verbose Mode",
        "@TODO"
    };
}

namespace openspace {

RenderableSolarImagery::RenderableSolarImagery(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _activeInstruments(ActiveInstrumentsInfo)
    , _contrastValue(ContrastValueInfo, 0.f, -15.f, 15.f)
    , _enableBorder(EnableBorderInfo, false)
    , _enableFrustum(EnableFrustumInfo, false)
    , _gammaValue(GammaValueInfo, 0.9f, 0.1f, 10.f)
    , _minRealTimeUpdateInterval(MinRealTimeUpdateIntervalInfo, 65, 0, 300)
    , _moveFactor(MoveFactorInfo, 1.0, 0.0, 1.0)
    , _planeOpacity(PlaneOpacityInfo, 1.f, 0.f, 1.f)
    , _resolutionLevel(ResolutionLevelInfo, 2, 0, 5)
    , _usePBO(UsePBOInfo, true)
    , _verboseMode(VerboseModeInfo, false)
{
    std::string rootPath;
    if (!dictionary.getValue("RootPath", rootPath)) {
        throw ghoul::RuntimeError("RootPath has to be specified");
    }
    
    std::string transferfunctionPath;
    if (dictionary.getValue("TransferfunctionPath", transferfunctionPath)) {
        SpacecraftImageryManager::ref().loadTransferFunctions(transferfunctionPath, _tfMap);
    }

    SpacecraftImageryManager::ref().loadImageMetadata(rootPath, _imageMetadataMap);

    // Add GUI names
    unsigned int guiNameCount = 0;
    using K = std::string;
    using V = SpacecraftImageryManager::ImageMetadataStateSequence;
    for (const std::pair<K, V>& el : _imageMetadataMap) {
        _activeInstruments.addOption(guiNameCount++, el.first);
    }

    if (dictionary.hasKey(KeyStartInstrument)) {
        _currentActiveInstrument = dictionary.value<std::string>(KeyStartInstrument);
    }
    else {
        _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
            _activeInstruments
        );
    }
    // Some sanity checks
    if (_imageMetadataMap.empty()) {
        LERROR("Images map is empty! Check your path");
    }

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
        _enableBorder.setValue(_enableFrustum.value());
    });

    _activeInstruments.onChange([this]() {
        _pboIsDirty = false;
        _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
            _activeInstruments
        );
        // Update image size
        const SpacecraftImageryManager::ImageMetadataStateSequence& stateSequence =
            _imageMetadataMap[_currentActiveInstrument];
        const TimedependentState<ImageMetadata>& state = stateSequence.state(
            OsEng.timeManager().time().j2000Seconds()
        );
        std::shared_ptr<ImageMetadata> im = state.contents();
        _imageSize = static_cast<unsigned int>(
            im->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)))
        );

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
        const SpacecraftImageryManager::ImageMetadataStateSequence& stateSequence =
            _imageMetadataMap[_currentActiveInstrument];
        const TimedependentState<ImageMetadata>& state = stateSequence.state(
            OsEng.timeManager().time().j2000Seconds()
        );
        std::shared_ptr<ImageMetadata> im = state.contents();
        _imageSize = static_cast<unsigned int>(
            im->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)))
        );

        // Upload asap
        updateTextureGPU(/*asyncUpload=*/false, /*resChanged=*/true);
        clearBuffer();
    });

    _moveFactor.onChange([this]() {
        _spacecraftCameraPlane->createPlaneAndFrustum(_moveFactor);
    });

    _deltaTimeLast = 1.0;
}

void RenderableSolarImagery::initializeGL() {
    // Initialize PBO's
    for (size_t i = 0; i < SolarBufferSize; ++i) {
        _pbos[i] = std::make_unique<PixelBufferObject>(
            MaxImageResolution * MaxImageResolution * sizeof(IMG_PRECISION)
        );
    }

    // Get first image size
    const SpacecraftImageryManager::ImageMetadataStateSequence& stateSequenceStart =
        _imageMetadataMap[_currentActiveInstrument];
    const TimedependentState<ImageMetadata>& stateStart = stateSequenceStart.state(
        OsEng.timeManager().time().j2000Seconds()
    );

    std::shared_ptr<ImageMetadata> imStart = stateStart.contents();
    _imageSize = imStart->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)));

    _texture = std::make_unique<ghoul::opengl::Texture>(
        nullptr,
        glm::uvec3(_imageSize, _imageSize, 1),
        ghoul::opengl::Texture::Format::Red, // Format of the pixeldata
        GL_R8, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
        GL_UNSIGNED_BYTE, // Type of data
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
        );

    _texture->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
    _texture->uploadTexture();

    // Initialize time
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch()
    );
    _lastUpdateRealTime = _realTime;

    performImageTimestep(OsEng.timeManager().time().j2000Seconds());
    listen();

    _spacecraftCameraPlane = std::make_unique<SpacecraftCameraPlane>(_moveFactor);
}

void RenderableSolarImagery::deinitializeGL() {
    _spacecraftCameraPlane->destroy();
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

TransferFunction* RenderableSolarImagery::getTransferFunction() {
    return _lut;
}

const std::unique_ptr<ghoul::opengl::Texture>& RenderableSolarImagery::getImageryTexture()
{
    return _texture;
}

const SpacecraftCameraPlane& RenderableSolarImagery::getCameraPlane() {
    return *_spacecraftCameraPlane;
}

float RenderableSolarImagery::getContrastValue() {
    return _contrastValue;
}

float RenderableSolarImagery::getGammaValue() {
    return _gammaValue;
}

float RenderableSolarImagery::getImageResolutionFactor() {
    return _imageSize;
}

glm::vec2 RenderableSolarImagery::getCenterPixel() {
    return _currentCenterPixel;
}

float RenderableSolarImagery::getScale() {
    return _currentScale;
}

bool RenderableSolarImagery::isCoronaGraph() {
    return _isCoronaGraph;
}

DecodeData RenderableSolarImagery::getDecodeDataFromOsTime(int osTime) {
    const SpacecraftImageryManager::ImageMetadataStateSequence& stateSequence =
        _imageMetadataMap[_currentActiveInstrument];
    const TimedependentState<ImageMetadata>& state = stateSequence.state(osTime);

    double timeObserved = state.timeObserved();
    std::shared_ptr<ImageMetadata> im = state.contents();

    DecodeData decodeData {
        std::move(im),
        static_cast<unsigned int>(_resolutionLevel),
        timeObserved,
        _verboseMode
    };
    return decodeData;
}

bool RenderableSolarImagery::isReady() const {
    return _spacecraftCameraPlane->isReady() && _texture != nullptr;
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
            LDEBUG(fmt::format("Popped image {}", _solarImageData->im->filename));
        }
    } else {
        if (_verboseMode) {
            LWARNING(fmt::format(
                "{} -> Nothing to update, buffer is not ready, missing frames {}",
                _nodeName,
                _frameSkipCount
            ));
            _frameSkipCount++;
        }
    }
}

void RenderableSolarImagery::updateTextureGPU(bool asyncUpload, bool resChanged) {
    if (_usePBO && asyncUpload) {
        _currentPbo->activate();
        _texture->bind();
        // Send async to GPU by coping from PBO to texture objects
        glTexSubImage2D(
            _texture->type(),
            0,
            0,
            0,
            _imageSize,
            _imageSize,
            GL_RED,
            _texture->dataType(),
            nullptr
        );
        _currentPbo->deactivate();

        _busyPbos.erase(_currentPbo->id());
        _pboQueue.pop();
    } else { // Synchronous "normal" texture upload
        unsigned char* data = new unsigned char[
            _imageSize * _imageSize * sizeof(IMG_PRECISION)
        ];
        defer { delete[] data; };
        double osTime = OsEng.timeManager().time().j2000Seconds();
        const DecodeData& decodeData = getDecodeDataFromOsTime(osTime);
        decode(data, decodeData.im->filename);

        _currentScale = decodeData.im->scale;
        _currentCenterPixel = decodeData.im->centerPixel;

        _texture->bind();
        glTexImage2D(
            _texture->type(),
            0,
            _texture->internalFormat(),
            _imageSize,
            _imageSize,
            0,
            GL_RED,
            _texture->dataType(),
            data
        );
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
    const bool stateChanged = _imageMetadataMap[_currentActiveInstrument].hasStateChanged(
        osTime
    );

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
    //if (!isReady() || !isEnabled()) {
    //    return;
    //}

    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch()
    );
    _realTimeDiff = _realTime.count() - _lastUpdateRealTime.count();
    double dt = data.time.deltaTime();

    // Delta time changed, need to refill buffer
    if ((std::abs(_deltaTimeLast - dt)) > std::numeric_limits<double>::epsilon()) {
        _pboIsDirty = false;
        clearBuffer();
        _deltaTimeLast = dt;
    }

    // Continuously fill buffer
    if (_usePBO && _pboQueue.size() < SolarBufferSize &&
        (_isWithinFrustum || _initializePBO))
    {
        // Always add to buffer faster than pop ..
        double osTime = data.time.j2000Seconds();
        DecodeData decodeData = getDecodeDataFromOsTime(
            osTime + _bufferCountOffset * (dt * _minRealTimeUpdateInterval / 1000.0)
        );
        const std::string hash = decodeData.im->filename + std::to_string(_imageSize);

        // If job does not exist already and last popped time is not the same as the job trying to be enqueued
        if (!_streamBuffer.hasJob(hash) &&
            _currentActiveImageTime != decodeData.timeObserved)
        {
            // Get an available PBO, and add to Queue
            PixelBufferObject* pboToPush = getAvailablePbo();
            pboToPush->activate();
            IMG_PRECISION* _pboBufferData = pboToPush->mapToClientMemory<IMG_PRECISION>(
                /*shouldOrphanData=*/true,
                _imageSize * _imageSize * sizeof(IMG_PRECISION)
            );

            auto job = std::make_shared<DecodeJob>(
                _pboBufferData,
                decodeData,
                decodeData.im->filename + std::to_string(_imageSize)
            );
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
    for (int i = 0; i < SolarBufferSize; ++i) {
        if (_busyPbos.count(_pbos[i]->id()) == 0) {
            _busyPbos.insert(_pbos[i]->id());
            return _pbos[i].get();
        }
    }
    return nullptr;
}

void RenderableSolarImagery::render(const RenderData& data, RendererTasks&) {
    //if (!isReady() || !isEnabled()) {
    //    return;
    //}

     _isWithinFrustum = checkBoundaries(data);
    if (_isWithinFrustumLast != _isWithinFrustum) {
        clearBuffer();
    }
    _isWithinFrustumLast = _isWithinFrustum;

    // Update texture
    if (_timeToUpdateTexture /*&& !_updatingCurrentLevelOfResolution
        && !_updatingCurrentActiveChannel */ &&
        (_isWithinFrustum || _initializePBO || _pboIsDirty))
    {
        performImageTimestep(OsEng.timeManager().time().j2000Seconds());
        _lastUpdateRealTime = _realTime;
    }


    const glm::dvec3& sunPositionWorld = sceneGraphNode("Sun")->worldPosition();
    _spacecraftCameraPlane->render(
        data,
        *_texture,
        _lut,
        sunPositionWorld,
        _planeOpacity,
        _contrastValue,
        _gammaValue,
        _enableBorder,
        _enableFrustum,
        _currentCenterPixel,
        _currentScale,
        _imagePlaneOffset,
        _isCoronaGraph
    );
}

} // namespace openspace
