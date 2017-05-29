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
#include <modules/solarbrowsing/rendering/renderablespacecraftcameraplane.h>
#include <modules/solarbrowsing/rendering/renderablespacecraftcamerasphere.h>
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
#include <chrono>
#include <math.h>

using namespace ghoul::opengl;
using namespace std::chrono;

typedef std::chrono::high_resolution_clock Clock;

namespace {
    static const std::string _loggerCat = "RenderableSpacecraftCameraPlane";
    double HALF_SUN_RADIUS = (1391600000.0 * 0.50); // Half sun radius divided by magic factor
    const double EPSILON = std::numeric_limits<double>::epsilon();
}

namespace openspace {

RenderableSpacecraftCameraPlane::RenderableSpacecraftCameraPlane(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _sharpenValue("sharpenValue", "Sharpen", 0.0, 0.0, 1.0)
    , _contrastValue("contrastValue", "Contrast", 0.0, -15.0, 15.0)
    , _planeOpacity("planeOpacity", "Plane Opacity", 1.0, 0.0, 1.0)
    , _disableFrustum("disableFrustum", "Disable Frustum", true)
    , _disableBorder("disableBorder", "Disable Border", true)
    , _gammaValue("gammaValue", "Gamma", 0.9, 0.1, 10.0)
    , _asyncUploadPBO("asyncUploadPBO", "Upload to PBO Async", true)
    , _activeInstruments("activeInstrument", "Active Instrument", properties::OptionProperty::DisplayType::Radio)
    , _bufferSize("bufferSize", "Buffer Size", 15, 1, 100)
    , _displayTimers("displayTimers", "Display Timers", false)
    , _lazyBuffering("lazyBuffering", "Lazy Buffering", true)
    , _minRealTimeUpdateInterval("minRealTimeUpdateInterval", "Min Update Interval", 85, 0, 300)
    , _moveFactor("movefactor", "Move Factor" , 1.0, 0.0, 1.0)
    , _resolutionLevel("resolutionlevel", "Level of detail", 3, 0, 5)
    , _target("target", "Target", "Sun")
    , _useBuffering("useBuffering", "Use Buffering", true)
    , _usePBO("usePBO", "Use PBO", true)
    , _planeSize("planeSize", "Plane Size", 50.0, 0.0, 1.0)
    , _verboseMode("verboseMode", "Verbose Mode", false)
{
    std::string target;
    if (dictionary.getValue("Target", target)) {
        _target = target;
    }

    glm::dvec2 magicPlaneOffset;
    if (dictionary.getValue("MagicOffsetFromCenter", magicPlaneOffset)) {
        _magicPlaneOffset = magicPlaneOffset;
    }

    float tmpStartResolutionLevel;
    if (!dictionary.getValue("StartResolutionLevel", tmpStartResolutionLevel)) {
        throw ghoul::RuntimeError("Plane must have a starting resolution level");
    }
    _resolutionLevel = static_cast<int>(tmpStartResolutionLevel);

    if (!dictionary.getValue("MagicPlaneFactor", _magicPlaneFactor)) {
        throw ghoul::RuntimeError("Plane must at the moment have a magic factor");
    }

    std::string rootPath;
    if (!dictionary.getValue("RootPath", rootPath)) {
        throw ghoul::RuntimeError("RootPath has to be specified");
    }

    //TODO(mnoven): Can't pass in an int to dictionary?
    float tmpResolution;
    if (!dictionary.getValue("Resolution", tmpResolution)){
        throw ghoul::RuntimeError("Resolution has to be specified");
    }
    _fullResolution = static_cast<unsigned int>(tmpResolution);

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

    SpacecraftImageryManager::ref().loadImageMetadata(rootPath, _imageMetadataMap2, _instrumentFilter);

    // Add GUI names
    unsigned int i = 0;
    for (auto& el : _imageMetadataMap2) {
        _activeInstruments.addOption(i++, el.first);
    }

    _currentActiveInstrument
          = _activeInstruments.getDescriptionByValue(_activeInstruments.value());

    std::string tfRootPath;
    if (dictionary.getValue("TransferfunctionPath", tfRootPath)) {
        SpacecraftImageryManager::ref().loadTransferFunctions(tfRootPath, _tfMap, _instrumentFilter);
        //throw ghoul::RuntimeError("TransferfunctionPath has to be specified");
    }

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

    _imageSize = _fullResolution / (pow(2, _resolutionLevel));
    _pbo = std::make_unique<PixelBufferObject>(_imageSize * _imageSize * sizeof(IMG_PRECISION));

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

    _activeInstruments.onChange([this]() {
        _pboIsDirty = false;
        _currentActiveInstrument
               = _activeInstruments.getDescriptionByValue(_activeInstruments.value());
        // Upload asap
        updateTextureGPU(/*asyncUpload=*/false);
        if (_useBuffering) {
            fillBuffer(Time::ref().deltaTime());
        } /*else {
            uploadImageDataToPBO();
        }*/
    });

    _resolutionLevel.onChange([this]() {
        _pboIsDirty = false;
        _imageSize = _fullResolution / (pow(2, _resolutionLevel));
        _pbo->setSize(_imageSize * _imageSize * sizeof(IMG_PRECISION));
        updateTextureGPU(/*asyncUpload=*/false, /*resChanged=*/true);
        if (_useBuffering) {
            fillBuffer(Time::ref().deltaTime());
        } /*else {
            uploadImageDataToPBO();
        }*/
       // _updatingCurrentLevelOfResolution = false;
    });

    _moveFactor.onChange([this]() {
        //updatePlane();
        _spacecraftCameraPlane->createPlaneAndFrustum(_moveFactor);
    });

    _deltaTimeLast = 1.0;

    if (_useBuffering) {
        fillBuffer(Time::ref().j2000Seconds());
    }

    // If no buffer is used this is needed
    _initializePBO = true;

    // Initialize PBO - not needed since buffer is filled async anyways
    // if (_usePBO) {
    //     LDEBUG("Initializing PBO with image " << _currentActiveImage);
    //     uploadImageDataToPBO(_currentActiveImage); // Begin fill PBO 1
    // }

    performImageTimestep(Time::ref().j2000Seconds());

    addProperty(_planeOpacity);
    addProperty(_disableBorder);
    addProperty(_disableFrustum);
    addProperty(_activeInstruments);
    addProperty(_sharpenValue);
    addProperty(_gammaValue);
    addProperty(_contrastValue);
    addProperty(_bufferSize);
    addProperty(_displayTimers);
    addProperty(_planeSize);
    addProperty(_resolutionLevel);
    addProperty(_lazyBuffering);
    addProperty(_minRealTimeUpdateInterval);
    //addProperty(_asyncUploadPBO);
    addProperty(_useBuffering);
    addProperty(_usePBO);
    addProperty(_target);
    addProperty(_moveFactor);
    addProperty(_verboseMode);
}

DecodeData RenderableSpacecraftCameraPlane::getDecodeDataFromOsTime(const int& osTime) {
    auto& stateSequence = _imageMetadataMap2[_currentActiveInstrument];
    auto& state = stateSequence.getState(osTime);
    const double& timeObserved = state.timeObserved();
    const std::string stateFilename = state.contents()->filename;
    DecodeData decodeData {_imageSize * _imageSize, stateFilename, _resolutionLevel, _verboseMode, timeObserved};
    return std::move(decodeData);
}

void RenderableSpacecraftCameraPlane::fillBuffer(const double& dt) {
    _streamBuffer.clear();
    if (_verboseMode) {
        LDEBUG("Refilling Buffer. dt: " << dt);
    }
    const double& osTime = Time::ref().j2000Seconds();
    const double& startTime = getDecodeDataFromOsTime(osTime).timeObserved;

    for (int i = 0; i < _bufferSize; i++) {
        const double& time = startTime + (dt * i) * (static_cast<double>(_minRealTimeUpdateInterval) / 1000.0);
        DecodeData decodeData = getDecodeDataFromOsTime(time);
        auto job = std::make_shared<DecodeJob>(decodeData, decodeData.path + std::to_string(decodeData.totalImageSize));
        _streamBuffer.enqueueJob(job);
        if (_verboseMode) {
            LDEBUG("Enqueueing " << decodeData.path);
        }
    }
    _initializePBO = true;
}

bool RenderableSpacecraftCameraPlane::isReady() const {
    return _spacecraftCameraPlane->isReady() && _texture != nullptr;
}

bool RenderableSpacecraftCameraPlane::initialize() {
    _spacecraftCameraPlane = std::make_unique<SpacecraftCameraPlane>(
          _magicPlaneOffset, _magicPlaneFactor, _moveFactor);
    return isReady();
}

bool RenderableSpacecraftCameraPlane::deinitialize() {
    return _spacecraftCameraPlane->destroy();
}

void RenderableSpacecraftCameraPlane::uploadImageDataToPBO() {
    _pbo->activate();
    IMG_PRECISION* _pboBufferData = _pbo->mapToClientMemory<IMG_PRECISION>(/*shouldOrphanData=*/true);

    if (!_useBuffering) {
        const std::string filename = getDecodeDataFromOsTime(Time::ref().j2000Seconds()).path;
        decode(_pboBufferData, filename);
        _pboIsDirty = true;
    } else {
        std::shared_ptr<SolarImageData> _solarImageData = _streamBuffer.popFinishedJob();
        if (_solarImageData) {
            auto t1 = Clock::now();
            std::memcpy(_pboBufferData, _solarImageData->data, _imageSize * _imageSize * sizeof(unsigned char));
            auto t2 = Clock::now();

            if (_displayTimers) {
                LDEBUG("Memcpy time "
                       << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1)
                                .count()
                       << " ms" << std::endl);
            }

            const double& osTime = Time::ref().j2000Seconds();
            DecodeData decodeData = getDecodeDataFromOsTime(osTime + (_bufferSize + 1) * (Time::ref().deltaTime() * (static_cast<double>(_minRealTimeUpdateInterval )/1000.0)));
            auto job = std::make_shared<DecodeJob>(decodeData, decodeData.path + std::to_string(decodeData.totalImageSize));
            _streamBuffer.enqueueJob(job);
            _initializePBO = false;
            _pboIsDirty = true;

            if (_verboseMode)  {
                LDEBUG("Popped image" << _solarImageData->name);
                LDEBUG("Adding work from PBO " << decodeData.path);
            }
        } else {
            if (_verboseMode) {
                LWARNING("Nothing to update, buffer is not ready");
            }
        }
    }
    _pbo->releaseMappedBuffer();
    _pbo->deactivate();
}

void RenderableSpacecraftCameraPlane::updateTextureGPU(bool asyncUpload, bool resChanged) {
    if (_usePBO && asyncUpload) {
            _pbo->activate();
            _texture->bind();
            // Send async to GPU by coping from PBO to texture objects
            glTexSubImage2D(_texture->type(), 0, 0, 0, _imageSize, _imageSize,
                            GLint(_texture->format()), _texture->dataType(), nullptr);
            _pbo->deactivate();
            _pboIsDirty = false;
    } else {
        unsigned char* data
              = new unsigned char[_imageSize * _imageSize * sizeof(IMG_PRECISION)];
        const double& osTime = Time::ref().j2000Seconds();
        const auto& decodeData = getDecodeDataFromOsTime(osTime);
        decode(data, decodeData.path);

        _texture->bind();
        if (!resChanged) {
            glTexSubImage2D(_texture->type(), 0, 0, 0, _imageSize, _imageSize,
                            GLint(_texture->format()), _texture->dataType(), data);
        } else {
            glTexImage2D(_texture->type(), 0, _texture->internalFormat(), _imageSize,
                         _imageSize, 0, GLint(_texture->format()), _texture->dataType(),
                         data);
        }
        delete[] data;
    }
}

void RenderableSpacecraftCameraPlane::decode(unsigned char* buffer,
                                             const std::string& filename)
{
    SimpleJ2kCodec j2c(_verboseMode);
    j2c.DecodeIntoBuffer(filename, buffer, _resolutionLevel);
}

void RenderableSpacecraftCameraPlane::performImageTimestep(const double& osTime) {
    if (_pboIsDirty || !_usePBO) {
        updateTextureGPU();
    }
    bool stateChanged = _imageMetadataMap2[_currentActiveInstrument].hasStateChanged(osTime);

    // Time to pop from buffer !!! - And refill with buffer offset
    if (stateChanged || _initializePBO) {
        // Refill PBO
        if (_verboseMode) {
            LDEBUG("Time to update image! ");
        }
        if (_usePBO /*&& !_initializePBO*/) {
            uploadImageDataToPBO();
        }
    }
}

void RenderableSpacecraftCameraPlane::update(const UpdateData& data) {
    _realTime = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
    _realTimeDiff = _realTime.count() - _lastUpdateRealTime.count();
    if (_useBuffering) {
        const double& dt = Time::ref().deltaTime();
        // Delta time changed, need to refill buffer
        if ((abs(_deltaTimeLast - dt)) > EPSILON) {
            fillBuffer(dt);
            _pboIsDirty = false;
        }
        _deltaTimeLast = dt;
    }

    bool timeToUpdateTexture = _realTimeDiff > _minRealTimeUpdateInterval;
    // Update texture
    // The bool blockers might probably not be needed now
    if (timeToUpdateTexture && !_updatingCurrentLevelOfResolution
        && !_updatingCurrentActiveChannel) {
        performImageTimestep(data.time);
        _lastUpdateRealTime = _realTime;
    }

    // Update lookup table, TODO: No need to do this every update
    _lut = _tfMap[_currentActiveInstrument].get();
    _spacecraftCameraPlane->update();
}

void RenderableSpacecraftCameraPlane::render(const RenderData& data) {
    if (!isReady()) {
        return;
    }
    // TODO: We want to create sun imagery node from within the module
    const glm::dvec3& sunPositionWorld
          = OsEng.renderEngine().scene()->sceneGraphNode(_target)->worldPosition();

    _spacecraftCameraPlane->render(data, *_texture, _lut, sunPositionWorld, _planeOpacity,
                                   _contrastValue, _gammaValue, _disableBorder,
                                   _disableFrustum);
}

} // namespace openspace
