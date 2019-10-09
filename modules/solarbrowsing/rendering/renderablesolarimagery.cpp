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

#include <modules/solarbrowsing/solarbrowsingmodule.h>
#include <modules/solarbrowsing/util/j2kcodec.h>
#include <modules/solarbrowsing/rendering/spacecraftcameraplane.h>
#include <modules/solarbrowsing/util/pixelbufferobject.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
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
    static const
    openspace::properties::Property::PropertyInfo MinRealTimeUpdateIntervalInfo = {
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
    , _verboseMode(VerboseModeInfo, false)
{
    std::string rootPath;
    if (!dictionary.getValue("RootPath", rootPath)) {
        throw ghoul::RuntimeError("RootPath has to be specified");
    }

    SolarBrowsingModule* solarbrowsingModule =
        global::moduleEngine.module<SolarBrowsingModule>();

    SpacecraftImageryManager& spacecraftImageryManager =
        solarbrowsingModule->spacecraftImageryManager();
    
    std::string transferfunctionPath;
    if (dictionary.getValue("TransferfunctionPath", transferfunctionPath)) {
        spacecraftImageryManager.loadTransferFunctions(transferfunctionPath, _tfMap);
    }

    spacecraftImageryManager.loadImageMetadata(rootPath, _imageMetadataMap);

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
    addProperty(_moveFactor);
    addProperty(_verboseMode);
}

void RenderableSolarImagery::listen() {
    _enableFrustum.onChange([this]() {
        _enableBorder.setValue(_enableFrustum.value());
    });

    _activeInstruments.onChange([this]() {
        _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
            _activeInstruments
        );
        // Update image size
        const SpacecraftImageryManager::ImageMetadataStateSequence& stateSequence =
            _imageMetadataMap[_currentActiveInstrument];
        const TimedependentState<ImageMetadata>& state = stateSequence.state(
            global::timeManager.time().j2000Seconds()
        );
        std::shared_ptr<ImageMetadata> im = state.contents();
        _imageSize = static_cast<unsigned int>(
            im->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)))
        );

        // Upload asap
        updateTextureGPU(/*asyncUpload=*/false);
    });

    _resolutionLevel.onChange([this]() {
        // Update image size
        const SpacecraftImageryManager::ImageMetadataStateSequence& stateSequence =
            _imageMetadataMap[_currentActiveInstrument];
        const TimedependentState<ImageMetadata>& state = stateSequence.state(
            global::timeManager.time().j2000Seconds()
        );
        std::shared_ptr<ImageMetadata> im = state.contents();
        _imageSize = static_cast<unsigned int>(
            im->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)))
        );

        // Upload asap
        updateTextureGPU(/*asyncUpload=*/false, /*resChanged=*/true);
    });

    _moveFactor.onChange([this]() {
        _spacecraftCameraPlane->createPlaneAndFrustum(_moveFactor);
    });
}

void RenderableSolarImagery::initializeGL() {
    // Get first image size
    const SpacecraftImageryManager::ImageMetadataStateSequence& stateSequenceStart =
        _imageMetadataMap[_currentActiveInstrument];
    const TimedependentState<ImageMetadata>& stateStart = stateSequenceStart.state(
        global::timeManager.time().j2000Seconds()
    );

    std::shared_ptr<ImageMetadata> imStart = stateStart.contents();
    _imageSize =
        imStart->fullResolution / (std::pow(2, static_cast<int>(_resolutionLevel)));

    _texture = std::make_unique<ghoul::opengl::Texture>(
        nullptr,
        glm::uvec3(_imageSize, _imageSize, 1),
        ghoul::opengl::Texture::Format::Red, // Format of the pixeldata
        GL_R8, // INTERNAL format:
        // More preferable to give explicit precision here,
        // otherwise up to the driver to decide
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

    performImageTimestep(global::timeManager.time().j2000Seconds());
    listen();

    _spacecraftCameraPlane = std::make_unique<SpacecraftCameraPlane>(_moveFactor);
}

void RenderableSolarImagery::deinitializeGL() {
    _spacecraftCameraPlane->destroy();
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
    return _spacecraftCameraPlane &&
        _spacecraftCameraPlane->isReady() &&
        _texture != nullptr;
}

void RenderableSolarImagery::updateTextureGPU(bool asyncUpload, bool resChanged) {
    unsigned char* data = new unsigned char[
        _imageSize * _imageSize * sizeof(IMG_PRECISION)
    ];
    defer { delete[] data; };
    double osTime = global::timeManager.time().j2000Seconds();
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

void RenderableSolarImagery::decode(unsigned char* buffer, const std::string& filename) {
    J2kCodec j2c(_verboseMode);
    j2c.decodeIntoBuffer(filename, buffer, _resolutionLevel);
}

void RenderableSolarImagery::performImageTimestep(const double& osTime) {
    const bool stateChanged = _imageMetadataMap[_currentActiveInstrument].hasStateChanged(
        osTime
    );
    // Time to pop from buffer!
    if (stateChanged) {
        updateTextureGPU();
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
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch()
    );
    _realTimeDiff = _realTime.count() - _lastUpdateRealTime.count();
    _timeToUpdateTexture = _realTimeDiff > _minRealTimeUpdateInterval;

    // Update lookup table, TODO: No need to do this every update
    _lut = _tfMap[_currentActiveInstrument].get();
    _spacecraftCameraPlane->update();
}

void RenderableSolarImagery::render(const RenderData& data, RendererTasks&) {
    _isWithinFrustum = checkBoundaries(data);
    _isWithinFrustumLast = _isWithinFrustum;

    // Update texture
    if (_timeToUpdateTexture && _isWithinFrustum) {
        performImageTimestep(global::timeManager.time().j2000Seconds());
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
