/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <modules/solarbrowsing/rendering/spacecraftcameraplane.h>
#include <modules/solarbrowsing/util/solarbrowsinghelper.h>
#include <modules/solarbrowsing/util/structs.h>
#include <modules/solarbrowsing/util/asyncimagedecoder.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/timemanager.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <fstream>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "RenderableSolarImagery";

    constexpr unsigned int DefaultTextureSize = 32;

    constexpr openspace::properties::Property::PropertyInfo ActiveInstrumentsInfo = {
        "ActiveInstrument",
        "Active instrument",
        "The active instrument of the current spacecraft imagery.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableBorderInfo = {
        "EnableBorder",
        "Enable border",
        "Enables border around the current spacecraft imagery.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableFrustumInfo = {
        "EnableFrustum",
        "Enable frustum",
        "Enables frustum around the current spacecraft imagery.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MoveFactorInfo = {
        "MoveFactor",
        "Move factor",
        "How close to the Sun to render the imagery.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DownsamplingLevelInfo = {
        "DownsamplingLevel",
        "Downsampling level",
        "How much to downsample the original data. 0 is original resolution.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ContrastValueInfo = {
        "ContrastValue",
        "Contrast",
        "Contrast of the current spacecraft imagery",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo GammaValueInfo = {
        "GammaValue",
        "Gamma",
        "Gamma of the current spacecraft imagery.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo VerboseModeInfo = {
        "VerboseMode",
        "Verbose mode",
        "Output information about image decoding etc.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PredictFramesAfterInfo = {
        "PredictFramesAfter",
        "Predict frames after",
        "Determines how many images to pre-fetch after the current image frame.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PredictFramesBeforeInfo = {
        "PredictFramesBefore",
        "Predict frames before",
        "Determines how many images to pre-fetch before the current image frame.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // A RenderableSolarImagery renders time-sequenced solar observations from spacecraft
    // instruments as textured planes in 3D space. The renderable automatically displays
    // the image corresponding to the current simulation time.
    //
    // Multiple instruments are supported (e.g., AIA-171, AIA-193, EUVI-A-171), and the
    // active instrument can be selected via the `ActiveInstrument` property.
    //
    // For optimal performance, images are decoded asynchronously in the background and
    // cached to disk. When changing time, the previous image remains visible until the
    // new one is ready. The renderable also predicts and pre-loads nearby frames based
    // on playback direction to ensure smooth playback.
    //
    // The `DownsamplingLevel` property can be used to reduce image resolution for
    // improved performance. A value of 0 uses full resolution, while higher values
    // progressively reduce the resolution (1 = half, 2 = quarter, etc.).
    //
    // Visual adjustments can be made via color mapping (transfer functions), gamma,
    // and contrast controls. Coronagraph instruments can optionally display frustum
    // visualization.
    struct [[codegen::Dictionary(RenderableSolarImagery)]] Parameters {
        // The root directory containing solar imagery organized by instrument. Each
        // subdirectory represents an instrument and contains its observation images
        std::filesystem::path imageDirectory [[codegen::directory()]];

        // Directory containing color tables (transfer functions) for each instrument.
        std::filesystem::path transferfunctionDir [[codegen::directory()]];

        // The instrument to display on startup (e.g., "AIA-171"). If not specified,
        // the first available instrument is used
        std::optional<std::string> startInstrument;

        // [[codegen::verbatim(EnableBorderInfo.description)]]
        std::optional<bool> enableBorder;

        // [[codegen::verbatim(EnableFrustumInfo.description)]]
        std::optional<bool> enableFrustum;

        // [[codegen::verbatim(MoveFactorInfo.description)]]
        std::optional<float> moveFactor;

        // [[codegen::verbatim(DownsamplingLevelInfo.description)]]
        std::optional<int> downsamplingLevel;

        // [[codegen::verbatim(ContrastValueInfo.description)]]
        std::optional<float> contrast;

        // [[codegen::verbatim(GammaValueInfo.description)]]
        std::optional<float> gamma;

        // [[codegen::verbatim(VerboseModeInfo.description)]]
        std::optional<bool> verboseMode;

        // [[codegen::verbatim(PredictFramesAfterInfo.description)]]
        std::optional<int> predictFramesAfter;

        // [[codegen::verbatim(PredictFramesBeforeInfo.description)]]
        std::optional<int> predictFramesBefore;
    };
#include "renderablesolarimagery_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSolarImagery::Documentation() {
    return codegen::doc<Parameters>("renderablesolarimegary");
}

RenderableSolarImagery::RenderableSolarImagery(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _activeInstruments(ActiveInstrumentsInfo)
    , _contrastValue(ContrastValueInfo, 0.f, -15.f, 15.f)
    , _enableBorder(EnableBorderInfo, false)
    , _enableFrustum(EnableFrustumInfo, false)
    , _gammaValue(GammaValueInfo, 0.9f, 0.1f, 10.f)
    , _moveFactor(MoveFactorInfo, 1.0, 0.0, 1.0)
    , _downsamplingLevel(DownsamplingLevelInfo, 2, 0, 5)
    , _verboseMode(VerboseModeInfo, false)
    , _predictFramesAfter(PredictFramesAfterInfo, 10, 0, 20)
    , _predictFramesBefore(PredictFramesBeforeInfo, 2, 0, 20)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    //SolarBrowsingModule* solarbrowsingModule =
    //    global::moduleEngine->module<SolarBrowsingModule>();

    //SpacecraftImageryManager& spacecraftImageryManager =
    //    solarbrowsingModule->spacecraftImageryManager();

    solarbrowsing::loadTransferFunctions(p.transferfunctionDir, _tfMap);
    solarbrowsing::loadImageMetadata(p.imageDirectory, _imageMetadataMap);

    _enableBorder = p.enableBorder.value_or(_enableBorder);
    addProperty(_enableBorder);

    _enableFrustum = p.enableFrustum.value_or(_enableFrustum);
    _enableFrustum.onChange([this]() {
        _enableBorder.setValue(_enableFrustum.value());
    });
    addProperty(_enableFrustum);

    // Add Instrument GUI names
    unsigned int guiNameCount = 0;
    using T = Timeline<ImageMetadata>;
    for (const std::pair<InstrumentName, T>& instrument : _imageMetadataMap) {
        _activeInstruments.addOption(guiNameCount++, instrument.first);
    }

    if (p.startInstrument.has_value()) {
        _currentActiveInstrument = p.startInstrument.value();
    }
    else {
        _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
            _activeInstruments
        );
    }

    _activeInstruments.onChange([this]() {
        _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
            _activeInstruments
        );
        _currentImage = nullptr;
        _predictionIsDirty = true;
    });
    addProperty(_activeInstruments);

    _downsamplingLevel = p.downsamplingLevel.value_or(_downsamplingLevel);
    _downsamplingLevel.onChange([this]() {
        _currentImage = nullptr;
        _predictionIsDirty = true;
    });
    addProperty(_downsamplingLevel);

    _moveFactor = p.moveFactor.value_or(_moveFactor);
    _moveFactor.onChange([this]() {
        _spacecraftCameraPlane->createPlaneAndFrustum(_moveFactor);
    });
    addProperty(_moveFactor);

    _gammaValue = p.gamma.value_or(_gammaValue);
    addProperty(_gammaValue);

    _contrastValue = p.contrast.value_or(_contrastValue);
    addProperty(_contrastValue);

    _verboseMode = p.verboseMode.value_or(_verboseMode);
    addProperty(_verboseMode);

    _predictFramesAfter = p.predictFramesAfter.value_or(_predictFramesAfter);
    _predictFramesAfter.onChange([this]() { _predictionIsDirty = true;  });
    addProperty(_predictFramesAfter);

    _predictFramesBefore = p.predictFramesBefore.value_or(_predictFramesBefore);
    _predictFramesBefore.onChange([this]() { _predictionIsDirty = true;  });
    addProperty(_predictFramesBefore);

    _asyncDecoder = std::make_unique<solarbrowsing::AsyncImageDecoder>(
        std::thread::hardware_concurrency() / 2
    );
}

void RenderableSolarImagery::initializeGL() {
    _spacecraftCameraPlane = std::make_unique<SpacecraftCameraPlane>(_moveFactor);
    _imageryTexture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(DefaultTextureSize, DefaultTextureSize, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::Red, // Format of the pixeldata
        GL_R8, // INTERNAL format:
        // More preferable to give explicit precision here,
        // otherwise up to the driver to decide
        GL_UNSIGNED_BYTE, // Type of data
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge,
        ghoul::opengl::Texture::AllocateData::Yes,
        ghoul::opengl::Texture::TakeOwnership::No
    );

    updateImageryTexture();
}

void RenderableSolarImagery::deinitializeGL() {
    _spacecraftCameraPlane->destroy();
}

bool RenderableSolarImagery::isReady() const {
    return _spacecraftCameraPlane &&
        _spacecraftCameraPlane->isReady();
}

void RenderableSolarImagery::render(const RenderData& data, RendererTasks&) {
    updateImageryTexture();
    const glm::dvec3& sunPositionWorld = sceneGraphNode("Sun")->worldPosition();
    _spacecraftCameraPlane->render(
        data,
        *_imageryTexture,
        _tfMap[_currentActiveInstrument].get(),
        sunPositionWorld,
        opacity(),
        _contrastValue,
        _gammaValue,
        _enableBorder,
        _enableFrustum,
        _currentCenterPixel,
        _currentScale,
        _isCoronaGraph
    );
}

void RenderableSolarImagery::update(const UpdateData& data) {
    const Keyframe<ImageMetadata>* keyframe =
        _imageMetadataMap[_currentActiveInstrument].lastKeyframeBefore(
            global::timeManager->time().j2000Seconds(),
            true
    );

    requestPredictiveFrames(keyframe, data);
    _spacecraftCameraPlane->update();
}

TransferFunction* RenderableSolarImagery::transferFunction() {
    return _tfMap[_currentActiveInstrument].get();
}

const std::unique_ptr<ghoul::opengl::Texture>&
RenderableSolarImagery::imageryTexture() const {
    return _imageryTexture;
}

const SpacecraftCameraPlane& RenderableSolarImagery::cameraPlane() const {
    return *_spacecraftCameraPlane;
}

float RenderableSolarImagery::contrastValue() const {
    return _contrastValue;
}

float RenderableSolarImagery::gammaValue() const {
    return _gammaValue;
}

float RenderableSolarImagery::scale() const {
    return _currentScale;
}

bool RenderableSolarImagery::isCoronaGraph() const {
    return _isCoronaGraph;
}

glm::vec2 RenderableSolarImagery::getCenterPixel() const {
    return _currentCenterPixel;
}

void RenderableSolarImagery::updateImageryTexture() {
    const Keyframe<ImageMetadata>* keyframe =
        _imageMetadataMap[_currentActiveInstrument].lastKeyframeBefore(
            global::timeManager->time().j2000Seconds(),
            true
    );

    if (!keyframe) {
        // No keyframe avaialble so we clear the texture
        if (_currentImage != nullptr) {
            // No need to re-upload an empty image.
            _isCoronaGraph = false;
            _currentScale = 0;
            _currentCenterPixel = glm::vec2(2.f);
            _currentImage = nullptr;

            // Create some dummy data that will be uploaded to GPU avoid UB
            std::vector<unsigned char> buffer;
            buffer.resize(static_cast<size_t>(DefaultTextureSize) * DefaultTextureSize *
                sizeof(ImagePrecision)
            );
            _imageryTexture->setDimensions(
                glm::uvec3(DefaultTextureSize, DefaultTextureSize, 1)
            );
            _imageryTexture->setPixelData(
                buffer.data(),
                ghoul::opengl::Texture::TakeOwnership::No
            );
            _imageryTexture->uploadTexture();
        }
        return;
    }

    if (_currentImage == &(keyframe->data)) {
        // This keyframe is already uploaded to the GPU.
        return;
    }

    unsigned int imageSize = static_cast<unsigned int>(
        keyframe->data.fullResolution /
        std::pow(2, static_cast<unsigned int>(_downsamplingLevel))
    );

    std::filesystem::path cached = FileSys.cacheManager()->cachedFilename(
        keyframe->data.filePath,
        std::format("{}x{}", imageSize, imageSize),
        "solarbrowsing"
    );

    // If the current keyframe image has not yet been decoded and cached we'll just wait
    // until it is available. The previous image will be shown until the new one is ready.
    if (std::filesystem::exists(cached)) {
        // Load data from cache
        solarbrowsing::DecodedImageData data = loadDecodedDataFromCache(
            cached,
            &keyframe->data,
            imageSize
        );

        _isCoronaGraph = data.metadata->isCoronaGraph;
        _currentScale = data.metadata->scale;
        _currentCenterPixel = data.metadata->centerPixel;
        _currentImage = data.metadata;

        _imageryTexture->setDimensions(glm::uvec3(data.imageSize, data.imageSize, 1));
        _imageryTexture->setPixelData(
            data.buffer.data(),
            ghoul::opengl::Texture::TakeOwnership::No
        );
        _imageryTexture->uploadTexture();
    }
}

void RenderableSolarImagery::requestPredictiveFrames(
                                                  const Keyframe<ImageMetadata>* keyframe,
                                                                   const UpdateData& data)
{
    if (!keyframe) {
        return;
    }

    // Only update prediction if we've moved to a different keyframe
    if (!_predictionIsDirty && _lastPredictedKeyframe == keyframe)
    {
        // We've already predicted this keyframe
        return;
    }

    // Detech playback direction
    const double now = data.time.j2000Seconds();
    const double prevTime = data.previousFrameTime.j2000Seconds();
    const double dt = now - prevTime;

    const bool isPlayingForward = dt >= 0;
    const bool isPaused = now == prevTime;

    // Find keyframes within prediction window
    int framesBefore = 0;
    int framesAfter = 0;

    if (isPaused) {
        framesBefore = _predictFramesAfter / 2;
        framesAfter = _predictFramesAfter / 2;
    }
    else if (isPlayingForward) {
        framesBefore = _predictFramesBefore;
        framesAfter = _predictFramesAfter;
    }
    else {
        // Swap for backward
        framesBefore = _predictFramesAfter;
        framesAfter = _predictFramesBefore;
    }

    // Get the corresponding keyframes within the prediction window
    const Timeline<ImageMetadata>& timeline = _imageMetadataMap[_currentActiveInstrument];
    const std::deque<Keyframe<ImageMetadata>>& keyframes = timeline.keyframes();

    // Find the current keyframe iterator
    auto currentIt = std::find_if(
        keyframes.begin(),
        keyframes.end(),
        [keyframe](const Keyframe<ImageMetadata>& kf) {
            return &kf == keyframe;
        }
    );

    if (currentIt == keyframes.end()) {
        return;
    }

    auto requestFrameIfNeeded = [this](const Keyframe<ImageMetadata>& kf) {
        // Check if the keyframe has already been decoded and exists in cache
        const int imageSize = kf.data.fullResolution /
            static_cast<int>(std::pow(2, _downsamplingLevel.value())
        );

        std::filesystem::path cacheFile = FileSys.cacheManager()->cachedFilename(
            kf.data.filePath,
            std::format("{}x{}", imageSize, imageSize),
            "solarbrowsing"
        );

        // Skip if file is already cached
        if (std::filesystem::exists(cacheFile)) {
            return;
        }

        // Request new images to decode
        solarbrowsing::DecodeRequest request(
            &kf.data,
            _downsamplingLevel,
            [this, cacheFile](solarbrowsing::DecodedImageData&& decodedData) {
                saveDecodedDataToCache(cacheFile, decodedData);
            }
        );
        _asyncDecoder->requestDecode(std::move(request));
    };

    // Request frames after and before the current keyframe
    for (int i = 0; i <= framesAfter; i++) {
        auto afterIt = std::next(currentIt, i);
        if (afterIt == keyframes.end()) {
            break;
        }
        requestFrameIfNeeded(*afterIt);
    }

    std::deque<Keyframe<ImageMetadata>>::const_iterator beforeIt = currentIt;
    for (int i = 0; i < framesBefore && beforeIt != keyframes.begin(); i++) {
        beforeIt--;
        requestFrameIfNeeded(*beforeIt);
    }

    _lastPredictedKeyframe = keyframe;
    _predictionIsDirty = false;
}

solarbrowsing::DecodedImageData RenderableSolarImagery::loadDecodedDataFromCache(
                                                        const std::filesystem::path& path,
                                                        const ImageMetadata* metadata,
                                                        unsigned int imageSize)
{
    std::ifstream file = std::ifstream(path, std::ifstream::binary);
    if (!file.good()) {
        FileSys.cacheManager()->removeCacheFile(
            metadata->filePath,
            std::format("{}x{}", imageSize, imageSize)
        );
        throw ghoul::RuntimeError(std::format("Error, could not open cache file '{}'",
            path
        ));
    }

    size_t nEntries = 0;
    file.read(reinterpret_cast<char*>(&nEntries), sizeof(nEntries));
    solarbrowsing::DecodedImageData data;
    data.imageSize = imageSize;
    data.metadata = metadata;
    data.buffer.resize(nEntries);
    file.read(reinterpret_cast<char*>(data.buffer.data()), nEntries * sizeof(uint8_t));

    if (!file) {
        file.close();
        FileSys.cacheManager()->removeCacheFile(
            metadata->filePath,
            std::format("{}x{}", imageSize, imageSize)
        );
        throw ghoul::RuntimeError(std::format("Failed to read image data from cache '{}'",
            path
        ));
    }

    return data;
}

void RenderableSolarImagery::saveDecodedDataToCache(const std::filesystem::path& path,
                                              const solarbrowsing::DecodedImageData& data)
{
    LINFO(std::format("Saving cache '{}'", path));
    std::ofstream file = std::ofstream(path, std::ofstream::binary);
    size_t nEntries = data.buffer.size();
    file.write(reinterpret_cast<const char*>(&nEntries), sizeof(nEntries));
    file.write(
        reinterpret_cast<const char*>(data.buffer.data()),
        nEntries * sizeof(uint8_t)
    );
    file.close();
}

} // namespace openspace
