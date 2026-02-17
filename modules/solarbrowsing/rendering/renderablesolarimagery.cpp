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
#include <modules/solarbrowsing/util/solarbrowsinghelper.h>
#include <modules/solarbrowsing/util/structs.h>
#include <modules/solarbrowsing/util/asyncimagedecoder.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "RenderableSolarImagery";
    constexpr double SUN_RADIUS = (1391600000.0 * 0.5);


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

    solarbrowsing::loadTransferFunctions(p.transferfunctionDir, _tfMap);
    _imageMetadataMap = solarbrowsing::loadImageMetadata(p.imageDirectory);

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
        createPlaneAndFrustum(_moveFactor);
    });
    addProperty(_moveFactor);

    _gammaValue = p.gamma.value_or(_gammaValue);
    addProperty(_gammaValue);

    _contrastValue = p.contrast.value_or(_contrastValue);
    addProperty(_contrastValue);

    _predictFramesAfter = p.predictFramesAfter.value_or(_predictFramesAfter);
    _predictFramesAfter.onChange([this]() { _predictionIsDirty = true;  });
    addProperty(_predictFramesAfter);

    _predictFramesBefore = p.predictFramesBefore.value_or(_predictFramesBefore);
    _predictFramesBefore.onChange([this]() { _predictionIsDirty = true;  });
    addProperty(_predictFramesBefore);

    _verboseMode = p.verboseMode.value_or(_verboseMode);
    _verboseMode.onChange([this]() {
        if (_asyncDecoder) {
            _asyncDecoder->setVerboseFlag(_verboseMode);
        }
    });
    addProperty(_verboseMode);

    _asyncDecoder = std::make_unique<solarbrowsing::AsyncImageDecoder>(
        std::thread::hardware_concurrency() / 2,
        _verboseMode
    );
}

void RenderableSolarImagery::initializeGL() {
    // Initialize plane buffer
    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);
    // Initialize frustum buffer
    glGenVertexArrays(1, &_frustum);
    glGenBuffers(1, &_frustumPositionBuffer);
    if (!_planeShader) {
        _planeShader = global::renderEngine->buildRenderProgram("SpacecraftImagePlaneProgram",
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_vs.glsl"),
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_fs.glsl")
        );
        if (!_planeShader) {
            return;
        }
    }

    if (!_frustumShader) {
        _frustumShader = global::renderEngine->buildRenderProgram("SpacecraftFrustumProgram",
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimagefrustum_vs.glsl"),
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimagefrustum_fs.glsl")
        );
        if (!_frustumShader) {
            return;
        }
    }

    ghoul::opengl::updateUniformLocations(*_planeShader, _uniformCachePlane);
    ghoul::opengl::updateUniformLocations(*_frustumShader, _uniformCacheFrustum);
    createPlaneAndFrustum(_moveFactor);

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
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;
    glDeleteVertexArrays(1, &_frustum);
    _frustum = 0;

    if (_planeShader) {
        global::renderEngine->removeRenderProgram(_planeShader.get());
        _planeShader = nullptr;
    }

    if (_frustumShader) {
        global::renderEngine->removeRenderProgram(_frustumShader.get());
        _frustumShader = nullptr;
    }
}

bool RenderableSolarImagery::isReady() const {
    return _planeShader && _frustumShader;
}

void RenderableSolarImagery::render(const RenderData& data, RendererTasks&) {
    updateImageryTexture();
    const glm::dvec3& sunPositionWorld = sceneGraphNode("Sun")->worldPosition();

    glEnable(GL_CULL_FACE);

    // Perform necessary transforms
    const glm::dmat4& viewMatrix = data.camera.combinedViewMatrix();
    const glm::mat4& projectionMatrix = data.camera.projectionMatrix();

    // TODO: We want to create sun imagery node from within the module,
    // @TODO (anden88 2026-02-17): I'm not sure what the original TODO is referencing.
    const glm::dvec3& spacecraftPosWorld = data.modelTransform.translation;
    const glm::dmat3 spacecraftRotWorld = data.modelTransform.rotation;

    const glm::dvec3 sunDir = sunPositionWorld - spacecraftPosWorld;
    const glm::dvec3 offset = sunDir * _gaussianMoveFactor;

    _position = spacecraftPosWorld + offset;
    // Normal should point from plane toward spacecraft (i.e. plane faces spacecraft).
    _normal = glm::normalize(spacecraftPosWorld - sunPositionWorld);

    // (anden88 2025-12-10): An attempt was made to use the glm::lookAt to "simplify"
    // the rotation matrix without having to build the basis vectors ourselves. However,
    // the plane rotation would be rotating in all different kinds of orientations.
    // _rotation = glm::lookAt(spacecraftPosWorld, glm::dvec3(sunPositionWorld), glm::normalize(up));
    // _rotation[3] = glm::dvec4(0.0, 0.0, 0.0, 1.0);

    // Pick a world up. Prefer the spacecraft local +Z transformed to world, but fall back
    // to a global up if nearly collinear with the normal.
    glm::vec3 worldUp = spacecraftRotWorld * glm::dvec3(0.0, 0.0, 1.0);
    if (std::abs(glm::dot(worldUp, _normal)) > 0.9999) {
        // Nearly parallel: pick another stable up (e.g. world Y)
        worldUp = glm::dvec3(0.0, 1.0, 0.0);
    }

    // Build tangent basis for the plane: right, upOnPlane, normal.
    glm::vec3 right = glm::normalize(glm::cross(worldUp, _normal));
    // Already normalized if right and N are normalized.
    glm::vec3 upOnPlane = glm::cross(_normal, right);

    // Build a rotation matrix that transforms local axes -> world axes.
    // Local axes: +X = right, +Y = upOnPlane, +Z = _normal
    glm::dmat4 rot = glm::dmat4(1.0);
    rot[0] = glm::dvec4(right, 0.0);      // first column = world X for local +X
    rot[1] = glm::dvec4(upOnPlane, 0.0);  // second column = world Y for local +Y
    rot[2] = glm::dvec4(_normal, 0.0);    // third column = world Z for local +Z

    _rotation = std::move(rot);

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), _position) *
        _rotation *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))
    );
    const glm::dmat4 modelViewTransform = viewMatrix * modelTransform;

    // For frustum
    const glm::dmat4 spacecraftModelTransform =
        glm::translate(glm::dmat4(1.0), spacecraftPosWorld) *
        _rotation *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))
    );

    _planeShader->activate();
    ghoul::opengl::TextureUnit imageUnit;
    imageUnit.activate();
    _imageryTexture->bind();

    _planeShader->setUniform(_uniformCachePlane.isCoronaGraph, _isCoronaGraph);
    _planeShader->setUniform(_uniformCachePlane.scale, _currentScale);
    _planeShader->setUniform(_uniformCachePlane.centerPixel, _currentCenterPixel);
    _planeShader->setUniform(_uniformCachePlane.imageryTexture, imageUnit);
    _planeShader->setUniform(_uniformCachePlane.planeOpacity, opacity());
    _planeShader->setUniform(_uniformCachePlane.gammaValue, _gammaValue);
    _planeShader->setUniform(_uniformCachePlane.contrastValue, _contrastValue);
    _planeShader->setUniform(
        _uniformCachePlane.modelViewProjectionTransform,
        projectionMatrix * glm::mat4(modelViewTransform)
    );

    ghoul::opengl::TextureUnit tfUnit;
    tfUnit.activate();
    TransferFunction* lut = _tfMap[_currentActiveInstrument].get();
    if (lut) {
        lut->bind();
        _planeShader->setUniform(_uniformCachePlane.hasLut, true);
    }
    else {
        _planeShader->setUniform(_uniformCachePlane.hasLut, false);
    }
    // Must bind all sampler2D, otherwise undefined behaviour
    _planeShader->setUniform(_uniformCachePlane.lut, tfUnit);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    _planeShader->deactivate();
    _frustumShader->activate();

    _frustumShader->setUniform(_uniformCacheFrustum.scale, _currentScale);
    _frustumShader->setUniform(_uniformCacheFrustum.centerPixel, _currentCenterPixel);
    _frustumShader->setUniform(_uniformCacheFrustum.planeOpacity, opacity());
    _frustumShader->setUniform(
        _uniformCacheFrustum.modelViewProjectionTransform,
        projectionMatrix * glm::mat4(viewMatrix * spacecraftModelTransform)
    );
    _frustumShader->setUniform(
        _uniformCacheFrustum.modelViewProjectionTransformPlane,
        projectionMatrix * glm::mat4(modelViewTransform)
    );

    glBindVertexArray(_frustum);

    if (_enableBorder && _enableFrustum) {
        glDrawArrays(GL_LINES, 0, 16);
    }
    else if (!_enableBorder && _enableFrustum) {
        glDrawArrays(GL_LINES, 0, 8);
    }
    else if (!_enableFrustum && _enableBorder) {
        glDrawArrays(GL_LINES, 8, 16);
    }
    _frustumShader->deactivate();

    glDisable(GL_CULL_FACE);
}

void RenderableSolarImagery::update(const UpdateData& data) {
    const Keyframe<ImageMetadata>* keyframe =
        _imageMetadataMap[_currentActiveInstrument].lastKeyframeBefore(
            global::timeManager->time().j2000Seconds(),
            true
    );

    requestPredictiveFrames(keyframe, data);
    if (_planeShader->isDirty()) {
        _planeShader->rebuildFromFile();
    }

    if (_frustumShader->isDirty()) {
        _frustumShader->rebuildFromFile();
    }
}

TransferFunction* RenderableSolarImagery::transferFunction() {
    return _tfMap[_currentActiveInstrument].get();
}

const std::unique_ptr<ghoul::opengl::Texture>&
RenderableSolarImagery::imageryTexture() const {
    return _imageryTexture;
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
        solarbrowsing::DecodedImageData data = solarbrowsing::loadDecodedDataFromCache(
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
                saveDecodedDataToCache(cacheFile, decodedData, _verboseMode);
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

void RenderableSolarImagery::createPlaneAndFrustum(double moveDistance) {
    // Computing the image plane position using linear scale is not sufficient for fine
    // tuning movement near the Sun. A Gaussian function* (3.1) is used to address this
    // issue: *https://www.diva-portal.org/smash/get/diva2:1147161/FULLTEXT01.pdf
    //_gaussianMoveFactor = a * exp(-(pow((moveDistance - 1) - b, 2.0)) / (2.0 * pow(c, 2.0)));
    _gaussianMoveFactor = exp(-(pow((moveDistance - 1), 2.0)) / (2.0));
    _size = static_cast<float>(_gaussianMoveFactor * SUN_RADIUS);
    createPlane();
    createFrustum();
}

void RenderableSolarImagery::createPlane() const {
    const GLfloat size = _size;
    const GLfloat vertex_data[] = {
        // x      y     z     w     s     t
        -size, -size, 0.f, 0.f, 0.f, 0.f,
         size,  size, 0.f, 0.f, 1.f, 1.f,
        -size,  size, 0.f, 0.f, 0.f, 1.f,
        -size, -size, 0.f, 0.f, 0.f, 0.f,
         size, -size, 0.f, 0.f, 1.f, 0.f,
         size,  size, 0.f, 0.f, 1.f, 1.f,
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(0)
    );
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );
}

void RenderableSolarImagery::createFrustum() const {
    // Vertex orders x, y, z, w
    // Where w indicates if vertex should be drawn in spacecraft
    // or planes coordinate system
    const GLfloat vertex_data[] = {
        0.f,    0.f,    0.f, 0.0,
        _size,  _size,  0.f, 1.0,
        0.f,    0.f,    0.f, 0.0,
        -_size, -_size, 0.f, 1.0,
        0.f,    0.f,    0.f, 0.0,
        _size,  -_size, 0.f, 1.0,
        0.f,    0.f,    0.f, 0.0,
        -_size, _size,  0.f, 1.0,
        // Borders
        // Left
        -_size, -_size, 0.f, 1.0,
        -_size, _size,  0.f, 1.0,
        // Top
        -_size, _size,  0.f, 1.0,
        _size,  _size,  0.f, 1.0,
        // Right
        _size,  _size,  0.f, 1.0,
        _size,  -_size, 0.f, 1.0,
        // Bottom
        _size,  -_size, 0.f, 1.0,
        -_size, -_size, 0.f, 1.0,
    };
    glBindVertexArray(_frustum);
    glBindBuffer(GL_ARRAY_BUFFER, _frustumPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, reinterpret_cast<void*>(0));
}

const glm::vec3& RenderableSolarImagery::planeNormal() const {
    return _normal;
}
const glm::dvec3& RenderableSolarImagery::planeWorldPosition() const {
    return _position;
}
const glm::dmat4& RenderableSolarImagery::planeWorldRotation() const {
    return _rotation;
}

} // namespace openspace
