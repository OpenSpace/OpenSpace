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

#include <modules/base/basemodule.h>
#include <modules/solarbrowsing/solarbrowsingmodule.h>
#include <modules/solarbrowsing/util/asyncimagedecoder.h>
#include <modules/solarbrowsing/util/dynamichelioviewerimagedownloader.h>
#include <modules/solarbrowsing/util/solarbrowsinghelper.h>
#include <modules/solarbrowsing/util/structs.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <scn/scan.h>
#include <fstream>
#include <optional>
#include <string_view>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "RenderableSolarImagery";
    constexpr size_t DefaultTextureSize = 32;

    enum FaceMode {
        FrontOnly = 0,
        SolidBack,
        DoubleSided
    };

    constexpr Property::PropertyInfo ActiveInstrumentInfo = {
        "ActiveInstrument",
        "Active instrument",
        "The active instrument of the current spacecraft imagery.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo EnableBorderInfo = {
        "EnableBorder",
        "Enable border",
        "Enables border around the current spacecraft imagery.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo EnableFrustumInfo = {
        "EnableFrustum",
        "Enable frustum",
        "Enables frustum around the current spacecraft imagery.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo FaceModeInfo = {
        "FaceMode",
        "Face Mode",
        "Specifies how the plane is rendered: front side only, with a solid backside, "
        "or textured on both sides.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo MoveFactorInfo = {
        "MoveFactor",
        "Move factor",
        "Controls how close to the Sun to render the imagery.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo DownsamplingLevelInfo = {
        "DownsamplingLevel",
        "Downsampling level",
        "Controls how much to downsample the original data (0 is original resolution).",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo BlackTransparencyThresholdInfo = {
        "BlackTransparencyThreshold",
        "Black transparency threshold",
        "Pixels with intensity below this threshold are discarded.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo ContrastValueInfo = {
        "ContrastValue",
        "Contrast",
        "Contrast of the current spacecraft imagery.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo GammaValueInfo = {
        "GammaValue",
        "Gamma",
        "Gamma of the current spacecraft imagery.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo VerboseModeInfo = {
        "VerboseMode",
        "Verbose mode",
        "Output information about image decoding.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo PredictFramesAfterInfo = {
        "PredictFramesAfter",
        "Predict frames after",
        "Determines how many images to pre-fetch after the current image frame.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo PredictFramesBeforeInfo = {
        "PredictFramesBefore",
        "Predict frames before",
        "Determines how many images to pre-fetch before the current image frame.",
        Property::Visibility::AdvancedUser
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
    // and contrast controls. Coronagraph instruments can optionally display a frustum
    // visualization.
    struct [[codegen::Dictionary(RenderableSolarImagery)]] Parameters {
        enum class LoadingType {
            StaticLoading,
            DynamicDownloading
        };

        enum class [[codegen::map(FaceMode)]] FaceMode {
            FrontOnly [[codgen::key("Front Only")]],
            SolidBack [[codegen::key("Solid Back")]],
            DoubleSided [[codegen::key("Double Sided")]]
        };
        // The root directory containing solar imagery organized by instrument. Each
        // subdirectory represents an instrument and contains its observation images.
        std::filesystem::path imageDirectory [[codegen::directory()]];

        // The instrument to display on startup (e.g., "AIA-171"). If not specified,
        // the first available instrument is used.
        std::optional<std::string> startInstrument;

        // [[codegen::verbatim(EnableBorderInfo.description)]]
        std::optional<bool> enableBorder;

        // [[codegen::verbatim(EnableFrustumInfo.description)]]
        std::optional<bool> enableFrustum;

        enum class [[codegen::map(FaceMode)]] FaceMode {
            FrontOnly [[codgen::key("Front Only")]],
            SolidBack [[codegen::key("Solid Back")]],
            DoubleSided [[codegen::key("Double Sided")]]
        };

        // [[codegen::verbatim(FaceModeInfo.description)]]
        std::optional<FaceMode> faceMode;

        // [[codegen::verbatim(MoveFactorInfo.description)]]
        std::optional<float> moveFactor;

        // [[codegen::verbatim(DownsamplingLevelInfo.description)]]
        std::optional<int> downsamplingLevel;

        // [[codegen::verbatim(BlackTransparencyThresholdInfo.description)]]
        std::optional<float> blackTransparencyThreshold;

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

        // Optional runtime streaming settings for HelioViewer-backed imagery downloads.
        // Supported keys are:
        // `Enable`, `SourceId`, `SpacecraftName`, `Instrument`, `CadenceSeconds`,
        // `DownloadDirectory`, `PrefetchFramesBefore`, `PrefetchFramesAfter`,
        // `MaxConcurrentDownloads`, `SaveDownloadsOnShutdown`,
        // `RetryBackoffSeconds`, and `MaxRetries`.
        //
        // The downloader currently manages one active instrument stream per
        // RenderableSolarImagery instance. If `Instrument` is omitted, the downloader
        // follows the currently selected instrument and rebuilds itself when the active
        // instrument changes.
        std::optional<ghoul::Dictionary> dynamicDownload;
    };

    std::optional<double> timestampFromHelioviewerFilename(
        const std::filesystem::path& path)
    {
        const std::string stem = path.stem().string();
        const size_t suffixSeparator = stem.find("__", 24);
        const std::string timestampStem =
            (suffixSeparator == std::string::npos) ? stem : stem.substr(0, suffixSeparator);

        auto r = scn::scan<int, int, int, int, int, int, int>(
            timestampStem,
            "{}_{}_{}__{}_{}_{}_{}"
        );
        if (!r) {
            return std::nullopt;
        }

        auto& [year, month, day, hour, minute, second, millisecond] = r->values();
        const std::string timestamp = std::format(
            "{:04d}-{:02d}-{:02d}T{:02d}:{:02d}:{:02d}.{:03d}",
            year,
            month,
            day,
            hour,
            minute,
            second,
            millisecond
        );
        return Time::convertTime(timestamp);
    }

    std::optional<std::string> instrumentFromHelioviewerFilename(
        const std::filesystem::path& path)
    {
        const std::string stem = path.stem().string();
        const size_t separator = stem.rfind("__");
        if (separator == std::string::npos) {
            return std::nullopt;
        }

        const std::string spacecraftAndInstrument = stem.substr(separator + 2);
        const size_t firstUnderscore = spacecraftAndInstrument.find('_');
        if (firstUnderscore == std::string::npos ||
            firstUnderscore + 1 >= spacecraftAndInstrument.size())
        {
            return std::nullopt;
        }

        return spacecraftAndInstrument.substr(firstUnderscore + 1);
    }
} // namespace
#include "renderablesolarimagery_codegen.cpp"

namespace openspace {

    openspace::Documentation RenderableSolarImagery::Documentation() {
        return codegen::doc<Parameters>("solarbrowsing_renderable_solarimegary");
    }

    RenderableSolarImagery::RenderableSolarImagery(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _activeInstruments(ActiveInstrumentInfo)
        , _blackTransparencyThreshold(BlackTransparencyThresholdInfo, 0.01f, 0.0f, 0.1f)
        , _contrastValue(ContrastValueInfo, 0.f, -15.f, 15.f)
        , _enableBorder(EnableBorderInfo, false)
        , _enableFrustum(EnableFrustumInfo, false)
        , _faceMode(FaceModeInfo)
        , _gammaValue(GammaValueInfo, 0.9f, 0.1f, 10.f)
        , _moveFactor(MoveFactorInfo, 1.0, 0.0, 1.0)
        , _downsamplingLevel(DownsamplingLevelInfo, 2, 0, 5)
        , _verboseMode(VerboseModeInfo, false)
        , _predictFramesAfter(PredictFramesAfterInfo, 10, 0, 20)
        , _predictFramesBefore(PredictFramesBeforeInfo, 2, 0, 20)
    {
        const Parameters p = codegen::bake<Parameters>(dictionary);

        addProperty(Fadeable::_opacity);

        _imageDirectory = p.imageDirectory;
        if (p.dynamicDownload.has_value()) {
            const ghoul::Dictionary& dynamicDownload = *p.dynamicDownload;
            if (dynamicDownload.hasValue<bool>("Enable")) {
                _enableDynamicDownload = dynamicDownload.value<bool>("Enable");
            }
            if (dynamicDownload.hasValue<double>("SourceId")) {
                _dynamicSourceId = static_cast<int>(dynamicDownload.value<double>("SourceId"));
            }
            else if (dynamicDownload.hasValue<int>("SourceId")) {
                _dynamicSourceId = dynamicDownload.value<int>("SourceId");
            }
            if (dynamicDownload.hasValue<std::string>("SpacecraftName")) {
                _dynamicSpacecraftName = dynamicDownload.value<std::string>(
                    "SpacecraftName"
                );
            }
            if (dynamicDownload.hasValue<std::string>("Instrument")) {
                _dynamicInstrument = dynamicDownload.value<std::string>("Instrument");
            }
            if (dynamicDownload.hasValue<double>("CadenceSeconds")) {
                _dynamicCadenceSeconds = dynamicDownload.value<double>("CadenceSeconds");
            }
            if (dynamicDownload.hasValue<std::string>("DownloadDirectory")) {
                _downloadDirectory = std::filesystem::path(
                    dynamicDownload.value<std::string>("DownloadDirectory")
                );
            }
            if (dynamicDownload.hasValue<double>("PrefetchFramesBefore")) {
                _dynamicPrefetchBefore = static_cast<int>(
                    dynamicDownload.value<double>("PrefetchFramesBefore")
                );
            }
            else if (dynamicDownload.hasValue<int>("PrefetchFramesBefore")) {
                _dynamicPrefetchBefore = dynamicDownload.value<int>("PrefetchFramesBefore");
            }
            if (dynamicDownload.hasValue<double>("PrefetchFramesAfter")) {
                _dynamicPrefetchAfter = static_cast<int>(
                    dynamicDownload.value<double>("PrefetchFramesAfter")
                );
            }
            else if (dynamicDownload.hasValue<int>("PrefetchFramesAfter")) {
                _dynamicPrefetchAfter = dynamicDownload.value<int>("PrefetchFramesAfter");
            }
            if (dynamicDownload.hasValue<double>("MaxConcurrentDownloads")) {
                _dynamicMaxConcurrentDownloads = static_cast<int>(
                    dynamicDownload.value<double>("MaxConcurrentDownloads")
                );
            }
            else if (dynamicDownload.hasValue<int>("MaxConcurrentDownloads")) {
                _dynamicMaxConcurrentDownloads = dynamicDownload.value<int>(
                    "MaxConcurrentDownloads"
                );
            }
            if (dynamicDownload.hasValue<bool>("SaveDownloadsOnShutdown")) {
                _saveDownloadsOnShutdown = dynamicDownload.value<bool>(
                    "SaveDownloadsOnShutdown"
                );
            }
            if (dynamicDownload.hasValue<double>("RetryBackoffSeconds")) {
                _dynamicRetryBackoffSeconds = dynamicDownload.value<double>(
                    "RetryBackoffSeconds"
                );
            }
            if (dynamicDownload.hasValue<double>("MaxRetries")) {
                _dynamicMaxRetries = static_cast<int>(
                    dynamicDownload.value<double>("MaxRetries")
                );
            }
            else if (dynamicDownload.hasValue<int>("MaxRetries")) {
                _dynamicMaxRetries = dynamicDownload.value<int>("MaxRetries");
            }
        }

        _imageMetadataMap = loadImageMetadata(p.imageDirectory);
        _tfMap = loadTransferFunctions(p.imageDirectory, _imageMetadataMap);
        const bool hasData = std::any_of(
            _imageMetadataMap.begin(),
            _imageMetadataMap.end(),
            [](const std::pair<const InstrumentName, Timeline<ImageMetadata>>& p) {
                return p.second.nKeyframes() > 0;
            }
        );

        if (!hasData) {
            LWARNING(std::format(
                "Could not find any image data in '{}'. Image data can be downloaded using "
                "the HelioviewerDownloadTask. See the solar browsing documentation for more "
                "information", p.imageDirectory
            ));
        }

        _enableBorder = p.enableBorder.value_or(_enableBorder);
        addProperty(_enableBorder);

        _enableFrustum = p.enableFrustum.value_or(_enableFrustum);
        _enableFrustum.onChange([this]() {
            _enableBorder = _enableFrustum.value();
            });
        addProperty(_enableFrustum);

        _faceMode.addOption(FaceMode::FrontOnly, "Front Only");
        _faceMode.addOption(FaceMode::SolidBack, "Solid Back");
        _faceMode.addOption(FaceMode::DoubleSided, "Double Sided");
        _faceMode = FaceMode::SolidBack;

        if (p.faceMode.has_value()) {
            _faceMode = codegen::map<FaceMode>(*p.faceMode);
        }
        addProperty(_faceMode);

        // Add Instrument GUI names
        unsigned int guiNameCount = 0;
        using T = Timeline<ImageMetadata>;
        for (const std::pair<const InstrumentName, T>& instrument : _imageMetadataMap) {
            _activeInstruments.addOption(guiNameCount++, instrument.first);
        }

        if (p.startInstrument.has_value()) {
            _currentActiveInstrument = p.startInstrument.value();
            // Update the option property to show the correct label
            const std::vector<OptionProperty::Option>& options =
                _activeInstruments.options();

            auto it = std::find_if(
                options.begin(),
                options.end(),
                [this](const OptionProperty::Option& option) {
                    return option.description == _currentActiveInstrument;
                }
            );

            if (it != options.end()) {
                _activeInstruments = it->value;
            }
        }
        else {
            if (_activeInstruments.hasOption()) {
                _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
                    _activeInstruments
                );
            }
        }

        _activeInstruments.onChange([this]() {
            _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
                _activeInstruments
            );
            _currentKeyframe = NoActiveKeyframe;
            _predictionIsDirty = true;
            if (_enableDynamicDownload && !_dynamicInstrument.has_value()) {
                _dynamicDownloaderInstrument.clear();
                _dynamicDownloader = nullptr;
            }
            });
        addProperty(_activeInstruments);

        _downsamplingLevel = p.downsamplingLevel.value_or(_downsamplingLevel);
        _downsamplingLevel.onChange([this]() {
            _currentKeyframe = NoActiveKeyframe;
            _predictionIsDirty = true;
            });
        addProperty(_downsamplingLevel);

        _moveFactor = p.moveFactor.value_or(_moveFactor);
        _moveFactor.onChange([this]() { createPlaneAndFrustum(_moveFactor); });
        addProperty(_moveFactor);

        _blackTransparencyThreshold = p.blackTransparencyThreshold.value_or(
            _blackTransparencyThreshold
        );
        addProperty(_blackTransparencyThreshold);

        _gammaValue = p.gamma.value_or(_gammaValue);
        addProperty(_gammaValue);

        _contrastValue = p.contrast.value_or(_contrastValue);
        addProperty(_contrastValue);

        _predictFramesAfter = p.predictFramesAfter.value_or(_predictFramesAfter);
        _predictFramesAfter.onChange([this]() { _predictionIsDirty = true; });
        addProperty(_predictFramesAfter);

        _predictFramesBefore = p.predictFramesBefore.value_or(_predictFramesBefore);
        _predictFramesBefore.onChange([this]() { _predictionIsDirty = true; });
        addProperty(_predictFramesBefore);

        _verboseMode = p.verboseMode.value_or(_verboseMode);
        _verboseMode.onChange([this]() {
            if (_asyncDecoder) {
                _asyncDecoder->setVerboseFlag(_verboseMode);
            }
            });
        addProperty(_verboseMode);

        _asyncDecoder = std::make_unique<AsyncImageDecoder>(
            std::thread::hardware_concurrency() / 2,
            _verboseMode
        );
    }

    void RenderableSolarImagery::initializeGL() {
        _planeShader = BaseModule::ProgramObjectManager.request(
            "SpacecraftImagePlaneProgram",
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return global::renderEngine->buildRenderProgram(
                    "SpacecraftImagePlaneProgram",
                    absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_vs.glsl"),
                    absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_fs.glsl")
                );
            }
        );

        _frustumShader = BaseModule::ProgramObjectManager.request(
            "SpacecraftFrustumProgram",
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return global::renderEngine->buildRenderProgram(
                    "SpacecraftFrustumProgram",
                    absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimagefrustum_vs.glsl"),
                    absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimagefrustum_fs.glsl")
                );
            }
        );

        // Initialize plane buffer
        glCreateVertexArrays(1, &_quadVao);
        glCreateBuffers(1, &_vertexPositionBuffer);
        glVertexArrayVertexBuffer(_quadVao, 0, _vertexPositionBuffer, 0, sizeof(PlaneVertex));

        glEnableVertexArrayAttrib(_quadVao, 0);
        glVertexArrayAttribFormat(_quadVao, 0, 2, GL_FLOAT, GL_FALSE, 0);
        glVertexArrayAttribBinding(_quadVao, 0, 0);
        // ST coordinates
        glEnableVertexArrayAttrib(_quadVao, 1);
        glVertexArrayAttribFormat(
            _quadVao,
            1,
            2,
            GL_FLOAT,
            GL_FALSE,
            offsetof(PlaneVertex, texCoords)
        );
        glVertexArrayAttribBinding(_quadVao, 1, 0);

        // Initialize frustum buffer
        glCreateVertexArrays(1, &_frustumVao);
        glCreateBuffers(1, &_frustumPositionBuffer);
        glVertexArrayVertexBuffer(
            _frustumVao,
            0,
            _frustumPositionBuffer,
            0,
            sizeof(FrustumVertex)
        );

        // Position
        glEnableVertexArrayAttrib(_frustumVao, 0);
        glVertexArrayAttribFormat(_frustumVao, 0, 4, GL_FLOAT, GL_FALSE, 0);
        glVertexArrayAttribBinding(_frustumVao, 0, 0);

        ghoul::opengl::updateUniformLocations(*_planeShader, _uniformCachePlane);
        ghoul::opengl::updateUniformLocations(*_frustumShader, _uniformCacheFrustum);
        createPlaneAndFrustum(_moveFactor);

        _imageryTexture = std::make_unique<ghoul::opengl::Texture>(
            ghoul::opengl::Texture::FormatInit{
                .dimensions = glm::uvec3(DefaultTextureSize, DefaultTextureSize, 1),
                .type = GL_TEXTURE_2D,
                .format = ghoul::opengl::Texture::Format::Red,
                .dataType = GL_UNSIGNED_BYTE,
            },
            ghoul::opengl::Texture::SamplerInit{
                .wrapping = ghoul::opengl::Texture::WrappingMode::ClampToEdge,
            }
            );

        updateImageryTexture();
    }

    void RenderableSolarImagery::deinitializeGL() {
        glDeleteVertexArrays(1, &_quadVao);
        glDeleteVertexArrays(1, &_frustumVao);
        _imageryTexture = nullptr;

        BaseModule::ProgramObjectManager.release(
            "SpacecraftImagePlaneProgram",
            [](ghoul::opengl::ProgramObject* p) {
                global::renderEngine->removeRenderProgram(p);
            }
        );
        _planeShader = nullptr;

        BaseModule::ProgramObjectManager.release(
            "SpacecraftFrustumProgram",
            [](ghoul::opengl::ProgramObject* p) {
                global::renderEngine->removeRenderProgram(p);
            }
        );
        _frustumShader = nullptr;
    }

    void RenderableSolarImagery::deinitialize() {
        if (_dynamicDownloader) {
            _dynamicDownloader->deinitialize(_saveDownloadsOnShutdown);
            _dynamicDownloader = nullptr;
        }

        Renderable::deinitialize();
    }

    bool RenderableSolarImagery::isReady() const {
        return _planeShader && _frustumShader;
    }

    void RenderableSolarImagery::render(const RenderData& data, RendererTasks&) {
        updateImageryTexture();
        const glm::dvec3& sunPositionWorld = sceneGraphNode("Sun")->worldPosition();

        switch (_faceMode) {
        case FaceMode::FrontOnly:
            glEnable(GL_CULL_FACE);
            glCullFace(GL_BACK);
            break;
        case FaceMode::SolidBack:
        case FaceMode::DoubleSided:
            glDisable(GL_CULL_FACE);
            break;
        default:
            throw ghoul::MissingCaseException();
        }

        // Perform necessary transforms
        const glm::dmat4& viewMatrix = data.camera.combinedViewMatrix();
        const glm::mat4& projectionMatrix = data.camera.projectionMatrix();

        const glm::dvec3& spacecraftPosWorld = data.modelTransform.translation;
        const glm::dmat3 spacecraftRotWorld = data.modelTransform.rotation;

        const glm::dvec3 sunDir = sunPositionWorld - spacecraftPosWorld;
        const glm::dvec3 offset = sunDir * _gaussianMoveFactor;

        _position = spacecraftPosWorld + offset;
        // Normal should point from plane toward spacecraft (i.e. plane faces spacecraft)
        _normal = glm::normalize(spacecraftPosWorld - sunPositionWorld);

        // (anden88 2025-12-10): An attempt was made to use the glm::lookAt to "simplify"
        // the rotation matrix without having to build the basis vectors ourselves. However,
        // the plane rotation would be rotating in all different kinds of orientations.
         //_rotation = glm::lookAt(
         //    spacecraftPosWorld,
         //    glm::dvec3(sunPositionWorld),
         //    glm::normalize(up)
         //);
        // _rotation[3] = glm::dvec4(0.0, 0.0, 0.0, 1.0);

        // Pick a world up. Prefer the spacecraft local +Z transformed to world, but fall back
        // to a global up if nearly collinear with the normal
        glm::vec3 worldUp = spacecraftRotWorld * glm::dvec3(0.0, 0.0, 1.0);
        if (std::abs(glm::dot(worldUp, _normal)) > 0.9999) {
            // Nearly parallel: pick another stable up (e.g. world Y)
            worldUp = glm::dvec3(0.0, 1.0, 0.0);
        }

        // Build tangent basis for the plane: right, upOnPlane, normal
        glm::vec3 right = glm::normalize(glm::cross(worldUp, _normal));
        // Already normalized if right and N are normalized
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
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));
        const glm::dmat4 modelViewTransform = viewMatrix * modelTransform;

        // For frustum
        const glm::dmat4 spacecraftModelTransform =
            glm::translate(glm::dmat4(1.0), spacecraftPosWorld) *
            _rotation *
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

        _planeShader->activate();
        ghoul::opengl::TextureUnit imageUnit;
        imageUnit.bind(*_imageryTexture);

        _planeShader->setUniform(_uniformCachePlane.isCoronaGraph, _isCoronaGraph);
        _planeShader->setUniform(_uniformCachePlane.scale, _currentScale);
        _planeShader->setUniform(_uniformCachePlane.centerPixel, _currentCenterPixel);
        _planeShader->setUniform(_uniformCachePlane.imageryTexture, imageUnit);
        _planeShader->setUniform(_uniformCachePlane.planeOpacity, opacity());
        _planeShader->setUniform(
            _uniformCachePlane.blackTransparencyThreshold,
            _blackTransparencyThreshold
        );
        _planeShader->setUniform(_uniformCachePlane.gammaValue, _gammaValue);
        _planeShader->setUniform(_uniformCachePlane.contrastValue, _contrastValue);
        _planeShader->setUniform(
            _uniformCachePlane.modelViewProjectionTransform,
            projectionMatrix * glm::mat4(modelViewTransform)
        );

        ghoul::opengl::TextureUnit tfUnit;
        TransferFunction* lut = transferFunction();
        if (lut) {
            tfUnit.bind(lut->texture());
            _planeShader->setUniform(_uniformCachePlane.hasLut, true);
        }
        else {
            _planeShader->setUniform(_uniformCachePlane.hasLut, false);
        }
        // Must bind all sampler2D, otherwise undefined behaviour
        _planeShader->setUniform(_uniformCachePlane.lut, tfUnit);
        _planeShader->setUniform(_uniformCachePlane.faceMode, _faceMode);

        glBindVertexArray(_quadVao);
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

        glBindVertexArray(_frustumVao);

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
        TransferFunction* tf = transferFunction();
        if (tf) {
            tf->update();
        }

        if (_enableDynamicDownload) {
            ensureDynamicDownloader();
            if (_dynamicDownloader) {
                _dynamicDownloader->update(
                    data.time.j2000Seconds(),
                    data.time.j2000Seconds() - data.previousFrameTime.j2000Seconds()
                );
                ingestDownloadedFiles();
            }
        }

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

    void RenderableSolarImagery::ensureDynamicDownloader() {
        if (!_enableDynamicDownload || _dynamicSourceId < 0 || _dynamicSpacecraftName.empty()) {
            return;
        }

        const std::string instrument = _dynamicInstrument.value_or(_currentActiveInstrument);
        if (instrument.empty()) {
            return;
        }

        if (_dynamicDownloader && instrument == _dynamicDownloaderInstrument) {
            return;
        }

        if (_dynamicDownloader) {
            _dynamicDownloader->deinitialize(_saveDownloadsOnShutdown);
        }

        std::filesystem::path outputDirectory = _downloadDirectory.value_or(
            _imageDirectory / instrument
        );

        _dynamicDownloader = std::make_unique<DynamicHelioviewerImageDownloader>(
            outputDirectory,
            _dynamicSpacecraftName,
            _dynamicSourceId,
            instrument,
            _dynamicCadenceSeconds,
            _dynamicPrefetchBefore,
            _dynamicPrefetchAfter,
            _dynamicMaxConcurrentDownloads,
            _dynamicRetryBackoffSeconds,
            _dynamicMaxRetries
        );
        _dynamicDownloaderInstrument = instrument;
    }

    void RenderableSolarImagery::ingestDownloadedFiles() {
        if (!_dynamicDownloader) {
            return;
        }

        bool activeInstrumentChanged = false;
        bool transferFunctionsMayNeedRefresh = false;

        for (const std::filesystem::path& filePath : _dynamicDownloader->downloadedFiles()) {
            ingestFile(filePath, activeInstrumentChanged, transferFunctionsMayNeedRefresh);
        }

        if (transferFunctionsMayNeedRefresh) {
            _tfMap = loadTransferFunctions(_imageDirectory, _imageMetadataMap);
        }

        if (activeInstrumentChanged) {
            _predictionIsDirty = true;
            _currentKeyframe = NoActiveKeyframe;
        }

        _dynamicDownloader->clearDownloaded();
    }

    void RenderableSolarImagery::ingestFile(
        const std::filesystem::path& filePath,
        bool& activeInstrumentChanged,
        bool& transferFunctionsMayNeedRefresh)
    {
        const std::optional<ImageMetadata> metadata = parseJ2kMetadata(filePath);
        if (!metadata.has_value()) {
            LERROR(std::format(
                "Failed to parse metadata for streamed Helioviewer image '{}'",
                filePath
            ));
            return;
        }

        const std::optional<double> timestamp = timestampFromHelioviewerFilename(filePath);
        if (!timestamp.has_value()) {
            LERROR(std::format(
                "Failed to derive timestamp from streamed Helioviewer image '{}'",
                filePath
            ));
            return;
        }

        const std::string instrument = instrumentFromHelioviewerFilename(filePath).value_or(
            _dynamicDownloaderInstrument
        );
        const bool hadInstrument = _imageMetadataMap.contains(instrument);
        _imageMetadataMap[instrument].addKeyframe(*timestamp, *metadata);

        if (!hadInstrument) {
            const int optionValue = static_cast<int>(_activeInstruments.options().size());
            _activeInstruments.addOption(optionValue, instrument);
            transferFunctionsMayNeedRefresh = true;

            if (_activeInstruments.options().size() == 1) {
                _activeInstruments = optionValue;
                _currentActiveInstrument = instrument;
                activeInstrumentChanged = true;
            }
        }

        if (instrument == _currentActiveInstrument) {
            activeInstrumentChanged = true;
        }
    }

    TransferFunction* RenderableSolarImagery::transferFunction() {
        return _tfMap[_currentActiveInstrument].get();
    }

    const std::unique_ptr<ghoul::opengl::Texture>&
        RenderableSolarImagery::imageryTexture() const
    {
        return _imageryTexture;
    }

    float RenderableSolarImagery::contrastValue() const {
        return _contrastValue;
    }

    float RenderableSolarImagery::blackTransparencyThreshold() const {
        return _blackTransparencyThreshold;
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

    glm::vec2 RenderableSolarImagery::centerPixel() const {
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
            if (_currentKeyframe != NoActiveKeyframe) {
                // No need to re-upload an empty image
                _isCoronaGraph = false;
                _currentScale = 0;
                _currentCenterPixel = glm::vec2(2.f);
                _currentKeyframe = NoActiveKeyframe;

                // Create some dummy data that will be uploaded to the GPU to avoid UB
                std::vector<unsigned char> buffer;
                buffer.resize(
                    DefaultTextureSize * DefaultTextureSize *
                    sizeof(ImagePrecision)
                );
                _imageryTexture->resize(
                    glm::uvec3(DefaultTextureSize, DefaultTextureSize, 1)
                );
                _imageryTexture->setPixelData(
                    reinterpret_cast<std::byte*>(buffer.data())
                );
            }
            return;
        }

        if (_currentKeyframe == keyframe->id) {
            // This keyframe is already uploaded to the GPU
            return;
        }

        const float fullRes = static_cast<float>(keyframe->data.fullResolution);
        const unsigned int imageSize = static_cast<unsigned int>(
            fullRes / std::pow(2.f, static_cast<float>(_downsamplingLevel))
            );

        SolarBrowsingModule* module = global::moduleEngine->module<SolarBrowsingModule>();

        std::filesystem::path path = keyframe->data.filePath;
        std::filesystem::path cached = module->cacheManager()->cachedFilename(
            path.replace_extension(".bin"),
            std::format("{}x{}", imageSize, imageSize)
        );

        // If the current keyframe image has not yet been decoded and cached we'll just wait
        // until it is available. The previous image will be shown until the new one is ready
        if (std::filesystem::exists(cached)) {
            // Load data from cache
            DecodedImageData data = loadDecodedDataFromCache(
                cached,
                keyframe->data,
                imageSize
            );

            _isCoronaGraph = data.metadata.isCoronaGraph;
            _currentScale = data.metadata.scale;
            _currentCenterPixel = data.metadata.centerPixel;
            _currentKeyframe = keyframe->id;

            _imageryTexture->resize(glm::uvec3(data.imageSize, data.imageSize, 1));
            _imageryTexture->setPixelData(
                reinterpret_cast<std::byte*>(data.buffer.data())
            );
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
        if (!_predictionIsDirty && _lastPredictedKeyframe == keyframe->id) {
            // Already predicted this keyframe
            return;
        }

        // Detect playback direction
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
            // Swap for backwards playtime
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
            [keyframe](const Keyframe<ImageMetadata>& kf) { return &kf == keyframe; }
        );

        if (currentIt == keyframes.end()) {
            return;
        }

        auto requestFrameIfNeeded = [this](const Keyframe<ImageMetadata>& kf) {
            // Check if the keyframe has already been decoded and exists in cache
            const int imageSize = kf.data.fullResolution /
                static_cast<int>(std::pow(2, _downsamplingLevel.value())
                    );

            SolarBrowsingModule* module = global::moduleEngine->module<SolarBrowsingModule>();

            std::filesystem::path path = kf.data.filePath;
            std::filesystem::path cacheFile = module->cacheManager()->cachedFilename(
                path.replace_extension(".bin"),
                std::format("{}x{}", imageSize, imageSize)
            );

            // Skip if file is already cached
            if (std::filesystem::exists(cacheFile)) {
                return;
            }

            // Request new images to decode
            DecodeRequest request(
                kf.data,
                _downsamplingLevel,
                [this, cacheFile](DecodedImageData&& decodedData) {
                    saveDecodedDataToCache(cacheFile, decodedData, _verboseMode);
                }
            );
            _asyncDecoder->requestDecode(std::move(request));
            };

        // Request frames before and after the current keyframe
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

        _lastPredictedKeyframe = keyframe->id;
        _predictionIsDirty = false;
    }

    void RenderableSolarImagery::createPlaneAndFrustum(double moveDistance) {
        // Computing the image plane position using linear scale is not sufficient for fine
        // tuning movement near the Sun. A Gaussian function* (3.1) is used to address this
        // issue: *https://www.diva-portal.org/smash/get/diva2:1147161/FULLTEXT01.pdf
        _gaussianMoveFactor = exp(-(pow((moveDistance - 1), 2.0)) / (2.0));
        _size = static_cast<float>(_gaussianMoveFactor * distanceconstants::SolarRadius);
        createPlane();
        createFrustum();
    }

    void RenderableSolarImagery::createPlane() const {
        const std::array<PlaneVertex, 6> vertexData = {
            PlaneVertex{ glm::vec2(-_size, -_size), glm::vec2(0.f, 0.f) },
            PlaneVertex{ glm::vec2(_size,  _size), glm::vec2(1.f, 1.f) },
            PlaneVertex{ glm::vec2(-_size,  _size), glm::vec2(0.f, 1.f) },
            PlaneVertex{ glm::vec2(-_size, -_size), glm::vec2(0.f, 0.f) },
            PlaneVertex{ glm::vec2(_size, -_size), glm::vec2(1.f, 0.f) },
            PlaneVertex{ glm::vec2(_size,  _size), glm::vec2(1.f, 1.f) },
        };

        glNamedBufferData(
            _vertexPositionBuffer,
            sizeof(vertexData),
            vertexData.data(),
            GL_STATIC_DRAW
        );
    }

    void RenderableSolarImagery::createFrustum() const {
        // Vertex orders x, y, z, w
        // Where w indicates if vertex should be drawn in spacecraft or planes coordinate
        // system
        const std::array<FrustumVertex, 16> vertexData = {
            FrustumVertex{ glm::vec4(0.f,    0.f,   0.f, 0.f) },
            FrustumVertex{ glm::vec4(_size,  _size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(0.f,    0.f,   0.f, 0.f) },
            FrustumVertex{ glm::vec4(-_size, -_size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(0.f,    0.f,   0.f, 0.f) },
            FrustumVertex{ glm::vec4(_size, -_size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(0.f,    0.f,   0.f, 0.f) },
            FrustumVertex{ glm::vec4(-_size,  _size, 0.f, 1.f) },
            // Borders
            // Left
            FrustumVertex{ glm::vec4(-_size, -_size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(-_size,  _size, 0.f, 1.f) },
            // Top
            FrustumVertex{ glm::vec4(-_size,  _size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(_size,  _size, 0.f, 1.f) },
            // Right
            FrustumVertex{ glm::vec4(_size,  _size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(_size, -_size, 0.f, 1.f) },
            // Bottom
            FrustumVertex{ glm::vec4(_size, -_size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(-_size, -_size, 0.f, 1.f) },
        };

        glNamedBufferData(
            _frustumPositionBuffer,
            sizeof(vertexData),
            vertexData.data(),
            GL_STATIC_DRAW
        );
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
