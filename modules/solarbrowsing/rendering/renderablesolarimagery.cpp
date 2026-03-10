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
#include <string_view>

namespace {
    using namespace openspace;

    constexpr double SunRadius = 1391600000.0 * 0.5;
    constexpr unsigned int DefaultTextureSize = 32;

    enum FaceMode {
        FrontOnly = 0,
        SolidBack,
        DoubleSided
    };

    constexpr Property::PropertyInfo ActiveInstrumentsInfo = {
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
        "How close to the Sun to render the imagery.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo DownsamplingLevelInfo = {
        "DownsamplingLevel",
        "Downsampling level",
        "How much to downsample the original data. 0 is original resolution.",
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

    constexpr Property::PropertyInfo CacheDataInfo = {
        "CacheData",
        "Cache downloaded data",
        "Keep dynamically downloaded Helioviewer files on shutdown.",
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
    // and contrast controls. Coronagraph instruments can optionally display frustum
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
        std::optional<std::filesystem::path> imageDirectory [[codegen::directory()]];

        // Choose whether to load existing local imagery or dynamically download a single
        // Helioviewer stream at runtime.
        std::optional<LoadingType> loadingType;

        // The instrument to display on startup (e.g., "AIA-171"). If not specified,
        // the first available instrument is used. In dynamic mode, this is the only
        // supported instrument for Phase 1.
        std::optional<std::string> startInstrument;

        // Dynamic downloading only: folder where downloaded JP2 files are stored.
        std::optional<std::filesystem::path> downloadFolder [[codegen::directory()]];

        // Dynamic downloading only: spacecraft / observatory display name used in file
        // naming, for example "SDO".
        std::optional<std::string> spacecraftName;

        // Dynamic downloading only: Helioviewer sourceId.
        std::optional<int> sourceId;

        // Dynamic downloading only: path to the colormap to use for the downloaded stream.
        std::optional<std::filesystem::path> colorMap;

        // Dynamic downloading only: desired cadence in seconds.
        std::optional<double> cadence;

        // Dynamic downloading only: number of files to queue / track.
        std::optional<int> numberOfFilesToQueue;

        // Dynamic downloading only: keep files on shutdown.
        std::optional<bool> cacheData;

        // [[codegen::verbatim(EnableBorderInfo.description)]]
        std::optional<bool> enableBorder;

        // [[codegen::verbatim(EnableFrustumInfo.description)]]
        std::optional<bool> enableFrustum;

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
    };

    double timestampFromSolarImageFilename(const std::filesystem::path& path) {
        const std::string fileName = path.stem().string();

        auto r = scn::scan<int, int, int, int, int, int, int>(
            fileName,
            "{}_{}_{}__{}_{}_{}_{}"
        );

        if (!r) {
            throw ghoul::RuntimeError(std::format(
                "Failed to parse timestamp from solar image filename '{}'",
                fileName
            ));
        }

        auto& [year, month, day, hour, minute, second, millisecond] = r->values();

        const std::string dateTime = std::format(
            "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}",
            year, month, day, hour, minute, second, millisecond
        );

        return global::timeManager->time().convertTime(dateTime);
    }
} // namespace
#include "renderablesolarimagery_codegen.cpp"

namespace openspace {

openspace::Documentation RenderableSolarImagery::Documentation() {
    return codegen::doc<Parameters>("solarbrowsing_renderablesolarimegary");
}

RenderableSolarImagery::RenderableSolarImagery(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _activeInstruments(ActiveInstrumentsInfo)
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

    _imageMetadataMap = loadImageMetadata(p.imageDirectory);
    _tfMap = loadTransferFunctions(p.imageDirectory, _imageMetadataMap);

    _enableBorder = p.enableBorder.value_or(_enableBorder);
    addProperty(_enableBorder);

    _enableFrustum = p.enableFrustum.value_or(_enableFrustum);
    _enableFrustum.onChange([this]() {
        _enableBorder = _enableFrustum.value();
    });
    addProperty(_enableFrustum);

    _faceMode.addOption(FaceMode::FrontOnly, "Front only");
    _faceMode.addOption(FaceMode::SolidBack, "Solid back");
    _faceMode.addOption(FaceMode::DoubleSided, "Double sided");
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

    RenderableSolarImagery::RenderableSolarImagery(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _activeInstruments(ActiveInstrumentsInfo)
        , _blackTransparencyThreshold(BlackTransparencyThresholdInfo, 0.01f, 0.0f, 0.1f)
        , _contrastValue(ContrastValueInfo, 0.f, -15.f, 15.f)
        , _enableBorder(EnableBorderInfo, false)
        , _enableFrustum(EnableFrustumInfo, false)
        , _gammaValue(GammaValueInfo, 0.9f, 0.1f, 10.f)
        , _moveFactor(MoveFactorInfo, 1.0, 0.0, 1.0)
        , _downsamplingLevel(DownsamplingLevelInfo, 2, 0, 5)
        , _verboseMode(VerboseModeInfo, false)
        , _predictFramesAfter(PredictFramesAfterInfo, 10, 0, 20)
        , _predictFramesBefore(PredictFramesBeforeInfo, 2, 0, 20)
        , _cacheData(CacheDataInfo, false)
    {
        const Parameters p = codegen::bake<Parameters>(dictionary);

        addProperty(Fadeable::_opacity);

        _loadingType =
            (p.loadingType.value_or(Parameters::LoadingType::StaticLoading) ==
                Parameters::LoadingType::StaticLoading)
            ? LoadingType::StaticLoading
            : LoadingType::DynamicDownloading;

        _enableBorder = p.enableBorder.value_or(_enableBorder);
        addProperty(_enableBorder);

        _enableFrustum = p.enableFrustum.value_or(_enableFrustum);
        _enableFrustum.onChange([this]() {
            _enableBorder = _enableFrustum.value();
            });
        addProperty(_enableFrustum);

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

        _cacheData = p.cacheData.value_or(_cacheData);
        if (_loadingType == LoadingType::DynamicDownloading) {
            addProperty(_cacheData);
        }

        _asyncDecoder = std::make_unique<AsyncImageDecoder>(
            std::thread::hardware_concurrency() / 2,
            _verboseMode
        );

        if (_loadingType == LoadingType::StaticLoading) {
            if (!p.imageDirectory.has_value()) {
                throw ghoul::RuntimeError(
                    "RenderableSolarImagery with StaticLoading requires 'imageDirectory'"
                );
            }

            _imageDirectory = *p.imageDirectory;
            _imageMetadataMap = loadImageMetadata(_imageDirectory);
            _tfMap = loadTransferFunctions(_imageDirectory, _imageMetadataMap);

            // Add Instrument GUI names
            unsigned int guiNameCount = 0;
            using T = Timeline<ImageMetadata>;
            for (const std::pair<InstrumentName, T>& instrument : _imageMetadataMap) {
                _activeInstruments.addOption(guiNameCount++, instrument.first);
            }

            if (p.startInstrument.has_value()) {
                _currentActiveInstrument = p.startInstrument.value();

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
                _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
                    _activeInstruments
                );
            }

            _activeInstruments.onChange([this]() {
                _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
                    _activeInstruments
                );
                _currentKeyframe = NoActiveKeyframe;
                _predictionIsDirty = true;
                });
            addProperty(_activeInstruments);
        }
        else {
            if (!p.downloadFolder.has_value()) {
                throw ghoul::RuntimeError(
                    "RenderableSolarImagery with DynamicDownloading requires 'downloadFolder'"
                );
            }
            if (!p.spacecraftName.has_value()) {
                throw ghoul::RuntimeError(
                    "RenderableSolarImagery with DynamicDownloading requires 'spacecraftName'"
                );
            }
            if (!p.sourceId.has_value()) {
                throw ghoul::RuntimeError(
                    "RenderableSolarImagery with DynamicDownloading requires 'sourceId'"
                );
            }
            if (!p.colorMap.has_value()) {
                throw ghoul::RuntimeError(
                    "RenderableSolarImagery with DynamicDownloading requires 'colorMap'"
                );
            }
            if (!p.startInstrument.has_value()) {
                throw ghoul::RuntimeError(
                    "RenderableSolarImagery with DynamicDownloading requires 'startInstrument'"
                );
            }

            _downloadFolder = absPath(*p.downloadFolder);
            _spacecraftName = *p.spacecraftName;
            _sourceId = *p.sourceId;
            _colorMapPath = absPath(*p.colorMap);
            _cadence = p.cadence.value_or(3600.0);
            _nFilesToQueue = p.numberOfFilesToQueue.value_or(20);

            std::filesystem::create_directories(_downloadFolder);

            _currentActiveInstrument = *p.startInstrument;
            _activeInstruments.addOption(0, _currentActiveInstrument);
            _activeInstruments = 0;
            addProperty(_activeInstruments);

            const std::filesystem::path dstColorMap =
                _downloadFolder / std::format("{}.txt", _currentActiveInstrument);

            std::filesystem::copy(
                _colorMapPath,
                dstColorMap,
                std::filesystem::copy_options::skip_existing
            );

            _tfMap[_currentActiveInstrument] = std::make_shared<TransferFunction>(dstColorMap);

            ingestExistingDynamicFiles();
        }
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

        if (it != options.end()) {
            _activeInstruments = it->value;
        }
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
        _currentKeyframe = NoActiveKeyframe;
        _predictionIsDirty = true;
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
    imageUnit.bind(*_imageryTexture);

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
    TransferFunction* lut = _tfMap[_currentActiveInstrument].get();
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
    _tfMap[_currentActiveInstrument]->update();

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
RenderableSolarImagery::imageryTexture() const
{
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
            buffer.resize(static_cast<size_t>(DefaultTextureSize) * DefaultTextureSize *
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

    unsigned int imageSize = static_cast<unsigned int>(
        keyframe->data.fullResolution /
        std::pow(2, static_cast<unsigned int>(_downsamplingLevel))
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

        glCreateVertexArrays(1, &_quadVao);
        glCreateBuffers(1, &_vertexPositionBuffer);
        glVertexArrayVertexBuffer(_quadVao, 0, _vertexPositionBuffer, 0, sizeof(PlaneVertex));

        glEnableVertexArrayAttrib(_quadVao, 0);
        glVertexArrayAttribFormat(_quadVao, 0, 2, GL_FLOAT, GL_FALSE, 0);
        glVertexArrayAttribBinding(_quadVao, 0, 0);

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

        glCreateVertexArrays(1, &_frustumVao);
        glCreateBuffers(1, &_frustumPositionBuffer);
        glVertexArrayVertexBuffer(
            _frustumVao,
            0,
            _frustumPositionBuffer,
            0,
            sizeof(FrustumVertex)
        );

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
        if (_loadingType == LoadingType::DynamicDownloading && _dynamicDownloader) {
            _dynamicDownloader->deinitialize(_cacheData);
        }

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

    bool RenderableSolarImagery::isReady() const {
        return _planeShader && _frustumShader;
    }

    void RenderableSolarImagery::render(const RenderData& data, RendererTasks&) {
        updateImageryTexture();
        const glm::dvec3& sunPositionWorld = sceneGraphNode("Sun")->worldPosition();

        glEnable(GL_CULL_FACE);

        const glm::dmat4& viewMatrix = data.camera.combinedViewMatrix();
        const glm::mat4& projectionMatrix = data.camera.projectionMatrix();

        const glm::dvec3& spacecraftPosWorld = data.modelTransform.translation;
        const glm::dmat3 spacecraftRotWorld = data.modelTransform.rotation;

        const glm::dvec3 sunDir = sunPositionWorld - spacecraftPosWorld;
        const glm::dvec3 offset = sunDir * _gaussianMoveFactor;

        _position = spacecraftPosWorld + offset;
        _normal = glm::normalize(spacecraftPosWorld - sunPositionWorld);

        glm::vec3 worldUp = spacecraftRotWorld * glm::dvec3(0.0, 0.0, 1.0);
        if (std::abs(glm::dot(worldUp, _normal)) > 0.9999) {
            worldUp = glm::dvec3(0.0, 1.0, 0.0);
        }

        glm::vec3 right = glm::normalize(glm::cross(worldUp, _normal));
        glm::vec3 upOnPlane = glm::cross(_normal, right);

        glm::dmat4 rot = glm::dmat4(1.0);
        rot[0] = glm::dvec4(right, 0.0);
        rot[1] = glm::dvec4(upOnPlane, 0.0);
        rot[2] = glm::dvec4(_normal, 0.0);

        _rotation = std::move(rot);

        const glm::dmat4 modelTransform =
            glm::translate(glm::dmat4(1.0), _position) *
            _rotation *
            glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))
            );
        const glm::dmat4 modelViewTransform = viewMatrix * modelTransform;

        const glm::dmat4 spacecraftModelTransform =
            glm::translate(glm::dmat4(1.0), spacecraftPosWorld) *
            _rotation *
            glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))
            );

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
        TransferFunction* lut = _tfMap[_currentActiveInstrument].get();
        if (lut) {
            tfUnit.bind(lut->texture());
            _planeShader->setUniform(_uniformCachePlane.hasLut, true);
        }
        else {
            _planeShader->setUniform(_uniformCachePlane.hasLut, false);
        }
        _planeShader->setUniform(_uniformCachePlane.lut, tfUnit);

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
        if (_loadingType == LoadingType::DynamicDownloading) {
            if (!_dynamicDownloader) {
                _dynamicDownloader = std::make_unique<DynamicHelioviewerImageDownloader>(
                    _downloadFolder,
                    _spacecraftName,
                    _sourceId,
                    _currentActiveInstrument,
                    _cadence,
                    _nFilesToQueue
                );
            }

            _dynamicDownloader->update(
                data.time.j2000Seconds(),
                global::timeManager->deltaTime()
            );

            for (const std::filesystem::path& p : _dynamicDownloader->downloadedFiles()) {
                ingestDownloadedFile(p);
            }
            _dynamicDownloader->clearDownloaded();
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

    TransferFunction* RenderableSolarImagery::transferFunction() {
        return _tfMap[_currentActiveInstrument].get();
    }

    const std::unique_ptr<ghoul::opengl::Texture>&
        RenderableSolarImagery::imageryTexture() const
    {
        return _imageryTexture;
    }

    float RenderableSolarImagery::blackTransparencyThreshold() const {
        return _blackTransparencyThreshold;
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

    float RenderableSolarImagery::planeOpacity() const
    {
        return opacity();
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
            if (_currentKeyframe != NoActiveKeyframe) {
                _isCoronaGraph = false;
                _currentScale = 0;
                _currentCenterPixel = glm::vec2(2.f);
                _currentKeyframe = NoActiveKeyframe;

                std::vector<unsigned char> buffer;
                buffer.resize(
                    static_cast<size_t>(DefaultTextureSize) *
                    DefaultTextureSize *
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
            return;
        }

        unsigned int imageSize = static_cast<unsigned int>(
            keyframe->data.fullResolution /
            std::pow(2, static_cast<unsigned int>(_downsamplingLevel))
            );

        std::filesystem::path path = kf.data.filePath;
        std::filesystem::path cacheFile = module->cacheManager()->cachedFilename(
            path.replace_extension(".bin"),
            std::format("{}x{}", imageSize, imageSize)
        );

        if (std::filesystem::exists(cached)) {
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

        if (!_predictionIsDirty && _lastPredictedKeyframe == keyframe->id) {
            return;
        }

        const double now = data.time.j2000Seconds();
        const double prevTime = data.previousFrameTime.j2000Seconds();
        const double dt = now - prevTime;

        const bool isPlayingForward = dt >= 0;
        const bool isPaused = now == prevTime;

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
            framesBefore = _predictFramesAfter;
            framesAfter = _predictFramesBefore;
        }

        const Timeline<ImageMetadata>& timeline = _imageMetadataMap[_currentActiveInstrument];
        const std::deque<Keyframe<ImageMetadata>>& keyframes = timeline.keyframes();

        auto currentIt = std::find_if(
            keyframes.begin(),
            keyframes.end(),
            [keyframe](const Keyframe<ImageMetadata>& kf) { return &kf == keyframe; }
        );

        if (currentIt == keyframes.end()) {
            return;
        }

        auto requestFrameIfNeeded = [this](const Keyframe<ImageMetadata>& kf) {
            const int imageSize = kf.data.fullResolution /
                static_cast<int>(std::pow(2, _downsamplingLevel.value()));

            std::filesystem::path cacheFile = FileSys.cacheManager()->cachedFilename(
                kf.data.filePath,
                std::format("{}x{}", imageSize, imageSize),
                "solarbrowsing"
            );

            if (std::filesystem::exists(cacheFile)) {
                return;
            }

            DecodeRequest request(
                kf.data,
                _downsamplingLevel,
                [this, cacheFile](DecodedImageData&& decodedData) {
                    saveDecodedDataToCache(cacheFile, decodedData, _verboseMode);
                }
            );
            _asyncDecoder->requestDecode(std::move(request));
            };

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
        _gaussianMoveFactor = exp(-(pow((moveDistance - 1), 2.0)) / (2.0));
        _size = static_cast<float>(_gaussianMoveFactor * SunRadius);
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
        const std::array<FrustumVertex, 16> vertexData = {
            FrustumVertex{ glm::vec4(0.f,    0.f,   0.f, 0.f) },
            FrustumVertex{ glm::vec4(_size,  _size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(0.f,    0.f,   0.f, 0.f) },
            FrustumVertex{ glm::vec4(-_size, -_size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(0.f,    0.f,   0.f, 0.f) },
            FrustumVertex{ glm::vec4(_size, -_size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(0.f,    0.f,   0.f, 0.f) },
            FrustumVertex{ glm::vec4(-_size,  _size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(-_size, -_size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(-_size,  _size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(-_size,  _size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(_size,  _size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(_size,  _size, 0.f, 1.f) },
            FrustumVertex{ glm::vec4(_size, -_size, 0.f, 1.f) },
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

    void RenderableSolarImagery::ingestDownloadedFile(const std::filesystem::path& path) {
        if (_ingestedFiles.contains(path.string())) {
            return;
        }

        std::optional<ImageMetadata> im = parseJ2kMetadata(path);
        if (!im.has_value()) {
            LERROR(std::format("Failed to parse J2K metadata from '{}'", path));
            return;
        }

        const double timestamp = timestampFromSolarImageFilename(path);
        _imageMetadataMap[_currentActiveInstrument].addKeyframe(timestamp, std::move(*im));
        _ingestedFiles.insert(path.string());
        _predictionIsDirty = true;
    }

    void RenderableSolarImagery::ingestExistingDynamicFiles() {
        if (!std::filesystem::is_directory(_downloadFolder)) {
            return;
        }

        std::vector<std::filesystem::path> files = ghoul::filesystem::walkDirectory(
            _downloadFolder,
            ghoul::filesystem::Recursive::No,
            ghoul::filesystem::Sorted::Yes,
            [](const std::filesystem::path& p) {
                const std::string ext = p.extension().string();
                return (ext == ".jp2") || (ext == ".j2k");
            }
        );

        for (const std::filesystem::path& p : files) {
            ingestDownloadedFile(p);
        }
    }

} // namespace openspace
