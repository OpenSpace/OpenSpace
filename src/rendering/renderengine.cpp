/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/rendering/renderengine.h>

#include <openspace/openspace.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/screenlog.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/versionchecker.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/io/texture/texturereadercmap.h>
#include <ghoul/io/model/modelreader.h>
#include <ghoul/io/model/modelreaderassimp.h>
#include <ghoul/io/model/modelreaderbinary.h>
#include <ghoul/io/texture/texturereaderstb.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/easing.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringconversion.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

#include "renderengine_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "RenderEngine";

    constexpr std::chrono::seconds ScreenLogTimeToLive(15);
    constexpr std::string_view RenderFsPath = "${SHADERS}/render.frag";

    constexpr std::string_view KeyFontMono = "Mono";
    constexpr std::string_view KeyFontLight = "Light";

    constexpr openspace::properties::Property::PropertyInfo ShowOverlayClientsInfo = {
        "ShowOverlayOnClients",
        "Show Overlay Information on Clients",
        "If this value is enabled, the overlay information text is also automatically "
        "rendered on client nodes. This values is disabled by default.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShowLogInfo = {
        "ShowLog",
        "Show the on-screen log",
        "This value determines whether the on-screen log will be shown or hidden. Even "
        "if it is shown, all 'Debug' and 'Trace' level messages are omitted from this "
        "log.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo VerticalLogOffsetInfo = {
        "VerticalLogOffset",
        "Vertical Log Offset",
        "The vertical offset for the on-screen log in [0,1] coordinates, a factor that "
        "is scaled with the vertical resolution.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShowVersionInfo = {
        "ShowVersion",
        "Shows the version on-screen information",
        "This value determines whether the Git version information (branch and commit) "
        "hash are shown on the screen.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShowCameraInfo = {
        "ShowCamera",
        "Shows information about the current camera state, such as friction",
        "This value determines whether the information about the current camera state is "
        "shown on the screen.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ScreenshotWindowIdsInfo = {
        "ScreenshotWindowId",
        "Screenshow Window Ids",
        "The list of window identifiers whose screenshot will be taken the next time "
        "anyone triggers a screenshot. If this list is empty (the default), all windows "
        "will have their screenshot taken. Id's that do not exist are silently ignored.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyWarpingInfo = {
        "ApplyWarpingScreenshot",
        "Apply Warping to Screenshots",
        "This value determines whether a warping should be applied before taking a "
        "screenshot. If it is enabled, all post processing is applied as well, which "
        "includes everything rendered on top of the rendering, such as the user "
        "interface.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShowStatisticsInfo = {
        "ShowStatistics",
        "Show Statistics",
        "Show updating, rendering, and network statistics on all rendering nodes.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ScreenshotUseDateInfo = {
        "ScreenshotUseDate",
        "Screenshot Folder uses Date",
        "If this value is set to 'true', screenshots will be saved to a folder that "
        "contains the time at which this value was enabled.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShowFrameNumberInfo = {
        "ShowFrameInformation",
        "Show Frame Information",
        "If this value is enabled, the current frame number and frame times are rendered "
        "into the window.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableMasterInfo = {
        "DisableMasterRendering",
        "Disable Master Rendering",
        "If this value is enabled, the rendering on the master node will be disabled. "
        "Every other aspect of the application will be unaffected by this and it will "
        "still respond to user input. This setting is reasonably only useful in the case "
        "of multi-pipeline environments, such as planetariums, where the output of the "
        "master node is not required and performance can be gained by disabling it.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo GlobalRotationInfo = {
        "GlobalRotation",
        "Global Rotation",
        "Applies a global view rotation. Use this to rotate the position of the "
        "focus node away from the default location on the screen. This setting "
        "persists even when a new focus node is selected. Defined using pitch, yaw, "
        "roll in radians.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ScreenSpaceRotationInfo = {
        "ScreenSpaceRotation",
        "Screen Space Rotation",
        "Applies a rotation to all screen space renderables. Defined using pitch, yaw, "
        "roll in radians.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MasterRotationInfo = {
        "MasterRotation",
        "Master Rotation",
        "Applies a view rotation for only the master node, defined using pitch, yaw, "
        "roll in radians.This can be used to compensate the master view direction for "
        "tilted display systems in clustered immersive environments.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableHDRPipelineInfo = {
        "DisableHDRPipeline",
        "Disable HDR Rendering",
        "If this value is enabled, the rendering will disable the HDR color handling and "
        "the LDR color pipeline will be used. Be aware of possible over exposure in the "
        "final colors.",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo HDRExposureInfo = {
        "HDRExposure",
        "HDR Exposure",
        "This value determines the amount of light per unit area reaching the equivalent "
        "of an electronic image sensor.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo GammaInfo = {
        "Gamma",
        "Gamma Correction",
        "Gamma, is the nonlinear operation used to encode and decode luminance or "
        "tristimulus values in the image",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HueInfo = {
        "Hue",
        "Hue",
        "Hue.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SaturationInfo = {
        "Saturation",
        "Saturation",
        "Saturation.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ValueInfo = {
        "Value",
        "Value",
        "Value.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FramerateLimitInfo = {
        "FramerateLimit",
        "Framerate Limit",
        "If set to a value bigger than 0, the framerate will be limited to that many "
        "frames per second without using V-Sync.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HorizFieldOfViewInfo = {
        "HorizFieldOfView",
        "Horizontal Field of View",
        "Adjusts the degrees of the horizontal field of view. The vertical field of "
        "view will be automatically adjusted to match, according to the current "
        "aspect ratio.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo GlobalBlackoutFactorInfo = {
        "BlackoutFactor",
        "Blackout Factor",
        "The blackout factor of the rendering. This can be used for fading in or out the "
        "rendering window.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyBlackoutToMasterInfo = {
        "ApplyBlackoutToMaster",
        "Apply Blackout to Master",
        "If this value is 'true', the blackout factor is applied to the master node. "
        "Regardless of this value, the clients will always adhere to the factor.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FXAAInfo = {
        "FXAA",
        "Enable FXAA",
        "Enable FXAA.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EnabledFontColorInfo = {
        "EnabledFontColor",
        "Enabled Font Color",
        "The font color used for enabled options.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisabledFontColorInfo = {
        "DisabledFontColor",
        "Disabled Font Color",
        "The font color used for disabled options.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

RenderEngine::RenderEngine()
    : properties::PropertyOwner({ "RenderEngine", "Render Engine" })
    , _showOverlayOnClients(ShowOverlayClientsInfo, false)
    , _showLog(ShowLogInfo, true)
    , _verticalLogOffset(VerticalLogOffsetInfo, 0.f, 0.f, 1.f)
    , _showVersionInfo(ShowVersionInfo, true)
    , _showCameraInfo(ShowCameraInfo, true)
    , _screenshotWindowIds(ScreenshotWindowIdsInfo)
    , _applyWarping(ApplyWarpingInfo, false)
    , _showStatistics(ShowStatisticsInfo, false)
    , _screenshotUseDate(ScreenshotUseDateInfo, false)
    , _showFrameInformation(ShowFrameNumberInfo, false)
    , _disableMasterRendering(DisableMasterInfo, false)
    , _globalBlackOutFactor(GlobalBlackoutFactorInfo, 1.f, 0.f, 1.f)
    , _applyBlackoutToMaster(ApplyBlackoutToMasterInfo, true)
    , _enableFXAA(FXAAInfo, true)
    , _disableHDRPipeline(DisableHDRPipelineInfo, false)
    , _hdrExposure(HDRExposureInfo, 3.7f, 0.01f, 10.f)
    , _gamma(GammaInfo, 0.95f, 0.01f, 5.f)
    , _hue(HueInfo, 0.f, 0.f, 360.f)
    , _saturation(SaturationInfo, 1.f, 0.f, 2.f)
    , _value(ValueInfo, 1.f, 0.f, 2.f)
    , _framerateLimit(FramerateLimitInfo, 0, 0, 500)
    , _horizFieldOfView(HorizFieldOfViewInfo, 80.f, 1.f, 179.f)
    , _globalRotation(
        GlobalRotationInfo,
        glm::vec3(0.f),
        glm::vec3(-glm::pi<float>()),
        glm::vec3(glm::pi<float>())
    )
    , _screenSpaceRotation(
        ScreenSpaceRotationInfo,
        glm::vec3(0.f),
        glm::vec3(-glm::pi<float>()),
        glm::vec3(glm::pi<float>())
    )
    , _masterRotation(
        MasterRotationInfo,
        glm::vec3(0.f),
        glm::vec3(-glm::pi<float>()),
        glm::vec3(glm::pi<float>())
    )
    , _enabledFontColor(EnabledFontColorInfo, glm::vec4(0.2f, 0.75f, 0.2f, 1.f))
    , _disabledFontColor(DisabledFontColorInfo, glm::vec4(0.55f, 0.2f, 0.2f, 1.f))
{
    addProperty(_showOverlayOnClients);
    addProperty(_showLog);
    addProperty(_verticalLogOffset);
    addProperty(_showVersionInfo);
    addProperty(_showCameraInfo);

    _enableFXAA.onChange([this]() { _renderer.enableFXAA(_enableFXAA); });
    addProperty(_enableFXAA);

    _disableHDRPipeline.onChange([this]() {
        _renderer.setDisableHDR(_disableHDRPipeline);
    });
    addProperty(_disableHDRPipeline);

    _hdrExposure.onChange([this]() { _renderer.setHDRExposure(_hdrExposure); });
    addProperty(_hdrExposure);

    _gamma.onChange([this]() { _renderer.setGamma(_gamma); });
    addProperty(_gamma);

    auto setHueValueSaturation = [this]() {
        _renderer.setHueValueSaturation(_hue / 360.f, _value, _saturation);
    };
    _hue.onChange(setHueValueSaturation);
    addProperty(_hue);

    _saturation.onChange(setHueValueSaturation);
    addProperty(_saturation);

    _value.onChange(setHueValueSaturation);
    addProperty(_value);

    addProperty(_globalBlackOutFactor);
    addProperty(_applyBlackoutToMaster);
    addProperty(_screenshotWindowIds);
    addProperty(_applyWarping);

    _showStatistics.onChange([this]() {
        global::windowDelegate->showStatistics(_showStatistics);
    });
    addProperty(_showStatistics);

    _screenshotUseDate.onChange([this]() {
        // If there is no screenshot folder, don't bother with handling the change
        if (!FileSys.hasRegisteredToken("${STARTUP_SCREENSHOT}")) {
            return;
        }

        if (_screenshotUseDate) {
            // Going from 'false' -> 'true'
            // We might need to create the folder first

            const std::time_t now = std::time(nullptr);
            std::tm* nowTime = std::localtime(&now);
            std::array<char, 128> date;
            strftime(date.data(), sizeof(date), "%Y-%m-%d-%H-%M", nowTime);

            const std::filesystem::path newFolder = absPath(
                "${STARTUP_SCREENSHOT}/" + std::string(date.data())
            );

            FileSys.registerPathToken(
                "${SCREENSHOTS}",
                newFolder,
                ghoul::filesystem::FileSystem::Override::Yes
            );
        }
        else {
            // Going from 'true' -> 'false'
            // We reset the screenshot folder back to what it was in the beginning
            FileSys.registerPathToken(
                "${SCREENSHOTS}",
                absPath("${STARTUP_SCREENSHOT}"),
                ghoul::filesystem::FileSystem::Override::Yes
            );
        }
        global::windowDelegate->setScreenshotFolder(absPath("${SCREENSHOTS}"));
    });
    addProperty(_screenshotUseDate);

    _horizFieldOfView.onChange([this]() {
        if (global::windowDelegate->isMaster()) {
            global::windowDelegate->setHorizFieldOfView(_horizFieldOfView);
        }
    });
    addProperty(_horizFieldOfView);

    addProperty(_showFrameInformation);

    addProperty(_framerateLimit);
    addProperty(_globalRotation);
    addProperty(_screenSpaceRotation);
    addProperty(_masterRotation);
    addProperty(_disableMasterRendering);

    _enabledFontColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_enabledFontColor);

    _disabledFontColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_disabledFontColor);
}

RenderEngine::~RenderEngine() {}

const FramebufferRenderer& RenderEngine::renderer() const {
    return _renderer;
}

void RenderEngine::initialize() {
    ZoneScoped;

    // We have to perform these initializations here as the OsEng has not been initialized
    // in our constructor
    _globalRotation = global::configuration->globalRotation;
    _screenSpaceRotation = global::configuration->screenSpaceRotation;
    _masterRotation = global::configuration->masterRotation;
    _disableMasterRendering = global::configuration->isRenderingOnMasterDisabled;
    _screenshotUseDate = global::configuration->shouldUseScreenshotDate;

    ghoul::io::TextureReader::ref().addReader(
        std::make_unique<ghoul::io::TextureReaderSTB>()
    );

    ghoul::io::TextureReader::ref().addReader(
        std::make_unique<ghoul::io::TextureReaderCMAP>()
    );

    ghoul::io::ModelReader::ref().addReader(
        std::make_unique<ghoul::io::ModelReaderAssimp>()
    );

    ghoul::io::ModelReader::ref().addReader(
        std::make_unique<ghoul::io::ModelReaderBinary>()
    );

    _versionString = OPENSPACE_VERSION_STRING_FULL;
    if (global::versionChecker->hasLatestVersionInfo()) {
        VersionChecker::SemanticVersion latest = global::versionChecker->latestVersion();

        const VersionChecker::SemanticVersion current {
            OPENSPACE_VERSION_MAJOR,
            OPENSPACE_VERSION_MINOR,
            OPENSPACE_VERSION_PATCH
        };
        if (current < latest) {
            _versionString += std::format(
                " [Available: {}.{}.{}]", latest.major, latest.minor, latest.patch
            );
        }
    }
}

void RenderEngine::initializeGL() {
    ZoneScoped;

    LTRACE("RenderEngine::initializeGL(begin)");

    _renderer.setResolution(renderingResolution());
    _renderer.enableFXAA(_enableFXAA);
    _renderer.setHDRExposure(_hdrExposure);
    _renderer.initialize();

    // set the close clip plane and the far clip plane to extreme values while in
    // development
    global::windowDelegate->setNearFarClippingPlane(0.001f, 1000.f);

    // Set horizontal FOV value with whatever the field of view (in degrees) is of the
    // initialized window
    _horizFieldOfView = static_cast<float>(global::windowDelegate->getHorizFieldOfView());

    const Configuration::FontSizes fontSize = global::configuration->fontSize;
    {
        ZoneScopedN("Fonts");
        TracyGpuZone("Fonts");
        _fontFrameInfo = global::fontManager->font(KeyFontMono, fontSize.frameInfo);
        _fontShutdown = global::fontManager->font(KeyFontMono, fontSize.shutdown);
        _fontCameraInfo = global::fontManager->font(KeyFontMono, fontSize.cameraInfo);
        _fontVersionInfo = global::fontManager->font(KeyFontMono, fontSize.versionInfo);
        _fontLog = global::fontManager->font(KeyFontLight, fontSize.log);
    }

    {
        ZoneScopedN("Log");
        LINFO("Initializing Log");
        auto log = std::make_unique<ScreenLog>(ScreenLogTimeToLive);
        _log = log.get();
        ghoul::logging::LogManager::ref().addLog(std::move(log));
    }

    LINFO("Finished initializing GL");
    LTRACE("RenderEngine::initializeGL(end)");
}

void RenderEngine::deinitializeGL() {
    ZoneScoped;

    _renderer.deinitialize();
}

void RenderEngine::updateScene() {
    ZoneScoped;

    if (!_scene) {
        return;
    }

    _scene->updateInterpolations();

    const Time& currentTime = global::timeManager->time();
    const Time& integrateFromTime = global::timeManager->integrateFromTime();

    _scene->update({
        TransformData{ glm::dvec3(0.0), glm::dmat3(1.0), glm::dvec3(1.0) },
        currentTime,
        integrateFromTime
    });

    LTRACE("RenderEngine::updateSceneGraph(end)");
}

void RenderEngine::updateShaderPrograms() {
    ZoneScoped;

    for (ghoul::opengl::ProgramObject* program : _programs) {
        try {
            if (program->isDirty()) {
                program->rebuildFromFile();
            }
        }
        catch (const ghoul::opengl::ShaderObject::ShaderCompileError& e) {
            LERRORC(e.component, e.what());
        }
    }
}

void RenderEngine::updateRenderer() {
    ZoneScoped;

    const bool windowResized = global::windowDelegate->windowHasResized();

    if (windowResized) {
        _renderer.setResolution(renderingResolution());

        using FR = ghoul::fontrendering::FontRenderer;
        FR::defaultRenderer().setFramebufferSize(fontResolution());
        FR::defaultProjectionRenderer().setFramebufferSize(renderingResolution());
        // Override the aspect ratio property value to match that of resized window
        _horizFieldOfView =
            static_cast<float>(global::windowDelegate->getHorizFieldOfView());
    }

    _renderer.update();
}

void RenderEngine::updateScreenSpaceRenderables() {
    ZoneScoped;

    for (std::unique_ptr<ScreenSpaceRenderable>& ssr : *global::screenSpaceRenderables) {
        ssr->update();
    }
}

glm::ivec2 RenderEngine::renderingResolution() const {
    return global::windowDelegate->currentDrawBufferResolution();
}

glm::ivec2 RenderEngine::fontResolution() const {
    const std::string& value = global::configuration->onScreenTextScaling;
    if (value == "framebuffer") {
        return global::windowDelegate->currentViewportSize();
    }
    else {
        return global::windowDelegate->currentViewportResolution();
    }
}

glm::mat4 RenderEngine::globalRotation() const {
    const glm::vec3 rot = _globalRotation;

    const glm::quat pitch = glm::angleAxis(rot.x, glm::vec3(1.f, 0.f, 0.f));
    const glm::quat yaw = glm::angleAxis(rot.y, glm::vec3(0.f, 1.f, 0.f));
    const glm::quat roll = glm::angleAxis(rot.z, glm::vec3(0.f, 0.f, 1.f));

    return glm::mat4_cast(glm::normalize(pitch * yaw * roll));
}

glm::mat4 RenderEngine::screenSpaceRotation() const {
    const glm::vec3 rot = _screenSpaceRotation;

    const glm::quat pitch = glm::angleAxis(rot.x, glm::vec3(1.f, 0.f, 0.f));
    const glm::quat yaw = glm::angleAxis(rot.y, glm::vec3(0.f, 1.f, 0.f));
    const glm::quat roll = glm::angleAxis(rot.z, glm::vec3(0.f, 0.f, 1.f));

    return glm::mat4_cast(glm::normalize(pitch * yaw * roll));
}

glm::mat4 RenderEngine::nodeRotation() const {
    if (!global::windowDelegate->isMaster()) {
        return glm::mat4(1.f);
    }
    const glm::vec3 rot = _masterRotation;

    const glm::quat pitch = glm::angleAxis(rot.x, glm::vec3(1.f, 0.f, 0.f));
    const glm::quat yaw = glm::angleAxis(rot.y, glm::vec3(0.f, 1.f, 0.f));
    const glm::quat roll = glm::angleAxis(rot.z, glm::vec3(0.f, 0.f, 1.f));

    return glm::mat4_cast(glm::normalize(pitch * yaw * roll));
}

uint64_t RenderEngine::frameNumber() const {
    return _frameNumber;
}

void RenderEngine::render(const glm::mat4& sceneMatrix, const glm::mat4& viewMatrix,
                          const glm::mat4& projectionMatrix)
{
    ZoneScoped;

    LTRACE("RenderEngine::render(begin)");

    const WindowDelegate& delegate = *global::windowDelegate;

    const glm::mat4 globalRot = globalRotation();
    const glm::mat4 nodeRot = nodeRotation();
    const glm::mat4 combinedGlobalRot = nodeRot * globalRot;

    if (_camera) {
        _camera->sgctInternal.setViewMatrix(viewMatrix * combinedGlobalRot * sceneMatrix);
        _camera->sgctInternal.setSceneMatrix(combinedGlobalRot * sceneMatrix);
        _camera->sgctInternal.setProjectionMatrix(projectionMatrix);
        _camera->invalidateCache();
    }

    const int fpsLimit = _framerateLimit;
    if (fpsLimit > 0) {
        // Using a sleep here is not optimal, but we are not looking for perfect timing
        std::this_thread::sleep_until(_lastFrameTime);
        const double delta = (1.0 / fpsLimit) * 1000.0 * 1000.0;
        auto now = std::chrono::high_resolution_clock::now();
        _lastFrameTime = now + std::chrono::microseconds(static_cast<int>(delta));
    }

    const bool renderingEnabled = delegate.isMaster() ? !_disableMasterRendering : true;
    if (renderingEnabled && combinedBlackoutFactor() > 0.f) {
        _renderer.render(_scene, _camera, combinedBlackoutFactor());
    }

    // The CEF webbrowser fix has to be called at least once per frame and we are doing
    // that in the renderer::render method.  So if we disable the rendering, that fix is
    // no longer called as we lose access to the Web UI.  Since we are calling the fix
    // many times anyway, we can just add one call to it here and not lose much
    if (global::callback::webBrowserPerformanceHotfix) {
        (*global::callback::webBrowserPerformanceHotfix)();
    }

    if (_showFrameInformation) {
        ZoneScopedN("Show Frame Information");

        glm::vec2 penPosition = glm::vec2(
            fontResolution().x / 2 - 50,
            fontResolution().y / 3
        );

        std::string fn = std::to_string(_frameNumber);
        const WindowDelegate::Frustum frustum = global::windowDelegate->frustumMode();
        std::string fr = [](WindowDelegate::Frustum f) -> std::string {
            switch (f) {
                case WindowDelegate::Frustum::Mono:     return "";
                case WindowDelegate::Frustum::LeftEye:  return "(left)";
                case WindowDelegate::Frustum::RightEye: return "(right)";
                default:                              throw ghoul::MissingCaseException();
            }
        }(frustum);

        std::string sgFn = std::to_string(global::windowDelegate->swapGroupFrameNumber());
        std::string dt = std::to_string(global::windowDelegate->deltaTime());
        std::string avgDt = std::to_string(global::windowDelegate->averageDeltaTime());

        const std::string res = std::format(
            "Frame: {} {}\nSwap group frame: {}\nDt: {}\nAvg Dt: {}",
            fn, fr, sgFn, dt, avgDt
        );
        RenderFont(*_fontFrameInfo, penPosition, res);
    }

    if (renderingEnabled && !delegate.isGuiWindow()) {
        ZoneScopedN("Render Screenspace Renderable");

        std::vector<ScreenSpaceRenderable*> ssrs;
        ssrs.reserve(global::screenSpaceRenderables->size());
        for (const std::unique_ptr<ScreenSpaceRenderable>& ssr :
            *global::screenSpaceRenderables)
        {
            if (ssr->isEnabled() && ssr->isReady()) {
                ssrs.push_back(ssr.get());
            }
        }

        std::sort(
            ssrs.begin(),
            ssrs.end(),
            [](ScreenSpaceRenderable* lhs, ScreenSpaceRenderable* rhs) {
                // Render back to front
                return lhs->depth() > rhs->depth();
            }
        );


        glDisable(GL_DEPTH_TEST);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        for (ScreenSpaceRenderable* ssr : ssrs) {
            ssr->render(combinedBlackoutFactor());
        }
        glDisable(GL_BLEND);
    }
    LTRACE("RenderEngine::render(end)");
}

bool RenderEngine::mouseActivationCallback(const glm::dvec2& mousePosition) const {
    auto intersects = [](const glm::dvec2& mousePos, const glm::ivec4& bbox) {
        return mousePos.x >= bbox.x && mousePos.x <= bbox.x + bbox.z &&
               mousePos.y <= bbox.y && mousePos.y >= bbox.y - bbox.w;
    };


    if (intersects(mousePosition, _cameraButtonLocations.rotation)) {
        constexpr std::string_view ToggleRotationFrictionScript = R"(
            local f = 'NavigationHandler.OrbitalNavigator.Friction.RotationalFriction';
            openspace.setPropertyValueSingle(f, not openspace.propertyValue(f));)";

        global::scriptEngine->queueScript(
            std::string(ToggleRotationFrictionScript),
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
        return true;
    }

    if (intersects(mousePosition, _cameraButtonLocations.zoom)) {
        constexpr std::string_view ToggleZoomFrictionScript = R"(
            local f = 'NavigationHandler.OrbitalNavigator.Friction.ZoomFriction';
            openspace.setPropertyValueSingle(f, not openspace.propertyValue(f));)";

        global::scriptEngine->queueScript(
            std::string(ToggleZoomFrictionScript),
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
        return true;
    }

    if (intersects(mousePosition, _cameraButtonLocations.roll)) {
        constexpr std::string_view ToggleRollFrictionScript = R"(
            local f = 'NavigationHandler.OrbitalNavigator.Friction.RollFriction';
            openspace.setPropertyValueSingle(f, not openspace.propertyValue(f));)";

        global::scriptEngine->queueScript(
            std::string(ToggleRollFrictionScript),
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
        return true;
    }

    return false;
}

void RenderEngine::renderOverlays(const ShutdownInformation& shutdownInfo) {
    ZoneScoped;

    const bool isMaster = global::windowDelegate->isMaster();
    if (isMaster || _showOverlayOnClients) {
        renderScreenLog();
        renderVersionInformation();
        renderDashboard();
        renderCameraInformation();

        if (shutdownInfo.inShutdown) {
            renderShutdownInformation(shutdownInfo.timer, shutdownInfo.waitTime);
        }
    }
}

void RenderEngine::renderEndscreen() {
    glEnable(GL_BLEND);

    rendering::helper::renderBox(
        glm::vec2(0.f),
        glm::vec2(1.f),
        glm::vec4(0.f, 0.f, 0.f, 0.5f)
    );

    const glm::vec2 dpiScaling = global::windowDelegate->dpiScaling();
    const glm::ivec2 res =
        glm::vec2(global::windowDelegate->firstWindowResolution()) / dpiScaling;
    glViewport(0, 0, res.x, res.y);

    constexpr std::string_view Text = "Shutting down";
    const glm::vec2 size = _fontShutdown->boundingBox(Text);
    glm::vec2 penPosition = glm::vec2(
        fontResolution().x / 2 - size.x / 2,
        fontResolution().y / 2 - size.y / 2
    );
    RenderFont(*_fontShutdown, penPosition, Text);
}

void RenderEngine::renderShutdownInformation(float timer, float fullTime) {
    ZoneScoped;

    timer = std::max(timer, 0.f);

    // Render progressive overlay
    glEnable(GL_BLEND);

    // t = 1.f -> start of shutdown counter    t = 0.f -> timer has reached shutdown
    const float t = 1.f - (timer / fullTime);

    rendering::helper::renderBox(
        glm::vec2(0.f),
        glm::vec2(1.f),
        glm::vec4(0.f, 0.f, 0.f, ghoul::circularEaseOut(t))
    );

    // No need to print the text if we are just about to finish since otherwise we'll be
    // overplotting the actual "shutdown in progress" text
    if (timer == 0.f) {
        return;
    }

    constexpr std::string_view FirstLine = "Shutdown in: {:.2f}s/{:.2f}s";
    const glm::vec2 size1 = _fontShutdown->boundingBox(
        std::format(FirstLine, timer, fullTime)
    );

    glm::vec2 penPosition = glm::vec2(
        fontResolution().x / 2 - size1.x / 2,
        fontResolution().y / 2 - size1.y / 2
    );

    RenderFont(
        *_fontShutdown,
        penPosition,
        std::format(FirstLine, timer, fullTime),
        ghoul::fontrendering::CrDirection::Down
    );
    // Important: Length of this string is the same as the first line to make them align
    RenderFont(*_fontShutdown, penPosition, "Press ESC again to abort");
}

void RenderEngine::renderDashboard() const {
    ZoneScoped;

    const glm::vec2 dashboardStart = global::dashboard->getStartPositionOffset();
    glm::vec2 penPosition = glm::vec2(
        dashboardStart.x,
        dashboardStart.y + fontResolution().y - global::luaConsole->currentHeight()
    );

    global::dashboard->render(penPosition);
}

float RenderEngine::combinedBlackoutFactor() const {
    if (global::windowDelegate->isMaster()) {
        return _applyBlackoutToMaster ? _globalBlackOutFactor : 1.f;
    }
    else {
        return _globalBlackOutFactor;
    }
}

void RenderEngine::postDraw() {
    ZoneScoped;

    ++_frameNumber;
}

Scene* RenderEngine::scene() {
    return _scene;
}

void RenderEngine::setScene(Scene* scene) {
    _scene = scene;
}

void RenderEngine::setCamera(Camera* camera) {
    _camera = camera;
}

ghoul::opengl::OpenGLStateCache& RenderEngine::openglStateCache() {
    if (_openglStateCache == nullptr) {
        _openglStateCache = ghoul::opengl::OpenGLStateCache::instance();
    }
    return *_openglStateCache;
}

float RenderEngine::hdrExposure() const {
    return _hdrExposure;
}

bool RenderEngine::isHdrDisabled() const {
    return _disableHDRPipeline;
}

/**
 * Build a program object for rendering with the used renderer
 */
std::unique_ptr<ghoul::opengl::ProgramObject> RenderEngine::buildRenderProgram(
                                                                  const std::string& name,
                                                      const std::filesystem::path& vsPath,
                                                      const std::filesystem::path& fsPath,
                                                                   ghoul::Dictionary data)
{
    ghoul::Dictionary dict = std::move(data);

    // set path to the current renderer's main fragment shader
    dict.setValue("rendererData", _rendererData);
    // parameterize the main fragment shader program with specific contents.
    // fsPath should point to a shader file defining a Fragment getFragment() function
    // instead of a void main() setting glFragColor, glFragDepth, etc.
    dict.setValue("fragmentPath", fsPath);

    using namespace ghoul::opengl;
    std::unique_ptr<ProgramObject> program = ProgramObject::Build(
        name,
        vsPath,
        absPath(RenderFsPath),
        dict
    );

    if (program) {
        _programs.push_back(program.get());
    }
    return program;
}

/**
* Build a program object for rendering with the used renderer
*/
std::unique_ptr<ghoul::opengl::ProgramObject> RenderEngine::buildRenderProgram(
                                                                  const std::string& name,
                                                      const std::filesystem::path& vsPath,
                                                      const std::filesystem::path& fsPath,
                                                      const std::filesystem::path& csPath,
                                                                   ghoul::Dictionary data)
{
    ghoul::Dictionary dict = std::move(data);
    dict.setValue("rendererData", _rendererData);

    // parameterize the main fragment shader program with specific contents.
    // fsPath should point to a shader file defining a Fragment getFragment() function
    // instead of a void main() setting glFragColor, glFragDepth, etc.
    dict.setValue("fragmentPath", fsPath);

    using namespace ghoul::opengl;
    std::unique_ptr<ProgramObject> program = ProgramObject::Build(
        name,
        vsPath,
        absPath(RenderFsPath),
        csPath,
        dict
    );

    if (program) {
        _programs.push_back(program.get());
    }
    return program;
}

void RenderEngine::removeRenderProgram(ghoul::opengl::ProgramObject* program) {
    if (!program) {
        return;
    }

    auto it = std::find(_programs.begin(), _programs.end(), program);
    if (it != _programs.end()) {
        _programs.erase(it);
    }
}

void RenderEngine::setRendererData(ghoul::Dictionary rendererData) {
    _rendererData = std::move(rendererData);
    for (ghoul::opengl::ProgramObject* program : _programs) {
        ghoul::Dictionary dict = program->dictionary();
        dict.setValue("rendererData", _rendererData);
        program->setDictionary(dict);
    }
}

void RenderEngine::setResolveData(ghoul::Dictionary resolveData) {
    _resolveData = std::move(resolveData);
    for (ghoul::opengl::ProgramObject* program : _programs) {
        ghoul::Dictionary dict = program->dictionary();
        dict.setValue("resolveData", _resolveData);
        program->setDictionary(dict);
    }
}

void RenderEngine::takeScreenshot() {
    // We only create the directory here, as we don't want to spam the users
    // screenshot folder everytime we start OpenSpace even when we are not taking any
    // screenshots. So the first time we actually take one, we create the folder:

    if (!std::filesystem::is_directory(absPath("${SCREENSHOTS}"))) {
        std::filesystem::create_directories(absPath("${SCREENSHOTS}"));
    }

    _latestScreenshotNumber = global::windowDelegate->takeScreenshot(
        _applyWarping,
        _screenshotWindowIds
    );
}

/**
 * Resets the screenshot index to 0
 */
void RenderEngine::resetScreenshotNumber() {
    _latestScreenshotNumber = 0;
    global::windowDelegate->resetScreenshotNumber();
}

unsigned int RenderEngine::latestScreenshotNumber() const {
    return _latestScreenshotNumber;
}

scripting::LuaLibrary RenderEngine::luaLibrary() {
    return {
        "",
        {
            codegen::lua::AddScreenSpaceRenderable,
            codegen::lua::RemoveScreenSpaceRenderable,
            codegen::lua::TakeScreenshot,
            codegen::lua::DpiScaling,
            codegen::lua::ResetScreenshotNumber
        }
    };
}

void RenderEngine::addScreenSpaceRenderable(std::unique_ptr<ScreenSpaceRenderable> s) {
    const std::string identifier = s->identifier();

    auto it = std::find_if(
        global::screenSpaceRenderables->begin(),
        global::screenSpaceRenderables->end(),
        [&identifier](const std::unique_ptr<ScreenSpaceRenderable>& ssr) {
            return ssr->identifier() == identifier;
        }
    );
    if (it != global::screenSpaceRenderables->end()) {
        LERROR(std::format(
            "Cannot add scene space renderable. Identifier '{}' already exists",
            identifier
        ));
        return;
    }

    s->initialize();
    s->initializeGL();

    // We should do one update cycle to make sure that we have all the data that we need
    s->update();

    ScreenSpaceRenderable* ssr = s.get();
    global::screenSpaceRootPropertyOwner->addPropertySubOwner(ssr);
    global::screenSpaceRenderables->push_back(std::move(s));

    global::eventEngine->publishEvent<events::EventScreenSpaceRenderableAdded>(ssr);
}

void RenderEngine::removeScreenSpaceRenderable(ScreenSpaceRenderable* s) {
    const auto it = std::find_if(
        global::screenSpaceRenderables->begin(),
        global::screenSpaceRenderables->end(),
        [s](const std::unique_ptr<ScreenSpaceRenderable>& r) { return r.get() == s; }
    );

    if (it != global::screenSpaceRenderables->end()) {
        global::eventEngine->publishEvent<events::EventScreenSpaceRenderableRemoved>(s);
        s->deinitializeGL();
        s->deinitialize();
        global::screenSpaceRootPropertyOwner->removePropertySubOwner(s);
        global::screenSpaceRenderables->erase(it);
    }
}

void RenderEngine::removeScreenSpaceRenderable(std::string_view identifier) {
    ScreenSpaceRenderable* s = screenSpaceRenderable(identifier);
    if (s) {
        removeScreenSpaceRenderable(s);
    }
}

ScreenSpaceRenderable* RenderEngine::screenSpaceRenderable(std::string_view identifier) {
    const auto it = std::find_if(
        global::screenSpaceRenderables->begin(),
        global::screenSpaceRenderables->end(),
        [&identifier](const std::unique_ptr<ScreenSpaceRenderable>& s) {
            return s->identifier() == identifier;
        }
    );

    if (it != global::screenSpaceRenderables->end()) {
        return it->get();
    }
    else {
        return nullptr;
    }
}

std::vector<ScreenSpaceRenderable*> RenderEngine::screenSpaceRenderables() const {
    std::vector<ScreenSpaceRenderable*> res(global::screenSpaceRenderables->size());
    std::transform(
        global::screenSpaceRenderables->begin(),
        global::screenSpaceRenderables->end(),
        res.begin(),
        [](const std::unique_ptr<ScreenSpaceRenderable>& p) { return p.get(); }
    );
    return res;
}

void RenderEngine::renderCameraInformation() {
    ZoneScoped;

    if (!_showCameraInfo) {
        return;
    }

    const glm::vec4 EnabledColor = _enabledFontColor;
    const glm::vec4 DisabledColor = _disabledFontColor;

    const glm::vec2 rotationBox = _fontCameraInfo->boundingBox("Rotation");

    float penPosY = fontResolution().y - rotationBox.y;

    constexpr float YSeparation = 5.f;
    constexpr float XSeparation = 5.f;

    const interaction::OrbitalNavigator& nav =
        global::navigationHandler->orbitalNavigator();

    using FR = ghoul::fontrendering::FontRenderer;

    _cameraButtonLocations.rotation = glm::ivec4(
        fontResolution().x - rotationBox.x - XSeparation,
        fontResolution().y - penPosY,
        rotationBox.x,
        rotationBox.y
    );
    FR::defaultRenderer().render(
        *_fontCameraInfo,
        glm::vec2(fontResolution().x - rotationBox.x - XSeparation, penPosY),
        "Rotation",
        nav.hasRotationalFriction() ? EnabledColor : DisabledColor
    );
    penPosY -= rotationBox.y + YSeparation;

    const glm::vec2 zoomBox = _fontCameraInfo->boundingBox("Zoom");

    _cameraButtonLocations.zoom = glm::ivec4(
        fontResolution().x - zoomBox.x - XSeparation,
        fontResolution().y - penPosY,
        zoomBox.x,
        zoomBox.y
    );
    FR::defaultRenderer().render(
        *_fontCameraInfo,
        glm::vec2(fontResolution().x - zoomBox.x - XSeparation, penPosY),
        "Zoom",
        nav.hasZoomFriction() ? EnabledColor : DisabledColor
    );
    penPosY -= zoomBox.y + YSeparation;

    const glm::vec2 rollBox = _fontCameraInfo->boundingBox("Roll");

    _cameraButtonLocations.roll = glm::ivec4(
        fontResolution().x - rollBox.x - XSeparation,
        fontResolution().y - penPosY,
        rollBox.x,
        rollBox.y
    );
    FR::defaultRenderer().render(
        *_fontCameraInfo,
        glm::vec2(fontResolution().x - rollBox.x - XSeparation, penPosY),
        "Roll",
        nav.hasRollFriction() ? EnabledColor : DisabledColor
    );
}

void RenderEngine::renderVersionInformation() {
    ZoneScoped;

    if (!_showVersionInfo) {
        return;
    }

    using FR = ghoul::fontrendering::FontRenderer;
    const glm::vec2 versionBox = _fontVersionInfo->boundingBox(_versionString);
    const glm::vec2 commitBox = _fontVersionInfo->boundingBox(OPENSPACE_GIT_FULL);

    FR::defaultRenderer().render(
        *_fontVersionInfo,
        glm::vec2(fontResolution().x - versionBox.x - 10.f, 5.f),
        _versionString,
        glm::vec4(0.5f, 0.5f, 0.5f, 1.f)
    );

    // If a developer hasn't placed the Git command in the path, this variable will be
    // empty
    if (!OPENSPACE_GIT_COMMIT.empty()) {
        // We check OPENSPACE_GIT_COMMIT but use OPENSPACE_GIT_FULL on purpose since
        // OPENSPACE_GIT_FULL will never be empty (always will contain at least @, but
        // checking for that is a bit brittle)
        FR::defaultRenderer().render(
            *_fontVersionInfo,
            glm::vec2(fontResolution().x - commitBox.x - 10.f, versionBox.y + 5.f),
            OPENSPACE_GIT_FULL,
            glm::vec4(0.5, 0.5, 0.5, 1.f)
        );
    }

#ifdef _DEBUG
    [[maybe_unused]] float debugOffset = 0.f;
    {
        const glm::vec2 debugBox = _fontVersionInfo->boundingBox("Debug build");
        debugOffset = debugBox.y;
        const glm::vec2 penPosition = glm::vec2(
            fontResolution().x - debugBox.x - 10.f,
            versionBox.y + commitBox.y + 5.f
        );
        FR::defaultRenderer().render(
            *_fontVersionInfo,
            penPosition,
            "Debug build",
            glm::vec4(0.2f, 0.75f, 0.15f, 1.f)
        );
    }
#else // !_DEBUG
    [[maybe_unused]] const float debugOffset = 0.f;
#endif // _DEBUG

#ifdef TRACY_ENABLE
    {
        // If we have Tracy enabled, we should inform the user about it that the
        // application will crash after a while if no profiler is attached

        ZoneScopedN("Tracy Information");

        const glm::vec2 tracyBox = _fontVersionInfo->boundingBox("TRACY PROFILING");
        const glm::vec2 penPosition = glm::vec2(
            fontResolution().x - tracyBox.x - 10.f,
            versionBox.y + commitBox.y + debugOffset + 5.f
        );
        FR::defaultRenderer().render(
            *_fontVersionInfo,
            penPosition,
            "TRACY PROFILING",
            glm::vec4(0.8f, 0.2f, 0.15f, 1.f)
        );
    }
#endif // TRACY_ENABLE

}

void RenderEngine::renderScreenLog() {
    ZoneScoped;

    if (!_showLog) {
        return;
    }

    _log->removeExpiredEntries();

    constexpr size_t MaxNumberMessages = 20;
    constexpr int CategoryLength = 30;
    constexpr int MessageLength = 280;
    constexpr std::chrono::seconds FadeTime(5);

    const std::vector<ScreenLog::LogEntry>& entries = _log->entries();
    const glm::ivec2 fontRes = fontResolution();

    size_t nRows = 1;
    const auto now = std::chrono::steady_clock::now();
    for (size_t i = 1; i <= std::min(MaxNumberMessages, entries.size()); i += 1) {
        ZoneScopedN("Entry");

        const ScreenLog::LogEntry& it = entries[entries.size() - i];

        const std::chrono::duration<double> diff = now - it.timeStamp;

        float alpha = 1.f;
        const std::chrono::duration<double> ttf = ScreenLogTimeToLive - FadeTime;
        if (diff > ttf) {
            const double d = (diff - ttf).count();
            const float t = static_cast<float>(d) / static_cast<float>(FadeTime.count());
            const float p = 0.8f - t;
            alpha = (p <= 0.f) ? 0.f : std::pow(p, 0.4f);
        }

        // Since all log entries are ordered, once one exceeds alpha, all have
        if (alpha <= 0.f) {
            break;
        }
        const std::string_view message =
            std::string_view(it.message).substr(0, MessageLength);
        nRows += std::count(message.begin(), message.end(), '\n');

        const glm::vec4 white = glm::vec4(0.9f, 0.9f, 0.9f, alpha);

        std::array<char, 15 + 1 + CategoryLength + 3> buf;
        {
            std::fill(buf.begin(), buf.end(), char(0));
            char* end = std::format_to(
                buf.data(),
                "{:<15} {}{}",
                it.timeString,
                std::string_view(it.category).substr(0, CategoryLength),
                it.category.length() > CategoryLength ? "..." : ""
            );

            RenderFont(
                *_fontLog,
                glm::vec2(
                    10.f,
                    _fontLog->pointSize() * nRows * 2 + fontRes.y * _verticalLogOffset
                ),
                std::string_view(buf.data(), end - buf.data()),
                white
            );
        }

        {
            glm::vec4 color = ghoul::toColor(it.level);
            color.a = alpha;

            const std::string_view lvl = ghoul::to_string(it.level);
            std::fill(buf.begin(), buf.end(), char(0));
            char* end = std::format_to(buf.data(), "({})", lvl);
            RenderFont(
                *_fontLog,
                glm::vec2(
                    10 + (30 + 3) * _fontLog->pointSize(),
                    _fontLog->pointSize() * nRows * 2 + fontRes.y * _verticalLogOffset
                ),
                std::string_view(buf.data(), end - buf.data()),
                color
            );
        }

        RenderFont(
            *_fontLog,
            glm::vec2(
                10 + 44 * _fontLog->pointSize(),
                _fontLog->pointSize() * nRows * 2 + fontRes.y * _verticalLogOffset
            ),
            message,
            white
        );
        ++nRows;
    }
}

} // namespace openspace
