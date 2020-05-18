/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/performance/performancemanager.h>
#include <openspace/performance/performancemeasurement.h>
#include <openspace/rendering/abufferrenderer.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/screenlog.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/versionchecker.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/io/texture/texturereadercmap.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringconversion.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

#ifdef GHOUL_USE_DEVIL
#include <ghoul/io/texture/texturereaderdevil.h>
#endif //GHOUL_USE_DEVIL
#ifdef GHOUL_USE_FREEIMAGE
#include <ghoul/io/texture/texturereaderfreeimage.h>
#endif // GHOUL_USE_FREEIMAGE

#ifdef GHOUL_USE_SOIL
#include <ghoul/io/texture/texturereadersoil.h>
#include <ghoul/io/texture/texturewriter.h>
#include <ghoul/io/texture/texturewritersoil.h>
#endif //GHOUL_USE_SOIL

#ifdef GHOUL_USE_STB_IMAGE
#include <ghoul/io/texture/texturereaderstb.h>
#endif // GHOUL_USE_STB_IMAGE

#include "renderengine_lua.inl"

namespace {
    constexpr const char* _loggerCat = "RenderEngine";

    constexpr const std::chrono::seconds ScreenLogTimeToLive(15);
    constexpr const char* RenderFsPath = "${SHADERS}/render.frag";

    constexpr const char* KeyFontMono = "Mono";
    constexpr const char* KeyFontLight = "Light";

    constexpr openspace::properties::Property::PropertyInfo PerformanceInfo = {
        "PerformanceMeasurements",
        "Performance Measurements",
        "If this value is enabled, detailed performance measurements about the updates "
        "and rendering of the scene graph nodes are collected each frame. These values "
        "provide some information about the impact of individual nodes on the overall "
        "performance."
    };

    constexpr openspace::properties::Property::PropertyInfo ShowOverlaySlavesInfo = {
        "ShowOverlayOnSlaves",
        "Show Overlay Information on Slaves",
        "If this value is enabled, the overlay information text is also automatically "
        "rendered on the slave nodes. This values is disabled by default."
    };

    constexpr openspace::properties::Property::PropertyInfo ShowLogInfo = {
        "ShowLog",
        "Show the on-screen log",
        "This value determines whether the on-screen log will be shown or hidden. Even "
        "if it is shown, all 'Debug' and 'Trace' level messages are omitted from this "
        "log."
    };

    constexpr openspace::properties::Property::PropertyInfo ShowVersionInfo = {
        "ShowVersion",
        "Shows the version on-screen information",
        "This value determines whether the Git version information (branch and commit) "
        "hash are shown on the screen."
    };

    constexpr openspace::properties::Property::PropertyInfo ShowCameraInfo = {
        "ShowCamera",
        "Shows information about the current camera state, such as friction",
        "This value determines whether the information about the current camrea state is "
        "shown on the screen"
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyWarpingInfo = {
        "ApplyWarpingScreenshot",
        "Apply Warping to Screenshots",
        "This value determines whether a warping should be applied before taking a "
        "screenshot. If it is enabled, all post processing is applied as well, which "
        "includes everything rendered on top of the rendering, such as the user "
        "interface."
    };

    constexpr openspace::properties::Property::PropertyInfo ShowFrameNumberInfo = {
        "ShowFrameInformation",
        "Show Frame Information",
        "If this value is enabled, the current frame number and frame times are rendered "
        "into the window."
    };

#ifdef OPENSPACE_WITH_INSTRUMENTATION
    constexpr openspace::properties::Property::PropertyInfo SaveFrameInfo = {
        "SaveFrameInformation",
        "Save Frame Information",
        "Saves the frame information to disk"
    };
#endif // OPENSPACE_WITH_INSTRUMENTATION

    constexpr openspace::properties::Property::PropertyInfo DisableMasterInfo = {
        "DisableMasterRendering",
        "Disable Master Rendering",
        "If this value is enabled, the rendering on the master node will be disabled. "
        "Every other aspect of the application will be unaffected by this and it will "
        "still respond to user input. This setting is reasonably only useful in the case "
        "of multi-pipeline environments, such as planetariums, where the output of the "
        "master node is not required and performance can be gained by disabling it."
    };

    constexpr openspace::properties::Property::PropertyInfo GlobalRotationInfo = {
        "GlobalRotation",
        "Global Rotation",
        "Applies a global view rotation. Use this to rotate the position of the "
        "focus node away from the default location on the screen. This setting "
        "persists even when a new focus node is selected. Defined using pitch, yaw, "
        "roll in radians"
    };

    constexpr openspace::properties::Property::PropertyInfo ScreenSpaceRotationInfo = {
        "ScreenSpaceRotation",
        "Screen Space Rotation",
        "Applies a rotation to all screen space renderables. "
        "Defined using pitch, yaw, roll in radians."
    };

    constexpr openspace::properties::Property::PropertyInfo MasterRotationInfo = {
        "MasterRotation",
        "Master Rotation",
        "Applies a view rotation for only the master node, defined using "
        "pitch, yaw, roll in radians. This can be used to compensate the master view "
        "direction for tilted display systems in clustered immersive environments."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableHDRPipelineInfo = {
       "DisableHDRPipeline",
       "Disable HDR Rendering",
       "If this value is enabled, the rendering will disable the HDR color handling "
       "and the LDR color pipeline will be used. Be aware of possible over exposure "
       "in the final colors."
    };

    constexpr openspace::properties::Property::PropertyInfo HDRExposureInfo = {
        "HDRExposure",
        "HDR Exposure",
        "This value determines the amount of light per unit area reaching the "
        "equivalent of an electronic image sensor."
    };

    constexpr openspace::properties::Property::PropertyInfo GammaInfo = {
        "Gamma",
        "Gamma Correction",
        "Gamma, is the nonlinear operation used to encode and decode luminance or "
        "tristimulus values in the image."
    };

    constexpr openspace::properties::Property::PropertyInfo HueInfo = {
        "Hue",
        "Hue",
        "Hue"
    };

    constexpr openspace::properties::Property::PropertyInfo SaturationInfo = {
        "Saturation",
        "Saturation",
        "Saturation"
    };

    constexpr openspace::properties::Property::PropertyInfo ValueInfo = {
        "Value",
        "Value",
        "Value"
    };

    constexpr openspace::properties::Property::PropertyInfo HorizFieldOfViewInfo = {
        "HorizFieldOfView",
        "Horizontal Field of View",
        "Adjusts the degrees of the horizontal field of view. The vertical field of "
        "view will be automatically adjusted to match, according to the current "
        "aspect ratio."
    };

    constexpr openspace::properties::Property::PropertyInfo GlobalBlackoutFactorInfo = {
        "BlackoutFactor",
        "Blackout Factor",
        "The blackout factor of the rendering. This can be used for fading in or out the "
        "rendering window"
    };

    constexpr openspace::properties::Property::PropertyInfo FXAAInfo = {
        "FXAA",
        "Enable FXAA",
        "Enable FXAA"
    };
} // namespace


namespace openspace {

RenderEngine::RenderEngine()
    : properties::PropertyOwner({ "RenderEngine" })
    , _doPerformanceMeasurements(PerformanceInfo)
    , _showOverlayOnSlaves(ShowOverlaySlavesInfo, false)
    , _showLog(ShowLogInfo, true)
    , _showVersionInfo(ShowVersionInfo, true)
    , _showCameraInfo(ShowCameraInfo, true)
    , _applyWarping(ApplyWarpingInfo, false)
    , _showFrameInformation(ShowFrameNumberInfo, false)
#ifdef OPENSPACE_WITH_INSTRUMENTATION
    , _saveFrameInformation(SaveFrameInfo, false)
#endif // OPENSPACE_WITH_INSTRUMENTATION
    , _disableMasterRendering(DisableMasterInfo, false)
    , _globalBlackOutFactor(GlobalBlackoutFactorInfo, 1.f, 0.f, 1.f)
    , _enableFXAA(FXAAInfo, true)
    , _disableHDRPipeline(DisableHDRPipelineInfo, false)
    , _hdrExposure(HDRExposureInfo, 3.7f, 0.01f, 10.f)
    , _gamma(GammaInfo, 0.95f, 0.01f, 5.f)
    , _hue(HueInfo, 0.f, 0.f, 360.f)
    , _saturation(SaturationInfo, 1.f, 0.0f, 2.f)
    , _value(ValueInfo, 1.f, 0.f, 2.f)
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
{
    _doPerformanceMeasurements.onChange([this](){
        global::performanceManager.setEnabled(_doPerformanceMeasurements);
    });
    addProperty(_doPerformanceMeasurements);

    addProperty(_showOverlayOnSlaves);
    addProperty(_showLog);
    addProperty(_showVersionInfo);
    addProperty(_showCameraInfo);

    // @TODO (maci 2019-08-23) disabling FXAA on
    // MacOS for now until we have fix or MSAA option.
#ifdef __APPLE__
    _enableFXAA = false;
#endif

    _enableFXAA.onChange([this]() {
        if (_renderer) {
            _renderer->enableFXAA(_enableFXAA);
        }
        });
    addProperty(_enableFXAA);

    _disableHDRPipeline.onChange([this]() {
        if (_renderer) {
            _renderer->setDisableHDR(_disableHDRPipeline);
        }
    });
    addProperty(_disableHDRPipeline);

    _hdrExposure.onChange([this]() {
        if (_renderer) {
            _renderer->setHDRExposure(_hdrExposure);
        }
    });
    addProperty(_hdrExposure);

    _gamma.onChange([this]() {
        if (_renderer) {
            _renderer->setGamma(_gamma);
        }
    });
    addProperty(_gamma);

    _hue.onChange([this]() {
        if (_renderer) {
            const float h = _hue / 360.f;
            _renderer->setHue(h);
        }
    });

    addProperty(_hue);

    _saturation.onChange([this]() {
        if (_renderer) {
            _renderer->setSaturation(_saturation);
        }
    });

    addProperty(_saturation);

    _value.onChange([this]() {
        if (_renderer) {
            _renderer->setValue(_value);
        }
    });

    addProperty(_value);

    addProperty(_globalBlackOutFactor);
    addProperty(_applyWarping);

    _horizFieldOfView.onChange([this]() {
        if (global::windowDelegate.isMaster()) {
            global::windowDelegate.setHorizFieldOfView(_horizFieldOfView);
        }
    });
    addProperty(_horizFieldOfView);

    addProperty(_showFrameInformation);
#ifdef OPENSPACE_WITH_INSTRUMENTATION
    _saveFrameInformation.onChange([&]() {
        if (_saveFrameInformation) {
            _frameInfo.lastSavedFrame = frameNumber();
        }
    });
    addProperty(_saveFrameInformation);
#endif // OPENSPACE_WITH_INSTRUMENTATION

    addProperty(_globalRotation);
    addProperty(_screenSpaceRotation);
    addProperty(_masterRotation);
    addProperty(_disableMasterRendering);
}

RenderEngine::~RenderEngine() {} // NOLINT

void RenderEngine::setRendererFromString(const std::string& renderingMethod) {
    _rendererImplementation = rendererFromString(renderingMethod);

    std::unique_ptr<Renderer> newRenderer = nullptr;
    switch (_rendererImplementation) {
        case RendererImplementation::Framebuffer:
            newRenderer = std::make_unique<FramebufferRenderer>();
            break;
        case RendererImplementation::ABuffer:
#ifdef OPENSPACE_WITH_ABUFFER_RENDERER
            newRenderer = std::make_unique<ABufferRenderer>();
#endif // OPENSPACE_WITH_ABUFFER_RENDERER
            break;
        case RendererImplementation::Invalid:
            LFATAL(fmt::format("Rendering method '{}' not available", renderingMethod));
            return;
    }

    setRenderer(std::move(newRenderer));
}

void RenderEngine::initialize() {
    ZoneScoped

    // We have to perform these initializations here as the OsEng has not been initialized
    // in our constructor
    _globalRotation = static_cast<glm::vec3>(global::configuration.globalRotation);
    _screenSpaceRotation =
        static_cast<glm::vec3>(global::configuration.screenSpaceRotation);
    _masterRotation = static_cast<glm::vec3>(global::configuration.masterRotation);
    _disableMasterRendering = global::configuration.isRenderingOnMasterDisabled;

#ifdef GHOUL_USE_DEVIL
    ghoul::io::TextureReader::ref().addReader(
        std::make_unique<ghoul::io::TextureReaderDevIL>()
    );
#endif // GHOUL_USE_DEVIL
#ifdef GHOUL_USE_FREEIMAGE
    ghoul::io::TextureReader::ref().addReader(
        std::make_unique<ghoul::io::TextureReaderFreeImage>()
    );
#endif // GHOUL_USE_FREEIMAGE
#ifdef GHOUL_USE_SOIL
    ghoul::io::TextureReader::ref().addReader(
        std::make_unique<ghoul::io::TextureReaderSOIL>()
    );
    ghoul::io::TextureWriter::ref().addWriter(
        std::make_unique<ghoul::io::TextureWriterSOIL>()
    );
#endif // GHOUL_USE_SOIL
#ifdef GHOUL_USE_STB_IMAGE
    ghoul::io::TextureReader::ref().addReader(
        std::make_unique<ghoul::io::TextureReaderSTB>()
    );
#endif // GHOUL_USE_STB_IMAGE

    ghoul::io::TextureReader::ref().addReader(
        std::make_unique<ghoul::io::TextureReaderCMAP>()
    );
}

void RenderEngine::initializeGL() {
    ZoneScoped

    LTRACE("RenderEngine::initializeGL(begin)");

    std::string renderingMethod = global::configuration.renderingMethod;
    if (renderingMethod == "ABuffer") {
        using Version = ghoul::systemcapabilities::Version;

        // The default rendering method has a requirement of OpenGL 4.3, so if we are
        // below that, we will fall back to frame buffer operation
        if (OpenGLCap.openGLVersion() < Version{ 4,3,0 }) {
            LINFO("Falling back to framebuffer implementation due to OpenGL limitations");
            renderingMethod = "Framebuffer";
        }
    }

    LINFO(fmt::format("Setting renderer from string: {}", renderingMethod));
    setRendererFromString(renderingMethod);



    // TODO:    Fix the power scaled coordinates in such a way that these
    //            values can be set to more realistic values

    // set the close clip plane and the far clip plane to extreme values while in
    // development
    global::windowDelegate.setNearFarClippingPlane(0.001f, 1000.f);

    // Set horizontal FOV value with whatever the field of view (in degrees) is of the
    // initialized window
    _horizFieldOfView = static_cast<float>(global::windowDelegate.getHorizFieldOfView());

    constexpr const float FontSizeFrameinfo = 32.f;
    _fontFrameInfo = global::fontManager.font(KeyFontMono, FontSizeFrameinfo);
    constexpr const float FontSizeTime = 15.f;
    _fontDate = global::fontManager.font(KeyFontMono, FontSizeTime);
    constexpr const float FontSizeMono = 10.f;
    _fontInfo = global::fontManager.font(KeyFontMono, FontSizeMono);
    constexpr const float FontSizeLight = 8.f;
    _fontLog = global::fontManager.font(KeyFontLight, FontSizeLight);

    LINFO("Initializing Log");
    std::unique_ptr<ScreenLog> log = std::make_unique<ScreenLog>(ScreenLogTimeToLive);
    _log = log.get();
    ghoul::logging::LogManager::ref().addLog(std::move(log));

    LINFO("Finished initializing GL");
    LTRACE("RenderEngine::initializeGL(end)");
}

void RenderEngine::deinitializeGL() {
    ZoneScoped

    _renderer = nullptr;
}

void RenderEngine::updateScene() {
    ZoneScoped

    if (!_scene) {
        return;
    }

    _scene->updateInterpolations();

    const Time& currentTime = global::timeManager.time();
    const Time& integrateFromTime = global::timeManager.integrateFromTime();

    _scene->update({
        { glm::dvec3(0.0), glm::dmat3(1.0), glm::dvec3(1.0) },
        currentTime,
        integrateFromTime,
        _doPerformanceMeasurements
    });

    LTRACE("RenderEngine::updateSceneGraph(end)");
}

void RenderEngine::updateShaderPrograms() {
    ZoneScoped

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
    ZoneScoped

    const bool windowResized = global::windowDelegate.windowHasResized();

    if (windowResized) {
        _renderer->setResolution(renderingResolution());

        using FR = ghoul::fontrendering::FontRenderer;
        FR::defaultRenderer().setFramebufferSize(fontResolution());
        FR::defaultProjectionRenderer().setFramebufferSize(renderingResolution());
        //Override the aspect ratio property value to match that of resized window
        _horizFieldOfView =
            static_cast<float>(global::windowDelegate.getHorizFieldOfView());
    }

    _renderer->update();
}

void RenderEngine::updateScreenSpaceRenderables() {
    ZoneScoped

    for (std::unique_ptr<ScreenSpaceRenderable>& ssr : global::screenSpaceRenderables) {
        ssr->update();
    }
}

glm::ivec2 RenderEngine::renderingResolution() const {
    return global::windowDelegate.currentDrawBufferResolution();
}

glm::ivec2 RenderEngine::fontResolution() const {
    const std::string& value = global::configuration.onScreenTextScaling;
    if (value == "framebuffer") {
        return global::windowDelegate.currentViewportSize();
    }
    else {
        return global::windowDelegate.currentSubwindowSize();
    }
}

glm::mat4 RenderEngine::globalRotation() const {
    glm::vec3 rot = _globalRotation;

    glm::quat pitch = glm::angleAxis(rot.x, glm::vec3(1.f, 0.f, 0.f));
    glm::quat yaw = glm::angleAxis(rot.y, glm::vec3(0.f, 1.f, 0.f));
    glm::quat roll = glm::angleAxis(rot.z, glm::vec3(0.f, 0.f, 1.f));

    return glm::mat4_cast(glm::normalize(pitch * yaw * roll));
}

glm::mat4 RenderEngine::screenSpaceRotation() const {
    glm::vec3 rot = _screenSpaceRotation;

    glm::quat pitch = glm::angleAxis(rot.x, glm::vec3(1.f, 0.f, 0.f));
    glm::quat yaw = glm::angleAxis(rot.y, glm::vec3(0.f, 1.f, 0.f));
    glm::quat roll = glm::angleAxis(rot.z, glm::vec3(0.f, 0.f, 1.f));

    return glm::mat4_cast(glm::normalize(pitch * yaw * roll));
}

glm::mat4 RenderEngine::nodeRotation() const {
    if (!global::windowDelegate.isMaster()) {
        return glm::mat4(1.f);
    }
    glm::vec3 rot = _masterRotation;

    glm::quat pitch = glm::angleAxis(rot.x, glm::vec3(1.f, 0.f, 0.f));
    glm::quat yaw = glm::angleAxis(rot.y, glm::vec3(0.f, 1.f, 0.f));
    glm::quat roll = glm::angleAxis(rot.z, glm::vec3(0.f, 0.f, 1.f));

    return glm::mat4_cast(glm::normalize(pitch * yaw * roll));
}

uint64_t RenderEngine::frameNumber() const {
    return _frameNumber;
}

void RenderEngine::render(const glm::mat4& sceneMatrix, const glm::mat4& viewMatrix,
                          const glm::mat4& projectionMatrix)
{
    ZoneScoped

    LTRACE("RenderEngine::render(begin)");

    const WindowDelegate& delegate = global::windowDelegate;

    const glm::mat4 globalRot = globalRotation();
    const glm::mat4 nodeRot = nodeRotation();
    glm::mat4 combinedGlobalRot = nodeRot * globalRot;

    if (_camera) {
        _camera->sgctInternal.setViewMatrix(
            viewMatrix * combinedGlobalRot * sceneMatrix
        );
        _camera->sgctInternal.setSceneMatrix(combinedGlobalRot * sceneMatrix);
        _camera->sgctInternal.setProjectionMatrix(projectionMatrix);
        _camera->invalidateCache();
    }

    const bool masterEnabled = delegate.isMaster() ? !_disableMasterRendering : true;
    if (masterEnabled && !delegate.isGuiWindow() && _globalBlackOutFactor > 0.f) {
        _renderer->render(
            _scene,
            _camera,
            _globalBlackOutFactor
        );
    }

    if (_showFrameInformation) {
        ZoneScopedN("Show Frame Information")

        glm::vec2 penPosition = glm::vec2(
            fontResolution().x / 2 - 50,
            fontResolution().y / 3
        );

        std::string fn = std::to_string(_frameNumber);
        WindowDelegate::Frustum frustum = global::windowDelegate.frustumMode();
        std::string fr = [](WindowDelegate::Frustum frustum) -> std::string {
            switch (frustum) {
                case WindowDelegate::Frustum::Mono: return "";
                case WindowDelegate::Frustum::LeftEye: return "(left)";
                case WindowDelegate::Frustum::RightEye: return "(right)";
                default: throw std::logic_error("Unhandled case label");
            }
        }(frustum);

        std::string sgFn = std::to_string(global::windowDelegate.swapGroupFrameNumber());
        std::string dt = std::to_string(global::windowDelegate.deltaTime());
        std::string avgDt = std::to_string(global::windowDelegate.averageDeltaTime());

        std::string res = "Frame: " + fn + ' ' + fr + '\n' +
                          "Swap group frame: " + sgFn + '\n' +
                          "Dt: " + dt + '\n' + "Avg Dt: " + avgDt;
        RenderFont(*_fontFrameInfo, penPosition, res);
    }

    if (masterEnabled && !delegate.isGuiWindow() && _globalBlackOutFactor > 0.f) {
        ZoneScopedN("Render Screenspace Renderable")

        std::vector<ScreenSpaceRenderable*> ssrs;
        ssrs.reserve(global::screenSpaceRenderables.size());
        for (const std::unique_ptr<ScreenSpaceRenderable>& ssr :
            global::screenSpaceRenderables)
        {
            if (ssr->isEnabled() && ssr->isReady()) {
                ssrs.push_back(ssr.get());
            }
        }

        std::sort(
            ssrs.begin(),
            ssrs.end(),
            [](ScreenSpaceRenderable* lhs, ScreenSpaceRenderable* rhs) {
                // Render back to front.
                return lhs->depth() > rhs->depth();
            }
        );


        glDisable(GL_DEPTH_TEST);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        for (ScreenSpaceRenderable* ssr : ssrs) {
            ssr->render();
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
        constexpr const char* ToggleRotationFrictionScript = R"(
            local f = 'NavigationHandler.OrbitalNavigator.Friction.RotationalFriction';
            openspace.setPropertyValue(f, not openspace.getPropertyValue(f));)";

        global::scriptEngine.queueScript(
            ToggleRotationFrictionScript,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        return true;
    }

    if (intersects(mousePosition, _cameraButtonLocations.zoom)) {
        constexpr const char* ToggleZoomFrictionScript = R"(
            local f = 'NavigationHandler.OrbitalNavigator.Friction.ZoomFriction';
            openspace.setPropertyValue(f, not openspace.getPropertyValue(f));)";

        global::scriptEngine.queueScript(
            ToggleZoomFrictionScript,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        return true;
    }

    if (intersects(mousePosition, _cameraButtonLocations.roll)) {
        constexpr const char* ToggleRollFrictionScript = R"(
            local f = 'NavigationHandler.OrbitalNavigator.Friction.RollFriction';
            openspace.setPropertyValue(f, not openspace.getPropertyValue(f));)";

        global::scriptEngine.queueScript(
            ToggleRollFrictionScript,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        return true;
    }

    return false;
}

void RenderEngine::renderOverlays(const ShutdownInformation& shutdownInfo) {
    ZoneScoped

    const bool isMaster = global::windowDelegate.isMaster();
    if (isMaster || _showOverlayOnSlaves) {
        renderScreenLog();
        renderVersionInformation();
        renderDashboard();

        if (!shutdownInfo.inShutdown) {
            // We render the camera information in the same location as the shutdown info
            // and we won't need this if we are shutting down
            renderCameraInformation();
        }
        else {
            // If we are in shutdown mode, we can display the remaining time
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

    const glm::vec2 dpiScaling = global::windowDelegate.dpiScaling();
    const glm::ivec2 res =
        glm::vec2(global::windowDelegate.currentSubwindowSize()) / dpiScaling;
    glViewport(0, 0, res.x, res.y);

    using FR = ghoul::fontrendering::FontRenderer;
    using BBox = FR::BoundingBoxInformation;
    BBox size = FR::defaultRenderer().boundingBox(
        *_fontDate,
        "Shutting down"
    );
    glm::vec2 penPosition = glm::vec2(
        fontResolution().x / 2 - size.boundingBox.x / 2,
        fontResolution().y / 2 - size.boundingBox.y / 2
    );
    RenderFont(*_fontDate, penPosition, "Shutting down");
}

void RenderEngine::renderShutdownInformation(float timer, float fullTime) {
    ZoneScoped

    timer = std::max(timer, 0.f);

    using BBox = ghoul::fontrendering::FontRenderer::BoundingBoxInformation;
    BBox size = ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_fontDate,
        fmt::format("Shutdown in: {:.2f}s/{:.2f}s", timer, fullTime)
    );

    glm::vec2 penPosition = glm::vec2(
        fontResolution().x - size.boundingBox.x - 10,
        fontResolution().y - size.boundingBox.y
    );

    RenderFont(
        *_fontDate,
        penPosition,
        fmt::format("Shutdown in: {:.2f}s/{:.2f}s", timer, fullTime),
        ghoul::fontrendering::CrDirection::Down
    );

    RenderFont(
        *_fontDate,
        penPosition,
        // Important: length of this string is the same as the shutdown time text
        // to make them align
        "Press ESC again to abort",
        ghoul::fontrendering::CrDirection::Down
    );
}

void RenderEngine::renderDashboard() {
    ZoneScoped

    std::unique_ptr<performance::PerformanceMeasurement> perf;
    if (global::performanceManager.isEnabled()) {
        perf = std::make_unique<performance::PerformanceMeasurement>(
            "Main Dashboard::render"
        );
    }

    glm::vec2 dashboardStart = global::dashboard.getStartPositionOffset();
    glm::vec2 penPosition = glm::vec2(
        dashboardStart.x,
        dashboardStart.y + fontResolution().y - global::luaConsole.currentHeight()
    );

    global::dashboard.render(penPosition);

#ifdef REALTIME_CAMERA_POS_DISPLAY
    penPosition += glm::vec2(0.f, -50.f);

    glm::dvec3 p = _camera->positionVec3();
    glm::dquat rot = _camera->rotationQuaternion();
    std::string fc = global::navigationHandler.focusNode()->identifier();
    RenderFont(
        *_fontInfo,
        penPosition,
        fmt::format("Pos: {} {} {}\nOrientation: {} {} {} {}\nFocus: {}",
            p.x, p.y, p.z, rot[0], rot[1], rot[2], rot[3], fc
            )
    );
#endif
}

void RenderEngine::postDraw() {
    ZoneScoped

    ++_frameNumber;

    if (global::performanceManager.isEnabled()) {
        global::performanceManager.storeScenePerformanceMeasurements(
            scene()->allSceneGraphNodes()
        );
    }

#ifdef OPENSPACE_WITH_INSTRUMENTATION
    if (_saveFrameInformation) {
        _frameInfo.frames.push_back({
            frameNumber(),
            global::windowDelegate.deltaTime(),
            global::windowDelegate.averageDeltaTime()
        });
    }

    const uint16_t next = _frameInfo.lastSavedFrame + _frameInfo.saveEveryNthFrame;
    const bool shouldSave = _saveFrameInformation && frameNumber() >= next;
    if (shouldSave) {
        std::string filename = fmt::format(
            "_inst_renderengine_{}_{}.txt",
            _frameInfo.lastSavedFrame, _frameInfo.saveEveryNthFrame
        );
        std::ofstream file(absPath("${BIN}/" + filename));
        for (const FrameInfo& i : _frameInfo.frames) {
            std::string line = fmt::format(
                "{}\t{}\t{}", i.iFrame, i.deltaTime, i.avgDeltaTime
            );
            file << line << '\n';
        }

        _frameInfo.frames.clear();
        _frameInfo.lastSavedFrame = frameNumber();
    }
#endif // OPENSPACE_WITH_INSTRUMENTATION
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

const Renderer& RenderEngine::renderer() const {
    return *_renderer;
}

RenderEngine::RendererImplementation RenderEngine::rendererImplementation() const {
    return _rendererImplementation;
}

float RenderEngine::globalBlackOutFactor() {
    return _globalBlackOutFactor;
}

void RenderEngine::setGlobalBlackOutFactor(float opacity) {
    _globalBlackOutFactor = opacity;
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
                                                                const std::string& vsPath,
                                                                       std::string fsPath,
                                                                   ghoul::Dictionary data)
{
    ghoul::Dictionary dict = std::move(data);

    // set path to the current renderer's main fragment shader
    dict.setValue("rendererData", _rendererData);
    // parameterize the main fragment shader program with specific contents.
    // fsPath should point to a shader file defining a Fragment getFragment() function
    // instead of a void main() setting glFragColor, glFragDepth, etc.
    dict.setValue("fragmentPath", std::move(fsPath));

    using namespace ghoul::opengl;
    std::unique_ptr<ProgramObject> program = ProgramObject::Build(
        name,
        vsPath,
        absPath(RenderFsPath),
        std::move(dict)
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
                                                                const std::string& vsPath,
                                                                       std::string fsPath,
                                                                const std::string& csPath,
                                                                   ghoul::Dictionary data)
{
    ghoul::Dictionary dict = std::move(data);
    dict.setValue("rendererData", _rendererData);

    // parameterize the main fragment shader program with specific contents.
    // fsPath should point to a shader file defining a Fragment getFragment() function
    // instead of a void main() setting glFragColor, glFragDepth, etc.
    dict.setValue("fragmentPath", std::move(fsPath));

    using namespace ghoul::opengl;
    std::unique_ptr<ProgramObject> program = ProgramObject::Build(
        name,
        vsPath,
        absPath(RenderFsPath),
        csPath,
        std::move(dict)
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

    auto it = std::find(
        _programs.begin(),
        _programs.end(),
        program
    );

    if (it != _programs.end()) {
        _programs.erase(it);
    }
}

/**
* Set renderer data
* Called from the renderer, whenever it needs to update
* the dictionary of all rendering programs.
*/
void RenderEngine::setRendererData(ghoul::Dictionary rendererData) {
    _rendererData = std::move(rendererData);
    for (ghoul::opengl::ProgramObject* program : _programs) {
        ghoul::Dictionary dict = program->dictionary();
        dict.setValue("rendererData", _rendererData);
        program->setDictionary(dict);
    }
}

/**
* Set resolve data
* Called from the renderer, whenever it needs to update
* the dictionary of all post rendering programs.
*/
void RenderEngine::setResolveData(ghoul::Dictionary resolveData) {
    _resolveData = std::move(resolveData);
    for (ghoul::opengl::ProgramObject* program : _programs) {
        ghoul::Dictionary dict = program->dictionary();
        dict.setValue("resolveData", _resolveData);
        program->setDictionary(dict);
    }
}

/**
 * Take a screenshot and store it in the ${SCREENSHOTS} directory
 */
void RenderEngine::takeScreenshot() {
    // We only create the directory here, as we don't want to spam the users
    // screenshot folder everytime we start OpenSpace even when we are not taking any
    // screenshots. So the first time we actually take one, we create the folder:

    if (!FileSys.directoryExists(absPath("${SCREENSHOTS}"))) {
        FileSys.createDirectory(
            absPath("${SCREENSHOTS}"),
            ghoul::filesystem::FileSystem::Recursive::Yes
        );
    }

    _latestScreenshotNumber = global::windowDelegate.takeScreenshot(_applyWarping);
}

/**
 * Get the latest screenshot filename
 */
unsigned int RenderEngine::latestScreenshotNumber() const {
    return _latestScreenshotNumber;
}

/**
 * Set raycasting uniforms on the program object, and setup raycasting.
 */
void RenderEngine::preRaycast(ghoul::opengl::ProgramObject& programObject) {
    _renderer->preRaycast(programObject);
}

/**
 * Tear down raycasting for the specified program object.
 */
void RenderEngine::postRaycast(ghoul::opengl::ProgramObject& programObject) {
    _renderer->postRaycast(programObject);
}

/**
 * Set renderer
 */
void RenderEngine::setRenderer(std::unique_ptr<Renderer> renderer) {
    if (_renderer) {
        _renderer->deinitialize();
    }

    _renderer = std::move(renderer);
    _renderer->setResolution(renderingResolution());
    _renderer->enableFXAA(_enableFXAA);
    _renderer->setHDRExposure(_hdrExposure);
    _renderer->initialize();
}

scripting::LuaLibrary RenderEngine::luaLibrary() {
    return {
        "",
        {
            {
                "setRenderer",
                &luascriptfunctions::setRenderer,
                {},
                "string",
                "Sets the renderer (ABuffer or FrameBuffer)"
            },
            {
                "addScreenSpaceRenderable",
                &luascriptfunctions::addScreenSpaceRenderable,
                {},
                "table",
                "Will create a ScreenSpaceRenderable from a lua Table and add it in the "
                "RenderEngine"
            },
            {
                "removeScreenSpaceRenderable",
                &luascriptfunctions::removeScreenSpaceRenderable,
                {},
                "string",
                "Given a ScreenSpaceRenderable name this script will remove it from the "
                "renderengine"
            },
            {
                "takeScreenshot",
                &luascriptfunctions::takeScreenshot,
                {},
                "",
                "Take a screenshot and return the screenshot number. The screenshot will "
                "be stored in the ${SCREENSHOTS} folder. "
            }
        },
    };
}

void RenderEngine::addScreenSpaceRenderable(std::unique_ptr<ScreenSpaceRenderable> s) {

    const std::string identifier = s->identifier();

    if (std::find_if(
        global::screenSpaceRenderables.begin(),
        global::screenSpaceRenderables.end(),
        [&identifier](const std::unique_ptr<ScreenSpaceRenderable>& ssr) {
            return ssr->identifier() == identifier;
        }) != global::screenSpaceRenderables.end()
    ) {
        LERROR(fmt::format(
            "Cannot add scene space renderable. "
            "An element with identifier '{}' already exists",
            identifier
        ));
        return;
    }

    s->initialize();
    s->initializeGL();

    global::screenSpaceRootPropertyOwner.addPropertySubOwner(s.get());
    global::screenSpaceRenderables.push_back(std::move(s));
}

void RenderEngine::removeScreenSpaceRenderable(ScreenSpaceRenderable* s) {
    const auto it = std::find_if(
        global::screenSpaceRenderables.begin(),
        global::screenSpaceRenderables.end(),
        [s](const std::unique_ptr<ScreenSpaceRenderable>& r) { return r.get() == s; }
    );

    if (it != global::screenSpaceRenderables.end()) {
        s->deinitialize();
        global::screenSpaceRootPropertyOwner.removePropertySubOwner(s);

        global::screenSpaceRenderables.erase(it);
    }
}

void RenderEngine::removeScreenSpaceRenderable(const std::string& identifier) {
    ScreenSpaceRenderable* s = screenSpaceRenderable(identifier);
    if (s) {
        removeScreenSpaceRenderable(s);
    }
}

ScreenSpaceRenderable* RenderEngine::screenSpaceRenderable(
                                                            const std::string& identifier)
{
    const auto it = std::find_if(
        global::screenSpaceRenderables.begin(),
        global::screenSpaceRenderables.end(),
        [&identifier](const std::unique_ptr<ScreenSpaceRenderable>& s) {
            return s->identifier() == identifier;
        }
    );

    if (it != global::screenSpaceRenderables.end()) {
        return it->get();
    }
    else {
        return nullptr;
    }
}

std::vector<ScreenSpaceRenderable*> RenderEngine::screenSpaceRenderables() const {
    std::vector<ScreenSpaceRenderable*> res(global::screenSpaceRenderables.size());
    std::transform(
        global::screenSpaceRenderables.begin(),
        global::screenSpaceRenderables.end(),
        res.begin(),
        [](const std::unique_ptr<ScreenSpaceRenderable>& p) { return p.get(); }
    );
    return res;
}

RenderEngine::RendererImplementation RenderEngine::rendererFromString(
                                                 const std::string& renderingMethod) const
{
    const std::map<std::string, RenderEngine::RendererImplementation> RenderingMethods = {
#ifdef OPENSPACE_WITH_ABUFFER_RENDERER
        { "ABuffer", RendererImplementation::ABuffer },
#endif // OPENSPACE_WITH_ABUFFER_RENDERER
        { "Framebuffer", RendererImplementation::Framebuffer }
    };

    if (RenderingMethods.find(renderingMethod) != RenderingMethods.end()) {
        return RenderingMethods.at(renderingMethod);
    }
    else {
        return RendererImplementation::Invalid;
    }
}

void RenderEngine::renderCameraInformation() {
    ZoneScoped

    if (!_showCameraInfo) {
        return;
    }

    const glm::vec4 EnabledColor  = glm::vec4(0.2f, 0.75f, 0.2f, 1.f);
    const glm::vec4 DisabledColor = glm::vec4(0.55f, 0.2f, 0.2f, 1.f);

    using FR = ghoul::fontrendering::FontRenderer;
    const FR::BoundingBoxInformation rotationBox = FR::defaultRenderer().boundingBox(
        *_fontInfo,
        "Rotation"
    );

    float penPosY = fontResolution().y - rotationBox.boundingBox.y;

    constexpr const float YSeparation = 5.f;
    constexpr const float XSeparation = 5.f;

    const interaction::OrbitalNavigator& nav =
        global::navigationHandler.orbitalNavigator();

    _cameraButtonLocations.rotation = {
        fontResolution().x - rotationBox.boundingBox.x - XSeparation,
        fontResolution().y - penPosY,
        rotationBox.boundingBox.x,
        rotationBox.boundingBox.y
    };
    FR::defaultRenderer().render(
        *_fontInfo,
        glm::vec2(fontResolution().x - rotationBox.boundingBox.x - XSeparation, penPosY),
        "Rotation",
        nav.hasRotationalFriction() ? EnabledColor : DisabledColor
    );
    penPosY -= rotationBox.boundingBox.y + YSeparation;

    FR::BoundingBoxInformation zoomBox = FR::defaultRenderer().boundingBox(
        *_fontInfo,
        "Zoom"
    );

    _cameraButtonLocations.zoom = {
        fontResolution().x - zoomBox.boundingBox.x - XSeparation,
        fontResolution().y - penPosY,
        zoomBox.boundingBox.x,
        zoomBox.boundingBox.y
    };
    FR::defaultRenderer().render(
        *_fontInfo,
        glm::vec2(fontResolution().x - zoomBox.boundingBox.x - XSeparation, penPosY),
        "Zoom",
        nav.hasZoomFriction() ? EnabledColor : DisabledColor
    );
    penPosY -= zoomBox.boundingBox.y + YSeparation;

    FR::BoundingBoxInformation rollBox = FR::defaultRenderer().boundingBox(
        *_fontInfo,
        "Roll"
    );

    _cameraButtonLocations.roll = {
        fontResolution().x - rollBox.boundingBox.x - XSeparation,
        fontResolution().y - penPosY,
        rollBox.boundingBox.x,
        rollBox.boundingBox.y
    };
    FR::defaultRenderer().render(
        *_fontInfo,
        glm::vec2(fontResolution().x - rollBox.boundingBox.x - XSeparation, penPosY),
        "Roll",
        nav.hasRollFriction() ? EnabledColor : DisabledColor
    );
}

void RenderEngine::renderVersionInformation() {
    ZoneScoped

    if (!_showVersionInfo) {
        return;
    }

    std::string versionString = OPENSPACE_VERSION_STRING_FULL;

    if (global::versionChecker.hasLatestVersionInfo()) {
        VersionChecker::SemanticVersion latestVersion =
            global::versionChecker.latestVersion();

        VersionChecker::SemanticVersion currentVersion {
            OPENSPACE_VERSION_MAJOR,
            OPENSPACE_VERSION_MINOR,
            OPENSPACE_VERSION_PATCH
        };
        if (currentVersion < latestVersion) {
            versionString += fmt::format(
                " [Available: {}.{}.{}]",
                latestVersion.major, latestVersion.minor, latestVersion.patch
            );
        }
    }

    using FR = ghoul::fontrendering::FontRenderer;
    const FR::BoundingBoxInformation versionBox = FR::defaultRenderer().boundingBox(
        *_fontInfo,
        versionString
    );

    const FR::BoundingBoxInformation commitBox = FR::defaultRenderer().boundingBox(
        *_fontInfo,
        fmt::format("{}@{}", OPENSPACE_GIT_BRANCH, OPENSPACE_GIT_COMMIT)
    );

    FR::defaultRenderer().render(
        *_fontInfo,
        glm::vec2(
            fontResolution().x - versionBox.boundingBox.x - 10.f,
            5.f
        ),
        versionString,
        glm::vec4(0.5, 0.5, 0.5, 1.f)
    );

    // If a developer hasn't placed the Git command in the path, this variable will be
    // empty
    if (!std::string(OPENSPACE_GIT_COMMIT).empty()) {
        // We check OPENSPACE_GIT_COMMIT but puse OPENSPACE_GIT_FULL on purpose since
        // OPENSPACE_GIT_FULL will never be empty (always will contain at least @, but
        // checking for that is a bit brittle)
        FR::defaultRenderer().render(
            *_fontInfo,
            glm::vec2(
                fontResolution().x - commitBox.boundingBox.x - 10.f,
                versionBox.boundingBox.y + 5.f
            ),
            OPENSPACE_GIT_FULL,
            glm::vec4(0.5, 0.5, 0.5, 1.f)
        );
    }
}

void RenderEngine::renderScreenLog() {
    ZoneScoped

    if (!_showLog) {
        return;
    }

    _log->removeExpiredEntries();

    constexpr const int MaxNumberMessages = 10;
    constexpr const int CategoryLength = 30;
    constexpr const int MessageLength = 140;
    constexpr const std::chrono::seconds FadeTime(5);

    const std::vector<ScreenLog::LogEntry>& entries = _log->entries();
    auto lastEntries =
        entries.size() > MaxNumberMessages ?
        std::make_pair(entries.rbegin(), entries.rbegin() + MaxNumberMessages) :
        std::make_pair(entries.rbegin(), entries.rend());

    size_t nr = 1;
    const auto now = std::chrono::steady_clock::now();
    for (auto& it = lastEntries.first; it != lastEntries.second; ++it) {
        const ScreenLog::LogEntry* e = &(*it);

        std::chrono::duration<double> diff = now - e->timeStamp;

        float alpha = 1.f;
        std::chrono::duration<double> ttf = ScreenLogTimeToLive - FadeTime;
        if (diff > ttf) {
            double d = (diff - ttf).count();
            float t = static_cast<float>(d) / static_cast<float>(FadeTime.count());
            float p = 0.8f - t;
            alpha = (p <= 0.f) ? 0.f : pow(p, 0.4f);
        }

        // Since all log entries are ordered, once one exceeds alpha, all have
        if (alpha <= 0.f) {
            break;
        }

        const std::string lvl = "(" + ghoul::to_string(e->level) + ")";
        const std::string& message = e->message.substr(0, MessageLength);
        nr += std::count(message.begin(), message.end(), '\n');

        const glm::vec4 white(0.9f, 0.9f, 0.9f, alpha);

        RenderFont(
            *_fontLog,
            glm::vec2(10.f, _fontLog->pointSize() * nr * 2),
            fmt::format(
                "{:<15} {}{}",
                e->timeString,
                e->category.substr(0, CategoryLength),
                e->category.length() > CategoryLength ? "..." : ""
            ),
            white
        );

        glm::vec4 color = glm::vec4(0.f);
        switch (e->level) {
            case ghoul::logging::LogLevel::Debug:
                color = glm::vec4(0.f, 1.f, 0.f, alpha);
                break;
            case ghoul::logging::LogLevel::Warning:
                color = glm::vec4(1.f, 1.f, 0.f, alpha);
                break;
            case ghoul::logging::LogLevel::Error:
                color = glm::vec4(1.f, 0.f, 0.f, alpha);
                break;
            case ghoul::logging::LogLevel::Fatal:
                color = glm::vec4(0.3f, 0.3f, 0.85f, alpha);
                break;
            default:
                color = white;
                break;
        }

        RenderFont(
            *_fontLog,
            glm::vec2(10 + 30 * _fontLog->pointSize(), _fontLog->pointSize() * nr * 2),
            lvl,
            color
        );

        RenderFont(
            *_fontLog,
            glm::vec2(10 + 41 * _fontLog->pointSize(), _fontLog->pointSize() * nr * 2),
            message,
            white
        );
        ++nr;
    }
}

} // namespace openspace
