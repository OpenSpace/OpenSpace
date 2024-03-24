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

#include <openspace/engine/openspaceengine.h>

#include <openspace/openspace.h>
#include <openspace/camera/camera.h>
#include <openspace/documentation/core_registration.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/logfactory.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/interactionmonitor.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/json.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/navigation/waypoint.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/loadingscreen.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scene/scenelicensewriter.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/screenlog.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/transformationmanager.h>
#include <ghoul/ghoul.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stacktrace.h>
#include <ghoul/misc/stringconversion.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/debugcontext.h>
#include <ghoul/opengl/shaderpreprocessor.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>
#include <glbinding/glbinding.h>
#include <glbinding-aux/types_to_string.h>
#include <filesystem>
#include <future>
#include <numeric>
#include <sstream>

#ifdef WIN32
#include <Windows.h>
#endif // WIN32

#ifdef __APPLE__
#include <openspace/interaction/touchbar.h>
#endif // __APPLE__

#include "openspaceengine_lua.inl"

namespace {
    // Helper structs for the visitor pattern of the std::variant
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

    constexpr std::string_view _loggerCat = "OpenSpaceEngine";

    constexpr std::string_view stringify(openspace::OpenSpaceEngine::Mode m) {
        using Mode = openspace::OpenSpaceEngine::Mode;
        switch (m) {
            case Mode::UserControl: return "UserControl";
            case Mode::CameraPath: return "CameraPath";
            case Mode::SessionRecordingPlayback: return "SessionRecording";
        }
        throw ghoul::MissingCaseException();
    }

    constexpr openspace::properties::Property::PropertyInfo PrintEventsInfo = {
        "PrintEvents",
        "Print Events",
        "If this is enabled, all events that are propagated through the system are "
        "printed to the log",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo VisibilityInfo = {
        "PropertyVisibility",
        "Property Visibility",
        "Hides or displays different settings in the GUI depending on how advanced they "
        "are",
        openspace::properties::Property::Visibility::Always
    };

    constexpr openspace::properties::Property::PropertyInfo FadeDurationInfo = {
        "FadeDuration",
        "Fade Duration (seconds)",
        "Controls how long time the fading in/out takes when enabling/disabling an "
        "object through a checkbox in the UI. Holding SHIFT while clicking the "
        "checkbox will enable/disable the renderable without fading, as will setting "
        "this value to zero.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableMouseInputInfo = {
        "DisableMouseInputs",
        "Disable All Mouse Inputs",
        "Disables all mouse inputs. Useful when using touch interaction, to prevent "
        "double inputs on touch (from both touch input and inserted mouse inputs)",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    void viewportChanged() {
        using namespace openspace;

        // Needs to be updated since each render call potentially targets a different
        // window and/or viewport
        using FR = ghoul::fontrendering::FontRenderer;
        FR::defaultRenderer().setFramebufferSize(global::renderEngine->fontResolution());

        FR::defaultProjectionRenderer().setFramebufferSize(
            global::renderEngine->renderingResolution()
        );
    }

    void resetPropertyChangeFlagsOfSubowners(openspace::properties::PropertyOwner* po) {
        using namespace openspace;

        for (properties::PropertyOwner* subOwner : po->propertySubOwners()) {
            resetPropertyChangeFlagsOfSubowners(subOwner);
        }
        for (properties::Property* p : po->properties()) {
            p->resetToUnchanged();
        }
    }

    void resetPropertyChangeFlags() {
        using namespace openspace;

        ZoneScoped;

        Scene* scene = global::renderEngine->scene();
        if (!scene) {
            return;
        }

        const std::vector<SceneGraphNode*> nodes = scene->allSceneGraphNodes();
        for (SceneGraphNode* n : nodes) {
            resetPropertyChangeFlagsOfSubowners(n);
        }
    }

} // namespace

namespace openspace {

class Scene;

OpenSpaceEngine::OpenSpaceEngine()
    : properties::PropertyOwner({ "OpenSpaceEngine", "OpenSpace Engine" })
    , _printEvents(PrintEventsInfo, false)
    , _visibility(VisibilityInfo)
    , _fadeOnEnableDuration(FadeDurationInfo, 1.f, 0.f, 5.f)
    , _disableAllMouseInputs(DisableMouseInputInfo, false)
{
    FactoryManager::initialize();
    SpiceManager::initialize();
    TransformationManager::initialize();

    addProperty(_printEvents);

    using Visibility = openspace::properties::Property::Visibility;
    _visibility.addOptions({
        { static_cast<int>(Visibility::NoviceUser), "Novice User" },
        { static_cast<int>(Visibility::User), "User" },
        { static_cast<int>(Visibility::AdvancedUser), "Advanced User" },
        { static_cast<int>(Visibility::Developer), "Developer" },
        { static_cast<int>(Visibility::Hidden), "Everything" },
    });
    addProperty(_visibility);

    addProperty(_fadeOnEnableDuration);
    addProperty(_disableAllMouseInputs);
}

OpenSpaceEngine::~OpenSpaceEngine() {}

void OpenSpaceEngine::registerPathTokens() {
    LTRACE("OpenSpaceEngine::initialize(begin)");

    // Registering Path tokens. If the BASE path is set, it is the only one that will
    // overwrite the default path of the cfg directory
    using T = std::string;
    for (const std::pair<const T, T>& path : global::configuration->pathTokens) {
        std::string fullKey = "${" + path.first + "}";
        LDEBUG(std::format("Registering path '{}': {}", fullKey, path.second));

        const bool overrideBase = (fullKey == "${BASE}");
        if (overrideBase) {
            LINFO(std::format("Overriding base path with '{}'", path.second));
        }

        const bool overrideTemporary = (fullKey == "${TEMPORARY}");

        using Override = ghoul::filesystem::FileSystem::Override;
        FileSys.registerPathToken(
            std::move(fullKey),
            path.second,
            Override(overrideBase || overrideTemporary)
        );
    }
    LTRACE("OpenSpaceEngine::initialize(end)");
}

void OpenSpaceEngine::initialize() {
    ZoneScoped;

    LTRACE("OpenSpaceEngine::initialize(begin)");

    global::initialize();
    // Initialize the general capabilities component
    SysCap.addComponent(
        std::make_unique<ghoul::systemcapabilities::GeneralCapabilitiesComponent>()
    );

    _printEvents = global::configuration->isPrintingEvents;
    _visibility = static_cast<int>(global::configuration->propertyVisibility);

    std::string cacheFolder = absPath("${CACHE}").string();
    if (global::configuration->usePerProfileCache) {
        cacheFolder = cacheFolder + "-" + global::configuration->profile;

        LINFO(std::format("Old cache: {}", absPath("${CACHE}")));
        LINFO(std::format("New cache: {}", cacheFolder));
        FileSys.registerPathToken(
            "${CACHE}",
            cacheFolder,
            ghoul::filesystem::FileSystem::Override::Yes
        );
    }

    // Create directories that doesn't exist
    for (const std::string& token : FileSys.tokens()) {
        if (!std::filesystem::is_directory(absPath(token))) {
            std::filesystem::create_directories(absPath(token));
        }
    }

    try {
        FileSys.createCacheManager(cacheFolder);
    }
    catch (const ghoul::RuntimeError& e) {
        LFATAL("Could not create Cache Manager");
        LFATALC(e.component, e.message);
    }



    // Initialize the requested logs from the configuration file
    // We previously initialized the LogManager with a console log to provide some logging
    // until we know which logs should be added
    if (ghoul::logging::LogManager::isInitialized()) {
        ghoul::logging::LogManager::deinitialize();
    }

    const ghoul::logging::LogLevel level = ghoul::from_string<ghoul::logging::LogLevel>(
        global::configuration->logging.level
    );
    const bool immediateFlush = global::configuration->logging.forceImmediateFlush;

    using ImmediateFlush = ghoul::logging::LogManager::ImmediateFlush;
    ghoul::logging::LogManager::initialize(level, ImmediateFlush(immediateFlush));

    for (const ghoul::Dictionary& log : global::configuration->logging.logs) {
        try {
            LogMgr.addLog(createLog(log));
        }
        catch (const documentation::SpecificationError& e) {
            LERROR("Failed loading of log");
            logError(e);
            throw;
        }
    }

#ifdef WIN32
    if (IsDebuggerPresent()) {
        LogMgr.addLog(std::make_unique<ghoul::logging::VisualStudioOutputLog>());
    }
#endif // WIN32

#ifndef GHOUL_LOGGING_ENABLE_TRACE
    if (level == ghoul::logging::LogLevel::Trace) {
        LWARNING(
            "Desired logging level is set to 'Trace' but application was "
            "compiled without Trace support"
        );
    }
#endif // GHOUL_LOGGING_ENABLE_TRACE

    if (!global::configuration->scriptLog.empty() &&
        global::configuration->scriptLogRotation > 0)
    {
        int rot = global::configuration->scriptLogRotation;
        while (rot > 0) {
            // Move all of the existing logs one position up

            const std::filesystem::path file = absPath(global::configuration->scriptLog);
            const std::string fname = file.stem().string();
            const std::string ext = file.extension().string();

            std::filesystem::path newCandidate = file;
            newCandidate.replace_filename(std::format("{}-{}{}", fname, rot, ext));

            std::filesystem::path oldCandidate = file;
            if (rot > 1) {
                // We don't actually have a -0 version, it is just the base name
                oldCandidate.replace_filename(
                    std::format("{}-{}{}", fname, rot - 1, ext)
                );
            }

            if (std::filesystem::exists(newCandidate)) {
                std::filesystem::remove(newCandidate);
            }
            if (std::filesystem::exists(oldCandidate)) {
                std::filesystem::rename(oldCandidate, newCandidate);
            }

            rot--;
        }
    }


    LINFOC("OpenSpace Version", std::string(OPENSPACE_VERSION_STRING_FULL));
    LINFOC("Commit", std::string(OPENSPACE_GIT_FULL));

    // Register modules
    global::moduleEngine->initialize(global::configuration->moduleConfigurations);

    // After registering the modules, the documentations for the available classes
    // can be added as well
    for (OpenSpaceModule* m : global::moduleEngine->modules()) {
        for (const documentation::Documentation& doc : m->documentations()) {
            DocEng.addDocumentation(doc);
        }
    }
    DocEng.addDocumentation(Configuration::Documentation);

    // Register the provided shader directories
    ghoul::opengl::ShaderPreprocessor::addIncludePath(absPath("${SHADERS}"));

    // Register Lua script functions
    LDEBUG("Registering Lua libraries");
    registerCoreClasses(*global::scriptEngine);

    // Process profile file
    std::filesystem::path profile;
    if (!std::filesystem::is_regular_file(global::configuration->profile)) {
        const std::filesystem::path userCandidate = absPath(std::format(
            "${{USER_PROFILES}}/{}.profile", global::configuration->profile
        ));
        const std::filesystem::path profileCandidate = absPath(std::format(
            "${{PROFILES}}/{}.profile", global::configuration->profile
        ));

        // Give the user profile priority if there are both
        if (std::filesystem::is_regular_file(userCandidate)) {
            profile = userCandidate;
        }
        else if (std::filesystem::is_regular_file(profileCandidate)) {
            profile = profileCandidate;
        }
        else {
            throw ghoul::RuntimeError(std::format(
                "Could not load profile '{}': File does not exist",
                global::configuration->profile
            ));
        }
    }
    else {
        profile = global::configuration->profile;
    }

    // Load the profile
    *global::profile = Profile(profile);

    // Set up asset loader
    _assetManager = std::make_unique<AssetManager>(
        global::scriptEngine->luaState(),
        absPath("${ASSETS}")
    );

    global::scriptEngine->addLibrary(_assetManager->luaLibrary());

    for (OpenSpaceModule* module : global::moduleEngine->modules()) {
        global::scriptEngine->addLibrary(module->luaLibrary());

        for (const scripting::LuaLibrary& l : module->luaLibraries()) {
            global::scriptEngine->addLibrary(l);
        }
    }

    global::scriptEngine->initialize();

    _shutdown.waitTime = global::configuration->shutdownCountdown;

    global::navigationHandler->initialize();

    global::renderEngine->initialize();

    for (const std::function<void()>& func : *global::callback::initialize) {
        ZoneScopedN("[Module] initialize");

        func();
    }

    LTRACE("OpenSpaceEngine::initialize(end)");
}

void OpenSpaceEngine::initializeGL() {
    ZoneScoped;

    LTRACE("OpenSpaceEngine::initializeGL(begin)");

    glbinding::Binding::initialize(global::windowDelegate->openGLProcedureAddress);
    //glbinding::Binding::useCurrentContext();

    LDEBUG("Adding OpenGL capabilities components");
    // Detect and log OpenCL and OpenGL versions and available devices
    SysCap.addComponent(
        std::make_unique<ghoul::systemcapabilities::OpenGLCapabilitiesComponent>()
    );

    LDEBUG("Detecting capabilities");
    SysCap.detectCapabilities();

    using Verbosity = ghoul::systemcapabilities::SystemCapabilitiesComponent::Verbosity;
    const Verbosity verbosity = ghoul::from_string<Verbosity>(
        global::configuration->logging.capabilitiesVerbosity
    );
    SysCap.logCapabilities(verbosity);

    const std::string versionCheckUrl = global::configuration->versionCheckUrl;
    if (!versionCheckUrl.empty()) {
        global::versionChecker->requestLatestVersion(versionCheckUrl);
    }

    // Check the required OpenGL versions of the registered modules
    const ghoul::systemcapabilities::Version version =
        global::moduleEngine->requiredOpenGLVersion();
    LINFO(std::format("Required OpenGL version: {}", ghoul::to_string(version)));

    if (OpenGLCap.openGLVersion() < version) {
        throw ghoul::RuntimeError(
            "An included module required a higher OpenGL version than is supported on "
            "this system",
            "OpenSpaceEngine"
        );
    }

    {
        // Check the available OpenGL extensions against the required extensions
        using OCC = ghoul::systemcapabilities::OpenGLCapabilitiesComponent;
        for (OpenSpaceModule* m : global::moduleEngine->modules()) {
            for (const std::string& ext : m->requiredOpenGLExtensions()) {
                if (!SysCap.component<OCC>().isExtensionSupported(ext)) {
                    LFATAL(std::format(
                        "Module '{}' required OpenGL extension '{}' which is not "
                        "available on this system. Some functionality related to this "
                        "module will probably not work",
                        m->guiName(), ext
                    ));
                }
            }
        }
    }

    rendering::helper::initialize();

    loadFonts();

    _loadingScreen = std::make_unique<LoadingScreen>(
        LoadingScreen::ShowMessage(
            global::configuration->loadingScreen.isShowingMessages
        ),
        LoadingScreen::ShowNodeNames(
            global::configuration->loadingScreen.isShowingNodeNames
        ),
        LoadingScreen::ShowLogMessages(
            global::configuration->loadingScreen.isShowingLogMessages
        )
    );

    _loadingScreen->render();

    LTRACE("OpenSpaceEngine::initializeGL::Console::initialize(begin)");
    try {
        global::luaConsole->initialize();
        global::luaConsole->setCommandInputButton(global::configuration->consoleKey);
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR("Error initializing Console with error:");
        LERRORC(e.component, e.message);
    }
    LTRACE("OpenSpaceEngine::initializeGL::Console::initialize(end)");

    LTRACE("OpenSpaceEngine::initializeGL::DebugContext(begin)");
    bool debugActive = global::configuration->openGLDebugContext.isActive;

    // Debug output is not available before 4.3
    const ghoul::systemcapabilities::Version minVersion = {
        .major = 4,
        .minor = 3,
        .release = 0
    };
    if (debugActive && OpenGLCap.openGLVersion() < minVersion) {
        LINFO("OpenGL Debug context requested, but insufficient version available");
        debugActive = false;
    }

    if (debugActive) {
        using namespace ghoul::opengl::debug;

        const bool synchronous = global::configuration->openGLDebugContext.isSynchronous;
        setDebugOutput(DebugOutput(debugActive), SynchronousOutput(synchronous));

        for (const Configuration::OpenGLDebugContext::IdentifierFilter& f :
            global::configuration->openGLDebugContext.identifierFilters)
        {
            setDebugMessageControl(
                ghoul::from_string<Source>(f.source),
                ghoul::from_string<Type>(f.type),
                { f.identifier },
                Enabled::No
            );

        }

        for (const std::string& sev :
            global::configuration->openGLDebugContext.severityFilters)
        {
            setDebugMessageControl(
                Source::DontCare,
                Type::DontCare,
                ghoul::from_string<Severity>(sev),
                Enabled::No
            );
        }

        ghoul::opengl::debug::setDebugCallback(
            [](Source source, Type type, Severity severity, unsigned int id,
                const std::string& message)
            {
                const std::string s = ghoul::to_string(source);
                const std::string t = ghoul::to_string(type);

                const std::string cat = std::format("OpenGL ({}) [{}] {{{}}}", s, t, id);
                switch (severity) {
                    case Severity::High:
                        LERRORC(cat, message);
                        break;
                    case Severity::Medium:
                        LWARNINGC(cat, message);
                        break;
                    case Severity::Low:
                        LINFOC(cat, message);
                        break;
                    case Severity::Notification:
                        LDEBUGC(cat, message);
                        break;
                    default:
                        throw ghoul::MissingCaseException();
                }

                if (global::configuration->openGLDebugContext.printStacktrace) {
                    std::string stackString = "Stacktrace\n";
                    std::vector<std::string> stack = ghoul::stackTrace();
                    for (size_t i = 0; i < stack.size(); i++) {
                        stackString += std::format("{}: {}\n", i, stack[i]);
                    }
                    LDEBUGC(cat, stackString);
                }
            }
        );
    }
    LTRACE("OpenSpaceEngine::initializeGL::DebugContext(end)");

    // The ordering of the KeyCheckOpenGLState and KeyLogEachOpenGLCall are important as
    // the callback mask in glbinding is stateful for each context, and since
    // KeyLogEachOpenGLCall is more specific, we want it to be able to overwrite the
    // state from KeyCheckOpenGLState
    if (global::configuration->isCheckingOpenGLState) {
        using namespace glbinding;

        // Infinite loop -- welcome to the danger zone
        setCallbackMaskExcept(CallbackMask::After, { "glGetError" });
        setAfterCallback([](const FunctionCall& f) {
            const GLenum error = glGetError();
            switch (error) {
                case GL_NO_ERROR:
                    break;
                case GL_INVALID_ENUM:
                    LERRORC(
                        "OpenGL Invalid State",
                        std::format("Function '{}': GL_INVALID_ENUM", f.function->name())
                    );
                    break;
                case GL_INVALID_VALUE:
                    LERRORC(
                        "OpenGL Invalid State",
                        std::format("Function '{}': GL_INVALID_VALUE", f.function->name())
                    );
                    break;
                case GL_INVALID_OPERATION:
                    LERRORC(
                        "OpenGL Invalid State",
                        std::format(
                            "Function '{}': GL_INVALID_OPERATION", f.function->name()
                        ));
                    break;
                case GL_INVALID_FRAMEBUFFER_OPERATION:
                    LERRORC(
                        "OpenGL Invalid State",
                        std::format(
                            "Function '{}': GL_INVALID_FRAMEBUFFER_OPERATION",
                            f.function->name()
                        )
                    );
                    break;
                case GL_OUT_OF_MEMORY:
                    LERRORC(
                        "OpenGL Invalid State",
                        std::format("Function '{}': GL_OUT_OF_MEMORY", f.function->name())
                    );
                    break;
                default:
                    LERRORC(
                        "OpenGL Invalid State",
                        std::format("Unknown error code: {0:x}", static_cast<int>(error))
                    );
            }
        });
    }

    if (global::configuration->isLoggingOpenGLCalls) {
        using namespace ghoul::logging;
        const LogLevel lvl = ghoul::from_string<LogLevel>(
            global::configuration->logging.level
        );
        if (lvl > LogLevel::Trace) {
            LWARNING(
                "Logging OpenGL calls is enabled, but the selected log level does "
                "not include TRACE, so no OpenGL logs will be printed"
            );
        }
        else {
            using namespace glbinding;

            setCallbackMask(CallbackMask::After | CallbackMask::ParametersAndReturnValue);
            glbinding::setAfterCallback([](const glbinding::FunctionCall& call) {
                std::string arguments = std::accumulate(
                    call.parameters.begin(),
                    call.parameters.end(),
                    std::string("("),
                    [](const std::string& a, const std::unique_ptr<AbstractValue>& v) {
                        std::stringstream s;
                        s << v.get();
                        return a + s.str() + ", ";
                    }
                );
                // Remove the final ", "
                arguments = arguments.substr(0, arguments.size() - 2) + ")";

                std::string returnValue;
                std::stringstream s;
                if (call.returnValue) {
                    s << call.returnValue.get();
                    returnValue = " -> " + s.str();
                }

                LTRACEC(
                    "OpenGL",
                    call.function->name() + std::move(arguments) + std::move(returnValue)
                );
            });
        }
    }

    LDEBUG("Initializing Rendering Engine");
    global::renderEngine->initializeGL();

    global::moduleEngine->initializeGL();


    for (const std::function<void()>& func : *global::callback::initializeGL) {
        ZoneScopedN("[Module] initializeGL");
        func();
    }

    LINFO("Finished initializing OpenGL");

    LTRACE("OpenSpaceEngine::initializeGL(end)");
}

void OpenSpaceEngine::loadAssets() {
    ZoneScoped;

    LTRACE("OpenSpaceEngine::loadAsset(begin)");

    global::windowDelegate->setBarrier(false);
    global::windowDelegate->setSynchronization(false);
    defer {
        global::windowDelegate->setSynchronization(true);
        global::windowDelegate->setBarrier(true);
    };

    std::unique_ptr<SceneInitializer> sceneInitializer;
    if (global::configuration->useMultithreadedInitialization) {
        const unsigned int nAvailableThreads = std::min(
            std::thread::hardware_concurrency() - 1,
            4u
        );
        const unsigned int nThreads = nAvailableThreads == 0 ? 2 : nAvailableThreads;
        sceneInitializer = std::make_unique<MultiThreadedSceneInitializer>(nThreads);
    }
    else {
        sceneInitializer = std::make_unique<SingleThreadedSceneInitializer>();
    }

    _scene = std::make_unique<Scene>(std::move(sceneInitializer));
    global::renderEngine->setScene(_scene.get());
    global::rootPropertyOwner->addPropertySubOwner(_scene.get());

    Camera* camera = _scene->camera();
    global::renderEngine->setCamera(camera);
    global::navigationHandler->setCamera(camera);
    const SceneGraphNode* parent = camera->parent();
    if (parent) {
        global::navigationHandler->orbitalNavigator().setFocusNode(parent->identifier());
    }
    else {
        global::navigationHandler->orbitalNavigator().setFocusNode(
            _scene->root()->identifier()
        );
    }

    for (const std::string& a : global::profile->assets) {
        _assetManager->add(a);
    }

    _loadingScreen->exec(*_assetManager, *_scene);
    _loadingScreen = nullptr;


    global::renderEngine->updateScene();

    global::syncEngine->addSyncables(global::timeManager->syncables());
    if (_scene && _scene->camera()) {
        global::syncEngine->addSyncables(_scene->camera()->syncables());
    }

#ifdef __APPLE__
    showTouchbar();
#endif // APPLE

    runGlobalCustomizationScripts();

    _writeDocumentationTask = std::async(&OpenSpaceEngine::writeDocumentation, this);

    LTRACE("OpenSpaceEngine::loadAsset(end)");
}

void OpenSpaceEngine::deinitialize() {
    ZoneScoped;

    LTRACE("OpenSpaceEngine::deinitialize(begin)");

    for (const std::function<void()>& func : *global::callback::deinitialize) {
        func();
    }

    global::navigationHandler->deinitialize();

    LTRACE("deinitialize(begin)");
    if (global::parallelPeer->status() != ParallelConnection::Status::Disconnected) {
        global::parallelPeer->disconnect();
    }
    if (global::renderEngine->scene() && global::renderEngine->scene()->camera()) {
        global::syncEngine->removeSyncables(
            global::renderEngine->scene()->camera()->syncables()
        );
    }
    global::sessionRecording->deinitialize();
    global::versionChecker->cancel();

    _assetManager = nullptr;

    global::deinitialize();

    FactoryManager::deinitialize();
    TransformationManager::deinitialize();
    SpiceManager::deinitialize();

    if (_printEvents) {
        events::Event* e = global::eventEngine->firstEvent();
        events::logAllEvents(e);
    }

    ghoul::fontrendering::FontRenderer::deinitialize();

    ghoul::logging::LogManager::deinitialize();

    LTRACE("deinitialize(end)");
    LTRACE("OpenSpaceEngine::deinitialize(end)");
}

void OpenSpaceEngine::deinitializeGL() {
    ZoneScoped;

    LTRACE("OpenSpaceEngine::deinitializeGL(begin)");

    viewportChanged();

    // We want to render an image informing the user that we are shutting down
    global::renderEngine->renderEndscreen();
    global::windowDelegate->swapBuffer();

    global::openSpaceEngine->assetManager().deinitialize();
    global::openSpaceEngine->_scene = nullptr;
    global::renderEngine->setScene(nullptr);

    for (const std::function<void()>& func : *global::callback::deinitializeGL) {
        func();
    }

    _loadingScreen = nullptr;

    global::deinitializeGL();

    rendering::helper::deinitialize();

    LTRACE("OpenSpaceEngine::deinitializeGL(end)");
}

void OpenSpaceEngine::createUserDirectoriesIfNecessary() {
    LTRACE(absPath("${USER}").string());

    if (!std::filesystem::exists(absPath("${USER_ASSETS}"))) {
        std::filesystem::create_directories(absPath("${USER_ASSETS}"));
    }
    if (!std::filesystem::exists(absPath("${USER_PROFILES}"))) {
        std::filesystem::create_directories(absPath("${USER_PROFILES}"));
    }
    if (!std::filesystem::exists(absPath("${USER_CONFIG}"))) {
        std::filesystem::create_directories(absPath("${USER_CONFIG}"));
    }
}

void OpenSpaceEngine::runGlobalCustomizationScripts() {
    ZoneScoped;

    LINFO("Running Global initialization scripts");
    const ghoul::lua::LuaState state;
    global::scriptEngine->initializeLuaState(state);

    for (const std::string& script : global::configuration->globalCustomizationScripts) {
        std::filesystem::path s = absPath(script);
        if (std::filesystem::is_regular_file(s)) {
            try {
                LINFO(std::format("Running global customization script: {}", s));
                ghoul::lua::runScriptFile(state, s.string());
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
        else {
            LDEBUG(std::format("Ignoring non-existing script file: {}", s));
        }
    }
}

void OpenSpaceEngine::loadFonts() {
    global::fontManager->initialize();

    using T = std::string;
    for (const std::pair<const T, T>& font : global::configuration->fonts) {
        std::string key = font.first;
        std::filesystem::path fontName = absPath(font.second);

        if (!std::filesystem::is_regular_file(fontName)) {
            LERROR(std::format("Could not find font '{}' for key '{}'", fontName, key));
            continue;
        }

        LDEBUG(std::format("Registering font '{}' with key '{}'", fontName, key));
        const bool success = global::fontManager->registerFontPath(key, fontName);

        if (!success) {
            LERROR(std::format(
                "Error registering font '{}' with key '{}'", fontName, key
            ));
        }
    }

    try {
        ghoul::fontrendering::FontRenderer::initialize();
    }
    catch (const ghoul::RuntimeError& err) {
        LERRORC(err.component, err.message);
    }
}

void OpenSpaceEngine::writeDocumentation() {
    ZoneScoped;

    // Write documentation to json files if config file supplies path for doc files
    std::string path = global::configuration->documentation.path;
    if (path.empty()) {
        // if path was empty, that means that no documentation is requested
        return;
    }
    path = absPath(path).string() + '/';

    // Start the async requests as soon as possible so they are finished when we need them
    std::future<nlohmann::json> settings = std::async(
        &properties::PropertyOwner::generateJson,
        global::rootPropertyOwner
    );

    std::future<nlohmann::json> scene = std::async(
        &properties::PropertyOwner::generateJson,
        _scene.get()
    );
    SceneLicenseWriter writer;

    nlohmann::json scripting = global::scriptEngine->generateJson();
    nlohmann::json factory = FactoryManager::ref().generateJson();
    nlohmann::json keybindings = global::keybindingManager->generateJson();
    nlohmann::json license = writer.generateJsonGroupedByLicense();
    nlohmann::json sceneProperties = settings.get();
    nlohmann::json sceneGraph = scene.get();

    sceneProperties["name"] = "Settings";
    sceneGraph["name"] = "Scene";

    // Add this here so that the generateJson function is the same as before to ensure
    // backwards compatibility
    nlohmann::json scriptingResult;
    scriptingResult["name"] = "Scripting API";
    scriptingResult["data"] = scripting;

    nlohmann::json documentation = {
        sceneGraph, sceneProperties, keybindings, license, scriptingResult, factory
    };

    nlohmann::json result;
    result["documentation"] = documentation;

    std::ofstream out(absPath("${DOCUMENTATION}/documentationData.js"));
    out << "var data = " << result.dump();
    out.close();
}

void OpenSpaceEngine::preSynchronization() {
    ZoneScoped;
    TracyGpuZone("preSynchronization");

    LTRACE("OpenSpaceEngine::preSynchronization(begin)");

    FileSys.triggerFilesystemEvents();

    // Reset the temporary, frame-based storage
    global::memoryManager->TemporaryMemory.reset();

    if (_isRenderingFirstFrame) {
        global::profile->ignoreUpdates = true;
        loadAssets();
        global::renderEngine->scene()->setPropertiesFromProfile(*global::profile);
        global::timeManager->setTimeFromProfile(*global::profile);
        global::timeManager->setDeltaTimeSteps(global::profile->deltaTimes);
        setActionsFromProfile(*global::profile);
        setKeybindingsFromProfile(*global::profile);
        setModulesFromProfile(*global::profile);
        setMarkInterestingNodesFromProfile(*global::profile);
        global::profile->ignoreUpdates = false;
        resetPropertyChangeFlagsOfSubowners(global::rootPropertyOwner);
        global::windowDelegate->setSynchronization(false);
    }

    const bool master = global::windowDelegate->isMaster();

    global::syncEngine->preSynchronization(SyncEngine::IsMaster(master));
    if (master) {
        const double dt =
            global::sessionRecording->isSavingFramesDuringPlayback() ?
            global::sessionRecording->fixedDeltaTimeDuringFrameOutput() :
            global::windowDelegate->deltaTime();

        global::timeManager->preSynchronization(dt);

        const std::vector<std::string> schedScripts = global::scriptScheduler->progressTo(
            global::timeManager->time().j2000Seconds()
        );
        for (const std::string& script : schedScripts) {
            global::scriptEngine->queueScript(
                script,
                scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                scripting::ScriptEngine::ShouldSendToRemote::Yes
            );
        }

        global::renderEngine->updateScene();

        if (_scene) {
            Camera* camera = _scene->camera();
            if (camera) {
                global::navigationHandler->updateCamera(dt);
                camera->invalidateCache();
            }
        }
        global::sessionRecording->preSynchronization();
        global::parallelPeer->preSynchronization();
        global::interactionMonitor->updateActivityState();
    }

    for (const std::function<void()>& func : *global::callback::preSync) {
        ZoneScopedN("[Module] preSync");

        func();
    }

    if (_isRenderingFirstFrame) {
        setCameraFromProfile(*global::profile);
        setAdditionalScriptsFromProfile(*global::profile);
    }

    // Handle callback(s) for change in engine mode
    if (_modeLastFrame != _currentMode) {
        using K = CallbackHandle;
        using V = ModeChangeCallback;
        for (const std::pair<K, V>& it : _modeChangeCallbacks) {
            it.second();
        }
    }
    _modeLastFrame = _currentMode;

    LTRACE("OpenSpaceEngine::preSynchronization(end)");
}

void OpenSpaceEngine::postSynchronizationPreDraw() {
    ZoneScoped;
    TracyGpuZone("postSynchronizationPreDraw");
    LTRACE("OpenSpaceEngine::postSynchronizationPreDraw(begin)");

    const bool master = global::windowDelegate->isMaster();
    global::syncEngine->postSynchronization(SyncEngine::IsMaster(master));

    if (_shutdown.inShutdown) {
        if (_shutdown.timer <= 0.f) {
            global::eventEngine->publishEvent<events::EventApplicationShutdown>(
                events::EventApplicationShutdown::State::Finished
            );
            global::windowDelegate->terminate();
            return;
        }
        _shutdown.timer -= static_cast<float>(global::windowDelegate->averageDeltaTime());
    }

    _assetManager->update();

    global::renderEngine->updateScene();
    global::renderEngine->updateRenderer();
    global::renderEngine->updateScreenSpaceRenderables();
    global::renderEngine->updateShaderPrograms();
    global::luaConsole->update();

    if (!master) {
        _scene->camera()->invalidateCache();
    }

    for (const std::function<void()>& func : *global::callback::postSyncPreDraw) {
        ZoneScopedN("[Module] postSyncPreDraw");

        func();
    }

    // Testing this every frame has minimal impact on the performance --- abock
    // Debug build: 1-2 us ; Release build: <= 1 us
    using ghoul::logging::LogManager;
    int warningCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Warning);
    int errorCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Error);
    int fatalCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Fatal);

    if (warningCounter > 0) {
        LWARNINGC("Logging", std::format("Number of Warnings: {}", warningCounter));
    }
    if (errorCounter > 0) {
        LWARNINGC("Logging", std::format("Number of Errors: {}", errorCounter));
    }
    if (fatalCounter > 0) {
        LWARNINGC("Logging", std::format("Number of Fatals: {}", fatalCounter));
    }

    LogMgr.resetMessageCounters();

    LTRACE("OpenSpaceEngine::postSynchronizationPreDraw(end)");
}

void OpenSpaceEngine::render(const glm::mat4& sceneMatrix, const glm::mat4& viewMatrix,
                             const glm::mat4& projectionMatrix)
{
    ZoneScoped;
    TracyGpuZone("Render");
    LTRACE("OpenSpaceEngine::render(begin)");

    viewportChanged();

    global::renderEngine->render(sceneMatrix, viewMatrix, projectionMatrix);

    for (const std::function<void()>& func : *global::callback::render) {
        ZoneScopedN("[Module] render");

        func();
    }

    LTRACE("OpenSpaceEngine::render(end)");
}

void OpenSpaceEngine::drawOverlays() {
    ZoneScoped;
    TracyGpuZone("Draw2D");
    LTRACE("OpenSpaceEngine::drawOverlays(begin)");

    viewportChanged();

    const bool isGuiWindow =
        global::windowDelegate->hasGuiWindow() ?
        global::windowDelegate->isGuiWindow() :
        true;

    if (isGuiWindow) {
        global::renderEngine->renderOverlays(_shutdown);
        global::luaConsole->render();
        global::sessionRecording->render();
    }

    for (const std::function<void()>& func : *global::callback::draw2D) {
        ZoneScopedN("[Module] draw2D");
        func();
    }

    LTRACE("OpenSpaceEngine::drawOverlays(end)");
}

void OpenSpaceEngine::postDraw() {
    ZoneScoped;
    TracyGpuZone("postDraw");
    LTRACE("OpenSpaceEngine::postDraw(begin)");

    global::renderEngine->postDraw();

    for (const std::function<void()>& func : *global::callback::postDraw) {
        ZoneScopedN("[Module] postDraw");

        func();
    }

    if (_isRenderingFirstFrame) {
        global::windowDelegate->setSynchronization(true);
        resetPropertyChangeFlags();
        _isRenderingFirstFrame = false;
    }

    //
    // Handle events
    //
    const events::Event* e = global::eventEngine->firstEvent();
    if (_printEvents) {
        events::logAllEvents(e);
    }
    global::eventEngine->triggerActions();
    global::eventEngine->triggerTopics();


    global::eventEngine->postFrameCleanup();
    global::memoryManager->PersistentMemory.housekeeping();

    LTRACE("OpenSpaceEngine::postDraw(end)");
}

void OpenSpaceEngine::keyboardCallback(Key key, KeyModifier mod, KeyAction action,
                                       IsGuiWindow isGuiWindow)
{
    ZoneScoped;

    if (_loadingScreen) {
        // If the loading screen object exists, we are currently loading and want key
        // presses to behave differently
        if (key == Key::Escape) {
            _loadingScreen->abort();
        }

        return;
    }

    // We need to do this check before the callback functions as we would otherwise
    // immediately cancel a shutdown if someone pressed the ESC key. Similar argument for
    // only checking for the Press action.  Since the 'Press' of ESC will trigger the
    // shutdown, the 'Release' in some frame later would cancel it immediately again
    if (action == KeyAction::Press && _shutdown.inShutdown) {
        _shutdown.inShutdown = false;
        global::eventEngine->publishEvent<events::EventApplicationShutdown>(
            events::EventApplicationShutdown::State::Aborted
        );
        return;
    }

    for (const global::callback::KeyboardCallback& func : *global::callback::keyboard) {
        const bool isConsumed = func(key, mod, action, isGuiWindow);
        if (isConsumed) {
            return;
        }
    }

    if (!global::configuration->isConsoleDisabled) {
        const bool isConsumed = global::luaConsole->keyboardCallback(key, mod, action);
        if (isConsumed) {
            return;
        }
    }

    global::navigationHandler->keyboardCallback(key, mod, action);

    if (!global::navigationHandler->disabledKeybindings()) {
        global::keybindingManager->keyboardCallback(key, mod, action);
    }

    global::interactionMonitor->markInteraction();
}

void OpenSpaceEngine::charCallback(unsigned int codepoint, KeyModifier modifier,
                                   IsGuiWindow isGuiWindow)
{
    ZoneScoped;

    for (const global::callback::CharacterCallback& func : *global::callback::character) {
        const bool isConsumed = func(codepoint, modifier, isGuiWindow);
        if (isConsumed) {
            return;
        }
    }

    global::luaConsole->charCallback(codepoint, modifier);
    global::interactionMonitor->markInteraction();

    if (_shutdown.inShutdown) {
        _shutdown.inShutdown = false;
        global::eventEngine->publishEvent<events::EventApplicationShutdown>(
            events::EventApplicationShutdown::State::Aborted
        );
    }
}

void OpenSpaceEngine::mouseButtonCallback(MouseButton button, MouseAction action,
                                          KeyModifier mods, IsGuiWindow isGuiWindow)
{
    ZoneScoped;

    if (_disableAllMouseInputs) {
        return;
    }

    using F = global::callback::MouseButtonCallback;
    for (const F& func : *global::callback::mouseButton) {
        const bool isConsumed = func(button, action, mods, isGuiWindow);
        if (isConsumed) {
            // If the mouse was released, we still want to forward it to the navigation
            // handler in order to reliably terminate a rotation or zoom, or to the other
            // callbacks to for example release a drag and drop of a UI window.
            // Accidentally moving the cursor over a UI window is easy to miss and leads
            // to weird continuing movement
            if (action == MouseAction::Release) {
                continue;
            }
            else {
                return;
            }
        }
    }

    // Check if the user clicked on one of the 'buttons' the RenderEngine is drawing.
    // Only handle the clicks if we are in a GUI window
    if (action == MouseAction::Press && isGuiWindow) {
        const bool isConsumed =
            global::renderEngine->mouseActivationCallback(_mousePosition);
        if (isConsumed) {
            return;
        }
    }

    global::navigationHandler->mouseButtonCallback(button, action);
    global::interactionMonitor->markInteraction();

    if (_shutdown.inShutdown) {
        _shutdown.inShutdown = false;
        global::eventEngine->publishEvent<events::EventApplicationShutdown>(
            events::EventApplicationShutdown::State::Aborted
        );
    }
}

void OpenSpaceEngine::mousePositionCallback(double x, double y, IsGuiWindow isGuiWindow) {
    ZoneScoped;

    if (_disableAllMouseInputs) {
        return;
    }

    using F = global::callback::MousePositionCallback;
    for (const F& func : *global::callback::mousePosition) {
        func(x, y, isGuiWindow);
    }

    global::navigationHandler->mousePositionCallback(x, y);
    global::interactionMonitor->markInteraction();

    _mousePosition = glm::vec2(static_cast<float>(x), static_cast<float>(y));
}

void OpenSpaceEngine::mouseScrollWheelCallback(double posX, double posY,
                                               IsGuiWindow isGuiWindow)
{
    ZoneScoped;

    if (_disableAllMouseInputs) {
        return;
    }

    using F = global::callback::MouseScrollWheelCallback;
    for (const F& func : *global::callback::mouseScrollWheel) {
        const bool isConsumed = func(posX, posY, isGuiWindow);
        if (isConsumed) {
            return;
        }
    }

    global::navigationHandler->mouseScrollWheelCallback(posY);
    global::interactionMonitor->markInteraction();
}

void OpenSpaceEngine::touchDetectionCallback(TouchInput input) {
    ZoneScoped;

    for (const std::function<bool(TouchInput)>& func : *global::callback::touchDetected) {
        const bool isConsumed = func(input);
        if (isConsumed) {
            return;
        }
    }
}

void OpenSpaceEngine::touchUpdateCallback(TouchInput input) {
    ZoneScoped;

    for (const std::function<bool(TouchInput)>& func : *global::callback::touchUpdated) {
        const bool isConsumed = func(input);
        if (isConsumed) {
            return;
        }
    }
}

void OpenSpaceEngine::touchExitCallback(TouchInput input) {
    ZoneScoped;

    for (const std::function<void(TouchInput)>& func : *global::callback::touchExit) {
        func(input);
    }
}

void OpenSpaceEngine::handleDragDrop(std::filesystem::path file) {
    const ghoul::lua::LuaState s(ghoul::lua::LuaState::IncludeStandardLibrary::Yes);
    const std::filesystem::path path = absPath("${SCRIPTS}/drag_drop_handler.lua");
    int status = luaL_loadfile(s, path.string().c_str());
    if (status != LUA_OK) {
        const std::string error = lua_tostring(s, -1);
        LERROR(error);
        return;
    }

    ghoul::lua::push(s, file);
    lua_setglobal(s, "filename");

    std::string basename = file.filename().string();
    ghoul::lua::push(s, std::move(basename));
    lua_setglobal(s, "basename");

    std::string extension = file.extension().string();
    extension = ghoul::toLowerCase(extension);

    ghoul::lua::push(s, extension);
    lua_setglobal(s, "extension");

    status = lua_pcall(s, 0, 1, 0);
    if (status != LUA_OK) {
        const std::string error = lua_tostring(s, -1);
        LERROR(error);
        return;
    }

    if (lua_isnil(s, -1)) {
        LWARNING(std::format("Unhandled file dropped: {}", file));
        return;
    }

    std::string script = ghoul::lua::value<std::string>(s);
    global::scriptEngine->queueScript(
        std::move(script),
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

std::vector<std::byte> OpenSpaceEngine::encode() {
    ZoneScoped;

    return global::syncEngine->encodeSyncables();
}

void OpenSpaceEngine::decode(std::vector<std::byte> data) {
    ZoneScoped;

    global::syncEngine->decodeSyncables(std::move(data));
}

properties::Property::Visibility OpenSpaceEngine::visibility() const {
    return static_cast<properties::Property::Visibility>(_visibility.value());
}

void OpenSpaceEngine::toggleShutdownMode() {
    if (_shutdown.inShutdown) {
        // If we are already in shutdown mode, we want to disable it
        _shutdown.inShutdown = false;
        global::eventEngine->publishEvent<events::EventApplicationShutdown>(
            events::EventApplicationShutdown::State::Aborted
        );
    }
    else {
        // Else, we have to enable it
        _shutdown.timer = _shutdown.waitTime;
        _shutdown.inShutdown = true;
        global::eventEngine->publishEvent<events::EventApplicationShutdown>(
            events::EventApplicationShutdown::State::Started
        );
    }
}

OpenSpaceEngine::Mode OpenSpaceEngine::currentMode() const {
    return _currentMode;
}

bool OpenSpaceEngine::setMode(Mode newMode) {
    if (_currentMode == Mode::CameraPath && newMode == Mode::CameraPath) {
        // Special case: It is okay to trigger another camera path while one is
        // already playing. So just return that we were successful
        return true;
    }
    else if (newMode == _currentMode) {
        LERROR("Cannot switch to the currectly active mode");
        return false;
    }
    else if (_currentMode != Mode::UserControl && newMode != Mode::UserControl) {
        LERROR(std::format(
            "Cannot switch to mode '{}' when in '{}' mode",
            stringify(newMode), stringify(_currentMode)
        ));
        return false;
    }

    LDEBUG(std::format("Mode: {}", stringify(newMode)));

    _currentMode = newMode;
    return true;
}

void OpenSpaceEngine::resetMode() {
    _currentMode = Mode::UserControl;
    LDEBUG(std::format("Reset engine mode to '{}'", stringify(_currentMode)));
}

OpenSpaceEngine::CallbackHandle OpenSpaceEngine::addModeChangeCallback(
                                                                    ModeChangeCallback cb)
{
    const CallbackHandle handle = _nextCallbackHandle++;
    _modeChangeCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

void OpenSpaceEngine::removeModeChangeCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _modeChangeCallbacks.begin(),
        _modeChangeCallbacks.end(),
        [handle](const std::pair<CallbackHandle, ModeChangeCallback>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _modeChangeCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _modeChangeCallbacks.erase(it);
}

scripting::LuaLibrary OpenSpaceEngine::luaLibrary() {
    return {
        "",
        {
            codegen::lua::ToggleShutdown,
            codegen::lua::WriteDocumentation,
            codegen::lua::SetScreenshotFolder,
            codegen::lua::AddTag,
            codegen::lua::RemoveTag,
            codegen::lua::DownloadFile,
            codegen::lua::CreateSingleColorImage,
            codegen::lua::IsMaster,
            codegen::lua::Version,
            codegen::lua::ReadCSVFile,
            codegen::lua::ResetCamera,
            codegen::lua::Configuration,
            codegen::lua::LayerServer,
            codegen::lua::LoadJson
        },
        {
            absPath("${SCRIPTS}/core_scripts.lua")
        }
    };
}

LoadingScreen* OpenSpaceEngine::loadingScreen() {
    return _loadingScreen.get();
}

AssetManager& OpenSpaceEngine::assetManager() {
    ghoul_assert(_assetManager, "Asset Manager must not be nullptr");
    return *_assetManager;
}

void setCameraFromProfile(const Profile& p) {
    if (!p.camera.has_value()) {
        // If the camera is not specified, we want to set it to a sensible default value
        interaction::NavigationState nav;
        nav.anchor = "Root";
        nav.referenceFrame = "Root";
        global::navigationHandler->setNavigationStateNextFrame(nav);
        return;
    }

    auto checkNodeExists = [](const std::string& node) {
        if (global::renderEngine->scene()->sceneGraphNode(node) == nullptr) {
            throw ghoul::RuntimeError(std::format(
                "Error when setting camera from profile. Could not find node '{}'", node
            ));
        }
    };

    std::visit(
        overloaded {
            [&checkNodeExists](const Profile::CameraNavState& navStateProfile) {
                interaction::NavigationState nav;
                nav.anchor = navStateProfile.anchor;
                checkNodeExists(nav.anchor);
                if (navStateProfile.aim.has_value() && !(*navStateProfile.aim).empty()) {
                    nav.aim = navStateProfile.aim.value();
                    checkNodeExists(nav.aim);
                }
                if (navStateProfile.referenceFrame.empty()) {
                    nav.referenceFrame = nav.anchor;
                }
                else {
                    nav.referenceFrame = navStateProfile.referenceFrame;
                    checkNodeExists(navStateProfile.referenceFrame);
                }
                nav.position = navStateProfile.position;
                if (navStateProfile.up.has_value()) {
                    nav.up = navStateProfile.up;
                }
                if (navStateProfile.yaw.has_value()) {
                    nav.yaw = navStateProfile.yaw.value();
                }
                if (navStateProfile.pitch.has_value()) {
                    nav.pitch = navStateProfile.pitch.value();
                }
                global::navigationHandler->setNavigationStateNextFrame(nav);
            },
            [&checkNodeExists](const Profile::CameraGoToGeo& geo) {
                // Instead of direct calls to navigation state code, Lua commands with
                // globebrowsing goToGeo are used because this prevents a module
                // dependency in this core code. Eventually, goToGeo will be incorporated
                // in the OpenSpace core and this code will change.
                checkNodeExists(geo.anchor);
                std::string geoScript = std::format("openspace.globebrowsing.goToGeo"
                    "([[{}]], {}, {}", geo.anchor, geo.latitude, geo.longitude);
                if (geo.altitude.has_value()) {
                    geoScript += std::format(", {}", geo.altitude.value());
                }
                geoScript += ")";
                global::scriptEngine->queueScript(
                    geoScript,
                    scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                    scripting::ScriptEngine::ShouldSendToRemote::Yes
                );
            },
            [&checkNodeExists](const Profile::CameraGoToNode& node) {
                using namespace interaction;

                checkNodeExists(node.anchor);

                NodeCameraStateSpec spc = {
                    .identifier = node.anchor,
                    .height = node.height,
                    .useTargetUpDirection = true
                };
                global::navigationHandler->setCameraFromNodeSpecNextFrame(std::move(spc));
            }
        },
        p.camera.value()
    );
}

void setModulesFromProfile(const Profile& p) {
    for (Profile::Module mod : p.modules) {
        const std::vector<OpenSpaceModule*>& m = global::moduleEngine->modules();
        const auto it = std::find_if(m.begin(), m.end(),
            [&mod](const OpenSpaceModule* moduleSearch) {
                return (moduleSearch->identifier() == mod.name);
            });
        if (it != m.end()) {
            if (mod.loadedInstruction.has_value()) {
                global::scriptEngine->queueScript(
                    mod.loadedInstruction.value(),
                    scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                    scripting::ScriptEngine::ShouldSendToRemote::Yes
                );
            }
        }
        else {
            if (mod.notLoadedInstruction.has_value()) {
                global::scriptEngine->queueScript(
                    mod.notLoadedInstruction.value(),
                    scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                    scripting::ScriptEngine::ShouldSendToRemote::Yes
                );
            }
        }
    }
}

void setActionsFromProfile(const Profile& p) {
    for (Profile::Action a : p.actions) {
        if (a.identifier.empty()) {
            LERROR("Identifier must to provided to register action");
        }
        if (global::actionManager->hasAction(a.identifier)) {
            LERROR(std::format(
                "Action for identifier '{}' already existed & registered", a.identifier
            ));
        }
        if (a.script.empty()) {
            LERROR(std::format(
                "Identifier '{}' does not provide a Lua command to execute", a.identifier
            ));
        }
        interaction::Action action;
        action.identifier = a.identifier;
        action.command = a.script;
        action.name = a.name;
        action.documentation = a.documentation;
        action.guiPath = a.guiPath;
        action.isLocal = interaction::Action::IsLocal(a.isLocal);
        global::actionManager->registerAction(std::move(action));
    }
}

void setKeybindingsFromProfile(const Profile& p) {
    for (Profile::Keybinding k : p.keybindings) {
        if (k.action.empty()) {
            LERROR("Action must not be empty");
        }
        if (!global::actionManager->hasAction(k.action)) {
            LERROR(std::format("Action '{}' does not exist", k.action));
        }
        if (k.key.key == openspace::Key::Unknown) {
            LERROR(std::format(
                "Could not find key '{}'",
                std::to_string(static_cast<uint16_t>(k.key.key))
            ));
        }
        global::keybindingManager->bindKey(k.key.key, k.key.modifier, k.action);
    }
}

void setMarkInterestingNodesFromProfile(const Profile& p) {
    for (const std::string& nodeName : p.markNodes) {
        SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(nodeName);
        if (node) {
            node->addTag("GUI.Interesting");
        }
    }
}

void setAdditionalScriptsFromProfile(const Profile& p) {
    for (const std::string& a : p.additionalScripts) {
        global::scriptEngine->queueScript(
            a,
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }
}

}  // namespace openspace
