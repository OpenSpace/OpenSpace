/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/logfactory.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/virtualpropertymanager.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/interactionmonitor.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/dashboarditem.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/loadingscreen.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/assetloader.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/scene/timeframe.h>
#include <openspace/scene/lightsource.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scene/translation.h>
#include <openspace/scene/scenelicensewriter.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/task.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/transformationmanager.h>
#include <ghoul/ghoul.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stacktrace.h>
#include <ghoul/misc/stringconversion.h>
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

#ifdef __APPLE__
#include <openspace/interaction/touchbar.h>
#endif // __APPLE__

#include "openspaceengine_lua.inl"

namespace {
    // Helper structs for the visitor pattern of the std::variant
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...)->overloaded<Ts...>;

    constexpr const char* _loggerCat = "OpenSpaceEngine";
} // namespace

namespace openspace {

class Scene;

OpenSpaceEngine::OpenSpaceEngine() {
    FactoryManager::initialize();
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Renderable>>(),
        "Renderable"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Translation>>(),
        "Translation"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Rotation>>(),
        "Rotation"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Scale>>(),
        "Scale"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<TimeFrame>>(),
        "TimeFrame"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<LightSource>>(),
        "LightSource"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Task>>(),
        "Task"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<ResourceSynchronization>>(),
        "ResourceSynchronization"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<DashboardItem>>(),
        "DashboardItem"
    );

    SpiceManager::initialize();
    TransformationManager::initialize();
}

OpenSpaceEngine::~OpenSpaceEngine() {} // NOLINT

void OpenSpaceEngine::registerPathTokens() {
    LTRACE("OpenSpaceEngine::initialize(begin)");

    // Registering Path tokens. If the BASE path is set, it is the only one that will
    // overwrite the default path of the cfg directory
    using T = std::string;
    for (const std::pair<const T, T>& path : global::configuration->pathTokens) {
        std::string fullKey = "${" + path.first + "}";
        LDEBUG(fmt::format("Registering path '{}': '{}'", fullKey, path.second));

        const bool overrideBase = (fullKey == "${BASE}");
        if (overrideBase) {
            LINFO(fmt::format("Overriding base path with '{}'", path.second));
        }

        const bool overrideTemporary = (fullKey == "${TEMPORARY}");

        using Override = ghoul::filesystem::FileSystem::Override;
        FileSys.registerPathToken(
            std::move(fullKey),
            std::move(path.second),
            Override(overrideBase || overrideTemporary)
        );
    }
    LTRACE("OpenSpaceEngine::initialize(end)");
}

void OpenSpaceEngine::initialize() {
    ZoneScoped

    LTRACE("OpenSpaceEngine::initialize(begin)");

    global::initialize();

    const std::string versionCheckUrl = global::configuration->versionCheckUrl;
    if (!versionCheckUrl.empty()) {
        global::versionChecker->requestLatestVersion(versionCheckUrl);
    }

    std::string cacheFolder = absPath("${CACHE}").string();
    if (global::configuration->usePerProfileCache) {
        std::string profile = global::configuration->profile;
        if (profile.empty()) {
            throw ghoul::RuntimeError(
                "Unexpected error: Configuration file profile was empty"
            );
        }
        cacheFolder = cacheFolder + "-" + profile;

        LINFO(fmt::format("Old cache: {}", absPath("${CACHE}")));
        LINFO(fmt::format("New cache: {}", cacheFolder));
        FileSys.registerPathToken(
            "${CACHE}",
            cacheFolder,
            ghoul::filesystem::FileSystem::Override::Yes
        );
    }

    // Create directories that doesn't exist
    for (const std::string& token : FileSys.tokens()) {
        if (!std::filesystem::is_directory(token)) {
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

    ghoul::logging::LogLevel level = ghoul::from_string<ghoul::logging::LogLevel>(
        global::configuration->logging.level
    );
    bool immediateFlush = global::configuration->logging.forceImmediateFlush;

    using ImmediateFlush = ghoul::logging::LogManager::ImmediateFlush;
    ghoul::logging::LogManager::initialize(level, ImmediateFlush(immediateFlush));

    for (const ghoul::Dictionary& log : global::configuration->logging.logs) {
        try {
            LogMgr.addLog(createLog(log));
        }
        catch (const documentation::SpecificationError& e) {
            LERROR("Failed loading of log");
            for (const documentation::TestResult::Offense& o : e.result.offenses) {
                LERRORC(o.offender, ghoul::to_string(o.reason));
            }
            for (const documentation::TestResult::Warning& w : e.result.warnings) {
                LWARNINGC(w.offender, ghoul::to_string(w.reason));
            }
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
    DocEng.addDocumentation(configuration::Configuration::Documentation);

    // Register the provided shader directories
    ghoul::opengl::ShaderPreprocessor::addIncludePath(absPath("${SHADERS}"));

    // Register Lua script functions
    LDEBUG("Registering Lua libraries");
    registerCoreClasses(*global::scriptEngine);

    // Process profile file (must be provided in configuration file)
    if (!global::configuration->profile.empty()) {
        std::string inputProfilePath = absPath("${PROFILES}").string();
        std::string outputScenePath = absPath("${TEMPORARY}").string();
        std::string inputProfile = inputProfilePath + "/" + global::configuration->profile
            + ".profile";
        std::string inputUserProfile = absPath("${USER_PROFILES}").string() + "/" +
            global::configuration->profile + ".profile";

        if (std::filesystem::is_regular_file(inputUserProfile)) {
            inputProfile = inputUserProfile;
        }

        if (!std::filesystem::is_regular_file(inputProfile)) {
            LERROR(fmt::format(
                "Could not load profile '{}': File does not exist", inputProfile)
            );
        }
        else {
            // Load the profile
            std::ifstream inFile;
            try {
                inFile.open(inputProfile, std::ifstream::in);
            }
            catch (const std::ifstream::failure& e) {
                throw ghoul::RuntimeError(fmt::format(
                    "Exception opening profile file for read: {} ({})",
                    inputProfile, e.what())
                );
            }

            std::string content(
                (std::istreambuf_iterator<char>(inFile)),
                std::istreambuf_iterator<char>()
            );
            *global::profile = Profile(content);
        }
    }

    // Set up asset loader
    global::openSpaceEngine->_assetManager = std::make_unique<AssetManager>(
        global::scriptEngine->luaState(),
        absPath("${ASSETS}").string()
    );

    global::scriptEngine->addLibrary(
        global::openSpaceEngine->_assetManager->luaLibrary()
    );

    for (OpenSpaceModule* module : global::moduleEngine->modules()) {
        global::scriptEngine->addLibrary(module->luaLibrary());

        for (scripting::LuaLibrary& l : module->luaLibraries()) {
            global::scriptEngine->addLibrary(l);
        }
    }

    global::scriptEngine->initialize();

    // To be concluded
    _documentationJson.clear();
    _documentationJson += "{\"documentation\":[";

    writeStaticDocumentation();

    _shutdown.waitTime = global::configuration->shutdownCountdown;

    global::navigationHandler->initialize();

    global::renderEngine->initialize();

    for (const std::function<void()>& func : *global::callback::initialize) {
        ZoneScopedN("[Module] initialize")

        func();
    }

    global::openSpaceEngine->_assetManager->initialize();

    LTRACE("OpenSpaceEngine::initialize(end)");
}

std::string OpenSpaceEngine::generateFilePath(std::string openspaceRelativePath) {
    // @TODO (abock, 2021-05-16) This whole function can die, I think
    std::string path = absPath(openspaceRelativePath).string();
    // Needs to handle either windows (which seems to require double back-slashes)
    // or unix path slashes.
    const std::string search = "\\";
    const std::string replace = "\\\\";
    if (path.find(search) != std::string::npos) {
        size_t start_pos = 0;
        while ((start_pos = path.find(search, start_pos)) != std::string::npos) {
            path.replace(start_pos, search.length(), replace);
            start_pos += replace.length();
        }
        path.append(replace);
    }
    else {
        path.append("/");
    }
    return path.append(global::configuration->profile);
}

void OpenSpaceEngine::initializeGL() {
    ZoneScoped

    LTRACE("OpenSpaceEngine::initializeGL(begin)");

    glbinding::Binding::initialize(global::windowDelegate->openGLProcedureAddress);
    //glbinding::Binding::useCurrentContext();

    LDEBUG("Adding system components");
    // Detect and log OpenCL and OpenGL versions and available devices
    SysCap.addComponent(
        std::make_unique<ghoul::systemcapabilities::GeneralCapabilitiesComponent>()
    );
    SysCap.addComponent(
        std::make_unique<ghoul::systemcapabilities::OpenGLCapabilitiesComponent>()
    );

    LDEBUG("Detecting capabilities");
    SysCap.detectCapabilities();

    using Verbosity = ghoul::systemcapabilities::SystemCapabilitiesComponent::Verbosity;
    Verbosity verbosity = ghoul::from_string<Verbosity>(
        global::configuration->logging.capabilitiesVerbosity
    );
    SysCap.logCapabilities(verbosity);


    // Check the required OpenGL versions of the registered modules
    ghoul::systemcapabilities::Version version =
        global::moduleEngine->requiredOpenGLVersion();
    LINFO(fmt::format("Required OpenGL version: {}", ghoul::to_string(version)));

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
                    LFATAL(fmt::format(
                        "Module {} required OpenGL extension {} which is not available "
                        "on this system. Some functionality related to this module will "
                        "probably not work.", m->guiName(), ext
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
        LoadingScreen::ShowProgressbar(
            global::configuration->loadingScreen.isShowingProgressbar
        )
        );

    _loadingScreen->render();





    LTRACE("OpenSpaceEngine::initializeGL::Console::initialize(begin)");
    try {
        global::luaConsole->initialize();
    }
    catch (ghoul::RuntimeError& e) {
        LERROR("Error initializing Console with error:");
        LERRORC(e.component, e.message);
    }
    LTRACE("OpenSpaceEngine::initializeGL::Console::initialize(end)");

    LTRACE("OpenSpaceEngine::initializeGL::DebugContext(begin)");
    bool debugActive = global::configuration->openGLDebugContext.isActive;

    // Debug output is not available before 4.3
    const ghoul::systemcapabilities::Version minVersion = { 4, 3, 0 };
    if (debugActive && OpenGLCap.openGLVersion() < minVersion) {
        LINFO("OpenGL Debug context requested, but insufficient version available");
        debugActive = false;
    }

    if (debugActive) {
        using namespace ghoul::opengl::debug;

        bool synchronous = global::configuration->openGLDebugContext.isSynchronous;
        setDebugOutput(DebugOutput(debugActive), SynchronousOutput(synchronous));

        for (const configuration::Configuration::OpenGLDebugContext::IdentifierFilter&f :
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

        auto callback = [](Source source, Type type, Severity severity,
            unsigned int id, std::string message) -> void
        {
            const std::string s = ghoul::to_string(source);
            const std::string t = ghoul::to_string(type);

            const std::string category =
                "OpenGL (" + s + ") [" + t + "] {" + std::to_string(id) + "}";
            switch (severity) {
                case Severity::High:
                    LERRORC(category, message);
                    break;
                case Severity::Medium:
                    LWARNINGC(category, message);
                    break;
                case Severity::Low:
                    LINFOC(category, message);
                    break;
                case Severity::Notification:
                    LDEBUGC(category, message);
                    break;
                default:
                    throw ghoul::MissingCaseException();
            }

            if (global::configuration->openGLDebugContext.printStacktrace) {
                std::string stackString = "Stacktrace\n";
                std::vector<std::string> stack = ghoul::stackTrace();
                for (size_t i = 0; i < stack.size(); i++) {
                    stackString += fmt::format("{}: {}\n", i, stack[i]);
                }
                LDEBUGC(category, stackString);
            }
        };
        ghoul::opengl::debug::setDebugCallback(callback);
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
                        fmt::format("Function {}: GL_INVALID_ENUM", f.function->name())
                    );
                    break;
                case GL_INVALID_VALUE:
                    LERRORC(
                        "OpenGL Invalid State",
                        fmt::format("Function {}: GL_INVALID_VALUE", f.function->name())
                    );
                    break;
                case GL_INVALID_OPERATION:
                    LERRORC(
                        "OpenGL Invalid State",
                        fmt::format(
                            "Function {}: GL_INVALID_OPERATION", f.function->name()
                        ));
                    break;
                case GL_INVALID_FRAMEBUFFER_OPERATION:
                    LERRORC(
                        "OpenGL Invalid State",
                        fmt::format(
                            "Function {}: GL_INVALID_FRAMEBUFFER_OPERATION",
                            f.function->name()
                        )
                    );
                    break;
                case GL_OUT_OF_MEMORY:
                    LERRORC(
                        "OpenGL Invalid State",
                        fmt::format("Function {}: GL_OUT_OF_MEMORY", f.function->name())
                    );
                    break;
                default:
                    LERRORC(
                        "OpenGL Invalid State",
                        fmt::format("Unknown error code: {0:x}", static_cast<int>(error))
                    );
            }
        });
    }

    if (global::configuration->isLoggingOpenGLCalls) {
        using namespace ghoul::logging;
        LogLevel lvl = ghoul::from_string<LogLevel>(global::configuration->logging.level);
        if (lvl > LogLevel::Trace) {
            LWARNING(
                "Logging OpenGL calls is enabled, but the selected log level does "
                "not include TRACE, so no OpenGL logs will be printed");
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
        ZoneScopedN("[Module] initializeGL")
        func();
    }

    LINFO("Finished initializing OpenGL");

    LTRACE("OpenSpaceEngine::initializeGL(end)");
}

void OpenSpaceEngine::scheduleLoadSingleAsset(std::string assetPath) {
    _hasScheduledAssetLoading = true;
    _scheduledAssetPathToLoad = std::move(assetPath);
}

void OpenSpaceEngine::loadAsset(const std::string& assetName) {
    ZoneScoped

    LTRACE("OpenSpaceEngine::loadAsset(begin)");

    global::windowDelegate->setBarrier(false);
    global::windowDelegate->setSynchronization(false);
    defer {
        global::windowDelegate->setSynchronization(true);
        global::windowDelegate->setBarrier(true);
    };

    if (_scene) {
        ZoneScopedN("Reset scene")

        global::syncEngine->removeSyncables(global::timeManager->getSyncables());
        if (_scene && _scene->camera()) {
            global::syncEngine->removeSyncables(_scene->camera()->getSyncables());
        }
        global::renderEngine->setScene(nullptr);
        global::renderEngine->setCamera(nullptr);
        global::navigationHandler->setCamera(nullptr);
        _scene->clear();
        global::rootPropertyOwner->removePropertySubOwner(_scene.get());
    }

    std::unique_ptr<SceneInitializer> sceneInitializer;
    if (global::configuration->useMultithreadedInitialization) {
        unsigned int nAvailableThreads = std::min(
            std::thread::hardware_concurrency() - 1,
            4u
        );
        unsigned int nThreads = nAvailableThreads == 0 ? 2 : nAvailableThreads;
        sceneInitializer = std::make_unique<MultiThreadedSceneInitializer>(nThreads);
    }
    else {
        sceneInitializer = std::make_unique<SingleThreadedSceneInitializer>();
    }

    _scene = std::make_unique<Scene>(std::move(sceneInitializer));
    global::renderEngine->setScene(_scene.get());

    global::rootPropertyOwner->addPropertySubOwner(_scene.get());
    _scene->setCamera(std::make_unique<Camera>());
    Camera* camera = _scene->camera();
    camera->setParent(_scene->root());

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

    _assetManager->removeAll();
    if (!assetName.empty()) {
        _assetManager->add(assetName);
    }
    for (const std::string& a : global::profile->assets) {
        _assetManager->add(a);
    }

    _loadingScreen->setPhase(LoadingScreen::Phase::Construction);
    _loadingScreen->postMessage("Loading assets");

    _assetManager->update();

    _loadingScreen->setPhase(LoadingScreen::Phase::Synchronization);
    _loadingScreen->postMessage("Synchronizing assets");

    std::vector<const Asset*> allAssets = _assetManager->rootAsset().subTreeAssets();

    std::unordered_set<ResourceSynchronization*> resourceSyncs;
    for (const Asset* a : allAssets) {
        std::vector<ResourceSynchronization*> syncs = a->ownSynchronizations();

        for (ResourceSynchronization* s : syncs) {
            ZoneScopedN("Update resource synchronization")

            if (s->state() == ResourceSynchronization::State::Syncing) {
                LoadingScreen::ProgressInfo progressInfo;
                progressInfo.progress = s->progress();

                resourceSyncs.insert(s);
                _loadingScreen->updateItem(
                    s->name(),
                    s->name(),
                    LoadingScreen::ItemStatus::Started,
                    progressInfo
                );
            }
        }
    }
    _loadingScreen->setItemNumber(static_cast<int>(resourceSyncs.size()));

    bool loading = true;
    while (loading) {
        if (_shouldAbortLoading) {
            global::windowDelegate->terminate();
            break;
        }
        _loadingScreen->render();
        _assetManager->update();

        loading = false;
        auto it = resourceSyncs.begin();
        while (it != resourceSyncs.end()) {
            if ((*it)->state() == ResourceSynchronization::State::Syncing) {
                LoadingScreen::ProgressInfo progressInfo;
                progressInfo.progress = (*it)->progress();

                if ((*it)->nTotalBytesIsKnown()) {
                    progressInfo.currentSize = (*it)->nSynchronizedBytes();
                    progressInfo.totalSize = (*it)->nTotalBytes();
                }

                loading = true;
                _loadingScreen->updateItem(
                    (*it)->name(),
                    (*it)->name(),
                    LoadingScreen::ItemStatus::Started,
                    progressInfo
                );
                ++it;
            }
            else {
                LoadingScreen::ProgressInfo progressInfo;
                progressInfo.progress = 1.f;

                _loadingScreen->tickItem();
                _loadingScreen->updateItem(
                    (*it)->name(),
                    (*it)->name(),
                    LoadingScreen::ItemStatus::Finished,
                    progressInfo
                );
                it = resourceSyncs.erase(it);
            }
        }
    }
    if (_shouldAbortLoading) {
        _loadingScreen = nullptr;
        return;
    }

    _loadingScreen->setPhase(LoadingScreen::Phase::Initialization);

    _loadingScreen->postMessage("Initializing scene");
    while (_scene->isInitializing()) {
        _loadingScreen->render();
    }

    _loadingScreen->postMessage("Initializing OpenGL");
    _loadingScreen->finalize();

    _loadingScreen = nullptr;

    global::renderEngine->updateScene();

    global::syncEngine->addSyncables(global::timeManager->getSyncables());
    if (_scene && _scene->camera()) {
        global::syncEngine->addSyncables(_scene->camera()->getSyncables());
    }

#ifdef __APPLE__
    showTouchbar();
#endif // APPLE

    runGlobalCustomizationScripts();

    _writeDocumentationTask = std::async(&OpenSpaceEngine::writeSceneDocumentation, this);

    LTRACE("OpenSpaceEngine::loadAsset(end)");
}

void OpenSpaceEngine::deinitialize() {
    ZoneScoped

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
            global::renderEngine->scene()->camera()->getSyncables()
        );
    }
    global::sessionRecording->deinitialize();
    global::versionChecker->cancel();

    _assetManager = nullptr;

    global::deinitialize();

    FactoryManager::deinitialize();
    TransformationManager::deinitialize();
    SpiceManager::deinitialize();

    ghoul::fontrendering::FontRenderer::deinitialize();

    ghoul::logging::LogManager::deinitialize();

    LTRACE("deinitialize(end)");


    LTRACE("OpenSpaceEngine::deinitialize(end)");
}

void OpenSpaceEngine::deinitializeGL() {
    ZoneScoped

    LTRACE("OpenSpaceEngine::deinitializeGL(begin)");

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

void OpenSpaceEngine::writeStaticDocumentation() {
    std::string path = global::configuration->documentation.path;
    if (!path.empty()) {

        DocEng.addHandlebarTemplates(global::scriptEngine->templatesToRegister());
        DocEng.addHandlebarTemplates(FactoryManager::ref().templatesToRegister());
        DocEng.addHandlebarTemplates(DocEng.templatesToRegister());

        _documentationJson += "{\"name\":\"Scripting\",";
        _documentationJson += "\"identifier\":\"" + global::scriptEngine->jsonName();
        _documentationJson += "\",\"data\":" + global::scriptEngine->generateJson();
        _documentationJson += "},";

        _documentationJson += "{\"name\":\"Top Level\",";
        _documentationJson += "\"identifier\":\"" + DocEng.jsonName();
        _documentationJson += "\",\"data\":" + DocEng.generateJson();
        _documentationJson += "},";

        _documentationJson += "{\"name\":\"Factory\",";
        _documentationJson += "\"identifier\":\"" + FactoryManager::ref().jsonName();
        _documentationJson += "\",\"data\":" + FactoryManager::ref().generateJson();
        _documentationJson += "},";
    }
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
    ZoneScoped

    LINFO("Running Global initialization scripts");
    ghoul::lua::LuaState state;
    global::scriptEngine->initializeLuaState(state);

    for (const std::string& script : global::configuration->globalCustomizationScripts) {
        std::filesystem::path s = absPath(script);
        if (std::filesystem::is_regular_file(s)) {
            try {
                LINFO(fmt::format("Running global customization script: {}", s));
                ghoul::lua::runScriptFile(state, s.string());
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
        else {
            LDEBUG(fmt::format("Ignoring non-existing script file: {}", s));
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
            LERROR(fmt::format("Could not find font {} for key '{}'", fontName, key));
            continue;
        }

        LDEBUG(fmt::format("Registering font {} with key '{}'", fontName, key));
        bool success = global::fontManager->registerFontPath(key, fontName);

        if (!success) {
            LERROR(fmt::format(
                "Error registering font {} with key '{}'", fontName, key
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

void OpenSpaceEngine::writeSceneDocumentation() {
    ZoneScoped

    // Write documentation to json files if config file supplies path for doc files

    std::string path = global::configuration->documentation.path;
    if (!path.empty()) {
        std::future<std::string> root = std::async(
            &properties::PropertyOwner::generateJson,
            global::rootPropertyOwner
        );

        std::future<std::string> scene = std::async(
            &properties::PropertyOwner::generateJson,
            _scene.get()
        );



        path = absPath(path).string() + '/';
        _documentationJson += "{\"name\":\"Keybindings\",\"identifier\":\"";
        _documentationJson += global::keybindingManager->jsonName() + "\",";
        _documentationJson += "\"data\":";
        _documentationJson += global::keybindingManager->generateJson();
        _documentationJson += "},";
        _documentationJson += "{\"name\":\"Scene License Information\",";
        _documentationJson += "\"identifier\":\"sceneLicense";
        _documentationJson += "\",\"data\":";
        _documentationJson += SceneLicenseWriter().generateJson();
        _documentationJson += "},";
        _documentationJson += "{\"name\":\"Scene Properties\",";
        _documentationJson += "\"identifier\":\"propertylist";// + _scene->jsonName();
        _documentationJson += "\",\"data\":" + root.get();
        _documentationJson += "},";
        _documentationJson += "{\"name\":\"Scene Graph Information\",";
        _documentationJson += "\"identifier\":\"propertylist";
        _documentationJson += "\",\"data\":" + scene.get();
        _documentationJson += "}";

        //add templates for the jsons we just registered
        DocEng.addHandlebarTemplates(global::keybindingManager->templatesToRegister());
        //TODO this is in efficaiant, here i am just instaning the class to get
        //at a member variable which is staticly defined. How do i just get that
        SceneLicenseWriter writer;
        DocEng.addHandlebarTemplates(writer.templatesToRegister());
        DocEng.addHandlebarTemplates(global::rootPropertyOwner->templatesToRegister());

        //the static documentation shoudl be finished already
        //so now that we wrote the static and secene json files
        //we should write the html file that uses them.
        _documentationJson += "]}";

        DocEng.writeDocumentationHtml(path, _documentationJson);
    }
    //no else, if path was empty, that means that no documentation is requested
}

void OpenSpaceEngine::preSynchronization() {
    ZoneScoped
    TracyGpuZone("preSynchronization")

    LTRACE("OpenSpaceEngine::preSynchronization(begin)");

    FileSys.triggerFilesystemEvents();

    // Reset the temporary, frame-based storage
    global::memoryManager->TemporaryMemory.reset();

    if (_hasScheduledAssetLoading) {
        LINFO(fmt::format("Loading asset: {}", absPath(_scheduledAssetPathToLoad)));
        global::profile->ignoreUpdates = true;
        loadAsset(_scheduledAssetPathToLoad);
        global::profile->ignoreUpdates = false;
        resetPropertyChangeFlagsOfSubowners(global::rootPropertyOwner);
        _hasScheduledAssetLoading = false;
        _scheduledAssetPathToLoad.clear();
    }
    else if (_isRenderingFirstFrame) {
        global::profile->ignoreUpdates = true;
        loadAsset("");
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

    bool master = global::windowDelegate->isMaster();

    global::syncEngine->preSynchronization(SyncEngine::IsMaster(master));
    if (master) {
        double dt = global::windowDelegate->deltaTime();

        if (global::sessionRecording->isSavingFramesDuringPlayback()) {
            dt = global::sessionRecording->fixedDeltaTimeDuringFrameOutput();
        }

        global::timeManager->preSynchronization(dt);

        using Iter = std::vector<std::string>::const_iterator;
        std::pair<Iter, Iter> scheduledScripts = global::scriptScheduler->progressTo(
            global::timeManager->time().j2000Seconds()
        );
        for (Iter it = scheduledScripts.first; it != scheduledScripts.second; ++it) {
            global::scriptEngine->queueScript(
                *it,
                scripting::ScriptEngine::RemoteScripting::Yes
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
        ZoneScopedN("[Module] preSync")

        func();
    }

    if (_isRenderingFirstFrame) {
        setCameraFromProfile(*global::profile);
        setAdditionalScriptsFromProfile(*global::profile);
    }
    LTRACE("OpenSpaceEngine::preSynchronization(end)");
}

void OpenSpaceEngine::postSynchronizationPreDraw() {
    ZoneScoped
    TracyGpuZone("postSynchronizationPreDraw")
    LTRACE("OpenSpaceEngine::postSynchronizationPreDraw(begin)");

    bool master = global::windowDelegate->isMaster();
    global::syncEngine->postSynchronization(SyncEngine::IsMaster(master));

    // This probably doesn't have to be done here every frame, but doing it earlier gives
    // weird results when using side_by_side stereo --- abock
    using FR = ghoul::fontrendering::FontRenderer;
    FR::defaultRenderer().setFramebufferSize(global::renderEngine->fontResolution());

    FR::defaultProjectionRenderer().setFramebufferSize(
        global::renderEngine->renderingResolution()
    );

    if (_shutdown.inShutdown) {
        if (_shutdown.timer <= 0.f) {
            global::windowDelegate->terminate();
            return;
        }
        _shutdown.timer -= static_cast<float>(global::windowDelegate->averageDeltaTime());
    }

    const bool updated = _assetManager->update();
    if (updated) {
        if (_writeDocumentationTask.valid()) {
            // If there still is a documentation creation task the previous frame, we need
            // to wait for it to finish first, or else we might write to the same file
            _writeDocumentationTask.wait();
        }
        _writeDocumentationTask = std::async(
            &OpenSpaceEngine::writeSceneDocumentation, this
        );
    }

    global::renderEngine->updateScene();
    global::renderEngine->updateRenderer();
    global::renderEngine->updateScreenSpaceRenderables();
    global::renderEngine->updateShaderPrograms();

    if (!master) {
        _scene->camera()->invalidateCache();
    }

    for (const std::function<void()>& func : *global::callback::postSyncPreDraw) {
        ZoneScopedN("[Module] postSyncPreDraw")

        func();
    }

    // Testing this every frame has minimal impact on the performance --- abock
    // Debug build: 1-2 us ; Release build: <= 1 us
    using ghoul::logging::LogManager;
    int warningCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Warning);
    int errorCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Error);
    int fatalCounter = LogMgr.messageCounter(ghoul::logging::LogLevel::Fatal);

    if (warningCounter > 0) {
        LWARNINGC("Logging", fmt::format("Number of Warnings: {}", warningCounter));
    }
    if (errorCounter > 0) {
        LWARNINGC("Logging", fmt::format("Number of Errors: {}", errorCounter));
    }
    if (fatalCounter > 0) {
        LWARNINGC("Logging", fmt::format("Number of Fatals: {}", fatalCounter));
    }

    LogMgr.resetMessageCounters();

    LTRACE("OpenSpaceEngine::postSynchronizationPreDraw(end)");
}

void OpenSpaceEngine::render(const glm::mat4& sceneMatrix, const glm::mat4& viewMatrix,
                             const glm::mat4& projectionMatrix)
{
    ZoneScoped
    TracyGpuZone("Render")
    LTRACE("OpenSpaceEngine::render(begin)");

    const bool isGuiWindow =
        global::windowDelegate->hasGuiWindow() ?
        global::windowDelegate->isGuiWindow() :
        true;

    if (isGuiWindow) {
        global::luaConsole->update();
    }

    global::renderEngine->render(sceneMatrix, viewMatrix, projectionMatrix);

    for (const std::function<void()>& func : *global::callback::render) {
        ZoneScopedN("[Module] render")

        func();
    }

    LTRACE("OpenSpaceEngine::render(end)");
}

void OpenSpaceEngine::drawOverlays() {
    ZoneScoped
    TracyGpuZone("Draw2D")
    LTRACE("OpenSpaceEngine::drawOverlays(begin)");

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
        ZoneScopedN("[Module] draw2D")
        func();
    }

    LTRACE("OpenSpaceEngine::drawOverlays(end)");
}

void OpenSpaceEngine::postDraw() {
    ZoneScoped
    TracyGpuZone("postDraw")
    LTRACE("OpenSpaceEngine::postDraw(begin)");

    global::renderEngine->postDraw();

    for (const std::function<void()>& func : *global::callback::postDraw) {
        ZoneScopedN("[Module] postDraw")

        func();
    }

    if (_isRenderingFirstFrame) {
        global::windowDelegate->setSynchronization(true);
        resetPropertyChangeFlags();
        _isRenderingFirstFrame = false;
    }

    global::memoryManager->PersistentMemory.housekeeping();

    LTRACE("OpenSpaceEngine::postDraw(end)");
}

void OpenSpaceEngine::resetPropertyChangeFlags() {
    ZoneScoped

    std::vector<SceneGraphNode*> nodes =
        global::renderEngine->scene()->allSceneGraphNodes();
    for (SceneGraphNode* n : nodes) {
        resetPropertyChangeFlagsOfSubowners(n);
    }
}

void OpenSpaceEngine::resetPropertyChangeFlagsOfSubowners(properties::PropertyOwner* po) {
    for (properties::PropertyOwner* subOwner : po->propertySubOwners()) {
        resetPropertyChangeFlagsOfSubowners(subOwner);
    }
    for (properties::Property* p : po->properties()) {
        p->resetToUnchanged();
    }
}

void OpenSpaceEngine::keyboardCallback(Key key, KeyModifier mod, KeyAction action) {
    ZoneScoped

    if (_loadingScreen) {
        // If the loading screen object exists, we are currently loading and want key
        // presses to behave differently
        if (key == Key::Escape) {
            _shouldAbortLoading = true;
        }

        return;
    }

    using F = std::function<bool (Key, KeyModifier, KeyAction)>;
    for (const F& func : *global::callback::keyboard) {
        const bool isConsumed = func(key, mod, action);
        if (isConsumed) {
            return;
        }
    }

    if (!global::configuration->isConsoleDisabled) {
        bool isConsoleConsumed = global::luaConsole->keyboardCallback(key, mod, action);
        if (isConsoleConsumed) {
            return;
        }
    }

    global::navigationHandler->keyboardCallback(key, mod, action);
    global::keybindingManager->keyboardCallback(key, mod, action);
    global::interactionMonitor->markInteraction();
}

void OpenSpaceEngine::charCallback(unsigned int codepoint, KeyModifier modifier) {
    ZoneScoped

    using F = std::function<bool (unsigned int, KeyModifier)>;
    for (const F& func : *global::callback::character) {
        bool isConsumed = func(codepoint, modifier);
        if (isConsumed) {
            return;
        }
    }

    global::luaConsole->charCallback(codepoint, modifier);
    global::interactionMonitor->markInteraction();
}

void OpenSpaceEngine::mouseButtonCallback(MouseButton button,
                                          MouseAction action,
                                          KeyModifier mods)
{
    ZoneScoped

    using F = std::function<bool (MouseButton, MouseAction, KeyModifier)>;
    for (const F& func : *global::callback::mouseButton) {
        bool isConsumed = func(button, action, mods);
        if (isConsumed) {
            // If the mouse was released, we still want to forward it to the navigation
            // handler in order to reliably terminate a rotation or zoom. Accidentally
            // moving the cursor over a UI window is easy to miss and leads to weird
            // continuing movement
            if (action == MouseAction::Release) {
                break;
            }
            else {
                return;
            }
        }
    }

    // Check if the user clicked on one of the 'buttons' the RenderEngine is drawing
    if (action == MouseAction::Press) {
        bool isConsumed = global::renderEngine->mouseActivationCallback(_mousePosition);
        if (isConsumed) {
            return;
        }
    }

    global::navigationHandler->mouseButtonCallback(button, action);
    global::interactionMonitor->markInteraction();
}

void OpenSpaceEngine::mousePositionCallback(double x, double y) {
    ZoneScoped

    using F = std::function<void (double, double)>;
    for (const F& func : *global::callback::mousePosition) {
        func(x, y);
    }

    global::navigationHandler->mousePositionCallback(x, y);
    global::interactionMonitor->markInteraction();

    _mousePosition = glm::vec2(static_cast<float>(x), static_cast<float>(y));
}

void OpenSpaceEngine::mouseScrollWheelCallback(double posX, double posY) {
    ZoneScoped

    using F = std::function<bool (double, double)>;
    for (const F& func : *global::callback::mouseScrollWheel) {
        bool isConsumed = func(posX, posY);
        if (isConsumed) {
            return;
        }
    }

    global::navigationHandler->mouseScrollWheelCallback(posY);
    global::interactionMonitor->markInteraction();
}

void OpenSpaceEngine::touchDetectionCallback(TouchInput input) {
    ZoneScoped

    using F = std::function<bool (TouchInput)>;
    for (const F& func : *global::callback::touchDetected) {
        bool isConsumed = func(input);
        if (isConsumed) {
            return;
        }
    }
}

void OpenSpaceEngine::touchUpdateCallback(TouchInput input) {
    ZoneScoped

    using F = std::function<bool(TouchInput)>;
    for (const F& func : *global::callback::touchUpdated) {
        bool isConsumed = func(input);
        if (isConsumed) {
            return;
        }
    }
}

void OpenSpaceEngine::touchExitCallback(TouchInput input) {
    ZoneScoped

    using F = std::function<void(TouchInput)>;
    for (const F& func : *global::callback::touchExit) {
        func(input);
    }
}

void OpenSpaceEngine::handleDragDrop(const std::string& file) {
    std::filesystem::path f(file);

    ghoul::lua::LuaState s(ghoul::lua::LuaState::IncludeStandardLibrary::Yes);
    std::filesystem::path absolutePath = absPath("${SCRIPTS}/drag_drop_handler.lua");
    int status = luaL_loadfile(s, absolutePath.string().c_str());
    if (status != LUA_OK) {
        std::string error = lua_tostring(s, -1);
        LERROR(error);
        return;
    }

    ghoul::lua::push(s, file);
    lua_setglobal(s, "filename");

    std::string basename = f.filename().string();
    ghoul::lua::push(s, basename);
    lua_setglobal(s, "basename");

    std::string extension = f.extension().string();
    std::transform(
        extension.begin(), extension.end(),
        extension.begin(),
        [](char c) { return static_cast<char>(::tolower(c)); }
    );
    ghoul::lua::push(s, extension);
    lua_setglobal(s, "extension");

    status = lua_pcall(s, 0, 1, 0);
    if (status != LUA_OK) {
        std::string error = lua_tostring(s, -1);
        LERROR(error);
        return;
    }

    if (lua_isnil(s, -1)) {
        LWARNING(fmt::format("Unhandled file dropped: {}", file));
        return;
    }

    std::string script = ghoul::lua::value<std::string>(s);
    global::scriptEngine->queueScript(
        script,
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

std::vector<std::byte> OpenSpaceEngine::encode() {
    ZoneScoped

    std::vector<std::byte> buffer = global::syncEngine->encodeSyncables();
    return buffer;
}

void OpenSpaceEngine::decode(std::vector<std::byte> data) {
    ZoneScoped

    global::syncEngine->decodeSyncables(std::move(data));
}

void OpenSpaceEngine::toggleShutdownMode() {
    if (_shutdown.inShutdown) {
        // If we are already in shutdown mode, we want to disable it
        _shutdown.inShutdown = false;
    }
    else {
        // Else, we have to enable it
        _shutdown.timer = _shutdown.waitTime;
        _shutdown.inShutdown = true;
    }
}

void setCameraFromProfile(const Profile& p) {
    std::visit(
        overloaded{
            [](const Profile::CameraNavState& navStateProfile) {
                interaction::NavigationState nav;
                nav.anchor = navStateProfile.anchor;
                if (navStateProfile.aim.has_value()) {
                    nav.aim = navStateProfile.aim.value();
                }
                if (nav.referenceFrame.empty()) {
                    nav.referenceFrame = "Root";
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
            [](const Profile::CameraGoToGeo& geo) {
                //Instead of direct calls to navigation state code, lua commands with
                //globebrowsing goToGeo are used because this prevents a module
                //dependency in this core code. Eventually, goToGeo will be incorporated
                //in the OpenSpace core and this code will change.
                std::string geoScript = fmt::format("openspace.globebrowsing.goToGeo"
                    "([[{}]], {}, {}", geo.anchor, geo.latitude, geo.longitude);
                if (geo.altitude.has_value()) {
                    geoScript += fmt::format(", {}", geo.altitude.value());
                }
                geoScript += ")";
                global::scriptEngine->queueScript(
                    geoScript,
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
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
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            }
        }
        else {
            if (mod.notLoadedInstruction.has_value()) {
                global::scriptEngine->queueScript(
                    mod.notLoadedInstruction.value(),
                    scripting::ScriptEngine::RemoteScripting::Yes
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
            LERROR(
                fmt::format("Action for identifier '{}' already existed & registered",
                a.identifier)
            );
        }
        if (a.script.empty()) {
            LERROR(
                fmt::format("Identifier '{}' doesn't provide a Lua command to execute",
                a.identifier)
            );
        }
        interaction::Action action;
        action.identifier = a.identifier;
        action.command = a.script;
        action.name = a.name;
        action.documentation = a.documentation;
        action.guiPath = a.guiPath;
        action.synchronization = interaction::Action::IsSynchronized(a.isLocal);
        global::actionManager->registerAction(std::move(action));
    }
}

void setKeybindingsFromProfile(const Profile& p) {
    for (Profile::Keybinding k : p.keybindings) {
        if (k.action.empty()) {
            LERROR("Action must not be empty");
        }
        if (!global::actionManager->hasAction(k.action)) {
            LERROR(fmt::format("Action '{}' does not exist", k.action));
        }
        if (k.key.key == openspace::Key::Unknown) {
            LERROR(
                fmt::format(
                    "Could not find key '{}'",
                    std::to_string(static_cast<uint16_t>(k.key.key))
                )
            );
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
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

scripting::LuaLibrary OpenSpaceEngine::luaLibrary() {
    return {
        "",
        {
            {
                "toggleShutdown",
                &luascriptfunctions::toggleShutdown,
                {},
                "",
                "Toggles the shutdown mode that will close the application after the "
                "count down timer is reached"
            },
            {
                "writeDocumentation",
                &luascriptfunctions::writeDocumentation,
                {},
                "",
                "Writes out documentation files"
            },
            {
                "downloadFile",
                &luascriptfunctions::downloadFile,
                {},
                "",
                "Downloads a file from Lua scope"
            },
            {
                "addVirtualProperty",
                &luascriptfunctions::addVirtualProperty,
                {},
                "type, name, identifier,"
                "[description, value, minimumValue, maximumValue]",
                "Adds a virtual property that will set a group of properties"
            },
            {
                "removeVirtualProperty",
                &luascriptfunctions::removeVirtualProperty,
                {},
                "string",
                "Removes a previously added virtual property"
            },
            {
                "removeAllVirtualProperties",
                &luascriptfunctions::removeAllVirtualProperties,
                {},
                "",
                "Remove all registered virtual properties"
            },
            {
                "setScreenshotFolder",
                &luascriptfunctions::setScreenshotFolder,
                {},
                "string",
                "Sets the folder used for storing screenshots or session recording frames"
            },
            {
                "addTag",
                &luascriptfunctions::addTag,
                {},
                "string, string",
                "Adds a tag (second argument) to a scene graph node (first argument)"
            },
            {
                "removeTag",
                &luascriptfunctions::removeTag,
                {},
                "string, string",
                "Removes a tag (second argument) from a scene graph node (first argument)"
            },
            {
                "createSingleColorImage",
                &luascriptfunctions::createSingleColorImage,
                {},
                "string, vec3",
                "Creates a 1 pixel image with a certain color in the cache folder and "
                "returns the path to the file. If a cached file with the given name "
                "already exists, the path to that file is returned. The first argument "
                "is the name of the file, without extension. The second is the RGB "
                "color, given as {r, g, b} with values between 0 and 1."
            },
            {
                "isMaster",
                &luascriptfunctions::isMaster,
                {},
                "",
                "Returns whether the current OpenSpace instance is the master node of a "
                "cluster configuration. If this instance is not part of a cluster, this "
                "function also returns 'true'."
            }
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

}  // namespace openspace
