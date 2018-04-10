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

#include <openspace/engine/openspaceengine.h>

#include <openspace/openspace.h>

#include <openspace/documentation/core_registration.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/logfactory.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/virtualpropertymanager.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/interaction/luaconsole.h>
#include <openspace/network/networkengine.h>
#include <openspace/network/parallelpeer.h>

#include <openspace/performance/performancemeasurement.h>

#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/dashboarditem.h>
#include <openspace/rendering/loadingscreen.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/scripting/scriptengine.h>

#include <openspace/scene/asset.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/assetloader.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/scene/scenelicense.h>
#include <openspace/scene/translation.h>
#include <openspace/util/resourcesynchronization.h>

#include <openspace/util/factorymanager.h>
#include <openspace/util/openspacemodule.h>
#include <openspace/util/synchronizationwatcher.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/task.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/transformationmanager.h>

#include <ghoul/ghoul.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/misc/defer.h>
#include <ghoul/opengl/debugcontext.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/systemcapabilities/systemcapabilities.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

#include <glbinding/callbacks.h>

#if defined(_MSC_VER) && defined(OPENSPACE_ENABLE_VLD)
#include <vld.h>
#endif

#ifdef WIN32
#include <Windows.h>
#endif

#ifdef __APPLE__
#include <openspace/interaction/touchbar.h>
#endif // __APPLE__


#include <numeric>

#include "openspaceengine_lua.inl"

using namespace openspace::scripting;
using namespace ghoul::filesystem;
using namespace ghoul::logging;
using namespace ghoul::cmdparser;

namespace {
    constexpr const char* _loggerCat = "OpenSpaceEngine";
    constexpr const char* SgctDefaultConfigFile = "${CONFIG}/single.xml";

    constexpr const char* SgctConfigArgumentCommand = "-config";

    constexpr const int CacheVersion = 1;

    const glm::ivec3 FontAtlasSize{ 1536, 1536, 1 };

    struct {
        std::string configurationName;
        std::string sgctConfigurationName;
        std::string sceneName;
        std::string cacheFolder;
    } commandlineArgumentPlaceholders;


    static const openspace::properties::Property::PropertyInfo VersionInfo = {
        "VersionInfo",
        "Version Information",
        "This value contains the full string identifying this OpenSpace Version"
    };

    static const openspace::properties::Property::PropertyInfo SourceControlInfo = {
        "SCMInfo",
        "Source Control Management Information",
        "This value contains information from the SCM, such as commit hash and branch"
    };
} // namespace

namespace openspace {

namespace properties { class Property; }

class Scene;

OpenSpaceEngine* OpenSpaceEngine::_engine = nullptr;

OpenSpaceEngine::OpenSpaceEngine(std::string programName,
                                 std::unique_ptr<WindowWrapper> windowWrapper)
    : _configurationManager(new ConfigurationManager)
    , _scene(nullptr)
    , _dashboard(new Dashboard)
    , _downloadManager(nullptr)
    , _console(new LuaConsole)
    , _moduleEngine(new ModuleEngine)
    , _networkEngine(new NetworkEngine)
    , _parallelPeer(new ParallelPeer)
    , _renderEngine(new RenderEngine)
    , _syncEngine(std::make_unique<SyncEngine>(4096))
    , _timeManager(new TimeManager)
    , _windowWrapper(std::move(windowWrapper))
    , _commandlineParser(new ghoul::cmdparser::CommandlineParser(
        programName, ghoul::cmdparser::CommandlineParser::AllowUnknownCommands::Yes
    ))
    , _navigationHandler(new interaction::NavigationHandler)
    , _keyBindingManager(new interaction::KeyBindingManager)
    , _scriptEngine(new scripting::ScriptEngine)
    , _scriptScheduler(new scripting::ScriptScheduler)
    , _virtualPropertyManager(new VirtualPropertyManager)
    , _rootPropertyOwner(new properties::PropertyOwner({ "" }))
    , _loadingScreen(nullptr)
    , _versionInformation{
        properties::StringProperty(VersionInfo, OPENSPACE_VERSION_STRING_FULL),
        properties::StringProperty(SourceControlInfo, OPENSPACE_GIT_FULL)
    }
    , _hasScheduledAssetLoading(false)
    , _scheduledAssetPathToLoad("")
    , _shutdown({false, 0.f, 0.f})
    , _isFirstRenderingFirstFrame(true)
{
    _rootPropertyOwner->addPropertySubOwner(_moduleEngine.get());

    _navigationHandler->setPropertyOwner(_rootPropertyOwner.get());
    // New property subowners also have to be added to the ImGuiModule callback!
    _rootPropertyOwner->addPropertySubOwner(_navigationHandler.get());

    _rootPropertyOwner->addPropertySubOwner(_renderEngine.get());
    _rootPropertyOwner->addPropertySubOwner(_renderEngine->screenSpaceOwner());

    if (_windowWrapper) {
        _rootPropertyOwner->addPropertySubOwner(_windowWrapper.get());
    }
    _rootPropertyOwner->addPropertySubOwner(_parallelPeer.get());
    _rootPropertyOwner->addPropertySubOwner(_console.get());
    _rootPropertyOwner->addPropertySubOwner(_dashboard.get());

    _versionInformation.versionString.setReadOnly(true);
    _rootPropertyOwner->addProperty(_versionInformation.versionString);
    _versionInformation.sourceControlInformation.setReadOnly(true);
    _rootPropertyOwner->addProperty(_versionInformation.sourceControlInformation);

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

    _syncEngine->addSyncable(_scriptEngine.get());
}

OpenSpaceEngine& OpenSpaceEngine::ref() {
    ghoul_assert(_engine, "OpenSpaceEngine not created");
    return *_engine;
}

OpenSpaceEngine::~OpenSpaceEngine() {}

bool OpenSpaceEngine::isCreated() {
    return _engine != nullptr;
}

void OpenSpaceEngine::create(int argc, char** argv,
                             std::unique_ptr<WindowWrapper> windowWrapper,
                             std::vector<std::string>& sgctArguments,
                             bool& requestClose, bool consoleLog)
{
    ghoul_assert(!_engine, "OpenSpaceEngine was already created");
    //ghoul_assert(windowWrapper != nullptr, "No Window Wrapper was provided");

    requestClose = false;

    LDEBUG("Initialize FileSystem");

    ghoul::initialize();

    // Initialize the LogManager and add the console log as this will be used every time
    // and we need a fall back if something goes wrong between here and when we add the
    // logs from the configuration file. If the user requested as specific loglevel in the
    // configuration file, we will deinitialize this LogManager and reinitialize it later
    // with the correct LogLevel
    LogManager::initialize(
        LogLevel::Debug,
        ghoul::logging::LogManager::ImmediateFlush::Yes
    );
    if (consoleLog) {
        LogMgr.addLog(std::make_unique<ConsoleLog>());
    }

    // Sanity check of values
    if (argc < 1 || argv == nullptr) {
        throw ghoul::RuntimeError(
            "No arguments were passed to this function",
            "OpenSpaceEngine"
        );
    }

    // Create other objects
    LDEBUG("Creating OpenSpaceEngine");
    _engine = new OpenSpaceEngine(std::string(argv[0]), std::move(windowWrapper));

    // Query modules for commandline arguments
    _engine->gatherCommandlineArguments();

    // Parse commandline arguments
    std::vector<std::string> args(argv, argv + argc);
    std::shared_ptr<const std::vector<std::string>> arguments =
        _engine->_commandlineParser->setCommandLine(args);

    bool showHelp = _engine->_commandlineParser->execute();
    if (showHelp) {
        _engine->_commandlineParser->displayHelp();
        requestClose = true;
        return;
    }
    sgctArguments = *arguments;

    // Find configuration
    std::string configurationFilePath = commandlineArgumentPlaceholders.configurationName;
    if (configurationFilePath.empty()) {
        LDEBUG("Finding configuration");
        configurationFilePath =
            ConfigurationManager::findConfiguration(configurationFilePath);
    }
    configurationFilePath = absPath(configurationFilePath);

    if (!FileSys.fileExists(configurationFilePath)) {
        throw ghoul::FileNotFoundError(
            "Configuration file '" + configurationFilePath + "'"
        );
    }
    LINFO(fmt::format("Configuration Path: '{}'", configurationFilePath));

    // Loading configuration from disk
    LDEBUG("Loading configuration from disk");
    try {
        _engine->configurationManager().loadFromFile(configurationFilePath);
    }
    catch (const documentation::SpecificationError& e) {
        LFATAL(fmt::format(
            "Loading of configuration file '{}' failed", configurationFilePath
        ));
        for (const documentation::TestResult::Offense& o : e.result.offenses) {
            LERRORC(o.offender, std::to_string(o.reason));
        }
        for (const documentation::TestResult::Warning& w : e.result.warnings) {
            LWARNINGC(w.offender, std::to_string(w.reason));
        }
        throw;
    }
    catch (const ghoul::RuntimeError& e) {
        LFATAL(fmt::format(
            "Loading of configuration file '{}' failed", configurationFilePath
        ));
        LFATALC(e.component, e.message);
        throw;
    }

    const bool hasCacheCommandline = !commandlineArgumentPlaceholders.cacheFolder.empty();
    const bool hasCacheConfig = _engine->configurationManager().hasKeyAndValue<bool>(
        ConfigurationManager::KeyPerSceneCache
    );
    std::string cacheFolder = absPath("${CACHE}");
    if (hasCacheCommandline || hasCacheConfig) {
        if (hasCacheCommandline) {
            cacheFolder = commandlineArgumentPlaceholders.cacheFolder;
        }
        if (hasCacheConfig) {
            std::string scene = _engine->configurationManager().value<std::string>(
                ConfigurationManager::KeyConfigAsset
            );
            cacheFolder += "-" + ghoul::filesystem::File(scene).baseName();
        }

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
        if (!FileSys.directoryExists(token)) {
            std::string p = absPath(token);
            FileSys.createDirectory(p, ghoul::filesystem::FileSystem::Recursive::Yes);
        }
    }

    // Initialize the requested logs from the configuration file
    _engine->configureLogging(consoleLog);

    LINFOC("OpenSpace Version", std::string(OPENSPACE_VERSION_STRING_FULL));
    LINFOC("Commit", std::string(OPENSPACE_GIT_FULL));

    ghoul::Dictionary moduleConfigurations;
    if (_engine->configurationManager().hasKeyAndValue<ghoul::Dictionary>(
        ConfigurationManager::KeyModuleConfigurations))
    {
        _engine->configurationManager().getValue<ghoul::Dictionary>(
            ConfigurationManager::KeyModuleConfigurations,
            moduleConfigurations
        );
    }

    // Register modules
    _engine->_moduleEngine->initialize(moduleConfigurations);

    // After registering the modules, the documentations for the available classes
    // can be added as well
    for (OpenSpaceModule* m : _engine->_moduleEngine->modules()) {
        for (const documentation::Documentation& doc : m->documentations()) {
            DocEng.addDocumentation(doc);
        }
    }

    // Create the cachemanager
    try {
        FileSys.createCacheManager(cacheFolder, CacheVersion);
    }
    catch (const ghoul::RuntimeError& e) {
        LFATAL("Could not create Cache Manager");
        LFATALC(e.component, e.message);
    }

    // Register the provided shader directories
    ghoul::opengl::ShaderPreprocessor::addIncludePath(absPath("${SHADERS}"));

    // Determining SGCT configuration file
    LDEBUG("Determining SGCT configuration file");
    std::string sgctConfigurationPath = SgctDefaultConfigFile;
    _engine->configurationManager().getValue(
        ConfigurationManager::KeyConfigSgct, sgctConfigurationPath);

    if (!commandlineArgumentPlaceholders.sgctConfigurationName.empty()) {
        LDEBUG(fmt::format(
            "Overwriting SGCT configuration file with commandline argument: {}",
            commandlineArgumentPlaceholders.sgctConfigurationName
        ));
        sgctConfigurationPath = commandlineArgumentPlaceholders.sgctConfigurationName;
    }

    // Prepend the outgoing sgctArguments with the program name
    // as well as the configuration file that sgct is supposed to use
    sgctArguments.insert(sgctArguments.begin(), argv[0]);
    sgctArguments.insert(sgctArguments.begin() + 1, SgctConfigArgumentCommand);
    sgctArguments.insert(sgctArguments.begin() + 2, absPath(sgctConfigurationPath));

    // Set up asset loader
    std::unique_ptr<SynchronizationWatcher> w =
        std::make_unique<SynchronizationWatcher>();
    SynchronizationWatcher* rawWatcher = w.get();

    _engine->_assetManager = std::make_unique<AssetManager>(
        std::make_unique<AssetLoader>(
            *OsEng.scriptEngine().luaState(),
            rawWatcher,
            FileSys.absPath("${ASSETS}")
        ),
        std::move(w)
    );
}

void OpenSpaceEngine::destroy() {
    if (_engine->parallelPeer().status() !=
        ParallelConnection::Status::Disconnected)
    {
        _engine->parallelPeer().disconnect();
    }

    _engine->_syncEngine->removeSyncables(_engine->timeManager().getSyncables());
    _engine->_syncEngine->removeSyncables(_engine->_renderEngine->getSyncables());

    _engine->_renderEngine->deinitializeGL();

    _engine->_moduleEngine->deinitializeGL();
    _engine->_moduleEngine->deinitialize();
    _engine->_console->deinitialize();

    _engine->_scriptEngine->deinitialize();

    delete _engine;
    _engine = nullptr;
    FactoryManager::deinitialize();
    TransformationManager::deinitialize();
    SpiceManager::deinitialize();

    ghoul::fontrendering::FontRenderer::deinitialize();

    LogManager::deinitialize();

    ghoul::deinitialize();
    LTRACE("OpenSpaceEngine::destroy(end)");
}

void OpenSpaceEngine::initialize() {
    LTRACE("OpenSpaceEngine::initialize(begin)");

    glbinding::Binding::useCurrentContext();
    glbinding::Binding::initialize();

    // clear the screen so the user doesn't have to see old buffer contents from the
    // graphics card
    LDEBUG("Clearing all Windows");
    _windowWrapper->clearAllWindows(glm::vec4(0.f, 0.f, 0.f, 1.f));

    LDEBUG("Adding system components");
    // Detect and log OpenCL and OpenGL versions and available devices
    SysCap.addComponent(
        std::make_unique<ghoul::systemcapabilities::GeneralCapabilitiesComponent>()
    );
    SysCap.addComponent(
        std::make_unique<ghoul::systemcapabilities::OpenGLCapabilitiesComponent>()
    );

    // @BUG:  This will call OpenGL functions, should it should be in the initializeGL
    LDEBUG("Detecting capabilities");
    SysCap.detectCapabilities();

    using Verbosity = ghoul::systemcapabilities::SystemCapabilitiesComponent::Verbosity;
    Verbosity verbosity = Verbosity::Default;
    if (configurationManager().hasKey(ConfigurationManager::KeyCapabilitiesVerbosity)) {
        static const std::map<std::string, Verbosity> VerbosityMap = {
            { "None", Verbosity::None },
            { "Minimal", Verbosity::Minimal },
            { "Default", Verbosity::Default },
            { "Full", Verbosity::Full }
        };

        std::string v = configurationManager().value<std::string>(
            ConfigurationManager::KeyCapabilitiesVerbosity
        );
        ghoul_assert(
            VerbosityMap.find(v) != VerbosityMap.end(),
            "Missing check for syscaps verbosity in openspace.cfg documentation"
        );
        verbosity = VerbosityMap.find(v)->second;
    }
    SysCap.logCapabilities(verbosity);

    // Check the required OpenGL versions of the registered modules
    ghoul::systemcapabilities::Version version =
        _engine->_moduleEngine->requiredOpenGLVersion();
    LINFO(fmt::format("Required OpenGL version: {}", std::to_string(version)));

    if (OpenGLCap.openGLVersion() < version) {
        throw ghoul::RuntimeError(
            "Module required higher OpenGL version than is supported",
            "OpenSpaceEngine"
        );
    }

    _downloadManager = std::make_unique<DownloadManager>();


    // Register Lua script functions
    LDEBUG("Registering Lua libraries");
    registerCoreClasses(*_scriptEngine);

    _scriptEngine->addLibrary(_engine->_assetManager->luaLibrary());

    for (OpenSpaceModule* module : _moduleEngine->modules()) {
        _scriptEngine->addLibrary(module->luaLibrary());

        for (scripting::LuaLibrary& l : module->luaLibraries()) {
            _scriptEngine->addLibrary(l);
        }
    }

    // TODO: Maybe move all scenegraph and renderengine stuff to initializeGL
    scriptEngine().initialize();

    writeStaticDocumentation();

    if (configurationManager().hasKey(ConfigurationManager::KeyShutdownCountdown)) {
        _shutdown.waitTime = static_cast<float>(configurationManager().value<double>(
            ConfigurationManager::KeyShutdownCountdown
        ));
    }

    if (!commandlineArgumentPlaceholders.sceneName.empty()) {
        configurationManager().setValue(
            ConfigurationManager::KeyConfigAsset,
            commandlineArgumentPlaceholders.sceneName
        );
    }

    // Initialize the NavigationHandler
    _navigationHandler->initialize();

    // Load a light and a monospaced font
    loadFonts();


    _renderEngine->initialize();
    _loadingScreen = _engine->createLoadingScreen();
    _loadingScreen->render();

    for (const auto& func : _moduleCallbacks.initialize) {
        func();
    }

    std::string assetPath = "";
    configurationManager().getValue(ConfigurationManager::KeyConfigAsset, assetPath);
    _engine->_assetManager->initialize();
    scheduleLoadSingleAsset(assetPath);

    LTRACE("OpenSpaceEngine::initialize(end)");
}

void OpenSpaceEngine::scheduleLoadSingleAsset(std::string assetPath) {
    _hasScheduledAssetLoading = true;
    _scheduledAssetPathToLoad = assetPath;
}

std::unique_ptr<LoadingScreen> OpenSpaceEngine::createLoadingScreen() {
    bool showMessage = true;
    constexpr const char* kMessage = ConfigurationManager::KeyLoadingScreenShowMessage;
    if (configurationManager().hasKey(kMessage)) {
        showMessage = configurationManager().value<bool>(kMessage);
    }

    bool showNodeNames = true;
    constexpr const char* kNames = ConfigurationManager::KeyLoadingScreenShowNodeNames;

    if (configurationManager().hasKey(kNames)) {
        showNodeNames = configurationManager().value<bool>(kNames);
    }

    bool showProgressbar = true;
    constexpr const char* kProgress =
                                    ConfigurationManager::KeyLoadingScreenShowProgressbar;

    if (configurationManager().hasKey(kProgress)) {
        showProgressbar = configurationManager().value<bool>(kProgress);
    }
    return std::make_unique<LoadingScreen>(
        LoadingScreen::ShowMessage(showMessage),
        LoadingScreen::ShowNodeNames(showNodeNames),
        LoadingScreen::ShowProgressbar(showProgressbar)
    );
}

void OpenSpaceEngine::loadSingleAsset(const std::string& assetPath) {
    LTRACE("OpenSpaceEngine::loadSingleAsset(begin)");

    windowWrapper().setBarrier(false);
    windowWrapper().setSynchronization(false);
    defer {
        windowWrapper().setSynchronization(true);
        windowWrapper().setBarrier(true);
    };

    if (assetPath == "") {
        return;
    }
    if (_scene) {
        _syncEngine->removeSyncables(_timeManager->getSyncables());
        _syncEngine->removeSyncables(_renderEngine->getSyncables());
        _renderEngine->setScene(nullptr);
        _renderEngine->setCamera(nullptr);
        _navigationHandler->setCamera(nullptr);
        _scene->clear();
        _rootPropertyOwner->removePropertySubOwner(_scene.get());
    }

    bool multiThreadedInitialization = configurationManager().hasKeyAndValue<bool>(
        ConfigurationManager::KeyUseMultithreadedInitialization
    ) && configurationManager().value<bool>(
        ConfigurationManager::KeyUseMultithreadedInitialization
    );

    std::unique_ptr<SceneInitializer> sceneInitializer;
    if (multiThreadedInitialization) {
        unsigned int nAvailableThreads = std::thread::hardware_concurrency();
        unsigned int nThreads = nAvailableThreads == 0 ? 2 : nAvailableThreads - 1;
        sceneInitializer = std::make_unique<MultiThreadedSceneInitializer>(nThreads);
    } else {
        sceneInitializer = std::make_unique<SingleThreadedSceneInitializer>();
    }

    _scene = std::make_unique<Scene>(std::move(sceneInitializer));
    _rootPropertyOwner->addPropertySubOwner(_scene.get());
    _scene->setCamera(std::make_unique<Camera>());
    Camera* camera = _scene->camera();
    camera->setParent(_scene->root());

    _renderEngine->setCamera(camera);
    _navigationHandler->setCamera(camera);
    _navigationHandler->setFocusNode(camera->parent());

    _renderEngine->setScene(_scene.get());

    _assetManager->removeAll();
    _assetManager->add(assetPath);

    _loadingScreen->setPhase(LoadingScreen::Phase::Construction);
    _loadingScreen->postMessage("Loading assets");

    _assetManager->update();

    _loadingScreen->setPhase(LoadingScreen::Phase::Synchronization);
    _loadingScreen->postMessage("Synchronizing assets");

    std::vector<std::shared_ptr<Asset>> allAssets =
        _assetManager->rootAsset()->subTreeAssets();

    std::unordered_set<std::shared_ptr<ResourceSynchronization>> resourceSyncs;
    for (const auto& a : allAssets) {
        std::vector<std::shared_ptr<ResourceSynchronization>> syncs =
            a->ownSynchronizations();

        for (const auto& s : syncs) {
            if (s->state() == ResourceSynchronization::State::Syncing) {
                resourceSyncs.insert(s);
                _loadingScreen->updateItem(
                    s->name(),
                    s->name(),
                    LoadingScreen::ItemStatus::Started,
                    s->progress()
                );
            }
        }
    }
    _loadingScreen->setItemNumber(static_cast<int>(resourceSyncs.size()));

    bool loading = true;
    while (loading) {
        _loadingScreen->render();
        _assetManager->update();

        loading = false;
        auto it = resourceSyncs.begin();
        while (it != resourceSyncs.end()) {
            if ((*it)->state() == ResourceSynchronization::State::Syncing) {
                loading = true;
                _loadingScreen->updateItem(
                    (*it)->name(),
                    (*it)->name(),
                    LoadingScreen::ItemStatus::Started,
                    (*it)->progress()
                );
                ++it;
            } else {
                _loadingScreen->tickItem();
                _loadingScreen->updateItem(
                    (*it)->name(),
                    (*it)->name(),
                    LoadingScreen::ItemStatus::Finished,
                    1.0f
                );
                it = resourceSyncs.erase(it);
            }
        }
    }

    _loadingScreen->setPhase(LoadingScreen::Phase::Initialization);

    _loadingScreen->postMessage("Initializing scene");
    while (_scene->isInitializing()) {
        _loadingScreen->render();
    }

    _loadingScreen->postMessage("Initializing OpenGL");
    _loadingScreen->finalize();
    _renderEngine->updateScene();

    _renderEngine->setGlobalBlackOutFactor(0.0);
    _renderEngine->startFading(1, 3.0);

    _syncEngine->addSyncables(_timeManager->getSyncables());
    _syncEngine->addSyncables(_renderEngine->getSyncables());

#ifdef __APPLE__
    showTouchbar();
#endif // APPLE

    runGlobalCustomizationScripts();

    writeSceneDocumentation();

    LTRACE("OpenSpaceEngine::loadSingleAsset(end)");
}

void OpenSpaceEngine::deinitialize() {
    LTRACE("OpenSpaceEngine::deinitialize(begin)");

    for (const auto& func : _engine->_moduleCallbacks.deinitializeGL) {
        func();
    }

    for (const auto& func : _engine->_moduleCallbacks.deinitialize) {
        func();
    }

    _engine->assetManager().deinitialize();
    _engine->_scene = nullptr;

    _navigationHandler->deinitialize();
    _renderEngine->deinitialize();

    LTRACE("OpenSpaceEngine::deinitialize(end)");
}

void OpenSpaceEngine::writeStaticDocumentation() {
    // If a LuaDocumentationFile was specified, generate it now
    if (configurationManager().hasKey(ConfigurationManager::KeyLuaDocumentation)) {
        _scriptEngine->writeDocumentation(
            absPath(configurationManager().value<std::string>(
                ConfigurationManager::KeyLuaDocumentation
            ))
        );
    }

    // If a general documentation was specified, generate it now
    if (configurationManager().hasKey(ConfigurationManager::KeyDocumentation)) {
        DocEng.writeDocumentation(
            absPath(configurationManager().value<std::string>(
                ConfigurationManager::KeyDocumentation
            ))
        );
    }

    // If a factory documentation was specified, generate it now
    if (configurationManager().hasKey(ConfigurationManager::KeyFactoryDocumentation)) {
        FactoryManager::ref().writeDocumentation(
            absPath(configurationManager().value<std::string>(
                ConfigurationManager::KeyFactoryDocumentation
            ))
        );
    }
}

void OpenSpaceEngine::gatherCommandlineArguments() {
    commandlineArgumentPlaceholders.configurationName = "";
    _commandlineParser->addCommand(std::make_unique<SingleCommand<std::string>>(
        commandlineArgumentPlaceholders.configurationName, "--config", "-c",
        "Provides the path to the OpenSpace configuration file"
    ));

    commandlineArgumentPlaceholders.sgctConfigurationName = "";
    _commandlineParser->addCommand(std::make_unique<SingleCommand<std::string>>(
        commandlineArgumentPlaceholders.sgctConfigurationName, "--sgct", "-s",
        "Provides the path to the SGCT configuration file, overriding the value set in "
        "the OpenSpace configuration file"
    ));

    commandlineArgumentPlaceholders.sceneName = "";
    _commandlineParser->addCommand(std::make_unique<SingleCommand<std::string>>(
        commandlineArgumentPlaceholders.sceneName, "--scene", "", "Provides the path to "
        "the scene file, overriding the value set in the OpenSpace configuration file"
    ));

    commandlineArgumentPlaceholders.cacheFolder = "";
    _commandlineParser->addCommand(std::make_unique<SingleCommand<std::string>>(
        commandlineArgumentPlaceholders.cacheFolder, "--cacheDir", "", "Provides the "
        "path to a cache file, overriding the value set in the OpenSpace configuration "
        "file"
    ));
}

void OpenSpaceEngine::runGlobalCustomizationScripts() {
    // @CLEANUP:  Move this into the scene loading?  ---abock
    LINFO("Running Global initialization scripts");
    ghoul::lua::LuaState state;
    OsEng.scriptEngine().initializeLuaState(state);

    std::string k = ConfigurationManager::KeyGlobalCustomizationScripts;
    if (_configurationManager->hasKey(k)) {
        ghoul::Dictionary dict = _configurationManager->value<ghoul::Dictionary>(k);
        for (int i = 1; i <= static_cast<int>(dict.size()); ++i) {
            std::string script = absPath(dict.value<std::string>(std::to_string(i)));

            if (FileSys.fileExists(script)) {
                try {
                    LINFO(fmt::format("Running global customization script: {}", script));
                    ghoul::lua::runScriptFile(state, script);
                } catch (ghoul::RuntimeError& e) {
                    LERRORC(e.component, e.message);
                }
            }
            else {
                LDEBUG(fmt::format("Ignoring non-existing script file: {}", script));
            }
        }
    }
}

void OpenSpaceEngine::loadFonts() {
    ghoul::Dictionary fonts;
    configurationManager().getValue(ConfigurationManager::KeyFonts, fonts);

    _fontManager = std::make_unique<ghoul::fontrendering::FontManager>(FontAtlasSize);

    for (const std::string& key : fonts.keys()) {
        std::string font = absPath(fonts.value<std::string>(key));

        if (!FileSys.fileExists(font)) {
            LERROR(fmt::format("Could not find font '{}'", font));
            continue;
        }

        LDEBUG(fmt::format("Registering font '{}' with key '{}'", font, key));
        bool success = _fontManager->registerFontPath(key, font);

        if (!success) {
            LERROR(fmt::format("Error registering font '{}' with key '{}'", font, key));
        }
    }

    try {
        bool initSuccess = ghoul::fontrendering::FontRenderer::initialize();
        if (!initSuccess) {
            LERROR("Error initializing default font renderer");
        }

        using FR = ghoul::fontrendering::FontRenderer;
        FR::defaultRenderer().setFramebufferSize(_renderEngine->fontResolution());

        FR::defaultProjectionRenderer().setFramebufferSize(
            _renderEngine->renderingResolution()
        );
    }
    catch (const ghoul::RuntimeError& err) {
        LERRORC(err.component, err.message);
    }
}

void OpenSpaceEngine::configureLogging(bool consoleLog) {
    constexpr const char* KeyLogLevel = ConfigurationManager::KeyLoggingLogLevel;
    constexpr const char* KeyLogImmediateFlush =
                                           ConfigurationManager::KeyLoggingImmediateFlush;
    constexpr const char* KeyLogs = ConfigurationManager::KeyLoggingLogs;

    if (configurationManager().hasKeyAndValue<std::string>(KeyLogLevel)) {
        std::string logLevel = "Info";
        configurationManager().getValue(KeyLogLevel, logLevel);

        bool immediateFlush = false;
        configurationManager().getValue(KeyLogImmediateFlush, immediateFlush);

        LogLevel level = ghoul::logging::levelFromString(logLevel);
        LogManager::deinitialize();
        using ImmediateFlush = ghoul::logging::LogManager::ImmediateFlush;
        LogManager::initialize(
            level,
            immediateFlush ? ImmediateFlush::Yes : ImmediateFlush::No
        );
        if (consoleLog) {
            LogMgr.addLog(std::make_unique<ConsoleLog>());
        }
    }

    if (configurationManager().hasKeyAndValue<ghoul::Dictionary>(KeyLogs)) {
        ghoul::Dictionary logs = configurationManager().value<ghoul::Dictionary>(KeyLogs);

        for (size_t i = 1; i <= logs.size(); ++i) {
            ghoul::Dictionary logInfo = logs.value<ghoul::Dictionary>(std::to_string(i));

            try {
                LogMgr.addLog(createLog(logInfo));
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

#ifdef WIN32
    if (IsDebuggerPresent()) {
        LogMgr.addLog(std::make_unique<VisualStudioOutputLog>());
    }
#endif // WIN32

#ifndef GHOUL_LOGGING_ENABLE_TRACE
    std::string logLevel = "Info";
    configurationManager().getValue(KeyLogLevel, logLevel);
    LogLevel level = ghoul::logging::levelFromString(logLevel);

    if (level == ghoul::logging::LogLevel::Trace) {
        LWARNING(
            "Desired logging level is set to 'Trace' but application was " <<
            "compiled without Trace support"
        );
    }
#endif // GHOUL_LOGGING_ENABLE_TRACE
}

void OpenSpaceEngine::writeSceneDocumentation() {
    // Write keyboard documentation.
    if (configurationManager().hasKey(ConfigurationManager::KeyKeyboardShortcuts)) {
        keyBindingManager().writeDocumentation(
            absPath(configurationManager().value<std::string>(
                ConfigurationManager::KeyKeyboardShortcuts
                ))
        );
    }

    if (configurationManager().hasKey(ConfigurationManager::KeySceneLicenseDocumentation))
    {
        _scene->writeSceneLicenseDocumentation(
            absPath(configurationManager().value<std::string>(
                ConfigurationManager::KeySceneLicenseDocumentation
                ))
        );
    }

    // If a PropertyDocumentationFile was specified, generate it now.
    if (configurationManager().hasKey(
            ConfigurationManager::KeyScenePropertyDocumentation
        ))
    {
        _scene->writeDocumentation(
            absPath(configurationManager().value<std::string>(
                ConfigurationManager::KeyScenePropertyDocumentation
                ))
        );
    }

    if (configurationManager().hasKey(ConfigurationManager::KeyPropertyDocumentation)) {
        _rootPropertyOwner->writeDocumentation(
            absPath(configurationManager().value<std::string>(
                ConfigurationManager::KeyPropertyDocumentation
                ))
        );
    }
}

void OpenSpaceEngine::initializeGL() {
    LTRACE("OpenSpaceEngine::initializeGL(begin)");

    LTRACE("OpenSpaceEngine::initializeGL::Console::initialize(begin)");
    try {
        _engine->_console->initialize();
    }
    catch (ghoul::RuntimeError& e) {
        LERROR("Error initializing Console with error:");
        LERRORC(e.component, e.message);
    }
    LTRACE("OpenSpaceEngine::initializeGL::Console::initialize(end)");

    if (_configurationManager->hasKey(ConfigurationManager::KeyOpenGLDebugContext)) {
        LTRACE("OpenSpaceEngine::initializeGL::DebugContext(begin)");
        ghoul::Dictionary dict = _configurationManager->value<ghoul::Dictionary>(
            ConfigurationManager::KeyOpenGLDebugContext
        );
        bool debug = dict.value<bool>(ConfigurationManager::PartActivate);

        // Debug output is not available before 4.3
        const ghoul::systemcapabilities::Version minVersion = { 4, 3, 0 };
        if (OpenGLCap.openGLVersion() < minVersion) {
            LINFO("OpenGL Debug context requested, but insufficient version available");
            debug = false;
        }

        if (debug) {
            using namespace ghoul::opengl::debug;

            bool synchronous = true;
            if (dict.hasKey(ConfigurationManager::PartSynchronous)) {
                synchronous = dict.value<bool>(ConfigurationManager::PartSynchronous);
            }

            setDebugOutput(DebugOutput(debug), SynchronousOutput(synchronous));

            if (dict.hasKey(ConfigurationManager::PartFilterIdentifier)) {
                ghoul::Dictionary filterDict = dict.value<ghoul::Dictionary>(
                    ConfigurationManager::PartFilterIdentifier
                );

                for (size_t i = 1; i <= filterDict.size(); ++i) {
                    ghoul::Dictionary id = filterDict.value<ghoul::Dictionary>(
                        std::to_string(i)
                    );

                    const unsigned int identifier = static_cast<unsigned int>(
                        id.value<double>(
                            ConfigurationManager::PartFilterIdentifierIdentifier
                        )
                    );

                    const std::string s = id.value<std::string>(
                        ConfigurationManager::PartFilterIdentifierSource
                    );

                    const std::string t = id.value<std::string>(
                        ConfigurationManager::PartFilterIdentifierType
                    );

                    setDebugMessageControl(
                        ghoul::from_string<Source>(s),
                        ghoul::from_string<Type>(t),
                        { identifier },
                        Enabled::No
                    );
                }
            }

            if (dict.hasKey(ConfigurationManager::PartFilterSeverity)) {
                ghoul::Dictionary filterDict = dict.value<ghoul::Dictionary>(
                    ConfigurationManager::PartFilterIdentifier
                );

                for (size_t i = 1; i <= filterDict.size(); ++i) {
                    std::string severity = filterDict.value<std::string>(
                        std::to_string(i)
                    );

                    setDebugMessageControl(
                        Source::DontCare,
                        Type::DontCare,
                        ghoul::from_string<Severity>(severity),
                        Enabled::No
                    );
                }
            }

            auto callback = [](Source source, Type type, Severity severity,
                unsigned int id, std::string message) -> void
            {
                const std::string s = std::to_string(source);
                const std::string t = std::to_string(type);

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
            };
            ghoul::opengl::debug::setDebugCallback(callback);
        }
        LTRACE("OpenSpaceEngine::initializeGL::DebugContext(end)");
    }

    // The ordering of the KeyCheckOpenGLState and KeyLogEachOpenGLCall are important as
    // the callback mask in glbinding is stateful for each context, and since
    // KeyLogEachOpenGLCall is more specific, we want it to be able to overwrite the
    // state from KeyCheckOpenGLState
    if (_configurationManager->hasKey(ConfigurationManager::KeyCheckOpenGLState)) {
        const bool val = _configurationManager->value<bool>(
            ConfigurationManager::KeyCheckOpenGLState
        );

        if (val) {
            using namespace glbinding;
            setCallbackMaskExcept(CallbackMask::After, { "glGetError" });
            setAfterCallback([](const FunctionCall& f) {
                const GLenum error = glGetError();
                switch (error) {
                    case GL_NO_ERROR:
                        break;
                    case GL_INVALID_ENUM:
                        LERRORC(
                            "OpenGL Invalid State",
                            fmt::format("Function {}: GL_INVALID_ENUM", f.toString())
                        );
                        break;
                    case GL_INVALID_VALUE:
                        LERRORC(
                            "OpenGL Invalid State",
                            fmt::format("Function {}: GL_INVALID_VALUE", f.toString())
                        );
                        break;
                    case GL_INVALID_OPERATION:
                        LERRORC(
                            "OpenGL Invalid State",
                            fmt::format("Function {}: GL_INVALID_OPERATION", f.toString())
                        );
                        break;
                    case GL_INVALID_FRAMEBUFFER_OPERATION:
                        LERRORC(
                            "OpenGL Invalid State",
                            fmt::format(
                                "Function {}: GL_INVALID_FRAMEBUFFER_OPERATION",
                                f.toString()
                            )
                        );
                        break;
                    case GL_OUT_OF_MEMORY:
                        LERRORC(
                            "OpenGL Invalid State",
                            fmt::format("Function {}: GL_OUT_OF_MEMORY", f.toString())
                        );
                        break;
                    default:
                        LERRORC(
                            "OpenGL Invalid State",
                            fmt::format("Unknown error code: {0:x}", error)
                        );
                }
            });
        }
    }

    if (_configurationManager->hasKey(ConfigurationManager::KeyLogEachOpenGLCall)) {
        const bool val = _configurationManager->value<bool>(
            ConfigurationManager::KeyLogEachOpenGLCall
        );

        if (val) {
            using namespace glbinding;
            setCallbackMask(CallbackMask::After | CallbackMask::ParametersAndReturnValue);
            glbinding::setAfterCallback([](const glbinding::FunctionCall& call) {
                std::string arguments = std::accumulate(
                    call.parameters.begin(),
                    call.parameters.end(),
                    std::string("("),
                    [](std::string a, AbstractValue* v) {
                        return a + ", " + v->asString();
                    }
                );

                std::string returnValue = call.returnValue ?
                    " -> " + call.returnValue->asString() :
                    "";

                LTRACEC(
                    "OpenGL",
                    call.function->name() + arguments + returnValue
                );
            });
        }
    }

    LDEBUG("Initializing Rendering Engine");
    _renderEngine->initializeGL();

    _moduleEngine->initializeGL();

    for (const auto& func : _moduleCallbacks.initializeGL) {
        func();
    }

    LINFO("Finished initializing OpenGL");

    LTRACE("OpenSpaceEngine::initializeGL(end)");
}

void OpenSpaceEngine::preSynchronization() {
    LTRACE("OpenSpaceEngine::preSynchronization(begin)");

    std::unique_ptr<performance::PerformanceMeasurement> perf;
    if (OsEng.renderEngine().performanceManager()) {
        perf = std::make_unique<performance::PerformanceMeasurement>(
            "OpenSpaceEngine::preSynchronization",
            OsEng.renderEngine().performanceManager()
            );
    }

    FileSys.triggerFilesystemEvents();

    if (_hasScheduledAssetLoading) {
        LINFO(fmt::format("Loading asset: {}", _scheduledAssetPathToLoad));
        loadSingleAsset(_scheduledAssetPathToLoad);
        _hasScheduledAssetLoading = false;
        _scheduledAssetPathToLoad = "";
    }

    if (_isFirstRenderingFirstFrame) {
        _windowWrapper->setSynchronization(false);
    }

    bool master = _windowWrapper->isMaster();

    _syncEngine->preSynchronization(SyncEngine::IsMaster(master));
    if (master) {
        double dt = _windowWrapper->averageDeltaTime();
        _timeManager->preSynchronization(dt);

        using Iter = std::vector<std::string>::const_iterator;
        std::pair<Iter, Iter> scheduledScripts = _scriptScheduler->progressTo(
            timeManager().time().j2000Seconds()
        );
        for (Iter it = scheduledScripts.first; it != scheduledScripts.second; ++it) {
            _scriptEngine->queueScript(
                *it, ScriptEngine::RemoteScripting::Yes
            );
        }

        _renderEngine->updateScene();
        _navigationHandler->updateCamera(dt);

        Camera* camera = _renderEngine->camera();
        if (camera) {
            _navigationHandler->updateCamera(dt);
            _renderEngine->camera()->invalidateCache();
        }
        _parallelPeer->preSynchronization();
    }

    for (const auto& func : _moduleCallbacks.preSync) {
        func();
    }
    LTRACE("OpenSpaceEngine::preSynchronization(end)");
}

void OpenSpaceEngine::postSynchronizationPreDraw() {
    LTRACE("OpenSpaceEngine::postSynchronizationPreDraw(begin)");

    std::unique_ptr<performance::PerformanceMeasurement> perf;
    if (OsEng.renderEngine().performanceManager()) {
        perf = std::make_unique<performance::PerformanceMeasurement>(
            "OpenSpaceEngine::postSynchronizationPreDraw",
            OsEng.renderEngine().performanceManager()
        );
    }

    bool master = _windowWrapper->isMaster();
    _syncEngine->postSynchronization(SyncEngine::IsMaster(master));

    if (_shutdown.inShutdown) {
        if (_shutdown.timer <= 0.f) {
            _windowWrapper->terminate();
        }
        _shutdown.timer -= static_cast<float>(_windowWrapper->averageDeltaTime());
    }


    const bool updated = _assetManager->update();
    if (updated) {
        writeSceneDocumentation();
    }

    _renderEngine->updateScene();
    _renderEngine->updateFade();
    _renderEngine->updateRenderer();
    _renderEngine->updateScreenSpaceRenderables();
    _renderEngine->updateShaderPrograms();

    if (!master) {
        _renderEngine->camera()->invalidateCache();
    }

    for (const auto& func : _moduleCallbacks.postSyncPreDraw) {
        func();
    }

    // Testing this every frame has minimal impact on the performance --- abock
    // Debug build: 1-2 us ; Release build: <= 1 us
    using ghoul::logging::LogManager;
    int warningCounter = LogMgr.messageCounter(LogLevel::Warning);
    int errorCounter = LogMgr.messageCounter(LogLevel::Error);
    int fatalCounter = LogMgr.messageCounter(LogLevel::Fatal);

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

void OpenSpaceEngine::render(const glm::mat4& sceneMatrix,
                             const glm::mat4& viewMatrix,
                             const glm::mat4& projectionMatrix)
{
    LTRACE("OpenSpaceEngine::render(begin)");

    std::unique_ptr<performance::PerformanceMeasurement> perf;
    if (OsEng.renderEngine().performanceManager()) {
        perf = std::make_unique<performance::PerformanceMeasurement>(
            "OpenSpaceEngine::render",
            OsEng.renderEngine().performanceManager()
        );
    }

    const bool isGuiWindow =
        _windowWrapper->hasGuiWindow() ? _windowWrapper->isGuiWindow() : true;
    if (isGuiWindow) {
        _console->update();
    }

    _renderEngine->render(sceneMatrix, viewMatrix, projectionMatrix);

    for (const auto& func : _moduleCallbacks.render) {
        func();
    }

    LTRACE("OpenSpaceEngine::render(end)");
}

void OpenSpaceEngine::drawOverlays() {
    LTRACE("OpenSpaceEngine::drawOverlays(begin)");

    std::unique_ptr<performance::PerformanceMeasurement> perf;
    if (OsEng.renderEngine().performanceManager()) {
        perf = std::make_unique<performance::PerformanceMeasurement>(
            "OpenSpaceEngine::drawOverlays",
            OsEng.renderEngine().performanceManager()
        );
    }

    const bool isGuiWindow =
        _windowWrapper->hasGuiWindow() ? _windowWrapper->isGuiWindow() : true;

    if (isGuiWindow) {
        _renderEngine->renderOverlays(_shutdown);
        _console->render();
    }

    for (const auto& func : _moduleCallbacks.draw2D) {
        func();
    }

    LTRACE("OpenSpaceEngine::drawOverlays(end)");
}

void OpenSpaceEngine::postDraw() {
    LTRACE("OpenSpaceEngine::postDraw(begin)");

    std::unique_ptr<performance::PerformanceMeasurement> perf;
    if (OsEng.renderEngine().performanceManager()) {
        perf = std::make_unique<performance::PerformanceMeasurement>(
            "OpenSpaceEngine::postDraw",
            OsEng.renderEngine().performanceManager()
        );
    }

    _renderEngine->postDraw();

    for (const auto& func : _moduleCallbacks.postDraw) {
        func();
    }

    if (_isFirstRenderingFirstFrame) {
        _windowWrapper->setSynchronization(true);
        _isFirstRenderingFirstFrame = false;
    }


    LTRACE("OpenSpaceEngine::postDraw(end)");
}

void OpenSpaceEngine::keyboardCallback(Key key, KeyModifier mod, KeyAction action) {
    for (const auto& func : _moduleCallbacks.keyboard) {
        const bool consumed = func(key, mod, action);
        if (consumed) {
            return;
        }
    }

    const bool consoleConsumed = _console->keyboardCallback(key, mod, action);
    if (consoleConsumed) {
        return;
    }

    _navigationHandler->keyboardCallback(key, mod, action);
    _keyBindingManager->keyboardCallback(key, mod, action);
}

void OpenSpaceEngine::charCallback(unsigned int codepoint, KeyModifier modifier) {
    for (const auto& func : _moduleCallbacks.character) {
        bool consumed = func(codepoint, modifier);
        if (consumed) {
            return;
        }
    }

    _console->charCallback(codepoint, modifier);
}

void OpenSpaceEngine::mouseButtonCallback(MouseButton button, MouseAction action) {
    for (const auto& func : _moduleCallbacks.mouseButton) {
        bool consumed = func(button, action);
        if (consumed) {
            return;
        }
    }

    _navigationHandler->mouseButtonCallback(button, action);
}

void OpenSpaceEngine::mousePositionCallback(double x, double y) {
    for (const auto& func : _moduleCallbacks.mousePosition) {
        func(x, y);
    }

    _navigationHandler->mousePositionCallback(x, y);
}

void OpenSpaceEngine::mouseScrollWheelCallback(double posX, double posY) {
    for (const auto& func : _moduleCallbacks.mouseScrollWheel) {
        bool consumed = func(posX, posY);
        if (consumed) {
            return;
        }
    }

    _navigationHandler->mouseScrollWheelCallback(posY);
}

void OpenSpaceEngine::encode() {
    _syncEngine->encodeSyncables();

    _networkEngine->publishStatusMessage();
    _networkEngine->sendMessages();
}

void OpenSpaceEngine::decode() {
    _syncEngine->decodeSyncables();
}

void OpenSpaceEngine::externalControlCallback(const char* receivedChars, int size,
                                              int /*clientId*/)
{
    if (size == 0) {
        return;
    }

    _networkEngine->handleMessage(std::string(receivedChars, size));
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
            }
        },
        {
            absPath("${SCRIPTS}/core_scripts.lua")
        }
    };
}

// Registers a callback for a specific CallbackOption
void OpenSpaceEngine::registerModuleCallback(OpenSpaceEngine::CallbackOption option,
                                             std::function<void()> function)
{
    switch (option) {
        case CallbackOption::Initialize:
            _moduleCallbacks.initialize.push_back(std::move(function));
            break;
        case CallbackOption::Deinitialize:
            _moduleCallbacks.deinitialize.push_back(std::move(function));
            break;
        case CallbackOption::InitializeGL:
            _moduleCallbacks.initializeGL.push_back(std::move(function));
            break;
        case CallbackOption::DeinitializeGL:
            _moduleCallbacks.deinitializeGL.push_back(std::move(function));
            break;
        case CallbackOption::PreSync:
            _moduleCallbacks.preSync.push_back(std::move(function));
            break;
        case CallbackOption::PostSyncPreDraw:
            _moduleCallbacks.postSyncPreDraw.push_back(std::move(function));
            break;
        case CallbackOption::Render:
            _moduleCallbacks.render.push_back(std::move(function));
            break;
        case CallbackOption::Draw2D:
            _moduleCallbacks.draw2D.push_back(std::move(function));
            break;
        case CallbackOption::PostDraw:
            _moduleCallbacks.postDraw.push_back(std::move(function));
            break;
        default:
            throw ghoul::MissingCaseException();
    }
}

void OpenSpaceEngine::registerModuleKeyboardCallback(
                               std::function<bool (Key, KeyModifier, KeyAction)> function)
{
    _moduleCallbacks.keyboard.push_back(std::move(function));
}

void OpenSpaceEngine::registerModuleCharCallback(
                                 std::function<bool (unsigned int, KeyModifier)> function)
{
    _moduleCallbacks.character.push_back(std::move(function));
}

void OpenSpaceEngine::registerModuleMouseButtonCallback(
                                  std::function<bool (MouseButton, MouseAction)> function)
{
    _moduleCallbacks.mouseButton.push_back(std::move(function));
}

void OpenSpaceEngine::registerModuleMousePositionCallback(
                                            std::function<void (double, double)> function)
{
    _moduleCallbacks.mousePosition.push_back(std::move(function));
}

void OpenSpaceEngine::registerModuleMouseScrollWheelCallback(
                                            std::function<bool (double, double)> function)
{
    _moduleCallbacks.mouseScrollWheel.push_back(std::move(function));
}

ConfigurationManager& OpenSpaceEngine::configurationManager() {
    ghoul_assert(_configurationManager, "ConfigurationManager must not be nullptr");
    return *_configurationManager;
}

LuaConsole& OpenSpaceEngine::console() {
    ghoul_assert(_console, "LuaConsole must not be nullptr");
    return *_console;
}

Dashboard& OpenSpaceEngine::dashboard() {
    ghoul_assert(_dashboard, "Dashboard must not be nullptr");
    return *_dashboard;
}

DownloadManager& OpenSpaceEngine::downloadManager() {
    ghoul_assert(_downloadManager, "Download Manager must not be nullptr");
    return *_downloadManager;
}

NetworkEngine& OpenSpaceEngine::networkEngine() {
    ghoul_assert(_networkEngine, "NetworkEngine must not be nullptr");
    return *_networkEngine;
}

ModuleEngine& OpenSpaceEngine::moduleEngine() {
    ghoul_assert(_moduleEngine, "ModuleEngine must not be nullptr");
    return *_moduleEngine;
}

ParallelPeer& OpenSpaceEngine::parallelPeer() {
    ghoul_assert(_parallelPeer, "ParallelPeer must not be nullptr");
    return *_parallelPeer;
}

RenderEngine& OpenSpaceEngine::renderEngine() {
    ghoul_assert(_renderEngine, "RenderEngine must not be nullptr");
    return *_renderEngine;
}

TimeManager& OpenSpaceEngine::timeManager() {
    ghoul_assert(_timeManager, "Download Manager must not be nullptr");
    return *_timeManager;
}

LoadingScreen& OpenSpaceEngine::loadingScreen() {
    ghoul_assert(_loadingScreen, "Loading Screen must not be nullptr");
    return *_loadingScreen;
}

WindowWrapper& OpenSpaceEngine::windowWrapper() {
    ghoul_assert(_windowWrapper, "Window Wrapper must not be nullptr");
    return *_windowWrapper;
}

AssetManager& OpenSpaceEngine::assetManager() {
    ghoul_assert(_assetManager, "Asset Manager must not be nullptr");
    return *_assetManager;
}

ghoul::fontrendering::FontManager& OpenSpaceEngine::fontManager() {
    ghoul_assert(_fontManager, "Font Manager must not be nullptr");
    return *_fontManager;
}

interaction::NavigationHandler& OpenSpaceEngine::navigationHandler() {
    ghoul_assert(_navigationHandler, "NavigationHandler must not be nullptr");
    return *_navigationHandler;
}

interaction::KeyBindingManager& OpenSpaceEngine::keyBindingManager() {
    ghoul_assert(_keyBindingManager, "KeyBindingManager must not be nullptr");
    return *_keyBindingManager;
}

properties::PropertyOwner& OpenSpaceEngine::rootPropertyOwner() {
    ghoul_assert(
                 _rootPropertyOwner,
                 "Root Property Namespace must not be nullptr"
                 );
    return *_rootPropertyOwner;
}

VirtualPropertyManager& OpenSpaceEngine::virtualPropertyManager() {
    ghoul_assert(
        _virtualPropertyManager,
        "Virtual Property Manager must not be nullptr"
    );

    return *_virtualPropertyManager;
}

ScriptEngine& OpenSpaceEngine::scriptEngine() {
    ghoul_assert(_scriptEngine, "ScriptEngine must not be nullptr");
    return *_scriptEngine;
}

ScriptScheduler& OpenSpaceEngine::scriptScheduler() {
    ghoul_assert(_scriptScheduler, "ScriptScheduler must not be nullptr");
    return *_scriptScheduler;
}

}  // namespace openspace
