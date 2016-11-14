/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
#include <openspace/engine/settingsengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/keyboardcontroller.h>
#include <openspace/interaction/luaconsole.h>
#include <openspace/interaction/mousecontroller.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/network/networkengine.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/scene/translation.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/syncbuffer.h>
#include <openspace/util/transformationmanager.h>

#include <ghoul/ghoul.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/onscopeexit.h>
#include <ghoul/systemcapabilities/systemcapabilities>

#include <fstream>
#include <queue>

#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
#include <modules/onscreengui/include/gui.h>
#endif

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
#include <modules/iswa/rendering/iswagroup.h>
#include <modules/iswa/util/iswamanager.h>
#endif

#if defined(_MSC_VER) && defined(OPENSPACE_ENABLE_VLD)
#include <vld.h>
#endif

#ifdef WIN32
#include <WinBase.h>
#endif

#include "openspaceengine_lua.inl"

using namespace openspace::scripting;
using namespace ghoul::filesystem;
using namespace ghoul::logging;
using namespace ghoul::cmdparser;

namespace {
    const std::string _loggerCat = "OpenSpaceEngine";
    const std::string _sgctDefaultConfigFile = "${SGCT}/single.xml";
    const std::string _defaultCacheLocation = "${BASE_PATH}/cache";
    
    const std::string _sgctConfigArgumentCommand = "-config";
    
    const std::string PreInitializeFunction = "preInitialization";
    const std::string PostInitializationFunction = "postInitialization";

    const int CacheVersion = 1;
    const int DownloadVersion = 1;
    
    struct {
        std::string configurationName;
        std::string sgctConfigurationName;
        std::string sceneName;
        std::string cacheFolder;
    } commandlineArgumentPlaceholders;
}

namespace openspace {

namespace properties {
    class Property;
}
    
OpenSpaceEngine* OpenSpaceEngine::_engine = nullptr;

OpenSpaceEngine::OpenSpaceEngine(std::string programName,
                                 std::unique_ptr<WindowWrapper> windowWrapper)
    : _configurationManager(new ConfigurationManager)
    , _interactionHandler(new interaction::InteractionHandler)
    , _renderEngine(new RenderEngine)
    , _scriptEngine(new scripting::ScriptEngine)
    , _scriptScheduler(new scripting::ScriptScheduler)
    , _networkEngine(new NetworkEngine)
    , _syncEngine(std::make_unique<SyncEngine>(new SyncBuffer(4096)))
    , _commandlineParser(new ghoul::cmdparser::CommandlineParser(
        programName, ghoul::cmdparser::CommandlineParser::AllowUnknownCommands::Yes
      ))
    , _console(new LuaConsole)
    , _moduleEngine(new ModuleEngine)
    , _settingsEngine(new SettingsEngine)
    , _timeManager(new TimeManager)
    , _downloadManager(nullptr)
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
    , _gui(new gui::GUI)
#endif
    , _parallelConnection(new ParallelConnection)
    , _windowWrapper(std::move(windowWrapper))
    , _globalPropertyNamespace(new properties::PropertyOwner)
    , _isMaster(false)
    , _runTime(0.0)
    , _isInShutdownMode(false)
    , _shutdownCountdown(0.f)
    , _shutdownWait(0.f)
    , _isFirstRenderingFirstFrame(true)
{
    _interactionHandler->setPropertyOwner(_globalPropertyNamespace.get());
    _globalPropertyNamespace->addPropertySubOwner(_interactionHandler.get());
    _globalPropertyNamespace->addPropertySubOwner(_settingsEngine.get());
    _globalPropertyNamespace->addPropertySubOwner(_renderEngine.get());

    FactoryManager::initialize();
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Renderable>>(),
        "Renderable"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Translation>>(),
        "Translation"
    );
    SpiceManager::initialize();
    Time::initialize();
    TransformationManager::initialize();
}

OpenSpaceEngine::~OpenSpaceEngine() {
    LINFO("_windowWrapper->isUsingSwapGroups(): " << _windowWrapper->isUsingSwapGroups());
    LINFO("_windowWrapper->isSwapGroupMaster(): " << _windowWrapper->isSwapGroupMaster());
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
    _gui->deinitializeGL();
#endif
    _interactionHandler->deinitialize();
    _renderEngine->deinitialize();

    _globalPropertyNamespace = nullptr;
    _windowWrapper = nullptr;
    _parallelConnection = nullptr;
    _configurationManager = nullptr;
    _interactionHandler = nullptr;
    _renderEngine = nullptr;
    _scriptEngine = nullptr;
    _networkEngine = nullptr;
    _syncEngine = nullptr;
    _commandlineParser = nullptr;
    _console = nullptr;
    _moduleEngine = nullptr;
    _settingsEngine = nullptr;
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
    _gui = nullptr;
#endif
}

OpenSpaceEngine& OpenSpaceEngine::ref() {
    ghoul_assert(_engine, "OpenSpaceEngine not created");
    return *_engine;
}

bool OpenSpaceEngine::create(int argc, char** argv,
                             std::unique_ptr<WindowWrapper> windowWrapper,
                             std::vector<std::string>& sgctArguments)
{
    ghoul_assert(!_engine, "OpenSpaceEngine was already created");
    ghoul_assert(windowWrapper != nullptr, "No Window Wrapper was provided");
    
    ghoul::initialize();

    // Initialize the LogManager and add the console log as this will be used every time
    // and we need a fall back if something goes wrong between here and when we add the
    // logs from the configuration file. If the user requested as specific loglevel in the
    // configuration file, we will deinitialize this LogManager and reinitialize it later
    // with the correct LogLevel
    LogManager::initialize(
        LogManager::LogLevel::Debug,
        ghoul::logging::LogManager::ImmediateFlush::Yes
    );
    LogMgr.addLog(std::make_unique<ConsoleLog>());

    LDEBUG("Initialize FileSystem");

#ifdef __APPLE__
    ghoul::filesystem::File app(argv[0]);
    std::string dirName = app.directoryName();
    LINFO("Setting starting directory to '" << dirName << "'");
    FileSys.setCurrentDirectory(dirName);
#endif

    // Sanity check of values
    if (argc < 1 || argv == nullptr) {
        LFATAL("No arguments were passed to this function");
        return false;
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
        return false;
    }
    sgctArguments = *arguments;

    // Find configuration
    std::string configurationFilePath = commandlineArgumentPlaceholders.configurationName;
    if (configurationFilePath.empty()) {
        LDEBUG("Finding configuration");
        try {
            configurationFilePath =
                ConfigurationManager::findConfiguration(configurationFilePath);
        }
        catch (const ghoul::RuntimeError& e) {
            LFATALC(e.component, e.message);
        }
    }
    configurationFilePath = absPath(configurationFilePath);
    LINFO("Configuration Path: '" << configurationFilePath << "'");

    // Loading configuration from disk
    LDEBUG("Loading configuration from disk");
    try {
        _engine->configurationManager().loadFromFile(configurationFilePath);
    }
    catch (const ghoul::RuntimeError& e) {
        LFATAL("Loading of configuration file '" << configurationFilePath << "' failed");
        LFATALC(e.component, e.message);
        return false;
    }

    bool hasCacheCommandline = !commandlineArgumentPlaceholders.cacheFolder.empty();
    bool hasCacheConfiguration = _engine->configurationManager().hasKeyAndValue<bool>(
        ConfigurationManager::KeyPerSceneCache
    );
    std::string cacheFolder = absPath("${CACHE}");
    if (hasCacheCommandline) {
        cacheFolder = commandlineArgumentPlaceholders.cacheFolder;
        //FileSys.registerPathToken(
        //    "${CACHE}",
        //    commandlineArgumentPlaceholders.cacheFolder,
        //    ghoul::filesystem::FileSystem::Override::Yes
        //);
    }
    if (hasCacheConfiguration) {
        std::string scene = _engine->configurationManager().value<std::string>(
            ConfigurationManager::KeyConfigScene
        );
        cacheFolder += "-" + ghoul::filesystem::File(scene).baseName();
    }

    if (hasCacheCommandline || hasCacheConfiguration) {
        LINFO("Old cache: " << absPath("${CACHE}"));
        LINFO("New cache: " << cacheFolder);
        FileSys.registerPathToken(
            "${CACHE}",
            cacheFolder,
            ghoul::filesystem::FileSystem::Override::Yes
        );
    }

    // Initialize the requested logs from the configuration file
    _engine->configureLogging();

    LINFOC("OpenSpace Version", 
        OPENSPACE_VERSION_MAJOR << "." <<
        OPENSPACE_VERSION_MINOR << "." <<
        OPENSPACE_VERSION_PATCH << " (" << OPENSPACE_VERSION_STRING << ")");

    // Create directories that doesn't exist
    auto tokens = FileSys.tokens();
    for (const std::string& token : tokens) {
        if (!FileSys.directoryExists(token)) {
            std::string p = absPath(token);
            LDEBUG("Directory '" << p << "' does not exist, creating.");
            FileSys.createDirectory(p, ghoul::filesystem::FileSystem::Recursive::Yes);
        }
    }

    // Register modules
    _engine->_moduleEngine->initialize();

    registerCoreClasses(DocEng);
    // After registering the modules, the documentations for the available classes
    // can be added as well
    for (OpenSpaceModule* m : _engine->_moduleEngine->modules()) {
        for (auto&& doc : m->documentations()) {
            DocEng.addDocumentation(doc);
        }
    }

    // Create the cachemanager
    FileSys.createCacheManager(
        absPath("${" + ConfigurationManager::KeyCache + "}"), CacheVersion
    );
    _engine->_console->initialize();

    // Register the provided shader directories
    ghoul::opengl::ShaderPreprocessor::addIncludePath(absPath("${SHADERS}"));

    // Determining SGCT configuration file
    LDEBUG("Determining SGCT configuration file");
    std::string sgctConfigurationPath = _sgctDefaultConfigFile;
    _engine->configurationManager().getValue(
        ConfigurationManager::KeyConfigSgct, sgctConfigurationPath);

    if (!commandlineArgumentPlaceholders.sgctConfigurationName.empty()) {
        LDEBUG("Overwriting SGCT configuration file with commandline argument: " <<
            commandlineArgumentPlaceholders.sgctConfigurationName);
        sgctConfigurationPath = commandlineArgumentPlaceholders.sgctConfigurationName;
    }

    // Prepend the outgoing sgctArguments with the program name
    // as well as the configuration file that sgct is supposed to use
    sgctArguments.insert(sgctArguments.begin(), argv[0]);
    sgctArguments.insert(sgctArguments.begin() + 1, _sgctConfigArgumentCommand);
    sgctArguments.insert(sgctArguments.begin() + 2, absPath(sgctConfigurationPath));

    return true;
}

void OpenSpaceEngine::destroy() {
    _engine->_moduleEngine->deinitialize();
    _engine->_console->deinitialize();

    _engine->_scriptEngine->deinitialize();
    delete _engine;
    FactoryManager::deinitialize();
    Time::deinitialize();
    SpiceManager::deinitialize();

    ghoul::fontrendering::FontRenderer::deinitialize();

    LogManager::deinitialize();

    ghoul::deinitialize();
}

bool OpenSpaceEngine::initialize() {
    // clear the screen so the user don't have to see old buffer contents from the
    // graphics card
    clearAllWindows();

    // Detect and log OpenCL and OpenGL versions and available devices
    SysCap.addComponent(
        std::make_unique<ghoul::systemcapabilities::GeneralCapabilitiesComponent>()
    );
    SysCap.addComponent(
        std::make_unique<ghoul::systemcapabilities::OpenGLCapabilitiesComponent>()
    );
    SysCap.detectCapabilities();

    using Verbosity = ghoul::systemcapabilities::SystemCapabilitiesComponent::Verbosity;
    Verbosity verbosity = Verbosity::Default;
    if (configurationManager().hasKeyAndValue<std::string>(

        ConfigurationManager::KeyCapabilitiesVerbosity)) {
        std::map<std::string, Verbosity> verbosityMap = {
            { "None", Verbosity::None },
            { "Minimal", Verbosity::Minimal },
            { "Default", Verbosity::Default },
            { "Full", Verbosity::Full }
        };

        std::string v = configurationManager().value<std::string>(ConfigurationManager::KeyCapabilitiesVerbosity);
        if (verbosityMap.find(v) != verbosityMap.end())
            verbosity = verbosityMap[v];
    }
    SysCap.logCapabilities(verbosity);

    // Check the required OpenGL versions of the registered modules
    ghoul::systemcapabilities::OpenGLCapabilitiesComponent::Version version =
        _engine->_moduleEngine->requiredOpenGLVersion();
    LINFO("Required OpenGL version: " << version.toString());

    if (OpenGLCap.openGLVersion() < version) {
        LFATAL("Module required higher OpenGL version than is supported");
        return false;
    }

    std::string requestURL = "";
    bool success = configurationManager().getValue(ConfigurationManager::KeyDownloadRequestURL, requestURL);
    if (success) {
        _downloadManager = std::make_unique<DownloadManager>(requestURL, DownloadVersion);
    }

    // Register Lua script functions
    LDEBUG("Registering Lua libraries");
    registerCoreClasses(*_scriptEngine);

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    _scriptEngine->addLibrary(IswaManager::luaLibrary());
#endif

    // TODO: Maybe move all scenegraph and renderengine stuff to initializeGL
    scriptEngine().initialize();

    writeDocumentation();

    bool disableMasterRendering = false;
    configurationManager().getValue(
        ConfigurationManager::KeyDisableMasterRendering, disableMasterRendering);
    _renderEngine->setDisableRenderingOnMaster(disableMasterRendering);

    configurationManager().getValue(
        ConfigurationManager::KeyShutdownCountdown, _shutdownWait
    );

    if (!commandlineArgumentPlaceholders.sceneName.empty())
        configurationManager().setValue(
            ConfigurationManager::KeyConfigScene,
            commandlineArgumentPlaceholders.sceneName);

    // Initialize the SettingsEngine
    _settingsEngine->initialize();
    _settingsEngine->setModules(_moduleEngine->modules());

    // Initialize the InteractionHandler
    _interactionHandler->initialize();

    // Load a light and a monospaced font
    loadFonts();

    // Initialize the Scene
    Scene* sceneGraph = new Scene;
    sceneGraph->initialize();
    
    std::string scenePath = "";
    configurationManager().getValue(ConfigurationManager::KeyConfigScene, scenePath);
    sceneGraph->scheduleLoadSceneFile(scenePath);

    // Initialize the RenderEngine
    _renderEngine->setSceneGraph(sceneGraph);
    _renderEngine->initialize();
    _renderEngine->setGlobalBlackOutFactor(0.0);
    _renderEngine->startFading(1, 3.0);

    // Run start up scripts
    try {
        runPreInitializationScripts(scenePath);
    }
    catch (const ghoul::RuntimeError& e) {
        LFATALC(e.component, e.message);
    }

#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
    LINFO("Initializing GUI");
    _gui->initialize();
    _gui->_globalProperty.setSource(
            [&]() {
            std::vector<properties::PropertyOwner*> res = {
                _settingsEngine.get(),
                _interactionHandler.get(),
                _renderEngine.get()
            };
            return res;
        }
    );

    OsEng.gui()._screenSpaceProperty.setSource(
        [&]() {
            const auto& ssr = renderEngine().screenSpaceRenderables();
            return std::vector<properties::PropertyOwner*>(ssr.begin(), ssr.end());
        }
    );

    OsEng.gui()._property.setSource(
        [&]() {
            const auto& nodes = renderEngine().scene()->allSceneGraphNodes();
            return std::vector<properties::PropertyOwner*>(nodes.begin(), nodes.end());
        }
    );

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    OsEng.gui()._iswa.setSource(
        [&]() {
            const auto& groups = IswaManager::ref().groups();
            std::vector<properties::PropertyOwner*> res;
            std::transform(
                groups.begin(),
                groups.end(),
                std::back_inserter(res),
                [](const auto& val) {
                    return val.second.get(); 
                }
            );
            return res;
        }
    );
#endif
    
#endif

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    IswaManager::initialize();
#endif

    _syncEngine->addSyncables(Time::ref().getSyncables());
    _syncEngine->addSyncables(_renderEngine->getSyncables());
    _syncEngine->addSyncable(_scriptEngine.get());

    LINFO("Finished initializing");
    return true;
}


void OpenSpaceEngine::writeDocumentation() {
    // If a LuaDocumentationFile was specified, generate it now
    const std::string LuaDocumentationType =
        ConfigurationManager::KeyLuaDocumentation + "." + ConfigurationManager::PartType;
    const std::string LuaDocumentationFile =
        ConfigurationManager::KeyLuaDocumentation + "." + ConfigurationManager::PartFile;

    const bool hasLuaDocType = configurationManager().hasKey(LuaDocumentationType);
    const bool hasLuaDocFile = configurationManager().hasKey(LuaDocumentationFile);
    if (hasLuaDocType && hasLuaDocFile) {
        std::string luaDocumentationType;
        configurationManager().getValue(LuaDocumentationType, luaDocumentationType);
        std::string luaDocumentationFile;
        configurationManager().getValue(LuaDocumentationFile, luaDocumentationFile);

        luaDocumentationFile = absPath(luaDocumentationFile);
        _scriptEngine->writeDocumentation(luaDocumentationFile, luaDocumentationType);
    }

    // If a general documentation was specified, generate it now
    const std::string DocumentationType =
        ConfigurationManager::KeyDocumentation + '.' + ConfigurationManager::PartType;
    const std::string DocumentationFile =
        ConfigurationManager::KeyDocumentation + '.' + ConfigurationManager::PartFile;

    const bool hasDocumentationType = configurationManager().hasKey(DocumentationType);
    const bool hasDocumentationFile = configurationManager().hasKey(DocumentationFile);
    if (hasDocumentationType && hasDocumentationFile) {
        std::string documentationType;
        configurationManager().getValue(DocumentationType, documentationType);
        std::string documentationFile;
        configurationManager().getValue(DocumentationFile, documentationFile);
        documentationFile = absPath(documentationFile);
        DocEng.writeDocumentation(documentationFile, documentationType);
    }

    const std::string FactoryDocumentationType =
        ConfigurationManager::KeyFactoryDocumentation + '.' + ConfigurationManager::PartType;

    const std::string FactoryDocumentationFile =
        ConfigurationManager::KeyFactoryDocumentation + '.' + ConfigurationManager::PartFile;
    bool hasFactoryDocumentationType = configurationManager().hasKey(FactoryDocumentationType);
    bool hasFactoryDocumentationFile = configurationManager().hasKey(FactoryDocumentationFile);
    if (hasFactoryDocumentationType && hasFactoryDocumentationFile) {
        std::string type = configurationManager().value<std::string>(FactoryDocumentationType);
        std::string file = configurationManager().value<std::string>(FactoryDocumentationFile);

        FactoryManager::ref().writeDocumentation(absPath(file), type);
    }
}

bool OpenSpaceEngine::isInitialized() {
    return _engine != nullptr;
}

void OpenSpaceEngine::clearAllWindows() {
    _windowWrapper->clearAllWindows(glm::vec4(0.f, 0.f, 0.f, 1.f));
}

void OpenSpaceEngine::gatherCommandlineArguments() {
    commandlineArgumentPlaceholders.configurationName = "";
    _commandlineParser->addCommand(std::make_unique<SingleCommand<std::string>>(
        &commandlineArgumentPlaceholders.configurationName, "-config", "-c",
        "Provides the path to the OpenSpace configuration file"
    ));

    commandlineArgumentPlaceholders.sgctConfigurationName = "";
    _commandlineParser->addCommand(std::make_unique<SingleCommand<std::string>>(
        &commandlineArgumentPlaceholders.sgctConfigurationName, "-sgct", "-s",
        "Provides the path to the SGCT configuration file, overriding the value set in "
        "the OpenSpace configuration file"
    ));

    commandlineArgumentPlaceholders.sceneName = "";
    _commandlineParser->addCommand(std::make_unique<SingleCommand<std::string>>(
        &commandlineArgumentPlaceholders.sceneName, "-scene", "", "Provides the path to "
        "the scene file, overriding the value set in the OpenSpace configuration file"
    ));

    commandlineArgumentPlaceholders.cacheFolder = "";
    _commandlineParser->addCommand(std::make_unique<SingleCommand<std::string>>(
        &commandlineArgumentPlaceholders.cacheFolder, "-cacheDir", "", "Provides the "
        "path to a cache file, overriding the value set in the OpenSpace configuration "
        "file"
    ));
}

void OpenSpaceEngine::runScripts(const ghoul::Dictionary& scripts) {
    for (size_t i = 1; i <= scripts.size(); ++i) {
        std::stringstream stream;
        stream << i;
        const std::string& key = stream.str();
        const bool hasKey = scripts.hasKeyAndValue<std::string>(key);
        if (!hasKey) {
            LERROR("The startup scripts have to be declared in a simple array format."
                " Startup scripts did not contain the key '" << key << "'");
            break;
        }

        std::string scriptPath;
        scripts.getValue(key, scriptPath);
        std::string&& absoluteScriptPath = absPath(scriptPath);
        _engine->scriptEngine().runScriptFile(absoluteScriptPath);
    }
}


void OpenSpaceEngine::runPreInitializationScripts(const std::string& sceneDescription) {
    LINFO("Running Initialization scripts");
    lua_State* state = ghoul::lua::createNewLuaState();
    OnExit(
           // Delete the Lua state at the end of the scope, no matter what
           [state](){ghoul::lua::destroyLuaState(state);}
    );
    OsEng.scriptEngine().initializeLuaState(state);

    // First execute the script to get all global variables
    ghoul::lua::runScriptFile(state, absPath(sceneDescription));

    // Get the preinitialize function
    lua_getglobal(state, PreInitializeFunction.c_str());
    bool isFunction = lua_isfunction(state, -1);
    if (!isFunction) {
        LERROR("Error executing startup script '" << sceneDescription << "'. Scene '" <<
               sceneDescription << "' does not have a function '" <<
               PreInitializeFunction << "'");
        return;
    }
    
    // And execute the preinitialize function
    int success = lua_pcall(state, 0, 0, 0);
    if (success != 0) {
        LERROR("Error executing '" << PreInitializeFunction << "': " <<
               lua_tostring(state, -1));
    }
}

void OpenSpaceEngine::runPostInitializationScripts(const std::string& sceneDescription) {
    LINFO("Running Setup scripts");
    lua_State* state = ghoul::lua::createNewLuaState();
    OnExit(
        // Delete the Lua state at the end of the scope, no matter what
        [state](){ghoul::lua::destroyLuaState(state);}
    );
    OsEng.scriptEngine().initializeLuaState(state);
    
    // First execute the script to get all global variables
    ghoul::lua::runScriptFile(state, absPath(sceneDescription));
    
    // Get the preinitialize function
    lua_getglobal(state, PostInitializationFunction.c_str());
    bool isFunction = lua_isfunction(state, -1);
    if (!isFunction) {
        LERROR("Error executing startup script '" << sceneDescription << "'. Scene '" <<
               sceneDescription << "' does not have a function '" <<
               PostInitializationFunction << "'");
        return;
    }
    
    // And execute the preinitialize function
    int success = lua_pcall(state, 0, 0, 0);
    if (success != 0) {
        LERROR("Error executing '" << PostInitializationFunction << "': " <<
               lua_tostring(state, -1)
        );
    }
}

void OpenSpaceEngine::loadFonts() {
    ghoul::Dictionary fonts;
    configurationManager().getValue(ConfigurationManager::KeyFonts, fonts);

    const glm::ivec3 fontAtlasSize{1024, 1024, 1};
    _fontManager = std::make_unique<ghoul::fontrendering::FontManager>(fontAtlasSize);
    
    for (const std::string& key : fonts.keys()) {
        std::string font;
        fonts.getValue(key, font);
        font = absPath(font);
        
        if (!FileSys.fileExists(font)) {
            LERROR("Could not find font '" << font << "'");
            continue;
        }

        LINFO("Registering font '" << font << "' with key '" << key << "'");
        bool success = _fontManager->registerFontPath(key, font);
        
        if (!success)
            LERROR("Error registering font '" << font << "' with key '" << key << "'");
    }
    
    try {
        bool initSuccess = ghoul::fontrendering::FontRenderer::initialize();
        if (!initSuccess)
            LERROR("Error initializing default font renderer");

        ghoul::fontrendering::FontRenderer::defaultRenderer().setFramebufferSize(
            _renderEngine->fontResolution()
        );
    }
    catch (const ghoul::RuntimeError& err) {
        LERRORC(err.component, err.message);
    }
}
    
void OpenSpaceEngine::configureLogging() {
    const std::string KeyLogLevel =
        ConfigurationManager::KeyLogging + '.' + ConfigurationManager::PartLogLevel;
    const std::string KeyLogImmediateFlush =
        ConfigurationManager::KeyLogging + '.' + ConfigurationManager::PartImmediateFlush;
    const std::string KeyLogs = 
        ConfigurationManager::KeyLogging + '.' + ConfigurationManager::PartLogs;

    if (configurationManager().hasKeyAndValue<std::string>(KeyLogLevel)) {
        std::string logLevel;
        configurationManager().getValue(KeyLogLevel, logLevel);

        bool immediateFlush = false;
        configurationManager().getValue(KeyLogImmediateFlush, immediateFlush);

        LogManager::LogLevel level = LogManager::levelFromString(logLevel);
        LogManager::deinitialize();
        using ImmediateFlush = ghoul::logging::LogManager::ImmediateFlush;
        LogManager::initialize(
            level,
            immediateFlush ? ImmediateFlush::Yes : ImmediateFlush::No
        );
        LogMgr.addLog(std::make_unique<ConsoleLog>());
    }

    if (configurationManager().hasKeyAndValue<ghoul::Dictionary>(KeyLogs)) {
        ghoul::Dictionary logs;
        configurationManager().getValue(KeyLogs, logs);

        for (size_t i = 1; i <= logs.size(); ++i) {
            ghoul::Dictionary logInfo;
            logs.getValue(std::to_string(i), logInfo);

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
}

bool OpenSpaceEngine::initializeGL() {
    LINFO("Initializing Rendering Engine");
    bool success = _renderEngine->initializeGL();
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
    LINFO("Initializing OnScreen GUI GL");
    try {
        _gui->initializeGL();
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(e.what());
    }
#endif
    LINFO("Finished initializing OpenGL");

    // If using swapgroups, 
    LINFO("_windowWrapper->isUsingSwapGroups(): " << _windowWrapper->isUsingSwapGroups());
    LINFO("_windowWrapper->isSwapGroupMaster(): " << _windowWrapper->isSwapGroupMaster());
    return success;
}

bool OpenSpaceEngine::isMaster(){
    return _isMaster;
}

void OpenSpaceEngine::setMaster(bool master){
    _isMaster = master;
}
    
double OpenSpaceEngine::runTime(){
    return _runTime;
}

void OpenSpaceEngine::setRunTime(double d){
    _runTime = d;
}
    
void OpenSpaceEngine::preSynchronization() {
    FileSys.triggerFilesystemEvents();

    if (_isFirstRenderingFirstFrame) {
        _windowWrapper->setSynchronization(false);
    }
    
    _syncEngine->presync(_isMaster);
    if (_isMaster) {
        double dt = _windowWrapper->averageDeltaTime();
        _timeManager->preSynchronization(dt);

        auto scheduledScripts = _scriptScheduler->progressTo(Time::ref().j2000Seconds());
        while(scheduledScripts.size()){
            auto scheduledScript = scheduledScripts.front();
            LINFO(scheduledScript);
            _scriptEngine->queueScript(scheduledScript, ScriptEngine::RemoteScripting::Yes);
            scheduledScripts.pop();
        }

        _interactionHandler->updateInputStates(dt);
        
        _renderEngine->updateSceneGraph();
        _interactionHandler->updateCamera();
        _renderEngine->camera()->invalidateCache();

        _parallelConnection->preSynchronization();

    }
}

void OpenSpaceEngine::postSynchronizationPreDraw() {
    _syncEngine->postsync(_isMaster);

    if (_isInShutdownMode) {
        if (_shutdownCountdown <= 0.f) {
            _windowWrapper->terminate();
        }
        _shutdownCountdown -= _windowWrapper->averageDeltaTime();
    }

    _renderEngine->updateFade();
    _renderEngine->updateRenderer();
    _renderEngine->updateScreenSpaceRenderables();
    _renderEngine->updateShaderPrograms();
    
    if (!_isMaster) {
        _renderEngine->updateSceneGraph();
        _renderEngine->camera()->invalidateCache();
    }   

    // Step the camera using the current mouse velocities which are synced
    //_interactionHandler->updateCamera();
    
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
    if (_isMaster && _gui->isEnabled() && _windowWrapper->isRegularRendering()) {
        glm::vec2 mousePosition = _windowWrapper->mousePosition();
        //glm::ivec2 drawBufferResolution = _windowWrapper->currentDrawBufferResolution();
        glm::ivec2 windowSize = _windowWrapper->currentWindowSize();
        glm::ivec2 renderingSize = _windowWrapper->currentWindowResolution();
        uint32_t mouseButtons = _windowWrapper->mouseButtons(2);
        
        double dt = _windowWrapper->averageDeltaTime();

        _gui->startFrame(
            static_cast<float>(dt),
            glm::vec2(windowSize),
            _windowWrapper->dpiScaling(),
            mousePosition,
            mouseButtons
        );
    }
#endif

    // Testing this every frame has minimal impact on the performance --- abock
    // Debug build: 1-2 us ; Release build: <= 1 us
    using ghoul::logging::LogManager;
    int warningCounter = LogMgr.messageCounter(LogManager::LogLevel::Warning);
    int errorCounter = LogMgr.messageCounter(LogManager::LogLevel::Error);
    int fatalCounter = LogMgr.messageCounter(LogManager::LogLevel::Fatal);

    if (warningCounter > 0)
        LWARNINGC("Logging", "Number of Warnings raised: " << warningCounter);
    if (errorCounter > 0)
        LWARNINGC("Logging", "Number of Errors raised: " << errorCounter);
    if (fatalCounter > 0)
        LWARNINGC("Logging", "Number of Fatals raised: " << fatalCounter);

    LogMgr.resetMessageCounters();

}

void OpenSpaceEngine::render(const glm::mat4& projectionMatrix, const glm::mat4& viewMatrix) {
    _renderEngine->render(projectionMatrix, viewMatrix);
}

void OpenSpaceEngine::postDraw() {
    _renderEngine->postDraw();

    bool showGui = _windowWrapper->hasGuiWindow() ? _windowWrapper->isGuiWindow() : true;
    if (showGui) {
        _renderEngine->renderScreenLog();
        if (_console->isVisible())
            _console->render();
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
        if (_gui->isEnabled() && _isMaster && _windowWrapper->isRegularRendering())
            _gui->endFrame();
#endif
    }

    if (_isInShutdownMode) {
        _renderEngine->renderShutdownInformation(_shutdownCountdown, _shutdownWait);
    }

    if (_isFirstRenderingFirstFrame) {
        _windowWrapper->setSynchronization(true);
        _isFirstRenderingFirstFrame = false;
    }

}

void OpenSpaceEngine::keyboardCallback(Key key, KeyModifier mod, KeyAction action) {
    if (_isMaster) {
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
        if (_gui->isEnabled()) {
            bool isConsumed = _gui->keyCallback(key, mod, action);
            if (isConsumed)
                return;
        }
#endif
        if (key == _console->commandInputButton()) {
            if (action == KeyAction::Press) {
                _console->toggleMode();
            }
        } else if (!_console->isVisible()) {
            _interactionHandler->keyboardCallback(key, mod, action);
        } else {
            _console->keyboardCallback(key, mod, action);
        }
    }
}

void OpenSpaceEngine::charCallback(unsigned int codepoint, KeyModifier modifier) {
    if (_isMaster) {
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
        if (_gui->isEnabled()) {
            const bool isConsumed = _gui->charCallback(codepoint, modifier);
            if (isConsumed)
                return;
        }
#endif
        if (_console->isVisible()) {
            _console->charCallback(codepoint, modifier);
        }
    }
}

void OpenSpaceEngine::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (_isMaster) {
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
        if (_gui->isEnabled()) {
            const bool isConsumed = _gui->mouseButtonCallback(button, action);
            if (isConsumed && action != MouseAction::Release)
                return;
        }
#endif
        _interactionHandler->mouseButtonCallback(button, action);
    }
}

void OpenSpaceEngine::mousePositionCallback(double x, double y) {
    if (_isMaster) {
        _interactionHandler->mousePositionCallback(x, y);
    }
}

void OpenSpaceEngine::mouseScrollWheelCallback(double pos) {
    if (_isMaster) {
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
        if (_gui->isEnabled()) {
            const bool isConsumed = _gui->mouseWheelCallback(pos);
            if (isConsumed)
                return;
        }
#endif
        _interactionHandler->mouseScrollWheelCallback(pos);
    }
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
                                              int clientId)
{
    if (size == 0)
        return;

    _networkEngine->handleMessage(std::string(receivedChars, size));
}

void OpenSpaceEngine::toggleShutdownMode() {
    if (_isInShutdownMode) {
        // If we are already in shutdown mode, we want to disable it instead
        LINFO("Disabled shutdown mode");
        _isInShutdownMode = false;
    }
    else {
        // Else, we hav eto enable it
        LINFO("Shutting down OpenSpace");
        _shutdownCountdown = _shutdownWait;
        _isInShutdownMode = true;
    }
}

scripting::LuaLibrary OpenSpaceEngine::luaLibrary() {
    return {
        "",
        {
            {
                "toggleShutdown",
                &luascriptfunctions::toggleShutdown,
                "",
                "Toggles the shutdown mode that will close the application after the count"
                "down timer is reached"
            },
            {
                "writeDocumentation",
                &luascriptfunctions::writeDocumentation,
                "",
                "Writes out documentation files"
            }
        }
    };
}

bool OpenSpaceEngine::useBusyWaitForDecode() {
    return _settingsEngine->busyWaitForDecode();
}

bool OpenSpaceEngine::logSGCTOutOfOrderErrors() {
    return _settingsEngine->logSGCTOutOfOrderErrors();
}

void OpenSpaceEngine::enableBarrier() {
    _windowWrapper->setBarrier(true);
}

void OpenSpaceEngine::disableBarrier() {
    _windowWrapper->setBarrier(false);
}

NetworkEngine& OpenSpaceEngine::networkEngine() {
    ghoul_assert(_networkEngine, "NetworkEngine must not be nullptr");
    return *_networkEngine;
}

ModuleEngine& OpenSpaceEngine::moduleEngine() {
    ghoul_assert(_moduleEngine, "ModuleEngine must not be nullptr");
    return *_moduleEngine;
}

ConfigurationManager& OpenSpaceEngine::configurationManager() {
    ghoul_assert(_configurationManager, "ConfigurationManager must not be nullptr");
    return *_configurationManager;
}

interaction::InteractionHandler& OpenSpaceEngine::interactionHandler() {
    ghoul_assert(_interactionHandler, "InteractionHandler must not be nullptr");
    return *_interactionHandler;
}

RenderEngine& OpenSpaceEngine::renderEngine() {
    ghoul_assert(_renderEngine, "RenderEngine must not be nullptr");
    return *_renderEngine;
}

ScriptEngine& OpenSpaceEngine::scriptEngine() {
    ghoul_assert(_scriptEngine, "ScriptEngine must not be nullptr");
    return *_scriptEngine;
}

ScriptScheduler& OpenSpaceEngine::scriptScheduler(){
    ghoul_assert(_scriptScheduler, "ScriptScheduler must not be nullptr");
    return *_scriptScheduler;
}

LuaConsole& OpenSpaceEngine::console() {
    ghoul_assert(_console, "LuaConsole must not be nullptr");
    return *_console;
}

#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
gui::GUI& OpenSpaceEngine::gui() {
    ghoul_assert(_gui, "GUI must not be nullptr");
    return *_gui;
}
#endif

ParallelConnection& OpenSpaceEngine::parallelConnection() {
    ghoul_assert(_parallelConnection, "ParallelConnection must not be nullptr");
    return *_parallelConnection;
}
    
properties::PropertyOwner& OpenSpaceEngine::globalPropertyOwner() {
    ghoul_assert(
        _globalPropertyNamespace,
        "Global Property Namespace must not be nullptr"
    );
    return *_globalPropertyNamespace;
}

WindowWrapper& OpenSpaceEngine::windowWrapper() {
    ghoul_assert(_windowWrapper, "Window Wrapper must not be nullptr");
    return *_windowWrapper;
}
    
ghoul::fontrendering::FontManager& OpenSpaceEngine::fontManager() {
    ghoul_assert(_fontManager, "Font Manager must not be nullptr");
    return *_fontManager;
}

DownloadManager& OpenSpaceEngine::downloadManager() {
    ghoul_assert(_downloadManager, "Download Manager must not be nullptr");
    return *_downloadManager;
}

TimeManager& OpenSpaceEngine::timeManager() {
    ghoul_assert(_timeManager, "Download Manager must not be nullptr");
    return *_timeManager;
}


}  // namespace openspace
