/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/logfactory.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/settingsengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/luaconsole.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/network/networkengine.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/translation.h>
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
#include <ghoul/lua/luastate.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/onscopeexit.h>
#include <ghoul/systemcapabilities/systemcapabilities>

#include <fstream>
#include <queue>

#if defined(_MSC_VER) && defined(OPENSPACE_ENABLE_VLD)
#include <vld.h>
#endif

#ifdef WIN32
#include <Windows.h>
#endif

#include "openspaceengine_lua.inl"

using namespace openspace::scripting;
using namespace ghoul::filesystem;
using namespace ghoul::logging;
using namespace ghoul::cmdparser;

namespace {
    const char* _loggerCat = "OpenSpaceEngine";
    const char* SgctDefaultConfigFile = "${SGCT}/single.xml";
    
    const char* SgctConfigArgumentCommand = "-config";
    
    const char* PreInitializeFunction = "preInitialization";
    const char* PostInitializationFunction = "postInitialization";

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
    , _parallelConnection(new ParallelConnection)
    , _windowWrapper(std::move(windowWrapper))
    , _globalPropertyNamespace(new properties::PropertyOwner)
    , _runTime(0.0)
    , _shutdown({false, 0.f, 0.f})
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
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Rotation>>(),
        "Rotation"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Scale>>(),
        "Scale"
    );

    SpiceManager::initialize();
    Time::initialize();
    TransformationManager::initialize();
}

//OpenSpaceEngine::~OpenSpaceEngine() {
//    _globalPropertyNamespace = nullptr;
//    _windowWrapper = nullptr;
//    _parallelConnection = nullptr;
//    _configurationManager = nullptr;
//    _interactionHandler = nullptr;
//    _renderEngine = nullptr;
//    _scriptEngine = nullptr;
//    _networkEngine = nullptr;
//    _syncEngine = nullptr;
//    _commandlineParser = nullptr;
//    _console = nullptr;
//    _moduleEngine = nullptr;
//    _settingsEngine = nullptr;
//}

OpenSpaceEngine& OpenSpaceEngine::ref() {
    ghoul_assert(_engine, "OpenSpaceEngine not created");
    return *_engine;
}

void OpenSpaceEngine::create(int argc, char** argv,
                             std::unique_ptr<WindowWrapper> windowWrapper,
                             std::vector<std::string>& sgctArguments, bool& requestClose)
{
    ghoul_assert(!_engine, "OpenSpaceEngine was already created");
    ghoul_assert(windowWrapper != nullptr, "No Window Wrapper was provided");
    
    requestClose = false;
    
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
        throw ghoul::RuntimeError(
            "No arguments were passed to this function",
            "OpenSpaceEngine"
        );
    }

    // Create other objects
    LDEBUG("Creating OpenSpaceEngine");
    _engine = new OpenSpaceEngine(std::string(argv[0]), std::move(windowWrapper));

    registerCoreClasses(DocEng);

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
            "Configuration file '" + configurationFilePath + "' not found"
        );
    }
    LINFO("Configuration Path: '" << configurationFilePath << "'");

    // Loading configuration from disk
    LDEBUG("Loading configuration from disk");
    try {
        _engine->configurationManager().loadFromFile(configurationFilePath);
    }
    catch (const documentation::SpecificationError& e) {
        LFATAL("Loading of configuration file '" << configurationFilePath << "' failed");
        for (const documentation::TestResult::Offense& o : e.result.offenses) {
            LERRORC(o.offender, std::to_string(o.reason));
        }
        for (const documentation::TestResult::Warning& w : e.result.warnings) {
            LWARNINGC(w.offender, std::to_string(w.reason));
        }
        throw;
    }
    catch (const ghoul::RuntimeError& e) {
        LFATAL("Loading of configuration file '" << configurationFilePath << "' failed");
        throw;
    }

    const bool hasCacheCommandline = !commandlineArgumentPlaceholders.cacheFolder.empty();
    const bool hasCacheConfiguration = _engine->configurationManager().hasKeyAndValue<bool>(
        ConfigurationManager::KeyPerSceneCache
    );
    std::string cacheFolder = absPath("${CACHE}");
    if (hasCacheCommandline) {
        cacheFolder = commandlineArgumentPlaceholders.cacheFolder;
        // @CLEANUP:  Why is this commented out? ---abock
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
        OPENSPACE_VERSION_PATCH <<
        " (" << OPENSPACE_VERSION_STRING << ")"
    );

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
    std::string sgctConfigurationPath = SgctDefaultConfigFile;
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
    sgctArguments.insert(sgctArguments.begin() + 1, SgctConfigArgumentCommand);
    sgctArguments.insert(sgctArguments.begin() + 2, absPath(sgctConfigurationPath));
}

void OpenSpaceEngine::destroy() {
    LTRACE("OpenSpaceEngine::destroy(begin)");
    for (const auto& func : _engine->_moduleCallbacks.deinitializeGL) {
        func();
    }

    for (const auto& func : _engine->_moduleCallbacks.deinitialize) {
        func();
    }

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
    LTRACE("OpenSpaceEngine::destroy(end)");
}

void OpenSpaceEngine::initialize() {
    LTRACE("OpenSpaceEngine::initialize(begin)");
    // clear the screen so the user don't have to see old buffer contents from the
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
    ghoul::systemcapabilities::OpenGLCapabilitiesComponent::Version version =
        _engine->_moduleEngine->requiredOpenGLVersion();
    LINFO("Required OpenGL version: " << version.toString());

    if (OpenGLCap.openGLVersion() < version) {
        throw ghoul::RuntimeError(
            "Module required higher OpenGL version than is supported",
            "OpenSpaceEngine"
        );
    }

    if (configurationManager().hasKey(ConfigurationManager::KeyDownloadRequestURL)) {
        const std::string requestUrl = configurationManager().value<std::string>(
            ConfigurationManager::KeyDownloadRequestURL
        );

        _downloadManager = std::make_unique<DownloadManager>(
            requestUrl,
            DownloadVersion
        );
    }

    // Register Lua script functions
    LDEBUG("Registering Lua libraries");
    registerCoreClasses(*_scriptEngine);
    
    for (OpenSpaceModule* module : _moduleEngine->modules()) {
        _scriptEngine->addLibrary(module->luaLibrary());
    }
         
    // TODO: Maybe move all scenegraph and renderengine stuff to initializeGL
    scriptEngine().initialize();

    writeDocumentation();

    if (configurationManager().hasKey(ConfigurationManager::KeyShutdownCountdown)) {
        _shutdown.waitTime = configurationManager().value<double>(
            ConfigurationManager::KeyShutdownCountdown
        );
    }

    if (!commandlineArgumentPlaceholders.sceneName.empty()) {
        configurationManager().setValue(
            ConfigurationManager::KeyConfigScene,
            commandlineArgumentPlaceholders.sceneName
        );
    }

    // Initialize the SettingsEngine
    _settingsEngine->initialize();
    _settingsEngine->setModules(_moduleEngine->modules());

    // Initialize the InteractionHandler
    _interactionHandler->initialize();

    // Load a light and a monospaced font
    loadFonts();

    // Initialize the Scene
    // @CLEANUP:  This should become a unique_ptr that is either created inside the
    // renderengine or moved into it ---abock
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
    
    for (const auto& func : _moduleCallbacks.initialize) {
        func();
    }

    // Run start up scripts
    runPreInitializationScripts(scenePath);

    _syncEngine->addSyncables(Time::ref().getSyncables());
    _syncEngine->addSyncables(_renderEngine->getSyncables());
    _syncEngine->addSyncable(_scriptEngine.get());

    LINFO("Finished initializing");
    LTRACE("OpenSpaceEngine::initialize(end)");
}

void OpenSpaceEngine::deinitialize() {
    LTRACE("OpenSpaceEngine::deinitialize(begin)");

    _interactionHandler->deinitialize();
    _renderEngine->deinitialize();

    LTRACE("OpenSpaceEngine::deinitialize(end)");
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
        std::string luaDocumentationType = configurationManager().value<std::string>(
            LuaDocumentationType
        );
        std::string luaDocumentationFile = configurationManager().value<std::string>(
            LuaDocumentationFile
        );

        _scriptEngine->writeDocumentation(
            absPath(luaDocumentationFile),
            luaDocumentationType
        );
    }

    // If a general documentation was specified, generate it now
    const std::string DocumentationType =
        ConfigurationManager::KeyDocumentation + '.' + ConfigurationManager::PartType;
    const std::string DocumentationFile =
        ConfigurationManager::KeyDocumentation + '.' + ConfigurationManager::PartFile;

    const bool hasDocumentationType = configurationManager().hasKey(DocumentationType);
    const bool hasDocumentationFile = configurationManager().hasKey(DocumentationFile);
    if (hasDocumentationType && hasDocumentationFile) {
        std::string documentationType = configurationManager().value<std::string>(
            DocumentationType
        );
        std::string documentationFile = configurationManager().value<std::string>(
            DocumentationFile
        );

        DocEng.writeDocumentation(
            absPath(documentationFile),
            documentationType
        );
    }

    const std::string FactoryDocumentationType =
        ConfigurationManager::KeyFactoryDocumentation + '.' +
        ConfigurationManager::PartType;

    const std::string FactoryDocumentationFile =
        ConfigurationManager::KeyFactoryDocumentation + '.' +
        ConfigurationManager::PartFile;
    
    bool hasFactoryDocumentationType = configurationManager().hasKey(
        FactoryDocumentationType
    );
    bool hasFactoryDocumentationFile = configurationManager().hasKey(
        FactoryDocumentationFile
    );
    if (hasFactoryDocumentationType && hasFactoryDocumentationFile) {
        std::string type = configurationManager().value<std::string>(
            FactoryDocumentationType
        );
        std::string file = configurationManager().value<std::string>(
            FactoryDocumentationFile
        );

        FactoryManager::ref().writeDocumentation(absPath(file), type);
    }
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

void OpenSpaceEngine::runPreInitializationScripts(const std::string& sceneDescription) {
    // @CLEANUP:  Move this into the scene loading?  ---abock
    LINFO("Running Initialization scripts");
    
    ghoul::lua::LuaState state;
    OsEng.scriptEngine().initializeLuaState(state);

    // First execute the script to get all global variables
    ghoul::lua::runScriptFile(state, absPath(sceneDescription));

    // Get the preinitialize function
    lua_getglobal(state, PreInitializeFunction);
    bool isFunction = lua_isfunction(state, -1);
    if (!isFunction) {
        LERROR(
            "Error executing startup script '" << sceneDescription << "'. Scene '" <<
            sceneDescription << "' does not have a function '" <<
            PreInitializeFunction << "'"
        );
        return;
    }
    
    // And execute the preinitialize function
    int success = lua_pcall(state, 0, 0, 0);
    if (success != 0) {
        LERROR(
            "Error executing '" << PreInitializeFunction << "': " <<
            lua_tostring(state, -1)
        );
    }
}

void OpenSpaceEngine::runPostInitializationScripts(const std::string& sceneDescription) {
    // @CLEANUP:  Move this into the scene loading?  ---abock
    LINFO("Running Setup scripts");
    ghoul::lua::LuaState state;
    OsEng.scriptEngine().initializeLuaState(state);
    
    // First execute the script to get all global variables
    ghoul::lua::runScriptFile(state, absPath(sceneDescription));
    
    // Get the preinitialize function
    lua_getglobal(state, PostInitializationFunction);
    bool isFunction = lua_isfunction(state, -1);
    if (!isFunction) {
        LERROR(
            "Error executing startup script '" << sceneDescription << "'. Scene '" <<
            sceneDescription << "' does not have a function '" <<
            PostInitializationFunction << "'"
        );
        return;
    }
    
    // And execute the preinitialize function
    int success = lua_pcall(state, 0, 0, 0);
    if (success != 0) {
        LERROR(
            "Error executing '" << PostInitializationFunction << "': " <<
            lua_tostring(state, -1)
        );
    }
}

void OpenSpaceEngine::loadFonts() {
    ghoul::Dictionary fonts;
    configurationManager().getValue(ConfigurationManager::KeyFonts, fonts);

    glm::ivec3 fontAtlasSize{1024, 1024, 1};
    _fontManager = std::make_unique<ghoul::fontrendering::FontManager>(fontAtlasSize);
    
    for (const std::string& key : fonts.keys()) {
        std::string font = fonts.value<std::string>(key);
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
        
        LogMgr.addLog(std::make_unique<ConsoleLog>());
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

void OpenSpaceEngine::initializeGL() {
    LTRACE("OpenSpaceEngine::initializeGL(begin)");

    LINFO("Initializing Rendering Engine");
    // @CLEANUP:  Remove the return statement and replace with exceptions ---abock
    _renderEngine->initializeGL();
    
    for (const auto& func : _moduleCallbacks.initializeGL) {
        func();
    }
    
    LINFO("Finished initializing OpenGL");

    // If using swapgroups,
    
    LINFO("IsUsingSwapGroups: " << _windowWrapper->isUsingSwapGroups());
    LINFO("IsSwapGroupMaster: " << _windowWrapper->isSwapGroupMaster());
    
    LTRACE("OpenSpaceEngine::initializeGL(end)");
}

double OpenSpaceEngine::runTime() {
    return _runTime;
}

void OpenSpaceEngine::setRunTime(double d) {
    _runTime = d;
}
    
void OpenSpaceEngine::preSynchronization() {
    LTRACE("OpenSpaceEngine::preSynchronization(begin)");
    FileSys.triggerFilesystemEvents();

    if (_isFirstRenderingFirstFrame) {
        _windowWrapper->setSynchronization(false);
    }
    
    bool master = _windowWrapper->isMaster();
    
    _syncEngine->presync(master);
    if (master) {
        double dt = _windowWrapper->averageDeltaTime();
        _timeManager->preSynchronization(dt);

        auto scheduledScripts = _scriptScheduler->progressTo(Time::ref().j2000Seconds());
        for (auto it = scheduledScripts.first; it != scheduledScripts.second; ++it) {
            _scriptEngine->queueScript(
                *it, ScriptEngine::RemoteScripting::Yes
            );
        }

        _interactionHandler->updateInputStates(dt);
        
        _renderEngine->updateSceneGraph();
        _interactionHandler->updateCamera(dt);
        _renderEngine->camera()->invalidateCache();

        _parallelConnection->preSynchronization();

    }
    
    for (const auto& func : _moduleCallbacks.preSync) {
        func();
    }
    LTRACE("OpenSpaceEngine::preSynchronization(end)");
}

void OpenSpaceEngine::postSynchronizationPreDraw() {
    LTRACE("OpenSpaceEngine::postSynchronizationPreDraw(begin)");
    
    bool master = _windowWrapper->isMaster();
    _syncEngine->postsync(master);

    if (_shutdown.inShutdown) {
        if (_shutdown.timer <= 0.f) {
            _windowWrapper->terminate();
        }
        _shutdown.timer -= _windowWrapper->averageDeltaTime();
    }

    _renderEngine->updateSceneGraph();
    _renderEngine->updateFade();
    _renderEngine->updateRenderer();
    _renderEngine->updateScreenSpaceRenderables();
    _renderEngine->updateShaderPrograms();
    
    if (!master) {
        _renderEngine->camera()->invalidateCache();
    }   

    // Step the camera using the current mouse velocities which are synced
    //_interactionHandler->updateCamera();
    
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
        LWARNINGC("Logging", "Number of Warnings raised: " << warningCounter);
    }
    if (errorCounter > 0) {
        LWARNINGC("Logging", "Number of Errors raised: " << errorCounter);
    }
    if (fatalCounter > 0) {
        LWARNINGC("Logging", "Number of Fatals raised: " << fatalCounter);
    }

    LogMgr.resetMessageCounters();
    
    LTRACE("OpenSpaceEngine::postSynchronizationPreDraw(end)");
}

void OpenSpaceEngine::render(const glm::mat4& viewMatrix,
                             const glm::mat4& projectionMatrix)
{
    LTRACE("OpenSpaceEngine::render(begin)");
    _renderEngine->render(viewMatrix, projectionMatrix);
    
    for (const auto& func : _moduleCallbacks.render) {
        func();
    }
    
    // @CLEANUP:  Replace the two windows by a single call to whether a gui should be
    // rendered ---abock
    bool showGui = _windowWrapper->hasGuiWindow() ? _windowWrapper->isGuiWindow() : true;
    if (showGui && _windowWrapper->isMaster() && _windowWrapper->isRegularRendering()) {
        _renderEngine->renderScreenLog();
        _console->render();
    }

    if (_shutdown.inShutdown) {
        _renderEngine->renderShutdownInformation(_shutdown.timer, _shutdown.waitTime);
    }

    LTRACE("OpenSpaceEngine::render(end)");
}

void OpenSpaceEngine::postDraw() {
    LTRACE("OpenSpaceEngine::postDraw(begin)");
    
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

    _interactionHandler->keyboardCallback(key, mod, action);
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
    
    _interactionHandler->mouseButtonCallback(button, action);
}

void OpenSpaceEngine::mousePositionCallback(double x, double y) {
    for (const auto& func : _moduleCallbacks.mousePosition) {
        func(x, y);
    }

    _interactionHandler->mousePositionCallback(x, y);
}

void OpenSpaceEngine::mouseScrollWheelCallback(double pos) {
    for (const auto& func : _moduleCallbacks.mouseScrollWheel) {
        bool consumed = func(pos);
        if (consumed) {
            return;
        }
    }
    
    _interactionHandler->mouseScrollWheelCallback(pos);
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
    if (size == 0) {
        return;
    }

    _networkEngine->handleMessage(std::string(receivedChars, size));
}

void OpenSpaceEngine::toggleShutdownMode() {
    if (_shutdown.inShutdown) {
        // If we are already in shutdown mode, we want to disable it
        LINFO("Disabled shutdown mode");
        _shutdown.inShutdown = false;
    }
    else {
        // Else, we have to enable it
        LINFO("Shutting down OpenSpace");
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

void OpenSpaceEngine::enableBarrier() {
    _windowWrapper->setBarrier(true);
}

void OpenSpaceEngine::disableBarrier() {
    _windowWrapper->setBarrier(false);
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
        case CallbackOption::PostDraw:
            _moduleCallbacks.postDraw.push_back(std::move(function));
            break;
        default:
            ghoul_assert(false, "Missing case label");
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
                                                    std::function<bool (double)> function)
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

ParallelConnection& OpenSpaceEngine::parallelConnection() {
    ghoul_assert(_parallelConnection, "ParallelConnection must not be nullptr");
    return *_parallelConnection;
}

RenderEngine& OpenSpaceEngine::renderEngine() {
    ghoul_assert(_renderEngine, "RenderEngine must not be nullptr");
    return *_renderEngine;
}
    
SettingsEngine& OpenSpaceEngine::settingsEngine() {
    ghoul_assert(_settingsEngine, "Settings Engine must not be nullptr");
    return *_settingsEngine;
}

TimeManager& OpenSpaceEngine::timeManager() {
    ghoul_assert(_timeManager, "Download Manager must not be nullptr");
    return *_timeManager;
}

WindowWrapper& OpenSpaceEngine::windowWrapper() {
    ghoul_assert(_windowWrapper, "Window Wrapper must not be nullptr");
    return *_windowWrapper;
}

ghoul::fontrendering::FontManager& OpenSpaceEngine::fontManager() {
    ghoul_assert(_fontManager, "Font Manager must not be nullptr");
    return *_fontManager;
}

interaction::InteractionHandler& OpenSpaceEngine::interactionHandler() {
    ghoul_assert(_interactionHandler, "InteractionHandler must not be nullptr");
    return *_interactionHandler;
}

properties::PropertyOwner& OpenSpaceEngine::globalPropertyOwner() {
    ghoul_assert(
        _globalPropertyNamespace,
        "Global Property Namespace must not be nullptr"
    );
    return *_globalPropertyNamespace;
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
