/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

// sgct header has to be included before all others due to Windows header
#define SGCT_WINDOWS_INCLUDE
#include "sgct.h"

#include <openspace/interaction/deviceidentifier.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/time.h>
#include <openspace/util/spice.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/constants.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/systemcapabilities/systemcapabilities.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>

using namespace ghoul::filesystem;
using namespace ghoul::logging;

using namespace openspace::scripting;

namespace {
    const std::string _loggerCat = "OpenSpaceEngine";
    const std::string _configurationFile = "openspace.cfg";
    const std::string _basePathToken = "${BASE_PATH}";
    const std::string _sgctDefaultConfigFile = "${SGCT}/single.xml";
    
    const std::string _sgctConfigArgumentCommand = "-config";
    
    struct {
        std::string configurationName;
    } commandlineArgumentPlaceholders;
}

using namespace ghoul::cmdparser;

namespace openspace {

OpenSpaceEngine* OpenSpaceEngine::_engine = nullptr;

OpenSpaceEngine::OpenSpaceEngine(std::string programName)
    : _configurationManager(new ghoul::Dictionary)
    , _interactionHandler(new InteractionHandler)
    , _renderEngine(new RenderEngine)
    , _scriptEngine(new ScriptEngine)
    , _commandlineParser(new CommandlineParser(programName, true))
{
}

OpenSpaceEngine::~OpenSpaceEngine()
{
    delete _configurationManager;
    _configurationManager = nullptr;
    delete _interactionHandler;
    _interactionHandler = nullptr;
    delete _renderEngine;
    _renderEngine = nullptr;
    delete _scriptEngine;
    _scriptEngine = nullptr;
    delete _commandlineParser;
    _commandlineParser = nullptr;

	SpiceManager::deinitialize();
    Spice::deinit();
    Time::deinitialize();
    DeviceIdentifier::deinit();
    FileSystem::deinitialize();
    LogManager::deinitialize();
}

OpenSpaceEngine& OpenSpaceEngine::ref()
{
    assert(_engine);
    return *_engine;
}
    
bool OpenSpaceEngine::gatherCommandlineArguments()
{
    // TODO: Get commandline arguments from all modules

    CommandlineCommand* configurationFileCommand = new SingleCommand<std::string>(
          &commandlineArgumentPlaceholders.configurationName, "-config", "-c",
          "Provides the path to the OpenSpace configuration file");
    _commandlineParser->addCommand(configurationFileCommand);
    
    return true;
}

void OpenSpaceEngine::registerPathsFromDictionary(const ghoul::Dictionary& dictionary)
{
    const std::vector<std::string>& pathKeys = dictionary.keys();
    for (const std::string& key : pathKeys) {
        std::string p;
        if (dictionary.getValue(key, p)) {
            const std::string fullKey
                  = ghoul::filesystem::FileSystem::TokenOpeningBraces + key
                    + ghoul::filesystem::FileSystem::TokenClosingBraces;
            LDEBUG("Registering path " << fullKey << ": " << p);
			
			bool override = (_basePathToken == fullKey);
			
            if (override)
                LINFO("Overriding base path with '" << p << "'");
            FileSys.registerPathToken(fullKey, p, override);
        }
    }
}

bool OpenSpaceEngine::registerBasePathFromConfigurationFile(const std::string& filename)
{
    if (!FileSys.fileExists(filename))
        return false;

    const std::string absolutePath = FileSys.absolutePath(filename);

    std::string::size_type last
          = absolutePath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
    if (last == std::string::npos)
        return false;

    std::string basePath = absolutePath.substr(0, last);

    FileSys.registerPathToken(_basePathToken, basePath);

    return true;
}

bool OpenSpaceEngine::findConfiguration(std::string& filename)
{
    //return FileSys.fileExists(filename);
    std::string currentDirectory = FileSys.absolutePath(FileSys.currentDirectory());
    size_t occurrences = std::count(currentDirectory.begin(), currentDirectory.end(),
                                    ghoul::filesystem::FileSystem::PathSeparator);

    std::string cfgname = _configurationFile;

    bool cfgFileFound = false;
    for (size_t i = 0; i < occurrences; ++i) {
        if (i > 0)
            cfgname = "../" + cfgname;
        if (FileSys.fileExists(cfgname)) {
            cfgFileFound = true;
            break;
        }
    }
    if (!cfgFileFound)
        return false;

    filename = cfgname;

    return true;
}

bool OpenSpaceEngine::create(int argc, char** argv,
                             std::vector<std::string>& sgctArguments)
{
    // TODO custom assert (ticket #5)
    assert(_engine == nullptr);

    // initialize Ghoul logging
    LogManager::initialize(LogManager::LogLevel::Debug, true);
    LogMgr.addLog(new ConsoleLog);

    
    // Initialize FileSystem
    ghoul::filesystem::FileSystem::initialize();
    
    
    // Sanity check of values
    if (argc < 1) {
        LFATAL("No arguments were passed to the function");
        return false;
    }
    
    // create other objects
    LDEBUG("Creating OpenSpaceEngine");
    _engine = new OpenSpaceEngine(std::string(argv[0]));
    
    
    // Query modules for commandline arguments
    const bool gatherSuccess = _engine->gatherCommandlineArguments();
    if (!gatherSuccess)
        return false;
    

    // Parse commandline arguments
    std::vector<std::string> remainingArguments;
    _engine->_commandlineParser->setCommandLine(argc, argv, &sgctArguments);
    const bool executeSuccess = _engine->_commandlineParser->execute();
    if (!executeSuccess)
        return false;
    
    
    // Find configuration
    std::string configurationFilePath = commandlineArgumentPlaceholders.configurationName;
    if (configurationFilePath.empty()) {
        LDEBUG("Finding configuration");
        const bool findConfigurationSuccess = OpenSpaceEngine::findConfiguration(configurationFilePath);
        if (!findConfigurationSuccess) {
            LFATAL("Could not find OpenSpace configuration file!");
            return false;
        }
    }
	LINFO("Configuration Path: '" << FileSys.absolutePath(configurationFilePath) << "'");


    // Registering base path
    LDEBUG("Registering base path");
    if (!OpenSpaceEngine::registerBasePathFromConfigurationFile(configurationFilePath)) {
        LFATAL("Could not register base path");
        return false;
    }

    
    // Loading configuration from disk
    LDEBUG("Loading configuration from disk");
    ghoul::Dictionary& configuration = _engine->configurationManager();
    ghoul::lua::loadDictionaryFromFile(configurationFilePath, configuration);
    const bool hasKey = configuration.hasKey(constants::openspaceengine::keyPaths);
    const bool hasValue = configuration.hasValue<ghoul::Dictionary>(constants::openspaceengine::keyPaths);
    if (hasKey && hasValue) {
        ghoul::Dictionary pathsDictionary;
        configuration.getValue(constants::openspaceengine::keyPaths, pathsDictionary);
        OpenSpaceEngine::registerPathsFromDictionary(pathsDictionary);
    }
    else {
        LFATAL("Configuration file does not contain paths token '" << constants::openspaceengine::keyPaths << "'");
        return false;
    }

    
    // Determining SGCT configuration file
    LDEBUG("Determining SGCT configuration file");
    std::string sgctConfigurationPath = _sgctDefaultConfigFile;
    if (configuration.hasKey(constants::openspaceengine::keyConfigSgct))
        configuration.getValue(constants::openspaceengine::keyConfigSgct, sgctConfigurationPath);

    
    // Prepend the outgoing sgctArguments with the program name
    // as well as the configuration file that sgct is supposed to use
    sgctArguments.insert(sgctArguments.begin(), argv[0]);
    sgctArguments.insert(sgctArguments.begin() + 1, _sgctConfigArgumentCommand);
    sgctArguments.insert(sgctArguments.begin() + 2, absPath(sgctConfigurationPath));
    
    return true;
}

void OpenSpaceEngine::destroy()
{
    delete _engine;
}

bool OpenSpaceEngine::isInitialized()
{
    return _engine != nullptr;
}

bool OpenSpaceEngine::initialize()
{
    // clear the screen so the user don't have to see old buffer contents from the
    // graphics card
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    GLFWwindow* win = sgct::Engine::instance()->getActiveWindowPtr()->getWindowHandle();
    glfwSwapBuffers(win);
    int samples = sqrt(sgct::Engine::instance()->getActiveWindowPtr()->getNumberOfAASamples());
    LDEBUG("samples: " << samples);

    int x1, xSize, y1, ySize;
    sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewportPixelCoords(x1, y1, xSize, ySize);
    std::string sourceHeader = "";
    sourceHeader += "#define SCREEN_WIDTH  " + std::to_string(xSize) + "\n";
    sourceHeader += "#define SCREEN_HEIGHT " + std::to_string(ySize) + "\n";
    sourceHeader += "#define ABUFFER_SINGLE_LINKED     " + std::to_string(ABUFFER_SINGLE_LINKED) + "\n";
    sourceHeader += "#define ABUFFER_FIXED             " + std::to_string(ABUFFER_FIXED) + "\n";
    sourceHeader += "#define ABUFFER_DYNAMIC           " + std::to_string(ABUFFER_DYNAMIC) + "\n";
    sourceHeader += "#define ABUFFER_IMPLEMENTATION    " + std::to_string(ABUFFER_IMPLEMENTATION) + "\n";
    _shaderBuilder.createSourceFile(true);
    _shaderBuilder.sourceFileHeader(sourceHeader);

    // Register the filepaths from static function enables easy testing
    // registerFilePaths();
    _context.createContextFromGLContext();

    // Detect and log OpenCL and OpenGL versions and available devices
    ghoul::systemcapabilities::SystemCapabilities::initialize();
    SysCap.addComponent(new ghoul::systemcapabilities::CPUCapabilitiesComponent);
    SysCap.addComponent(new ghoul::systemcapabilities::OpenCLCapabilitiesComponent);
    SysCap.addComponent(new ghoul::systemcapabilities::OpenGLCapabilitiesComponent);
    SysCap.detectCapabilities();
    SysCap.logCapabilities();

	std::string timeKernel = "";
	using constants::openspaceengine::keyConfigTimekernel;
	if (OsEng.configurationManager().hasKeyAndValue<std::string>(keyConfigTimekernel)) {
		OsEng.configurationManager().getValue(keyConfigTimekernel, timeKernel);
	}

    // initialize OpenSpace helpers
	SpiceManager::initialize();
    Time::initialize(timeKernel);
    Spice::init();
    Spice::ref().loadDefaultKernels();
    FactoryManager::initialize();


    scriptEngine().initialize();

	// Register Lua script functions
	LDEBUG("Registering Lua libraries");
	scriptEngine().addLibrary(SceneGraph::luaLibrary());
	scriptEngine().addLibrary(Time::luaLibrary());

    // Load scenegraph
    SceneGraph* sceneGraph = new SceneGraph;
    _renderEngine->setSceneGraph(sceneGraph);
    if (!OsEng.configurationManager().hasValue<std::string>(
              constants::openspaceengine::keyConfigScene)) {
        LFATAL("Configuration needs to point to the scene file");
        return false;
    }

    std::string sceneDescriptionPath;
    bool success = _configurationManager->getValue(
          constants::openspaceengine::keyConfigScene, sceneDescriptionPath);

    if (!FileSys.fileExists(sceneDescriptionPath)) {
        LFATAL("Could not find '" << sceneDescriptionPath << "'");
        return false;
    }

    std::string scenePath;
    success = _configurationManager->getValue(constants::openspaceengine::keyPathScene, scenePath);
    if (!success) {
        LFATAL("Could not find SCENEPATH key in configuration file");
        return false;
    }

    // initialize the RenderEngine, needs ${SCENEPATH} to be set
    _renderEngine->initialize();
    sceneGraph->loadScene(sceneDescriptionPath, scenePath);
    sceneGraph->initialize();

#ifdef FLARE_ONLY
    _flare = new Flare();
    _flare->initialize();
#endif

    // Initialize OpenSpace input devices
    DeviceIdentifier::init();
    DeviceIdentifier::ref().scanDevices();
    _engine->_interactionHandler->connectDevices();

    // Run start up scripts
    using ghoul::Dictionary;
    using constants::openspaceengine::keyStartupScript;
    const bool hasScript = _engine->configurationManager().hasKeyAndValue<Dictionary>(keyStartupScript);
    if (hasScript) {
        Dictionary scripts;
        _engine->configurationManager().getValue(keyStartupScript, scripts);
        
        for (size_t i = 0; i < scripts.size(); ++i) {
            std::stringstream stream;
            // Dictionary-size is 0-based; script numbers are 1-based
            stream << (i + 1);
            const std::string& key = stream.str();
            const bool hasKey = scripts.hasKeyAndValue<std::string>(key);
            if (!hasKey) {
                LERROR("The startup scripts have to be declared in a simple array format");
                break;
            }
            
            std::string scriptPath;
            scripts.getValue(key, scriptPath);
            const std::string absoluteScriptPath = absPath(scriptPath);
            _engine->scriptEngine().runScriptFile(absoluteScriptPath);
        }
    }

#ifdef OPENSPACE_VIDEO_EXPORT
    LINFO("OpenSpace compiled with video export; press Print Screen to start/stop recording");
#endif

    return true;
}

ghoul::Dictionary& OpenSpaceEngine::configurationManager()
{
    // TODO custom assert (ticket #5)
    assert(_configurationManager);
    return *_configurationManager;
}

ghoul::opencl::CLContext& OpenSpaceEngine::clContext()
{
    return _context;
}

InteractionHandler& OpenSpaceEngine::interactionHandler()
{
    // TODO custom assert (ticket #5)
    assert(_interactionHandler);
    return *_interactionHandler;
}

RenderEngine& OpenSpaceEngine::renderEngine()
{
    // TODO custom assert (ticket #5)
    assert(_renderEngine);
    return *_renderEngine;
}

ScriptEngine& OpenSpaceEngine::scriptEngine()
{
    // TODO custom assert (ticket #5)
    assert(_scriptEngine);
    return *_scriptEngine;
}


ShaderCreator& OpenSpaceEngine::shaderBuilder()
{
    // TODO custom assert (ticket #5)
    return _shaderBuilder;
}

bool OpenSpaceEngine::initializeGL()
{
    return _renderEngine->initializeGL();
}

void OpenSpaceEngine::preSynchronization()
{
#ifdef WIN32
    // Sleeping for 0 milliseconds will trigger any pending asynchronous procedure calls 
    SleepEx(0, TRUE);
#endif
    if (sgct::Engine::instance()->isMaster()) {
        const double dt = sgct::Engine::instance()->getDt();

        _interactionHandler->update(dt);
        _interactionHandler->lockControls();

		Time::ref().advanceTime(dt);
    }
#ifdef FLARE_ONLY
    _flare->preSync();
#endif
}

void OpenSpaceEngine::postSynchronizationPreDraw()
{
    _renderEngine->postSynchronizationPreDraw();
#ifdef FLARE_ONLY
    _flare->postSyncPreDraw();
#endif
}

void OpenSpaceEngine::render()
{
#ifdef FLARE_ONLY
    _flare->render();
#else 
    _renderEngine->render();
#endif
}

void OpenSpaceEngine::postDraw()
{
    if (sgct::Engine::instance()->isMaster()) {
        _interactionHandler->unlockControls();
    }
#ifdef OPENSPACE_VIDEO_EXPORT
    float speed = 0.01;
    glm::vec3 euler(0.0, speed, 0.0);
    glm::quat rot = glm::quat(euler);
    glm::vec3 euler2(0.0, -speed, 0.0);
    glm::quat rot2 = glm::quat(euler2);
    _interactionHandler->orbit(rot);
    _interactionHandler->rotate(rot2);
    if(_doVideoExport)
        sgct::Engine::instance()->takeScreenshot();
#endif
#ifdef FLARE_ONLY
    _flare->postDraw();
#endif
}

void OpenSpaceEngine::keyboardCallback(int key, int action)
{
    if (sgct::Engine::instance()->isMaster()) {
        _interactionHandler->keyboardCallback(key, action);
    }
#ifdef OPENSPACE_VIDEO_EXPORT
    // LDEBUG("key: " << key);
    // LDEBUG("SGCT_KEY_PRINT_SCREEN: " << SGCT_KEY_PRINT_SCREEN);
    if(action == SGCT_PRESS && key == SGCT_KEY_PRINT_SCREEN) 
        _doVideoExport = !_doVideoExport;
#endif
#ifdef FLARE_ONLY
    _flare->keyboard(key, action);
#endif
}

void OpenSpaceEngine::mouseButtonCallback(int key, int action)
{
    if (sgct::Engine::instance()->isMaster()) {
        _interactionHandler->mouseButtonCallback(key, action);
    }
#ifdef FLARE_ONLY
    _flare->mouse(key, action);
#endif
}

void OpenSpaceEngine::mousePositionCallback(int x, int y)
{
    _interactionHandler->mousePositionCallback(x, y);
}

void OpenSpaceEngine::mouseScrollWheelCallback(int pos)
{
    _interactionHandler->mouseScrollWheelCallback(pos);
}

void OpenSpaceEngine::encode()
{
#ifdef FLARE_ONLY
    _flare->encode();
#else
    std::vector<char> dataStream(1024);

    size_t offset = 0;
    // serialization
    _renderEngine->serialize(dataStream, offset);

    _synchronizationBuffer.setVal(dataStream);
    sgct::SharedData::instance()->writeVector(&_synchronizationBuffer);
#endif
}

void OpenSpaceEngine::decode()
{
#ifdef FLARE_ONLY
    _flare->decode();
#else
    sgct::SharedData::instance()->readVector(&_synchronizationBuffer);
    std::vector<char> dataStream = std::move(_synchronizationBuffer.getVal());
    size_t offset = 0;

    // deserialize in the same order as done in serialization
    _renderEngine->deserialize(dataStream, offset);
#endif
}

void OpenSpaceEngine::externalControlCallback(const char* receivedChars,
                                              int size, int clientId)
{
	if (size == 0)
		return;

	// The first byte determines the type of message
	const char type = receivedChars[0];
	switch (type) {
		case '0':  // LuaScript
		{
			std::string script = std::string(receivedChars + 1);
			LINFO("Received Lua Script: '" << script << "'");
			_scriptEngine->runScript(script);
		}
	}


}

}  // namespace openspace
