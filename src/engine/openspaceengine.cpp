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

// openspace
#include <openspace/engine/logfactory.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/keyboardcontroller.h>
#include <openspace/interaction/mousecontroller.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scenegraph/scenegraph.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/constants.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/syncbuffer.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/systemcapabilities/systemcapabilities.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>

// sgct
#define SGCT_WINDOWS_INCLUDE
#include <sgct.h>

// std
#include <iostream>
#include <fstream>

using namespace openspace::scripting;
using namespace ghoul::filesystem;
using namespace ghoul::logging;
using namespace ghoul::cmdparser;

namespace {
    const std::string _loggerCat = "OpenSpaceEngine";
    const std::string _configurationFile = "openspace.cfg";
    const std::string _sgctDefaultConfigFile = "${SGCT}/single.xml";
	const std::string _defaultCacheLocation = "${BASE_PATH}/cache";
    
    const std::string _sgctConfigArgumentCommand = "-config";
    
    struct {
        std::string configurationName;
		std::string sgctConfigurationName;
    } commandlineArgumentPlaceholders;
}


namespace openspace {

OpenSpaceEngine* OpenSpaceEngine::_engine = nullptr;

OpenSpaceEngine::OpenSpaceEngine(std::string programName)
	: _commandlineParser(programName, true)
    , _syncBuffer(nullptr)
{
	// initialize OpenSpace helpers
	SpiceManager::initialize();
	Time::initialize();
	FactoryManager::initialize();
	ghoul::systemcapabilities::SystemCapabilities::initialize();
}

OpenSpaceEngine::~OpenSpaceEngine() {
	ghoul::systemcapabilities::SystemCapabilities::deinitialize();
	FactoryManager::deinitialize();
	Time::deinitialize();
	SpiceManager::deinitialize();
}

OpenSpaceEngine& OpenSpaceEngine::ref() {
    assert(_engine);
    return *_engine;
}
   
void OpenSpaceEngine::clearAllWindows() {
	size_t n = sgct::Engine::instance()->getNumberOfWindows();
	for (size_t i = 0; i < n; ++i) {
		glClearColor(0, 0, 0, 0);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		GLFWwindow* win = sgct::Engine::instance()->getWindowPtr(i)->getWindowHandle();
		glfwSwapBuffers(win);
	}
	
}

bool OpenSpaceEngine::gatherCommandlineArguments() {
    // TODO: Get commandline arguments from all modules

	commandlineArgumentPlaceholders.configurationName = "";
    CommandlineCommand* configurationFileCommand = new SingleCommand<std::string>(
          &commandlineArgumentPlaceholders.configurationName, "-config", "-c",
          "Provides the path to the OpenSpace configuration file");
    _commandlineParser.addCommand(configurationFileCommand);

	commandlineArgumentPlaceholders.sgctConfigurationName = "";
	CommandlineCommand* sgctConfigFileCommand = new SingleCommand<std::string>(
		&commandlineArgumentPlaceholders.sgctConfigurationName, "-sgct", "-s",
		"Provides the path to the SGCT configuration file, overriding the value set in"
		"the OpenSpace configuration file");
	_commandlineParser.addCommand(sgctConfigFileCommand);
    
    return true;
}

bool OpenSpaceEngine::findConfiguration(std::string& filename) {
	using ghoul::filesystem::Directory;

	Directory directory = FileSys.currentDirectory();
	std::string configurationName = _configurationFile;

	while (true) {
		std::string&& fullPath = FileSys.pathByAppendingComponent(directory,
																  configurationName);
		bool exists = FileSys.fileExists(fullPath);
		if (exists) {
			filename = fullPath;
			return true;
		}

		Directory nextDirectory = directory.parentDirectory(true);

		if (directory.path() == nextDirectory.path())
			// We have reached the root of the file system and did not find the file
			return false;
		directory = nextDirectory;
	}
}

bool OpenSpaceEngine::loadSpiceKernels() {
	// Load time kernel
	using constants::configurationmanager::keySpiceTimeKernel;
	std::string timeKernel;
	bool success = configurationManager().getValue(keySpiceTimeKernel, timeKernel);
	if (!success) {
		LERROR("Configuration file does not contain a '" << keySpiceTimeKernel << "'");
		return false;
	}
	SpiceManager::KernelIdentifier id =
		SpiceManager::ref().loadKernel(timeKernel);
	if (id == SpiceManager::KernelFailed) {
		LERROR("Error loading time kernel '" << timeKernel << "'");
		return false;
	}

	// Load SPICE leap second kernel
	using constants::configurationmanager::keySpiceLeapsecondKernel;
	std::string leapSecondKernel;
	success = configurationManager().getValue(keySpiceLeapsecondKernel, leapSecondKernel);
	if (!success) {
		LERROR("Configuration file does not have a '" << keySpiceLeapsecondKernel << "'");
		return false;
	}
	id = SpiceManager::ref().loadKernel(std::move(leapSecondKernel));
	if (id == SpiceManager::KernelFailed) {
		LERROR("Error loading leap second kernel '" << leapSecondKernel << "'");
		return false;
	}
	return true;
}

void OpenSpaceEngine::runStartupScripts() {
	ghoul::Dictionary scripts;
	configurationManager().getValue(
		constants::configurationmanager::keyStartupScript, scripts);
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

void OpenSpaceEngine::loadFonts() {
	sgct_text::FontManager::FontPath local = sgct_text::FontManager::FontPath::FontPath_Local;

	ghoul::Dictionary fonts;
	configurationManager().getValue(constants::configurationmanager::keyFonts, fonts);

	for (auto key : fonts.keys()) {
		std::string font;
		fonts.getValue(key, font);
		font = absPath(font);

		LINFO("Registering font '" << font << "' with key '" << key << "'");
		sgct_text::FontManager::instance()->addFont(key, font, local);
	}
}

void OpenSpaceEngine::createLogs() {
	using constants::configurationmanager::keyLogs;

	if (configurationManager().hasKeyAndValue<ghoul::Dictionary>(keyLogs)) {
		ghoul::Dictionary logs;
		configurationManager().getValue(keyLogs, logs);

		for (size_t i = 1; i <= logs.size(); ++i) {
			ghoul::Dictionary logInfo;
			logs.getValue(std::to_string(i), logInfo);

			Log* log = LogFactory::createLog(logInfo);

			if (log)
				LogMgr.addLog(log);
		}
	}
}

bool OpenSpaceEngine::create(int argc, char** argv,
                             std::vector<std::string>& sgctArguments)
{
    // TODO custom assert (ticket #5)
    assert(_engine == nullptr);

	// Initialize the Logmanager and add the console log as this will be used every time
	// and we need a fall back if something goes wrong between here and when we add the
	// logs from the configuration file
    LogManager::initialize(LogManager::LogLevel::Debug, true);
    LogMgr.addLog(new ConsoleLog);
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
    _engine->_commandlineParser.setCommandLine(argc, argv, &sgctArguments);
    const bool executeSuccess = _engine->_commandlineParser.execute();
    if (!executeSuccess)
        return false;
    
    // Find configuration
    std::string configurationFilePath = commandlineArgumentPlaceholders.configurationName;
    if (configurationFilePath.empty()) {
        LDEBUG("Finding configuration");
        const bool findConfigurationSuccess =
            OpenSpaceEngine::findConfiguration(configurationFilePath);
        if (!findConfigurationSuccess) {
            LFATAL("Could not find OpenSpace configuration file!");
            return false;
        }
    }
	configurationFilePath = absPath(configurationFilePath);
	LINFO("Configuration Path: '" << configurationFilePath << "'");

    // Loading configuration from disk
    LDEBUG("Loading configuration from disk");
	const bool configLoadSuccess = _engine->configurationManager().loadFromFile(
																configurationFilePath);
	if (!configLoadSuccess) {
		LFATAL("Loading of configuration file '" << configurationFilePath << "' failed");
		return false;
	}

	// Initialize the requested logs from the configuration file
	_engine->createLogs();

	// Create directories that doesn't exist
	auto tokens = FileSys.tokens();
	for (auto token : tokens) {
		if (!FileSys.directoryExists(token)) {
			std::string p = absPath(token);
			LDEBUG("Directory '" << p <<"' does not exsist, creating.");
			if(FileSys.createDirectory(p, true))
				LERROR("Directory '" << p <<"' could not be created");
		}
	}

	// Create the cachemanager
	FileSys.createCacheManager(absPath("${" + constants::configurationmanager::keyCache + "}"));
	_engine->_console.loadHistory();

	_engine->_syncBuffer = new SyncBuffer(1024);

    // Determining SGCT configuration file
    LDEBUG("Determining SGCT configuration file");
    std::string sgctConfigurationPath = _sgctDefaultConfigFile;
    _engine->configurationManager().getValue(
        constants::configurationmanager::keyConfigSgct, sgctConfigurationPath);

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
	delete _engine;
	FileSystem::deinitialize();
	LogManager::deinitialize();
}

bool OpenSpaceEngine::isInitialized() {
    return _engine != nullptr;
}

bool OpenSpaceEngine::initialize() {
    // clear the screen so the user don't have to see old buffer contents from the
    // graphics card
	clearAllWindows();

    // Detect and log OpenCL and OpenGL versions and available devices
    SysCap.addComponent(new ghoul::systemcapabilities::CPUCapabilitiesComponent);
    SysCap.addComponent(new ghoul::systemcapabilities::OpenGLCapabilitiesComponent);
    SysCap.detectCapabilities();
    SysCap.logCapabilities();
	
	// Load SPICE time kernel
	bool success = loadSpiceKernels();
	if (!success)
		return false;

	// Register Lua script functions
	LDEBUG("Registering Lua libraries");
	_scriptEngine.addLibrary(RenderEngine::luaLibrary());
	_scriptEngine.addLibrary(SceneGraph::luaLibrary());
	_scriptEngine.addLibrary(Time::luaLibrary());
	_scriptEngine.addLibrary(interaction::InteractionHandler::luaLibrary());
	_scriptEngine.addLibrary(LuaConsole::luaLibrary());

	// TODO: Maybe move all scenegraph and renderengine stuff to initializeGL
	scriptEngine().initialize();

	// If a LuaDocumentationFile was specified, generate it now
	using constants::configurationmanager::keyLuaDocumentationType;
	using constants::configurationmanager::keyLuaDocumentationFile;
	const bool hasType = configurationManager().hasKey(keyLuaDocumentationType);
	const bool hasFile = configurationManager().hasKey(keyLuaDocumentationFile);
	if (hasType && hasFile) {
		std::string luaDocumentationType;
		configurationManager().getValue(keyLuaDocumentationType, luaDocumentationType);
		std::string luaDocumentationFile;
		configurationManager().getValue(keyLuaDocumentationFile, luaDocumentationFile);

		luaDocumentationFile = absPath(luaDocumentationFile);
		_scriptEngine.writeDocumentation(luaDocumentationFile, luaDocumentationType);
	}


	// Load scenegraph
    SceneGraph* sceneGraph = new SceneGraph;
    _renderEngine.setSceneGraph(sceneGraph);
	
    // initialize the RenderEngine
	_renderEngine.initialize();
	sceneGraph->initialize();

    std::string sceneDescriptionPath;
	success = configurationManager().getValue(
		constants::configurationmanager::keyConfigScene, sceneDescriptionPath);
	if (success)
	    sceneGraph->scheduleLoadSceneFile(sceneDescriptionPath);

    // Initialize OpenSpace input devices
    //DeviceIdentifier::init();
    //DeviceIdentifier::ref().scanDevices();

	_interactionHandler.setKeyboardController(new interaction::KeyboardControllerFixed);
	//_interactionHandler.setKeyboardController(new interaction::KeyboardControllerLua);
	_interactionHandler.setMouseController(new interaction::TrackballMouseController);

    // Run start up scripts
	runStartupScripts();

	// Load a light and a monospaced font
	loadFonts();

    return true;
}

ConfigurationManager& OpenSpaceEngine::configurationManager() {
    return _configurationManager;
}

interaction::InteractionHandler& OpenSpaceEngine::interactionHandler() {
    return _interactionHandler;
}

RenderEngine& OpenSpaceEngine::renderEngine() {
    return _renderEngine;
}

ScriptEngine& OpenSpaceEngine::scriptEngine() {
    return _scriptEngine;
}

LuaConsole& OpenSpaceEngine::console() {
	return _console;
}

bool OpenSpaceEngine::initializeGL()
{
    return _renderEngine.initializeGL();
}

void OpenSpaceEngine::preSynchronization() {
	FileSys.triggerFilesystemEvents();
    if (sgct::Engine::instance()->isMaster()) {
        const double dt = sgct::Engine::instance()->getDt();

        _interactionHandler.update(dt);
        _interactionHandler.lockControls();

		//Time::ref().advanceTime(dt);
    }
}

void OpenSpaceEngine::postSynchronizationPreDraw() {
    _renderEngine.postSynchronizationPreDraw();
}

void OpenSpaceEngine::render() {
    _renderEngine.render();

	// If currently writing a command, render it to screen
	sgct::SGCTWindow* w = sgct::Engine::instance()->getActiveWindowPtr();
	if (sgct::Engine::instance()->isMaster() && !w->isUsingFisheyeRendering() && _console.isVisible()) {
		_console.render();
	}
}


void OpenSpaceEngine::postDraw() {
    if (sgct::Engine::instance()->isMaster())
        _interactionHandler.unlockControls();

	_renderEngine.postDraw();
}

void OpenSpaceEngine::keyboardCallback(int key, int action) {
	if (sgct::Engine::instance()->isMaster()) {
		if (key == _console.commandInputButton() && (action == SGCT_PRESS || action == SGCT_REPEAT))
			_console.toggleVisibility();

		if (!_console.isVisible()) {
			_interactionHandler.keyboardCallback(key, action);
		}
		else {
			_console.keyboardCallback(key, action);
		}
	}
}

void OpenSpaceEngine::charCallback(unsigned int codepoint) {
	if (_console.isVisible()) {
		_console.charCallback(codepoint);
	}
}

void OpenSpaceEngine::mouseButtonCallback(int key, int action) {
    if (sgct::Engine::instance()->isMaster())
        _interactionHandler.mouseButtonCallback(key, action);
}

void OpenSpaceEngine::mousePositionCallback(int x, int y) {
    _interactionHandler.mousePositionCallback(x, y);
}

void OpenSpaceEngine::mouseScrollWheelCallback(int pos) {
    _interactionHandler.mouseScrollWheelCallback(pos);
}

void OpenSpaceEngine::encode()
{
	if (_syncBuffer) {
		_renderEngine.serialize(_syncBuffer);
		_syncBuffer->write();
	}
}

void OpenSpaceEngine::decode()
{
	if (_syncBuffer) {
		_syncBuffer->read();
		_renderEngine.deserialize(_syncBuffer);
	}
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
			_scriptEngine.runScript(script);
		}
	}
}

}  // namespace openspace
