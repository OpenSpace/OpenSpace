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

#include <openspace/interaction/deviceidentifier.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
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

#include <iostream>
#include <fstream>

using namespace ghoul::filesystem;
using namespace ghoul::logging;

using namespace openspace::scripting;

namespace {
    const std::string _loggerCat = "OpenSpaceEngine";
    const std::string _configurationFile = "openspace.cfg";
    const std::string _sgctDefaultConfigFile = "${SGCT}/single.xml";
    
    const std::string _sgctConfigArgumentCommand = "-config";

#ifdef WIN32
    const unsigned int CommandInputButton = SGCT_KEY_BACKSLASH;     // Button left of 1 and abobe TAB
    const unsigned int IgnoreCodepoint = 167;                       // Correesponding codepoint
#else
    const unsigned int CommandInputButton = SGCT_KEY_GRAVE_ACCENT;  // Button left of 1 and abobe TAB
    const unsigned int IgnoreCodepoint = 167;                       // Correesponding codepoint

    // Dangerus as fuck
    bool exec(const std::string& cmd, std::string& value)
    {
      FILE* pipe = popen(cmd.c_str(), "r");
      if (!pipe) 
        return false;

      const int buffer_size = 1024;
      char buffer[buffer_size];
      value = "";
      while(!feof(pipe))
      {
        if(fgets(buffer, buffer_size, pipe) != NULL)
        {
          value += buffer;
        }
      }
      pclose(pipe);
      return true;
    }
#endif
    
    struct {
        std::string configurationName;
    } commandlineArgumentPlaceholders;

	// TODO: Put this functio nsomewhere appropriate
	// get text from clipboard
	std::string getClipboardText()
	{
#ifdef WIN32
		// Try opening the clipboard
		if (!OpenClipboard(nullptr))
			return "";

		// Get handle of clipboard object for ANSI text
		HANDLE hData = GetClipboardData(CF_TEXT);
		if (hData == nullptr)
			return "";

		// Lock the handle to get the actual text pointer
		char * pszText = static_cast<char*>(GlobalLock(hData));
		if (pszText == nullptr)
			return "";

		// Save text in a string class instance
		std::string text(pszText);

		// Release the lock
		GlobalUnlock(hData);

		// Release the clipboard
		CloseClipboard();

		text.erase(std::remove(text.begin(), text.end(), '\r'), text.end());
		return text;
#else
        std::string text;
        if(exec("xclip -o -sel c -f", text))
            return text.substr(0, text.length()-1);
        return ""; // remove a line ending

#endif
	}

	// TODO: Put this function somewhere appropriate
	// set text to clipboard
	bool setClipboardText(std::string text)
	{
#ifdef WIN32
		char *ptrData = nullptr;
		HANDLE hData = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, text.length() + 1);

		ptrData = (char*)GlobalLock(hData);
		memcpy(ptrData, text.c_str(), text.length() + 1);

		GlobalUnlock(hData);

		if (!OpenClipboard(nullptr))
			return false;

		if (!EmptyClipboard())
			return false;

		SetClipboardData(CF_TEXT, hData);

		CloseClipboard();

		return true;
#else
        std::stringstream cmd;
        cmd << "echo \"" << text << "\" | xclip -i -sel c -f";
        std::string buf;
        return exec(cmd.str(), buf);
#endif
	}

	std::string UnicodeToUTF8(unsigned int codepoint){
		std::string out;

		if (codepoint <= 0x7f)
			out.append(1, static_cast<char>(codepoint));
		else if (codepoint <= 0x7ff)
		{
			out.append(1, static_cast<char>(0xc0 | ((codepoint >> 6) & 0x1f)));
			out.append(1, static_cast<char>(0x80 | (codepoint & 0x3f)));
		}
		else if (codepoint <= 0xffff)
		{
			out.append(1, static_cast<char>(0xe0 | ((codepoint >> 12) & 0x0f)));
			out.append(1, static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f)));
			out.append(1, static_cast<char>(0x80 | (codepoint & 0x3f)));
		}
		else
		{
			out.append(1, static_cast<char>(0xf0 | ((codepoint >> 18) & 0x07)));
			out.append(1, static_cast<char>(0x80 | ((codepoint >> 12) & 0x3f)));
			out.append(1, static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f)));
			out.append(1, static_cast<char>(0x80 | (codepoint & 0x3f)));
		}
		return out;
	}
}

using namespace ghoul::cmdparser;

namespace openspace {

OpenSpaceEngine* OpenSpaceEngine::_engine = nullptr;

OpenSpaceEngine::OpenSpaceEngine(std::string programName)
	: _commandlineParser(programName, true)
	, _inputCommand(false)
	, _inputPosition(0)
	, _activeCommand(0)
	, _commands({""})
{
}

OpenSpaceEngine::~OpenSpaceEngine() {
	SpiceManager::deinitialize();
    Time::deinitialize();
    DeviceIdentifier::deinit();
    FileSystem::deinitialize();
    LogManager::deinitialize();
}

OpenSpaceEngine& OpenSpaceEngine::ref() {
    assert(_engine);
    return *_engine;
}
    
bool OpenSpaceEngine::gatherCommandlineArguments() {
    // TODO: Get commandline arguments from all modules

    CommandlineCommand* configurationFileCommand = new SingleCommand<std::string>(
          &commandlineArgumentPlaceholders.configurationName, "-config", "-c",
          "Provides the path to the OpenSpace configuration file");
    _commandlineParser.addCommand(configurationFileCommand);
    
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
	LINFO("Configuration Path: '" << FileSys.absolutePath(configurationFilePath) << "'");

    // Loading configuration from disk
    LDEBUG("Loading configuration from disk");
	const bool configLoadSuccess = _engine->configurationManager().loadFromFile(
																configurationFilePath);
	if (!configLoadSuccess) {
		LERROR("Loading of configuration file '" << configurationFilePath << "' failed");
		return false;
	}

	// make sure cache is registered, false since we don't want to override
	FileSys.registerPathToken("${CACHE}", "${BASE_PATH}/cache", false);

	// Create directories that doesn't exsist
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
	FileSys.createCacheManager("${CACHE}");

    // Determining SGCT configuration file
    LDEBUG("Determining SGCT configuration file");
    std::string sgctConfigurationPath = _sgctDefaultConfigFile;
    _engine->configurationManager().getValue(
        constants::configurationmanager::keyConfigSgct, sgctConfigurationPath);

    // Prepend the outgoing sgctArguments with the program name
    // as well as the configuration file that sgct is supposed to use
    sgctArguments.insert(sgctArguments.begin(), argv[0]);
    sgctArguments.insert(sgctArguments.begin() + 1, _sgctConfigArgumentCommand);
    sgctArguments.insert(sgctArguments.begin() + 2, absPath(sgctConfigurationPath));
    
    return true;
}

void OpenSpaceEngine::destroy() {
    delete _engine;
}

bool OpenSpaceEngine::isInitialized() {
    return _engine != nullptr;
}

bool OpenSpaceEngine::initialize() {
    // clear the screen so the user don't have to see old buffer contents from the
    // graphics card
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    GLFWwindow* win = sgct::Engine::instance()->getActiveWindowPtr()->getWindowHandle();
    glfwSwapBuffers(win);
    //int samples = sqrt(sgct::Engine::instance()->getActiveWindowPtr()->getNumberOfAASamples());
    //LDEBUG("samples: " << samples);


    // Register the filepaths from static function enables easy testing
    // registerFilePaths();
    _context.createContextFromGLContext();

    // Detect and log OpenCL and OpenGL versions and available devices
    ghoul::systemcapabilities::SystemCapabilities::initialize();
    SysCap.addComponent(new ghoul::systemcapabilities::CPUCapabilitiesComponent);
    //SysCap.addComponent(new ghoul::systemcapabilities::OpenCLCapabilitiesComponent);
    SysCap.addComponent(new ghoul::systemcapabilities::OpenGLCapabilitiesComponent);
    SysCap.detectCapabilities();
    SysCap.logCapabilities();

    // initialize OpenSpace helpers
	SpiceManager::initialize();
    Time::initialize();
	
	// Load SPICE time kernel
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
	
	//// metakernel loading doesnt seem to work... it should. to tired to even
	//// CK
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/ck/merged_nhpc_2006_v011.bc");
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/ck/merged_nhpc_2007_v006.bc");
	//// FK													
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/fk/nh_v200.tf");
	//// IK													
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/ik/nh_lorri_v100.ti");
	//// LSK already loaded									
	////PCK													
	////SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/pck/pck00008.tpc");
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/pck/new_horizons_413.tsc");
	////SPK												
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/spk/de413.bsp");
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/spk/jup260.bsp");
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/spk/nh_nep_ura_000.bsp");
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/spk/nh_recon_e2j_v1.bsp");
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/spk/nh_recon_j2sep07_prelimv1.bsp");
	//SpiceManager::ref().loadKernel("${OPENSPACE_DATA}/spice/JupiterNhKernels/spk/sb_2002jf56_2.bsp");
	
    FactoryManager::initialize();

    scriptEngine().initialize();

	// Register Lua script functions
	LDEBUG("Registering Lua libraries");
	scriptEngine().addLibrary(RenderEngine::luaLibrary());
	scriptEngine().addLibrary(SceneGraph::luaLibrary());
	scriptEngine().addLibrary(Time::luaLibrary());
	scriptEngine().addLibrary(InteractionHandler::luaLibrary());

    // Load scenegraph
    SceneGraph* sceneGraph = new SceneGraph;
    _renderEngine.setSceneGraph(sceneGraph);
	
    // initialize the RenderEngine, needs ${SCENEPATH} to be set
	_renderEngine.initialize();
	sceneGraph->initialize();

    std::string sceneDescriptionPath;
	success = configurationManager().getValue(
		constants::configurationmanager::keyConfigScene, sceneDescriptionPath);
	if (success)
	    sceneGraph->scheduleLoadSceneFile(sceneDescriptionPath);

    // Initialize OpenSpace input devices
    DeviceIdentifier::init();
    DeviceIdentifier::ref().scanDevices();
    _interactionHandler.connectDevices();

    // Run start up scripts
    ghoul::Dictionary scripts;
	success = configurationManager().getValue(
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

	// Load a light and a monospaced font
	//sgct_text::FontManager::instance()->addFont(constants::fonts::keyMono, "ubuntu-font-family/UbuntuMono-R.ttf", absPath("${FONTS}/"));
	//sgct_text::FontManager::instance()->addFont(constants::fonts::keyLight, "ubuntu-font-family/Ubuntu-L.ttf", absPath("${FONTS}/"));
	sgct_text::FontManager::FontPath local = sgct_text::FontManager::FontPath::FontPath_Local;
	sgct_text::FontManager::instance()->addFont(constants::fonts::keyMono, absPath("${FONTS}/Droid_Sans_Mono/DroidSansMono.ttf"), local);
	sgct_text::FontManager::instance()->addFont(constants::fonts::keyLight, absPath("${FONTS}/Roboto/Roboto-Regular.ttf"), local);

    return true;
}

ConfigurationManager& OpenSpaceEngine::configurationManager() {
    return _configurationManager;
}

ghoul::opencl::CLContext& OpenSpaceEngine::clContext() {
    return _context;
}

InteractionHandler& OpenSpaceEngine::interactionHandler() {
    return _interactionHandler;
}

RenderEngine& OpenSpaceEngine::renderEngine() {
    return _renderEngine;
}

ScriptEngine& OpenSpaceEngine::scriptEngine() {
    return _scriptEngine;
}

bool OpenSpaceEngine::initializeGL()
{
    return _renderEngine.initializeGL();
}

void OpenSpaceEngine::preSynchronization() {
#ifdef WIN32
    // Sleeping for 0 milliseconds will trigger any pending asynchronous procedure calls 
    SleepEx(0, TRUE);
#endif
    if (sgct::Engine::instance()->isMaster()) {
        const double dt = sgct::Engine::instance()->getDt();

        _interactionHandler.update(dt);
        _interactionHandler.lockControls();

		Time::ref().advanceTime(dt);
    }
}

void OpenSpaceEngine::postSynchronizationPreDraw() {
    _renderEngine.postSynchronizationPreDraw();
}

void OpenSpaceEngine::render() {
    _renderEngine.render();

	// If currently writing a command, render it to screen
	sgct::SGCTWindow* w = sgct::Engine::instance()->getActiveWindowPtr();
	if (sgct::Engine::instance()->isMaster() && !w->isUsingFisheyeRendering() && _inputCommand) {
		renderActiveCommand();
	}
}

void OpenSpaceEngine::renderActiveCommand() {

	const int font_size = 10;
	int x1, xSize, y1, ySize;
	sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewportPixelCoords(x1, y1, xSize, ySize);
	int startY = ySize - 2 * font_size;
	startY = startY - font_size * 10 * 2;

	const int font_with = font_size*0.7;
	const glm::vec4 red(1, 0, 0, 1);
	const glm::vec4 green(0, 1, 0, 1);
	const glm::vec4 white(1, 1, 1, 1);
	const sgct_text::Font* font = sgct_text::FontManager::instance()->getFont(constants::fonts::keyMono, font_size);
	Freetype::print(font, 10, startY, red, "$");
	Freetype::print(font, 10 + font_size, startY, white, "%s", _commands.at(_activeCommand).c_str());

	size_t n = std::count(_commands.at(_activeCommand).begin(), _commands.at(_activeCommand).begin() + _inputPosition, '\n');
	size_t p = _commands.at(_activeCommand).find_last_of('\n', _inputPosition);
	size_t linepos = _inputPosition;

	if (n>0) {
		if (p == _inputPosition) {
			p = _commands.at(_activeCommand).find_last_of('\n', _inputPosition - 1);
			if (p != std::string::npos) {
				linepos -= p + 1;
			}
			else {
				linepos = _inputPosition - 1;
			}
		}
		else{
			linepos -= p + 1;
		}
	}
	char buffer[10];
	sprintf(buffer, "%%%lus", linepos + 1);
	Freetype::print(font, 10 + static_cast<float>(font_size)*0.5, startY - (font_size)*(n + 1)*3.0 / 2.0, green, buffer, "^");
}

void OpenSpaceEngine::postDraw() {
    if (sgct::Engine::instance()->isMaster())
        _interactionHandler.unlockControls();

#ifdef OPENSPACE_VIDEO_EXPORT
    float speed = 0.01;
    glm::vec3 euler(0.0, speed, 0.0);
    glm::quat rot = glm::quat(euler);
    glm::vec3 euler2(0.0, -speed, 0.0);
    glm::quat rot2 = glm::quat(euler2);
    _interactionHandler->orbit(rot);
    _interactionHandler->rotate(rot2);
	if(_doVideoExport)
		_renderEngine.takeScreenshot();
#endif
	_renderEngine.postDraw();

#ifdef FLARE_ONLY
    _flare->postDraw();
#endif
}

void OpenSpaceEngine::addToCommand(std::string c) {
	size_t length = c.length();
	_commands.at(_activeCommand).insert(_inputPosition, c);
	_inputPosition += length;
}

void OpenSpaceEngine::keyboardCallback(int key, int action) {
	if (sgct::Engine::instance()->isMaster()) {

		if (key == CommandInputButton && (action == SGCT_PRESS || action == SGCT_REPEAT)) {
			_inputCommand = !_inputCommand;
		}

		if (!_inputCommand) {
			_interactionHandler.keyboardCallback(key, action);
		}
		else {
			handleCommandInput(key, action);
		}
	}
}

void OpenSpaceEngine::handleCommandInput(int key, int action) {
	if (action == SGCT_PRESS || action == SGCT_REPEAT) {
		const size_t windowIndex = sgct::Engine::instance()->getFocusedWindowIndex();
		const bool mod_CONTROL = sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_LEFT_CONTROL) ||
			sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_RIGHT_CONTROL);
		const bool mod_SHIFT = sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_LEFT_SHIFT) ||
			sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_RIGHT_SHIFT);

		// Paste from clipboard
		if (key == SGCT_KEY_V) {
			if (mod_CONTROL) {
				addToCommand(getClipboardText());
			}
		}

		// Copy to clipboard
		if (key == SGCT_KEY_C) {
			if (mod_CONTROL) {
				setClipboardText(_commands.at(_activeCommand));
			}
		}

		// Go to the previous character
		if (key == SGCT_KEY_LEFT) {
			if (_inputPosition > 0)
				_inputPosition -= 1;
		}

		// Go to the next character
		if (key == SGCT_KEY_RIGHT) {
			if (_inputPosition < _commands.at(_activeCommand).length())
				++_inputPosition;
		}

		// Go to previous command
		if (key == SGCT_KEY_UP) {
			if (_activeCommand > 0)
				--_activeCommand;
			_inputPosition = _commands.at(_activeCommand).length();
		}

		// Go to next command (the last is empty)
		if (key == SGCT_KEY_DOWN) {
			if (_activeCommand < _commands.size()-1)
				++_activeCommand;
			_inputPosition = _commands.at(_activeCommand).length();
		}

		// Remove character before _inputPosition
		if (key == SGCT_KEY_BACKSPACE) {
			if (_inputPosition > 0) {
				_commands.at(_activeCommand).erase(_inputPosition - 1, 1);
				--_inputPosition;
			}
		}

		// Remove character after _inputPosition
		if (key == SGCT_KEY_DELETE) {
			if (_inputPosition <= _commands.at(_activeCommand).size()) {
				_commands.at(_activeCommand).erase(_inputPosition, 1);
			}
		}

		// Go to the beginning of command string
		if (key == SGCT_KEY_HOME) {
			_inputPosition = 0;
		}

		// Go to the end of command string
		if (key == SGCT_KEY_END) {
			_inputPosition = _commands.at(_activeCommand).size();
		}

		if (key == SGCT_KEY_ENTER) {

			// SHIFT+ENTER == new line
			if (mod_SHIFT) {
				addToCommand("\n");
			}
			// CTRL+ENTER == Debug print the command
			else if (mod_CONTROL) {
				LDEBUG("Active command from next line:\n" << _commands.at(_activeCommand));
			}
			// ENTER == run lua script
			else {
				if (_commands.at(_activeCommand) != "") {

					_scriptEngine.runScript(_commands.at(_activeCommand));
					_commandsHistory.push_back(_commands.at(_activeCommand));
					_commands = _commandsHistory;
					_commands.push_back("");
					_activeCommand = _commands.size() - 1;
					_inputPosition = 0;
				}
				else {
					_commands = _commandsHistory;
					_commands.push_back("");
					_inputCommand = false;
				}
			}
		}
	}
}

void OpenSpaceEngine::charCallback(unsigned int codepoint) {

	// SGCT_KEY_BACKSLASH == 92 but that corresponds to codepoint 167
	if (_inputCommand && codepoint != IgnoreCodepoint) {

#ifndef WIN32
        const size_t windowIndex = sgct::Engine::instance()->getFocusedWindowIndex();
        const bool mod_CONTROL = sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_LEFT_CONTROL) ||
            sgct::Engine::instance()->getKey(windowIndex, SGCT_KEY_RIGHT_CONTROL);

        const int codepoint_C = 99;
        const int codepoint_V = 118;
        if(mod_CONTROL && (codepoint == codepoint_C || codepoint == codepoint_V)) {
            return;
        }
#endif
		addToCommand(UnicodeToUTF8(codepoint));
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
//#ifdef FLARE_ONLY
//    _flare->encode();
//#else
//    std::vector<char> dataStream(1024);
//
//    size_t offset = 0;
//    // serialization
//    _renderEngine->serialize(dataStream, offset);
//
//    _synchronizationBuffer.setVal(dataStream);
//    sgct::SharedData::instance()->writeVector(&_synchronizationBuffer);
//#endif
}

void OpenSpaceEngine::decode()
{
//#ifdef FLARE_ONLY
//    _flare->decode();
//#else
//    sgct::SharedData::instance()->readVector(&_synchronizationBuffer);
//    std::vector<char> dataStream = std::move(_synchronizationBuffer.getVal());
//    size_t offset = 0;
//
//    // deserialize in the same order as done in serialization
//    _renderEngine->deserialize(dataStream, offset);
//#endif
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
