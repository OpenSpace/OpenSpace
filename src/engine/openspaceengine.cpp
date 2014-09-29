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

using namespace ghoul::filesystem;
using namespace ghoul::logging;

using namespace openspace::scripting;

namespace {
    const std::string _loggerCat = "OpenSpaceEngine";
    const std::string _configurationFile = "openspace.cfg";
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
	: _commandlineParser(programName, true)
{
}

OpenSpaceEngine::~OpenSpaceEngine()
{
	SpiceManager::deinitialize();
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
    _commandlineParser.addCommand(configurationFileCommand);
    
    return true;
}

bool OpenSpaceEngine::findConfiguration(std::string& filename)
{
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
    //int samples = sqrt(sgct::Engine::instance()->getActiveWindowPtr()->getNumberOfAASamples());
    //LDEBUG("samples: " << samples);

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

    // initialize OpenSpace helpers
	SpiceManager::initialize();
    Time::initialize();

	using constants::configurationmanager::keySpiceTimeKernel;
	std::string timeKernel;
	bool success = OsEng.configurationManager().getValue(keySpiceTimeKernel, timeKernel);
	if (!success) {
		LERROR("Configuration file does not contain a '" << keySpiceTimeKernel << "'");
		return false;
	}
	SpiceManager::ref().loadKernel(std::move(timeKernel));

	using constants::configurationmanager::keySpiceLeapsecondKernel;
	std::string leapSecondKernel;
	success = OsEng.configurationManager().getValue(keySpiceLeapsecondKernel, leapSecondKernel);
	if (!success) {
		LERROR("Configuration file does not contain a '" << keySpiceLeapsecondKernel << "'");
		return false;
	}
	SpiceManager::ref().loadKernel(std::move(leapSecondKernel));
	
    FactoryManager::initialize();

    scriptEngine().initialize();

	// Register Lua script functions
	LDEBUG("Registering Lua libraries");
	scriptEngine().addLibrary(SceneGraph::luaLibrary());
	scriptEngine().addLibrary(Time::luaLibrary());

    // Load scenegraph
    SceneGraph* sceneGraph = new SceneGraph;
    _renderEngine.setSceneGraph(sceneGraph);
	


    // initialize the RenderEngine, needs ${SCENEPATH} to be set
	_renderEngine.initialize();
	sceneGraph->initialize();

    std::string sceneDescriptionPath;
	success = OsEng.configurationManager().getValue(
		constants::configurationmanager::keyConfigScene, sceneDescriptionPath);
	if (success)
	    sceneGraph->scheduleLoadSceneFile(sceneDescriptionPath);

    //_renderEngine.setSceneGraph(sceneGraph);

#ifdef FLARE_ONLY
    _flare = new Flare();
    _flare->initialize();
#endif

    // Initialize OpenSpace input devices
    DeviceIdentifier::init();
    DeviceIdentifier::ref().scanDevices();
    _engine->_interactionHandler.connectDevices();

    // Run start up scripts
    ghoul::Dictionary scripts;
	success = _engine->configurationManager().getValue(
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

#ifdef OPENSPACE_VIDEO_EXPORT
    LINFO("OpenSpace compiled with video export; press Print Screen to start/stop recording");
#endif

    return true;
}

ConfigurationManager& OpenSpaceEngine::configurationManager()
{
    return _configurationManager;
}

ghoul::opencl::CLContext& OpenSpaceEngine::clContext()
{
    return _context;
}

InteractionHandler& OpenSpaceEngine::interactionHandler()
{
    return _interactionHandler;
}

RenderEngine& OpenSpaceEngine::renderEngine()
{
    return _renderEngine;
}

ScriptEngine& OpenSpaceEngine::scriptEngine()
{
    return _scriptEngine;
}


ShaderCreator& OpenSpaceEngine::shaderBuilder()
{
    // TODO custom assert (ticket #5)
    return _shaderBuilder;
}

bool OpenSpaceEngine::initializeGL()
{
    return _renderEngine.initializeGL();
}

void OpenSpaceEngine::preSynchronization()
{
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
#ifdef FLARE_ONLY
    _flare->preSync();
#endif
}

void OpenSpaceEngine::postSynchronizationPreDraw()
{
    _renderEngine.postSynchronizationPreDraw();
#ifdef FLARE_ONLY
    _flare->postSyncPreDraw();
#endif
}

void OpenSpaceEngine::render()
{
#ifdef FLARE_ONLY
    _flare->render();
#else 
    _renderEngine.render();
#endif
}

void OpenSpaceEngine::postDraw()
{
    if (sgct::Engine::instance()->isMaster()) {
        _interactionHandler.unlockControls();
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
        _interactionHandler.keyboardCallback(key, action);
    }
#ifdef OPENSPACE_VIDEO_EXPORT
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
        _interactionHandler.mouseButtonCallback(key, action);
    }
#ifdef FLARE_ONLY
    _flare->mouse(key, action);
#endif
}

void OpenSpaceEngine::mousePositionCallback(int x, int y)
{
    _interactionHandler.mousePositionCallback(x, y);
}

void OpenSpaceEngine::mouseScrollWheelCallback(int pos)
{
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
