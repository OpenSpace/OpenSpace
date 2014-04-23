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
#include "sgct.h"

#include <openspace/interaction/deviceidentifier.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util//spice.h>
#include <openspace/util/factorymanager.h>



#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/misc/configurationmanager.h>
#include <ghoul/systemcapabilities/systemcapabilities.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/commandlinecommand.h>

#include <ghoul/opencl/clcontext.h>
#include <ghoul/opencl/clprogram.h>
#include <ghoul/opencl/clkernel.h>
#include <ghoul/opencl/clworksize.h>
#include <ghoul/opencl/clcommandqueue.h>

using namespace ghoul::filesystem;
using namespace ghoul::logging;

namespace {
    const std::string _loggerCat = "OpenSpaceEngine";
}

namespace openspace {

OpenSpaceEngine* OpenSpaceEngine::_engine = nullptr;

OpenSpaceEngine::OpenSpaceEngine()
    : _configurationManager(nullptr)
    , _interactionHandler(nullptr)
    , _renderEngine(nullptr)
    //, _scriptEngine(nullptr)
{}

OpenSpaceEngine::~OpenSpaceEngine() {
    delete _configurationManager;
    delete _interactionHandler;
    delete _renderEngine;
    
    // TODO deallocate scriptengine when starting to use it
    //delete _scriptEngine;

    Spice::deinit();
    Time::deinit();
    DeviceIdentifier::deinit();
    LogManager::deinitialize();
}

OpenSpaceEngine& OpenSpaceEngine::ref() {
    assert(_engine);
    return *_engine;
}

bool OpenSpaceEngine::registerPathsFromDictionary(const ghoul::Dictionary& dictionary) {
    auto path_keys = dictionary.keys();
    for(auto key: path_keys) {
        std::string p;
        if(dictionary.getValue(key, p)) {
            std::stringstream ss;
            ss << "${" << key << "}";
            LDEBUG(ss.str() << ": " << p);
            FileSys.registerPathToken(ss.str(), p);
        }
    }

    return true;
}
bool OpenSpaceEngine::registerBasePathFromConfigurationFile(const std::string& filename) {
    if( ! FileSys.fileExists(filename))
        return false;
    
    const std::string absolutePath = FileSys.absolutePath(filename);
    
    auto last = absolutePath.find_last_of("/");
    if(last == absolutePath.npos)
        return false;
    
    std::string basePath = absolutePath.substr(0, last);
    
    FileSys.registerPathToken("${BASE_PATH}", basePath);
    
    return true;
}

bool OpenSpaceEngine::findConfiguration(std::string& filename) {
    if (filename != "") {
        return FileSys.fileExists(filename);
    }
    std::string currentDirectory = FileSys.currentDirectory();
    size_t occurrences = std::count(currentDirectory.begin(), currentDirectory.end(), '/');
    
    std::string cfgname = "openspace.cfg";
    
    for (int i = 0; i < occurrences; ++i) {
        if(i > 0) {
            cfgname = "../" + cfgname;
        }
        
        if(FileSys.fileExists(cfgname))
            break;
    }
    if ( ! FileSys.fileExists(cfgname)) {
        return false;
    }
    
    filename = cfgname;
    
    return true;
}

void OpenSpaceEngine::create(int argc, char** argv, std::vector<std::string>& sgctArguments) {
    // TODO custom assert (ticket #5)
    assert(_engine == nullptr);
    
    // initialize ghoul logging
    LogManager::initialize(LogManager::LogLevel::Debug, true);
    LogMgr.addLog(new ConsoleLog);
    
    // TODO change so initialize is not called in the create function
    ghoul::filesystem::FileSystem::initialize();
    
    // TODO parse arguments if filename is specified, if not use default
    std::string configurationFilePath = "";
    
    LDEBUG("Finding configuration");
    if( ! OpenSpaceEngine::findConfiguration(configurationFilePath)) {
        LFATAL("Could not find OpenSpace configuration file!");
        assert(false);
    }
    
    LDEBUG("registering base path");
    if( ! OpenSpaceEngine::registerBasePathFromConfigurationFile(configurationFilePath)) {
        LFATAL("Could not register base path");
        assert(false);
    }
    
    ghoul::Dictionary configuration;
    ghoul::lua::loadDictionary(configurationFilePath, configuration);
    if(configuration.hasKey("paths")) {
        ghoul::Dictionary pathsDictionary;
        if(configuration.getValue("paths", pathsDictionary)) {
            OpenSpaceEngine::registerPathsFromDictionary(pathsDictionary);
        }
    }
    
    std::string sgctConfigurationPath = "${SGCT}/single.xml";
    if(configuration.hasKey("sgctConfig")) {
        configuration.getValue("sgctConfig", sgctConfigurationPath);
    }
    
    sgctArguments.push_back("OpenSpace");
    sgctArguments.push_back("-config");
    sgctArguments.push_back(absPath(sgctConfigurationPath));

    // create objects
    _engine = new OpenSpaceEngine;
    _engine->_renderEngine = new RenderEngine;
    _engine->_interactionHandler = new InteractionHandler;
    _engine->_configurationManager = new ghoul::ConfigurationManager;
}

void OpenSpaceEngine::destroy() {
    delete _engine;
}

bool OpenSpaceEngine::isInitialized() {
    return _engine != nullptr;
}

bool OpenSpaceEngine::initialize() {

    // Register the filepaths from static function enables easy testing
    //registerFilePaths();
    _context.createContextFromGLContext();

    // initialize the configurationmanager with the default configuration
    //_configurationManager->loadConfiguration(absPath("${SCRIPTS}/DefaultConfig.lua"));
    
    // Detect and log OpenCL and OpenGL versions and available devices
    ghoul::systemcapabilities::SystemCapabilities::initialize();
    SysCap.addComponent(new ghoul::systemcapabilities::CPUCapabilitiesComponent);
    SysCap.addComponent(new ghoul::systemcapabilities::OpenCLCapabilitiesComponent);
    SysCap.addComponent(new ghoul::systemcapabilities::OpenGLCapabilitiesComponent);
    SysCap.detectCapabilities();
    SysCap.logCapabilities();

    // initialize OpenSpace helpers
    Time::init();
    Spice::init();
    Spice::ref().loadDefaultKernels();
    FactoryManager::initialize();

    // TODO add scenegraph file name
    // initialize the RenderEngine, needs ${SCENEPATH} to be set
    _renderEngine->initialize();

    // Initialize OpenSPace input devices
    DeviceIdentifier::init();
    DeviceIdentifier::ref().scanDevices();
    _engine->_interactionHandler->connectDevices();

     //_flare = new Flare();
    
    return true;
}

ghoul::ConfigurationManager& OpenSpaceEngine::configurationManager() {
    // TODO custom assert (ticket #5)
    assert(_configurationManager != nullptr);
    return *_configurationManager;
}
    
ghoul::opencl::CLContext& OpenSpaceEngine::clContext() {
    return _context;
}

InteractionHandler& OpenSpaceEngine::interactionHandler() {
    // TODO custom assert (ticket #5)
    assert(_configurationManager != nullptr);
    return *_interactionHandler;
}

RenderEngine& OpenSpaceEngine::renderEngine() {
    // TODO custom assert (ticket #5)
    assert(_configurationManager != nullptr);
    return *_renderEngine;
}

bool OpenSpaceEngine::initializeGL() {
    return _renderEngine->initializeGL();
}

void OpenSpaceEngine::preSynchronization() {
    if (sgct::Engine::instance()->isMaster()) {
        const double dt = sgct::Engine::instance()->getDt();
        
        _interactionHandler->update(dt);
        _interactionHandler->lockControls();

        //_flare->preSync();
    }
}

void OpenSpaceEngine::postSynchronizationPreDraw() {
    _renderEngine->postSynchronizationPreDraw();
}

void OpenSpaceEngine::render() {
    //_flare->render();
    _renderEngine->render();
}

void OpenSpaceEngine::postDraw() {
    if (sgct::Engine::instance()->isMaster()) {
        _interactionHandler->unlockControls();
        //_flare->postDraw();
    }
}

void OpenSpaceEngine::keyboardCallback(int key, int action) {
	if (sgct::Engine::instance()->isMaster()) {
		_interactionHandler->keyboardCallback(key, action);
		//_flare->keyboard(key, action);
	}
}

void OpenSpaceEngine::mouseButtonCallback(int key, int action) {
	if (sgct::Engine::instance()->isMaster()) {
		_interactionHandler->mouseButtonCallback(key, action);
		//_flare->mouse(key, action);
	}
}

void OpenSpaceEngine::mousePositionCallback(int x, int y) {
    _interactionHandler->mousePositionCallback(x, y);
}

void OpenSpaceEngine::mouseScrollWheelCallback(int pos) {
    _interactionHandler->mouseScrollWheelCallback(pos);
}

void OpenSpaceEngine::encode() {
	//_flare->encode();
}

void OpenSpaceEngine::decode() {
	//_flare->decode();
}

} // namespace openspace
