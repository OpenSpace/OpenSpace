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

#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logging>
#include <ghoul/misc/configurationmanager.h>
#include <ghoul/systemcapabilities/systemcapabilities.h>

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
    , _scriptEngine(nullptr)
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

void OpenSpaceEngine::create(int argc, char** argv, int& newArgc, char**& newArgv) {
    // TODO custom assert (ticket #5)
    assert(_engine == nullptr);
    
    // set the arguments for SGCT
    newArgc = 3;
    newArgv = new char*[3];
    newArgv[0] = "prog";
    newArgv[1] = "-config";
    newArgv[2] = "../../config/single.xml";
#ifdef __WIN32__
    // Windows uses fixed path to OpenSpace data
	newArgv[2] = "../../config/single.xml";
#elif __APPLE__
    // OS X uses local path to OpenSpace data
    newArgv[2] = "../../../config/single.xml";
#else
	// Linux is is a bin folder
    newArgv[2] = "../config/single.xml";
#endif
    
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

    // initialize ghou logging
    LogManager::initialize(LogManager::LogLevel::Debug, true);
    LogMgr.addLog(new ConsoleLog);

    // Register the filepaths from static function enables easy testing
    ghoul::filesystem::FileSystem::initialize();
    registerFilePaths();
    
    // initialize the configurationmanager with the default configuration
    _configurationManager->initialize();
    _configurationManager->loadConfiguration(absPath("${SCRIPTS}/DefaultConfig.lua"));
    
    // Detect and logOpenCL and OpenGL versions and available devices
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
    
    return true;
}

bool OpenSpaceEngine::registerFilePaths() {

// Check if CMake have provided an base path.
#ifndef BASE_PATH
#ifdef __WIN32__
    // Windows: Binary two folders down
	#define BASE_PATH "../.."
#elif __APPLE__
    // OS X : Binary three folders down
    #define BASE_PATH "../../.."
#else
    // Linux : Binary three folders down
    #define BASE_PATH ".."
#endif
#endif

    FileSys.registerPathToken("${BASE_PATH}", BASE_PATH);
	FileSys.registerPathToken("${SCRIPTS}", "${BASE_PATH}/scripts");
	FileSys.registerPathToken("${OPENSPACE-DATA}", "${BASE_PATH}/openspace-data");
	FileSys.registerPathToken("${SCENEPATH}", "${OPENSPACE-DATA}/scene");
	FileSys.registerPathToken("${SHADERS}", "${BASE_PATH}/shaders");

    return true;
}

ghoul::ConfigurationManager& OpenSpaceEngine::configurationManager() {
    // TODO custom assert (ticket #5)
    assert(_configurationManager != nullptr);
    return *_configurationManager;
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
    }
}

void OpenSpaceEngine::postSynchronizationPreDraw() {
    _renderEngine->postSynchronizationPreDraw();
}

void OpenSpaceEngine::render() {
    _renderEngine->render();
}

void OpenSpaceEngine::postDraw() {
    if (sgct::Engine::instance()->isMaster())
        _interactionHandler->unlockControls();
}

void OpenSpaceEngine::keyboardCallback(int key, int action) {
    _interactionHandler->keyboardCallback(key, action);
}

void OpenSpaceEngine::mouseButtonCallback(int key, int action) {
    _interactionHandler->mouseButtonCallback(key, action);
}

void OpenSpaceEngine::mousePositionCallback(int x, int y) {
    _interactionHandler->mousePositionCallback(x, y);
}

void OpenSpaceEngine::mouseScrollWheelCallback(int pos) {
    _interactionHandler->mouseScrollWheelCallback(pos);
}


} // namespace openspace

/*
void RenderEngine::encode() {
	
	// allocate a sgct shared double for syncing
	sgct::SharedDouble *shDouble = new sgct::SharedDouble();

	// sync the time
	shDouble->setVal(masterTime_);
	sharedDataInstance_->writeDouble(shDouble);
	
	// check that the camera has been allocated
	if(mainCamera_ != nullptr) {

		// sync position
		psc campos = mainCamera_->getPosition();
		for(int i = 0; i < 4; i++) {
			shDouble->setVal(campos[i]);
			sharedDataInstance_->writeDouble(shDouble);
		}

		// sync view direction
		glm::quat camrot = mainCamera_->getRotation();
		for(int i = 0; i < 4; i++) {
			shDouble->setVal(camrot[i]);
			sharedDataInstance_->writeDouble(shDouble);
		}
	}
	
	// deallocate
	delete shDouble;
	
}

void RenderEngine::decode() {
	
	// allocate a sgct shared double for syncing
	sgct::SharedDouble *shDouble = new sgct::SharedDouble();
	
	// sync the time
	sharedDataInstance_->Double(shDouble);
	masterTime_ = shDouble->getVal();
	
	// check that the camera has been allocated
	if(mainCamera_ != nullptr) {

		// sync position
		psc campos;
		for(int i = 0; i < 4; i++) {
			sharedDataInstance_->readDouble(shDouble);
			campos[i] = shDouble->getVal();
		}
		mainCamera_->setPosition(campos);

		// sync view direction
		glm::quat camrot;
		for(int i = 0; i < 4; i++) {
			sharedDataInstance_->readDouble(shDouble);
			camrot[i] = static_cast<float>(shDouble->getVal());
		}
		mainCamera_->setRotation(camrot);
	}
	
	// deallocate
	delete shDouble;
	
}
*/
