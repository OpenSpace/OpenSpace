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

#include "openspaceengine.h"

// sgct header has to be included before all others due to Windows header
#include "sgct.h"

#include "interaction/deviceidentifier.h"
#include "interaction/interactionhandler.h"
#include "rendering/renderengine.h"
#include "util/time.h"
#include "util//spice.h"

#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logging>

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
{

}

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
    _engine = new OpenSpaceEngine;

    LogManager::initialize(LogManager::LogLevel::Debug);
    LogMgr.addLog(new ConsoleLog);
    
	ghoul::filesystem::FileSystem::initialize();
    
#ifdef __WIN32__
    // Windows: Binary two folders down
	FileSys.registerPathToken("${BASE_PATH}", "../..");
#elif __APPLE_
    // OS X : Binary three folders down
	FileSys.registerPathToken("${BASE_PATH}", "../../..");
#else
    // Linux : Binary three folders down
	FileSys.registerPathToken("${BASE_PATH}", "..");
#endif
	FileSys.registerPathToken("${SCRIPTS}", "${BASE_PATH}/scripts");
	FileSys.registerPathToken("${OPENSPACE-DATA}", "${BASE_PATH}/openspace-data");
    
    // OLD
	//FileSys.registerPathToken("${SCRIPTS}", "${BASE_PATH}/openspace/scripts"); // FIX ME: tempoary path

    _engine->_configurationManager = new ghoul::ConfigurationManager;
    _engine->_configurationManager->initialize();
    
    newArgc = 3;
    newArgv = new char*[3];
    newArgv[0] = "prog";
    newArgv[1] = "-config";
#ifdef __WIN32__
    // Windows uses fixed path to OpenSpace data
	newArgv[2] = "C:/openspace/config/single.xml"; // FIX ME: tempoary path
#elif __APPLE__
    // OS X uses local path to OpenSpace data
    newArgv[2] = "../../../config/single.xml";
#else
	// Linux is is a bin folder
    newArgv[2] = "../config/single.xml";
#endif
    
    

    

    _engine->_renderEngine = new RenderEngine;

    _engine->_interactionHandler = new InteractionHandler;

}

void OpenSpaceEngine::destroy() {
    delete _engine;
}

bool OpenSpaceEngine::initialize() {
    //_configurationManager->initialize();
    //_configurationManager->loadConfiguration("${SCRIPTS}/config.lua");
    //_configurationManager->loadConfiguration("${SCRIPTS}/config2.lua");

    Time::init();
    Spice::init();
    Spice::ref().loadDefaultKernels();

    // TODO add scenegraph file name
    _renderEngine->initialize("");

    DeviceIdentifier::init();
    DeviceIdentifier::ref().scanDevices();

    _engine->_interactionHandler->connectDevices();

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
