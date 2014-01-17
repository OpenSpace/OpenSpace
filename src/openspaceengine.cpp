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

void OpenSpaceEngine::create(int& argc, char**& argv) {
    // TODO add parsing of configuration file, configuration file loading
    LogManager::initialize(LogManager::LogLevel::Debug);
    LogMgr.addLog(new ConsoleLog);

    ghoul::filesystem::FileSystem::initialize();
    //FileSys.registerPathToken("${BASE_PATH}", "../../..");
    FileSys.registerPathToken("${BASE_PATH}", "../..");
    FileSys.registerPathToken("${SCRIPTS}", "${BASE_PATH}/scripts");

    // TODO custom assert (ticket #5)
    assert(_engine == nullptr);
    _engine = new OpenSpaceEngine;
    _engine->_renderEngine = new RenderEngine;

    _engine->_interactionHandler = new InteractionHandler;

    _engine->_configurationManager = new ghoul::ConfigurationManager;
}

void OpenSpaceEngine::destroy() {
    delete _engine;
}

bool OpenSpaceEngine::initialize() {
    _configurationManager->initialize();
    _configurationManager->loadConfiguration("${SCRIPTS}/config.lua");
    _configurationManager->loadConfiguration("${SCRIPTS}/config2.lua");

    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);

    //ghoul::lua::lua_logStack(_configurationManager->_state);
    //
    //lua_getglobal(_configurationManager->_state, "printTable");
    //lua_getglobal(_configurationManager->_state, "config");
    //lua_pcall(_configurationManager->_state, 1, 0, NULL);

    int s = 0;
    char t = 0;
    long long u = 0;
    unsigned long d = 0;
    _configurationManager->setValue("key", s);
    _configurationManager->setValue("key", t);
    _configurationManager->setValue("key", u);
    _configurationManager->setValue("key", d);

    //LINFO("All Keys:");
    //std::vector<std::string> keys = _configurationManager->keys();
    //for (const std::string& k : keys) {
    //    LINFO(k);
    //}


    LINFO("Keys (t):");
    std::vector<std::string> keys = _configurationManager->keys("tt.t");
    for (const std::string& k : keys) {
        LINFO(k);
    }


    //LINFO("setting2");
    //_configurationManager->setValue("setting2", 5);

    //lua_getglobal(_configurationManager->_state, "printTable");
    //lua_getglobal(_configurationManager->_state, "config");
    //lua_pcall(_configurationManager->_state, 1, 0, NULL);

    //LINFO("t.s");
    //_configurationManager->setValue("t.s", 10);

    //lua_getglobal(_configurationManager->_state, "printTable");
    //lua_getglobal(_configurationManager->_state, "config");
    //lua_pcall(_configurationManager->_state, 1, 0, NULL);

    //LINFO("table.te.s.foo");
    //_configurationManager->setValue("table.te.s.foo", 12);

    //lua_getglobal(_configurationManager->_state, "printTable");
    //lua_getglobal(_configurationManager->_state, "config");
    //lua_pcall(_configurationManager->_state, 1, 0, NULL);


    //int v;
    //LINFO("setting2");
    //LARGE_INTEGER t1, t2;
    //QueryPerformanceCounter(&t1);
    //bool success = _configurationManager->getValue("setting2", v);
    //QueryPerformanceCounter(&t2);
    //LINFO("Get: " << ((t2.QuadPart - t1.QuadPart) / double(freq.QuadPart)) * 1000* 1000);
    //LINFO("Value:" << v);
    //QueryPerformanceCounter(&t1);
    //_configurationManager->setValue("setting2", 5);
    //QueryPerformanceCounter(&t2);
    //LINFO("Set: " << ((t2.QuadPart - t1.QuadPart) / double(freq.QuadPart)) * 1000* 1000);
    //success = _configurationManager->getValue("setting2", v);
    //LINFO("Value:" << v);

    ////_configurationManager->setValue("setting2", 5);
    //ghoul::lua::lua_logStack(_configurationManager->_state);
    ////success = _configurationManager->getValue("setting2", v);

    //LINFO("t.s");
    //QueryPerformanceCounter(&t1);
    //success = _configurationManager->getValue("t.s", v);
    //QueryPerformanceCounter(&t2);
    //LINFO(((t2.QuadPart - t1.QuadPart) / double(freq.QuadPart)) * 1000 * 1000);
    //ghoul::lua::lua_logStack(_configurationManager->_state);

    //LINFO("table.te.s.foo");
    //QueryPerformanceCounter(&t1);
    //success = _configurationManager->getValue("table.te.s.foo", v);
    //QueryPerformanceCounter(&t2);
    //LINFO(((t2.QuadPart - t1.QuadPart) / double(freq.QuadPart)) * 1000 * 1000);
    //ghoul::lua::lua_logStack(_configurationManager->_state);

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
	sharedDataInstance_->readDouble(shDouble);
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
