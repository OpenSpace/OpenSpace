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
#include <openspace/util/time.h>
#include <openspace/util/spice.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/constants.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/systemcapabilities/systemcapabilities.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/commandlinecommand.h>

using namespace ghoul::filesystem;
using namespace ghoul::logging;

namespace {
    const std::string _loggerCat = "OpenSpaceEngine";
    const std::string _configurationFile = "openspace.cfg";
    const std::string _basePathToken = "${BASE_PATH}";
    const std::string _sgctDefaultConfigFile = "${SGCT}/single.xml";
}

namespace openspace {

OpenSpaceEngine* OpenSpaceEngine::_engine = nullptr;

OpenSpaceEngine::OpenSpaceEngine()
    : _configurationManager(nullptr)
    , _interactionHandler(nullptr)
    , _renderEngine(nullptr)
//, _scriptEngine(nullptr)
{
}

OpenSpaceEngine::~OpenSpaceEngine()
{
    delete _configurationManager;
    delete _interactionHandler;
    delete _renderEngine;

    // TODO deallocate script engine when starting to use it
    // delete _scriptEngine;

    Spice::deinit();
    Time::deinit();
    DeviceIdentifier::deinit();
    LogManager::deinitialize();
}

OpenSpaceEngine& OpenSpaceEngine::ref()
{
    assert(_engine);
    return *_engine;
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
            LDEBUG(fullKey << ": " << p);
            FileSys.registerPathToken(fullKey, p);
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
    if (!filename.empty()) {
        return FileSys.fileExists(filename);
    } else {
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
}

void OpenSpaceEngine::create(int argc, char** argv,
                             std::vector<std::string>& sgctArguments)
{
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
    if (!OpenSpaceEngine::findConfiguration(configurationFilePath)) {
        LFATAL("Could not find OpenSpace configuration file!");
        assert(false);
    }

    // create objects
    _engine = new OpenSpaceEngine;
    _engine->_renderEngine = new RenderEngine;
    _engine->_interactionHandler = new InteractionHandler;
    _engine->_configurationManager = new ghoul::Dictionary;


    LDEBUG("Registering base path");
    if (!OpenSpaceEngine::registerBasePathFromConfigurationFile(configurationFilePath)) {
        LFATAL("Could not register base path");
        assert(false);
    }

    ghoul::Dictionary& configuration = *(_engine->_configurationManager);
    ghoul::lua::loadDictionaryFromFile(configurationFilePath, configuration);
    if (configuration.hasKey(constants::openspaceengine::keyPaths)) {
        ghoul::Dictionary pathsDictionary;
        if (configuration.getValue(constants::openspaceengine::keyPaths, pathsDictionary))
            OpenSpaceEngine::registerPathsFromDictionary(pathsDictionary);
    }

    std::string sgctConfigurationPath = _sgctDefaultConfigFile;
    if (configuration.hasKey(constants::openspaceengine::keyConfigSgct))
        configuration.getValue(constants::openspaceengine::keyConfigSgct, sgctConfigurationPath);

    sgctArguments.push_back(argv[0]);
    sgctArguments.push_back("-config");
    sgctArguments.push_back(absPath(sgctConfigurationPath));

    for (int i = 1; i < argc; ++i)
        sgctArguments.push_back(argv[i]);
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

    // Register the filepaths from static function enables easy testing
    // registerFilePaths();
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

    return true;
}

ghoul::Dictionary& OpenSpaceEngine::configurationManager()
{
    // TODO custom assert (ticket #5)
    assert(_configurationManager != nullptr);
    return *_configurationManager;
}

ghoul::opencl::CLContext& OpenSpaceEngine::clContext()
{
    return _context;
}

InteractionHandler& OpenSpaceEngine::interactionHandler()
{
    // TODO custom assert (ticket #5)
    assert(_configurationManager != nullptr);
    return *_interactionHandler;
}

RenderEngine& OpenSpaceEngine::renderEngine()
{
    // TODO custom assert (ticket #5)
    assert(_configurationManager != nullptr);
    return *_renderEngine;
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
#ifdef FLARE_ONLY
    _flare->postDraw();
#endif
}

void OpenSpaceEngine::keyboardCallback(int key, int action)
{
    if (sgct::Engine::instance()->isMaster()) {
        _interactionHandler->keyboardCallback(key, action);
    }
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

}  // namespace openspace
