/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/engine/settingsengine.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/util/openspacemodule.h>
#include <openspace/scene/scene.h>

#include <ghoul/ghoul.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <string>


namespace {
    const std::string _loggerCat = "SettingsEngine";
}


namespace openspace {



SettingsEngine::SettingsEngine()
    : _eyeSeparation("eyeSeparation", "Eye Separation" , 0.f, 0.f, 10.f)
    , _scenes("scenes", "Scene", properties::OptionProperty::DisplayType::DROPDOWN)
    , _showFrameNumber("showFrameNumber", "Show frame number", false)
    , _busyWaitForDecode("busyWaitForDecode", "Busy Wait for decode", false)
    , _logSGCTOutOfOrderErrors("logSGCTOutOfOrderErrors", "Log SGCT out-of-order", false)
    , _useDoubleBuffering("useDoubleBuffering", "Use double buffering", false)
{
    setName("Global Properties");
}

void SettingsEngine::initialize() {
    initEyeSeparation();
    initSceneFiles();
    initShowFrameNumber();
    initBusyWaitForDecode();
    initLogSGCTOutOfOrderErrors();
    initUseDoubleBuffering();
}
    
void SettingsEngine::setModules(std::vector<OpenSpaceModule*> modules) {
    for (OpenSpaceModule* m : modules) {
        addPropertySubOwner(m);
    }
}

void SettingsEngine::initEyeSeparation() {
    addProperty(_eyeSeparation);

    // Set interaction to change the window's (SGCT's) eye separation
    _eyeSeparation.onChange(
        [this]() { OsEng.windowWrapper().setEyeSeparationDistance(_eyeSeparation); });
}

void SettingsEngine::initShowFrameNumber() {
    addProperty(_showFrameNumber);

    _showFrameNumber.onChange(
        [this]() { OsEng.renderEngine().setShowFrameNumber(_showFrameNumber.value()); } );
}

void SettingsEngine::initBusyWaitForDecode() {
    addProperty(_busyWaitForDecode);
    _busyWaitForDecode.onChange(
        [this]() { 
        LINFO((_busyWaitForDecode.value() ? "Busy wait for decode" : "Async decode"));
    });
}

bool SettingsEngine::busyWaitForDecode() {
    return _busyWaitForDecode.value();
}

void SettingsEngine::initLogSGCTOutOfOrderErrors() {
    addProperty(_logSGCTOutOfOrderErrors);
    _logSGCTOutOfOrderErrors.onChange(
        [this]() {
        LINFO("Turn " << (_logSGCTOutOfOrderErrors.value() ? "on" : "off") << " SGCT out of order logging");
    });
}

bool SettingsEngine::logSGCTOutOfOrderErrors() {
    return _logSGCTOutOfOrderErrors.value();
}


void SettingsEngine::initUseDoubleBuffering() {
    addProperty(_useDoubleBuffering);
    _useDoubleBuffering.onChange(
        [this]() {
        LINFO("Turn " << (_useDoubleBuffering.value() ? "on" : "off") << " double buffering");
    });
}


bool SettingsEngine::useDoubleBuffering() {
    return _useDoubleBuffering.value();
}

void SettingsEngine::initSceneFiles() {
    addProperty(_scenes);

    // Load all matching files in the Scene
    // TODO: match regex with either with new ghoul readFiles or local code
    std::string sceneDir = "${SCENE}";
    std::vector<std::string> scenes = ghoul::filesystem::Directory(sceneDir).readFiles();
    for (std::size_t i = 0; i < scenes.size(); ++i) {
        std::size_t found = scenes[i].find_last_of("/\\");
        _scenes.addOption(i, scenes[i].substr(found+1));
    }

    // Set interaction to change ConfigurationManager and schedule the load
    _scenes.onChange(
        [this]() {
            std::string sceneFile = _scenes.getDescriptionByValue(_scenes);
            OsEng.configurationManager().setValue(
                ConfigurationManager::KeyConfigScene, sceneFile);
            OsEng.renderEngine().scene()->scheduleLoadSceneFile(sceneFile);
        }
    );
}

}
