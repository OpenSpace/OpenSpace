/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scene.h>


#include <ghoul/ghoul.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <string>


namespace {
    const char* _loggerCat = "SettingsEngine";
}

namespace openspace {

SettingsEngine::SettingsEngine()
    : properties::PropertyOwner("Global Properties")
    , _scenes("scenes", "Scene", properties::OptionProperty::DisplayType::Dropdown)
    , _busyWaitForDecode("busyWaitForDecode", "Busy Wait for decode", false)
    , _logSGCTOutOfOrderErrors("logSGCTOutOfOrderErrors", "Log SGCT out-of-order", false)
    , _useDoubleBuffering("useDoubleBuffering", "Use double buffering", false)
    , _spiceUseExceptions("enableSpiceExceptions", "Enable Spice Exceptions", false)
{
    _spiceUseExceptions.onChange([this] {
        if (_spiceUseExceptions) {
            SpiceManager::ref().setExceptionHandling(SpiceManager::UseException::Yes);
        } else {
            SpiceManager::ref().setExceptionHandling(SpiceManager::UseException::No);
        }
    });
    addProperty(_spiceUseExceptions);
    addProperty(_busyWaitForDecode);
    addProperty(_logSGCTOutOfOrderErrors);
    addProperty(_useDoubleBuffering);
    addProperty(_scenes);
}

void SettingsEngine::initialize() {
    // Load all matching files in the Scene
    // TODO: match regex with either with new ghoul readFiles or local code
    std::string sceneDir = "${SCENE}";
    std::vector<std::string> scenes = ghoul::filesystem::Directory(sceneDir).readFiles();
    for (std::size_t i = 0; i < scenes.size(); ++i) {
        std::size_t found = scenes[i].find_last_of("/\\");
        _scenes.addOption(i, scenes[i].substr(found + 1));
    }

    // Set interaction to change ConfigurationManager and schedule the load
    _scenes.onChange(
        [this]() {
        std::string sceneFile = _scenes.getDescriptionByValue(_scenes);
        OsEng.configurationManager().setValue(
            ConfigurationManager::KeyConfigScene, sceneFile);
        OsEng.scheduleLoadScene("${SCENE}/" + sceneFile);
    }
    );
}

void SettingsEngine::setModules(const std::vector<OpenSpaceModule*>& modules) {
    for (OpenSpaceModule* m : modules) {
        addPropertySubOwner(m);
    }
}

bool SettingsEngine::busyWaitForDecode() {
    return _busyWaitForDecode.value();
}

bool SettingsEngine::logSGCTOutOfOrderErrors() {
    return _logSGCTOutOfOrderErrors.value();
}

bool SettingsEngine::useDoubleBuffering() {
    return _useDoubleBuffering.value();
}

}  // namespace openspace
