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
#include <openspace/scene/scene.h>

#include <ghoul/ghoul.h>
#include <ghoul/filesystem/filesystem.h>

namespace openspace {

SettingsEngine::SettingsEngine()
    : _eyeSeparation("eyeSeparation", "Eye Separation" , 0.f, 0.f, 10.f)
    , _scenes("scenes", "Scene", properties::OptionProperty::DisplayType::DROPDOWN)
{
    setName("Global Properties");
}

void SettingsEngine::initialize() {
    initEyeSeparation();
    initSceneFiles();
}


void SettingsEngine::initEyeSeparation() {
    addProperty(_eyeSeparation);

    // Set interaction to change the window's (SGCT's) eye separation
    _eyeSeparation.onChange(
        [this]() { OsEng.windowWrapper().setEyeSeparationDistance(_eyeSeparation); });
}

void SettingsEngine::initSceneFiles() {
    addProperty(_scenes);

    // Load all matching files in the Scene
    // TODO: match regex with either with new ghoul readFiles or local code
    std::string sceneDir = "${SCENE}";
    std::vector<std::string> scenes = ghoul::filesystem::Directory(sceneDir).readFiles();
    for (std::size_t i = 0; i < scenes.size(); ++i) {
        _scenes.addOption(i, scenes[i]);
    }

    // Set interaction to change ConfigurationManager and schedule the load
    _scenes.onChange(
        [this]() {
            std::string sceneFile = _scenes.getDescriptionByValue(_scenes);
            OsEng.configurationManager().setValue(
                ConfigurationManager::KeyConfigScene, sceneFile);
            std::cout << "For a relatively atomic commit, just pretending to load "
                << sceneFile << std::endl;
        }
    );
}

}
