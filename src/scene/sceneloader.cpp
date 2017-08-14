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

#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/sceneloader.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/onscopeexit.h>

#include <unordered_set>

namespace {
    const char* _loggerCat = "SceneLoader";
    const char* KeyPathScene = "ScenePath";
    const std::string KeyAssets = "Assets";
    const std::string AssetExtension = ".asset";

    const char* KeyCamera = "Camera";
    const char* KeyCameraFocus = "Focus";
    const char* KeyCameraPosition = "Position";
    const char* KeyCameraRotation = "Rotation";
} // namespace

namespace openspace {

SceneLoader::SceneLoader(AssetLoader* assetLoader) 
    : _assetLoader(assetLoader)
{}

void SceneLoader::loadScene(Scene* scene, const std::string& path) {
    // Set up lua state.
    lua_State* state = ghoul::lua::createNewLuaState();
    OnExit(
        // Delete the Lua state at the end of the scope, no matter what.
        [state]() {ghoul::lua::destroyLuaState(state); }
    );
    OsEng.scriptEngine().initializeLuaState(state);

    std::string absScenePath = absPath(path);
    ghoul::filesystem::File sceneFile(absScenePath);
    std::string sceneDirectory = sceneFile.directoryName();

    ghoul::Dictionary sceneDictionary;
    if (!FileSys.fileExists(absScenePath)) {
        throw ghoul::FileNotFoundError(absScenePath);
    }
    ghoul::lua::loadDictionaryFromFile(absScenePath, sceneDictionary, state);

    documentation::testSpecificationAndThrow(Scene::Documentation(), sceneDictionary, "Scene");

    ghoul::Dictionary assetDictionary;
    sceneDictionary.getValue(KeyAssets, assetDictionary);

    // Above we generated a ghoul::Dictionary from the scene file; now we run the scene
    // file again to load any variables defined inside into the state that is passed to
    // the assets. This allows us to specify global variables that can then be used
    // inside the assets to toggle settings.
    ghoul::lua::runScriptFile(state, absScenePath);
    std::vector<std::string> keys = assetDictionary.keys();
    //ghoul::filesystem::Directory oldDirectory = FileSys.currentDirectory();

    for (const std::string& key : keys) {
        std::string assetName = assetDictionary.value<std::string>(key);
        _assetLoader->loadAsset(assetName);
    }
    try {
        _assetLoader->rootAsset()->initialize();
    } catch (const ghoul::RuntimeError& e) {
        LERROR("Could not initialize root asset: " << e.what());
    }
    
    ghoul::Dictionary cameraDictionary;
    sceneDictionary.getValue(KeyCamera, cameraDictionary);
    std::unique_ptr<LoadedCamera> loadedCamera = loadCamera(cameraDictionary);

    auto& nodeMap = scene->nodesByName();
    auto it = nodeMap.find(loadedCamera->parent);
    if (it != nodeMap.end()) {
        loadedCamera->camera->setParent(it->second);
    } else {
        LWARNING(
            "Could not find the camera parent '" + loadedCamera->parent +
            "'. Attaching camera to root node.");
        loadedCamera->camera->setParent(scene->root());
    }

    scene->setCamera(std::move(loadedCamera->camera));
}

std::unique_ptr<SceneLoader::LoadedCamera> SceneLoader::loadCamera(const ghoul::Dictionary& cameraDict) {
    std::string focus;
    glm::vec3 cameraPosition;
    glm::vec4 cameraRotation;

    bool readSuccessful = true;
    readSuccessful &= cameraDict.getValue(KeyCameraFocus, focus);
    readSuccessful &= cameraDict.getValue(KeyCameraPosition, cameraPosition);
    readSuccessful &= cameraDict.getValue(KeyCameraRotation, cameraRotation);

    std::unique_ptr<Camera> camera = std::make_unique<Camera>();

    camera->setPositionVec3(cameraPosition);
    camera->setRotation(glm::dquat(
        cameraRotation.x, cameraRotation.y, cameraRotation.z, cameraRotation.w));

    std::unique_ptr<LoadedCamera> loadedCamera = std::make_unique<LoadedCamera>(focus, std::move(camera));
    
    if (!readSuccessful) {
        throw Scene::InvalidSceneError(
            "Position, Rotation and Focus need to be defined for camera dictionary.");
    }
    
    return loadedCamera;
}

}
