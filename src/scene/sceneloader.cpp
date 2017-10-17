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
    const char* KeyModules = "Modules";
    const char* ModuleExtension = ".mod";
    //const char* KeyPathModule = "ModulePath";

    //const char* RootNodeName = "Root";
    const char* KeyName = "Name";
    const char* KeyParentName = "Parent";
    //const char* KeyDependencies = "Dependencies";
    const char* KeyCamera = "Camera";
    const char* KeyCameraFocus = "Focus";
    const char* KeyCameraPosition = "Position";
    const char* KeyCameraRotation = "Rotation";

    struct ModuleInformation {
        ghoul::Dictionary dictionary;
        std::string moduleFile;
        std::string modulePath;
        std::string moduleName;
    };
} // namespace

namespace openspace {

std::unique_ptr<Scene> SceneLoader::loadScene(const std::string& path) {
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

    std::string relativeSceneDirectory = ".";
    sceneDictionary.getValue<std::string>(KeyPathScene, relativeSceneDirectory);
    std::string modulesPath = FileSys.absPath(sceneDirectory + FileSys.PathSeparator + relativeSceneDirectory);

    ghoul::Dictionary moduleDictionary;
    sceneDictionary.getValue(KeyModules, moduleDictionary);

    // Above we generated a ghoul::Dictionary from the scene file; now we run the scene
    // file again to load any variables defined inside into the state that is passed to
    // the modules. This allows us to specify global variables that can then be used
    // inside the modules to toggle settings.
    ghoul::lua::runScriptFile(state, absScenePath);
    std::vector<std::string> keys = moduleDictionary.keys();

    std::vector<SceneLoader::LoadedNode> allNodes;

    {
        // Inside loadDirectory the working directory is changed (for now), so we need
        // to save the old state
        ghoul::filesystem::Directory oldDirectory = FileSys.currentDirectory();
        
        // Placing this head to guard against exceptions in the directory loading that
        // would otherwise mess up the working directory for everyone else
        OnExit([&]() { FileSys.setCurrentDirectory(oldDirectory); });

        for (const std::string& key : keys) {
            std::string fullName = moduleDictionary.value<std::string>(key);
            std::replace(fullName.begin(), fullName.end(), '/', FileSys.PathSeparator);
            std::string path = FileSys.pathByAppendingComponent(modulesPath, fullName);

            std::vector<SceneLoader::LoadedNode> nodes = loadDirectory(path, state);
            std::move(nodes.begin(), nodes.end(), std::back_inserter(allNodes));
        }
    }
    
    std::unique_ptr<Scene> scene = std::make_unique<Scene>();

    std::unique_ptr<SceneGraphNode> rootNode = std::make_unique<SceneGraphNode>();
    rootNode->setName(SceneGraphNode::RootNodeName);
    scene->setRoot(std::move(rootNode));

    addLoadedNodes(*scene, std::move(allNodes));

    ghoul::Dictionary cameraDictionary;
    sceneDictionary.getValue(KeyCamera, cameraDictionary);
    LoadedCamera loadedCamera = loadCamera(cameraDictionary);

    auto& nodeMap = scene->nodesByName();
    auto it = nodeMap.find(loadedCamera.parent);
    if (it != nodeMap.end()) {
        loadedCamera.camera->setParent(it->second);
    } else {
        LWARNING(
            "Could not find the camera parent '" + loadedCamera.parent +
            "'. Attaching camera to root node.");
        loadedCamera.camera->setParent(scene->root());
    }

    scene->setCamera(std::move(loadedCamera.camera));

    return scene;
}

std::vector<SceneGraphNode*> SceneLoader::importDirectory(Scene& scene, const std::string& path) {
    lua_State* state = ghoul::lua::createNewLuaState();
    OnExit(
        // Delete the Lua state at the end of the scope, no matter what.
        [state]() {ghoul::lua::destroyLuaState(state); }
    );
    OsEng.scriptEngine().initializeLuaState(state);

    std::string absDirectoryPath = absPath(path);

    ghoul::filesystem::Directory oldDirectory = FileSys.currentDirectory();
    std::vector<SceneLoader::LoadedNode> nodes = loadDirectory(path, state);
    FileSys.setCurrentDirectory(oldDirectory);
    return addLoadedNodes(scene, std::move(nodes));
}

SceneGraphNode* SceneLoader::importNodeDictionary(Scene& scene, const ghoul::Dictionary& dict) {
    std::vector<SceneLoader::LoadedNode> loadedNodes;
    loadedNodes.push_back(loadNode(dict));
    std::vector<SceneGraphNode*> nodes = addLoadedNodes(scene, std::move(loadedNodes));
    if (nodes.size() == 1) {
        return nodes[0];
    }
    return nullptr;
}

SceneLoader::LoadedCamera SceneLoader::loadCamera(const ghoul::Dictionary& cameraDict) {
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

    LoadedCamera loadedCamera(focus, std::move(camera));
    
    if (!readSuccessful) {
        throw Scene::InvalidSceneError(
            "Position, Rotation and Focus need to be defined for camera dictionary.");
    }
    
    return loadedCamera;
}


std::vector<SceneLoader::LoadedNode> SceneLoader::loadDirectory(
    const std::string& path,
    lua_State* luaState)
{
    std::string::size_type pos = path.find_last_of(FileSys.PathSeparator);
    if (pos == std::string::npos) {
        LERROR("Error parsing directory name '" << path << "'");
        return std::vector<SceneLoader::LoadedNode>();
    }
    std::string moduleName = path.substr(pos + 1);
    std::string moduleFile = FileSys.pathByAppendingComponent(path, moduleName) + ModuleExtension;

    if (FileSys.fileExists(moduleFile)) {
        // TODO: Get rid of changing the working directory (global state is bad) -- emiax
        // This requires refactoring all renderables to not use relative paths in constructors.
        // For now, no need to reset the directory here as it is done from the outside
        // function calling this method
        FileSys.setCurrentDirectory(ghoul::filesystem::Directory(path));
        
        // We have a module file, so it is a direct include.
        return loadModule(moduleFile, luaState);
    } else {
        std::vector<SceneLoader::LoadedNode> allLoadedNodes;
        // If we do not have a module file, we have to include all subdirectories.
        using ghoul::filesystem::Directory;
        using std::string;

        const Directory directory(path);
        const std::string directoryPath = directory.path();

        if (!FileSys.directoryExists(directoryPath)) {
            LERROR("The directory " << directoryPath << " does not exist.");
            return std::vector<SceneLoader::LoadedNode>();
        }

        for (const string& subdirectory : directory.readDirectories()) {
            std::vector<SceneLoader::LoadedNode> loadedNodes = loadDirectory(subdirectory, luaState);
            std::move(loadedNodes.begin(), loadedNodes.end(), std::back_inserter(allLoadedNodes));
        }
        return allLoadedNodes;
    }
}


SceneLoader::LoadedNode SceneLoader::loadNode(const ghoul::Dictionary& dictionary) {
    std::vector<std::string> dependencies;

    std::string nodeName = dictionary.value<std::string>(KeyName);
    std::string parentName = dictionary.value<std::string>(KeyParentName);
    std::unique_ptr<SceneGraphNode> node = SceneGraphNode::createFromDictionary(dictionary);

    if (dictionary.hasKey(SceneGraphNode::KeyDependencies)) {
        if (!dictionary.hasValue<ghoul::Dictionary>(SceneGraphNode::KeyDependencies)) {
            LERROR("Dependencies did not have the corrent type");
        }
        ghoul::Dictionary nodeDependencies;
        dictionary.getValue(SceneGraphNode::KeyDependencies, nodeDependencies);

        std::vector<std::string> keys = nodeDependencies.keys();
        for (const std::string& key : keys) {
            std::string value = nodeDependencies.value<std::string>(key);
            dependencies.push_back(value);
        }
    }
    return SceneLoader::LoadedNode(nodeName, parentName, dependencies, std::move(node));
}


std::vector<SceneLoader::LoadedNode> SceneLoader::loadModule(const std::string& path, lua_State* luaState) {
    ghoul::Dictionary moduleDictionary;
    try {
        ghoul::lua::loadDictionaryFromFile(path, moduleDictionary, luaState);
    } catch (const ghoul::lua::LuaRuntimeException& e) {
        LERRORC(e.component, e.message);
        return std::vector<SceneLoader::LoadedNode>();
    }

    std::vector<SceneLoader::LoadedNode> loadedNodes;
    std::vector<std::string> keys = moduleDictionary.keys();
    for (const std::string& key : keys) {
        ghoul::Dictionary nodeDictionary;
        if (!moduleDictionary.getValue(key, nodeDictionary)) {
            LERROR("Node dictionary did not have the corrent type");
            continue;
        }
        try {
            loadedNodes.push_back(loadNode(nodeDictionary));
        }
        catch (documentation::SpecificationError& e) {
            LERROR("Specification error in node from " << path);
            LERRORC(e.component, e.message);
            for (const documentation::TestResult::Offense& offense : e.result.offenses) {
                LERRORC(
                    e.component,
                    offense.offender + ": " + std::to_string(offense.reason)
                );
            }
            for (const documentation::TestResult::Warning& warning : e.result.warnings) {
                LWARNINGC(
                    e.component,
                    warning.offender + ": " + std::to_string(warning.reason)
                );
            }
        }
        catch (ghoul::RuntimeError& e) {
            LERROR("Failed loading node from " << path << ": " << e.message << ", " << e.component);
        }
    }
    return loadedNodes;
}

std::vector<SceneGraphNode*> SceneLoader::addLoadedNodes(Scene& scene, std::vector<SceneLoader::LoadedNode>&& loadedNodes) {
    std::map<std::string, SceneGraphNode*> existingNodes = scene.nodesByName();
    std::map<std::string, SceneGraphNode*> addedNodes;

    // Populate map of nodes to be added.
    // Also track new branches of nodes that are attached
    // to allow for recovery in case an invalid scene is generated.
    for (auto& loadedNode : loadedNodes) {
        std::string name = loadedNode.name;
        if (existingNodes.count(name) > 0) {
            LERROR("Node with name '" + name + "' already exists in scene");
            continue;
        }
        if (addedNodes.count(name) > 0) {
            LERROR("Duplicate node names '" + name + "' among loaded nodes");
        }

        SceneGraphNode* node = loadedNode.node.get();
        addedNodes[name] = node;
    }
    
    // Find a node by name among the exising nodes and the added nodes.
    auto findNode = [&existingNodes, &addedNodes](const std::string name) {
        std::map<std::string, SceneGraphNode*>::iterator it;
        if ((it = existingNodes.find(name)) != existingNodes.end()) {
            return it->second;
        }
        if ((it = addedNodes.find(name)) != addedNodes.end()) {
            return it->second;
        }
        return static_cast<SceneGraphNode*>(nullptr);
    };

    std::vector<SceneGraphNode*> attachedBranches;
    std::vector<std::unique_ptr<SceneGraphNode>> badNodes;
    
    // Attach each node to its parent and set up dependencies.
    for (auto& loadedNode : loadedNodes) {
        std::string parentName = loadedNode.parent;
        std::vector<std::string> dependencyNames = loadedNode.dependencies;

        SceneGraphNode* parent = findNode(parentName);
        if (!parent) {
            LERROR("Could not find parent '" + parentName + "' for '" + loadedNode.name + "'");
            badNodes.push_back(std::move(loadedNode.node));
            continue;
        }
        
        std::vector<SceneGraphNode*> dependencies;
        bool foundAllDeps = true;
        for (const auto& depName : dependencyNames) {
            SceneGraphNode* dep = findNode(depName);
            if (!dep) {
                LERROR("Could not find dependency '" + depName + "' for '" + loadedNode.name + "'");
                foundAllDeps = false;
                continue;
            }
            dependencies.push_back(dep);
        }

        if (!foundAllDeps) {
            badNodes.push_back(std::move(loadedNode.node));
            continue;
        }

        SceneGraphNode* child = loadedNode.node.get();
        parent->attachChild(std::move(loadedNode.node), SceneGraphNode::UpdateScene::No);
        child->setDependencies(dependencies, SceneGraphNode::UpdateScene::No);

        if (existingNodes.find(parentName) != existingNodes.end()) {
            attachedBranches.push_back(child);
        }
    }

    // Remove all bad nodes (parent or deps missing) and all their children and dependent nodes.
    // Use unsorted set `visited` to avoid infinite loop in case of circular deps.
    std::unordered_set<SceneGraphNode*> visited;
    for (size_t i = 0; i < badNodes.size(); i++) {
        auto& badNode = badNodes[i];
        for (auto c : badNode->children()) {
            visited.insert(c);
            badNodes.push_back(badNode->detachChild(*c, SceneGraphNode::UpdateScene::No));
        }
        for (auto& d : badNode->dependentNodes()) {
            SceneGraphNode* parent = d->parent();
            if (visited.count(d) == 0) {
                visited.insert(d);
                if (parent) {
                    badNodes.push_back(parent->detachChild(*d, SceneGraphNode::UpdateScene::No));
                }
            }
        }
    }
    // Warn for nodes that lack connection to the root.
    for (auto& node : addedNodes) {
        if (!node.second->scene()) {
            LWARNING("Node '" << node.first << "' is not connected to the root and will not be added to the scene");
        }
    }

    // Add the nodes to the scene.
    for (auto& node : attachedBranches) {
        scene.addNode(node, Scene::UpdateDependencies::No);
    }

    // Update dependencies: sort nodes topologically.
    scene.updateDependencies();

    // Return a vector of all added nodes.
    std::vector<SceneGraphNode*> addedNodesVector;
    std::transform(addedNodes.begin(), addedNodes.end(), std::back_inserter(addedNodesVector), [] (auto& pair) {
        return pair.second;
    });

    return addedNodesVector;
}
}
