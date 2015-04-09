/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/scene/scenegraph.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/constants.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>


namespace {
    const std::string _loggerCat = "SceneGraph";
    const std::string _moduleExtension = ".mod";
	const std::string _defaultCommonDirectory = "common";
	const std::string _commonModuleToken = "${COMMON_MODULE}";
}

namespace openspace {

void SceneGraph::clear() {
    // Untested ---abock
    for (SceneGraphNode* n : _nodes)
        delete n;

    _nodes.clear();
}

bool SceneGraph::loadFromFile(const std::string& sceneDescription) {
    clear(); // Move this to a later stage to retain a proper scenegraph when the loading fails ---abock

    std::string absSceneFile = absPath(sceneDescription);

    // See if scene file exists
    if (!FileSys.fileExists(absSceneFile, true)) {
        LERROR("Could not load scene file '" << absSceneFile << "'. " <<
            "File not found");
        return false;
    }
    LINFO("Loading SceneGraph from file '" << absSceneFile << "'");

    // Load dictionary
    ghoul::Dictionary sceneDictionary;
    bool success = ghoul::lua::loadDictionaryFromFile(absSceneFile, sceneDictionary);
    if (!success)
        return false;

    std::string sceneDescriptionDirectory =
    ghoul::filesystem::File(absSceneFile, true).directoryName();
    std::string sceneDirectory(".");
    sceneDictionary.getValue(constants::scenegraph::keyPathScene, sceneDirectory);

    // The scene path could either be an absolute or relative path to the description
    // paths directory
    std::string relativeCandidate = sceneDescriptionDirectory +
        ghoul::filesystem::FileSystem::PathSeparator + sceneDirectory;
    std::string absoluteCandidate = absPath(sceneDirectory);

    if (FileSys.directoryExists(relativeCandidate))
        sceneDirectory = relativeCandidate;
    else if (FileSys.directoryExists(absoluteCandidate))
        sceneDirectory = absoluteCandidate;
    else {
        LERROR("The '" << constants::scenegraph::keyPathScene << "' pointed to a "
            "path '" << sceneDirectory << "' that did not exist");
        return false;
    }

    using constants::scenegraph::keyModules;
    ghoul::Dictionary moduleDictionary;
    success = sceneDictionary.getValue(keyModules, moduleDictionary);
    if (!success)
        // There are no modules that are loaded
        return true;

    lua_State* state = ghoul::lua::createNewLuaState();
    OsEng.scriptEngine()->initializeLuaState(state);

    std::vector<std::string> keys = moduleDictionary.keys();

    // Get the common directory
    using constants::scenegraph::keyCommonFolder;
    bool commonFolderSpecified = sceneDictionary.hasKey(keyCommonFolder);
    bool commonFolderCorrectType = sceneDictionary.hasKeyAndValue<std::string>(keyCommonFolder);

    if (commonFolderSpecified) {
        if (commonFolderCorrectType) {
            std::string commonFolder = sceneDictionary.value<std::string>(keyCommonFolder);
            if (!FileSys.directoryExists(commonFolder))
                LERROR("Specified common folder '" << commonFolder << "' did not exist");
            else {
                if (!commonFolder.empty()) {
                    FileSys.registerPathToken(_commonModuleToken, commonFolder);
                    keys.push_back(commonFolder);
                }
            }
        }
        else
            LERROR("Specification for 'common' folder has invalid type");
    }

    std::sort(keys.begin(), keys.end());
    ghoul::filesystem::Directory oldDirectory = FileSys.currentDirectory();
    for (const std::string& key : keys) {
        std::string moduleName = moduleDictionary.value<std::string>(key);
        std::string modulePath = FileSys.pathByAppendingComponent(sceneDirectory, moduleName);

        if (!FileSys.directoryExists(modulePath)) {
            LERROR("Could not load module '" << moduleName << "'. Directory did not exist");
            continue;
        }

        std::string moduleFile = FileSys.pathByAppendingComponent(
            modulePath,
            moduleName + _moduleExtension
        );

        if (!FileSys.fileExists(moduleFile)) {
            LERROR("Could not load module file '" << moduleFile << "'. File did not exist");
            continue;
        }

        ghoul::Dictionary moduleDictionary;
        bool s = ghoul::lua::loadDictionaryFromFile(moduleFile, moduleDictionary, state);
        if (!s)
            continue;

        SceneGraphNode* root = new SceneGraphNode;
        root->setName(SceneGraphNode::RootNodeName);
        _nodes.push_back(root);

        std::vector<std::string> keys = moduleDictionary.keys();
        for (const std::string& key : keys) {
            if (!moduleDictionary.hasValue<ghoul::Dictionary>(key)) {
                LERROR("SceneGraphNode '" << key << "' is not a table in module '"
                                             << moduleFile << "'");
                continue;
            }

            ghoul::Dictionary element;
            std::string nodeName;
            std::string parentName;

            moduleDictionary.getValue(key, element);
            element.setValue(constants::scenegraph::keyPathModule, modulePath);

            element.getValue(constants::scenegraphnode::keyName, nodeName);
            element.getValue(constants::scenegraphnode::keyParentName, parentName);

            FileSys.setCurrentDirectory(modulePath);
            SceneGraphNode* node = SceneGraphNode::createFromDictionary(element);
            _nodes.push_back(node);

            _edges.emplace(parentName, nodeName);
        }
    }
    FileSys.setCurrentDirectory(oldDirectory);



    //// Load all nodes
    //for (SceneGraphNodeInternal* node : _nodes)
        //createSceneGraphNodeFromStub(node);

}

//bool SceneGraph::createSceneGraphNodeFromStub(SceneGraphNodeInternal* node) {
//
//}

bool SceneGraph::addSceneGraphNode(SceneGraphNode* node) {
    return true;
}

bool SceneGraph::removeSceneGraphNode(SceneGraphNode* node) {
    // How to handle orphaned nodes? (reparent to root?) --- abock
    return true;
}

std::vector<SceneGraphNode*> SceneGraph::linearList() {

}

} // namespace openspace
