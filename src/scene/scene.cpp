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

#include <openspace/scene/scene.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/script_helper.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

#include <iostream>
#include <iterator>
#include <fstream>
#include <string>
#include <chrono>

#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
#include <modules/onscreengui/include/gui.h>
#endif

#include "scene_lua.inl"

namespace {
    const std::string _loggerCat = "Scene";
    const std::string _moduleExtension = ".mod";
	const std::string _defaultCommonDirectory = "common";
	const std::string _commonModuleToken = "${COMMON_MODULE}";

    const std::string KeyCamera = "Camera";
    const std::string KeyFocusObject = "Focus";
    const std::string KeyPositionObject = "Position";
    const std::string KeyViewOffset = "Offset";
}

namespace openspace {

Scene::Scene() : _focus(SceneGraphNode::RootNodeName) {}

Scene::~Scene() {
    deinitialize();
}

bool Scene::initialize() {
    LDEBUG("Initializing SceneGraph");
   
    using ghoul::opengl::ShaderObject;
    using ghoul::opengl::ProgramObject;
   
    std::unique_ptr<ProgramObject> tmpProgram;

	// fboPassthrough program
    tmpProgram = ProgramObject::Build(
        "fboPassProgram",
		"${SHADERS}/fboPass_vs.glsl",
		"${SHADERS}/fboPass_fs.glsl");
	if (!tmpProgram)
        return false;

    tmpProgram->setIgnoreSubroutineUniformLocationError(ProgramObject::IgnoreError::Yes);
	OsEng.configurationManager().setValue("fboPassProgram", tmpProgram.get());

    return true;
}

bool Scene::deinitialize() {
	clearSceneGraph();
    return true;
}

void Scene::update(const UpdateData& data) {
	if (!_sceneGraphToLoad.empty()) {
		OsEng.renderEngine().scene()->clearSceneGraph();
        try {
            bool success = loadSceneInternal(_sceneGraphToLoad);
            _sceneGraphToLoad = "";
        }
        catch (const ghoul::RuntimeError& e) {
            LERROR(e.what());
            return;
        }
	}
    for (SceneGraphNode* node : _graph.nodes()) {
        try {
            node->update(data);
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.what());
        }
    }
}

void Scene::evaluate(Camera* camera) {
    for (SceneGraphNode* node : _graph.nodes())
        node->evaluate(camera);
	//_root->evaluate(camera);
}

void Scene::render(const RenderData& data) {
    for (SceneGraphNode* node : _graph.nodes())
        node->render(data);
}

std::vector<std::pair<Volume*, RenderData>> Scene::volumesToRender(const RenderData& data) const {
    std::vector<std::pair<Volume*, RenderData>> volumes;

    for (const SceneGraphNode* node : _graph.nodes()) {
        std::vector<std::pair<Volume*, RenderData>> newVolumes = node->volumesToRender(data);
        std::copy(newVolumes.begin(), newVolumes.end(), std::back_inserter(volumes));
    }
    return volumes;
}

void Scene::scheduleLoadSceneFile(const std::string& sceneDescriptionFilePath) {
	_sceneGraphToLoad = sceneDescriptionFilePath;
}

void Scene::clearSceneGraph() {
	// deallocate the scene graph. Recursive deallocation will occur
    _graph.clear();
	//if (_root) {
	//	_root->deinitialize();
	//	delete _root;
	//	_root = nullptr;
	//}

 //   _nodes.erase(_nodes.begin(), _nodes.end());
 //   _allNodes.erase(_allNodes.begin(), _allNodes.end());

    _focus.clear();
}

bool Scene::loadSceneInternal(const std::string& sceneDescriptionFilePath) {
    ghoul::Dictionary dictionary;
    ghoul::lua::loadDictionaryFromFile(sceneDescriptionFilePath, dictionary);

    _graph.loadFromFile(sceneDescriptionFilePath);

    // TODO: Make it less hard-coded and more flexible when nodes are not found
    ghoul::Dictionary cameraDictionary;
    if (dictionary.getValue(KeyCamera, cameraDictionary)) {
        LDEBUG("Camera dictionary found");
        std::string focus;

        if (cameraDictionary.hasKey(KeyFocusObject)
            && cameraDictionary.getValue(KeyFocusObject, focus))
        {
            auto focusIterator = std::find_if(
                _graph.nodes().begin(),
                _graph.nodes().end(),
                [focus](SceneGraphNode* node) {
                    return node->name() == focus;
                }
            );

            if (focusIterator != _graph.nodes().end()) {
                _focus = focus;
                LDEBUG("Setting camera focus to '" << _focus << "'");
            }
            else {
                LERROR("Could not find focus object '" << focus << "'");
                _focus = "Root";
            }
        }
    }

    // Initialize all nodes
    for (SceneGraphNode* node : _graph.nodes()) {
        try {
            bool success = node->initialize();
            if (success)
                LDEBUG(node->name() << " initialized successfully!");
            else
                LWARNING(node->name() << " not initialized.");
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(_loggerCat + "(" + e.component + ")", e.what());
        }
    }

    // update the position of all nodes
	// TODO need to check this; unnecessary? (ab)
	for (SceneGraphNode* node : _graph.nodes()) {
		node->update({ Time::ref().currentTime() });
    }

    for (auto it = _graph.nodes().rbegin(); it != _graph.nodes().rend(); ++it)
        (*it)->calculateBoundingSphere();


    // Calculate the bounding sphere for the scenegraph
    //_root->calculateBoundingSphere();

    // set the camera position
	Camera* c = OsEng.ref().renderEngine().camera();
    //auto focusIterator = _allNodes.find(_focus);
    auto focusIterator = std::find_if(
                _graph.nodes().begin(),
                _graph.nodes().end(),
                [&](SceneGraphNode* node) {
        return node->name() == _focus;
    }
    );

	glm::vec2 cameraScaling(1);
	psc cameraPosition(0,0,1,0);

    //if (_focus->)
    if (focusIterator != _graph.nodes().end()) {
        LDEBUG("Camera focus is '" << _focus << "'");
        SceneGraphNode* focusNode = *focusIterator;
        //Camera* c = OsEng.interactionHandler().getCamera();

        // TODO: Make distance depend on radius
        // TODO: Set distance and camera direction in some more smart way
        // TODO: Set scaling dependent on the position and distance
        // set position for camera
		const PowerScaledScalar bound = focusNode->calculateBoundingSphere();

        // this part is full of magic!
		glm::vec2 boundf = bound.vec2();
        //glm::vec2 scaling{1.0f, -boundf[1]};
		cameraScaling = glm::vec2(1.f, -boundf[1]);
        boundf[0] *= 5.0f;
        
		//psc cameraPosition = focusNode->position();
        //cameraPosition += psc(glm::vec4(0.f, 0.f, boundf));

		//cameraPosition = psc(glm::vec4(0.f, 0.f, 1.f,0.f));

		cameraPosition = focusNode->position();
		cameraPosition += psc(glm::vec4(0.f, 0.f, boundf));
		
		//why this line? (JK)
		//cameraPosition = psc(glm::vec4(0.f, 0.f, 1.f, 0.f));

		//c->setPosition(cameraPosition);
       // c->setCameraDirection(glm::vec3(0, 0, -1));
      //  c->setScaling(scaling);

        // Set the focus node for the interactionhandler
        OsEng.interactionHandler().setFocusNode(focusNode);
    }
    else
        OsEng.interactionHandler().setFocusNode(_graph.rootNode());

	glm::vec4 position;
    if (cameraDictionary.hasKeyAndValue<glm::vec4>(KeyPositionObject)) {
        try {
            position = cameraDictionary.value<glm::vec4>(KeyPositionObject);

            LDEBUG("Camera position is ("
                << position[0] << ", "
                << position[1] << ", "
                << position[2] << ", "
                << position[3] << ")");

            cameraPosition = psc(position);
        }
        catch (const ghoul::Dictionary::DictionaryError& e) {
            LERROR("Error loading Camera location: " << e.what());
        }
	}

	// the camera position
	const SceneGraphNode* fn = OsEng.interactionHandler().focusNode();
    // Check crash for when fn == nullptr

	glm::mat4 la = glm::lookAt(cameraPosition.vec3(), fn->worldPosition().vec3(), c->lookUpVector());

	c->setRotation(la);
	c->setPosition(cameraPosition);
	c->setScaling(cameraScaling);

	glm::vec3 viewOffset;
	if (cameraDictionary.hasKey(KeyViewOffset)
		&& cameraDictionary.getValue(KeyViewOffset, viewOffset)) {
	    glm::quat rot = glm::quat(viewOffset);
	    c->rotate(rot);
	}


	for (SceneGraphNode* node : _graph.nodes()) {
		std::vector<properties::Property*> properties = node->propertiesRecursive();
		for (properties::Property* p : properties) {
            OsEng.gui()._property.registerProperty(p);
		}
	}

    // If a LuaDocumentationFile was specified, generate it now
    const bool hasType = OsEng.configurationManager().hasKey(ConfigurationManager::KeyPropertyDocumentationType);
    const bool hasFile = OsEng.configurationManager().hasKey(ConfigurationManager::KeyPropertyDocumentationFile);
    if (hasType && hasFile) {
        std::string propertyDocumentationType;
        OsEng.configurationManager().getValue(ConfigurationManager::KeyPropertyDocumentationType, propertyDocumentationType);
        std::string propertyDocumentationFile;
        OsEng.configurationManager().getValue(ConfigurationManager::KeyPropertyDocumentationFile, propertyDocumentationFile);

        propertyDocumentationFile = absPath(propertyDocumentationFile);
        writePropertyDocumentation(propertyDocumentationFile, propertyDocumentationType);
    }


    OsEng.runSettingsScripts();

    OsEng.enableBarrier();

    return true;
}

//void Scene::loadModules(
//	const std::string& directory, 
//	const ghoul::Dictionary& dictionary) 
//{
//	// Struct containing dependencies and nodes
//	LoadMaps m;
//
//	// Get the common directory
//	std::string commonDirectory(_defaultCommonDirectory);
//	dictionary.getValue(constants::scenegraph::keyCommonFolder, commonDirectory);
//	FileSys.registerPathToken(_commonModuleToken, commonDirectory);
//
//    lua_State* state = ghoul::lua::createNewLuaState();
//    OsEng.scriptEngine()->initializeLuaState(state);
//
//	LDEBUG("Loading common module folder '" << commonDirectory << "'");
//	// Load common modules into LoadMaps struct
//	loadModule(m, FileSys.pathByAppendingComponent(directory, commonDirectory), state);
//
//	// Load the rest of the modules into LoadMaps struct
//    ghoul::Dictionary moduleDictionary;
//    if (dictionary.getValue(constants::scenegraph::keyModules, moduleDictionary)) {
//        std::vector<std::string> keys = moduleDictionary.keys();
//        std::sort(keys.begin(), keys.end());
//        for (const std::string& key : keys) {
//            std::string moduleFolder;
//			if (moduleDictionary.getValue(key, moduleFolder)) {
//                loadModule(m, FileSys.pathByAppendingComponent(directory, moduleFolder), state);
//			}
//        }
//    }
//
//    // Load and construct scenegraphnodes from LoadMaps struct
//    loadNodes(SceneGraphNode::RootNodeName, m);
//
//    // Remove loaded nodes from dependency list
//    for(const auto& name: m.loadedNodes) {
//    	m.dependencies.erase(name);
//    }
//
//    // Check to see what dependencies are not resolved.
//    for(auto& node: m.dependencies) {
//    	LWARNING(
//    		"'" << node.second << "'' not loaded, parent '" 
//    		<< node.first << "' not defined!");
//    }
//}

//void Scene::loadModule(LoadMaps& m,const std::string& modulePath, lua_State* state) {
//	auto pos = modulePath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
//    if (pos == modulePath.npos) {
//        LERROR("Bad format for module path: " << modulePath);
//        return;
//    }
//
//    std::string fullModule = modulePath + modulePath.substr(pos) + _moduleExtension;
//    LDEBUG("Loading nodes from: " << fullModule);
//
//    ghoul::filesystem::Directory oldDirectory = FileSys.currentDirectory();
//    FileSys.setCurrentDirectory(modulePath);
//
//    ghoul::Dictionary moduleDictionary;
//    ghoul::lua::loadDictionaryFromFile(fullModule, moduleDictionary, state);
//    std::vector<std::string> keys = moduleDictionary.keys();
//    for (const std::string& key : keys) {
//        if (!moduleDictionary.hasValue<ghoul::Dictionary>(key)) {
//            LERROR("SceneGraphElement '" << key << "' is not a table in module '"
//                                         << fullModule << "'");
//            continue;
//        }
//        
//        ghoul::Dictionary element;
//        std::string nodeName;
//        std::string parentName;
//
//        moduleDictionary.getValue(key, element);
//		element.setValue(constants::scenegraph::keyPathModule, modulePath);
//
//		element.getValue(constants::scenegraphnode::keyName, nodeName);
//		element.getValue(constants::scenegraphnode::keyParentName, parentName);
//
//		m.nodes[nodeName] = element;
//		m.dependencies.emplace(parentName,nodeName);
//    }
//
//    FileSys.setCurrentDirectory(oldDirectory);
//}

//void Scene::loadNodes(const std::string& parentName, LoadMaps& m) {
//	auto eqRange = m.dependencies.equal_range(parentName);
//	for (auto it = eqRange.first; it != eqRange.second; ++it) {
//		auto node = m.nodes.find((*it).second);
//		loadNode(node->second);
//		loadNodes((*it).second, m);
//	}
//	m.loadedNodes.emplace_back(parentName);
//}
//
//void Scene::loadNode(const ghoul::Dictionary& dictionary) {
//    SceneGraphNode* node = SceneGraphNode::createFromDictionary(dictionary);
//    if(node) {
//    	_allNodes.emplace(node->name(), node);
//    	_nodes.push_back(node);
//    }
//}

//void SceneGraph::loadModule(const std::string& modulePath) {
//    auto pos = modulePath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
//    if (pos == modulePath.npos) {
//        LERROR("Bad format for module path: " << modulePath);
//        return;
//    }
//
//    std::string fullModule = modulePath + modulePath.substr(pos) + _moduleExtension;
//    LDEBUG("Loading modules from: " << fullModule);
//
//    ghoul::filesystem::Directory oldDirectory = FileSys.currentDirectory();
//    FileSys.setCurrentDirectory(modulePath);
//
//    ghoul::Dictionary moduleDictionary;
//    ghoul::lua::loadDictionaryFromFile(fullModule, moduleDictionary);
//    std::vector<std::string> keys = moduleDictionary.keys();
//    for (const std::string& key : keys) {
//        if (!moduleDictionary.hasValue<ghoul::Dictionary>(key)) {
//            LERROR("SceneGraphElement '" << key << "' is not a table in module '"
//                                         << fullModule << "'");
//            continue;
//        }
//        
//        ghoul::Dictionary element;
//        moduleDictionary.getValue(key, element);
//
//        element.setValue(constants::scenegraph::keyPathModule, modulePath);
//
//		//each element in this new dictionary becomes a scenegraph node. 
//        SceneGraphNode* node = SceneGraphNode::createFromDictionary(element);
//
//        _allNodes.emplace(node->name(), node);
//        _nodes.push_back(node);
//    }
//
//    FileSys.setCurrentDirectory(oldDirectory);
//
//    // Print the tree
//    //printTree(_root);
//}

SceneGraphNode* Scene::root() const {
    return _graph.rootNode();
}
    
SceneGraphNode* Scene::sceneGraphNode(const std::string& name) const {
    return _graph.sceneGraphNode(name);
}

std::vector<SceneGraphNode*> Scene::allSceneGraphNodes() {
	return _graph.nodes();
}

void Scene::writePropertyDocumentation(const std::string& filename, const std::string& type) {
    if (type == "text") {
        LDEBUG("Writing documentation for properties");
        std::ofstream file(filename);
        if (!file.good()) {
            LERROR("Could not open file '" << filename << "' for writing property documentation");
            return;
        }

        using properties::Property;
        for (SceneGraphNode* node : _graph.nodes()) {
            std::vector<Property*> properties = node->propertiesRecursive();
            if (!properties.empty()) {
                file << node->name() << std::endl;

                for (Property* p : properties) {
                    file << p->fullyQualifiedIdentifier() << ":   " << p->guiName() << std::endl;
                }

                file << std::endl;
            }
        }
    }
    else
        LERROR("Undefined type '" << type << "' for Property documentation");
}

scripting::ScriptEngine::LuaLibrary Scene::luaLibrary() {
	return {
		"",
		{
			{
				"setPropertyValue",
				&luascriptfunctions::property_setValue,
				"string, *",
				"Sets a property identified by the URI in "
				"the first argument. The second argument can be any type, but it has to "
				" agree with the type that the property expects",
                true
			},
			{
				"getPropertyValue",
				&luascriptfunctions::property_getValue,
				"string",
				"Returns the value the property, identified by "
				"the provided URI."
			},
			{
				"loadScene",
				&luascriptfunctions::loadScene,
				"string",
				"Loads the scene found at the file passed as an "
				"argument. If a scene is already loaded, it is unloaded first"
			}
		}
	};
}

}  // namespace openspace
