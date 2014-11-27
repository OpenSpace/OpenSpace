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

// open space includes
#include <openspace/scenegraph/scenegraph.h>
#include <openspace/scenegraph/scenegraphnode.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/constants.h>
#include <openspace/query/query.h>
#include <openspace/util/time.h>
#include <openspace/abuffer/abuffer.h>

// ghoul includes
#include "ghoul/logging/logmanager.h"
#include "ghoul/opengl/programobject.h"
#include "ghoul/opengl/texturereader.h"
#include "ghoul/opengl/texture.h"

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>

#include <iostream>
#include <fstream>
#include <string>
#include <chrono>

namespace {
    const std::string _loggerCat = "SceneGraph";
    const std::string _moduleExtension = ".mod";
	const std::string _defaultCommonDirectory = "common";
	const std::string _commonModuleToken = "${COMMON_MODULE}";
}

namespace openspace {

namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * setPropertyValue(string, *):
 * Sets the property identified by the URI in the first argument to the value passed to
 * the second argument. The type of the second argument is arbitrary, but it must agree
 * with the type the denoted Property expects
 */
int property_setValue(lua_State* L) {
	using ghoul::lua::luaTypeToString;

	int nArguments = lua_gettop(L);
	if (nArguments != 2)
		return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);

	std::string uri = luaL_checkstring(L, -2);
	const int type = lua_type(L, -1);

	openspace::properties::Property* prop = property(uri);
	if (!prop)
		return luaL_error(L, "Property with URL '%s' could not be found", uri.c_str());

	if (type != prop->typeLua())
		return luaL_error(L, "Property '%s' does not accept input of type '%s'. \
							  Requested type: '%s'", uri.c_str(),
							  luaTypeToString(type).c_str(),
							  luaTypeToString(prop->typeLua()).c_str());
	else
		prop->setLua(L);

	return 0;
}

/**
 * \ingroup LuaScripts
 * getPropertyValue(string):
 * Returns the value of the property identified by the passed URI as a Lua object that can
 * be passed to the setPropertyValue method.
 */
int property_getValue(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	std::string uri = luaL_checkstring(L, -1);

	openspace::properties::Property* prop = property(uri);
	if (!prop)
		return luaL_error(L, "Property with URL '%s' could not be found", uri.c_str());
	else
		prop->getLua(L);
	return 1;
}

/**
 * \ingroup LuaScripts
 * getPropertyValue(string):
 * Returns the value of the property identified by the passed URI as a Lua object that can
 * be passed to the setPropertyValue method.
 */
int loadScene(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	std::string sceneFile = luaL_checkstring(L, -1);

	OsEng.renderEngine().sceneGraph()->scheduleLoadSceneFile(sceneFile);

	return 0;
}

} // namespace luascriptfunctions

SceneGraph::SceneGraph()
    : _focus(SceneGraphNode::RootNodeName)
    , _position(SceneGraphNode::RootNodeName)
    , _root(nullptr)
{
}

SceneGraph::~SceneGraph()
{
    deinitialize();
}

bool SceneGraph::initialize()
{
    LDEBUG("Initializing SceneGraph");
   
    using ghoul::opengl::ShaderObject;
    using ghoul::opengl::ProgramObject;

    ProgramObject* tmpProgram;

	ghoul::opengl::ProgramObject::ProgramObjectCallback cb = [this](ghoul::opengl::ProgramObject* program) {
		_programUpdateLock.lock();
		_programsToUpdate.insert(program);
		_programUpdateLock.unlock();
	};

	// Start Timing for building SceneGraph shaders
	typedef std::chrono::high_resolution_clock clock_;
	typedef std::chrono::duration<double, std::ratio<1> > second_;
	std::chrono::time_point<clock_> beginning(clock_::now());

	// pscstandard
	tmpProgram = ProgramObject::Build("pscstandard",
		"${SHADERS}/pscstandard_vs.glsl",
		"${SHADERS}/pscstandard_fs.glsl",
		cb);
    if( ! tmpProgram) return false;
	_programs.push_back(tmpProgram);
    OsEng.ref().configurationManager().setValue("pscShader", tmpProgram);

	// pscstandard
	tmpProgram = ProgramObject::Build("EphemerisProgram",
		"${SHADERS}/ephemeris_vs.glsl",
		"${SHADERS}/ephemeris_fs.glsl",
		cb);
	if (!tmpProgram) return false;
	_programs.push_back(tmpProgram);
	OsEng.ref().configurationManager().setValue("EphemerisProgram", tmpProgram);


    // RaycastProgram
	tmpProgram = ProgramObject::Build("RaycastProgram",
		"${SHADERS}/exitpoints.vert",
		"${SHADERS}/exitpoints.frag",
		cb);
	if (!tmpProgram) return false;
	_programs.push_back(tmpProgram);
    OsEng.ref().configurationManager().setValue("RaycastProgram", tmpProgram);

	// Point program
	tmpProgram = ProgramObject::Build("Point",
		"${SHADERS}/star_vs.glsl",
		"${SHADERS}/star_fs.glsl",
		"${SHADERS}/star_ge.glsl",
		cb);
	if (!tmpProgram) return false;
	_programs.push_back(tmpProgram);
	OsEng.ref().configurationManager().setValue("PointProgram", tmpProgram);

	// Grid program
	tmpProgram = ProgramObject::Build("Grid",
		"${SHADERS}/grid_vs.glsl",
		"${SHADERS}/grid_fs.glsl",
		cb);
	if (!tmpProgram) return false;
	_programs.push_back(tmpProgram);
	OsEng.ref().configurationManager().setValue("GridProgram", tmpProgram);

	// Plane program
	tmpProgram = ProgramObject::Build("Plane",
		"${SHADERS}/plane_vs.glsl",
		"${SHADERS}/plane_fs.glsl",
		cb);
	if (!tmpProgram) return false;
	_programs.push_back(tmpProgram);
	OsEng.ref().configurationManager().setValue("PlaneProgram", tmpProgram);

	// Done building shaders
    double elapsed = std::chrono::duration_cast<second_>(clock_::now()-beginning).count();
    LINFO("Time to load shaders: " << elapsed);


    return true;
}

bool SceneGraph::deinitialize()
{
	clearSceneGraph();

	// clean up all programs
	_programsToUpdate.clear();
	for (auto program : _programs) {
		delete program;
	}
	_programs.clear();
    return true;
}

void SceneGraph::update(const UpdateData& data)
{
	if (!_sceneGraphToLoad.empty()) {
		OsEng.renderEngine().sceneGraph()->clearSceneGraph();
		bool success = loadSceneInternal(_sceneGraphToLoad);
		_sceneGraphToLoad = "";
		if (!success)
			return;
#ifndef __APPLE__
		OsEng.renderEngine().abuffer()->invalidateABuffer();
#endif
	}
    for (auto node : _nodes)
        node->update(data);
}

void SceneGraph::evaluate(Camera* camera)
{
	if (_root)
		_root->evaluate(camera);
}

void SceneGraph::render(const RenderData& data)
{
	bool emptyProgramsToUpdate = _programsToUpdate.empty();
		
	_programUpdateLock.lock();
	for (auto program : _programsToUpdate) {
		LDEBUG("Attempting to recompile " << program->name());
		program->rebuildFromFile();
	}
	_programsToUpdate.erase(_programsToUpdate.begin(), _programsToUpdate.end());
	_programUpdateLock.unlock();

	if (!emptyProgramsToUpdate) {
		LDEBUG("Setting uniforms");
		// Ignore attribute locations
		for (auto program : _programs) {
			program->setIgnoreSubroutineUniformLocationError(true);
		}
	}

	if (_root)
		_root->render(data);
}

void SceneGraph::scheduleLoadSceneFile(const std::string& sceneDescriptionFilePath) {
	_sceneGraphToLoad = sceneDescriptionFilePath;
}

void SceneGraph::clearSceneGraph() {
	for (auto node : _nodes)
		node->deinitialize();

	// deallocate the scene graph. Recursive deallocation will occur
    delete _root;
    _root = nullptr;

    _nodes.erase(_nodes.begin(), _nodes.end());
    _allNodes.erase(_allNodes.begin(), _allNodes.end());

    _focus.clear();
    _position.clear();
}

bool SceneGraph::loadSceneInternal(const std::string& sceneDescriptionFilePath)
{
    using ghoul::Dictionary;
    using ghoul::lua::loadDictionaryFromFile;

	if (!FileSys.fileExists(sceneDescriptionFilePath)) {
		LFATAL("Scene description file '" << sceneDescriptionFilePath << "' not found");
		return false;
	}

    LDEBUG("Loading scenegraph nodes");
    if (_root != nullptr) {
        LFATAL("Scenegraph already loaded");
        return false;
    }

    // initialize the root node
    _root = new SceneGraphNode();
    _root->setName(SceneGraphNode::RootNodeName);
    _nodes.push_back(_root);
    _allNodes.emplace(SceneGraphNode::RootNodeName, _root);

    Dictionary dictionary;
	//load default.scene 
    loadDictionaryFromFile(sceneDescriptionFilePath, dictionary);

	std::string&& sceneDescriptionDirectory =
		ghoul::filesystem::File(sceneDescriptionFilePath).directoryName();
	std::string moduleDirectory(".");
	dictionary.getValue(constants::scenegraph::keyPathScene, moduleDirectory);

	std::string commonDirectory(_defaultCommonDirectory);
	dictionary.getValue(constants::scenegraph::keyCommonFolder, commonDirectory);
	FileSys.registerPathToken(_commonModuleToken, commonDirectory);

	// The scene path could either be an absolute or relative path to the description
	// paths directory
	std::string&& relativeCandidate = sceneDescriptionDirectory +
		ghoul::filesystem::FileSystem::PathSeparator + moduleDirectory;
	std::string&& absoluteCandidate = absPath(moduleDirectory);

	if (FileSys.directoryExists(relativeCandidate))
		moduleDirectory = relativeCandidate;
	else if (FileSys.directoryExists(absoluteCandidate))
		moduleDirectory = absoluteCandidate;
	else {
		LFATAL("The '" << constants::scenegraph::keyPathScene << "' pointed to a "
			"path '" << moduleDirectory << "' that did not exist");
		return false;
	}

	LDEBUG("Loading common module folder '" << commonDirectory << "'");
	loadModule(FileSys.pathByAppendingComponent(moduleDirectory, commonDirectory));

    Dictionary moduleDictionary;
    if (dictionary.getValue(constants::scenegraph::keyModules, moduleDictionary)) {
        std::vector<std::string> keys = moduleDictionary.keys();
        std::sort(keys.begin(), keys.end());
        for (const std::string& key : keys) {
            std::string moduleFolder;
			if (moduleDictionary.getValue(key, moduleFolder))
                loadModule(FileSys.pathByAppendingComponent(moduleDirectory, moduleFolder));
        }
    }

    // TODO: Make it less hard-coded and more flexible when nodes are not found
    Dictionary cameraDictionary;
    if (dictionary.getValue(constants::scenegraph::keyCamera, cameraDictionary)) {
        LDEBUG("Camera dictionary found");
        std::string focus;
        std::string position;

        if (cameraDictionary.hasKey(constants::scenegraph::keyFocusObject)
            && cameraDictionary.getValue(constants::scenegraph::keyFocusObject, focus)) {
            auto focusIterator = _allNodes.find(focus);
            if (focusIterator != _allNodes.end()) {
                _focus = focus;
                LDEBUG("Setting camera focus to '" << _focus << "'");
            }
            else
                LERROR("Could not find focus object '" << focus << "'");
        }
        if (cameraDictionary.hasKey(constants::scenegraph::keyPositionObject)
            && cameraDictionary.getValue(constants::scenegraph::keyPositionObject, position)) {
            auto positionIterator = _allNodes.find(position);
            if (positionIterator != _allNodes.end()) {
                _position = position;
                LDEBUG("Setting camera position to '" << _position << "'");
            }
            else
                LERROR("Could not find object '" << position << "' to position camera");
        }
    }

    // Initialize all nodes
    for (auto node : _nodes) {
		bool success = node->initialize();
        if (success)
            LDEBUG(node->name() << " initialized successfully!");
        else
            LWARNING(node->name() << " not initialized.");
    }

    // update the position of all nodes
	// TODO need to check this; unnecessary? (ab)
	for (auto node : _nodes)
		node->update({ Time::ref().currentTime() });

    // Calculate the bounding sphere for the scenegraph
    _root->calculateBoundingSphere();

    // set the camera position
    auto focusIterator = _allNodes.find(_focus);
    auto positionIterator = _allNodes.find(_position);

    if (focusIterator != _allNodes.end() && positionIterator != _allNodes.end()) {
        LDEBUG("Camera position is '" << _position << "', camera focus is '" << _focus
                                      << "'");
        SceneGraphNode* focusNode = focusIterator->second;
        SceneGraphNode* positionNode = positionIterator->second;
        Camera* c = OsEng.ref().renderEngine().camera();
        //Camera* c = OsEng.interactionHandler().getCamera();

        // TODO: Make distance depend on radius
        // TODO: Set distance and camera direction in some more smart way
        // TODO: Set scaling dependent on the position and distance
        // set position for camera
        const PowerScaledScalar bound = positionNode->calculateBoundingSphere();

        // this part is full of magic!
		glm::vec2 boundf = bound.vec2();
        glm::vec2 scaling{1.0f, -boundf[1]};
        boundf[0] *= 5.0f;
        
        psc cameraPosition = positionNode->position();
        cameraPosition += psc(glm::vec4(0.f, 0.f, boundf));

		cameraPosition = psc(glm::vec4(0.f, 0.f, 1.f,0.f));

		c->setPosition(cameraPosition);
        c->setCameraDirection(glm::vec3(0, 0, -1));
        c->setScaling(scaling);

        // Set the focus node for the interactionhandler
        OsEng.interactionHandler().setFocusNode(focusNode);
    }

    return true;
}

void SceneGraph::loadModule(const std::string& modulePath)
{
    auto pos = modulePath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
    if (pos == modulePath.npos) {
        LERROR("Bad format for module path: " << modulePath);
        return;
    }

    std::string fullModule = modulePath + modulePath.substr(pos) + _moduleExtension;
    LDEBUG("Loading modules from: " << fullModule);

    ghoul::Dictionary moduleDictionary;
    ghoul::lua::loadDictionaryFromFile(fullModule, moduleDictionary);
    std::vector<std::string> keys = moduleDictionary.keys();
    for (const std::string& key : keys) {
        if (!moduleDictionary.hasValue<ghoul::Dictionary>(key)) {
            LERROR("SceneGraphElement '" << key << "' is not a table in module '"
                                         << fullModule << "'");
            continue;
        }
        
        ghoul::Dictionary element;
        moduleDictionary.getValue(key, element);

        element.setValue(constants::scenegraph::keyPathModule, modulePath);

		//each element in this new dictionary becomes a scenegraph node. 
        SceneGraphNode* node = SceneGraphNode::createFromDictionary(element);

        _allNodes.emplace(node->name(), node);
        _nodes.push_back(node);
    }

    // Print the tree
    //printTree(_root);
}

void SceneGraph::printChildren() const
{
    _root->print();
}

SceneGraphNode* SceneGraph::root() const
{
    return _root;
}
    
SceneGraphNode* SceneGraph::sceneGraphNode(const std::string& name) const {
    auto it = _allNodes.find(name);
    if (it == _allNodes.end())
        return nullptr;
    else
        return it->second;
}

scripting::ScriptEngine::LuaLibrary SceneGraph::luaLibrary() {
	return {
		"",
		{
			{
				"setPropertyValue",
				&luascriptfunctions::property_setValue,
				"setPropertyValue(string, *): Sets a property identified by the URI in "
				"the first argument. The second argument can be any type, but it has to "
				" agree with the type that the property expects"
			},
			{
				"getPropertyValue",
				&luascriptfunctions::property_getValue,
				"getPropertyValue(string): Returns the value the property, identified by "
				"the provided URI."
			},
			{
				"loadScene",
				&luascriptfunctions::loadScene,
				"loadScene(string): Loads the scene found at the file passed as an "
				"argument. If a scene is already loaded, it is unloaded first"
			}
		}
	};
}

}  // namespace openspace
