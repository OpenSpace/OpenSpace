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
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/planets/renderableplanet.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/spice.h>
#include <openspace/util/constants.h>
#include <openspace/util/shadercreator.h>
#include <openspace/query/query.h>
#include <openspace/util/time.h>

// ghoul includes
#include "ghoul/opengl/programobject.h"
#include "ghoul/logging/logmanager.h"
#include "ghoul/logging/consolelog.h"
#include "ghoul/opengl/texturereader.h"
#include "ghoul/opengl/texture.h"

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/opengl/shadermanager.h>

#include <iostream>
#include <string>

#include <chrono>
 //#include <unistd.h>

namespace {
    const std::string _loggerCat = "SceneGraph";
    const std::string _moduleExtension = ".mod";
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
	const std::string _loggerCat = "property_setValue";

	// TODO Check for argument number (ab)
	std::string uri = luaL_checkstring(L, -2);
	const int type = lua_type(L, -1);
	//  boost::any propertyValue;
	//  switch (type) {
	//      case LUA_TNONE:
	//      case LUA_TLIGHTUSERDATA:
	//      case LUA_TFUNCTION:
	//      case LUA_TUSERDATA:
	//      case LUA_TTHREAD:
	//          LERROR("Function parameter was of type '" << luaTypeToString(type) << "'");
	//          return 0;
	//      case LUA_TNIL:
	//          propertyValue = 0;
	//          break;
	//      case LUA_TBOOLEAN:
	//          propertyValue = lua_toboolean(L, -1);
	//          break;
	//      case LUA_TNUMBER:
	//          propertyValue = lua_tonumber(L, -1);
	//          break;
	//      case LUA_TSTRING:
	//          propertyValue = std::string(lua_tostring(L, -1));
	//          break;
	//case LUA_TTABLE: {
	//	ghoul::Dictionary d;
	//	ghoul::lua::populateDictionary(L, d);
	//	propertyValue = d;
	//	break;
	//}
	//  }

	openspace::properties::Property* prop = property(uri);
	if (!prop) {
		LERROR("Property with uri '" << uri << "' could not be found");
		return 0;
	}

	if (type != prop->typeLua())
		LERROR("Property '" << uri << "' does not accept input of type '"
			<< luaTypeToString(type) << "'. Requested type: '"
			<< luaTypeToString(prop->typeLua()) << "'");
	else
		prop->setLua(L);
	//prop->set(propertyValue);

	return 0;
}

/**
 * \ingroup LuaScripts
 * getPropertyValue(string):
 * Returns the value of the property identified by the passed URI as a Lua object that can
 * be passed to the setPropertyValue method.
 */
int property_getValue(lua_State* L) {
	const std::string _loggerCat = "property_getValue";

	// TODO Check for argument number (ab)
	std::string uri = luaL_checkstring(L, -1);

	openspace::properties::Property* prop = property(uri);
	if (!prop) {
		LERROR("Property with uri '" << uri << "' could not be found");
		lua_pushnil(L);
	}
	else {
		prop->getLua(L);

		//switch (type) {
		//    case LUA_TNONE:
		//    case LUA_TLIGHTUSERDATA:
		//    case LUA_TFUNCTION:
		//    case LUA_TUSERDATA:
		//    case LUA_TTHREAD:
		//        LERROR("Function parameter was of type '" << luaTypeToString(type)
		//                                                    << "'");
		//        return 0;
		//    case LUA_TNIL:
		//        propertyValue = 0;
		//        break;
		//    case LUA_TBOOLEAN:
		//        propertyValue = lua_toboolean(L, -1);
		//        break;
		//    case LUA_TNUMBER:
		//        propertyValue = lua_tonumber(L, -1);
		//        break;
		//    case LUA_TSTRING:
		//        propertyValue = std::string(lua_tostring(L, -1));
		//        break;
		//    case LUA_TTABLE: {
		//        ghoul::Dictionary d;
		//        ghoul::lua::populateDictionary(L, d);
		//        propertyValue = d;
		//        break;
		//    }
		//}
	}
	return 1;
}

/**
 * \ingroup LuaScripts
 * getPropertyValue(string):
 * Returns the value of the property identified by the passed URI as a Lua object that can
 * be passed to the setPropertyValue method.
 */
int loadScene(lua_State* L) {
	const std::string _loggerCat = "loadScene";

	// TODO Check for argument number (ab)
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
    
    LDEBUG("Creating ProgramObjects");
    using ghoul::opengl::ShaderObject;
    using ghoul::opengl::ProgramObject;

    ShaderCreator sc = OsEng.shaderBuilder();
    ProgramObject* tmpProgram;

    typedef std::chrono::high_resolution_clock clock_;
    typedef std::chrono::duration<double, std::ratio<1> > second_;

    std::chrono::time_point<clock_> beginning(clock_::now());

    // pscstandard
    tmpProgram = sc.buildShader("pscstandard",
                                "${SHADERS}/pscstandard_vs.glsl",
                                "${SHADERS}/pscstandard_fs.glsl");
    if( ! tmpProgram) return false;
    OsEng.ref().configurationManager().setValue("pscShader", tmpProgram);

    // RaycastProgram
    tmpProgram = sc.buildShader("RaycastProgram",
                                "${SHADERS}/exitpoints.vert",
                                "${SHADERS}/exitpoints.frag");
    if( ! tmpProgram) return false;
    OsEng.ref().configurationManager().setValue("RaycastProgram", tmpProgram);


    // // TwoPassProgram
    // tmpProgram = sc.buildShader("TwoPassProgram",
    //                             "${SHADERS}/twopassraycaster.vert",
    //                             "${SHADERS}/twopassraycaster.frag");
    // if( ! tmpProgram) return false;
    // tmpProgram->setUniform("texBack", 0);
    // tmpProgram->setUniform("texFront", 1);
    // tmpProgram->setUniform("texVolume", 2);
    // OsEng.ref().configurationManager().setValue("TwoPassProgram", tmpProgram);

	// Quad
	tmpProgram = sc.buildShader("Quad",
		"${SHADERS}/quadVert.glsl",
		"${SHADERS}/quadFrag.glsl");
	if (!tmpProgram) return false;
	tmpProgram->setUniform("quadTex", 0);
	OsEng.ref().configurationManager().setValue("Quad", tmpProgram);

	// Star program
	tmpProgram = sc.buildShader("Star",
		"${SHADERS}/star_vs.glsl",
		"${SHADERS}/star_fs.glsl",
		"${SHADERS}/star_ge.glsl");
	if (!tmpProgram) return false;
	OsEng.ref().configurationManager().setValue("StarProgram", tmpProgram);

	// Point program
	tmpProgram = sc.buildShader("Point",
		"${SHADERS}/star_vs.glsl",
		"${SHADERS}/star_fs.glsl",
		"${SHADERS}/star_ge.glsl");
	if (!tmpProgram) return false;
	OsEng.ref().configurationManager().setValue("PointProgram", tmpProgram);

	// Grid program
	tmpProgram = sc.buildShader("Grid",
		"${SHADERS}/grid_vs.glsl",
		"${SHADERS}/grid_fs.glsl");
	if (!tmpProgram) return false;
	OsEng.ref().configurationManager().setValue("GridProgram", tmpProgram);



    double elapsed = std::chrono::duration_cast<second_>(clock_::now()-beginning).count();
    LINFO("Time to load shaders: " << elapsed);

    /*

    auto programCreator = [] (  const std::string& name, 
                                const std::string& vpath, 
                                const std::string& fpath) 
    {
        const std::string vsPath = absPath(vpath);
        const std::string fsPath = absPath(fpath);
        const ShaderObject::ShaderType vsType = ShaderObject::ShaderType::ShaderTypeVertex;
        const ShaderObject::ShaderType fsType = ShaderObject::ShaderType::ShaderTypeFragment;

        ProgramObject* po = new ProgramObject(name);
        ShaderObject* vs = new ShaderObject(vsType, vsPath, name + "Vertex");
        ShaderObject* fs = new ShaderObject(fsType, fsPath, name + "Fragment");
        po->attachObject(vs);
        po->attachObject(fs);
        if ( po->compileShaderObjects() && po->linkProgramObject())
            return po;

        // unsuccessful compilation, cleanup and return nullptr
        delete po;
        po = nullptr;
//<<<<<<< HEAD
    }

    ShaderObject* powerscale_vs
          = new ShaderObject(ShaderObject::ShaderType::ShaderTypeVertex,
                             absPath("${SHADERS}/pscstandard_vs.glsl"), "PS Vertex");
    ShaderObject* powerscale_fs
          = new ShaderObject(ShaderObject::ShaderType::ShaderTypeFragment,
                             absPath("${SHADERS}/pscstandard_fs.glsl"), "PS Fragment");

    po = new ProgramObject;
    po->attachObject(powerscale_vs);
    po->attachObject(powerscale_fs);

    if (!po->compileShaderObjects())
        return false;
    if (!po->linkProgramObject())
        return false;

    OsEng.ref().configurationManager().setValue("pscShader", po);

	ProgramObject* _gridProgram = new ProgramObject("GridProgram");
	ShaderObject* gridvs = new ShaderObject(ShaderObject::ShaderTypeVertex,
		absPath("${SHADERS}/grid_vs.glsl"));
	ShaderObject* gridfs = new ShaderObject(ShaderObject::ShaderTypeFragment,
		absPath("${SHADERS}/grid_fs.glsl"));
	_gridProgram->attachObject(gridvs);
	_gridProgram->attachObject(gridfs);
	_gridProgram->compileShaderObjects();
	_gridProgram->linkProgramObject();


	// STAR HALO RENDERING
	ProgramObject* _starProgram = new ProgramObject("StarProgram");
	ShaderObject* starvs = new ShaderObject(ShaderObject::ShaderTypeVertex,
					                        absPath("${SHADERS}/star_vs.glsl"));
	ShaderObject* starge = new ShaderObject(ShaderObject::ShaderTypeGeometry,
											absPath("${SHADERS}/star_ge.glsl"));
	ShaderObject* starfs = new ShaderObject(ShaderObject::ShaderTypeFragment, 
											absPath("${SHADERS}/star_fs.glsl"));
	_starProgram->attachObject(starvs);
	_starProgram->attachObject(starge);
	_starProgram->attachObject(starfs);
	_starProgram->compileShaderObjects();
	_starProgram->linkProgramObject();
	
	// STAR POINT RENDERING
	ProgramObject* _pointProgram = new ProgramObject("PointProgram");
	ShaderObject* starvs_point = new ShaderObject(ShaderObject::ShaderTypeVertex,
		absPath("${SHADERS}/star_vs_points.glsl"));
	ShaderObject* starge_point = new ShaderObject(ShaderObject::ShaderTypeGeometry,
		absPath("${SHADERS}/star_ge_points.glsl"));
	ShaderObject* starfs_point = new ShaderObject(ShaderObject::ShaderTypeFragment,
		absPath("${SHADERS}/star_fs_points.glsl"));

	_pointProgram->attachObject(starvs_point);
	_pointProgram->attachObject(starge_point);
	_pointProgram->attachObject(starfs_point);
	_pointProgram->compileShaderObjects();
	_pointProgram->linkProgramObject();

    ProgramObject* _fboProgram = new ProgramObject("RaycastProgram");
    ShaderObject* vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
                                                  absPath("${SHADERS}/exitpoints.vert"));
    ShaderObject* fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment, 
		                                          absPath("${SHADERS}/exitpoints.frag"));
    _fboProgram->attachObject(vertexShader);
    _fboProgram->attachObject(fragmentShader);
    _fboProgram->compileShaderObjects();
    _fboProgram->linkProgramObject();

    ProgramObject* _twopassProgram = new ProgramObject("TwoPassProgram");
    vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
                                    absPath("${SHADERS}/twopassraycaster.vert"));
    fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment,
                                      absPath("${SHADERS}/twopassraycaster.frag"));
    _twopassProgram->attachObject(vertexShader);
    _twopassProgram->attachObject(fragmentShader);
    _twopassProgram->compileShaderObjects();
    _twopassProgram->linkProgramObject();
    _twopassProgram->setUniform("texBack", 0);
    _twopassProgram->setUniform("texFront", 1);
    _twopassProgram->setUniform("texVolume", 2);

    ProgramObject* quad = new ProgramObject("Quad");
    ShaderObject* quadv = new ShaderObject(ShaderObject::ShaderTypeVertex,
                                           absPath("${SHADERS}/quadVert.glsl"));
    ShaderObject* quadf = new ShaderObject(ShaderObject::ShaderTypeFragment,
                                           absPath("${SHADERS}/quadFrag.glsl"));
    quad->attachObject(quadv);
    quad->attachObject(quadf);
    quad->compileShaderObjects();
    quad->linkProgramObject();
    quad->setUniform("quadTex", 0);

    OsEng.ref().configurationManager().setValue("RaycastProgram", _fboProgram);
    OsEng.ref().configurationManager().setValue("TwoPassProgram", _twopassProgram);
    OsEng.ref().configurationManager().setValue("Quad", quad);
	OsEng.ref().configurationManager().setValue("PointProgram", _pointProgram);
	OsEng.ref().configurationManager().setValue("StarProgram", _starProgram);
	OsEng.ref().configurationManager().setValue("GridProgram", _gridProgram);

=======
        return po;
    };
    // pscstandard
    tmpProgram = programCreator("pscstandard",
                                "${SHADERS}/pscstandard_vs.glsl",
                                "${SHADERS}/pscstandard_fs.glsl");
    if( ! tmpProgram) return false;
    OsEng.ref().configurationManager().setValue("pscShader", tmpProgram);

    // RaycastProgram
    tmpProgram = programCreator("RaycastProgram",
                                "${SHADERS}/exitpoints.vert",
                                "${SHADERS}/exitpoints.frag");
    if( ! tmpProgram) return false;
    OsEng.ref().configurationManager().setValue("RaycastProgram", tmpProgram);


    // TwoPassProgram
    tmpProgram = programCreator("TwoPassProgram",
                                "${SHADERS}/twopassraycaster.vert",
                                "${SHADERS}/twopassraycaster.frag");
    if( ! tmpProgram) return false;
    tmpProgram->setUniform("texBack", 0);
    tmpProgram->setUniform("texFront", 1);
    tmpProgram->setUniform("texVolume", 2);
    OsEng.ref().configurationManager().setValue("TwoPassProgram", tmpProgram);

    // Quad
    tmpProgram = programCreator("Quad",
                                "${SHADERS}/quadVert.glsl",
                                "${SHADERS}/quadFrag.glsl");
    if( ! tmpProgram) return false;
    tmpProgram->setUniform("quadTex", 0);
    OsEng.ref().configurationManager().setValue("Quad", tmpProgram);
    */
//>>>>>>> develop

    return true;
}

bool SceneGraph::deinitialize()
{
	clearSceneGraph();
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
	if (_root)
		_root->render(data);
}

void SceneGraph::scheduleLoadSceneFile(const std::string& sceneDescriptionFilePath) {
	_sceneGraphToLoad = sceneDescriptionFilePath;
}

void SceneGraph::clearSceneGraph() {
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
	dictionary.getValueSafe(constants::scenegraph::keyPathScene, moduleDirectory);

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

    Dictionary moduleDictionary;
    if (dictionary.getValue(constants::scenegraph::keyModules, moduleDictionary)) {
        std::vector<std::string> keys = moduleDictionary.keys();
        std::sort(keys.begin(), keys.end());
        for (const std::string& key : keys) {
            std::string moduleFolder;
			if (moduleDictionary.getValue(key, moduleFolder))
                loadModule(moduleDirectory + "/" + moduleFolder);
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
		node->update({Time::ref().currentTime()});

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
    auto pos = modulePath.find_last_of("/");
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
	scripting::ScriptEngine::LuaLibrary sceneGraphLibrary = {
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

	return std::move(sceneGraphLibrary);
}

}  // namespace openspace
