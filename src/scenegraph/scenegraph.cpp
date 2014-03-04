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
#include "scenegraph/scenegraph.h"
#include "scenegraph/scenegraphloader.h"
#include "rendering/renderableplanet.h"
#include "interaction/interactionhandler.h"
#include "util/spice.h"

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
#include <ghoul/misc/templatefactory.h>

#include <iostream>
#include <string>

namespace {
    std::string _loggerCat = "SceneGraph";
}

namespace openspace {
	
SceneGraph::SceneGraph() {
	_root = nullptr;
}

SceneGraph::~SceneGraph() {

	// deallocate the scene graph. Recursive deallocation will occur
    // no need to remove from _nodes and _allNodes.
	if(_root)
		delete _root;
	
	// deallocate shaders, iterate c++11 style
	for (auto& shaderTuple: _shaders) {

		// the shader is in the maps second position
		delete shaderTuple.second;
	}
		
}

void SceneGraph::initialize() {
	// logger string
	std::string _loggerCat = "SceneGraph::init";
    
    loadFromModulePath();
	
    //	SceneGraphLoader *loader = new SceneGraphLoader(&nodes_, &shaders_);
    //    root_ = loader->loadSceneGraph(absPath("${BASE_PATH}/modules"));
	update();
	//pss bs = root_->calculateBoundingSphere();
}

void SceneGraph::update() {
    for(auto node: _nodes) {
        //node->update();
    }
}

void SceneGraph::evaluate(Camera *camera) {
	_root->evaluate(camera);
}

void SceneGraph::render(Camera *camera) {
	_root->render(camera);
}

void SceneGraph::loadFromModulePath() {
    assert(_root == nullptr);

    ghoul::TemplateFactory<Renderable> renderablefactory;
    renderablefactory.registerClass<RenderablePlanet>("RenderablePlanet");
    
    ghoul::Dictionary dictionary;
    lua_State* state = luaL_newstate();
    if (state == nullptr) {
        LFATAL("Error creating new Lua state: Memory allocation error");
        return;
    }
    luaL_openlibs(state);
    
    _root = new SceneGraphNode;
    _root->initialize();
    _nodes.push_back(_root);
    _allNodes.insert ( std::make_pair("Root", _root));


    ghoul::lua::lua_loadIntoDictionary(state, &dictionary, absPath("${SCENEPATH}/default.scene"));
    
    ghoul::Dictionary* tmpDictionary;
    if(dictionary.getValue("modules", tmpDictionary)) {
        auto keys = tmpDictionary->keys();
        std::sort(keys.begin(), keys.end());
        for (auto key: keys) {
            std::string moduleFolder;
            if(tmpDictionary->getValue(key, moduleFolder)) {
                loadModulesFromModulePath(absPath("${SCENEPATH}/"+moduleFolder));
            }
        }
    }
    
    // Close the Lua state
    lua_close(state);
}

void SceneGraph::loadModulesFromModulePath(const std::string& modulePath) {
    lua_State* state = luaL_newstate();
    if (state == nullptr) {
        LFATAL("Error creating new Lua state: Memory allocation error");
        return;
    }
    luaL_openlibs(state);
    
    auto pos = modulePath.find_last_of("/");
    if (pos == modulePath.npos) {
        LFATAL("Bad format for module path: " << modulePath);
        return;
    }
    
    std::string fullModule = modulePath + modulePath.substr(pos) + ".mod";
    LDEBUG("Loading modules from: " << fullModule);
    
    ghoul::Dictionary moduleDictionary;
    ghoul::lua::lua_loadIntoDictionary(state, &moduleDictionary, fullModule);
    auto keys = moduleDictionary.keys();
    for (auto key: keys) {
        ghoul::Dictionary* singleModuleDictionary;
        if(moduleDictionary.getValue(key, singleModuleDictionary)) {
            std::string moduleName;
            if (singleModuleDictionary->getValue("Name", moduleName)) {
                std::string parentName;
                if ( ! singleModuleDictionary->getValue("Parent", parentName)) {
                    LDEBUG("Could not find 'Parent' key, using 'Root'.");
                    parentName = "Root";
                }
                
                auto parentIterator = _allNodes.find(parentName);
                if (parentIterator == _allNodes.end()) {
                    LDEBUG("Could not find parent named '"<< parentName <<
                           "' for '" << moduleName << "'." <<
                           " Check module definition order. Skipping module.");
                    continue;
                }
                
                // allocate SceneGraphNode and initialize with Dictionary
                SceneGraphNode* node = new SceneGraphNode;
                if(node->initializeWithDictionary(singleModuleDictionary)) {
                    // add to internal data structures
                    _allNodes.insert(std::make_pair(moduleName, node));
                    _nodes.push_back(node);
                    
                    // set child and parent
                    SceneGraphNode* parentNode = parentIterator->second;
                    parentNode->addNode(node);
                } else {
                    LFATAL("Unable to initialize module '"<< moduleName <<"' using dictionary.");
                    delete node;
                }
            }
        }
    }
    
    // Close the Lua state
    lua_close(state);
}
	
} // namespace openspace