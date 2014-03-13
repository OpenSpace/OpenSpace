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
#include "rendering/renderableplanet.h"
#include "interaction/interactionhandler.h"
#include "util/spice.h"
#include <openspaceengine.h>

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
}

void SceneGraph::initialize() {
	// logger string
	std::string _loggerCat = "SceneGraph::init";
    
    // The ${SCENEPATH} variable needs to be set and the scene definition needs to exist
    assert(FileSys.fileExists("${SCENEPATH}/default.scene"));
    
    // load the scene
    loadFromModulePath();
    
    // Calculate the bounding sphere for the scenegraph
	_root->calculateBoundingSphere();
    
}

void SceneGraph::update() {
    for(auto node: _nodes) {
        node->update();
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
    
    ghoul::Dictionary dictionary;
    lua_State* state = luaL_newstate();
    if (state == nullptr) {
        LFATAL("Error creating new Lua state: Memory allocation error");
        return;
    }
    luaL_openlibs(state);
    
    // initialize the root node
    _root = new SceneGraphNode;
    _root->initialize();
    _root->setName("Root");
    _nodes.push_back(_root);
    _allNodes.insert ( std::make_pair("Root", _root));


    ghoul::lua::lua_loadIntoDictionary(state, &dictionary, absPath("${SCENEPATH}/default.scene"));
    
    ghoul::Dictionary moduleDictionary;
    if(dictionary.getValue("Modules", moduleDictionary)) {
        auto keys = moduleDictionary.keys();
        std::sort(keys.begin(), keys.end());
        for (auto key: keys) {
            std::string moduleFolder;
            if(moduleDictionary.getValue(key, moduleFolder)) {
                loadModulesFromModulePath(absPath("${SCENEPATH}/"+moduleFolder));
            }
        }
    }
    
    // update relative positions for all loaded objects. Necessary for Spice position modules
    update();
    
    // TODO: Make it less hard-coded and more flexible when nodes are not found
    ghoul::Dictionary cameraDictionary;
    if(dictionary.getValue("Camera", cameraDictionary)) {
        LDEBUG("Cameradictionary found");
        std::string focus;
        std::string position;
        
        if (cameraDictionary.getValue("Focus", focus) &&
            cameraDictionary.getValue("Position", position)) {
            LDEBUG("focus & position found " << focus << " " << position);
            
            auto focusIterator = _allNodes.find(focus);
            auto positionIterator = _allNodes.find(position);
            
            if(focusIterator != _allNodes.end() && positionIterator != _allNodes.end()) {
                LDEBUG("Setting position and focus from config");
                SceneGraphNode* focusNode = focusIterator->second;
                SceneGraphNode* positionNode = positionIterator->second;
                Camera* c = OsEng.interactionHandler().getCamera();
                
                // TODO: Make distance depend on radius
                // TODO: Set distance and camera direction in some more smart way
                // TODO: Set scaling dependent on the position and distance
                // set position for camera
                psc cameraPosition = positionNode->getPosition();
                cameraPosition += psc(0.0,0.0,1.0,2.0);
                c->setPosition(cameraPosition);
                c->setCameraDirection(glm::vec3(0,0,-1));
                c->setScaling(glm::vec2(1.0,0.0));
                
                // Set the focus node for the interactionhandler
                OsEng.interactionHandler().setFocusNode(focusNode);
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
        ghoul::Dictionary singleModuleDictionary;
        if(moduleDictionary.getValue(key, singleModuleDictionary)) {
            std::string moduleName;
            if (singleModuleDictionary.getValue("Name", moduleName)) {
                std::string parentName;
                if ( ! singleModuleDictionary.getValue("Parent", parentName)) {
                    LWARNING("Could not find 'Parent' key, using 'Root'.");
                    parentName = "Root";
                }
                
                auto parentIterator = _allNodes.find(parentName);
                if (parentIterator == _allNodes.end()) {
                    LFATAL("Could not find parent named '"<< parentName <<
                           "' for '" << moduleName << "'." <<
                           " Check module definition order. Skipping module.");
                    continue;
                }
                
                // allocate SceneGraphNode and initialize with Dictionary
                SceneGraphNode* node = new SceneGraphNode;
                singleModuleDictionary.setValue("Path", modulePath);
                if(node->initializeWithDictionary(&singleModuleDictionary)) {
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

void SceneGraph::printChildren() const {
    _root->print();
}

SceneGraphNode* SceneGraph::root() const {
    return _root;
}
	
} // namespace openspace