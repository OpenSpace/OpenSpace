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
#include <openspace/rendering/renderableplanet.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/util/spice.h>
#include <openspace/engine/openspaceengine.h>

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

void printTree(SceneGraphNode* node, std::string pre = "") {
    LDEBUGC("Tree", pre << node->nodeName());
    auto children = node->children();
    for(auto child: children) {
        printTree(child, pre + "    ");
    }
}
	
SceneGraph::SceneGraph(): _focus("Root"), _position("Root"), _root(nullptr) {}

SceneGraph::~SceneGraph() {

	deinitialize();
}

bool SceneGraph::initialize() {
    LDEBUG("Initializing SceneGraph");
    
    using ghoul::opengl::ShaderObject;
    using ghoul::opengl::ProgramObject;
    using ghoul::opengl::ShaderManager;
    
    ProgramObject* po = nullptr;
    if (    OsEng.ref().configurationManager().hasKey("pscShader") &&
            OsEng.ref().configurationManager().getValue("pscShader", po)) {
        LWARNING("pscShader already in ConfigurationManager, deleting.");
        delete po;
        po = nullptr;
    }
    
    ShaderObject* powerscale_vs = new ShaderObject(ShaderObject::ShaderType::ShaderTypeVertex,
                                                   absPath("${SHADERS}/pscstandard_vs.glsl"),
                                                   "PS Vertex"
                                                   );
    ShaderObject* powerscale_fs = new ShaderObject(ShaderObject::ShaderType::ShaderTypeFragment,
                                                   absPath("${SHADERS}/pscstandard_fs.glsl"),
                                                   "PS Fragment"
                                                   );
    
    po = new ProgramObject;
    po->attachObject(powerscale_vs);
    po->attachObject(powerscale_fs);
    
    if( ! po->compileShaderObjects())
        return false;
    if( ! po->linkProgramObject())
        return false;
    
    OsEng.ref().configurationManager().setValue("pscShader", po);
    
    // Initialize all nodes
    for(auto node: _nodes) {
        bool success = node->initialize();
        if (success) {
            LDEBUG(node->nodeName() << " initialized successfully!");
        } else {
            LWARNING(node->nodeName() << " not initialized.");
        }
    }
    
    // update the position of all nodes
    update();
    
    // Calculate the bounding sphere for the scenegraph
	_root->calculateBoundingSphere();
    
    // set the camera position
    auto focusIterator = _allNodes.find(_focus);
    auto positionIterator = _allNodes.find(_position);
    
    if(focusIterator != _allNodes.end() && positionIterator != _allNodes.end()) {
        LDEBUG("Camera position is '"<< _position <<"', camera focus is '" << _focus << "'");
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

    
    return true;
}

bool SceneGraph::deinitialize() {

    // deallocate the scene graph. Recursive deallocation will occur
    if(_root)
        delete _root;
    _root = nullptr;
    
    _nodes.erase(_nodes.begin(), _nodes.end());
    _allNodes.erase(_allNodes.begin(), _allNodes.end());
    
    _focus = "";
    _position = "";
    
    return true;
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

bool SceneGraph::loadFromModulePath(const std::string& path) {
   
    LDEBUG("Loading scenegraph nodes");
    if(_root != nullptr) {
        LFATAL("Scenegraph already loaded");
        return false;
    }
    
    std::string defaultScene = path + "/default.scene";
    if( ! FileSys.fileExists(defaultScene)) {
        LFATAL("Could not find 'default.scene' in '" << path << "'");
        return false;
    }
    
    ghoul::Dictionary dictionary;
    lua_State* state = luaL_newstate();
    if (state == nullptr) {
        LFATAL("Error creating new Lua state: Memory allocation error");
        return false;
    }
    luaL_openlibs(state);
    
    // initialize the root node
    _root = new SceneGraphNode(ghoul::Dictionary());
    _root->setName("Root");
    _nodes.push_back(_root);
    _allNodes.insert ( std::make_pair("Root", _root));

    ghoul::lua::lua_loadIntoDictionary(state, &dictionary, defaultScene);
    
    ghoul::Dictionary moduleDictionary;
    if(dictionary.getValue("Modules", moduleDictionary)) {
        auto keys = moduleDictionary.keys();
        std::sort(keys.begin(), keys.end());
        for (auto key: keys) {
            std::string moduleFolder;
            if(moduleDictionary.getValue(key, moduleFolder)) {
                loadModulesFromModulePath(path +"/"+moduleFolder);
            }
        }
    }
    
    // TODO: Make it less hard-coded and more flexible when nodes are not found
    ghoul::Dictionary cameraDictionary;
    if(dictionary.getValue("Camera", cameraDictionary)) {
        LDEBUG("Cameradictionary found");
        std::string focus;
        std::string position;
        
        if(cameraDictionary.hasKey("Focus") && cameraDictionary.getValue("Focus", focus)) {
            auto focusIterator = _allNodes.find(focus);
            if (focusIterator != _allNodes.end()) {
                _focus = focus;
                LDEBUG("Setting camera focus to '"<< _focus << "'");
            }
        }
        if(cameraDictionary.hasKey("Position") && cameraDictionary.getValue("Position", position)) {
            auto positionIterator = _allNodes.find(position);
            if (positionIterator != _allNodes.end()) {
                _position = position;
                LDEBUG("Setting camera position to '"<< _position << "'");
            }
        }
    }
    
    // Close the Lua state
    lua_close(state);
    
    return true;
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
                singleModuleDictionary.setValue("Path", modulePath);
                SceneGraphNode* node = nullptr;
                node = new SceneGraphNode(singleModuleDictionary);
                if(node != nullptr) {
                    // add to internal data structures
                    _allNodes.insert(std::make_pair(moduleName, node));
                    _nodes.push_back(node);
                    
                    // set child and parent
                    SceneGraphNode* parentNode = parentIterator->second;
                    parentNode->addNode(node);
                }
            }
        }
    }
    
    // Close the Lua state
    lua_close(state);
    
    // Print the tree
    printTree(_root);
}

void SceneGraph::printChildren() const {
    _root->print();
}

SceneGraphNode* SceneGraph::root() const {
    return _root;
}
	
} // namespace openspace