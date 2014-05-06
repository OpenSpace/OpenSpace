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
#include <openspace/rendering/planets/renderableplanet.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/util/spice.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>

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
const std::string _loggerCat = "SceneGraph";
const std::string _rootNodeName = "Root";
const std::string _moduleExtension = ".mod";

}

namespace openspace {

void printTree(SceneGraphNode* node, std::string pre = "")
{
    LDEBUGC("Tree", pre << node->nodeName());
    const std::vector<SceneGraphNode*>& children = node->children();
    for (SceneGraphNode* child : children)
        printTree(child, pre + "    ");
}

SceneGraph::SceneGraph()
    : _focus("Root")
    , _position("Root")
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

    ProgramObject* po = nullptr;
    if (OsEng.ref().configurationManager().hasKey("pscShader")
        && OsEng.ref().configurationManager().getValue("pscShader", po)) {
        LWARNING("pscShader already in ConfigurationManager, deleting.");
        delete po;
        po = nullptr;
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

    ProgramObject* _fboProgram = new ProgramObject("RaycastProgram");
    ShaderObject* vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
                                                  absPath("${SHADERS}/exitpoints.vert"));
    ShaderObject* fragmentShader = new ShaderObject(
          ShaderObject::ShaderTypeFragment, absPath("${SHADERS}/exitpoints.frag"));
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

    // Initialize all nodes
    for (auto node : _nodes) {
        bool success = node->initialize();
        if (success)
            LDEBUG(node->nodeName() << " initialized successfully!");
        else
            LWARNING(node->nodeName() << " not initialized.");
    }

    // update the position of all nodes
    update();

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
        const pss bound = positionNode->calculateBoundingSphere();
        
        // this part is full of magic!
        glm::vec2 boundf = bound.getVec2f();
        glm::vec2 scaling{1.0f, -boundf[1]};
        boundf[0] *= 5.0f;
        
        psc cameraPosition = positionNode->getPosition();
        cameraPosition += psc(glm::vec4(0.f, 0.f, boundf));
        c->setPosition(cameraPosition);
        c->setCameraDirection(glm::vec3(0, 0, -1));
        c->setScaling(scaling);

        // Set the focus node for the interactionhandler
        OsEng.interactionHandler().setFocusNode(focusNode);
    }

    return true;
}

bool SceneGraph::deinitialize()
{
    // deallocate the scene graph. Recursive deallocation will occur
    delete _root;
    _root = nullptr;

    _nodes.erase(_nodes.begin(), _nodes.end());
    _allNodes.erase(_allNodes.begin(), _allNodes.end());

    _focus = "";
    _position = "";

    return true;
}

void SceneGraph::update()
{
    for (auto node : _nodes)
        node->update();
}

void SceneGraph::evaluate(Camera* camera)
{
    _root->evaluate(camera);
}

void SceneGraph::render(Camera* camera)
{
    _root->render(camera);
}

bool SceneGraph::loadScene(const std::string& sceneDescriptionFilePath,
                           const std::string& defaultModulePath)
{
    using ghoul::Dictionary;
    using ghoul::lua::loadDictionaryFromFile;

    LDEBUG("Loading scenegraph nodes");
    if (_root != nullptr) {
        LFATAL("Scenegraph already loaded");
        return false;
    }


    // initialize the root node
    _root = new SceneGraphNode();
    _root->setName(_rootNodeName);
    _nodes.push_back(_root);
    _allNodes.emplace(_rootNodeName, _root);

    Dictionary dictionary;
    loadDictionaryFromFile(sceneDescriptionFilePath, dictionary);
    Dictionary moduleDictionary;
    if (dictionary.getValue(constants::scenegraph::keyModules, moduleDictionary)) {
        std::vector<std::string> keys = moduleDictionary.keys();
        std::sort(keys.begin(), keys.end());
        for (const std::string& key : keys) {
            std::string moduleFolder;
            if (moduleDictionary.getValue(key, moduleFolder))
                loadModule(defaultModulePath + "/" + moduleFolder);
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

        SceneGraphNode* node = SceneGraphNode::createFromDictionary(element);

        _allNodes.emplace(node->nodeName(), node);
        _nodes.push_back(node);
    }

    // Print the tree
    printTree(_root);
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

}  // namespace openspace
