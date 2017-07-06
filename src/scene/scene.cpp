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

#include <openspace/scene/scene.h>

#include <openspace/openspace.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/nodeloader.h>
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
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/onscopeexit.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

#include <chrono>
#include <iostream>
#include <iterator>
#include <numeric>
#include <fstream>
#include <string>
#include <stack>
#include <unordered_map>

#include "scene_doc.inl"
#include "scene_lua.inl"

namespace {
    const char* _loggerCat = "Scene";
    const char* _moduleExtension = ".mod";
    const char* _commonModuleToken = "${COMMON_MODULE}";

    const char* KeyCamera = "Camera";
    const char* KeyFocusObject = "Focus";
    const char* KeyPositionObject = "Position";
    const char* KeyViewOffset = "Offset";

    const std::string KeyName = "Name";
    const std::string KeyParentName = "Parent";

    const char* MainTemplateFilename = "${OPENSPACE_DATA}/web/properties/main.hbs";
    const char* PropertyOwnerTemplateFilename = "${OPENSPACE_DATA}/web/properties/propertyowner.hbs";
    const char* PropertyTemplateFilename = "${OPENSPACE_DATA}/web/properties/property.hbs";
    const char* JsFilename = "${OPENSPACE_DATA}/web/properties/script.js";
} // namespace

namespace openspace {

Scene::Scene()
    : DocumentationGenerator(
        "Documented",
        "propertyOwners",
        {
            { "mainTemplate", MainTemplateFilename },
            { "propertyOwnerTemplate", PropertyOwnerTemplateFilename },
            { "propertyTemplate", PropertyTemplateFilename }
        },
        JsFilename
    )
{
    std::unique_ptr<SceneGraphNode> rootNode = std::make_unique<SceneGraphNode>();
    rootNode->setName(SceneGraphNode::RootNodeName);
    setRoot(std::move(rootNode));
}

Scene::~Scene(){
}
    
void Scene::setRoot(std::unique_ptr<SceneGraphNode> root) {
    if (_root) {
        removeNode(_root.get());
    }
    _root = std::move(root);
    _root->setScene(this);
    addNode(_root.get());
}

void Scene::setCamera(std::unique_ptr<Camera> camera) {
    _camera = std::move(camera);
}

Camera* Scene::camera() const {
    return _camera.get();
}

void Scene::addNode(SceneGraphNode* node, UpdateDependencies updateDeps) {
    // Add the node and all its children.
    node->traversePreOrder([this](SceneGraphNode* n) {
        _topologicallySortedNodes.push_back(n);
        _nodesByName[n->name()] = n;
    });
    
    if (updateDeps) {
        updateDependencies();
    }
}

void Scene::removeNode(SceneGraphNode* node, UpdateDependencies updateDeps) {
    // Remove the node and all its children.
    node->traversePostOrder([this](SceneGraphNode* node) {
        _topologicallySortedNodes.erase(
            std::remove(_topologicallySortedNodes.begin(), _topologicallySortedNodes.end(), node),
            _topologicallySortedNodes.end()
        );
        _nodesByName.erase(node->name());
    });
    
    if (updateDeps) {
        updateDependencies();
    }
}

void Scene::updateDependencies() {
    sortTopologically();
}

void Scene::sortTopologically() {
    _topologicallySortedNodes.insert(
        _topologicallySortedNodes.end(),
        std::make_move_iterator(_circularNodes.begin()),
        std::make_move_iterator(_circularNodes.end())
    );
    _circularNodes.clear();

    ghoul_assert(_topologicallySortedNodes.size() == _nodesByName.size(), "Number of scene graph nodes is inconsistent");
    
    if (_topologicallySortedNodes.empty())
        return;

    // Only the Root node can have an in-degree of 0
    SceneGraphNode* root = _nodesByName[SceneGraphNode::RootNodeName];
    if (!root) {
        throw Scene::InvalidSceneError("No root node found");
    }
    
    std::unordered_map<SceneGraphNode*, size_t> inDegrees;
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        size_t inDegree = node->dependencies().size();
        if (node->parent() != nullptr) {
            inDegree++;
            inDegrees[node] = inDegree;
        }
    }

    std::stack<SceneGraphNode*> zeroInDegreeNodes;
    zeroInDegreeNodes.push(root);
    
    std::vector<SceneGraphNode*> nodes;
    nodes.reserve(_topologicallySortedNodes.size());
    while (!zeroInDegreeNodes.empty()) {
        SceneGraphNode* node = zeroInDegreeNodes.top();
        nodes.push_back(node);
        zeroInDegreeNodes.pop();

        for (SceneGraphNode* n : node->dependentNodes()) {
            auto it = inDegrees.find(n);
            it->second -= 1;
            if (it->second == 0) {
                zeroInDegreeNodes.push(n);
                inDegrees.erase(it);
            }
        }
        for (SceneGraphNode* n : node->children()) {
            auto it = inDegrees.find(n);
            it->second -= 1;
            if (it->second == 0) {
                zeroInDegreeNodes.push(n);
                inDegrees.erase(it);
            }
        }
    }
    if (inDegrees.size() > 0) {
        LERROR("The scene contains circular dependencies. " << inDegrees.size() << " nodes will be disabled.");
    }

    for (auto it : inDegrees) {
        _circularNodes.push_back(it.first);
    }
    
    _topologicallySortedNodes = nodes;
}

void Scene::update(const UpdateData& data) {
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        try {
            LTRACE("Scene::update(begin '" + node->name() + "')");
            node->update(data);
            LTRACE("Scene::update(end '" + node->name() + "')");
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.what());
        }
    }
}

void Scene::render(const RenderData& data, RendererTasks& tasks) {
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        try {
            LTRACE("Scene::render(begin '" + node->name() + "')");
            node->render(data, tasks);
            LTRACE("Scene::render(end '" + node->name() + "')");
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.what());
        }
    }
}

void Scene::clear() {
    LINFO("Clearing current scene graph");
    _root = nullptr;
}

const std::map<std::string, SceneGraphNode*>& Scene::nodesByName() const {
    return _nodesByName;
}

SceneGraphNode* Scene::root() const {
    return _root.get();
}
    
SceneGraphNode* Scene::sceneGraphNode(const std::string& name) const {
    auto it = _nodesByName.find(name);
    if (it != _nodesByName.end()) {
        return it->second;
    }
    return nullptr;
}

const std::vector<SceneGraphNode*>& Scene::allSceneGraphNodes() const {
    return _topologicallySortedNodes;
}


SceneGraphNode* Scene::loadNode(const ghoul::Dictionary& dict) {
    std::vector<std::string> dependencyNames;

    std::string nodeName = dict.value<std::string>(KeyName);
    std::string parentName = dict.value<std::string>(KeyParentName);
    std::unique_ptr<SceneGraphNode> node = SceneGraphNode::createFromDictionary(dict);

    if (dict.hasKey(SceneGraphNode::KeyDependencies)) {
        if (!dict.hasValue<ghoul::Dictionary>(SceneGraphNode::KeyDependencies)) {
            LERROR("Dependencies did not have the corrent type");
        }
        ghoul::Dictionary nodeDependencies;
        dict.getValue(SceneGraphNode::KeyDependencies, nodeDependencies);

        std::vector<std::string> keys = nodeDependencies.keys();
        for (const std::string& key : keys) {
            std::string value = nodeDependencies.value<std::string>(key);
            dependencyNames.push_back(value);
        }
    }

    SceneGraphNode* parent = sceneGraphNode(parentName);
    if (!parent) {
        LERROR("Could not find parent '" + parentName + "' for '" + nodeName + "'");
        return nullptr;
    }

    std::vector<SceneGraphNode*> dependencies;
    bool foundAllDeps = true;
    for (const auto& depName : dependencyNames) {
        SceneGraphNode* dep = sceneGraphNode(depName);
        if (!dep) {
            LERROR("Could not find dependency '" + depName + "' for '" + nodeName + "'");
            foundAllDeps = false;
            continue;
        }
        dependencies.push_back(dep);
    }

    if (!foundAllDeps) {
        return nullptr;
    }

    SceneGraphNode* rawNodePointer = node.get();
    node->setDependencies(dependencies, SceneGraphNode::UpdateScene::No);
    parent->attachChild(std::move(node));
    return rawNodePointer;
}

std::string Scene::generateJson() const {
    std::function<std::string(properties::PropertyOwner*)> createJson =
        [&createJson](properties::PropertyOwner* owner) -> std::string
    {
        std::stringstream json;
        json << "{";
        json << "\"name\": \"" << owner->name() << "\",";

        json << "\"properties\": [";
        auto properties = owner->properties();
        for (properties::Property* p : properties) {
            json << "{";
            json << "\"id\": \"" << p->identifier() << "\",";
            json << "\"type\": \"" << p->className() << "\",";
            json << "\"fullyQualifiedId\": \"" << p->fullyQualifiedIdentifier() << "\",";
            json << "\"guiName\": \"" << p->guiName() << "\"";
            json << "}";
            if (p != properties.back()) {
                json << ",";
            }
        }
        json << "],";

        json << "\"propertyOwners\": [";
        auto propertyOwners = owner->propertySubOwners();
        for (properties::PropertyOwner* o : propertyOwners) {
            json << createJson(o);
            if (o != propertyOwners.back()) {
                json << ",";
            }
        }
        json << "]";
        json << "}";

        return json.str();
    };


    std::stringstream json;
    json << "[";
    std::vector<SceneGraphNode*> nodes = allSceneGraphNodes();
    if (!nodes.empty()) {
        json << std::accumulate(
            std::next(nodes.begin()),
            nodes.end(),
            createJson(*nodes.begin()),
            [createJson](std::string a, SceneGraphNode* n) {
            return a + "," + createJson(n);
        }
        );
    }

    json << "]";

    std::string jsonString = "";
    for (const char& c : json.str()) {
        if (c == '\'') {
            jsonString += "\\'";
        }
        else {
            jsonString += c;
        }
    }

    return jsonString;
}

scripting::LuaLibrary Scene::luaLibrary() {
    return {
        "",
        {
            {
                "setPropertyValue",
                &luascriptfunctions::property_setValue,
                "string, *",
                "Sets all property(s) identified by the URI (with potential wildcards) "
                "in the first argument. The second argument can be any type, but it has "
                "to match the type that the property (or properties) expect. If the "
                "first term (separated by '.') in the uri is bracketed with { }, then "
                "this term is treated as a group tag name, and the function will "
                "search through all property owners to find those that are tagged with "
                "this group name, and set their property values accordingly."
            },
            {
                "setPropertyValueRegex",
                &luascriptfunctions::property_setValueRegex,
                "string, *",
                "Sets all property(s) that pass the regular expression in the first "
                "argument. The second argument can be any type, but it has to match "
                "the type of the properties that matched the regular expression. "
                "The regular expression has to be of the ECMAScript grammar. If the "
                "first term (separated by '.') in the uri is bracketed with { }, then "
                "this term is treated as a group tag name, and the function will search "
                "through all property owners to find those that are tagged with this "
                "group name, and set their property values accordingly."
            },
            {
                "setPropertyValueSingle",
                &luascriptfunctions::property_setValueSingle,
                "string, *",
                "Sets all property(s) identified by the URI in the first argument to the "
                "value passed in the second argument. The type of the second argument is "
                "arbitrary, but it must agree with the type the denoted Property expects."
                " If the first term (separated by '.') in the uri is bracketed with { }, "
                " then this term is treated as a group tag name, and the function will "
                "search through all property owners to find those that are tagged with "
                "this group name, and set their property values accordingly."
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
            },
            {
                "addSceneGraphNode",
                &luascriptfunctions::addSceneGraphNode,
                "table",
                "Loads the SceneGraphNode described in the table and adds it to the "
                "SceneGraph"
            },
            {
                "removeSceneGraphNode",
                &luascriptfunctions::removeSceneGraphNode,
                "string",
                "Removes the SceneGraphNode identified by name"
            }
        }
    };
}

Scene::InvalidSceneError::InvalidSceneError(const std::string& error, const std::string& comp)
    : ghoul::RuntimeError(error, comp)
{}

}  // namespace openspace
