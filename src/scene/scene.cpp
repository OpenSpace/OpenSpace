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
#include <openspace/interaction/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/sceneloader.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/script_helper.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
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
    const char* MostProbableAttachedNode = "SolarSystemBarycenter";
    const char* ParentOfAllNodes = "Root";
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
    ),
    _dsgAttachedNodeName(MostProbableAttachedNode)
{}

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

const std::string & Scene::dsgAttachedNodeName() const {
    return _dsgAttachedNodeName;
}

void Scene::setDsgAttachedNodeName(const std::string & attachedNodeName) {
    _dsgAttachedNodeName = attachedNodeName;
}

void Scene::updateDsgAttachedNode(const Camera* camera) {
    _dsgAttachedNodeName = currentDsgAttachedNode(camera, _dsgAttachedNodeName);
}

std::string Scene::currentDsgAttachedNode(const Camera* camera, const std::string & dsgAttachedNodeName) const {
    if (camera == nullptr || dsgAttachedNodeName.empty()) {
        LERROR("Camera object not allocated or empty name scene passed to the method.");
        return MostProbableAttachedNode; // This choice is controversal. Better to avoid a crash.
    }
    const SceneGraphNode* node = sceneGraphNode(dsgAttachedNodeName);

    if (node == nullptr) {
        LERROR("There is no scenegraph node with name: " << dsgAttachedNodeName);
        return MostProbableAttachedNode;
    }

    // Starts in the last sphere of influency we know we were in, check if we are still inside it 
    // if not, check its parent and continue until we are inside a scene
    double distance = glm::length(camera->positionVec3() - node->dynamicWorldPosition());

    // Traverses the scenetree to find a scene we are within. 
    while (distance > node->sphereOfInfluency()) {
        if (node->parent() != nullptr) {
            node = node->parent();
            distance = glm::length(camera->positionVec3() - node->dynamicWorldPosition());
        }
        else {
            break;
        }
    }

    std::string attachedNodeName(node->name());
    std::vector<SceneGraphNode*> childrenScene(node->children());
    
    //Check if we are inside a child scene of the current scene. 
    bool outsideAllChildScenes = false;

    while (!childrenScene.empty() && !outsideAllChildScenes) {
        double lastChildDistanceValid = std::numeric_limits<double>::max();
        bool changedChild = false;
        for (auto childNode: childrenScene) {
            double childDistance = glm::length(camera->positionVec3() - childNode->dynamicWorldPosition());
                
            // Set the new scene that we are inside the scene radius.
            if ((childDistance < lastChildDistanceValid) && (childDistance < childNode->sphereOfInfluency())) {
                lastChildDistanceValid = childDistance;
                node = childNode;                
                changedChild = true;
            }
        }
        if (!changedChild) {
            outsideAllChildScenes = true;
        }
        else {
            childrenScene = node->children();
        }
    }

    return node->name();
}

const glm::dvec3 Scene::currentDisplacementPosition(const std::string & cameraParent,
    const SceneGraphNode* target) const {
    if (target != nullptr) {

        std::vector<const SceneGraphNode*> cameraPath;
        std::vector<const SceneGraphNode*> targetPath;
        std::vector<const SceneGraphNode*> commonParentPath;

        const SceneGraphNode* cameraParentNode = sceneGraphNode(cameraParent);             

        //Find common parent for camera and object
        std::string commonParentName(cameraParent);  // initiates to camera parent in case 
                                                     // other path is not found
        const SceneGraphNode * targetNode = sceneGraphNode(target->name());
        cameraPath = pathTo(cameraParentNode);
        targetPath = pathTo(targetNode);

        const SceneGraphNode* commonParentNode = findCommonParentNode(cameraParentNode, targetNode);
        commonParentPath = pathTo(commonParentNode);

        //Find the path from the camera to the common parent

        glm::dvec3 collectorCamera(pathCollector(cameraPath, commonParentNode->name(), true));
        glm::dvec3 collectorTarget(pathCollector(targetPath, commonParentNode->name(), false));

        return collectorTarget + collectorCamera;
    }
    else {
        LERROR("Target scenegraph node is null.");
        return glm::dvec3(0.0);
    }
}

const glm::dmat4 Scene::currentMatrixTransformation(const std::string & cameraParent,
    const SceneGraphNode* target) const {
    if (target != nullptr) {

        std::vector<const SceneGraphNode*> cameraPath;
        std::vector<const SceneGraphNode*> targetPath;
        std::vector<const SceneGraphNode*> commonParentPath;

        const SceneGraphNode* cameraParentNode = sceneGraphNode(cameraParent);

        //Find common parent for camera and object
        std::string commonParentName(cameraParent);  // initiates to camera parent in case 
                                                     // other path is not found
        const SceneGraphNode * targetNode = sceneGraphNode(target->name());
        cameraPath = pathTo(cameraParentNode);
        targetPath = pathTo(targetNode);

        const SceneGraphNode* commonParentNode = findCommonParentNode(cameraParentNode, targetNode);
        commonParentPath = pathTo(commonParentNode);

        //Find the path from the camera to the common parent

        glm::dmat4 collectorCamera(matrixCollector(cameraPath, commonParentNode->name(), false));
        glm::dmat4 collectorTarget(matrixCollector(targetPath, commonParentNode->name(), true));

        return collectorCamera * collectorTarget;
    }
    else {
        LERROR("Target scenegraph node is null.");
        return glm::dmat4(1.0);
    }
}


SceneGraphNode* Scene::findCommonParentNode(const SceneGraphNode * firstNode, const SceneGraphNode * secondNode) const {
    if (!firstNode || !secondNode) {
        LERROR("Empty scenegraph node pointer passed to the method.");
        return sceneGraphNode(ParentOfAllNodes); // This choice is controversal. Better to avoid a crash.
    }

    std::string strCommonParent = commonParent(pathTo(firstNode), pathTo(secondNode));

    return sceneGraphNode(strCommonParent);
}

std::vector<const SceneGraphNode*> Scene::pathTo(const SceneGraphNode* node) const {
    std::vector<const SceneGraphNode*> path;

    if (node == nullptr) {
        LERROR("Invalid (null) scenegraph node name passed to pathTo() method.");
        return path;
    }
    const SceneGraphNode *tmpNode = node;
    while (tmpNode->parent() != nullptr) {
        path.push_back(tmpNode);
        tmpNode = tmpNode->parent();
    }
    path.push_back(tmpNode);
    
    return path;
}

std::string Scene::commonParent(const std::vector<const SceneGraphNode*> & t1, const std::vector<const SceneGraphNode*> & t2) const {
    if (t1.empty() && t2.empty()) {
        LERROR("Empty paths passed to commonParent method.");
        return ParentOfAllNodes;
    }

    std::string commonParentReturn(MostProbableAttachedNode);
    int iterator = 0;
    int min = std::min(t1.size(), t2.size());
    int iteratorT1 = t1.size() - 1;
    int iteratorT2 = t2.size() - 1;
    while (iterator < min && t1[iteratorT1]->name() == t2[iteratorT2]->name()) {
        commonParentReturn = t1[iteratorT1]->name();
        --iteratorT1;
        --iteratorT2;
        iterator++;
    }

    return commonParentReturn;
}

glm::dvec3 Scene::pathCollector(const std::vector<const SceneGraphNode*> & path, const std::string & commonParentName,
    const bool inverse) const {
    if (path.empty() || commonParentName.empty()) {
        LERROR("Empty path or common parent name passed to pathCollector method.");
        return glm::dvec3();
    }

    const SceneGraphNode* firstElement = path.front();
    glm::dvec3 collector(path.back()->position());

    int depth = 0;
    // adds all elements to the collector, continues untill commomParent is found.
    while (firstElement->name() != commonParentName) {
        if (inverse)
            collector = collector - firstElement->position();
        else
            collector = collector + firstElement->position();

        firstElement = path[++depth];
    }

    return collector;
}

glm::dmat4 Scene::matrixCollector(const std::vector<const SceneGraphNode*> & path, const std::string & commonParentName,
    const bool inverse) const {
   
    if (path.empty() || commonParentName.empty()) {
        LERROR("Empty path or common parent name passed to matrixCollector method.");
        return glm::dmat4(1.0);
    }

    const SceneGraphNode* firstElement = path.front();    

    if (firstElement->name() == commonParentName) {
        if (inverse)
            return glm::inverse(firstElement->modelTransform());
        else
            return firstElement->modelTransform();
    }
        

    int depth = 0;
    glm::dmat4 collector = glm::dmat4(1.0);

    // adds all elements to the collector, continues untill commomParent is found.    
    while (firstElement->name() != commonParentName) {
        if (inverse)
            collector = glm::inverse(firstElement->modelTransform()) * collector;
        else
            collector = firstElement->modelTransform() * collector;

        firstElement = path[++depth];
    }

    return glm::dmat4(0);
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

void Scene::initialize() {
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        try {
            node->initialize();
        }
        catch (const ghoul::RuntimeError& e) {
            LERROR(node->name() << " not initialized.");
            LERRORC(std::string(_loggerCat) + "(" + e.component + ")", e.what());
        }
    }
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
            json << "\"guiName\": \"" << p->guiName() << "\",";
            json << "\"description\": \"" << escapedJson(p->description()) << "\"";
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
