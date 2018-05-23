/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#pragma optimize ("", off)
#include <fstream>

#include <openspace/scene/scene.h>

#include <openspace/openspace.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/loadingscreen.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/misc/threadpool.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <algorithm>
#include <chrono>
#include <iostream>
#include <iterator>
#include <fstream>
#include <string>
#include <stack>
#include <unordered_map>
#include <vector>

#include "scene_lua.inl"

std::fstream ff("d:/test.csv", std::fstream::out);

namespace {
    constexpr const char* _loggerCat = "Scene";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyParent = "Parent";
} // namespace

namespace openspace {

Scene::Scene(std::unique_ptr<SceneInitializer> initializer)
    : properties::PropertyOwner({"Scene", "Scene"})
    , _dirtyNodeRegistry(false)
    , _initializer(std::move(initializer))
{
    _rootDummy.setIdentifier(SceneGraphNode::RootNodeIdentifier);
    _rootDummy.setScene(this);
}

Scene::~Scene() {
    clear();
    _rootDummy.setScene(nullptr);
}

void Scene::attachNode(std::unique_ptr<SceneGraphNode> node) {
    _rootDummy.attachChild(std::move(node));
}

std::unique_ptr<SceneGraphNode> Scene::detachNode(SceneGraphNode& node) {
    return _rootDummy.detachChild(node);
}

void Scene::setCamera(std::unique_ptr<Camera> camera) {
    _camera = std::move(camera);
}

Camera* Scene::camera() const {
    return _camera.get();
}

void Scene::registerNode(SceneGraphNode* node) {
    if (_nodesByIdentifier.count(node->identifier())){
        throw Scene::InvalidSceneError(
            "Node with identifier " + node->identifier() + " already exits."
        );
    }

    _topologicallySortedNodes.push_back(node);
    _nodesByIdentifier[node->identifier()] = node;
    addPropertySubOwner(node);
    _dirtyNodeRegistry = true;
}

void Scene::unregisterNode(SceneGraphNode* node) {
    _topologicallySortedNodes.erase(
        std::remove(
            _topologicallySortedNodes.begin(),
            _topologicallySortedNodes.end(),
            node
        ),
        _topologicallySortedNodes.end()
    );
    _nodesByIdentifier.erase(node->identifier());
    // Just try to remove all properties; if the property doesn't exist, the
    // removeInterpolation will not do anything
    for (properties::Property* p : node->properties()) {
        removePropertyInterpolation(p);
    }
    removePropertySubOwner(node);
    _dirtyNodeRegistry = true;
}

void Scene::markNodeRegistryDirty() {
    _dirtyNodeRegistry = true;
}

void Scene::updateNodeRegistry() {
    sortTopologically();
    _dirtyNodeRegistry = false;
}

void Scene::addSceneLicense(SceneLicense license) {
    _licenses.push_back(std::move(license));
}

void Scene::sortTopologically() {
    _topologicallySortedNodes.insert(
        _topologicallySortedNodes.end(),
        std::make_move_iterator(_circularNodes.begin()),
        std::make_move_iterator(_circularNodes.end())
    );
    _circularNodes.clear();

    ghoul_assert(
        _topologicallySortedNodes.size() == _nodesByIdentifier.size(),
        "Number of scene graph nodes is inconsistent"
    );

    if (_topologicallySortedNodes.empty()) {
        return;
    }

    // Only the Root node can have an in-degree of 0
    SceneGraphNode* root = _nodesByIdentifier[SceneGraphNode::RootNodeIdentifier];
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
        LERROR(fmt::format(
            "The scene contains circular dependencies. {} nodes will be disabled",
            inDegrees.size()
        ));
    }

    for (auto it : inDegrees) {
        _circularNodes.push_back(it.first);
    }

    _topologicallySortedNodes = nodes;
}

void Scene::initializeNode(SceneGraphNode* node) {
    _initializer->initializeNode(node);
}

bool Scene::isInitializing() const {
    return _initializer->isInitializing();
}

/*

void Scene::initialize() {
    bool useMultipleThreads = true;
    if (OsEng.configurationManager().hasKey(
        ConfigurationManager::KeyUseMultithreadedInitialization
    ))
    {
        useMultipleThreads = OsEng.configurationManager().value<bool>(
            ConfigurationManager::KeyUseMultithreadedInitialization
        );
    }

    auto initFunction = [](SceneGraphNode* node){
        try {
            OsEng.loadingScreen().updateItem(
                node->name(),
                LoadingScreen::ItemStatus::Initializing
            );
            node->initialize();
            OsEng.loadingScreen().tickItem();
            OsEng.loadingScreen().updateItem(
                node->name(),
                LoadingScreen::ItemStatus::Finished
            );
        }
        catch (const ghoul::RuntimeError& e) {
            LERROR(node->name() << " not initialized.");
            LERRORC(std::string(_loggerCat) + "(" + e.component + ")", e.what());
            OsEng.loadingScreen().updateItem(
                node->name(),
                LoadingScreen::ItemStatus::Failed
            );
        }

    };

    if (useMultipleThreads) {
        unsigned int nThreads = std::thread::hardware_concurrency();

        ghoul::ThreadPool pool(nThreads == 0 ? 2 : nThreads - 1);

        OsEng.loadingScreen().postMessage("Initializing scene");

        for (SceneGraphNode* node : _topologicallySortedNodes) {
            pool.queue(initFunction, node);
        }

        pool.stop();
    }
    else {
        for (SceneGraphNode* node : _topologicallySortedNodes) {
            initFunction(node);
        }
    }
}

void Scene::initializeGL() {
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        try {
            node->initializeGL();
        }
        catch (const ghoul::RuntimeError& e) {
            LERROR(node->name() << " not initialized.");
            LERRORC(std::string(_loggerCat) + "(" + e.component + ")", e.what());
        }
    }
}
*/

void Scene::update(const UpdateData& data) {
    std::vector<SceneGraphNode*> initializedNodes =
        _initializer->getInitializedNodes();

    for (SceneGraphNode* node : initializedNodes) {
        try {
            node->initializeGL();
        } catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
    if (_dirtyNodeRegistry) {
        updateNodeRegistry();
    }
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        try {
            LTRACE("Scene::update(begin '" + node->identifier() + "')");
            node->update(data);
            LTRACE("Scene::update(end '" + node->identifier() + "')");
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.what());
        }
    }
}

void Scene::render(const RenderData& data, RendererTasks& tasks) {
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        try {
            LTRACE("Scene::render(begin '" + node->identifier() + "')");
            node->render(data, tasks);
            LTRACE("Scene::render(end '" + node->identifier() + "')");
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.what());
        }
    }
}

void Scene::clear() {
    LINFO("Clearing current scene graph");
    _rootDummy.clearChildren();
}

const std::unordered_map<std::string, SceneGraphNode*>& Scene::nodesByIdentifier() const {
    return _nodesByIdentifier;
}

SceneGraphNode* Scene::root() {
    return &_rootDummy;
}

const SceneGraphNode* Scene::root() const {
    return &_rootDummy;
}

SceneGraphNode* Scene::sceneGraphNode(const std::string& name) const {
    auto it = _nodesByIdentifier.find(name);
    if (it != _nodesByIdentifier.end()) {
        return it->second;
    }
    return nullptr;
}

const std::vector<SceneGraphNode*>& Scene::allSceneGraphNodes() const {
    return _topologicallySortedNodes;
}

SceneGraphNode* Scene::loadNode(const ghoul::Dictionary& dict) {
    // First interpret the dictionary
    std::vector<std::string> dependencyNames;

    const std::string nodeIdentifier = dict.value<std::string>(KeyIdentifier);
    const bool hasParent = dict.hasKey(KeyParent);

    if (_nodesByIdentifier.find(nodeIdentifier) != _nodesByIdentifier.end()) {
        LERROR(fmt::format(
            "Cannot add scene graph node '{}'. A node with that name already exists",
            nodeIdentifier
        ));
        return nullptr;
    }

    SceneGraphNode* parent = nullptr;
    if (hasParent) {
        const std::string parentIdentifier = dict.value<std::string>(KeyParent);
        parent = sceneGraphNode(parentIdentifier);
        if (!parent) {
            // TODO: Throw exception
            LERROR(fmt::format(
                "Could not find parent '{}' for '{}'", parentIdentifier, nodeIdentifier
            ));
            return nullptr;
        }
    }

    std::unique_ptr<SceneGraphNode> node = SceneGraphNode::createFromDictionary(dict);
    if (!node) {
        // TODO: Throw exception
        LERROR("Could not create node from dictionary: " + nodeIdentifier);
    }

    if (dict.hasKey(SceneGraphNode::KeyDependencies)) {
        if (!dict.hasValue<ghoul::Dictionary>(SceneGraphNode::KeyDependencies)) {
            // TODO: Throw exception
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

    // Make sure all dependencies are found
    std::vector<SceneGraphNode*> dependencies;
    bool foundAllDeps = true;
    for (const std::string& depName : dependencyNames) {
        SceneGraphNode* dep = sceneGraphNode(depName);
        if (!dep) {
            // TODO: Throw exception
            LERROR(fmt::format(
                "Could not find dependency '{}' for '{}'", depName, nodeIdentifier
            ));
            foundAllDeps = false;
            continue;
        }
        dependencies.push_back(dep);
    }

    if (!foundAllDeps) {
        return nullptr;
    }

    // Now attach the node to the graph
    SceneGraphNode* rawNodePointer = node.get();

    if (parent) {
        parent->attachChild(std::move(node));
    } else {
        attachNode(std::move(node));
    }

    rawNodePointer->setDependencies(dependencies);
    return rawNodePointer;
}

void Scene::addPropertyInterpolation(properties::Property* prop, float durationSeconds,
                             ghoul::EasingFunction easingFunction)
{
    ghoul_precondition(prop != nullptr, "prop must not be nullptr");
    ghoul_precondition(durationSeconds > 0.f, "durationSeconds must be positive");
    ghoul_postcondition(
        std::find_if(
            _propertyInterpolationInfos.begin(),
            _propertyInterpolationInfos.end(),
            [prop](const PropertyInterpolationInfo& info) {
                return info.prop == prop && !info.isExpired;
            }
        ) != _propertyInterpolationInfos.end(),
        "A new interpolation record exists for p that is not expired"
    );

    ghoul::EasingFunc<float> func =
        (easingFunction == ghoul::EasingFunction::Linear) ?
        nullptr :
        ghoul::easingFunction<float>(easingFunction);

    // First check if the current property already has an interpolation information
    for (std::vector<PropertyInterpolationInfo>::iterator it = _propertyInterpolationInfos.begin();
        it != _propertyInterpolationInfos.end();
        ++it)
    {
        if (it->prop == prop) {
            it->beginTime = std::chrono::steady_clock::now();
            it->durationSeconds = durationSeconds;
            it->easingFunction = func;
            // If we found it, we can break since we make sure that each property is only
            // represented once in this
            return;
        }
    }

    PropertyInterpolationInfo i;
    i.prop = prop;
    i.beginTime = std::chrono::steady_clock::now();
    i.durationSeconds = durationSeconds;
    i.easingFunction = func;

    _propertyInterpolationInfos.push_back(std::move(i));
}

void Scene::removePropertyInterpolation(properties::Property* prop) {
    ghoul_precondition(prop != nullptr, "prop must not be nullptr");
    ghoul_postcondition(
        std::find_if(
            _propertyInterpolationInfos.begin(),
            _propertyInterpolationInfos.end(),
            [prop](const PropertyInterpolationInfo& info) {
                return info.prop == prop;
            }
        ) == _propertyInterpolationInfos.end(),
        "No interpolation record exists for prop"
    );

    _propertyInterpolationInfos.erase(
        std::remove_if(
            _propertyInterpolationInfos.begin(),
            _propertyInterpolationInfos.end(),
            [prop](const PropertyInterpolationInfo& info) { return info.prop == prop; }
        ),
        _propertyInterpolationInfos.end()
    );
}

void Scene::updateInterpolations() {
    using namespace std::chrono;
    auto now = steady_clock::now();

    // First, let's update the properties
    for (PropertyInterpolationInfo& i : _propertyInterpolationInfos) {
        long long usPassed = duration_cast<std::chrono::microseconds>(
            now - i.beginTime
        ).count();

        const float t = glm::clamp(
            static_cast<float>(
                static_cast<double>(usPassed) /
                static_cast<double>(i.durationSeconds * 1000000)
            ),
            0.f,
            1.f
        );

        // @FRAGILE(abock): This method might crash if someone deleted the property
        //                  underneath us. We take care of removing entire PropertyOwners,
        //                  but we assume that Propertys live as long as their
        //                  SceneGraphNodes. This is true in general, but if Propertys are
        //                  created and destroyed often by the SceneGraphNode, this might
        //                  become a problem.
        i.prop->interpolateValue(t, i.easingFunction);

        i.isExpired = (t == 1.f);
    }

    _propertyInterpolationInfos.erase(
        std::remove_if(
            _propertyInterpolationInfos.begin(),
            _propertyInterpolationInfos.end(),
            [](const PropertyInterpolationInfo& i) {
                return i.isExpired;
            }
        ),
        _propertyInterpolationInfos.end()
    );
}

void Scene::writeSceneLicenseDocumentation(const std::string& path) const {
    SceneLicenseWriter writer(_licenses);
    writer.writeDocumentation(path);
}

scripting::LuaLibrary Scene::luaLibrary() {
    return {
        "",
        {
            {
                "setPropertyValue",
                &luascriptfunctions::property_setValue,
                {},
                "name, value [, duration, easing, optimization]",
                "Sets all property(s) identified by the URI (with potential wildcards) "
                "in the first argument. The second argument can be any type, but it has "
                "to match the type that the property (or properties) expect. If the "
                "third is not present or is '0', the value changes instantly, otherwise "
                "the change will take that many seconds and the value is interpolated at "
                "each steap in between. The fourth parameter is an optional easing "
                "function if a 'duration' has been specified. If 'duration' is 0, this "
                "parameter value is ignored. Otherwise, it can be one of many supported "
                "easing functions. See easing.h for available functions. The fifth "
                "argument must be either empty, 'regex', or 'single'. If the last "
                "argument is empty (the default), the URI is interpreted using a "
                "wildcard in which '*' is expanded to '(.*)' and bracketed components "
                "'{ }' are interpreted as group tag names. Then, the passed value will "
                "be set on all properties that fit the regex + group name combination. "
                "If the third argument is 'regex' neither the '*' expansion, nor the "
                "group tag expansion is performed and the first argument is used as an "
                "ECMAScript style regular expression that matches against the fully "
                "qualified IDs of properties. If the third arugment is 'single' no "
                "substitutions are performed and exactly 0 or 1 properties are changed."
            },
            {
                "setPropertyValueSingle",
                &luascriptfunctions::property_setValueSingle,
                {},
                "URI, value [, duration, easing]",
                "Sets the property identified by the URI in the first argument. The "
                "second argument can be any type, but it has to match the type that the "
                "property expects. If the third is not present or is '0', the value "
                "changes instantly, otherwise the change will take that many seconds and "
                "the value is interpolated at each steap in between. The fourth "
                "parameter is an optional easing function if a 'duration' has been "
                "specified. If 'duration' is 0, this parameter value is ignored. "
                "Otherwise, it has to be 'linear', 'easein', 'easeout', or 'easeinout'. "
                "This is the same as calling the setValue method and passing 'single' as "
                "the fourth argument to setPropertyValue."
            },
            {
                "getPropertyValue",
                &luascriptfunctions::property_getValue,
                {},
                "string",
                "Returns the value the property, identified by "
                "the provided URI."
            },
            {
                "loadScene",
                &luascriptfunctions::loadScene,
                {},
                "string",
                "Loads the scene found at the file passed as an "
                "argument. If a scene is already loaded, it is unloaded first"
            },
            {
                "addSceneGraphNode",
                &luascriptfunctions::addSceneGraphNode,
                {},
                "table",
                "Loads the SceneGraphNode described in the table and adds it to the "
                "SceneGraph"
            },
            {
                "removeSceneGraphNode",
                &luascriptfunctions::removeSceneGraphNode,
                {},
                "string",
                "Removes the SceneGraphNode identified by name"
            },
            {
                "hasSceneGraphNode",
                &luascriptfunctions::hasSceneGraphNode,
                {},
                "string",
                "Checks whether the specifies SceneGraphNode is present in the current "
                "scene"
            }
        }
    };
}

Scene::InvalidSceneError::InvalidSceneError(const std::string& error,
                                            const std::string& comp)
    : ghoul::RuntimeError(error, comp)
{}

}  // namespace openspace
