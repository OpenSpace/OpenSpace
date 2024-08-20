/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/camera/camera.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/easing.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <string>
#include <stack>

#include "scene_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "Scene";
    constexpr std::string_view KeyIdentifier = "Identifier";
    constexpr std::string_view KeyParent = "Parent";
    constexpr const char* RootNodeIdentifier = "Root";

#ifdef TRACY_ENABLE
    constexpr const char* renderBinToString(int renderBin) {
        // Synced with Renderable::RenderBin
        if (renderBin == 1) {
            return "Background";
        }
        else if (renderBin == 2) {
            return "Opaque";
        }
        else if (renderBin == 4) {
            return "PreDeferredTransparent";
        }
        else if (renderBin == 8) {
            return "Overlay";
        }
        else if (renderBin == 16) {
            return "PostDeferredTransparent";
        }
        else if (renderBin == 32) {
            return "Sticker";
        }
        else {
            throw ghoul::MissingCaseException();
        }
    }
#endif // TRACY_ENABLE

    std::chrono::steady_clock::time_point currentTimeForInterpolation() {
        using namespace openspace::global;
        if (sessionRecording->isSavingFramesDuringPlayback()) {
            return sessionRecording->currentPlaybackInterpolationTime();
        }
        else {
            return std::chrono::steady_clock::now();
        }
    }

    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;
} // namespace

namespace openspace {

Scene::InvalidSceneError::InvalidSceneError(std::string msg, std::string comp)
    : ghoul::RuntimeError(std::move(msg), std::move(comp))
{}

Scene::Scene(std::unique_ptr<SceneInitializer> initializer)
    : properties::PropertyOwner({"Scene", "Scene"})
    , _camera(std::make_unique<Camera>())
    , _initializer(std::move(initializer))
{
    _rootNode.setIdentifier(RootNodeIdentifier);
    _rootNode.setScene(this);
    _rootNode.setGuiHintHidden(true);

    _camera->setParent(&_rootNode);
}

Scene::~Scene() {
    LINFO("Clearing current scene graph");
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        if (node->identifier() == "Root") {
            continue;
        }

        LWARNING(std::format(
            "SceneGraphNode '{}' was not removed before shutdown",
            node->identifier()
        ));

        // There might still be scene graph nodes around that weren't removed by the asset
        // manager as they would have been added manually by the user. This also serves as
        // a backstop for assets that forgot to implement the onDeinitialize functions
        node->deinitializeGL();
        node->deinitialize();
    }
    _rootNode.clearChildren();
    _rootNode.setScene(nullptr);
}

void Scene::attachNode(ghoul::mm_unique_ptr<SceneGraphNode> node) {
    _rootNode.attachChild(std::move(node));
}

ghoul::mm_unique_ptr<SceneGraphNode> Scene::detachNode(SceneGraphNode& node) {
    return _rootNode.detachChild(node);
}

Camera* Scene::camera() const {
    return _camera.get();
}

void Scene::registerNode(SceneGraphNode* node) {
    if (_nodesByIdentifier.contains(node->identifier())) {
        throw Scene::InvalidSceneError(std::format(
            "Node with identifier '{}' already exists", node->identifier()
        ));
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
    ZoneScoped;

    sortTopologically();
    _dirtyNodeRegistry = false;
}

void Scene::sortTopologically() {
    ZoneScoped;

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
    SceneGraphNode* root = _nodesByIdentifier[RootNodeIdentifier];
    if (!root) {
        throw Scene::InvalidSceneError("No root node found");
    }

    std::unordered_map<SceneGraphNode*, size_t> inDegrees;
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        size_t inDegree = node->dependencies().size();
        if (node->parent()) {
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
            const auto it = inDegrees.find(n);
            it->second -= 1;
            if (it->second == 0) {
                zeroInDegreeNodes.push(n);
                inDegrees.erase(it);
            }
        }
        for (SceneGraphNode* n : node->children()) {
            const auto it = inDegrees.find(n);
            it->second -= 1;
            if (it->second == 0) {
                zeroInDegreeNodes.push(n);
                inDegrees.erase(it);
            }
        }
    }
    if (!inDegrees.empty()) {
        LERROR(std::format(
            "The scene contains circular dependencies. {} nodes will be disabled",
            inDegrees.size()
        ));
    }

    for (const std::pair<SceneGraphNode* const, size_t>& it : inDegrees) {
        _circularNodes.push_back(it.first);
    }

    _topologicallySortedNodes = nodes;
}

void Scene::initializeNode(SceneGraphNode* node) {
    ghoul_assert(node, "Node must not be nullptr");

    _initializer->initializeNode(node);
}

bool Scene::isInitializing() const {
    return _initializer->isInitializing();
}

void Scene::update(const UpdateData& data) {
    ZoneScoped;

    const std::vector<SceneGraphNode*> initialized = _initializer->takeInitializedNodes();
    for (SceneGraphNode* node : initialized) {
        try {
            node->initializeGL();
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
    if (_dirtyNodeRegistry) {
        updateNodeRegistry();
    }
    _camera->setAtmosphereDimmingFactor(1.f);
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        try {
            node->update(data);
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.what());
        }
    }
}

void Scene::render(const RenderData& data, RendererTasks& tasks) {
    ZoneScoped;
    ZoneText(
        renderBinToString(data.renderBinMask),
        strlen(renderBinToString(data.renderBinMask))
    );

    for (SceneGraphNode* node : _topologicallySortedNodes) {
        try {
            node->render(data, tasks);
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.what());
        }
        if (global::callback::webBrowserPerformanceHotfix) {
            (*global::callback::webBrowserPerformanceHotfix)();
        }
    }

    {
        ZoneScopedN("Get Error Hack");

        // @TODO(abock 2019-08-19) This glGetError call is a hack to prevent the GPU
        // thread and the CPU thread from diverging too much, particularly the uploading
        // of a lot of textures for the globebrowsing planets can cause a hard stuttering
        // effect. Asking for a glGetError after every rendering call will force the
        // threads to implicitly synchronize and thus prevent the stuttering.  The better
        // solution would be to reduce the number of uploads per frame, use a staggered
        // buffer, or something else like that preventing a large spike in uploads
        glGetError();
    }
}

const std::unordered_map<std::string, SceneGraphNode*>& Scene::nodesByIdentifier() const {
    return _nodesByIdentifier;
}

SceneGraphNode* Scene::root() {
    return &_rootNode;
}

const SceneGraphNode* Scene::root() const {
    return &_rootNode;
}

SceneGraphNode* Scene::sceneGraphNode(const std::string& name) const {
    const auto it = _nodesByIdentifier.find(name);
    if (it != _nodesByIdentifier.end()) {
        return it->second;
    }
    return nullptr;
}

const std::vector<SceneGraphNode*>& Scene::allSceneGraphNodes() const {
    return _topologicallySortedNodes;
}

SceneGraphNode* Scene::loadNode(const ghoul::Dictionary& nodeDictionary) {
    ZoneScoped;

    // First interpret the dictionary
    std::vector<std::string> dependencyNames;

    const std::string& nodeIdentifier = nodeDictionary.value<std::string>(KeyIdentifier);
    const bool hasParent = nodeDictionary.hasKey(KeyParent);

    if (_nodesByIdentifier.find(nodeIdentifier) != _nodesByIdentifier.end()) {
        LERROR(std::format(
            "Cannot add scene graph node '{}'. A node with that name already exists",
            nodeIdentifier
        ));
        return nullptr;
    }

    SceneGraphNode* parent = nullptr;
    if (hasParent) {
        const std::string parentIdentifier = nodeDictionary.value<std::string>(KeyParent);
        parent = sceneGraphNode(parentIdentifier);
        if (!parent) {
            // TODO: Throw exception
            LERROR(std::format(
                "Could not find parent '{}' for '{}'", parentIdentifier, nodeIdentifier
            ));
            return nullptr;
        }
    }

    ghoul::mm_unique_ptr<SceneGraphNode> node = SceneGraphNode::createFromDictionary(
        nodeDictionary
    );

    if (nodeDictionary.hasKey(SceneGraphNode::KeyDependencies)) {
        if (!nodeDictionary.hasValue<ghoul::Dictionary>(SceneGraphNode::KeyDependencies))
        {
            // TODO: Throw exception
            LERROR("Dependencies did not have the corrent type");
        }
        const ghoul::Dictionary nodeDependencies =
            nodeDictionary.value<ghoul::Dictionary>(SceneGraphNode::KeyDependencies);

        for (const std::string_view key : nodeDependencies.keys()) {
            std::string value = nodeDependencies.value<std::string>(key);
            dependencyNames.push_back(std::move(value));
        }
    }

    // Make sure all dependencies are found
    std::vector<SceneGraphNode*> dependencies;
    bool foundAllDeps = true;
    for (const std::string& depName : dependencyNames) {
        SceneGraphNode* dep = sceneGraphNode(depName);
        if (!dep) {
            // TODO: Throw exception
            LERROR(std::format(
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
    }
    else {
        attachNode(std::move(node));
    }

    rawNodePointer->setDependencies(dependencies);
    return rawNodePointer;
}

void Scene::addPropertyInterpolation(properties::Property* prop, float durationSeconds,
                                     std::string postScript,
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
    const std::chrono::steady_clock::time_point now = currentTimeForInterpolation();
    for (PropertyInterpolationInfo& info : _propertyInterpolationInfos) {
        if (info.prop == prop) {
            info.beginTime = now;
            info.durationSeconds = durationSeconds;
            info.postScript = std::move(postScript);
            info.easingFunction = func;
            // If we found it, we can break since we make sure that each property is only
            // represented once in this
            return;
        }
    }

    PropertyInterpolationInfo i = {
        .prop = prop,
        .beginTime = now,
        .durationSeconds = durationSeconds,
        .postScript = std::move(postScript),
        .easingFunction = func
    };
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
    ZoneScoped;

    using namespace std::chrono;

    const steady_clock::time_point now = currentTimeForInterpolation();
    // First, let's update the properties
    for (PropertyInterpolationInfo& i : _propertyInterpolationInfos) {
        const long long us =
            duration_cast<std::chrono::microseconds>(now - i.beginTime).count();

        const float t = glm::clamp(
            static_cast<float>(
                static_cast<double>(us) /
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

        if (i.isExpired) {
            if (!i.postScript.empty()) {
                // No sync or send because this is already inside a Lua script that was
                // triggered when the interpolation of the property was triggered,
                // therefore it has already been synced and sent to the connected nodes
                // and peers
                global::scriptEngine->queueScript(
                    std::move(i.postScript),
                    scripting::ScriptEngine::ShouldBeSynchronized::No,
                    scripting::ScriptEngine::ShouldSendToRemote::No
                );
            }

            global::eventEngine->publishEvent<events::EventInterpolationFinished>(i.prop);
        }
    }

    _propertyInterpolationInfos.erase(
        std::remove_if(
            _propertyInterpolationInfos.begin(),
            _propertyInterpolationInfos.end(),
            [](const PropertyInterpolationInfo& i) { return i.isExpired; }
        ),
        _propertyInterpolationInfos.end()
    );
}

void Scene::setPropertiesFromProfile(const Profile& p) {
    ghoul::lua::LuaState L;

    for (const Profile::Property& prop : p.properties) {
        if (prop.name.empty()) {
            LWARNING("Property name in profile was empty");
            continue;
        }
        std::string uriOrRegex = prop.name;
        std::string groupName;
        if (doesUriContainGroupTag(uriOrRegex, groupName)) {
            // Remove group name from start of regex and replace with '*'
            uriOrRegex = removeGroupNameFromUri(uriOrRegex);
        }
        _profilePropertyName = uriOrRegex;
        ghoul::lua::push(L, uriOrRegex);
        ghoul::lua::push(L, 0.0);

        std::string workingValue = prop.value;
        ghoul::trimSurroundingCharacters(workingValue, ' ');
        // Later functions expect the value to be at the last position on the stack
        propertyPushProfileValueToLua(L, workingValue);

        applyRegularExpression(
            L,
            uriOrRegex,
            allProperties(),
            0.0,
            groupName,
            ghoul::EasingFunction::Linear,
            ""
        );
        // Clear lua state stack
        lua_settop(L, 0);
    }
}

void Scene::propertyPushProfileValueToLua(ghoul::lua::LuaState& L,
                                                                 const std::string& value)
{
    _valueIsTable = false;
    ProfilePropertyLua elem = propertyProcessValue(L, value);
    if (!_valueIsTable) {
        std::visit(overloaded {
            [&L](bool v) {
                ghoul::lua::push(L, v);
            },
            [&L](float v) {
                ghoul::lua::push(L, v);
            },
            [&L](const std::string& v) {
                ghoul::lua::push(L, v);
            },
            [&L](ghoul::lua::nil_t v) {
                ghoul::lua::push(L, v);
            }
        }, elem);
    }
}

ProfilePropertyLua Scene::propertyProcessValue(ghoul::lua::LuaState& L,
                                                                 const std::string& value)
{
    ProfilePropertyLua result;
    const PropertyValueType pType = propertyValueType(value);

    switch (pType) {
        case PropertyValueType::Boolean:
            result = (value == "true");
            break;
        case PropertyValueType::Float:
            result = std::stof(value);
            break;
        case PropertyValueType::Nil:
            result = ghoul::lua::nil_t();
            break;
        case PropertyValueType::Table: {
            std::string val = value;
            ghoul::trimSurroundingCharacters(val, '{');
            ghoul::trimSurroundingCharacters(val, '}');
            handlePropertyLuaTableEntry(L, val);
            _valueIsTable = true;
            break;
        }
        case PropertyValueType::String:
        default: {
            std::string val = value;
            ghoul::trimSurroundingCharacters(val, '\"');
            ghoul::trimSurroundingCharacters(val, '[');
            ghoul::trimSurroundingCharacters(val, ']');
            result = val;
            break;
        }
    }
    return result;
}

void Scene::handlePropertyLuaTableEntry(ghoul::lua::LuaState& L, const std::string& value)
{
    PropertyValueType enclosedType = PropertyValueType::Nil;
    const size_t commaPos = value.find(',', 0);
    if (commaPos != std::string::npos) {
        enclosedType = propertyValueType(value.substr(0, commaPos));
    }
    else {
        enclosedType = propertyValueType(value);
    }

    switch (enclosedType) {
        case PropertyValueType::Boolean:
            LERROR(std::format(
                "A Lua table of bool values is not supported. (processing property '{}')",
                _profilePropertyName
            ));
            break;
        case PropertyValueType::Float:
            {
                std::vector<float> vals;
                processPropertyValueTableEntries(L, value, vals);
                ghoul::lua::push(L, vals);
            }
            break;
        case PropertyValueType::String:
            {
                std::vector<std::string> vals;
                processPropertyValueTableEntries(L, value, vals);
                ghoul::lua::push(L, vals);
            }
            break;
        case PropertyValueType::Table:
        default:
            LERROR(std::format(
                "Table-within-a-table values are not supported for profile a "
                "property (processing property '{}')", _profilePropertyName
            ));
            break;
    }
}

template <typename T>
void Scene::processPropertyValueTableEntries(ghoul::lua::LuaState& L,
    const std::string& value, std::vector<T>& table)
{
    size_t commaPos = 0;
    size_t prevPos = 0;
    std::string nextValue;
    while (commaPos != std::string::npos) {
        commaPos = value.find(',', prevPos);
        if (commaPos != std::string::npos) {
            nextValue = value.substr(prevPos, commaPos - prevPos);
            prevPos = commaPos + 1;
        }
        else {
            nextValue = value.substr(prevPos);
        }
        ghoul::trimSurroundingCharacters(nextValue, ' ');
        ProfilePropertyLua tableElement = propertyProcessValue(L, nextValue);
        try {
            table.push_back(std::get<T>(tableElement));
        }
        catch (std::bad_variant_access&) {
            LERROR(std::format(
                "Error attempting to parse profile property setting for '{}' using "
                "value = {}", _profilePropertyName, value
            ));
        }
    }
}

PropertyValueType Scene::propertyValueType(const std::string& value) {
    auto isFloatValue = [](const std::string& s) {
        try {
            float converted = std::numeric_limits<float>::min();
            converted = std::stof(s);
            return (converted != std::numeric_limits<float>::min());
        }
        catch (...) {
            return false;
        }
    };

    if (value == "true" || value == "false") {
        return PropertyValueType::Boolean;
    }
    else if (isFloatValue(value)) {
        return PropertyValueType::Float;
    }
    else if (value == "nil") {
        return PropertyValueType::Nil;
    }
    else if ((value.front() == '{') && (value.back() == '}')) {
        return PropertyValueType::Table;
    }
    else {
        return PropertyValueType::String;
    }
}

std::vector<properties::Property*> Scene::propertiesMatchingRegex(
                                                        const std::string& propertyString)
{
    return findMatchesInAllProperties(propertyString, allProperties(), "");
}

std::vector<std::string> Scene::allTags() {
    std::set<std::string> result;
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        const std::vector<std::string>& tags = node->tags();
        result.insert(tags.begin(), tags.end());
    }

    return std::vector<std::string>(result.begin(), result.end());
}

scripting::LuaLibrary Scene::luaLibrary() {
    return {
        "",
        {
            {
                "setPropertyValue",
                &luascriptfunctions::propertySetValue<false>,
                {},
                "",
                "Sets all property(s) identified by the URI (with potential wildcards) "
                "in the first argument. The second argument can be any type, but it has "
                "to match the type that the property (or properties) expect. If the "
                "third is not present or is '0', the value changes instantly, otherwise "
                "the change will take that many seconds and the value is interpolated at "
                "each step in between. The fourth parameter is an optional easing "
                "function if a 'duration' has been specified. If 'duration' is 0, this "
                "parameter value is ignored. Otherwise, it can be one of many supported "
                "easing functions. See easing.h for available functions. The fifth "
                "argument is another Lua script that will be executed when the "
                "interpolation provided in parameter 3 finishes.\n"
                "The URI is interpreted using a wildcard in which '*' is expanded to "
                "'(.*)' and bracketed components '{ }' are interpreted as group tag "
                "names. Then, the passed value will be set on all properties that fit "
                "the regex + group name combination.",
                {}
            },
            {
                "setPropertyValueSingle",
                &luascriptfunctions::propertySetValue<true>,
                {},
                "",
                "Sets the property identified by the URI in the first argument. The "
                "second argument can be any type, but it has to match the type that the "
                "property expects. If the third is not present or is '0', the value "
                "changes instantly, otherwise the change will take that many seconds and "
                "the value is interpolated at each step in between. The fourth "
                "parameter is an optional easing function if a 'duration' has been "
                "specified. If 'duration' is 0, this parameter value is ignored. "
                "Otherwise, it has to be one of the easing functions defined in the list "
                "below. This is the same as calling the setValue method and passing "
                "'single' as the fourth argument to setPropertyValue. The fifth argument "
                "is another Lua script that will be executed when the interpolation "
                "provided in parameter 3 finishes.\n Avaiable easing functions: "
                "Linear, QuadraticEaseIn, QuadraticEaseOut, QuadraticEaseInOut, "
                "CubicEaseIn, CubicEaseOut, CubicEaseInOut, QuarticEaseIn, "
                "QuarticEaseOut, QuarticEaseInOut, QuinticEaseIn, QuinticEaseOut, "
                "QuinticEaseInOut, SineEaseIn, SineEaseOut, SineEaseInOut, "
                "CircularEaseIn, CircularEaseOut, CircularEaseInOut, ExponentialEaseIn, "
                "ExponentialEaseOut, ExponentialEaseInOut, ElasticEaseIn, "
                "ElasticEaseOut, ElasticEaseInOut, BounceEaseIn, BounceEaseOut, "
                "BounceEaseInOut",
                {}
            },
            {
                "getPropertyValue",
                &luascriptfunctions::propertyGetValueDeprecated,
                {},
                "",
                "Returns the value the property, identified by the provided URI. "
                "Deprecated in favor of the 'propertyValue' function",
                {}
            },
            {
                "propertyValue",
                &luascriptfunctions::propertyGetValue,
                {},
                "",
                "Returns the value the property, identified by the provided URI. "
                "Deprecated in favor of the 'propertyValue' function",
                {}
            },
            codegen::lua::HasProperty,
            codegen::lua::PropertyDeprecated,
            codegen::lua::Property,
            codegen::lua::AddCustomProperty,
            codegen::lua::RemoveCustomProperty,
            codegen::lua::AddSceneGraphNode,
            codegen::lua::RemoveSceneGraphNode,
            codegen::lua::RemoveSceneGraphNodesFromRegex,
            codegen::lua::HasSceneGraphNode,
            codegen::lua::SceneGraphNodes,
            codegen::lua::NodeByRenderableType,
            codegen::lua::ScreenSpaceRenderables,
            codegen::lua::WorldPosition,
            codegen::lua::WorldRotation,
            codegen::lua::SetParent,
            codegen::lua::BoundingSphere,
            codegen::lua::InteractionSphere,
            codegen::lua::MakeIdentifier
        }
    };
}

std::string makeIdentifier(std::string str) {
    // Note that we want to preserve '-' and '_', but replace any other punctuation
    // marks. Hence, we first convert '_' to whitespaces to avoid them being replaced
    // in the puncutation check
    std::replace(str.begin(), str.end(), '_', ' ');
    std::replace_if(
        str.begin(),
        str.end(),
        [](unsigned char c) { return std::ispunct(c) != 0; },
        '-'
    );
    std::replace(str.begin(), str.end(), ' ', '_');
    return str;
}

}  // namespace openspace
