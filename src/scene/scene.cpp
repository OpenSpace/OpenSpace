/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <openspace/engine/globalscallbacks.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/sessionrecordinghandler.h>
#include <openspace/query/query.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/lua/lua_helper.h>
#include <source_location>
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
        if (sessionRecordingHandler->isSavingFramesDuringPlayback()) {
            return sessionRecordingHandler->currentPlaybackInterpolationTime();
        }
        else {
            return std::chrono::steady_clock::now();
        }
    }

    using ProfilePropertyLua = std::variant<bool, float, std::string, ghoul::lua::nil_t>;
    openspace::PropertyValueType propertyValueType(const std::string& value);
    void handlePropertyLuaTableEntry(ghoul::lua::LuaState& L, const std::string& value,
        bool& isTableValue, std::string_view propertyName);
    ProfilePropertyLua propertyProcessValue(ghoul::lua::LuaState& L,
        const std::string& value, bool& valueIsTable, std::string_view propertyName);
    template <typename T>
    void processPropertyValueTableEntries(ghoul::lua::LuaState& L,
        const std::string& value, std::vector<T>& table, bool& valueIsTable,
        std::string_view propertyName);


    /**
     * Accepts string version of a property value from a profile, and returns the
     * supported data types that can be pushed to a Lua state. Currently, the full range
     * of possible Lua values is not supported.
     *
     * \param value String representation of the value with which to set property
     */
    openspace::PropertyValueType propertyValueType(const std::string& value) {
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
            return openspace::PropertyValueType::Boolean;
        }
        else if (isFloatValue(value)) {
            return openspace::PropertyValueType::Float;
        }
        else if (value == "nil") {
            return openspace::PropertyValueType::Nil;
        }
        else if ((value.front() == '{') && (value.back() == '}')) {
            return openspace::PropertyValueType::Table;
        }
        else {
            return openspace::PropertyValueType::String;
        }
    }

    /**
     * Handles a Lua table entry, creating a vector of the correct variable type based
     * on the profile string, and pushes this vector to the Lua stack.
     *
     * \param L The Lua state to (eventually) push to
     * \param value String representation of the value with which to set property
     */
    void handlePropertyLuaTableEntry(ghoul::lua::LuaState& L, const std::string& value,
                                     bool& isTableValue, std::string_view propertyName)
    {
        openspace::PropertyValueType enclosedType = openspace::PropertyValueType::Nil;
        const size_t commaPos = value.find(',', 0);
        if (commaPos != std::string::npos) {
            enclosedType = propertyValueType(value.substr(0, commaPos));
        }
        else {
            enclosedType = propertyValueType(value);
        }

        switch (enclosedType) {
            case openspace::PropertyValueType::Boolean:
                LERROR(std::format(
                    "A Lua table of bool values is not supported. (processing "
                    "property '{}')", propertyName
                ));
                break;
            case openspace::PropertyValueType::Float:
            {
                std::vector<float> vals;
                processPropertyValueTableEntries(
                    L,
                    value,
                    vals,
                    isTableValue,
                    propertyName
                );
                ghoul::lua::push(L, vals);
            }
            break;
            case openspace::PropertyValueType::String:
            {
                std::vector<std::string> vals;
                processPropertyValueTableEntries(
                    L,
                    value,
                    vals,
                    isTableValue,
                    propertyName
                );
                ghoul::lua::push(L, vals);
            }
            break;
            case openspace::PropertyValueType::Table:
            default:
                LERROR(std::format(
                    "Table-within-a-table values are not supported for profile a "
                    "property (processing property '{}')", propertyName
                ));
                break;
        }
    }


    /**
     * Accepts string version of a property value from a profile, and processes it
     * according to the data type of the value.
     *
     * \param L The Lua state to (eventually) push to
     * \param value String representation of the value with which to set property
     * \return The ProfilePropertyLua variant type translated from string representation
     */
    ProfilePropertyLua propertyProcessValue(ghoul::lua::LuaState& L,
                                            const std::string& value, bool& valueIsTable,
                                            std::string_view propertyName)
    {
        ProfilePropertyLua result;
        const openspace::PropertyValueType pType = propertyValueType(value);

        switch (pType) {
            case openspace::PropertyValueType::Boolean:
                result = (value == "true");
                break;
            case openspace::PropertyValueType::Float:
                result = std::stof(value);
                break;
            case openspace::PropertyValueType::Nil:
                result = ghoul::lua::nil_t();
                break;
            case openspace::PropertyValueType::Table: {
                std::string val = value;
                ghoul::trimSurroundingCharacters(val, '{');
                ghoul::trimSurroundingCharacters(val, '}');
                handlePropertyLuaTableEntry(L, val, valueIsTable, propertyName);
                valueIsTable = true;
                break;
            }
            case openspace::PropertyValueType::String:
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

    /**
     * Accepts string version of a property value from a profile, and adds it to a vector
     * which will later be used to push as a Lua table containing values of type T
     *
     * \param L The Lua state to (eventually) push to
     * \param value String representation of the value with which to set property
     * \param table The std::vector container which has elements of type T for a Lua table
     */
    template <typename T>
    void processPropertyValueTableEntries(ghoul::lua::LuaState& L,
                                          const std::string& value, std::vector<T>& table,
                                          bool& valueIsTable,
                                          std::string_view propertyName)
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
            ProfilePropertyLua t = propertyProcessValue(
                L,
                nextValue,
                valueIsTable,
                propertyName
            );

            try {
                table.push_back(std::get<T>(t));
            }
            catch (std::bad_variant_access&) {
                LERROR(std::format(
                    "Error attempting to parse profile property setting for '{}' using "
                    "value = {}", propertyName, value
                ));
            }
        }
    }

    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;
} // namespace

namespace openspace {

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
        throw ghoul::RuntimeError(std::format(
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
        throw ghoul::RuntimeError("No root node found");
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

        // This method might crash if someone deleted the property underneath us. We take
        // care of removing entire PropertyOwners, but we assume that Propertys live as
        // long as their SceneGraphNodes. This is true in general, but if Propertys are
        // created and destroyed often by the SceneGraphNode, this might become a problem.
        i.prop->interpolateValue(t, i.easingFunction);

        i.isExpired = (t == 1.f);

        if (i.isExpired) {
            if (!i.postScript.empty()) {
                // No sync or send because this is already inside a Lua script that was
                // triggered when the interpolation of the property was triggered,
                // therefore it has already been synced and sent to the connected nodes
                // and peers
                using Script = scripting::ScriptEngine::Script;
                global::scriptEngine->queueScript({
                    .code = std::move(i.postScript),
                    .synchronized = Script::ShouldBeSynchronized::No,
                    .sendToRemote = Script::ShouldSendToRemote::No
                });
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
        std::string groupName = groupTag(uriOrRegex);
        if (!groupName.empty()) {
            // Remove group name from start of regex and replace with '*'
            uriOrRegex = removeGroupTagFromUri(uriOrRegex);
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
    ProfilePropertyLua elem = propertyProcessValue(
        L,
        value,
        _valueIsTable,
        _profilePropertyName
    );
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

std::vector<properties::Property*> Scene::propertiesMatchingRegex(
                                                          std::string_view propertyString)
{
    return findMatchesInAllProperties(propertyString, "");
}

std::vector<std::string> Scene::allTags() const {
    std::set<std::string> result;
    for (SceneGraphNode* node : _topologicallySortedNodes) {
        const std::vector<std::string>& tags = node->tags();
        result.insert(tags.begin(), tags.end());
    }

    return std::vector<std::string>(result.begin(), result.end());
}

void Scene::setGuiTreeOrder(const std::string& guiPath,
                            const std::vector<std::string>& list)
{
    _guiTreeOrderMap[guiPath] = list;
    global::eventEngine->publishEvent<events::EventGuiTreeUpdated>();
}

ghoul::Dictionary Scene::guiTreeOrder() const {
    ghoul::Dictionary dict;
    for (const auto& [key, list] : _guiTreeOrderMap) {
        dict.setValue(key, list);
    }
    return dict;
}

scripting::LuaLibrary Scene::luaLibrary() {
    return {
        "",
        {
            {
                "setPropertyValue",
                &luascriptfunctions::propertySetValue<false>,
                {
                    { "uri", "String" },
                    { "value", "Nil | String | Number | Boolean | Table" },
                    { "duration", "Number?", "0.0" },
                    { "easing", "EasingFunction?", "Linear" },
                    { "postscript", "String?", "" }
                },
                "",
                R"(Sets the property or properties identified by the URI to the specified
value. The `uri` identifies which property or properties are affected by this function
call and can include both wildcards `*` which match anything, as well as tags (`{tag}`)
which match scene graph nodes that have this tag. There is also the ability to combine two
tags through the `&`, `|`, and `~` operators. `{tag1&tag2}` will match anything that has
the tag1 and the tag2. `{tag1|tag2}` will match anything that has the tag1 or the tag 2,
and `{tag1~tag2}` will match anything that has tag1 but not tag2. If no wildcards or tags
are provided at most one property value will be changed. With wildcards or tags all
properties that match the URI are changed instead. The second argument's type must match
the type of the property or properties or an error is raised. If a duration is provided,
the requested change will occur over the provided number of seconds. If no duration is
provided or the duration is 0, the change occurs instantaneously.

For example `openspace.setPropertyValue("*Trail.Renderable.Enabled", true)` will enable
any property that ends with "Trail.Renderable.Enabled", for example
"StarTrail.Renderable.Enabled", "EarthTrail.Renderable.Enabled", but not
"EarthTrail.Renderable.Size".

`openspace.setPropertyValue("{tag1}.Renderable.Enabled", true)` will enable any node in
the scene that has the "tag1" assigned to it.

If you only want to change a single property value, also see the #setPropertyValueSingle
function as it will do so in a more efficient way. The `setPropertyValue` function will
work for individual property value, but is more computationally expensive.

\\param uri The URI that identifies the property or properties whose values should be
changed. The URI can contain 0 or 1 wildcard `*` characters or a tag expression (`{tag}`)
that identifies a property owner
\\param value The new value to which the property/properties identified by the `uri`
should be changed to. The type of this parameter must agree with the type of the selected
property
\\param duration The number of seconds over which the change will occur. If not provided
or the provided value is 0, the change is instantaneously.
\\param easing If a duration larger than 0 is provided, this parameter controls the manner
in which the parameter is interpolated. Has to be one of "Linear", "QuadraticEaseIn",
"QuadraticEaseOut", "QuadraticEaseInOut", "CubicEaseIn", "CubicEaseOut", "CubicEaseInOut",
"QuarticEaseIn", "QuarticEaseOut", "QuarticEaseInOut", "QuinticEaseIn", "QuinticEaseOut",
"QuinticEaseInOut", "SineEaseIn", "SineEaseOut", "SineEaseInOut", "CircularEaseIn",
"CircularEaseOut", "CircularEaseInOut", "ExponentialEaseIn", "ExponentialEaseOut",
"ExponentialEaseInOut", "ElasticEaseIn", "ElasticEaseOut", "ElasticEaseInOut",
"BounceEaseIn", "BounceEaseOut", "BounceEaseInOut"
\\param postscript A Lua script that will be executed once the change of property value
is completed. If a duration larger than 0 was provided, it is at the end of the
interpolation. If 0 was provided, the script runs immediately.
)",
                {
                    std::source_location::current().file_name(),
                    std::source_location::current().line()
                }
            },
            {
                "setPropertyValueSingle",
                &luascriptfunctions::propertySetValue<true>,
                {
                    { "uri", "String" },
                    { "value", "Nil | String | Number | Boolean | Table" },
                    { "duration", "Number?", "0.0" },
                    { "easing", "EasingFunction?", "Linear" },
                    { "postscript", "String?", "" }
                },
                "",
                R"(Sets the single property identified by the URI to the specified value.
The `uri` identifies which property is affected by this function call. The second
argument's type must match the type of the property or an error is raised. If a duration
is provided, the requested change will occur over the provided number of seconds. If no
duration is provided or the duration is 0, the change occurs instantaneously.

If you want to change multiple property values simultaneously, also see the
#setPropertyValue function. The `setPropertyValueSingle` function however will work more
efficiently for individual property values.

\\param uri The URI that identifies the property
\\param value The new value to which the property identified by the `uri` should be
changed to. The type of this parameter must agree with the type of the selected property
\\param duration The number of seconds over which the change will occur. If not provided
or the provided value is 0, the change is instantaneously.
\\param easing If a duration larger than 0 is provided, this parameter controls the manner
in which the parameter is interpolated. Has to be one of "Linear", "QuadraticEaseIn",
"QuadraticEaseOut", "QuadraticEaseInOut", "CubicEaseIn", "CubicEaseOut", "CubicEaseInOut",
"QuarticEaseIn", "QuarticEaseOut", "QuarticEaseInOut", "QuinticEaseIn", "QuinticEaseOut",
"QuinticEaseInOut", "SineEaseIn", "SineEaseOut", "SineEaseInOut", "CircularEaseIn",
"CircularEaseOut", "CircularEaseInOut", "ExponentialEaseIn", "ExponentialEaseOut",
"ExponentialEaseInOut", "ElasticEaseIn", "ElasticEaseOut", "ElasticEaseInOut",
"BounceEaseIn", "BounceEaseOut", "BounceEaseInOut"
\\param postscript This parameter specifies a Lua script that will be executed once the
change of property value is completed. If a duration larger than 0 was provided, it is
at the end of the interpolation. If 0 was provided, the script runs immediately.
)",
                {
                    std::source_location::current().file_name(),
                    std::source_location::current().line()
                }
            },
            {
                "propertyValue",
                &luascriptfunctions::propertyGetValue,
                {
                    { "uri", "String" }
                },
                "String | Number | Boolean | Table",
                "Returns the value of the property identified by the provided URI. This "
                "function will provide an error message if no property matching the URI "
                "is found.",
                {
                    std::source_location::current().file_name(),
                    std::source_location::current().line()
                }
            },
            codegen::lua::HasProperty,
            codegen::lua::Property,
            codegen::lua::PropertyOwner,
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
            codegen::lua::MakeIdentifier,
            codegen::lua::SetGuiOrder,
            codegen::lua::GuiOrder
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
