/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/scene/profile.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/easing.h>
#include <ghoul/lua/lua_helper.h>

namespace openspace {

namespace {

template <class T>
properties::PropertyOwner* findPropertyOwnerWithMatchingGroupTag(T* prop,
                                                            const std::string& tagToMatch)
{
    properties::PropertyOwner* tagMatchOwner = nullptr;
    properties::PropertyOwner* owner = prop->owner();

    if (owner) {
        const std::vector<std::string>& tags = owner->tags();
        for (const std::string& currTag : tags) {
            if (tagToMatch == currTag) {
                tagMatchOwner = owner;
                break;
            }
        }

        // Call recursively until we find an owner with matching tag or the top of the
        // ownership list
        if (tagMatchOwner == nullptr) {
            tagMatchOwner = findPropertyOwnerWithMatchingGroupTag(owner, tagToMatch);
        }
    }
    return tagMatchOwner;
}

void applyRegularExpression(lua_State* L, const std::string& regex,
                            const std::vector<properties::Property*>& properties,
                            double interpolationDuration, const std::string& groupName,
                            ghoul::EasingFunction easingFunction)
{
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    const bool isGroupMode = !groupName.empty();
    bool isLiteral = false;

    const int type = lua_type(L, -1);

    // Extract the property and node name to be searched for from regex
    std::string propertyName;
    std::string nodeName;
    size_t wildPos = regex.find_first_of("*");
    if (wildPos != std::string::npos) {
        nodeName = regex.substr(0, wildPos);
        propertyName = regex.substr(wildPos + 1, regex.length());

        // If none then malformed regular expression
        if (propertyName.empty() && nodeName.empty()) {
            LERRORC(
                "applyRegularExpression",
                fmt::format(
                    "Malformed regular expression: '{}': Empty both before and after '*'",
                    regex
                )
            );
            return;
        }

        // Currently do not support several wildcards
        if (regex.find_first_of("*", wildPos + 1) != std::string::npos) {
            LERRORC(
                "applyRegularExpression",
                fmt::format(
                    "Malformed regular expression: '{}': Currently only one '*' is "
                    "supported", regex
                )
            );
            return;
        }
    }
    // Literal or tag
    else {
        propertyName = regex;
        if (!isGroupMode) {
            isLiteral = true;
        }
    }

    // Stores whether we found at least one matching property. If this is false at the end
    // of the loop, the property name regex was probably misspelled.
    bool foundMatching = false;
    for (properties::Property* prop : properties) {
        // Check the regular expression for all properties
        const std::string id = prop->fullyQualifiedIdentifier();

        if (isLiteral && id != propertyName) {
            continue;
        }
        else if (!propertyName.empty()){
            size_t propertyPos = id.find(propertyName);
            if (propertyPos != std::string::npos) {
                // Check that the propertyName fully matches the property in id
                if ((propertyPos + propertyName.length() + 1) < id.length()) {
                    continue;
                }

                // Match node name
                if (!nodeName.empty() && id.find(nodeName) == std::string::npos) {
                    continue;
                }

                // Check tag
                if (isGroupMode) {
                    properties::PropertyOwner* matchingTaggedOwner =
                        findPropertyOwnerWithMatchingGroupTag(prop, groupName);
                    if (!matchingTaggedOwner) {
                        continue;
                    }
                }
            }
            else {
                continue;
            }
        }
        else if (!nodeName.empty()) {
            size_t nodePos = id.find(nodeName);
            if (nodePos != std::string::npos) {
                // Check tag
                if (isGroupMode) {
                    properties::PropertyOwner* matchingTaggedOwner =
                        findPropertyOwnerWithMatchingGroupTag(prop, groupName);
                    if (!matchingTaggedOwner) {
                        continue;
                    }
                }
                // Check that the nodeName fully matches the node in id
                else if (nodePos != 0) {
                    continue;
                }
            }
            else {
                continue;
            }
        }

        // Check that the types match
        if (type != prop->typeLua()) {
            LERRORC(
                "property_setValue",
                fmt::format(
                    "{}: Property '{}' does not accept input of type '{}'. Requested "
                    "type: '{}'",
                    errorLocation(L), prop->fullyQualifiedIdentifier(),
                    luaTypeToString(type), luaTypeToString(prop->typeLua())
                )
            );
        }
        else {
            // If the fully qualified id matches the regular expression, we queue the
            // value change if the types agree
            foundMatching = true;

            if (global::sessionRecording->isRecording()) {
                global::sessionRecording->savePropertyBaseline(*prop);
            }
            if (interpolationDuration == 0.0) {
                global::renderEngine->scene()->removePropertyInterpolation(prop);
                prop->setLuaValue(L);
            }
            else {
                prop->setLuaInterpolationTarget(L);
                global::renderEngine->scene()->addPropertyInterpolation(
                    prop,
                    static_cast<float>(interpolationDuration),
                    easingFunction
                );
            }
        }
    }

    if (!foundMatching) {
        LERRORC(
            "property_setValue",
            fmt::format(
                "{}: No property matched the requested URI '{}'", errorLocation(L), regex
            )
        );
    }
}

// Checks to see if URI contains a group tag (with { } around the first term). If so,
// returns true and sets groupName with the tag
bool doesUriContainGroupTag(const std::string& command, std::string& groupName) {
    const std::string name = command.substr(0, command.find_first_of("."));
    if (name.front() == '{' && name.back() == '}') {
        groupName = name.substr(1, name.length() - 2);
        return true;
    }
    else {
        return false;
    }
}

std::string removeGroupNameFromUri(const std::string& uri) {
    return uri.substr(uri.find_first_of("."));
}

} // namespace
} // namespace openspace

namespace openspace::luascriptfunctions {

int setPropertyCall_single(properties::Property& prop, const std::string& uri,
                           lua_State* L, double duration,
                           ghoul::EasingFunction easingFunction)
{
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    const int type = lua_type(L, -1);
    if (type != prop.typeLua()) {
        LERRORC(
            "property_setValue",
            fmt::format(
                "{}: Property '{}' does not accept input of type '{}'. "
                "Requested type: '{}'",
                errorLocation(L), uri, luaTypeToString(type),
                luaTypeToString(prop.typeLua())
            )
        );
    }
    else {
        if (global::sessionRecording->isRecording()) {
            global::sessionRecording->savePropertyBaseline(prop);
        }
        if (duration == 0.0) {
            global::renderEngine->scene()->removePropertyInterpolation(&prop);
            prop.setLuaValue(L);
        }
        else {
            prop.setLuaInterpolationTarget(L);
            global::renderEngine->scene()->addPropertyInterpolation(
                &prop,
                static_cast<float>(duration),
                easingFunction
            );
        }
    }
    return 0;
}

/**
 * \ingroup LuaScripts
 * setPropertyValue(string, *):
 * Sets all property(s) identified by the URI (with potential wildcards) in the first
 * argument. The second argument can be any type, but it has to match the type that the
 * property (or properties) expect.
 * If the first term (separated by '.') in the uri is bracketed with { }, then this
 * term is treated as a group tag name, and the function will search through all
 * property owners to find those that are tagged with this group name, and set their
 * property values accordingly.
 */

int property_setValue(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 2, 5 }, "lua::property_setValue");
    defer { lua_settop(L, 0); };

    std::string uriOrRegex =
        ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::No);
    std::string optimization;
    double interpolationDuration = 0.0;
    std::string easingMethodName;
    ghoul::EasingFunction easingMethod = ghoul::EasingFunction::Linear;

    if (lua_gettop(L) >= 3) {
        if (ghoul::lua::hasValue<double>(L, 3)) {
            interpolationDuration =
                ghoul::lua::value<double>(L, 3, ghoul::lua::PopValue::No);
        }
        else {
            optimization =
                ghoul::lua::value<std::string>(L, 3, ghoul::lua::PopValue::No);
        }

        if (lua_gettop(L) >= 4) {
            if (ghoul::lua::hasValue<double>(L, 4)) {
                interpolationDuration =
                    ghoul::lua::value<double>(L, 4, ghoul::lua::PopValue::No);
            }
            else {
                easingMethodName =
                    ghoul::lua::value<std::string>(L, 4, ghoul::lua::PopValue::No);
            }
        }

        if (lua_gettop(L) == 5) {
            optimization = ghoul::lua::value<std::string>(L, 5, ghoul::lua::PopValue::No);
        }

        // Later functions expect the value to be at the last position on the stack
        lua_pushvalue(L, 2);
    }

    if ((interpolationDuration == 0.0) && !easingMethodName.empty()) {
        LWARNINGC(
            "property_setValue",
            "Easing method specified while interpolation duration is equal to 0"
        );
    }

    if (!easingMethodName.empty()) {
        bool correctName = ghoul::isValidEasingFunctionName(easingMethodName);
        if (!correctName) {
            LWARNINGC(
                "property_setValue",
                fmt::format("{} is not a valid easing method", easingMethodName)
            );
        }
        else {
            easingMethod = ghoul::easingFunctionFromName(easingMethodName);
        }
    }

    if (optimization.empty()) {
        std::string groupName;
        if (doesUriContainGroupTag(uriOrRegex, groupName)) {
            // Remove group name from start of regex and replace with '*'
            uriOrRegex = removeGroupNameFromUri(uriOrRegex);
        }

        applyRegularExpression(
            L,
            uriOrRegex,
            allProperties(),
            interpolationDuration,
            groupName,
            easingMethod
        );
        return 0;
    }
    else if (optimization == "regex") {
        applyRegularExpression(
            L,
            uriOrRegex,
            allProperties(),
            interpolationDuration,
            "",
            easingMethod
        );
    }
    else if (optimization == "single") {
        properties::Property* prop = property(uriOrRegex);
        if (!prop) {
            LERRORC(
                "property_setValue",
                fmt::format(
                    "{}: Property with URI '{}' was not found",
                    ghoul::lua::errorLocation(L), uriOrRegex
                )
            );
            return 0;
        }
        return setPropertyCall_single(
            *prop,
            uriOrRegex,
            L,
            interpolationDuration,
            easingMethod
        );
    }
    else {
        LERRORC(
            "lua::property_setGroup",
            fmt::format(
                "{}: Unexpected optimization '{}'",
                ghoul::lua::errorLocation(L), optimization
            )
        );
    }
    return 0;
}

int property_setValueSingle(lua_State* L) {
    const int n = lua_gettop(L);
    if (n == 3) {
        // If we pass three arguments, the third one is the interpolation factor and the
        // user did not specify an easing factor, so we have to add that manually before
        // adding the single optimization
        ghoul::lua::push(L, ghoul::nameForEasingFunction(ghoul::EasingFunction::Linear));
    }

    ghoul::lua::push(L, "single");
    return property_setValue(L);
}

/**
 * \ingroup LuaScripts
 * hasProperty(string):
 * Returns whether a property with the given URI exists
 */
int property_hasProperty(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::property_hasProperty");
    const std::string uri = ghoul::lua::value<std::string>(L);

    properties::Property* prop = property(uri);
    ghoul::lua::push(L, prop != nullptr);
    return 1;
}

/**
 * \ingroup LuaScripts
 * getPropertyValue(string):
 * Returns the value of the property identified by the passed URI as a Lua object that can
 * be passed to the setPropertyValue method.
 */
int property_getValue(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::property_getValue");
    const std::string uri = ghoul::lua::value<std::string>(L);

    properties::Property* prop = property(uri);
    if (!prop) {
        LERRORC(
            "property_getValue",
            fmt::format(
                "{}: Property with URI '{}' was not found",
                ghoul::lua::errorLocation(L), uri
            )
        );
        return 0;
    }
    else {
        prop->getLuaValue(L);
    }
    return 1;
}

/**
 * \ingroup LuaScripts
 * getProperty
 * Returns a list of property identifiers that match the passed regular expression
 */
int property_getProperty(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::property_getProperty");
    std::string regex = ghoul::lua::value<std::string>(L);

    std::string groupName;
    if (doesUriContainGroupTag(regex, groupName)) {
        // Remove group name from start of regex and replace with '*'
        regex = removeGroupNameFromUri(regex);
    }

    // Extract the property and node name to be searched for from regex
    bool isLiteral = false;
    std::string propertyName;
    std::string nodeName;
    size_t wildPos = regex.find_first_of("*");
    if (wildPos != std::string::npos) {
        nodeName = regex.substr(0, wildPos);
        propertyName = regex.substr(wildPos + 1, regex.length());

        // If none then malformed regular expression
        if (propertyName.empty() && nodeName.empty()) {
            LERRORC(
                "property_getProperty",
                fmt::format(
                    "Malformed regular expression: '{}': Empty both before and after '*'",
                    regex
                )
            );
            return 0;
        }

        // Currently do not support several wildcards
        if (regex.find_first_of("*", wildPos + 1) != std::string::npos) {
            LERRORC(
                "property_getProperty",
                fmt::format(
                    "Malformed regular expression: '{}': "
                    "Currently only one '*' is supported", regex
                )
            );
            return 0;
        }
    }
    // Literal or tag
    else {
        propertyName = regex;
        if (groupName.empty()) {
            isLiteral = true;
        }
    }

    // Get all matching property uris and save to res
    std::vector<properties::Property*> props = allProperties();
    std::vector<std::string> res;
    for (properties::Property* prop : props) {
        // Check the regular expression for all properties
        const std::string& id = prop->fullyQualifiedIdentifier();

        if (isLiteral && id != propertyName) {
            continue;
        }
        else if (!propertyName.empty()) {
            size_t propertyPos = id.find(propertyName);
            if (propertyPos != std::string::npos) {
                // Check that the propertyName fully matches the property in id
                if ((propertyPos + propertyName.length() + 1) < id.length()) {
                    continue;
                }

                // Match node name
                if (!nodeName.empty() && id.find(nodeName) == std::string::npos) {
                    continue;
                }

                // Check tag
                if (!groupName.empty()) {
                    properties::PropertyOwner* matchingTaggedOwner =
                        findPropertyOwnerWithMatchingGroupTag(prop, groupName);
                    if (!matchingTaggedOwner) {
                        continue;
                    }
                }
            }
            else {
                continue;
            }
        }
        else if (!nodeName.empty()) {
            size_t nodePos = id.find(nodeName);
            if (nodePos != std::string::npos) {

                // Check tag
                if (!groupName.empty()) {
                    properties::PropertyOwner* matchingTaggedOwner =
                        findPropertyOwnerWithMatchingGroupTag(prop, groupName);
                    if (!matchingTaggedOwner) {
                        continue;
                    }
                }
                // Check that the nodeName fully matches the node in id
                else if (nodePos != 0) {
                    continue;
                }
            }
            else {
                continue;
            }
        }

        res.push_back(id);
    }

    lua_newtable(L);
    int number = 1;
    for (const std::string& s : res) {
        ghoul::lua::push(L, s);
        lua_rawseti(L, -2, number);
        ++number;
    }
    return 1;
}

int loadScene(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadScene");
    std::string sceneFile = ghoul::lua::value<std::string>(L);

    global::openSpaceEngine->scheduleLoadSingleAsset(std::move(sceneFile));
    return 0;
}

int addSceneGraphNode(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::addSceneGraphNode");
    const ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

    try {
        SceneGraphNode* node = global::renderEngine->scene()->loadNode(d);
        if (!node) {
            LERRORC("Scene", "Could not load scene graph node");
            return ghoul::lua::luaError(L, "Error loading scene graph node");
        }

        global::renderEngine->scene()->initializeNode(node);
    }
    catch (const documentation::SpecificationError& e) {
        std::string cat =
            d.hasValue<std::string>("Identifier") ?
            d.value<std::string>("Identifier") :
            "Scene";
        LERRORC(cat, ghoul::to_string(e.result));

        return ghoul::lua::luaError(
            L,
            fmt::format("Error loading scene graph node: {}", e.what())
        );
    }
    catch (const ghoul::RuntimeError& e) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Error loading scene graph node: {}", e.what())
        );
    }
    return 0;
}

int removeSceneGraphNode(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeSceneGraphNode");
    const std::string name = ghoul::lua::value<std::string>(L);

    SceneGraphNode* foundNode = sceneGraphNode(name);
    if (!foundNode) {
        LERRORC(
            "removeSceneGraphNode",
            fmt::format("Did not find a match for identifier: {} ", name)
        );
        return 0;
    }

    SceneGraphNode* parent = foundNode->parent();
    if (!parent) {
        LERRORC(
            "removeSceneGraphNode",
            fmt::format("Cannot remove root node")
        );
        return 0;
    }

    // Remove the node and all its children
    std::function<void(SceneGraphNode*)> removeNode =
        [&removeNode](SceneGraphNode* localNode) {

        if (localNode == global::navigationHandler->anchorNode()) {
            global::navigationHandler->setFocusNode(sceneGraph()->root());
        }

        if (localNode == global::navigationHandler->orbitalNavigator().aimNode()) {
            global::navigationHandler->orbitalNavigator().setAimNode("");
        }

        std::vector<SceneGraphNode*> children = localNode->children();

        ghoul::mm_unique_ptr<SceneGraphNode> n = localNode->parent()->detachChild(
            *localNode
        );
        ghoul_assert(n.get() == localNode, "Wrong node returned from detaching");

        for (SceneGraphNode* c : children) {
            removeNode(c);
        }

        localNode->deinitializeGL();
        localNode->deinitialize();
        n = nullptr;
    };

    removeNode(foundNode);
    return 0;
}

int removeSceneGraphNodesFromRegex(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeSceneGraphNodesFromRegex");
    const std::string name = ghoul::lua::value<std::string>(L);

    const std::vector<SceneGraphNode*>& nodes =
        global::renderEngine->scene()->allSceneGraphNodes();

    // Extract the property and node name to be searched for from name
    bool isLiteral = false;
    std::string propertyName;
    std::string nodeName;
    size_t wildPos = name.find_first_of("*");
    if (wildPos != std::string::npos) {
        nodeName = name.substr(0, wildPos);
        propertyName = name.substr(wildPos + 1, name.length());

        // If none then malformed regular expression
        if (propertyName.empty() && nodeName.empty()) {
            LERRORC(
                "removeSceneGraphNodesFromRegex",
                fmt::format(
                    "Malformed regular expression: '{}': Empty both before and after '*'",
                    name
                )
            );
            return 0;
        }

        // Currently do not support several wildcards
        if (name.find_first_of("*", wildPos + 1) != std::string::npos) {
            LERRORC(
                "removeSceneGraphNodesFromRegex",
                fmt::format(
                    "Malformed regular expression: '{}': "
                    "Currently only one '*' is supported", name
                )
            );
            return 0;
        }
    }
    // Literal or tag
    else {
        propertyName = name;
        isLiteral = true;
    }

    bool foundMatch = false;
    std::vector<SceneGraphNode*> markedList;
    for (SceneGraphNode* node : nodes) {
        const std::string& identifier = node->identifier();

        if (isLiteral && identifier != propertyName) {
            continue;
        }
        else if (!propertyName.empty()) {
            size_t propertyPos = identifier.find(propertyName);
            if (propertyPos != std::string::npos) {
                // Check that the propertyName fully matches the property in id
                if ((propertyPos + propertyName.length() + 1) < identifier.length()) {
                    continue;
                }

                // Match node name
                if (!nodeName.empty() && identifier.find(nodeName) == std::string::npos) {
                    continue;
                }
            }
            else {
                continue;
            }
        }
        else if (!nodeName.empty()) {
            size_t nodePos = identifier.find(nodeName);
            if (nodePos != std::string::npos) {
                // Check that the nodeName fully matches the node in id
                if (nodePos != 0) {
                    continue;
                }
            }
            else {
                continue;
            }
        }

        foundMatch = true;
        SceneGraphNode* parent = node->parent();
        if (!parent) {
            LERRORC(
                "removeSceneGraphNodesFromRegex", fmt::format("Cannot remove root node")
            );
        }
        else {
            markedList.push_back(node);
        }
    }

    if (!foundMatch) {
        LERRORC(
            "removeSceneGraphNodesFromRegex",
            fmt::format("Did not find a match for identifier: {}", name)
        );
        return 0;
    }

    // Add all the children
    std::function<void(SceneGraphNode*, std::vector<SceneGraphNode*>&)> markNode =
        [&markNode](SceneGraphNode* node, std::vector<SceneGraphNode*>& marked)
    {
        for (SceneGraphNode* child : node->children()) {
            markNode(child, marked);
        }

        const auto it = std::find(marked.cbegin(), marked.cend(), node);
        if (it == marked.end()) {
            marked.push_back(node);
        }
    };
    for (SceneGraphNode* node : markedList) {
        markNode(node, markedList);
    }

    // Remove all marked nodes
    std::function<void(SceneGraphNode*)> removeNode =
        [&removeNode, &markedList](SceneGraphNode* localNode) {

        if (localNode == global::navigationHandler->anchorNode()) {
            global::navigationHandler->setFocusNode(sceneGraph()->root());
        }

        if (localNode == global::navigationHandler->orbitalNavigator().aimNode()) {
            global::navigationHandler->orbitalNavigator().setAimNode("");
        }

        std::vector<SceneGraphNode*> children = localNode->children();

        ghoul::mm_unique_ptr<SceneGraphNode> n = localNode->parent()->detachChild(
            *localNode
        );
        ghoul_assert(n.get() == localNode, "Wrong node returned from detaching");

        for (SceneGraphNode* c : children) {
            removeNode(c);
        }

        markedList.erase(
            std::remove(markedList.begin(), markedList.end(), localNode),
            markedList.end()
        );

        localNode->deinitializeGL();
        localNode->deinitialize();
        n = nullptr;
    };

    while (!markedList.empty()) {
        removeNode(markedList[0]);
    }

    return 0;
}

int hasSceneGraphNode(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::hasSceneGraphNode");
    const std::string nodeName = ghoul::lua::value<std::string>(L);

    SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(nodeName);
    ghoul::lua::push(L, node != nullptr);
    return 1;
}

int addInterestingTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addInterestingTime");
    auto [name, time] = ghoul::lua::values<std::string, std::string>(L);

    global::renderEngine->scene()->addInterestingTime(
        { std::move(name), std::move(time) }
    );
    return 0;
}

int worldPosition(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::worldPosition");
    const std::string identifier = ghoul::lua::value<std::string>(L);

    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Did not find a match for identifier: {} ", identifier)
        );
    }

    glm::dvec3 pos = node->worldPosition();
    ghoul::lua::push(L, std::move(pos));
    return 1;
}

int worldRotation(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::worldRotation");
    std::string identifier = ghoul::lua::value<std::string>(L);

    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Did not find a match for identifier: {} ", identifier)
        );
    }

    glm::dmat3 rot = node->worldRotationMatrix();
    ghoul::lua::push(L, std::move(rot));
    return 1;
}

int setParent(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::setParent");
    auto [identifier, newParent] = ghoul::lua::values<std::string, std::string>(L);

    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Did not find a match for identifier: {} ", identifier)
        );
    }
    SceneGraphNode* newParentNode = sceneGraphNode(newParent);
    if (!newParentNode) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Did not find a match for new parent identifier: {} ", newParent)
        );
    }

    node->setParent(*newParentNode);
    global::renderEngine->scene()->markNodeRegistryDirty();

    return 0;
}

/**
 * \ingroup LuaScripts
 * isBoolValue(const std::string& s):
 * Used to check if a string is a lua bool type. Returns false if not a valid bool string.
 */
bool isBoolValue(const std::string& s) {
    return (s == "true" || s == "false");
}

/**
 * \ingroup LuaScripts
 * isFloatValue(const std::string& s):
 * Used to check if a string is a lua float value. Returns false if not a valid float.
 */
bool isFloatValue(const std::string& s) {
    try {
        float converted = std::stof(s);
        return true;
    }
    catch (...) {
        return false;
    }
}

}  // namespace openspace::luascriptfunctions
