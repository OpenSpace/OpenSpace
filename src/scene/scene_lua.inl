/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/matrix/dmat2property.h>
#include <openspace/properties/matrix/dmat3property.h>
#include <openspace/properties/matrix/dmat4property.h>
#include <openspace/properties/matrix/mat2property.h>
#include <openspace/properties/matrix/mat3property.h>
#include <openspace/properties/matrix/mat4property.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/longproperty.h>
#include <openspace/properties/scalar/shortproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/properties/scalar/ulongproperty.h>
#include <openspace/properties/scalar/ushortproperty.h>
#include <openspace/properties/vector/dvec2property.h>
#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/vector/dvec4property.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/ivec3property.h>
#include <openspace/properties/vector/ivec4property.h>
#include <openspace/properties/vector/uvec2property.h>
#include <openspace/properties/vector/uvec3property.h>
#include <openspace/properties/vector/uvec4property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>

namespace {

template <class T>
openspace::properties::PropertyOwner* findPropertyOwnerWithMatchingGroupTag(T* prop,
                                                            const std::string& tagToMatch)
{
    using namespace openspace;

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

std::vector<openspace::properties::Property*> findMatchesInAllProperties(
                                                                const std::string& regex,
                         const std::vector<openspace::properties::Property*>& properties,
                                                            const std::string& groupName)
{
    using namespace openspace;

    std::vector<properties::Property*> matches;
    const bool isGroupMode = !groupName.empty();
    bool isLiteral = false;

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
                "findMatchesInAllProperties",
                fmt::format(
                    "Malformed regular expression: '{}': Empty both before and after '*'",
                    regex
                )
            );
            return matches;
        }

        // Currently do not support several wildcards
        if (regex.find_first_of("*", wildPos + 1) != std::string::npos) {
            LERRORC(
                "findMatchesInAllProperties",
                fmt::format(
                    "Malformed regular expression: '{}': Currently only one '*' is "
                    "supported", regex
                )
            );
            return matches;
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
    for (properties::Property* prop : properties) {
        // Check the regular expression for all properties
        const std::string id = prop->fullyQualifiedIdentifier();

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
        matches.push_back(prop);
    }
    return matches;
}

void applyRegularExpression(lua_State* L, const std::string& regex,
                          const std::vector<openspace::properties::Property*>& properties,
                                                             double interpolationDuration,
                                                             const std::string& groupName,
                                                     ghoul::EasingFunction easingFunction)
{
    using namespace openspace;
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    const int type = lua_type(L, -1);

    std::vector<properties::Property*> matchingProps = findMatchesInAllProperties(
        regex,
        properties,
        groupName
    );

    // Stores whether we found at least one matching property. If this is false at the
    // end of the loop, the property name regex was probably misspelled.
    bool foundMatching = false;
    for (properties::Property* prop : matchingProps) {

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

namespace openspace::luascriptfunctions {

int setPropertyCallSingle(properties::Property& prop, const std::string& uri,
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

int propertySetValue(lua_State* L) {
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
            optimization = ghoul::lua::value<std::string>(L, 3, ghoul::lua::PopValue::No);
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
                "propertySetValue",
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
        return setPropertyCallSingle(
            *prop,
            uriOrRegex,
            L,
            interpolationDuration,
            easingMethod
        );
    }
    else {
        LERRORC(
            "lua::propertySetValue",
            fmt::format(
                "{}: Unexpected optimization '{}'",
                ghoul::lua::errorLocation(L), optimization
            )
        );
    }
    return 0;
}

int propertySetValueSingle(lua_State* L) {
    const int n = lua_gettop(L);
    if (n == 3) {
        // If we pass three arguments, the third one is the interpolation factor and the
        // user did not specify an easing factor, so we have to add that manually before
        // adding the single optimization
        ghoul::lua::push(L, ghoul::nameForEasingFunction(ghoul::EasingFunction::Linear));
    }

    ghoul::lua::push(L, "single");
    return propertySetValue(L);
}

int propertyGetValue(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::propertyGetValue");
    const std::string uri = ghoul::lua::value<std::string>(L);

    properties::Property* prop = property(uri);
    if (!prop) {
        LERRORC(
            "propertyGetValue",
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

}  // namespace openspace::luascriptfunctions

namespace {

/**
 * Returns whether a property with the given URI exists
 */
[[codegen::luawrap]] bool hasProperty(std::string uri) {
    openspace::properties::Property* prop = openspace::property(uri);
    return prop != nullptr;
}

/**
 * Returns a list of property identifiers that match the passed regular expression
 */
[[codegen::luawrap]] std::vector<std::string> getProperty(std::string regex) {
    using namespace openspace;

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
            throw ghoul::lua::LuaError(fmt::format(
                "Malformed regular expression: '{}': Empty both before and after '*'",
                regex
            ));
        }

        // Currently do not support several wildcards
        if (regex.find_first_of("*", wildPos + 1) != std::string::npos) {
            throw ghoul::lua::LuaError(fmt::format(
                "Malformed regular expression: '{}': Currently only one '*' is supported",
                regex
            ));
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

    return res;
}

/**
 * Loads the SceneGraphNode described in the table and adds it to the SceneGraph.
 */
[[codegen::luawrap]] void addSceneGraphNode(ghoul::Dictionary node) {
    using namespace openspace;
    try {
        SceneGraphNode* n = global::renderEngine->scene()->loadNode(node);
        if (!n) {
            LERRORC("Scene", "Could not load scene graph node");
            throw ghoul::lua::LuaError("Error loading scene graph node");
        }

        global::renderEngine->scene()->initializeNode(n);
    }
    catch (const documentation::SpecificationError& e) {
        std::string cat =
            node.hasValue<std::string>("Identifier") ?
            node.value<std::string>("Identifier") :
            "Scene";
        LERRORC(cat, ghoul::to_string(e.result));

        throw ghoul::lua::LuaError(
            fmt::format("Error loading scene graph node: {}", e.what())
        );
    }
    catch (const ghoul::RuntimeError& e) {
        throw ghoul::lua::LuaError(
            fmt::format("Error loading scene graph node: {}", e.what())
        );
    }
}

/**
 * Removes the SceneGraphNode identified by name or by extracting the 'Identifier' key if
 * the parameter is a table.
 */
[[codegen::luawrap]] void removeSceneGraphNode(
    std::variant<std::string, ghoul::Dictionary> node)
{
    using namespace openspace;
    std::string identifier;
    if (std::holds_alternative<std::string>(node)) {
        identifier = std::get<std::string>(node);
    }
    else {
        ghoul::Dictionary d = std::get<ghoul::Dictionary>(node);
        if (!d.hasValue<std::string>("Identifier")) {
            throw ghoul::lua::LuaError(
                "Table passed to removeSceneGraphNode does not contain an Identifier"
            );
        }
        identifier = d.value<std::string>("Identifier");
    }

    if (identifier == "Root") {
        throw ghoul::lua::LuaError("Cannot remove the 'Root' scene graph node");
    }

    SceneGraphNode* foundNode = sceneGraphNode(identifier);
    if (!foundNode) {
        throw ghoul::lua::LuaError(
            fmt::format("Did not find a match for identifier: {} ", identifier)
        );
    }

    SceneGraphNode* parent = foundNode->parent();
    if (!parent) {
        throw ghoul::lua::LuaError("Cannot remove root node");
    }

    // Remove the node and all its children
    std::function<void(SceneGraphNode*)> removeNode =
        [&removeNode](SceneGraphNode* localNode) {
        if (localNode == global::navigationHandler->anchorNode()) {
            global::navigationHandler->orbitalNavigator().setFocusNode(
                sceneGraph()->root()
            );
            global::navigationHandler->orbitalNavigator().startRetargetAnchor();
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
}

/**
 * Removes all SceneGraphNodes with identifiers matching the input regular expression.
 */
[[codegen::luawrap]] void removeSceneGraphNodesFromRegex(std::string name) {
    using namespace openspace;
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
            throw ghoul::lua::LuaError(
                fmt::format(
                    "Malformed regular expression: '{}': Empty both before and after '*'",
                    name
                )
            );
        }

        // Currently do not support several wildcards
        if (name.find_first_of("*", wildPos + 1) != std::string::npos) {
            throw ghoul::lua::LuaError(
                fmt::format(
                    "Malformed regular expression: '{}': "
                    "Currently only one '*' is supported",
                    name
                )
            );
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
            throw ghoul::lua::LuaError("Cannot remove root node");
        }
        else {
            markedList.push_back(node);
        }
    }

    if (!foundMatch) {
        throw ghoul::lua::LuaError(
            fmt::format("Did not find a match for identifier: {}", name)
        );
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
}

// Checks whether the specifies SceneGraphNode is present in the current scene.
[[codegen::luawrap]] bool hasSceneGraphNode(std::string nodeName) {
    using namespace openspace;
    SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(nodeName);
    return node != nullptr;
}

/**
 * Adds an interesting time to the current scene. The first argument is the name of the
 * time and the second argument is the time itself in the format YYYY-MM-DDThh:mm:ss.uuu
 */
[[codegen::luawrap]] void addInterestingTime(std::string name, std::string time) {
    openspace::global::renderEngine->scene()->addInterestingTime(
        { std::move(name), std::move(time) }
    );
}

/**
 * Returns the world position of the scene graph node with the given string as identifier.
 */
[[codegen::luawrap]] glm::dvec3 worldPosition(std::string identifier) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(
            fmt::format("Did not find a match for identifier: {} ", identifier)
        );
    }

    glm::dvec3 pos = node->worldPosition();
    return pos;
}

/**
 * Returns the world rotation matrix of the scene graph node with the given string as
 * identifier.
 */
[[codegen::luawrap]] glm::dmat3 worldRotation(std::string identifier) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(
            fmt::format("Did not find a match for identifier: {} ", identifier)
        );
    }

    glm::dmat3 rot = node->worldRotationMatrix();
    return rot;
}

/**
 * The scene graph node identified by the first string is reparented to be a child of the
 * scene graph node identified by the second string.
 */
[[codegen::luawrap]] void setParent(std::string identifier, std::string newParent) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(fmt::format(
            "Did not find a match for identifier: {} ", identifier
        ));
    }
    SceneGraphNode* newParentNode = sceneGraphNode(newParent);
    if (!newParentNode) {
        throw ghoul::lua::LuaError(fmt::format(
            "Did not find a match for new parent identifier: {} ", newParent
        ));
    }

    node->setParent(*newParentNode);
    global::renderEngine->scene()->markNodeRegistryDirty();
}

/**
 * Returns the bounding sphere of the scene graph node with the given string as
 * identifier.
 */
[[codegen::luawrap]] double boundingSphere(std::string identifier) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(fmt::format(
            "Did not find a match for identifier: {} ", identifier
        ));
    }

    double bs = node->boundingSphere();
    return bs;
}

/**
 * Returns the interaction sphere of the scene graph node with the given string as
 * identifier.
 */
[[codegen::luawrap]] double interactionSphere(std::string identifier) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(fmt::format(
            "Did not find a match for identifier: {} ", identifier
        ));
    }

    double is = node->interactionSphere();
    return is;
}

template <typename T>
void createCustomProperty(openspace::properties::Property::PropertyInfo info,
                          std::optional<std::string> onChange)
{
    T* p = new T(info);
    if (onChange.has_value() && !onChange->empty()) {
        p->onChange(
            [p, script = *onChange]() {
                using namespace ghoul::lua;
                LuaState s;
                openspace::global::scriptEngine->initializeLuaState(s);
                ghoul::lua::push(s, p->value());
                lua_setglobal(s, "value");
                ghoul::lua::runScript(s, script);
            }
        );
    }
    openspace::global::userPropertyOwner->addProperty(p);
}

/**
 * Creates a new property that lives in the `UserProperty` group.
 * 
 * \param identifier The identifier that is going to be used for the new property
 * \param type The type of the property, has to be one of "DMat2Property",
 *        "DMat3Property", "DMat4Property", "Mat2Property", "Mat3Property",
 *        "Mat4Property", "BoolProperty", "DoubleProperty", "FloatProperty",
 *        "IntProperty", "StringProperty", "StringListProperty", "LongProperty",
 *        "ShortProperty", "UIntProperty", "ULongProperty", "DVec2Property",
 *        "DVec3Property", "DVec4Property", "IVec2Property", "IVec3Property",
 *        "IVec4Property", "UVec2Property", "UVec3Property", "UVec4Property",
 *        "Vec2Property", "Vec3Property", "Vec4Property"
 * \param guiName The name that the property uses in the user interface. If this value is
 *        not provided, the `identifier` is used instead
 * \param description A description what the property is used for
 * \param onChange A Lua script that will be executed whenever the property changes
 */
[[codegen::luawrap]] void addCustomProperty(std::string identifier, std::string type,
                                            std::optional<std::string> guiName,
                                            std::optional<std::string> description,
                                            std::optional<std::string> onChange)
{
    using namespace openspace;

    if (identifier.empty()) {
        throw ghoul::lua::LuaError("Identifier must not empty");
    }

    if (global::userPropertyOwner->hasProperty(identifier)) {
        throw ghoul::lua::LuaError(fmt::format(
            "Failed to register property '{}' since a user-defined property with that "
            "name already exists",
            identifier
        ));
    }

    // @TODO (abock, 2022-05-01)  These if statements here are a bit gnarly since it
    // requires us to update them as soon as we add a new property type. It would be nicer
    // to have a factory function for this but right now this is the only place where that
    // factory would be used.

    const char* gui =
        guiName.has_value() && !guiName->empty() ?
        guiName->c_str() :
        identifier.c_str();

    properties::Property::PropertyInfo info = {
        identifier.c_str(),
        gui,
        description.has_value() ? description->c_str() : ""
    };
    if (type == "DMat2Property") {
        createCustomProperty<properties::DMat2Property>(info, std::move(onChange));
    }
    else if (type == "DMat3Property") {
        createCustomProperty<properties::DMat3Property>(info, std::move(onChange));
    }
    else if (type == "DMat4Property") {
        createCustomProperty<properties::DMat4Property>(info, std::move(onChange));
    }
    else if (type == "Mat2Property") {
        createCustomProperty<properties::Mat2Property>(info, std::move(onChange));
    }
    else if (type == "Mat3Property") {
        createCustomProperty<properties::Mat3Property>(info, std::move(onChange));
    }
    else if (type == "Mat4Property") {
        createCustomProperty<properties::Mat4Property>(info, std::move(onChange));
    }
    else if (type == "BoolProperty") {
        createCustomProperty<properties::BoolProperty>(info, std::move(onChange));
    }
    else if (type == "DoubleProperty") {
        createCustomProperty<properties::DoubleProperty>(info, std::move(onChange));
    }
    else if (type == "FloatProperty") {
        createCustomProperty<properties::FloatProperty>(info, std::move(onChange));
    }
    else if (type == "IntProperty") {
        createCustomProperty<properties::IntProperty>(info, std::move(onChange));
    }
    else if (type == "StringProperty") {
        createCustomProperty<properties::StringProperty>(info, std::move(onChange));
    }
    else if (type == "StringListProperty") {
        createCustomProperty<properties::StringListProperty>(info, std::move(onChange));
    }
    else if (type == "LongProperty") {
        createCustomProperty<properties::LongProperty>(info, std::move(onChange));
    }
    else if (type == "ShortProperty") {
        createCustomProperty<properties::ShortProperty>(info, std::move(onChange));
    }
    else if (type == "UIntProperty") {
        createCustomProperty<properties::UIntProperty>(info, std::move(onChange));
    }
    else if (type == "ULongProperty") {
        createCustomProperty<properties::ULongProperty>(info, std::move(onChange));
    }
    else if (type == "UShortProperty") {
        createCustomProperty<properties::UShortProperty>(info, std::move(onChange));
    }
    else if (type == "DVec2Property") {
        createCustomProperty<properties::DVec2Property>(info, std::move(onChange));
    }
    else if (type == "DVec3Property") {
        createCustomProperty<properties::DVec3Property>(info, std::move(onChange));
    }
    else if (type == "DVec4Property") {
        createCustomProperty<properties::DVec4Property>(info, std::move(onChange));
    }
    else if (type == "IVec2Property") {
        createCustomProperty<properties::IVec2Property>(info, std::move(onChange));
    }
    else if (type == "IVec3Property") {
        createCustomProperty<properties::IVec3Property>(info, std::move(onChange));
    }
    else if (type == "IVec4Property") {
        createCustomProperty<properties::IVec4Property>(info, std::move(onChange));
    }
    else if (type == "UVec2Property") {
        createCustomProperty<properties::UVec2Property>(info, std::move(onChange));
    }
    else if (type == "UVec3Property") {
        createCustomProperty<properties::UVec3Property>(info, std::move(onChange));
    }
    else if (type == "UVec4Property") {
        createCustomProperty<properties::UVec4Property>(info, std::move(onChange));
    }
    else if (type == "Vec2Property") {
        createCustomProperty<properties::Vec2Property>(info, std::move(onChange));
    }
    else if (type == "Vec3Property") {
        createCustomProperty<properties::Vec3Property>(info, std::move(onChange));
    }
    else if (type == "Vec4Property") {
        createCustomProperty<properties::Vec4Property>(info, std::move(onChange));
    }
    else {
        throw ghoul::lua::LuaError(fmt::format("Unsupported type {}", type));
    }
}

[[codegen::luawrap]] void removeCustomProperty(std::string identifier) {
    using namespace openspace;
    properties::Property* p = global::userPropertyOwner->property(identifier);
    if (p) {
        global::userPropertyOwner->removeProperty(p);
        delete p;
    }
    else {
        throw ghoul::lua::LuaError(fmt::format(
            "Could not find user-defined property '{}'", identifier
        ));
    }
}

} // namespace

#include "scene_lua_codegen.cpp"
