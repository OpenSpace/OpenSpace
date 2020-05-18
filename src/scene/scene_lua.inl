/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <ghoul/misc/defer.h>
#include <ghoul/misc/easing.h>
#include <regex>

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
    double interpolationDuration,
    const std::string& groupName,
    ghoul::EasingFunction easingFunction)
{
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    const bool isGroupMode = !groupName.empty();

    const int type = lua_type(L, -1);

    // Stores whether we found at least one matching property. If this is false at the end
    // of the loop, the property name regex was probably misspelled.
    bool foundMatching = false;
    std::regex r(regex);
    for (properties::Property* prop : properties) {
        // Check the regular expression for all properties
        const std::string& id = prop->fullyQualifiedIdentifier();

        if (std::regex_match(id, r)) {
            // If the fully qualified id matches the regular expression, we queue the
            // value change if the types agree
            if (isGroupMode) {
                properties::PropertyOwner* matchingTaggedOwner =
                    findPropertyOwnerWithMatchingGroupTag(
                        prop,
                        groupName
                    );
                if (!matchingTaggedOwner) {
                    continue;
                }
            }

            if (type != prop->typeLua()) {
                LERRORC(
                    "property_setValue",
                    fmt::format(
                        "{}: Property '{}' does not accept input of type '{}'. "
                        "Requested type: '{}'",
                        errorLocation(L),
                        prop->fullyQualifiedIdentifier(),
                        luaTypeToString(type),
                        luaTypeToString(prop->typeLua())
                    )
                );
            }
            else {
                foundMatching = true;

                if (interpolationDuration == 0.0) {
                    global::renderEngine.scene()->removePropertyInterpolation(prop);
                    prop->setLuaValue(L);
                }
                else {
                    prop->setLuaInterpolationTarget(L);
                    global::renderEngine.scene()->addPropertyInterpolation(
                        prop,
                        static_cast<float>(interpolationDuration),
                        easingFunction
                    );
                }
            }
        }
    }

    if (!foundMatching) {
        LERRORC(
            "property_setValue",
            fmt::format(
                "{}: No property matched the requested URI '{}'",
                errorLocation(L),
                regex
            )
        );
    }
}

// Checks to see if URI contains a group tag (with { } around the first term). If so,
// returns true and sets groupName with the tag
bool doesUriContainGroupTag(const std::string& command, std::string& groupName) {
    std::string name = command.substr(0, command.find_first_of("."));
    if (name.front() == '{' && name.back() == '}') {
        groupName = name.substr(1, name.length() - 2);
        return true;
    }
    else {
        return false;
    }
}

std::string replaceUriWithGroupName(std::string uri, std::string ownerName) {
    size_t pos = uri.find_first_of(".");
    return ownerName + "." + uri.substr(pos);
}

std::string extractUriWithoutGroupName(std::string uri) {
    size_t pos = uri.find_first_of(".");
    return uri.substr(pos);
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
                errorLocation(L),
                uri,
                luaTypeToString(type),
                luaTypeToString(prop.typeLua())
            )
        );
    }
    else {
        if (duration == 0.0) {
            global::renderEngine.scene()->removePropertyInterpolation(&prop);
            prop.setLuaValue(L);
        }
        else {
            prop.setLuaInterpolationTarget(L);
            global::renderEngine.scene()->addPropertyInterpolation(
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
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    ghoul::lua::checkArgumentsAndThrow(L, { 2, 5 }, "lua::property_setValue");
    defer { lua_settop(L, 0); };

    std::string uriOrRegex = ghoul::lua::value<std::string>(L, 1);
    std::string optimization;
    double interpolationDuration = 0.0;
    std::string easingMethodName;
    ghoul::EasingFunction easingMethod = ghoul::EasingFunction::Linear;

    if (lua_gettop(L) >= 3) {
        if (lua_type(L, 3) == LUA_TNUMBER) {
            interpolationDuration = ghoul::lua::value<double>(L, 3);
        }
        else {
            optimization = ghoul::lua::value<std::string>(L, 3);
        }

        if (lua_gettop(L) >= 4) {
            if (lua_type(L, 4) == LUA_TNUMBER) {
                interpolationDuration = ghoul::lua::value<double>(L, 4);
            }
            else {
                easingMethodName = ghoul::lua::value<std::string>(L, 4);
            }
        }

        if (lua_gettop(L) == 5) {
            optimization = ghoul::lua::value<std::string>(L, 5);
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
        bool correctName = ghoul::isValidEasingFunctionName(easingMethodName.c_str());
        if (!correctName) {
            LWARNINGC(
                "property_setValue",
                fmt::format("{} is not a valid easing method", easingMethodName)
            );
        }
        else {
            easingMethod = ghoul::easingFunctionFromName(easingMethodName.c_str());
        }
    }

    if (optimization.empty()) {
        // Replace all wildcards * with the correct regex (.*)
        size_t startPos = uriOrRegex.find("*");
        while (startPos != std::string::npos) {
            uriOrRegex.replace(startPos, 1, "(.*)");
            startPos += 4; // (.*)
            startPos = uriOrRegex.find("*", startPos);
        }

        std::string groupName;
        if (doesUriContainGroupTag(uriOrRegex, groupName)) {
            std::string pathRemainderToMatch = extractUriWithoutGroupName(uriOrRegex);
            // Remove group name from start of regex and replace with '.*'
            uriOrRegex = replaceUriWithGroupName(uriOrRegex, ".*");
        }

        try {
            applyRegularExpression(
                L,
                uriOrRegex,
                allProperties(),
                interpolationDuration,
                groupName,
                easingMethod
            );
        }
        catch (const std::regex_error& e) {
            LERRORC(
                "property_setValue",
                fmt::format(
                    "Malformed regular expression: '{}': {}", uriOrRegex, e.what()
                )
            );
        }
        return 0;
    }
    else if (optimization == "regex") {
        try {
            applyRegularExpression(
                L,
                uriOrRegex,
                allProperties(),
                interpolationDuration,
                "",
                easingMethod
            );
        }
        catch (const std::regex_error& e) {
            LERRORC(
                "property_setValueRegex",
                fmt::format(
                    "Malformed regular expression: '{}': {}", uriOrRegex, e.what()
                )
            );
        }
    }
    else if (optimization == "single") {
        properties::Property* prop = property(uriOrRegex);
        if (!prop) {
            LERRORC(
                "property_setValue",
                fmt::format(
                    "{}: Property with URI '{}' was not found",
                    errorLocation(L),
                    uriOrRegex
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
                errorLocation(L),
                optimization
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
 * getPropertyValue(string):
 * Returns the value of the property identified by the passed URI as a Lua object that can
 * be passed to the setPropertyValue method.
 */
int property_getValue(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::property_getValue");

    std::string uri = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
        );

    openspace::properties::Property* prop = property(uri);
    if (!prop) {
        LERRORC(
            "property_getValue",
            fmt::format(
                "{}: Property with URI '{}' was not found",
                ghoul::lua::errorLocation(L),
                uri
            )
        );
        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
        return 0;
    }
    else {
        prop->getLuaValue(L);
    }

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
* \ingroup LuaScripts
* getProperty
* Returns a list of property identifiers that match the passed regular expression
*/
int property_getProperty(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::property_getProperty");

    std::string regex = ghoul::lua::value<std::string>(L, 1);
    lua_pop(L, 1);

    std::string groupName;
    if (doesUriContainGroupTag(regex, groupName)) {
        std::string pathRemainderToMatch = extractUriWithoutGroupName(regex);
        // Remove group name from start of regex and replace with '.*'
        regex = replaceUriWithGroupName(regex, ".*");
    }

    // Replace all wildcards * with the correct regex (.*)
    size_t startPos = regex.find("*");
    while (startPos != std::string::npos) {
        regex.replace(startPos, 1, "(.*)");
        startPos += 4; // (.*)
        startPos = regex.find("*", startPos);
    }

    // Get all matching property uris and save to res
    std::regex r(regex);
    std::vector<properties::Property*> props = allProperties();
    std::vector<std::string> res;
    for (properties::Property* prop : props) {
        // Check the regular expression for all properties
        const std::string& id = prop->fullyQualifiedIdentifier();

        if (std::regex_match(id, r)) {
            // Filter on the groupname if there was one
            if (!groupName.empty()) {
                properties::PropertyOwner* matchingTaggedOwner =
                    findPropertyOwnerWithMatchingGroupTag(
                        prop,
                        groupName
                    );
                if (!matchingTaggedOwner) {
                    continue;
                }
                res.push_back(id);
            }
            else {
                res.push_back(id);
            }
        }
    }

    lua_newtable(L);
    int number = 1;
    for (const std::string& s : res) {
        lua_pushstring(L, s.c_str());
        lua_rawseti(L, -2, number);
        ++number;
    }

    return 1;
}

int loadScene(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadScene");

    const std::string& sceneFile = ghoul::lua::value<std::string>(L, 1);
    global::openSpaceEngine.scheduleLoadSingleAsset(sceneFile);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int addSceneGraphNode(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::addSceneGraphNode");

    ghoul::Dictionary d;
    try {
        ghoul::lua::luaDictionaryFromState(L, d);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERRORC("addSceneGraphNode", e.what());
        return ghoul::lua::luaError(L, "Error loading dictionary from lua state");
    }

    try {
        SceneGraphNode* node = global::renderEngine.scene()->loadNode(d);
        if (!node) {
            LERRORC("Scene", "Could not load scene graph node");
            return ghoul::lua::luaError(L, "Error loading scene graph node");
        }

        global::renderEngine.scene()->initializeNode(node);
    }
    catch (const documentation::SpecificationError& e) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Error loading scene graph node: {}: {}",
                e.what(), ghoul::to_string(e.result))
        );
    }
    catch (const ghoul::RuntimeError& e) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Error loading scene graph node: {}", e.what())
        );
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int removeSceneGraphNode(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeSceneGraphNode");

    std::string name = ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::Yes);

    const std::vector<SceneGraphNode*>& nodes =
        global::renderEngine.scene()->allSceneGraphNodes();

    // Replace all wildcards * with the correct regex (.*)
    size_t startPos = name.find("*");
    while (startPos != std::string::npos) {
        name.replace(startPos, 1, "(.*)");
        startPos += 4; // (.*)
        startPos = name.find("*", startPos);
    }

    bool foundMatch = false;
    std::vector<SceneGraphNode*> markedList;
    std::regex r(name);
    for (SceneGraphNode* node : nodes) {
        const std::string& identifier = node->identifier();

        if (std::regex_match(identifier, r)) {
            foundMatch = true;
            SceneGraphNode* parent = node->parent();
            if (!parent) {
                LERRORC(
                    "removeSceneGraphNode",
                    fmt::format("{}: Cannot remove root node")
                );
            }
            else {
                markedList.push_back(node);
            }
        }
    }

    if (!foundMatch) {
        LERRORC(
            "removeSceneGraphNode",
            "Did not find a match for identifier: " + name
        );
        return 0;
    }

    // Add all the children
    std::function<void(SceneGraphNode*, std::vector<SceneGraphNode*>&)> markNode =
        [&markNode](SceneGraphNode* node, std::vector<SceneGraphNode*>& markedList)
    {
        std::vector<SceneGraphNode*> children = node->children();
        for (SceneGraphNode* child : children) {
            markNode(child, markedList);
        }

        auto it = std::find(markedList.begin(), markedList.end(), node);
        if (it == markedList.end()) {
            markedList.push_back(node);
        }
    };
    for (SceneGraphNode* node : markedList) {
        markNode(node, markedList);
    }

    // Remove all marked nodes
    std::function<void(SceneGraphNode*)> removeNode =
        [&removeNode, &markedList](SceneGraphNode* localNode) {
        std::vector<SceneGraphNode*> children = localNode->children();

        std::unique_ptr<SceneGraphNode> n = localNode->parent()->detachChild(*localNode);
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


    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int hasSceneGraphNode(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::hasSceneGraphNode");

    std::string nodeName = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
        );
    SceneGraphNode* node = global::renderEngine.scene()->sceneGraphNode(nodeName);

    ghoul::lua::push(L, node != nullptr);

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int addInterestingTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addInterestingTime");

    std::string name = ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::No);
    std::string time = ghoul::lua::value<std::string>(L, 2, ghoul::lua::PopValue::No);
    lua_pop(L, 2);

    global::renderEngine.scene()->addInterestingTime(
        { std::move(name), std::move(time) }
    );

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

}  // namespace openspace::luascriptfunctions
