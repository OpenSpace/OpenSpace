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

#include <regex>

namespace openspace {

namespace {

void applyRegularExpression(lua_State* L, std::regex regex, std::vector<properties::Property*> properties, int type) {
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    for (properties::Property* prop : properties) {
        // Check the regular expression for all properties
        std::string id = prop->fullyQualifiedIdentifier();
        if (std::regex_match(id, regex)) {
            // If the fully qualified id matches the regular expression, we queue the
            // value change if the types agree

            if (type != prop->typeLua()) {
                LERRORC("property_setValue",
                        errorLocation(L) << "Property '" <<
                        prop->fullyQualifiedIdentifier() <<
                        "' does not accept input of type '" << luaTypeToString(type) <<
                        "'. Requested type: '" << luaTypeToString(prop->typeLua()) << "'"
                );
            }
            else {
                prop->setLuaValue(L);
                //ensure properties are synced over parallel connection
                std::string value;
                prop->getStringValue(value);
/*                OsEng.parallelConnection().scriptMessage(
                    prop->fullyQualifiedIdentifier(),
                    value
                );*/
            }

        }
    }
}
}


namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * setPropertyValueSingle(string, *):
 * Sets the property identified by the URI in the first argument to the value passed to
 * the second argument. The type of the second argument is arbitrary, but it must agree
 * with the type the denoted Property expects
 */
int property_setValueSingle(lua_State* L) {
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    SCRIPT_CHECK_ARGUMENTS("property_setValueSingle", L, 2, nArguments);

    std::string uri = luaL_checkstring(L, -2);
    const int type = lua_type(L, -1);

    properties::Property* prop = property(uri);
    if (!prop) {
        LERRORC("property_setValue", errorLocation(L) << "Property with URI '" << uri << "' was not found");
        return 0;
    }


    if (type != prop->typeLua()) {
        LERRORC("property_setValue", errorLocation(L) << "Property '" << uri <<
            "' does not accept input of type '" << luaTypeToString(type) <<
            "'. Requested type: '" << luaTypeToString(prop->typeLua()) << "'");
        return 0;
    }
    else {
        prop->setLuaValue(L);
        //ensure properties are synced over parallel connection
        std::string value;
        prop->getStringValue(value);
        //OsEng.parallelConnection().scriptMessage(prop->fullyQualifiedIdentifier(), value);
    }

    return 0;
}

/**
 * \ingroup LuaScripts
 * setPropertyValueRegex(string, *):
 * Sets all properties that pass the regular expression in the first argument. The second
 * argument can be any type, but it has to match the type of the properties that matched
 * the regular expression. The regular expression has to be of the ECMAScript grammar.
*/
int property_setValueRegex(lua_State* L) {
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    SCRIPT_CHECK_ARGUMENTS("property_setValueRegex<", L, 2, nArguments);

    std::string regex = luaL_checkstring(L, -2);
    try {
        applyRegularExpression(
            L,
            std::regex(regex, std::regex_constants::optimize),
            allProperties(),
            lua_type(L, -1)
        );
    }
    catch (const std::regex_error& e) {
        LERRORC("property_setValueRegex", "Malformed regular expression: '" << regex << "'");
    }

    return 0;
}

/**
* \ingroup LuaScripts
* setPropertyValue(string, *):
* Sets all properties identified by the URI (with potential wildcards) in the first
* argument. The second argument can be any type, but it has to match the type that the
* property (or properties) expect.
*/

int property_setValue(lua_State* L) {
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    SCRIPT_CHECK_ARGUMENTS("property_setValue", L, 2, nArguments);

    std::string regex = luaL_checkstring(L, -2);

    // Replace all wildcards *  with the correct regex (.*)
    size_t startPos = regex.find("*");
    while (startPos != std::string::npos) {
        regex.replace(startPos, 1, "(.*)");
        startPos += 4;

        startPos = regex.find("*", startPos);
    }


    applyRegularExpression(
        L,
        std::regex(regex/*, std::regex_constants::optimize*/),
        allProperties(),
        lua_type(L, -1)
    );

    return 0;
}

/**
 * \ingroup LuaScripts
 * getPropertyValue(string):
 * Returns the value of the property identified by the passed URI as a Lua object that can
 * be passed to the setPropertyValue method.
 */
int property_getValue(lua_State* L) {
    static const std::string _loggerCat = "property_getValue";
    using ghoul::lua::errorLocation;

    int nArguments = lua_gettop(L);
    SCRIPT_CHECK_ARGUMENTS("property_getValue", L, 1, nArguments);

    std::string uri = luaL_checkstring(L, -1);

    openspace::properties::Property* prop = property(uri);
    if (!prop) {
        LERRORC(
            "property_getValue",
            errorLocation(L) << "Property with URL '" << uri << "' was not found"
        );
        return 0;
    }
    else
        prop->getLuaValue(L);
    return 1;
}

/**
 * \ingroup LuaScripts
 * getPropertyValue(string):
 * Returns the value of the property identified by the passed URI as a Lua object that can
 * be passed to the setPropertyValue method.
 */
int loadScene(lua_State* L) {
    int nArguments = lua_gettop(L);
    SCRIPT_CHECK_ARGUMENTS("loadScene", L, 1, nArguments);

    std::string sceneFile = luaL_checkstring(L, -1);
    
    OsEng.scheduleLoadScene(sceneFile);
    return 0;
}

int addSceneGraphNode(lua_State* L) {
    using ghoul::lua::errorLocation;

    int nArguments = lua_gettop(L);
    SCRIPT_CHECK_ARGUMENTS("addSceneGraphNode", L, 1, nArguments);

    ghoul::Dictionary d;
    try {
        ghoul::lua::luaDictionaryFromState(L, d);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERRORC("addSceneGraphNode", e.what());
        return 0;
    }

    SceneLoader loader;
    SceneGraphNode* importedNode = loader.importNodeDictionary(*OsEng.renderEngine().scene(), d);
    importedNode->initialize();
        
    return 1;
}

int removeSceneGraphNode(lua_State* L) {
    using ghoul::lua::errorLocation;

    int nArguments = lua_gettop(L);
    SCRIPT_CHECK_ARGUMENTS("removeSceneGraphNode", L, 1, nArguments);

    std::string nodeName = luaL_checkstring(L, -1);
    SceneGraphNode* node = OsEng.renderEngine().scene()->sceneGraphNode(nodeName);
    if (!node) {
        LERRORC(
            "removeSceneGraphNode",
            errorLocation(L) << "Could not find node '" << nodeName << "'"
            );
        return 0;
    }
    SceneGraphNode* parent = node->parent();
    if (!parent) {
        LERRORC(
            "removeSceneGraphNode",
            errorLocation(L) << "Cannot remove root node"
            );
        return 0;
    }
    parent->detachChild(*node);
    return 1;
}

} // namespace luascriptfunctions

}  // namespace openspace
