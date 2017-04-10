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

namespace openspace {
namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * toggleShutdown():
 * Toggles the shutdown mode that will close the application after the countdown timer is
 * reached
 */
int toggleShutdown(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 0) {
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    }

    OsEng.toggleShutdownMode();

    return 0;
}

/**
* \ingroup LuaScripts
* writeDocumentation():
* Writes out documentation files
*/
int writeDocumentation(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 0) {
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    }

    OsEng.writeDocumentation();

    return 0;
}

/**
 * \ingroup LuaScripts
 * addVirtualProperty():
 * Adds a virtual property that will set a group of properties
 */
int addVirtualProperty(lua_State* L) {
    using namespace properties;

    const int nArguments = lua_gettop(L);
    if (nArguments != 6) {
        return luaL_error(L, "Expected %i arguments, got %i", 6, nArguments);
    }

    const std::string type = lua_tostring(L, -6);
    const std::string name = lua_tostring(L, -5);
    const std::string identifier = lua_tostring(L, -4);

    // @LEAK: This will leak the Property ---abock
    Property* prop;
    if (type == "BoolProperty") {
        bool v = lua_toboolean(L, -3);
        prop = new BoolProperty(identifier, name, v);
    }
    else if (type == "IntProperty") {
        int v = static_cast<int>(lua_tonumber(L, -3));
        int min = static_cast<int>(lua_tonumber(L, -2));
        int max = static_cast<int>(lua_tonumber(L, -1));

        prop = new IntProperty(identifier, name, v, min, max);
    }
    else if (type == "FloatProperty") {
        float v = static_cast<float>(lua_tonumber(L, -3));
        float min = static_cast<float>(lua_tonumber(L, -2));
        float max = static_cast<float>(lua_tonumber(L, -1));

        prop = new FloatProperty(identifier, name, v, min, max);
    }
    else if (type == "TriggerProperty") {
        prop = new TriggerProperty(identifier, name);
    }
    else {
        return luaL_error(L, "Unknown property type '%s'", type.c_str());
    }

    OsEng.virtualPropertyOwner().addProperty(prop);
    return 0;
}

/**
* \ingroup LuaScripts
* removeVirtualProperty():
* Removes a previously added virtual property
*/
int removeVirtualProperty(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    const std::string name = lua_tostring(L, -1);
    properties::Property* p = OsEng.virtualPropertyOwner().property(name);
    OsEng.virtualPropertyOwner().removeProperty(p);
    delete p;
    return 0;
}

/**
* \ingroup LuaScripts
* removeAllVirtualProperties():
* Remove all registered virtual properties
*/
int removeAllVirtualProperties(lua_State* L) {
    // @LEAK: Right now, we only *not* leak if this method is called at the end ---abock

    std::vector<properties::Property*> props = OsEng.virtualPropertyOwner().properties();
    for (properties::Property* p : props) {
        OsEng.virtualPropertyOwner().removeProperty(p);
        delete p;
    }
    return 0;
}


} // namespace luascriptfunctions
} // namespace openspace
