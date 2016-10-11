/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
 * setPort():
 * Set the port for parallel connection
 */
int setPort(lua_State* L) {
    
    const bool isFunction = (lua_isfunction(L, -1) != 0);
    if (isFunction) {
        // If the top of the stack is a function, it is ourself
        const char* msg = lua_pushfstring(L, "method called without argument");
        return luaL_error(L, "bad argument (%s)", msg);
    }

    const bool isNumber = (lua_isnumber(L, -1) != 0);
    if (isNumber) {
        int value = lua_tonumber(L, -1);
        std::string port = std::to_string(value);
        if(OsEng.isMaster()){
            OsEng.parallelConnection().setPort(port);
        }
        return 0;
    }
    else {
        const char* msg = lua_pushfstring(L, "%s expected, got %s",
            lua_typename(L, LUA_TNUMBER), luaL_typename(L, -1));
        return luaL_error(L, "bad argument #%d (%s)", 1, msg);
    }

    return 0;
}

int setAddress(lua_State* L) {
    
    const bool isFunction = (lua_isfunction(L, -1) != 0);
    if (isFunction) {
        // If the top of the stack is a function, it is ourself
        const char* msg = lua_pushfstring(L, "method called without argument");
        return luaL_error(L, "bad argument (%s)", msg);
    }

    const int type = lua_type(L, -1);
    if (type == LUA_TSTRING) {
        std::string address = luaL_checkstring(L, -1);
        if(OsEng.isMaster()){
            OsEng.parallelConnection().setAddress(address);
        }
        return 0;
    }
    else {
        const char* msg = lua_pushfstring(L, "%s expected, got %s",
            lua_typename(L, LUA_TSTRING), luaL_typename(L, -1));
        return luaL_error(L, "bad argument #%d (%s)", 1, msg);
    }

    return 0;
}

int setPassword(lua_State* L) {
    
    const bool isFunction = (lua_isfunction(L, -1) != 0);
    if (isFunction) {
        // If the top of the stack is a function, it is ourself
        const char* msg = lua_pushfstring(L, "method called without argument");
        return luaL_error(L, "bad argument (%s)", msg);
    }

    const int type = lua_type(L, -1);
    if (type == LUA_TSTRING) {
        std::string pwd = luaL_checkstring(L, -1);
        if(OsEng.isMaster()){
            OsEng.parallelConnection().setPassword(pwd);
        }
        return 0;
    }
    else {
        const char* msg = lua_pushfstring(L, "%s expected, got %s",
            lua_typename(L, LUA_TSTRING), luaL_typename(L, -1));
        return luaL_error(L, "bad argument #%d (%s)", 1, msg);
    }

    return 0;
}

int setDisplayName(lua_State* L) {
    
    const bool isFunction = (lua_isfunction(L, -1) != 0);
    if (isFunction) {
        // If the top of the stack is a function, it is ourself
        const char* msg = lua_pushfstring(L, "method called without argument");
        return luaL_error(L, "bad argument (%s)", msg);
    }

    const int type = lua_type(L, -1);
    if (type == LUA_TSTRING) {
        std::string name = luaL_checkstring(L, -1);
        if(OsEng.isMaster()){
            OsEng.parallelConnection().setName(name);
        }
        return 0;
    }
    else {
        const char* msg = lua_pushfstring(L, "%s expected, got %s",
            lua_typename(L, LUA_TSTRING), luaL_typename(L, -1));
        return luaL_error(L, "bad argument #%d (%s)", 1, msg);
    }

    return 0;
}

int connect(lua_State* L) {
    
    int nArguments = lua_gettop(L);
    if (nArguments != 0)
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    if(OsEng.isMaster()){
        OsEng.parallelConnection().clientConnect();
    }
    return 0;
}

int disconnect(lua_State* L) {
    
    int nArguments = lua_gettop(L);
    if (nArguments != 0)
    return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    if(OsEng.isMaster()){
        OsEng.parallelConnection().signalDisconnect();
    }
    return 0;
}

int requestHostship(lua_State* L) {
    
    const bool isFunction = (lua_isfunction(L, -1) != 0);
    if (isFunction) {
        // If the top of the stack is a function, it is ourself
        const char* msg = lua_pushfstring(L, "method called without argument");
        return luaL_error(L, "bad argument (%s)", msg);
    }
    
    const int type = lua_type(L, -1);
    if (type == LUA_TSTRING) {
        std::string pwd = luaL_checkstring(L, -1);
        if(OsEng.isMaster()){
            OsEng.parallelConnection().requestHostship(pwd);
        }
        return 0;
    }
    else {
        const char* msg = lua_pushfstring(L, "%s expected, got %s",
        lua_typename(L, LUA_TSTRING), luaL_typename(L, -1));
        return luaL_error(L, "bad argument #%d (%s)", 1, msg);
    }
    
    return 0;
}

int resignHostship(lua_State* L) {

    int nArguments = lua_gettop(L);
    if (nArguments != 0)
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    if (OsEng.isMaster()) {
        OsEng.parallelConnection().resignHostship();
    }
    return 0;
}

} // namespace luascriptfunctions

} // namespace openspace
