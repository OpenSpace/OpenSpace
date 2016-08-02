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

    int printInternal(ghoul::logging::LogManager::LogLevel level, lua_State* L) {
        using ghoul::lua::luaTypeToString;
        const std::string _loggerCat = "print";

        int nArguments = lua_gettop(L);
        if (nArguments != 1)
            return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

        const int type = lua_type(L, -1);
        switch (type) {
            case LUA_TNONE:
            case LUA_TLIGHTUSERDATA:
            case LUA_TTABLE:
            case LUA_TFUNCTION:
            case LUA_TUSERDATA:
            case LUA_TTHREAD:
                LOGC(level, "print", "Function parameter was of type '" <<
                     luaTypeToString(type) << "'");
            case LUA_TNIL:
                break;
            case LUA_TBOOLEAN:
                LOGC(level, "print", lua_toboolean(L, -1));
                break;
            case LUA_TNUMBER:
                LOGC(level, "print", lua_tonumber(L, -1));
                break;
            case LUA_TSTRING:
                LOGC(level, "print", lua_tostring(L, -1));
                break;
        }
        return 0;
    }

    /**
     * \ingroup LuaScripts
     * printDebug(*):
     * Logs the passed value to the installed LogManager with a LogLevel of 'Debug'.
     * For Boolean, numbers, and strings, the internal values are printed, for all other
     * types, the type is printed instead
     */
    int printDebug(lua_State* L) {
        return printInternal(ghoul::logging::LogManager::LogLevel::Debug, L);
    }

    /**
     * \ingroup LuaScripts
     * printInfo(*):
     * Logs the passed value to the installed LogManager with a LogLevel of 'Info'.
     * For Boolean, numbers, and strings, the internal values are printed, for all other
     * types, the type is printed instead
     */
    int printInfo(lua_State* L) {
        return printInternal(ghoul::logging::LogManager::LogLevel::Info, L);
    }

    /**
     * \ingroup LuaScripts
     * printWarning(*):
     * Logs the passed value to the installed LogManager with a LogLevel of 'Warning'.
     * For Boolean, numbers, and strings, the internal values are printed, for all other
     * types, the type is printed instead
     */
    int printWarning(lua_State* L) {
        return printInternal(ghoul::logging::LogManager::LogLevel::Warning, L);
    }

    /**
     * \ingroup LuaScripts
     * printError(*):
     * Logs the passed value to the installed LogManager with a LogLevel of 'Error'.
     * For Boolean, numbers, and strings, the internal values are printed, for all other
     * types, the type is printed instead
     */
    int printError(lua_State* L) {
        return printInternal(ghoul::logging::LogManager::LogLevel::Error, L);
    }

    /**
     * \ingroup LuaScripts
     * printFatal(*):
     * Logs the passed value to the installed LogManager with a LogLevel of 'Fatal'.
     * For Boolean, numbers, and strings, the internal values are printed, for all other
     * types, the type is printed instead
     */
    int printFatal(lua_State* L) {
        return printInternal(ghoul::logging::LogManager::LogLevel::Fatal, L);
    }

    /**
     * \ingroup LuaScripts
     * absPath(string):
     * Passes the argument to FileSystem::absolutePath, which resolves occuring path
     * tokens and returns the absolute path.
     */
    int absolutePath(lua_State* L) {
        int nArguments = lua_gettop(L);
        if (nArguments != 1)
            return luaL_error(L, "Expected %d arguments, got %d", 1, nArguments);

        std::string path = luaL_checkstring(L, -1);
        path = absPath(path);
        lua_pushstring(L, path.c_str());
        return 1;
    }

    /**
     * \ingroup LuaScripts
     * setPathToken(string, string):
     * Registers the path token provided by the first argument to the path in the second
     * argument. If the path token already exists, it will be silently overridden.
     */
    int setPathToken(lua_State* L) {
        int nArguments = lua_gettop(L);
        if (nArguments != 2)
            return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);

        std::string pathToken = luaL_checkstring(L, -1);
        std::string path = luaL_checkstring(L, -2);
        FileSys.registerPathToken(
            pathToken,
            path,
            ghoul::filesystem::FileSystem::Override::Yes
        );
        return 0;
    }

} // namespace luascriptfunctions

} // namespace openspace
