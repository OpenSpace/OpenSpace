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

#include <ghoul/filesystem/directory.h>

namespace openspace {

namespace luascriptfunctions {

namespace {

using walkFunc = std::vector<std::string>(ghoul::filesystem::Directory::*)(
    ghoul::filesystem::Directory::Recursive, ghoul::filesystem::Directory::Sort) const;

// Defining a common walk function that works off a pointer-to-member function (defined
// above) allows us to easily reuse this code
int walkCommon(lua_State* L, walkFunc func) {
    // @CPP17 Replace with std::invoke
#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))

    using namespace ghoul::filesystem;
    const int nArguments = lua_gettop(L);
    if (nArguments < 1 || nArguments > 3) {
        return luaL_error(L, "Expected %i-%i arguments, got %i", 1, 3, nArguments);
    }

    std::vector<std::string> result;
    if (nArguments == 1) {
        // Only the path was passed
        const std::string path = luaL_checkstring(L, -1);
        result = CALL_MEMBER_FN(Directory(path), func)(
            ghoul::filesystem::Directory::Recursive::No,
            ghoul::filesystem::Directory::Sort::No
            );
    }
    else if (nArguments == 2) {
        // The path and the recursive value were passed
        const std::string path = luaL_checkstring(L, -2);
        const bool recursive = lua_toboolean(L, -1) != 0;
        result = CALL_MEMBER_FN(Directory(path), func)(
            ghoul::filesystem::Directory::Recursive(recursive),
            ghoul::filesystem::Directory::Sort::No
            );
    }
    else if (nArguments == 3) {
        // All three arguments were passed
        const std::string path = luaL_checkstring(L, -3);
        const bool recursive = lua_toboolean(L, -2) != 0;
        const bool sorted = lua_toboolean(L, -1) != 0;
        result = CALL_MEMBER_FN(Directory(path), func)(
            ghoul::filesystem::Directory::Recursive(recursive),
            ghoul::filesystem::Directory::Sort(sorted)
            );
    }

    // Copy values into the lua_State
    lua_newtable(L);

    for (int i = 0; i < result.size(); ++i) {
        lua_pushstring(L, result[i].c_str());
        lua_rawseti(L, -2, i + 1);
    }

    return 1;
}
} // namespace

int printInternal(ghoul::logging::LogLevel level, lua_State* L) {
    using ghoul::lua::luaTypeToString;
    const std::string _loggerCat = "print";

    const int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

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
 * printTrace(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Trace'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printTrace(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Trace, L);
}

/**
 * \ingroup LuaScripts
 * printDebug(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Debug'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printDebug(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Debug, L);
}

/**
 * \ingroup LuaScripts
 * printInfo(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Info'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printInfo(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Info, L);
}

/**
 * \ingroup LuaScripts
 * printWarning(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Warning'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printWarning(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Warning, L);
}

/**
 * \ingroup LuaScripts
 * printError(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Error'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printError(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Error, L);
}

/**
 * \ingroup LuaScripts
 * printFatal(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Fatal'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printFatal(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Fatal, L);
}

/**
 * \ingroup LuaScripts
 * absPath(string):
 * Passes the argument to FileSystem::absolutePath, which resolves occuring path
 * tokens and returns the absolute path.
 */
int absolutePath(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %d arguments, got %d", 1, nArguments);
    }

    std::string path = luaL_checkstring(L, -1);
    path = absPath(path);
    path = FileSys.convertPathSeparator(path, '/');
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
    const int nArguments = lua_gettop(L);
    if (nArguments != 2) {
        return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);
    }

    const std::string path = luaL_checkstring(L, -1);
    const std::string pathToken = luaL_checkstring(L, -2);
    FileSys.registerPathToken(
        pathToken,
        path,
        ghoul::filesystem::FileSystem::Override::Yes
    );
    return 0;
}

/**
 * \ingroup LuaScripts
 * walkDirectory(string, bool, bool):
 * Walks a directory and returns the contents of the directory as absolute paths. The
 * first argument is the path of the directory that should be walked, the second argument
 * determines if the walk is recursive and will continue in contained directories. The
 * default value for this parameter is "false". The third argument determines whether the
 * table that is returned is sorted. The default value for this parameter is "false".
 */
int fileExists(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    const std::string file = luaL_checkstring(L, -1);
    const bool e = FileSys.fileExists(absPath(file));
    lua_pushboolean(
        L,
        e ? 1 : 0
    );
    return 1;
}

/**
 * \ingroup LuaScripts
 * walkDirectory(string, bool, bool):
 * Walks a directory and returns the contents of the directory as absolute paths. The 
 * first argument is the path of the directory that should be walked, the second argument
 * determines if the walk is recursive and will continue in contained directories. The
 * default value for this parameter is "false". The third argument determines whether the
 * table that is returned is sorted. The default value for this parameter is "false".
 */
int walkDirectory(lua_State* L) {
    return walkCommon(L, &ghoul::filesystem::Directory::read);
}

/**
 * \ingroup LuaScripts
 * walkDirectoryFiles(string, bool, bool):
 * Walks a directory and returns the files of the directory as absolute paths. The first
 * argument is the path of the directory that should be walked, the second argument
 * determines if the walk is recursive and will continue in contained directories. The
 * default value for this parameter is "false". The third argument determines whether the
 * table that is returned is sorted. The default value for this parameter is "false".
 */
int walkDirectoryFiles(lua_State* L) {
    return walkCommon(L, &ghoul::filesystem::Directory::readFiles);

}

/**
* \ingroup LuaScripts
* walkDirectory(string, bool, bool):
* Walks a directory and returns the subfolders of the directory as absolute paths. The
* first argument is the path of the directory that should be walked, the second argument
* determines if the walk is recursive and will continue in contained directories. The
* default value for this parameter is "false". The third argument determines whether the
* table that is returned is sorted. The default value for this parameter is "false".
*/
int walkDirectoryFolder(lua_State* L) {
    return walkCommon(L, &ghoul::filesystem::Directory::readDirectories);

}

} // namespace luascriptfunctions

} // namespace openspace
