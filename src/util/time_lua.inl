/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <ghoul/misc/assert.h>

#include <ghoul/fmt.h>
#include <ctime>

namespace openspace::luascriptfunctions {

/**
 * \ingroup LuaScripts
 * setDeltaTime(number):
 * Sets the delta time by calling the Time::setDeltaTime method
 */
int time_setDeltaTime(lua_State* L) {
    const bool isFunction = (lua_isfunction(L, -1) != 0);
    if (isFunction) {
        // If the top of the stack is a function, it is ourself
        const char* msg = lua_pushfstring(L, "method called without argument");
        return ghoul::lua::luaError(L, fmt::format("bad argument ({})", msg));
    }

    const bool isNumber = (lua_isnumber(L, -1) != 0);
    if (isNumber) {
        double value = lua_tonumber(L, -1);
        lua_pop(L, 1);
        OsEng.timeManager().time().setDeltaTime(value);
        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
        return 0;
    }
    else {
        const char* msg = lua_pushfstring(
            L,
            "%s expected, got %s",
            lua_typename(L, LUA_TNUMBER),
            luaL_typename(L, -1)
        );
        return ghoul::lua::luaError(L, fmt::format("bad argument #1 ({})", msg));
    }

}

/**
 * \ingroup LuaScripts
 * deltaTime():
 * Returns the delta time by calling the Time::deltaTime method
 */
int time_deltaTime(lua_State* L) {
    lua_pushnumber(L, OsEng.timeManager().time().deltaTime());
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
 * \ingroup LuaScripts
 * togglePause():
 * Toggles a pause functionm i.e. setting the delta time to 0 and restoring it afterwards
 */
int time_togglePause(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_togglePause");

    OsEng.timeManager().time().togglePause();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * \ingroup LuaScripts
 * togglePause():
 * Toggles a pause functionm i.e. setting the delta time to 0 and restoring it afterwards
 */
int time_setPause(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::time_setPause");

    bool pause = lua_toboolean(L, -1) == 1;
    OsEng.timeManager().time().setPause(pause);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}


/**
 * \ingroup LuaScripts
 * setTime({number, string}):
 * Sets the simulation time to the passed value. If the parameter is a number, it is
 * interpreted as the number of seconds past the J2000 epoch and the
 * Time::setTime(double) method is called. If the parameter is a string, it is
 * interpreted as a structured date string and the Time::setTime(std::string) method
 * is called
 */
int time_setTime(lua_State* L) {
    const bool isFunction = (lua_isfunction(L, -1) != 0);
    if (isFunction) {
        // If the top of the stack is a function, it is ourself
        const char* msg = lua_pushfstring(L, "method called without argument");
        return ghoul::lua::luaError(L, fmt::format("bad argument #1 ({})", msg));
    }

    const bool isNumber = (lua_isnumber(L, -1) != 0);
    const bool isString = (lua_isstring(L, -1) != 0);
    if (!isNumber && !isString) {
        const char* msg = lua_pushfstring(L, "%s or %s expected, got %s",
                                lua_typename(L, LUA_TNUMBER),
                                lua_typename(L, LUA_TSTRING), luaL_typename(L, -1));
        return ghoul::lua::luaError(L, fmt::format("bad argument #1 ({})", msg));
    }
    if (isNumber) {
        double value = lua_tonumber(L, -1);
        OsEng.timeManager().time().setTime(value);
        return 0;
    }
    if (isString) {
        const char* time = lua_tostring(L, -1);
        OsEng.timeManager().time().setTime(time);
        return 0;
    }
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * \ingroup LuaScripts
 * currentTime():
 * Returns the current simulation time as the number of seconds past the J2000 epoch.
 * It is returned by calling the Time::currentTime method.
 */
int time_currentTime(lua_State* L) {
    lua_pushnumber(L, OsEng.timeManager().time().j2000Seconds());
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
 * \ingroup LuaScripts
 * UTC():
 * Returns the current simulation time as a structured ISO 8601 string using the UTC
 * timezone by calling the Time::UTC method
 */
int time_currentTimeUTC(lua_State* L) {
    lua_pushstring(L, OsEng.timeManager().time().UTC().c_str());
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
 * \ingroup LuaScripts
 * currentWallTime():
 * Returns the current wallclock time as a structured ISO 8601 string in the UTC timezone.
 */
int time_currentWallTime(lua_State* L) {
    std::time_t t = std::time(nullptr);
    std::tm* utcTime = std::gmtime(&t);
    ghoul_assert(utcTime, "Conversion to UTC failed");

    std::string time = fmt::format(
        "{:04d}-{:02d}-{:02d}T{:02d}:{:02d}:{:02d}",
        utcTime->tm_year + 1900,
        utcTime->tm_mon + 1,
        utcTime->tm_mday,
        utcTime->tm_hour,
        utcTime->tm_min,
        utcTime->tm_sec
    );
    lua_pushstring(L, time.c_str());
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

} // namespace openspace::luascriptfunctions
