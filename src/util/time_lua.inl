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

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timeconversion.h>

#include <ghoul/fmt.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/misc.h>
#include <cctype>
#include <ctime>

namespace openspace::luascriptfunctions {

/**
 * \ingroup LuaScripts
 * setDeltaTime(number):
 * Sets the delta time by calling the Time::setDeltaTime method
 */
int time_setDeltaTime(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments == 1) {
        const bool isNumber = (lua_isnumber(L, 1) != 0);
        if (!isNumber) {
            lua_settop(L, 0);
            const char* msg = lua_pushfstring(
                L,
                "%s expected, got %s",
                lua_typename(L, LUA_TNUMBER),
                luaL_typename(L, -1)
            );
            return luaL_error(L, "bad argument #%d (%s)", 2, msg);
        }
        const double newDeltaTime = lua_tonumber(L, 1);
        global::timeManager.setDeltaTime(newDeltaTime);
    } else {
        lua_settop(L, 0);
        const char* msg = lua_pushfstring(L,
            "Bad number of arguments. Expected 1 or 2.");
        return ghoul::lua::luaError(L, fmt::format("bad argument ({})", msg));
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* time_interpolateDeltaTime(number [, number]):
* Interpolates the delta time by calling the Time::interpolateDeltaTime method
* Same behaviour as setDeltaTime, but interpolates the delta time.
* If interpolationDuration is not provided, the interpolation time will be based on the
* `defaultDeltaTimeInterpolationDuration` property of the TimeManager.
*/
int time_interpolateDeltaTime(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments == 2) {
        const bool deltaIsNumber = (lua_isnumber(L, 1) != 0);
        if (!deltaIsNumber) {
            lua_settop(L, 0);
            const char* msg = lua_pushfstring(
                L,
                "%s expected, got %s",
                lua_typename(L, LUA_TNUMBER),
                luaL_typename(L, -1)
            );
            return luaL_error(L, "bad argument #%d (%s)", 2, msg);
        }

        const bool durationIsNumber = (lua_isnumber(L, 2) != 0);
        if (!durationIsNumber) {
            lua_settop(L, 0);
            const char* msg = lua_pushfstring(
                L,
                "%s expected, got %s",
                lua_typename(L, LUA_TNUMBER),
                luaL_typename(L, -1)
            );
            return luaL_error(L, "bad argument #%d (%s)", 2, msg);
        }

        const double interpolationDuration = lua_tonumber(L, 2);
        const double newDeltaTime = lua_tonumber(L, 1);
        global::timeManager.interpolateDeltaTime(newDeltaTime, interpolationDuration);
    }
    else if (nArguments == 1) {
        const bool isNumber = (lua_isnumber(L, 1) != 0);
        if (!isNumber) {
            lua_settop(L, 0);
            const char* msg = lua_pushfstring(
                L,
                "%s expected, got %s",
                lua_typename(L, LUA_TNUMBER),
                luaL_typename(L, -1)
            );
            return luaL_error(L, "bad argument #%d (%s)", 2, msg);
        }
        const double newDeltaTime = lua_tonumber(L, 1);
        global::timeManager.interpolateDeltaTime(
            newDeltaTime,
            global::timeManager.defaultDeltaTimeInterpolationDuration()
        );
    }
    else {
        lua_settop(L, 0);
        const char* msg = lua_pushfstring(L,
            "Bad number of arguments. Expected 1 or 2.");
        return ghoul::lua::luaError(L, fmt::format("bad argument ({})", msg));
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}


/**
 * \ingroup LuaScripts
 * deltaTime():
 * Returns the delta time by calling the Time::deltaTime method
 */
int time_deltaTime(lua_State* L) {
    lua_pushnumber(L, global::timeManager.deltaTime());
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
 * \ingroup LuaScripts
 * togglePause():
 * Toggles pause, i.e. setting the delta time to 0 and restoring it afterwards
 */
int time_togglePause(lua_State* L) {
    const int nArguments = lua_gettop(L);

    if (nArguments == 0) {
        global::timeManager.setPause(!global::timeManager.isPaused());
    } else {
        lua_settop(L, 0);
        return luaL_error(
            L,
            "bad number of arguments, expected 0, got %i",
            nArguments
        );
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* interpolateTogglePause([interpolationDuration]):
* Same behaviour as togglePause, but with interpolation.
* If no interpolation duration is provided, the interpolation time will be based on the
* `defaultPauseInterpolationDuration` and `defaultUnpauseInterpolationDuration` properties
* of the TimeManager.
*/
int time_interpolateTogglePause(lua_State* L) {
    const int nArguments = lua_gettop(L);

    if (nArguments == 1) {
        const bool isNumber = (lua_isnumber(L, 1) != 0);
        if (!isNumber) {
            const char* msg = lua_pushfstring(
                L,
                "%s expected, got %s",
                lua_typename(L, LUA_TNUMBER),
                luaL_typename(L, -1)
            );
            return luaL_error(L, "bad argument #%d (%s)", 1, msg);
        }

        const double interpolationDuration = lua_tonumber(L, 1);

        global::timeManager.interpolatePause(
            !global::timeManager.isPaused(),
            interpolationDuration
        );
    }
    else if (nArguments == 0) {
        const bool pause = !global::timeManager.isPaused();
        global::timeManager.interpolatePause(pause,
            pause ?
            global::timeManager.defaultPauseInterpolationDuration() :
            global::timeManager.defaultUnpauseInterpolationDuration()
        );
    } else {
        lua_settop(L, 0);
        return luaL_error(
            L,
            "bad number of arguments, expected 0 or 1, got %i",
            nArguments
        );
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}


/**
 * \ingroup LuaScripts
 * togglePause():
 * Toggles a pause function i.e. setting the delta time to 0 and restoring it afterwards
 */
int time_setPause(lua_State* L) {
    const int nArguments = lua_gettop(L);

    if (nArguments == 1) {
        const bool pause = lua_toboolean(L, 1) == 1;
        global::timeManager.setPause(pause);
    } else {
        lua_settop(L, 0);
        return luaL_error(
            L,
            "bad number of arguments, expected 1, got %i",
            nArguments
        );
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* interpolateTogglePause(bool [, interpolationDuration]):
* Same behaviour as setPause, but with interpolation.
* If no interpolation duration is provided, the interpolation time will be based on the
* `defaultPauseInterpolationDuration` and `defaultUnpauseInterpolationDuration` properties
* of the TimeManager.
*/
int time_interpolatePause(lua_State* L) {
    const int nArguments = lua_gettop(L);

    if (nArguments == 2) {
        const bool isNumber = (lua_isnumber(L, 2) != 0);
        if (!isNumber) {
            lua_settop(L, 0);
            const char* msg = lua_pushfstring(
                L,
                "%s expected, got %s",
                lua_typename(L, LUA_TNUMBER),
                luaL_typename(L, -1)
            );
            return luaL_error(L, "bad argument #%d (%s)", 2, msg);
        }
        const double interpolationDuration = lua_tonumber(L, 2);
        const bool pause = lua_toboolean(L, 1) == 1;
        global::timeManager.interpolatePause(pause, interpolationDuration);
    }
    else if (nArguments == 1) {
        const bool pause = lua_toboolean(L, 1) == 1;
        global::timeManager.interpolatePause(pause,
            pause ?
            global::timeManager.defaultPauseInterpolationDuration() :
            global::timeManager.defaultUnpauseInterpolationDuration()
        );
    } else {
        lua_settop(L, 0);
        return luaL_error(
            L,
            "bad number of arguments, expected 1 or 2, got %i",
            nArguments
        );
    }

    lua_settop(L, 0);
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

    const bool isNumber = (lua_isnumber(L, 1) != 0);
    const bool isString = (lua_isstring(L, 1) != 0);
    if (!isNumber && !isString) {
        const char* msg = lua_pushfstring(
            L,
            "%s or %s expected, got %s",
            lua_typename(L, LUA_TNUMBER),
            lua_typename(L, LUA_TSTRING),
            luaL_typename(L, -1)
        );
        return ghoul::lua::luaError(L, fmt::format("bad argument #1 ({})", msg));
    }

    const int nArguments = lua_gettop(L);
    if (nArguments == 1) {
        if (isNumber) {
            double value = lua_tonumber(L, 1);
            global::timeManager.setTimeNextFrame(value);
            return 0;
        }
        if (isString) {
            const char* time = lua_tostring(L, 1);
            global::timeManager.setTimeNextFrame(Time::convertTime(time));
            return 0;
        }
        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    } else {
        return luaL_error(
            L,
            "bad number of arguments, expected 1 or 2, got %i",
            nArguments
        );
    }
    return 0;
}


/**
* \ingroup LuaScripts
* interpolateTime({number, string} [, interpolationDuration]):
* Interpolates the simulation time to the passed value.
* Same behaviour as setTime, but interpolates time.
* If interpolationDuration is not provided, the interpolation time will be based on the
* `defaultTimeInterpolationDuration` property of the TimeManager.
*/
int time_interpolateTime(lua_State* L) {
    const bool isFunction = (lua_isfunction(L, -1) != 0);
    if (isFunction) {
        // If the top of the stack is a function, it is ourself
        const char* msg = lua_pushfstring(L, "method called without argument");
        return ghoul::lua::luaError(L, fmt::format("bad argument #1 ({})", msg));
    }

    const bool isNumber = (lua_isnumber(L, 1) != 0);
    const bool isString = (lua_isstring(L, 1) != 0);
    if (!isNumber && !isString) {
        const char* msg = lua_pushfstring(
            L,
            "%s or %s expected, got %s",
            lua_typename(L, LUA_TNUMBER),
            lua_typename(L, LUA_TSTRING),
            luaL_typename(L, -1)
        );
        return ghoul::lua::luaError(L, fmt::format("bad argument #1 ({})", msg));
    }

    if (lua_gettop(L) == 1) {
        if (isNumber) {
            double value = lua_tonumber(L, 1);
            global::timeManager.interpolateTime(
                value,
                global::timeManager.defaultTimeInterpolationDuration()
            );
            return 0;
        }
        if (isString) {
            const char* time = lua_tostring(L, 1);
            global::timeManager.interpolateTime(
                Time::convertTime(time),
                global::timeManager.defaultTimeInterpolationDuration()
            );
            return 0;
        }
        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    }
    else {
        int nArguments = lua_gettop(L);
        if (nArguments != 2) {
            return luaL_error(
                L,
                "bad number of arguments, expected 1 or 2, got %i",
                nArguments
            );
        }

        double targetTime;
        if (lua_isnumber(L, 1)) {
            targetTime = lua_tonumber(L, 1);
        }
        else {
            targetTime = Time::convertTime(lua_tostring(L, 1));
        }

        const double duration = lua_tonumber(L, 2);
        if (duration > 0) {
            global::timeManager.interpolateTime(targetTime, duration);
        }
        else {
            global::timeManager.setTimeNextFrame(targetTime);
        }
    }
    return 0;
}

/**
 * \ingroup LuaScripts
 * currentTime():
 * Returns the current simulation time as the number of seconds past the J2000 epoch.
 * It is returned by calling the Time::currentTime method.
 */
int time_currentTime(lua_State* L) {
    lua_pushnumber(L, global::timeManager.time().j2000Seconds());
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
    lua_pushstring(L, global::timeManager.time().UTC().c_str());
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

int time_advancedTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::time_advanceTime");

    double j2000Seconds = -1.0;
    Time t;
    bool usesISO = false;
    if (lua_type(L, 1) == LUA_TSTRING) {
        j2000Seconds = Time::convertTime(ghoul::lua::value<std::string>(L, 1));
        usesISO = true;
    }
    else if (lua_type(L, 1) == LUA_TNUMBER) {
        j2000Seconds = ghoul::lua::value<double>(L, 1);
        usesISO = false;
    }

    double dt = 0.0;
    if (lua_type(L, 2) == LUA_TNUMBER) {
        dt = ghoul::lua::value<double>(L, 2);
    }
    else {
        std::string modifier = ghoul::lua::value<std::string>(L, 2);
        if (modifier.empty()) {
            return ghoul::lua::luaError(L, "Modifier string must not be empty");
        }
        ghoul::trimWhitespace(modifier);
        bool isNegative = false;
        if (modifier[0] == '-') {
            isNegative = true;
            modifier = modifier.substr(1);
        }

        auto it = std::find_if(
            modifier.begin(),
            modifier.end(),
            [](unsigned char c) {
                const bool digit = std::isdigit(c) != 0;
                const bool isDot = c == '.';
                return !digit && !isDot;
            }
        );

        double value = std::stod(std::string(modifier.begin(), it));

        std::string unitName = std::string(it, modifier.end());

        TimeUnit unit = TimeUnit::Second;
        if (unitName == "s") {
            unit = TimeUnit::Second;
        }
        else if (unitName == "m") {
            unit = TimeUnit::Minute;
        }
        else if (unitName == "h") {
            unit = TimeUnit::Hour;
        }
        else if (unitName == "d") {
            unit = TimeUnit::Day;
        }
        else if (unitName == "M") {
            unit = TimeUnit::Month;
        }
        else if (unitName == "y") {
            unit = TimeUnit::Year;
        }
        else {
            return ghoul::lua::luaError(
                L,
                fmt::format("Unknown unit '{}'", unitName)
            );
        }

        dt = convertTime(value, unit, TimeUnit::Second);
        if (isNegative) {
            dt *= -1.0;
        }
    }

    lua_pop(L, 2);

    if (usesISO) {
        ghoul::lua::push(L, Time(j2000Seconds + dt).ISO8601());
    }
    else {
        ghoul::lua::push(L, j2000Seconds + dt);
    }
    return 1;
}

} // namespace openspace::luascriptfunctions
