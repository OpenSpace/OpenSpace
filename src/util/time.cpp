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

#include <openspace/util/time.h>

#include <openspace/engine/globals.h>
#include <openspace/scene/profile.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/syncbuffer.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <mutex>
#include <string_view>

#include "time_lua.inl"

namespace openspace {

double Time::convertTime(const std::string& time) {
    ghoul_assert(!time.empty(), "timeString must not be empty");
    return SpiceManager::ref().ephemerisTimeFromDate(time);
}

double Time::convertTime(const char* time) {
    return SpiceManager::ref().ephemerisTimeFromDate(time);
}

Time::Time(double secondsJ2000) : _time(secondsJ2000) {}

Time::Time(const std::string& time) :
    _time(SpiceManager::ref().ephemerisTimeFromDate(time))
{}

Time Time::now() {
    Time now;
    time_t secondsSince1970;
    secondsSince1970 = time(nullptr);

    const time_t secondsInAYear = static_cast<time_t>(365.25 * 24 * 60 * 60);
    const double secondsSince2000 = static_cast<double>(
        secondsSince1970 - 30 * secondsInAYear
    );
    now.setTime(secondsSince2000);
    return now;
}

void Time::setTime(double value) {
    _time = value;
}

double Time::j2000Seconds() const {
    return _time;
}

double Time::advanceTime(double delta) {
    _time += delta;
    return _time;
}

void Time::setTime(const std::string& time) {
    _time = SpiceManager::ref().ephemerisTimeFromDate(time);
}

void Time::setTime(const char* time) {
    _time = SpiceManager::ref().ephemerisTimeFromDate(time);
}

std::string_view Time::UTC() const {
    constexpr const char Format[] = "YYYY MON DDTHR:MN:SC.### ::RND";
    char* b = reinterpret_cast<char*>(
        global::memoryManager->TemporaryMemory.allocate(32)
    );
    std::memset(b, 0, 32);

    SpiceManager::ref().dateFromEphemerisTime(_time, b, 32, Format);
    return std::string_view(b);
}

std::string_view Time::ISO8601() const {
    ZoneScoped

    constexpr const char Format[] = "YYYY-MM-DDTHR:MN:SC.###";
    constexpr const int S = sizeof(Format);
    char* b = reinterpret_cast<char*>(
        global::memoryManager->TemporaryMemory.allocate(S)
    );
    std::memset(b, 0, S);

    SpiceManager::ref().dateFromEphemerisTime(_time, b, S, Format);
    return std::string_view(b, S - 1);
}

void Time::ISO8601(char* buffer) const {
    constexpr const char Format[] = "YYYY-MM-DDTHR:MN:SC.###";
    constexpr const int S = sizeof(Format) + 1;
    std::memset(buffer, 0, S);
    SpiceManager::ref().dateFromEphemerisTime(_time, buffer, S, Format);
}

void Time::setTimeRelativeFromProfile(const std::string& setTime) {
    ghoul::lua::LuaState L(ghoul::lua::LuaState::IncludeStandardLibrary::Yes);

    luascriptfunctions::time_currentWallTime(L);
    ghoul::lua::push(L, setTime);
    luascriptfunctions::time_advancedTime(L);
    luascriptfunctions::time_setTime(L);
}

void Time::setTimeAbsoluteFromProfile(const std::string& setTime) {
    ghoul::lua::LuaState L(ghoul::lua::LuaState::IncludeStandardLibrary::Yes);

    ghoul::lua::push(L, setTime);
    luascriptfunctions::time_setTime(L);
}

scripting::LuaLibrary Time::luaLibrary() {
    return {
        "time",
        {
            {
                "setTime",
                &luascriptfunctions::time_setTime,
                "{number, string}",
                "Sets the current simulation time to the "
                "specified value. If the parameter is a number, the value is the number "
                "of seconds past the J2000 epoch. If it is a string, it has to be a "
                "valid ISO 8601-like date string of the format YYYY-MM-DDTHH:MN:SS. "
                "Note: providing time zone using the Z format is not supported. UTC is "
                "assumed."
            },
            {
                "setDeltaTime",
                &luascriptfunctions::time_setDeltaTime,
                "number",
                "Sets the amount of simulation time that happens "
                "in one second of real time"
            },
            {
                "setDeltaTimeSteps",
                &luascriptfunctions::time_setDeltaTimeSteps,
                "List of numbers",
                "Sets the list of discrete delta time steps for the simulation speed "
                "that can be quickly jumped between. The list will be sorted to be in "
                "increasing order. A negative verison of each specified time step will "
                "be added per default as well."
            },
            {
                "deltaTime",
                &luascriptfunctions::time_deltaTime,
                "",
                "Returns the amount of simulated time that passes in one "
                "second of real time"
            },
            {
                "setPause",
                &luascriptfunctions::time_setPause,
                "bool",
                "Pauses the simulation time or restores the delta time"
            },
            {
                "togglePause",
                &luascriptfunctions::time_togglePause,
                "",
                "Toggles the pause function, i.e. temporarily setting the delta time to "
                "0 and restoring it afterwards"
            },
            {
                "interpolateTime",
                &luascriptfunctions::time_interpolateTime,
                "{number, string} [, number]",
                "Sets the current simulation time to the specified value. "
                "If the first parameter is a number, the target is the number "
                "of seconds past the J2000 epoch. If it is a string, it has to be a "
                "valid ISO 8601-like date string of the format YYYY-MM-DDTHH:MN:SS "
                "(Note: providing time zone using the Z format is not supported. UTC is "
                "assumed). If a second input value is given, the interpolation is done "
                "over the specified number of seconds."
            },
            {
                "interpolateTimeRelative",
                &luascriptfunctions::time_interpolateTimeRelative,
                "number [, number]",
                "Increments the current simulation time by the specified number of "
                "seconds. If a second input value is given, the interpolation is done "
                "over the specified number of seconds."
            },
            {
                "interpolateDeltaTime",
                &luascriptfunctions::time_interpolateDeltaTime,
                "number [, number]",
                "Sets the amount of simulation time that happens in one second of real "
                "time. If a second input value is given, the interpolation is done "
                "over the specified number of seconds."
            },
            {
                "setNextDeltaTimeStep",
                &luascriptfunctions::time_setNextDeltaTimeStep,
                "",
                "Immediately set the simulation speed to the first delta time step in "
                "the list that is larger than the current choice of simulation speed, "
                "if any."
            },
            {
                "setPreviousDeltaTimeStep",
                &luascriptfunctions::time_setPreviousDeltaTimeStep,
                "",
                "Immediately set the simulation speed to the first delta time step in "
                "the list that is smaller than the current choice of simulation speed. "
                "if any."
            },
            {
                "interpolateNextDeltaTimeStep",
                &luascriptfunctions::time_interpolateNextDeltaTimeStep,
                "[number]",
                "Interpolate the simulation speed to the first delta time step in the "
                "list that is larger than the current simulation speed, if any. If an "
                "input value is given, the interpolation is done over the specified "
                "number of seconds."
            },
            {
                "interpolatePreviousDeltaTimeStep",
                &luascriptfunctions::time_interpolatePreviousDeltaTimeStep,
                "[number]",
                "Interpolate the simulation speed to the first delta time step in the "
                "list that is smaller than the current simulation speed, if any. If an "
                "input value is given, the interpolation is done over the specified "
                "number of seconds."
            },
            {
                "interpolatePause",
                &luascriptfunctions::time_interpolatePause,
                "bool [, number]",
                "Pauses the simulation time or restores the delta time. If a second "
                "input value is given, the interpolation is done over the specified "
                "number of seconds."
            },
            {
                "interpolateTogglePause",
                &luascriptfunctions::time_interpolateTogglePause,
                "[number]",
                "Toggles the pause function, i.e. temporarily setting the delta time to 0"
                " and restoring it afterwards. If an input value is given, the "
                "interpolation is done over the specified number of seconds."
            },
            {
                "pauseToggleViaKeyboard",
                &luascriptfunctions::time_pauseToggleViaKeyboard,
                "",
                "Toggles the pause function from a keypress. This function behaves like"
                " interpolateTogglePause during normal mode, and behaves like"
                " sessionRecording.pausePlayback when playing-back a recording."
            },
            {
                "currentTime",
                &luascriptfunctions::time_currentTime,
                "",
                "Returns the current time as the number of seconds since "
                "the J2000 epoch"
            },
            {
                "UTC",
                &luascriptfunctions::time_currentTimeUTC,
                "",
                "Returns the current time as an ISO 8601 date string "
                "(YYYY-MM-DDTHH:MN:SS)"
            },
            {
                "currentWallTime",
                &luascriptfunctions::time_currentWallTime,
                "",
                "Returns the current wall time as an ISO 8601 date string "
                "(YYYY-MM-DDTHH-MN-SS) in the UTC timezone"
            },
            {
                "currentApplicationTime",
                &luascriptfunctions::time_currentApplicationTime,
                "",
                "Returns the current application time as the number of seconds "
                "since the OpenSpace application started"
            },
            {
                "advancedTime",
                &luascriptfunctions::time_advancedTime,
                "string or number, string or number",
                "Modifies the passed time (first argument) by the delta time (second "
                "argument). The first argument can either be an ISO 8601 date string or "
                "the number of seconds past the J2000 epoch. The second argument can "
                "either be a string of the form [-]XX(s,m,h,d,M,y] with (s)econds, "
                "(m)inutes, (h)ours, (d)ays, (M)onths, and (y)ears as units and an "
                "optional - sign to move backwards in time. If the second argument is a "
                "number, it is interpreted as a number of seconds. The return value is "
                "of the same type as the first argument."
            }
        }
    };
}

} // namespace openspace
