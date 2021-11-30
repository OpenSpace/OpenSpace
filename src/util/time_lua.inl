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

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/interaction/sessionrecording.h>
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
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::time_setDeltaTime");
    double newDeltaTime = ghoul::lua::value<double>(L);

    global::timeManager->setDeltaTime(newDeltaTime);
    return 0;
}

/**
 * \ingroup LuaScripts
 * interpolateNextDeltaTimeStep(list of numbers):
 * Sets the list of discrete delta time steps for the simulation speed.
 */
int time_setDeltaTimeSteps(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::time_setDeltaTimeSteps");
    ghoul::Dictionary dict = ghoul::lua::value<ghoul::Dictionary>(L);

    const size_t nItems = dict.size();
    std::vector<double> inputDeltaTimes;
    inputDeltaTimes.reserve(nItems);

    for (size_t i = 1; i <= nItems; ++i) {
        std::string key = std::to_string(i);
        if (dict.hasValue<double>(key)) {
            const double time = dict.value<double>(key);
            inputDeltaTimes.push_back(time);
        }
        else {
            return ghoul::lua::luaError(
                L,
                "Error setting delta times. Expected list of numbers"
            );
        }
    }

    global::timeManager->setDeltaTimeSteps(inputDeltaTimes);
    return 0;
}

/**
 * \ingroup LuaScripts
 * setNextDeltaTimeStep():
 * Immediately set the simulation speed to the first delta time step in the list that is
 * larger than the current choice of simulation speed, if any.
 */
int time_setNextDeltaTimeStep(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_setNextDeltaTimeStep");
    global::timeManager->setNextDeltaTimeStep();
    return 0;
}

/**
 * \ingroup LuaScripts
 * setPreviousDeltaTimeStep():
 * Immediately set the simulation speed to the first delta time step in the list that is
 * smaller than the current choice of simulation speed, if any.
 */
int time_setPreviousDeltaTimeStep(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_setPreviousDeltaTimeStep");
    global::timeManager->setPreviousDeltaTimeStep();
    return 0;
}

/**
 * \ingroup LuaScripts
 * interpolateNextDeltaTimeStep([interpolationDuration]):
 * Interpolate the simulation speed to the next delta time step in the list. If an input
 * value is given, the interpolation is done over the specified number of seconds.
 * If interpolationDuration is not provided, the interpolation time will be based on the
 * `defaultDeltaTimeInterpolationDuration` property of the TimeManager.
 */
int time_interpolateNextDeltaTimeStep(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(
        L,
        { 0, 1 },
        "lua::time_interpolateNextDeltaTimeStep"
    );
    std::optional<double> interpDuration = ghoul::lua::value<std::optional<double>>(L);
    interpDuration = interpDuration.value_or(
        global::timeManager->defaultDeltaTimeInterpolationDuration()
    );

    global::timeManager->interpolateNextDeltaTimeStep(*interpDuration);
    return 0;
}

/**
 * \ingroup LuaScripts
 * interpolatePreviousDeltaTimeStep([interpolationDuration]):
 * Interpolate the simulation speed to the previous delta time step in the list. If an
 * input value is given, the interpolation is done over the specified number of seconds.
 * If interpolationDuration is not provided, the interpolation time will be based on the
 * `defaultDeltaTimeInterpolationDuration` property of the TimeManager.
 */
int time_interpolatePreviousDeltaTimeStep(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(
        L,
        { 0, 1 },
        "lua::time_interpolatePreviousDeltaTimeStep"
    );
    std::optional<double> interpDuration = ghoul::lua::value<std::optional<double>>(L);
    interpDuration = interpDuration.value_or(
        global::timeManager->defaultDeltaTimeInterpolationDuration()
    );

    global::timeManager->interpolatePreviousDeltaTimeStep(*interpDuration);
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
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::time_interpolateDeltaTime");
    auto [deltaTime, interpDuration] =
        ghoul::lua::values<double, std::optional<double>>(L);
    interpDuration = interpDuration.value_or(
        global::timeManager->defaultDeltaTimeInterpolationDuration()
    );

    global::timeManager->interpolateDeltaTime(deltaTime, *interpDuration);
    return 0;
}

/**
 * \ingroup LuaScripts
 * deltaTime():
 * Returns the delta time by calling the Time::deltaTime method
 */
int time_deltaTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_deltaTime");
    ghoul::lua::push(L, global::timeManager->deltaTime());
    return 1;
}

/**
 * \ingroup LuaScripts
 * togglePause():
 * Toggles pause, i.e. setting the delta time to 0 and restoring it afterwards
 */
int time_togglePause(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_togglePause");
    global::timeManager->setPause(!global::timeManager->isPaused());
    return 0;
}

/**
 * \ingroup LuaScripts
 * interpolateTogglePause([interpolationDuration]):
 * Same behaviour as togglePause, but with interpolation.
 * If no interpolation duration is provided, the interpolation time will be based on the
 * `defaultPauseInterpolationDuration` and `defaultUnpauseInterpolationDuration`
 * properties of the TimeManager.
 */
int time_interpolateTogglePause(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 0, 1 }, "lua::time_interpolateTogglePause");
    std::optional<bool> interpDuration = ghoul::lua::value<std::optional<bool>>(L);
    interpDuration = interpDuration.value_or(
        global::timeManager->isPaused() ?
        global::timeManager->defaultUnpauseInterpolationDuration() :
        global::timeManager->defaultPauseInterpolationDuration()
    );

    global::timeManager->interpolatePause(
        !global::timeManager->isPaused(),
        *interpDuration
    );
    return 0;
}

/**
 * \ingroup LuaScripts
 * time_pauseToggleViaKeyboard():
 * This allows for a keypress (via keybinding) to have dual functionality. In normal
*  operational mode it will behave just like time_interpolateTogglePause, but
 * during playback of a session recording it will pause the playback without manipulating
 * the delta time.
 */
int time_pauseToggleViaKeyboard(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_pauseToggleViaKeyboard");

    if (global::sessionRecording->isPlayingBack()) {
        bool isPlaybackPaused = global::sessionRecording->isPlaybackPaused();
        global::sessionRecording->setPlaybackPause(!isPlaybackPaused);
    }
    else {
        const bool pause = !global::timeManager->isPaused();
        global::timeManager->interpolatePause(pause,
            pause ?
            global::timeManager->defaultPauseInterpolationDuration() :
            global::timeManager->defaultUnpauseInterpolationDuration()
        );
    }
    return 0;
}

/**
 * \ingroup LuaScripts
 * togglePause():
 * Toggles a pause function i.e. setting the delta time to 0 and restoring it afterwards
 */
int time_setPause(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::time_setPause");
    bool pause = ghoul::lua::value<bool>(L);

    global::timeManager->setPause(pause);
    return 0;
}

/**
 * \ingroup LuaScripts
 * interpolateTogglePause(bool [, interpolationDuration]):
 * Same behaviour as setPause, but with interpolation.
 * If no interpolation duration is provided, the interpolation time will be based on the
 * `defaultPauseInterpolationDuration` and `defaultUnpauseInterpolationDuration`
 * properties of the TimeManager.
 */
int time_interpolatePause(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::time_interpolatePause");
    auto [pause, interpDuration] = ghoul::lua::values<bool, std::optional<double>>(L);
    interpDuration = interpDuration.value_or(
        pause ?
        global::timeManager->defaultPauseInterpolationDuration() :
        global::timeManager->defaultUnpauseInterpolationDuration()
    );

    global::timeManager->interpolatePause(pause, *interpDuration);
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
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::time_setTime");
    std::variant<std::string, double> time =
        ghoul::lua::value<std::variant<std::string, double>>(L);

    double t;
    if (std::holds_alternative<std::string>(time)) {
        t = Time::convertTime(std::get<std::string>(time));
    }
    else {
        t = std::get<double>(time);
    }

    global::timeManager->setTimeNextFrame(Time(t));
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
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::time_interpolateTime");
    auto [time, interpDuration] =
        ghoul::lua::values<std::variant<std::string, double>, std::optional<double>>(L);
    interpDuration = interpDuration.value_or(
        global::timeManager->defaultTimeInterpolationDuration()
    );

    double targetTime;
    if (std::holds_alternative<std::string>(time)) {
        targetTime = Time::convertTime(std::get<std::string>(time));
    }
    else {
        targetTime = std::get<double>(time);
    }

    if (*interpDuration > 0) {
        global::timeManager->interpolateTime(targetTime, *interpDuration);
    }
    else {
        global::timeManager->setTimeNextFrame(Time(targetTime));
    }
    return 0;
}

/**
 * \ingroup LuaScripts
 * interpolateTimeRelative(number [, interpolationDuration]):
 * Interpolates the simulation time relatively, based on the specified number of seconds.
 * If interpolationDuration is not provided, the interpolation time will be based on the
 * `defaultTimeInterpolationDuration` property of the TimeManager.
*/
int time_interpolateTimeRelative(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::time_interpolateTimeRelative");
    auto [delta, interpDuration] = ghoul::lua::values<double, std::optional<double>>(L);
    interpDuration = interpDuration.value_or(
        global::timeManager->defaultTimeInterpolationDuration()
    );

    if (*interpDuration > 0) {
        global::timeManager->interpolateTimeRelative(delta, *interpDuration);
    }
    else {
        global::timeManager->setTimeNextFrame(
            Time(global::timeManager->time().j2000Seconds() + delta)
        );
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
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_currentTime");
    ghoul::lua::push(L, global::timeManager->time().j2000Seconds());
    return 1;
}

/**
 * \ingroup LuaScripts
 * UTC():
 * Returns the current simulation time as a structured ISO 8601 string using the UTC
 * timezone by calling the Time::UTC method
 */
int time_currentTimeUTC(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_currentTimeUTC");
    ghoul::lua::push(L, global::timeManager->time().UTC());
    return 1;
}

/**
 * \ingroup LuaScripts
 * currentWallTime():
 * Returns the current wallclock time as a structured ISO 8601 string in the UTC timezone.
 */
int time_currentWallTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_currentWallTime");

    std::time_t t = std::time(nullptr);
    std::tm* utcTime = std::gmtime(&t);

    std::string time = fmt::format(
        "{:04d}-{:02d}-{:02d}T{:02d}:{:02d}:{:02d}",
        utcTime->tm_year + 1900, utcTime->tm_mon + 1, utcTime->tm_mday,
        utcTime->tm_hour, utcTime->tm_min, utcTime->tm_sec
    );
    ghoul::lua::push(L, time);
    return 1;
}

/**
 * \ingroup LuaScripts
 * currentTime():
 * Returns the current application time as the number of seconds since the OpenSpace
 * application started
 */
int time_currentApplicationTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::time_currentApplicationTime");
    ghoul::lua::push(L, global::windowDelegate->applicationTime());
    return 1;
}

int time_advancedTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::time_advanceTime");
    auto [base, change] =
        ghoul::lua::values<
            std::variant<std::string, double>, std::variant<std::string, double>
        >(L);

    double j2000Seconds = -1.0;
    bool usesISO = false;
    if (std::holds_alternative<std::string>(base)) {
        j2000Seconds = Time::convertTime(std::get<std::string>(base));
        usesISO = true;
    }
    else {
        j2000Seconds = std::get<double>(base);
        usesISO = false;
    }

    double dt = 0.0;
    if (std::holds_alternative<double>(change)) {
        dt = std::get<double>(change);
    }
    else {
        std::string modifier = std::get<std::string>(change);
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
            modifier.begin(), modifier.end(),
            [](unsigned char c) {
                const bool digit = std::isdigit(c) != 0;
                const bool isDot = c == '.';
                return !digit && !isDot;
            }
        );

        try {
            double value = std::stod(std::string(modifier.begin(), it));
            std::string uName = std::string(it, modifier.end());

            TimeUnit unit = TimeUnit::Second;
            if (uName == "s") { unit = TimeUnit::Second; }
            else if (uName == "m") { unit = TimeUnit::Minute; }
            else if (uName == "h") { unit = TimeUnit::Hour; }
            else if (uName == "d") { unit = TimeUnit::Day; }
            else if (uName == "M") { unit = TimeUnit::Month; }
            else if (uName == "y") { unit = TimeUnit::Year; }
            else {
                return ghoul::lua::luaError(L, fmt::format("Unknown unit '{}'", uName));
            }

            dt = convertTime(value, unit, TimeUnit::Second);
            if (isNegative) {
                dt *= -1.0;
            }
        }
        catch (...) {
            return ghoul::lua::luaError(L, fmt::format("Error parsing relative time "
                "offset '{}'", modifier));
        }
    }

    if (usesISO) {
        ghoul::lua::push(L, Time(j2000Seconds + dt).ISO8601());
    }
    else {
        ghoul::lua::push(L, j2000Seconds + dt);
    }
    return 1;
}

} // namespace openspace::luascriptfunctions
