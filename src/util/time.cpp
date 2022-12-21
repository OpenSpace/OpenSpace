/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/syncbuffer.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/misc.h>
#include <ghoul/misc/profiling.h>
#include <cctype>
#include <ctime>
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
    constexpr int S = sizeof(Format);
    char* b = reinterpret_cast<char*>(
        global::memoryManager->TemporaryMemory.allocate(S)
    );
    std::memset(b, 0, S);

    SpiceManager::ref().dateFromEphemerisTime(_time, b, S, Format);
    return std::string_view(b, S - 1);
}

void Time::ISO8601(char* buffer) const {
    constexpr const char Format[] = "YYYY-MM-DDTHR:MN:SC.###";
    constexpr int S = sizeof(Format) + 1;
    std::memset(buffer, 0, S);
    SpiceManager::ref().dateFromEphemerisTime(_time, buffer, S, Format);
}

void Time::setTimeRelativeFromProfile(const std::string& setTime) {
    std::string t = currentWallTime();
    std::variant<std::string, double> t2 = advancedTime(t, setTime);
    ::setTime(std::get<std::string>(t2));
}

void Time::setTimeAbsoluteFromProfile(const std::string& setTime) {
    ::setTime(setTime);
}

scripting::LuaLibrary Time::luaLibrary() {
    return {
        "time",
        {
            codegen::lua::SetDeltaTime,
            codegen::lua::SetDeltaTimeSteps,
            codegen::lua::SetNextDeltaTimeStep,
            codegen::lua::SetPreviousDeltaTimeStep,
            codegen::lua::InterpolateNextDeltaTimeStep,
            codegen::lua::InterpolatePreviousDeltaTimeStep,
            codegen::lua::InterpolateDeltaTime,
            codegen::lua::DeltaTime,
            codegen::lua::TogglePause,
            codegen::lua::InterpolateTogglePause,
            codegen::lua::PauseToggleViaKeyboard,
            codegen::lua::SetPause,
            codegen::lua::InterpolatePause,
            codegen::lua::SetTime,
            codegen::lua::InterpolateTime,
            codegen::lua::InterpolateTimeRelative,
            codegen::lua::CurrentTime,
            codegen::lua::CurrentTimeUTC,
            codegen::lua::CurrentTimeSpice,
            codegen::lua::CurrentWallTime,
            codegen::lua::CurrentApplicationTime,
            codegen::lua::AdvancedTime
        }
    };
}

} // namespace openspace
