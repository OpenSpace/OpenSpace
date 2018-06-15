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

#include <openspace/util/time.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/syncbuffer.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <mutex>

#include "time_lua.inl"

namespace openspace {

double Time::convertTime(const std::string& time) {
    ghoul_assert(!time.empty(), "timeString must not be empty");
    return SpiceManager::ref().ephemerisTimeFromDate(time);
}

Time::Time(double secondsJ2000) : _time(secondsJ2000) {}


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

void Time::setTime(double j2000Seconds, bool requireJump) {
    _time = j2000Seconds;
    _timeJumped = requireJump;
}

double Time::j2000Seconds() const {
    return _time;
}

double Time::advanceTime(double tickTime) {
    if (_timePaused) {
        return _time;
    }
    else {
        _time += _dt * tickTime;
    }
    return _time;
}

void Time::setDeltaTime(double deltaT) {
    _dt = deltaT;
}

double Time::deltaTime() const {
    return _dt;
}

void Time::setPause(bool pause) {
    _timePaused = pause;
}

bool Time::togglePause() {
    _timePaused = !_timePaused;
    return _timePaused;
}

void Time::setTime(std::string time, bool requireJump) {
    _time = SpiceManager::ref().ephemerisTimeFromDate(std::move(time));
    _timeJumped = requireJump;
}

std::string Time::UTC() const {
    return SpiceManager::ref().dateFromEphemerisTime(_time);
}

std::string Time::ISO8601() const {
    std::string datetime = SpiceManager::ref().dateFromEphemerisTime(_time);
    const std::string& month = datetime.substr(5, 3);

    std::string MM;
    if (month == "JAN") {
        MM = "01";
    }
    else if (month == "FEB") {
        MM = "02";
    }
    else if (month == "MAR") {
        MM = "03";
    }
    else if (month == "APR") {
        MM = "04";
    }
    else if (month == "MAY") {
        MM = "05";
    }
    else if (month == "JUN") {
        MM = "06";
    }
    else if (month == "JUL") {
        MM = "07";
    }
    else if (month == "AUG") {
        MM = "08";
    }
    else if (month == "SEP") {
        MM = "09";
    }
    else if (month == "OCT") {
        MM = "10";
    }
    else if (month == "NOV") {
        MM = "11";
    }
    else if (month == "DEC") {
        MM = "12";
    }
    else {
        ghoul_assert(false, "Bad month");
    }

    datetime.replace(4, 5, "-" + MM + "-");
    return datetime;
}

bool Time::timeJumped() const {
    return _timeJumped;
}

void Time::setTimeJumped(bool jumped) {
    _timeJumped = jumped;
}

bool Time::paused() const {
    return _timePaused;
}

scripting::LuaLibrary Time::luaLibrary() {
    return {
        "time",
        {
            {
                "setDeltaTime",
                &luascriptfunctions::time_setDeltaTime,
                {},
                "number",
                "Sets the amount of simulation time that happens "
                "in one second of real time"
            },
            {
                "deltaTime",
                &luascriptfunctions::time_deltaTime,
                {},
                "",
                "Returns the amount of simulated time that passes in one "
                "second of real time"
            },
            {
                "setPause",
                &luascriptfunctions::time_setPause,
                {},
                "bool",
                "Pauses the simulation time or restores the delta time"
            },
            {
                "togglePause",
                &luascriptfunctions::time_togglePause,
                {},
                "",
                "Toggles the pause function, i.e. temporarily setting the delta time to 0"
                " and restoring it afterwards"
            },
            {
                "setTime",
                &luascriptfunctions::time_setTime,
                {},
                "{number, string}",
                "Sets the current simulation time to the "
                "specified value. If the parameter is a number, the value is the number "
                "of seconds past the J2000 epoch. If it is a string, it has to be a "
                "valid ISO 8601-like date string of the format YYYY-MM-DDTHH:MN:SS. "
                "Note: providing time zone using the Z format is not supported. UTC is "
                "assumed."
            },
            {
                "currentTime",
                &luascriptfunctions::time_currentTime,
                {},
                "",
                "Returns the current time as the number of seconds since "
                "the J2000 epoch"
            },
            {
                "UTC",
                &luascriptfunctions::time_currentTimeUTC,
                {},
                "",
                "Returns the current time as an ISO 8601 date string "
                "(YYYY-MM-DDTHH:MN:SS)"
            },
            {
                "currentWallTime",
                &luascriptfunctions::time_currentWallTime,
                {},
                "",
                "Returns the current wall time as an ISO 8601 date string "
                "(YYYY-MM-DDTHH-MN-SS) in the UTC timezone"
            }
        }
    };
}

} // namespace openspace
