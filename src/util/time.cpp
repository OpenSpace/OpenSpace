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

void Time::setTime(std::string time) {
    _time = SpiceManager::ref().ephemerisTimeFromDate(std::move(time));
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

scripting::LuaLibrary Time::luaLibrary() {
    return {
        "time",
        {
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
                "interpolateTime",
                &luascriptfunctions::time_interpolateTime,
                {},
                "{number, string} [, number]",
                "Sets the current simulation time to the "
                "specified value. If the parameter is a number, the value is the number "
                "of seconds past the J2000 epoch. If it is a string, it has to be a "
                "valid ISO 8601-like date string of the format YYYY-MM-DDTHH:MN:SS. "
                "Note: providing time zone using the Z format is not supported. UTC is "
                "assumed."
            },
            {
                "interpolateDeltaTime",
                &luascriptfunctions::time_interpolateDeltaTime,
                {},
                "number",
                "Sets the amount of simulation time that happens "
                "in one second of real time"
            },
            {
                "interpolatePause",
                &luascriptfunctions::time_interpolatePause,
                {},
                "bool",
                "Pauses the simulation time or restores the delta time"
            },
            {
                "interpolateTogglePause",
                &luascriptfunctions::time_interpolateTogglePause,
                {},
                "",
                "Toggles the pause function, i.e. temporarily setting the delta time to 0"
                " and restoring it afterwards"
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
            },
            {
                "advancedTime",
                &luascriptfunctions::time_advancedTime,
                {},
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
