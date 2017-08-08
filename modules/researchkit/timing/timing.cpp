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

#include <modules/researchkit/timing/timing.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>

#include <ghoul/logging/logmanager.h>

#include <limits>

namespace {
    const std::string _loggerCat = "Research Kit Timing";

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo TimerInfo = {
        "Timer",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TimeoutInfo = {
        "_timeout",
        "Timeout",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TimeoutCyclesInfo = {
        "_timeoutCycles",
        "Timeout Cycles",
        "" // @TODO Missing documentation
    };
}

namespace openspace {
namespace rk {
namespace timing {

void printFrame() {
    LINFO("Printing frame");
}

Timer::Timer()
    : properties::PropertyOwner(TimerInfo)
    ,_tick(0)
    , _cycles(0) { }

bool Timer::tick() {
    // Tick first, so (timeout && rollover == 0) means "No timeout"
    _tick++;
    if (_tick == std::numeric_limits<std::size_t>::max()) {
        _tick = 0;
        _cycles++;
    }
    return false;
}

size_t Timer::getTick() {
    return _tick;
}

size_t Timer::getCycles() {
    return _cycles;
}

TimeoutTimer::TimeoutTimer()
    : Timer()
    , _timeout(TimeoutInfo, 0, 0, std::numeric_limits<unsigned long long>::max())
    , _timeoutCycles(TimeoutCyclesInfo, 0, 0, std::numeric_limits<unsigned long long>::max()) {

    addProperty(_timeout);
    addProperty(_timeoutCycles);
}

bool TimeoutTimer::tick() {
    // Tick first, so (timeout && rollover == 0) means "No timeout"
    Timer::tick();
    // Return whether past timeout
    const bool timedOut = (_timeout || _timeoutCycles ) && (_timeout <= _tick && _timeoutCycles <= _cycles);
    if (timedOut) callback();

    return timedOut;
}

void TimeoutTimer::setTimeout(unsigned long long t) {
    setTimeout(t, 0);
}

void TimeoutTimer::setTimeout(unsigned long long t, unsigned long long c) {
    _timeout = t;
    _timeoutCycles = c;
}

void ShutdownTimer::callback() {
    OsEng.windowWrapper().terminate();
}

} // namespace timing
} // namespace rk
} // namespace openspace
