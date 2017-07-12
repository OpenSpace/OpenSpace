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
}

namespace openspace {
namespace rk {
namespace timing {

void printFrame() {
    LINFO("Printing frame");
}

Timer::Timer()
    : _tick(0)
    , _cycles(0) { }

bool Timer::tick() {
    LINFO("Tick" << _tick);
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
    , _timeout(0)
    , _timeoutCycles(0) { }

bool TimeoutTimer::tick() {
    // Tick first, so (timeout && rollover == 0) means "No timeout"
    Timer::tick();
    // Return whether past timeout
    const bool timedOut = (_timeout <= _tick && _timeoutCycles <= _cycles);
    if (timedOut) callback();

    return timedOut;
}

void TimeoutTimer::setTimeout(size_t t) {
    setTimeout(t, 0);
}

void TimeoutTimer::setTimeout(size_t t, size_t r) {
    _timeout = t;
    _timeoutCycles = r;
}

void ShutdownTimer::callback() {
    OsEng.windowWrapper().terminate();
}

} // namespace timing
} // namespace rk
} // namespace openspace
