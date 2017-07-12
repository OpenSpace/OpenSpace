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

#ifndef __OPENSPACE_MODULE_RESEARCHKIT___TIMING___H__
#define __OPENSPACE_MODULE_RESEARCHKIT___TIMING___H__

#include <openspace/properties/propertyowner.h>

namespace openspace {
namespace rk {
namespace timing {

void printFrame();

class Timer : public properties::PropertyOwner {

public:
    Timer();

    // Increments tick counter, returns whether timeout has occured
    virtual bool tick();

    size_t getTick();
    size_t getCycles();

protected:
    size_t _tick;
    size_t _cycles;
};

class TimeoutTimer : public Timer {
public:
    TimeoutTimer();
    virtual bool tick() override;
    // Sets the value for the timeout: will cancel after t ticks
    void setTimeout(size_t t);
    // Sets the value for the timeout: will cancel after r*size_t::max + t ticks
    void setTimeout(size_t t, size_t r);
protected:
    size_t _timeout;
    size_t _timeoutCycles;

    virtual void callback() = 0;

};

class ShutdownTimer : public TimeoutTimer {

protected:
    virtual void callback();
};

} // namespace timing
} // namespace rk
} // namespace openspace

#endif // __OPENSPACE_MODULE_RESEARCHKIT___TIMING___H__
