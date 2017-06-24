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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___TIMEDEPENDENTSTATESEQUENCE___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___TIMEDEPENDENTSTATESEQUENCE___H__

#include <openspace/util/spicemanager.h>
#include <modules/solarbrowsing/util/timedependentstate.h>
#include <ghoul/logging/logmanager.h>

// TODO(mnoven): How to include loggercat in template class?
#include <iostream>

namespace openspace {

template<typename T>
class TimedependentStateSequence {
public:
    TimedependentStateSequence();
    TimedependentStateSequence(const std::vector<TimedependentState<T>>& states);

    void addState(const TimedependentState<T>& state);
    void displayStateTimes();
    bool hasStateChanged(const double& osTime);
    int getNumStates() { return _states.size(); }
    const std::vector<TimedependentState<T>>& getStates() { return _states; }

    TimedependentState<T>& getState(double osTime);
private:
    std::vector<TimedependentState<T>> _states;
    size_t _currentActiveStateIndex;
};

} // namespace openspace

#include "timedependentstatesequence.inl"

#endif // __OPENSPACE_MODULE_SOLARBROWSING___TIMEDEPENDENTSTATESEQUENCE___H__
