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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___STATEMANAGER___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___STATEMANAGER___H__

#include <modules/globebrowsing/other/concurrentjobmanager.h>
#include <modules/globebrowsing/other/threadpool.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/logging/logmanager.h>

// TODO(mnoven): How to include loggercat in template class?
#include <iostream>

namespace openspace {

template<typename T>
struct TimedependentState {
    TimedependentState(std::shared_ptr<T> state, const double& timeObserved, const std::string id = "") {
        _timeObserved = timeObserved;
        _state = state;
        _id = id;
    };

    const std::string& getId() const { return _id; }
    const double& getTimeObserved() const { return _timeObserved; }
    std::shared_ptr<T> contents() const { return _state; };

    bool operator<(const double val) const {
        return _timeObserved < val;
    }

private:
    double _timeObserved;
    std::shared_ptr<T> _state;
    std::string _id;
};

template<typename T>
class TimedependentStateSequence {
public:
    TimedependentStateSequence() {
        _currentActiveStateIndex = -1;
    }

    TimedependentStateSequence(const std::vector<TimedependentState<T>>& states) {
        _states = states;
        _currentActiveStateIndex = -1;
    };

    void addState(const TimedependentState<T>& state) {
        _states.push_back(state);
    }

    bool hasStateChanged(const double& osTime) {
        const auto& lowerBound = std::lower_bound(_states.begin(), _states.end(), osTime);
        size_t activeStateIndex = lowerBound - _states.begin();
     //   std::cout << "active state idex" << activeStateIndex;
      //  std::cout << "current ac" << _currentActiveStateIndex;
        bool changed = activeStateIndex != _currentActiveStateIndex;
        _currentActiveStateIndex = activeStateIndex;
        return changed;
    }

    void displayStateTimes() {
        for (auto& state : _states) {
            const double& time = state.getTimeObserved();
            const std::string& dateString = SpiceManager::ref().dateFromEphemerisTime(time);
            std::cout << "State " << state.getId() << " Time Observed: " << dateString << "\n";
        }
    }

    // TimedependentState<T>& getState(double osTime, bool& stateChanged) {
    //     const auto& lowerBound = std::lower_bound(_states.begin(), _states.end(), osTime);
    //     size_t activeStateIndex = lowerBound - _states.begin();
    //     if (activeStateIndex == _states.size()) {
    //         activeStateIndex = activeStateIndex - 1;
    //     }
    //     if (_currentActiveStateIndex != activeStateIndex) {
    //         stateChanged = true;
    //     }
    //     _currentActiveStateIndex = activeStateIndex;
    //     return _states[_currentActiveStateIndex];
    // };

    TimedependentState<T>& getState(double osTime) {
        const auto& lowerBound = std::lower_bound(_states.begin(), _states.end(), osTime);
        size_t activeStateIndex = lowerBound - _states.begin();
        if (activeStateIndex == _states.size()) {
            activeStateIndex = activeStateIndex - 1;
        }
      //  _currentActiveStateIndex = activeStateIndex;
        return _states[activeStateIndex];
    };

private:
    std::vector<TimedependentState<T>> _states;
    size_t _currentActiveStateIndex;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___STATEMANAGER___H__
