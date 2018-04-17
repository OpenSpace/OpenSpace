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

#include <openspace/util/spicemanager.h>

namespace openspace {

template <typename T>
TimedependentStateSequence<T>::TimedependentStateSequence(
                                                std::vector<TimedependentState<T>> states)
    : _states(std::move(states))
    , _currentActiveStateIndex(-1)
{}

template <typename T>
void TimedependentStateSequence<T>::addState(TimedependentState<T> state) {
    _states.push_back(std::move(state));
}

template <typename T>
bool TimedependentStateSequence<T>::hasStateChanged(const double& osTime) {
    const auto& lowerBound = std::lower_bound(_states.begin(), _states.end(), osTime);
    size_t activeStateIndex = lowerBound - _states.begin();
    bool hasChanged = activeStateIndex != _currentActiveStateIndex;
    _currentActiveStateIndex = activeStateIndex;
    return hasChanged;
}

template <typename T>
int TimedependentStateSequence<T>::numStates() const {
    return static_cast<int>(_states.size());
}

template <typename T>
const std::vector<TimedependentState<T>>& TimedependentStateSequence<T>::states() const {
    return _states;
}

template <typename T>
void TimedependentStateSequence<T>::displayStateTimes() const {
    for (const TimedependentState<T>& state : _states) {
        const double& time = state.timeObserved();
        const std::string& dateString = SpiceManager::ref().dateFromEphemerisTime(time);
        LINFOC(
            "TimedependentStateSequence",
            fmt::format("State '{}' | Time Observed: {}", state.id(), dateString)
        );
    }
}

template <typename T>
const TimedependentState<T>& TimedependentStateSequence<T>::state(double osTime) const {
    const auto& lowerBound = std::lower_bound(_states.begin(), _states.end(), osTime);
    size_t activeStateIndex = lowerBound - _states.begin();
    if (activeStateIndex == _states.size()) {
        activeStateIndex = activeStateIndex - 1;
    }
    return _states[activeStateIndex];
};

} // namespace openspace
