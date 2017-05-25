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

namespace openspace {

template<typename T>
TimedependentStateSequence<T>::TimedependentStateSequence() {
    _currentActiveStateIndex = -1;
}

template<typename T>
TimedependentStateSequence<T>::TimedependentStateSequence(const std::vector<TimedependentState<T>>& states) {
    _states = states;
    _currentActiveStateIndex = -1;
};

template<typename T>
void TimedependentStateSequence<T>::addState(const TimedependentState<T>& state) {
    _states.push_back(state);
}

template<typename T>
bool TimedependentStateSequence<T>::hasStateChanged(const double& osTime) {
    const auto& lowerBound = std::lower_bound(_states.begin(), _states.end(), osTime);
    size_t activeStateIndex = lowerBound - _states.begin();
    bool changed = activeStateIndex != _currentActiveStateIndex;
    _currentActiveStateIndex = activeStateIndex;
    return changed;
}

template<typename T>
void TimedependentStateSequence<T>::displayStateTimes() {
    for (auto& state : _states) {
        const double& time = state.getTimeObserved();
        const std::string& dateString = SpiceManager::ref().dateFromEphemerisTime(time);
        std::cout << "State " << state.id() << " Time Observed: " << dateString << "\n";
    }
}

template<typename T>
TimedependentState<T>& TimedependentStateSequence<T>::getState(double osTime) {
    const auto& lowerBound = std::lower_bound(_states.begin(), _states.end(), osTime);
    size_t activeStateIndex = lowerBound - _states.begin();
    if (activeStateIndex == _states.size()) {
        activeStateIndex = activeStateIndex - 1;
    }
    return _states[activeStateIndex];
};

} // namespace openspace
