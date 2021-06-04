/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include "modules/statemachine/include/statemachine.h"

#include <openspace/documentation/documentation.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "StateMachine";

    struct [[codegen::Dictionary(StateMachine)]] Parameters {
        // A list of states 
        std::vector<ghoul::Dictionary> states 
            [[codegen::reference("statemachine_state")]];

        // A list of transitions between the different states
        std::vector<ghoul::Dictionary> transitions 
            [[codegen::reference("statemachine_transition")]];
    };
#include "statemachine_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation StateMachine::Documentation() {
    return codegen::doc<Parameters>("statemachine_statemachine");
}

StateMachine::StateMachine(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    for (const ghoul::Dictionary& s : p.states) {
        _states.push_back(State(s));
    }

    for (const ghoul::Dictionary& t : p.transitions) {
        _transitions.push_back(Transition(t));
    }
}

void StateMachine::setInitialState(const std::string initialState) {
    int stateIndex = findState(initialState);

    if (stateIndex == -1) {
        LWARNING(fmt::format(
            "Attempting to initialize with undefined state '{}'", initialState
        ));
        return;
    }

    _currentState = &_states[stateIndex];
    _currentState->enter();
}

State* StateMachine::currentState() const {
    return _currentState;
}

bool StateMachine::isIdle() const {
    bool isIdle = true;
    for (unsigned int i = 0; i < _states.size(); ++i) {
        if (!_states[i].isIdle()) {
            isIdle = false;
            break;
        }
    }
    return isIdle;
}

void StateMachine::transitionTo(const std::string newState) {
    int stateIndex = findState(newState);
    if (stateIndex == -1) {
        LWARNING(fmt::format(
            "Attempting to transition to undefined state '{}'", newState
        ));
        return;
    }

    setState(_states[stateIndex]);
}

bool StateMachine::canGoTo(const std::string state) const {
    int transitionIndex = findTransitionTo(state);
    return (transitionIndex != -1) ? true : false;
}

void StateMachine::setState(State& newState) {
    if (!_currentState) {
        setInitialState(newState.name());
    }

    // Check if the transition from _currentState to newState exists
    int transitionIndex = findTransitionTo(newState.name());

    if (transitionIndex == -1) {
        LWARNING(fmt::format(
            "Transition from '{}' to '{}' is undefined",
            _currentState->name(), newState.name()
        ));
        return;
    }

    _currentState->exit();
    _transitions[transitionIndex].performAction();
    _currentState = &newState;
    _currentState->enter();
}

// Search if the transition from _currentState to newState exists.
// If yes then return the index to the transition,
// otherwise return -1
int StateMachine::findTransitionTo(const std::string state) const {
    for (unsigned int i = 0; i < _transitions.size(); ++i) {
        if (_transitions[i].from() == _currentState->name() &&
            _transitions[i].to() == state)
        {
            return i;
        }
    }
    return -1;
}

// Search if the state exist.
// If yes then return the index to the state,
// otherwise return -1
int StateMachine::findState(const std::string state) const {
    for (unsigned int i = 0; i < _states.size(); ++i) {
        if (_states[i].name() == state) {
            return i;
        }
    }
    return -1;
}

} // namespace openspace
