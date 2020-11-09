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

namespace {
    constexpr const char* StatesKey = "States";
    constexpr const char* TransitionsKey = "Transitions";
} // namespace

namespace openspace {

StateMachine::StateMachine(const ghoul::Dictionary& dictionary) {

    // States
    if (dictionary.hasKey(StatesKey)) {
        ghoul::Dictionary statesDictionary =
            dictionary.value<ghoul::Dictionary>(StatesKey);

        // Go through all states in the dictionary
        for (size_t i = 1; i <= statesDictionary.size(); ++i) {
            if (statesDictionary.hasKey(std::to_string(i))) {
                // One state
                ghoul::Dictionary state =
                    statesDictionary.value<ghoul::Dictionary>(std::to_string(i));
                _states.push_back(State(state));
            }
        }
    }

    // Transitions
    if (dictionary.hasKey(TransitionsKey)) {
        ghoul::Dictionary transitionsDictionary =
            dictionary.value<ghoul::Dictionary>(TransitionsKey);

        // Go through all states in the dictionary
        for (size_t i = 1; i <= transitionsDictionary.size(); ++i) {
            if (transitionsDictionary.hasKey(std::to_string(i))) {
                // One state
                ghoul::Dictionary transition =
                    transitionsDictionary.value<ghoul::Dictionary>(std::to_string(i));
                _transitions.push_back(Transition(transition));
            }
        }
    }
}

void StateMachine::setInitialState(std::string initialState) {
    // Find initialState in already defined states
    bool wasFound = false;
    for (unsigned int i = 0; i < _states.size(); ++i) {
        if (_states[i].name() == initialState) {
            _currentState = &_states[i];
            _currentState->enter(this);
            wasFound = true;
            break;
        }
    }

    if (!wasFound) {
        // TODO: Warn
    }
}

State* StateMachine::currentState() const {
    return _currentState;
}

void StateMachine::transitionTo(std::string newState) {
    // Find newState in already defined states
    bool wasFound = false;
    for (unsigned int i = 0; i < _states.size(); ++i) {
        if (_states[i].name() == newState) {
            setState(_states[i]);
            wasFound = true;
            break;
        }
    }

    if (!wasFound) {
        // TODO: Warn
    }
}

void StateMachine::setState(State& newState) {
    // If initial state to be set
    if (!_currentState) {
        setInitialState(newState.name());
    }

    // Check if transition has been defined
    int transitionIndex = -1;
    for (unsigned int i = 0; i < _transitions.size(); ++i) {
        if ( _transitions[i].from() == _currentState->name() &&
             _transitions[i].to() == newState.name() )
        {
            transitionIndex = i;
            break;
        }
    }

    if (transitionIndex == -1) {
        // TODO: Warn
        return;
    }

    // Exit current state
    _currentState->exit(this);

    // Transition to newState
    _transitions[transitionIndex].performAction();

    // Enter new state
    _currentState = &newState;
    _currentState->enter(this);
}

} // namespace openspace
