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

namespace openspace {

StateMachine::StateMachine(const ghoul::Dictionary& dictionary) {
    // Go through all states in the dictionary
    for (size_t i = 1; i <= dictionary.size(); ++i) {
        if (dictionary.hasKey(std::to_string(i))) {
            // One state
            ghoul::Dictionary stateDictionary = dictionary.value<ghoul::Dictionary>(std::to_string(i));
            _states.push_back(State(stateDictionary));
        }
    }
}

StateMachine::~StateMachine() {

}

State* StateMachine::currentState() const {
    return _currentState;
}

void StateMachine::setState(State& newState) {
    _currentState->exit(this);
    _currentState = &newState;
    _currentState->enter(this);
}

} // namespace openspace
