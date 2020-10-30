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

#include "modules/statemachine/include/defaultstate.h"

#include <iostream>

namespace openspace {

DefaultState::DefaultState() {
    _name = "DefaultState";
}

DefaultState::~DefaultState() {

}

State& DefaultState::getInstance() {
    static DefaultState singleton;
    return singleton;
}

void DefaultState::enter(openspace::StateMachine* statemachine) {
    std::cout << "Entering Default state!" << std::endl;
    _isIdle = false;
    activate(statemachine);
}

void DefaultState::activate(openspace::StateMachine* statemachine) {
    std::cout << "Executing Default state!" << std::endl;
    _isIdle = false;
    idle(statemachine);
}

void DefaultState::idle(openspace::StateMachine* statemachine) {
    std::cout << "Default state idle!" << std::endl;
    _isIdle = true;
}

void DefaultState::exit(openspace::StateMachine* statemachine) {
    std::cout << "Leaving Default sate!" << std::endl;
    _isIdle = false;
}

bool DefaultState::isIdle() {
    return _isIdle;
}

std::string DefaultState::name() {
    return _name;
}

} // namespace openspace
