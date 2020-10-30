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

#include "modules/statemachine/include/earthstate.h"

#include <iostream>

namespace openspace {

EarthState::EarthState() {
    _name = "EarthState";
}

EarthState::~EarthState() {

}

State& EarthState::getInstance() {
    static EarthState singleton;
    return singleton;
}

void EarthState::enter(openspace::StateMachine* statemachine) {
    std::cout << "Entering Earth state!" << std::endl;
    _isIdle = false;
    activate(statemachine);
}

void EarthState::activate(openspace::StateMachine* statemachine) {
    std::cout << "Executing Earth state!" << std::endl;
    _isIdle = false;
    idle(statemachine);
}

void EarthState::idle(openspace::StateMachine* statemachine) {
    std::cout << "Earth state idle!" << std::endl;
    _isIdle = true;
}

void EarthState::exit(openspace::StateMachine* statemachine) {
    std::cout << "Leaving Earth sate!" << std::endl;
    _isIdle = false;
}

bool EarthState::isIdle() {
    return _isIdle;
}

std::string EarthState::name() {
    return _name;
}

} // namespace openspace
