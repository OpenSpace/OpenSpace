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

#include <modules/statemachine/include/state.h>

#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>

namespace {
    constexpr const char* StateNameKey = "Identifier";
    constexpr const char* EnterFunctionKey = "Enter";
    constexpr const char* ExitFunctionKey = "Exit";
} // namespace

namespace openspace {

State::State(const ghoul::Dictionary& dictionary) {
    if (dictionary.hasValue<std::string>(StateNameKey)) {
        _name = dictionary.value<std::string>(StateNameKey);
    }

    if (dictionary.hasValue<std::string>(EnterFunctionKey)) {
        _enter = dictionary.value<std::string>(EnterFunctionKey);
    }

    if (dictionary.hasValue<std::string>(ExitFunctionKey)) {
        _exit = dictionary.value<std::string>(ExitFunctionKey);
    }

    _isIdle = true;
}

void State::enter(openspace::StateMachine* statemachine) {
    _isIdle = false;
    global::scriptEngine->queueScript(_enter,
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    // When script is done running, perform idle behaviour
    idle(statemachine);
}

void State::idle(openspace::StateMachine* statemachine) {
    _isIdle = true;
}

void State::exit(openspace::StateMachine* statemachine) {
    _isIdle = false;
    global::scriptEngine->queueScript(_exit,
        scripting::ScriptEngine::RemoteScripting::Yes
    );
    _isIdle = true;
}

bool State::isIdle() const{
    return _isIdle;
}

std::string State::name() const{
    return _name;
}

} // namespace openspace
