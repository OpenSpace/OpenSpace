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

#include <modules/statemachine/statemachinemodule.h>

#include <modules/statemachine/include/state.h>
#include <modules/statemachine/include/statemachine.h>
#include <modules/statemachine/include/transition.h>
#include <openspace/documentation/documentation.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/logging/logmanager.h>

#include "statemachinemodule_lua.inl"

namespace {
    constexpr const char* _loggerCat = "StateMachine";
}

namespace openspace {

StateMachineModule::StateMachineModule()
    : OpenSpaceModule(Name)
{ }

void StateMachineModule::initializeStateMachine(const ghoul::Dictionary& dictionary) {
    _machine = std::make_unique<StateMachine>(StateMachine(dictionary));
}

void StateMachineModule::setInitialState(const std::string initialState) {
    if (!_machine) {
        LWARNING("Attempting to use uninitialized state machine");
        return;
    }

    _machine->setInitialState(initialState);
}

std::string StateMachineModule::currentState() const {
    if (!_machine) {
        LWARNING("Attempting to use uninitialized state machine");
        return "";
    }

    return _machine->currentState()->name();
}

void StateMachineModule::transitionTo(const std::string newState) {
    if (!_machine) {
        LWARNING("Attempting to use uninitialized state machine");
        return;
    }

    _machine->transitionTo(newState);
}

bool StateMachineModule::canGoTo(const std::string state) const {
    if (!_machine) {
        LWARNING("Attempting to use uninitialized state machine");
        return false;
    }

    return _machine->canGoTo(state);
}

scripting::LuaLibrary StateMachineModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "statemachine";
    res.functions = {
         {
            "createStateMachine",
            &luascriptfunctions::createStateMachine,
            {},
            "table",
            "Creates a state machine from a table describing the states and "
            "transitions. See StateMachine documentation for details. "
        },
        {
            "goTo",
            &luascriptfunctions::goTo,
            {},
            "string",
            "Triggers a transition from the current state to th state with the given "
            "identifier. Requires that the specified string corresponds to an existing "
            "state, and that a transition between the two states exists."
        },
        {
            "setInitialState",
            &luascriptfunctions::setInitialState,
            {},
            "string",
            "Immediately sets the current state to the state with to the given name, if"
            "it exists. Must always be done after creating a state machine, before any "
            "transitions can take place."
        },
        {
            "currentState",
            &luascriptfunctions::currentState,
            {},
            "",
            "Returns the string name of the current state that the statemachine is in."
        },
        {
            "canGoTo",
            &luascriptfunctions::canGoTo,
            {},
            "string",
            "Returns true if there is a defined transition between the current state and "
            "the given string name of a state, otherwise false"
        },
    };
    return res;
}

std::vector<documentation::Documentation> StateMachineModule::documentations() const {
    return {
        State::Documentation(),
        StateMachine::Documentation(),
        Transition::Documentation()
    };
}

} // namespace openspace
