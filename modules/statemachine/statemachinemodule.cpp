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

void StateMachineModule::initializeStateMachine(const ghoul::Dictionary& states,
                                                const ghoul::Dictionary& transitions,
                                             const std::optional<std::string> startState)
{
    ghoul::Dictionary dictionary;
    dictionary.setValue("States", states);
    dictionary.setValue("Transitions", transitions);

    if (startState.has_value()) {
        dictionary.setValue("StartState", *startState);
    }

    _machine = std::make_unique<StateMachine>(dictionary);
}

bool StateMachineModule::hasStateMachine() const {
    return _machine != nullptr;
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

std::vector<std::string> StateMachineModule::possibleTransitions() const {
    if (!_machine) {
        LWARNING("Attempting to use uninitialized state machine");
        return std::vector<std::string>();
    }

    return _machine->possibleTransitions();
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

    return _machine->canTransitionTo(state);
}

scripting::LuaLibrary StateMachineModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "statemachine";
    res.functions = {
         {
            "createStateMachine",
            &luascriptfunctions::createStateMachine,
            {},
            "table, table, [string]",
            "Creates a state machine from a list of states and transitions. See State "
            "and Transition documentation for details. The optional thrid argument is "
            "the identifier of the desired initial state. If left out, the first state "
            "in the list will be used."
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
            "Immediately sets the current state to the state with the given name, if "
            "it exists. This is done without doing a transition and completely ignores "
            "the previous state."
        },
        {
            "currentState",
            &luascriptfunctions::currentState,
            {},
            "",
            "Returns the string name of the current state that the statemachine is in."
        },
        {
            "possibleTransitions",
            &luascriptfunctions::possibleTransitions,
            {},
            "",
            "Returns a list with the identifiers of all the states that can be "
            "transitioned to from the current state."
        },
        {
            "canGoTo",
            &luascriptfunctions::canGoTo,
            {},
            "string",
            "Returns true if there is a defined transition between the current state and "
            "the given string name of a state, otherwise false"
        },
        {
            "printCurrentStateInfo",
            &luascriptfunctions::printCurrentStateInfo,
            {},
            "",
            "Prints information about the current state and possible transitions to the log."
        }
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
