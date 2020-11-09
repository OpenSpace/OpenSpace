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

#include <openspace/scripting/lualibrary.h>
#include <modules/statemachine/statemachinemodule_lua.inl>
#include <string>

namespace openspace {

StateMachineModule::StateMachineModule()
    : OpenSpaceModule("StateMachine")
{

}

StateMachineModule::~StateMachineModule() {

}

void StateMachineModule::initializeStateMachine(const ghoul::Dictionary& dictionary) {
    _machine = std::make_unique<StateMachine>(StateMachine(dictionary));
}

void StateMachineModule::setInitialState(std::string initialState) {
    if (!_machine) {
        // TODO: Warn
        return;
    }

    _machine->setInitialState(initialState);
}

std::string StateMachineModule::currentState() const {
    if (!_machine) {
        // TODO: Warn
        return "";
    }

    return _machine->currentState()->name();
}

void StateMachineModule::transitionTo(std::string newState) {
    if (!_machine) {
        // TODO: Warn
        return;
    }

     _machine->transitionTo(newState);
}

void StateMachineModule::internalInitialize(const ghoul::Dictionary& dictionary) {

}

void StateMachineModule::internalDeinitialize() {

}

scripting::LuaLibrary StateMachineModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "statemachine";
    res.functions = {
         {
            "createStateMachine",
            &luascriptfunctions::createStateMachine,
            {},
            "list of tables",
            "List of tables describing the states and transitions for the state machine."
        },
        {
            "goTo",
            &luascriptfunctions::goTo,
            {},
            "String",
            "String name of State to go to."
        },
        {
            "setInitialState",
            &luascriptfunctions::setInitialState,
            {},
            "String",
            "String name of the first state to set and enter into."
        },
        {
            "getCurrentState",
            &luascriptfunctions::getCurrentState,
            {},
            "",
            "Returns the string name of the current state that the statemachine is in."
        },
    };
    return res;
}

} // namespace openspace
