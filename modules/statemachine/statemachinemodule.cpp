/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <string>
#include <optional>

#include "statemachinemodule_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "StateMachine";
} // namespace

namespace openspace {

StateMachineModule::StateMachineModule()
    : OpenSpaceModule(Name)
{ }

void StateMachineModule::initializeStateMachine(const ghoul::Dictionary& states,
                                                const ghoul::Dictionary& transitions,
                                                std::optional<std::string> startState)
{
    ghoul::Dictionary dictionary;
    dictionary.setValue("States", states);
    dictionary.setValue("Transitions", transitions);

    if (startState.has_value()) {
        dictionary.setValue("StartState", std::move(*startState));
    }

    try {
        _machine = std::make_unique<StateMachine>(dictionary);
        LINFO(std::format(
            "State machine was created with start state: {}", currentState()
        ));
    }
    catch (const documentation::SpecificationError& e) {
        LERROR(std::format("Error loading state machine: {}", e.what()));
        logError(e);
    }
}

void StateMachineModule::deinitializeStateMachine() {
    _machine.reset();
    _machine = nullptr;
}

bool StateMachineModule::hasStateMachine() const {
    return _machine != nullptr;
}

void StateMachineModule::setInitialState(const std::string& initialState) {
    if (!_machine) {
        LERROR("Attempting to use uninitialized state machine");
        return;
    }

    _machine->setInitialState(initialState);
}

std::string StateMachineModule::currentState() const {
    if (!_machine || !_machine->currentState()) {
        LERROR("Attempting to use uninitialized state machine");
        return "";
    }

    return _machine->currentState()->name();
}

std::vector<std::string> StateMachineModule::possibleTransitions() const {
    if (!_machine) {
        LERROR("Attempting to use uninitialized state machine");
        return std::vector<std::string>();
    }

    return _machine->possibleTransitions();
}

void StateMachineModule::transitionTo(const std::string& newState) {
    if (!_machine) {
        LERROR("Attempting to use uninitialized state machine");
        return;
    }

    _machine->transitionTo(newState);
}

bool StateMachineModule::canGoToState(const std::string& state) const {
    if (!_machine) {
        LERROR("Attempting to use uninitialized state machine");
        return false;
    }

    return _machine->canTransitionTo(state);
}

void StateMachineModule::saveToFile(const std::string& filename,
                                    std::string directory) const
{
    if (!_machine) {
        LERROR("Attempting to use uninitialized state machine");
        return;
    }

    if (directory.back() != '/') {
        directory += '/';
    }

    const std::filesystem::path outputFile = absPath(directory + filename);
    _machine->saveToDotFile(outputFile);
}

scripting::LuaLibrary StateMachineModule::luaLibrary() const {
    return {
        "statemachine",
        {
        codegen::lua::CreateStateMachine,
        codegen::lua::DestroyStateMachine,
        codegen::lua::GoToState,
        codegen::lua::SetInitialState,
        codegen::lua::CurrentState,
        codegen::lua::PossibleTransitions,
        codegen::lua::CanGoToState,
        codegen::lua::PrintCurrentStateInfo,
        codegen::lua::SaveToDotFile
        }
    };
}

std::vector<documentation::Documentation> StateMachineModule::documentations() const {
    return {
        State::Documentation(),
        StateMachine::Documentation(),
        Transition::Documentation()
    };
}

} // namespace openspace
