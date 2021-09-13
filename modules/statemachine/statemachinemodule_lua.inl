/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <optional>

namespace openspace::luascriptfunctions {

int createStateMachine(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 2, 3 }, "lua::createStateMachine");
    auto [states, transitions, startState] = ghoul::lua::values<
        ghoul::Dictionary, ghoul::Dictionary, std::optional<std::string>
    >(L);

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->initializeStateMachine(
        std::move(states),
        std::move(transitions),
        std::move(startState)
    );
    return 0;
}

int goToState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::goToState");
    std::string newState = ghoul::lua::value<std::string>(L);

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->transitionTo(newState);
    LINFOC("StateMachine", "Transitioning to " + newState);
    return 0;
}

int setInitialState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setStartState");
    std::string startState = ghoul::lua::value<std::string>(L);

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->setInitialState(startState);
    LINFOC("StateMachine", "Initial state set to: " + startState);
    return 0;
}

int currentState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::currentState");

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    std::string currentState = module->currentState();
    ghoul::lua::push(L, std::move(currentState));
    return 1;
}

int possibleTransitions(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::possibleTransitions");

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    std::vector<std::string> transitions = module->possibleTransitions();
    ghoul::lua::push(L, transitions);
    return 1;
}

int canGoToState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::canGoToState");
    std::string state = ghoul::lua::value<std::string>(L);

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    ghoul::lua::push(L, module->canGoToState(state));
    return 1;
}

int printCurrentStateInfo(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::printCurrentStateInfo");

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    if (module->hasStateMachine()) {
        std::string currentState = module->currentState();
        std::vector<std::string> transitions = module->possibleTransitions();
        LINFOC("StateMachine", fmt::format(
            "Currently in state: '{}'. Can transition to states: [ {} ]",
            currentState,
            ghoul::join(transitions, ",")
        ));
    }
    else {
        LINFOC("StateMachine", "No state machine has been created");
    }

    return 1;
}

int saveToDotFile(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::saveToDotFile");
    auto [filename, directory] =
        ghoul::lua::values<std::string, std::optional<std::string>>(L);

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();

    if (directory.has_value()) {
        module->saveToFile(filename, *directory);
    }
    else {
        module->saveToFile(filename);
    }
    return 0;
}

} //namespace openspace::luascriptfunctions
