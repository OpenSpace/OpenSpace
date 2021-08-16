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
    const int nArguments = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 2, 3 },
        "lua::createStateMachine"
    );

    // If three arguments, a start state was included
    std::optional<std::string> startState = std::nullopt;
    if (nArguments > 2) {
        startState = ghoul::lua::value<std::string>(L, 3, ghoul::lua::PopValue::Yes);
    }

    // Last dictionary is on top of the stack
    ghoul::Dictionary transitions;
    try {
        ghoul::lua::luaDictionaryFromState(L, transitions);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERRORC("createStateMachine", e.what());
        return 0;
    }

    // Pop, so that first dictionary is on top and can be read
    lua_pop(L, 1);
    ghoul::Dictionary states;
    try {
        ghoul::lua::luaDictionaryFromState(L, states);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERRORC("createStateMachine", e.what());
        return 0;
    }

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();

    module->initializeStateMachine(states, transitions, startState);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int goToState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::goToState");
    const bool isString = (lua_isstring(L, 1) != 0);

    if (!isString) {
        lua_settop(L, 0);
        const char* msg = lua_pushfstring(
            L,
            "%s expected, got %s",
            lua_typename(L, LUA_TSTRING),
            luaL_typename(L, 0)
        );
        return luaL_error(L, "bad argument #%d (%s)", 1, msg);
    }

    const std::string newState = lua_tostring(L, 1);
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->transitionTo(newState);
    LINFOC("StateMachine", "Transitioning to " + newState);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int setInitialState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setStartState");
    const bool isString = (lua_isstring(L, 1) != 0);

    if (!isString) {
        lua_settop(L, 0);
        const char* msg = lua_pushfstring(
            L,
            "%s expected, got %s",
            lua_typename(L, LUA_TSTRING),
            luaL_typename(L, 0)
        );
        return luaL_error(L, "bad argument #%d (%s)", 1, msg);
    }

    const std::string startState = lua_tostring(L, 1);
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->setInitialState(startState);
    LINFOC("StateMachine", "Initial state set to: " + startState);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int currentState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::currentState");

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    std::string currentState = module->currentState();

    lua_pushstring(L, currentState.c_str());
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int possibleTransitions(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::possibleTransitions");

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    std::vector<std::string> transitions = module->possibleTransitions();

    ghoul::lua::push(L, transitions);
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int canGoToState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::canGoToState");
    const bool isString = (lua_isstring(L, 1) != 0);

    if (!isString) {
        lua_settop(L, 0);
        const char* msg = lua_pushfstring(
            L,
            "%s expected, got %s",
            lua_typename(L, LUA_TSTRING),
            luaL_typename(L, 0)
        );
        return luaL_error(L, "bad argument #%d (%s)", 1, msg);
    }

    const std::string state = lua_tostring(L, 1);
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    ghoul::lua::push(L, module->canGoToState(state));

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
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

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 1;
}

} //namespace openspace::luascriptfunctions
