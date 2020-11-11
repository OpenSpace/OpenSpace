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
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>

namespace openspace::luascriptfunctions {

int createStateMachine(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::createStateMachine");

    ghoul::Dictionary dictionary;
    ghoul::lua::luaDictionaryFromState(L, dictionary);

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();

    module->initializeStateMachine(dictionary);
    LINFOC("StateMachine", "State machine was created.");

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int goTo(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::goTo");
    const bool isString = (lua_isstring(L, 1) != 0);

    if (!isString) {
        lua_settop(L, 0);
        const char* msg = lua_pushfstring(
            L,
            "%s expected, got %s",
            lua_typename(L, LUA_TSTRING),
            luaL_typename(L, -1)
        );
        return luaL_error(L, "bad argument #%d (%s)", 2, msg);
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
            luaL_typename(L, -1)
        );
        return luaL_error(L, "bad argument #%d (%s)", 2, msg);
    }

    const std::string startState = lua_tostring(L, 1);
    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    module->setInitialState(startState);
    LINFOC("StateMachine", "Initial state set to: " + startState);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int getCurrentState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getCurrentState");

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    std::string currentState = module->currentState();

    lua_newtable(L);
    lua_pushstring(L, currentState.c_str());

    return 1;
}

int getIsIdle(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getIsIdle");

    StateMachineModule* module = global::moduleEngine->module<StateMachineModule>();
    std::string isIdle  = module->isIdle() ? "True" : "False";

    lua_newtable(L);
    lua_pushstring(L, isIdle.c_str());
}

} //namespace openspace::luascriptfunctions
