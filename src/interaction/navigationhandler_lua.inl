/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/interaction/joystickstate.h>
#include <numeric>

namespace openspace::luascriptfunctions {

int restoreCameraStateFromFile(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::restoreCameraStateFromFile");

    using ghoul::lua::luaTypeToString;

    std::string cameraStateFilePath = ghoul::lua::checkStringAndPop(L);

    if (cameraStateFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.navigationHandler().restoreCameraStateFromFile(cameraStateFilePath);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int setCameraState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setCameraState");

    try {
        ghoul::Dictionary dictionary;
        ghoul::lua::luaDictionaryFromState(L, dictionary);
        OsEng.navigationHandler().setCameraStateFromDictionary(dictionary);
    } catch (const ghoul::RuntimeError& e) {
        lua_settop(L, 0);
        return luaL_error(L, "Could not set camera state: %s", e.what());
    }

    // @CLEANUP:  When luaDictionaryFromState doesn't leak space anymore, remove the next
    //            line ---abock(2018-02-15)
    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int saveCameraStateToFile(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::saveCameraStateToFile");

    using ghoul::lua::luaTypeToString;

    std::string cameraStateFilePath = ghoul::lua::checkStringAndPop(L);

    if (cameraStateFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.navigationHandler().saveCameraStateToFile(cameraStateFilePath);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int resetCameraDirection(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::resetCameraDirection");

    using ghoul::lua::luaTypeToString;

    OsEng.navigationHandler().resetCameraDirection();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int bindJoystickAxis(lua_State* L) {
    int n = ghoul::lua::checkArgumentsAndThrow(L, { 2, 4 }, "lua::bindJoystickAxis");

    int axis = static_cast<int>(lua_tonumber(L, 1));
    std::string axisType = lua_tostring(L, 2);

    bool shouldInvert = false;
    if (n > 2) {
        shouldInvert = lua_toboolean(L, 3);
    }
    
    bool shouldNormalize = false;
    if (n > 3) {
        shouldNormalize = lua_toboolean(L, 4);
    }

    OsEng.navigationHandler().setJoystickAxisMapping(
        axis,
        ghoul::from_string<interaction::JoystickState::AxisType>(axisType),
        interaction::JoystickState::AxisInvert(shouldInvert),
        interaction::JoystickState::AxisNormalize(shouldNormalize)
    );


    return 0;
}

int joystickAxis(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::joystickAxis");

    int axis = static_cast<int>(lua_tonumber(L, 1));

    using AI = interaction::JoystickState::AxisInformation;
    AI info = OsEng.navigationHandler().joystickAxisMapping(axis);

    lua_settop(L, 0);
    lua_pushstring(L, std::to_string(info.type).c_str());
    lua_pushboolean(L, info.invert);
    lua_pushboolean(L, info.normalize);

    return 3;
}

int bindJoystickButton(lua_State* L) {
    int n = ghoul::lua::checkArgumentsAndThrow(L, { 2, 4 }, "lua::bindJoystickButton");

    int button = static_cast<int>(lua_tonumber(L, 1));
    std::string command = lua_tostring(L, 2);

    interaction::JoystickAction action = interaction::JoystickAction::Press;
    if (n >= 3) {
        action = ghoul::from_string<interaction::JoystickAction>(lua_tostring(L, 3));
    }

    bool isRemote = true;
    if (n == 4) {
        isRemote = lua_toboolean(L, 4);
    }

    OsEng.navigationHandler().bindJoystickButtonCommand(
        button,
        std::move(command),
        action,
        interaction::JoystickState::ButtonCommandRemote(isRemote)
    );
    
    lua_settop(L, 0);
    return 0;
}

int clearJoystickButton(lua_State* L) {
    int n = ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::bindJoystickButton");

    int button = static_cast<int>(lua_tonumber(L, 1));

    OsEng.navigationHandler().clearJoystickButtonCommand(button);

    lua_settop(L, 0);
    return 0;
}

int joystickButton(lua_State* L) {
    int n = ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::bindJoystickButton");

    int button = static_cast<int>(lua_tonumber(L, 1));

    std::vector<std::string> cmds = OsEng.navigationHandler().joystickButtonCommand(
        button
    );

    std::string cmd = std::accumulate(
        cmds.begin(),
        cmds.end(),
        std::string(),
        [](std::string lhs, std::string rhs) {
            return lhs + ";" + rhs;
        }
    );

    lua_settop(L, 0);
    lua_pushstring(L, cmd.c_str());
    return 1;
}

} // namespace openspace::luascriptfunctions
