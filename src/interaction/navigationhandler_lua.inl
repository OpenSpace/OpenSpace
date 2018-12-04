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

#include <numeric>

namespace openspace::luascriptfunctions {

int restoreCameraStateFromFile(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::restoreCameraStateFromFile");

    const std::string& cameraStateFilePath = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    if (cameraStateFilePath.empty()) {
        return ghoul::lua::luaError(L, "filepath string is empty");
    }

    global::navigationHandler.restoreCameraStateFromFile(cameraStateFilePath);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int setCameraState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setCameraState");

    try {
        ghoul::Dictionary dictionary;
        ghoul::lua::luaDictionaryFromState(L, dictionary);
        global::navigationHandler.setCameraStateFromDictionary(dictionary);
    } catch (const ghoul::RuntimeError& e) {
        lua_settop(L, 0);
        return ghoul::lua::luaError(
            L,
            fmt::format("Could not set camera state: {}", e.what())
        );
    }

    // @CLEANUP:  When luaDictionaryFromState doesn't leak space anymore, remove the next
    //            line ---abock(2018-02-15)
    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int saveCameraStateToFile(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::saveCameraStateToFile");

    const std::string& cameraStateFilePath = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    if (cameraStateFilePath.empty()) {
        return ghoul::lua::luaError(L, "filepath string is empty");
    }

    global::navigationHandler.saveCameraStateToFile(cameraStateFilePath);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int resetCameraDirection(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::resetCameraDirection");

    global::navigationHandler.resetCameraDirection();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int bindJoystickAxis(lua_State* L) {
    const int n = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 2, 4 },
        "lua::bindJoystickAxis"
    );

    const int axis = ghoul::lua::value<int>(L, 1);
    const std::string& axisType = ghoul::lua::value<std::string>(L, 2);

    const bool shouldInvert = n > 2 ? ghoul::lua::value<bool>(L, 3) : false;
    const bool shouldNormalize = n > 3 ? ghoul::lua::value<bool>(L, 4) : false;

    global::navigationHandler.setJoystickAxisMapping(
        axis,
        ghoul::from_string<interaction::JoystickCameraStates::AxisType>(axisType),
        interaction::JoystickCameraStates::AxisInvert(shouldInvert),
        interaction::JoystickCameraStates::AxisNormalize(shouldNormalize)
    );

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int joystickAxis(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::joystickAxis");

    const int axis = ghoul::lua::value<int>(L, 1);

    using AI = interaction::JoystickCameraStates::AxisInformation;
    AI info = global::navigationHandler.joystickAxisMapping(axis);

    lua_settop(L, 0);
    const bool invert = info.invert;
    const bool normalize = info.normalize;
    ghoul::lua::push(L, ghoul::to_string(info.type), invert, normalize);

    ghoul_assert(lua_gettop(L) == 3, "Incorrect number of items left on stack");
    return 3;
}

int setJoystickAxisDeadzone(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::setJoystickAxisDeadzone");

    const int axis = ghoul::lua::value<int>(L, 1);
    const float deadzone = ghoul::lua::value<float>(L, 2);
    lua_settop(L, 0);

    global::navigationHandler.setJoystickAxisDeadzone(axis, deadzone);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int joystickAxisDeadzone(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setJoystickAxisDeadzone");

    const int axis = ghoul::lua::value<int>(L, 1, ghoul::lua::PopValue::Yes);

    const float deadzone = global::navigationHandler.joystickAxisDeadzone(axis);

    ghoul::lua::push(L, deadzone);
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int bindJoystickButton(lua_State* L) {
    const int n = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 3, 5 },
        "lua::bindJoystickButton"
    );

    const int button = ghoul::lua::value<int>(L, 1);
    const std::string& command = ghoul::lua::value<std::string>(L, 2);
    const std::string& documentation = ghoul::lua::value<std::string>(L, 3);

    interaction::JoystickAction action = interaction::JoystickAction::Press;
    if (n >= 4) {
        const std::string& actionStr = ghoul::lua::value<std::string>(L, 4);
        action = ghoul::from_string<interaction::JoystickAction>(actionStr);
    }

    const bool isRemote = n == 5 ? ghoul::lua::value<bool>(L, 5) : true;
    lua_settop(L, 0);

    global::navigationHandler.bindJoystickButtonCommand(
        button,
        std::move(command),
        action,
        interaction::JoystickCameraStates::ButtonCommandRemote(isRemote),
        std::move(documentation)
    );

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int clearJoystickButton(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::bindJoystickButton");

    const int button = ghoul::lua::value<int>(L, 1, ghoul::lua::PopValue::Yes);

    global::navigationHandler.clearJoystickButtonCommand(button);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int joystickButton(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::bindJoystickButton");

    const int button = ghoul::lua::value<int>(L, 1, ghoul::lua::PopValue::Yes);

    const std::vector<std::string>& cmds =
        global::navigationHandler.joystickButtonCommand(button);

    std::string cmd = std::accumulate(
        cmds.begin(),
        cmds.end(),
        std::string(),
        [](std::string lhs, std::string rhs) {
            return lhs + ";" + rhs;
        }
    );

    ghoul::lua::push(L, cmd);
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

} // namespace openspace::luascriptfunctions
