/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <openspace/interaction/scriptcamerastates.h>

namespace openspace::luascriptfunctions {

int loadNavigationState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadNavigationState");

    const std::string& cameraStateFilePath = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    if (cameraStateFilePath.empty()) {
        return ghoul::lua::luaError(L, "filepath string is empty");
    }

    global::navigationHandler.loadNavigationState(cameraStateFilePath);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int getNavigationState(lua_State* L) {
    const int n = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 0, 1 },
        "lua::getNavigationState"
    );

    interaction::NavigationHandler::NavigationState state;
    if (n == 1) {
        const std::string referenceFrameIdentifier = ghoul::lua::value<std::string>(L, 1);
        const SceneGraphNode* referenceFrame = sceneGraphNode(referenceFrameIdentifier);
        if (!referenceFrame) {
            LERROR(fmt::format(
                "Could not find node '{}' to use as reference frame",
                referenceFrameIdentifier
            ));
            lua_settop(L, 0);
            return 0;
        }
        state = global::navigationHandler.navigationState(*referenceFrame);
    }
    else {
        state = global::navigationHandler.navigationState();
    }

    lua_settop(L, 0);

    const auto pushVector = [](lua_State* L, glm::dvec3 v) {
        lua_newtable(L);
        ghoul::lua::push(L, 1, v.x);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 2, v.y);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 3, v.z);
        lua_rawset(L, -3);
    };

    lua_newtable(L);
    ghoul::lua::push(L, "Anchor", state.anchor);
    lua_rawset(L, -3);

    if (!state.aim.empty()) {
        ghoul::lua::push(L, "Aim", state.aim);
        lua_rawset(L, -3);
    }
    if (!state.referenceFrame.empty()) {
        ghoul::lua::push(L, "ReferenceFrame", state.referenceFrame);
        lua_rawset(L, -3);
    }
    ghoul::lua::push(L, "Position");
    pushVector(L, state.position);
    lua_rawset(L, -3);

    if (state.up.has_value()) {
        ghoul::lua::push(L, "Up");
        pushVector(L, state.up.value());
        lua_rawset(L, -3);
    }
    if (state.yaw != 0) {
        ghoul::lua::push(L, "Yaw", state.yaw);
        lua_rawset(L, -3);
    }
    if (state.pitch != 0) {
        ghoul::lua::push(L, "Pitch", state.pitch);
        lua_rawset(L, -3);
    }

    return lua_gettop(L);
}

int setNavigationState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setNavigationState");

    ghoul::Dictionary navigationStateDictionary;
    ghoul::lua::luaDictionaryFromState(L, navigationStateDictionary);

    openspace::documentation::TestResult r = openspace::documentation::testSpecification(
        interaction::NavigationHandler::NavigationState::Documentation(),
        navigationStateDictionary
    );

    if (!r.success) {
        lua_settop(L, 0);
        return ghoul::lua::luaError(
            L, fmt::format("Could not set camera state: {}", ghoul::to_string(r))
        );
    }

    global::navigationHandler.setNavigationStateNextFrame(navigationStateDictionary);

    // @CLEANUP:  When luaDictionaryFromState doesn't leak space anymore, remove the next
    //            line ---abock(2018-02-15)
    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int saveNavigationState(lua_State* L) {
    const int n = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 1, 2 },
        "lua::saveNavigationState"
    );

    const std::string& cameraStateFilePath = ghoul::lua::value<std::string>(L, 1);

    std::string referenceFrame = "";
    if (n > 1) {
        referenceFrame = ghoul::lua::value<std::string>(L, 2);
    }

    if (cameraStateFilePath.empty()) {
        return ghoul::lua::luaError(L, "filepath string is empty");
    }

    global::navigationHandler.saveNavigationState(cameraStateFilePath, referenceFrame);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int retargetAnchor(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::retargetAnchor");

    global::navigationHandler.orbitalNavigator().startRetargetAnchor();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int retargetAim(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::retargetAim");

    global::navigationHandler.orbitalNavigator().startRetargetAim();

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

int addGlobalRotation(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addGlobalRotation");

    const double v1 = ghoul::lua::value<double>(L, 1, ghoul::lua::PopValue::No);
    const double v2 = ghoul::lua::value<double>(L, 2, ghoul::lua::PopValue::No);

    global::navigationHandler.orbitalNavigator().scriptStates().addGlobalRotation(
        glm::dvec2(v1, v2)
    );

    lua_settop(L, 0);
    return 0;
}

int addLocalRotation(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addLocalRotation");

    const double v1 = ghoul::lua::value<double>(L, 1, ghoul::lua::PopValue::No);
    const double v2 = ghoul::lua::value<double>(L, 2, ghoul::lua::PopValue::No);

    global::navigationHandler.orbitalNavigator().scriptStates().addLocalRotation(
        glm::dvec2(v1, v2)
    );

    lua_settop(L, 0);
    return 0;
}

int addTruckMovement(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addTruckMovement");

    const double v1 = ghoul::lua::value<double>(L, 1, ghoul::lua::PopValue::No);
    const double v2 = ghoul::lua::value<double>(L, 2, ghoul::lua::PopValue::No);

    global::navigationHandler.orbitalNavigator().scriptStates().addTruckMovement(
        glm::dvec2(v1, v2)
    );

    lua_settop(L, 0);
    return 0;
}

int addLocalRoll(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addLocalRoll");

    const double v1 = ghoul::lua::value<double>(L, 1, ghoul::lua::PopValue::No);
    const double v2 = ghoul::lua::value<double>(L, 2, ghoul::lua::PopValue::No);

    global::navigationHandler.orbitalNavigator().scriptStates().addLocalRoll(
        glm::dvec2(v1, v2)
    );

    lua_settop(L, 0);
    return 0;
}

int addGlobalRoll(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addGlobalRoll");

    const double v1 = ghoul::lua::value<double>(L, 1, ghoul::lua::PopValue::No);
    const double v2 = ghoul::lua::value<double>(L, 2, ghoul::lua::PopValue::No);

    global::navigationHandler.orbitalNavigator().scriptStates().addGlobalRoll(
        glm::dvec2(v1, v2)
    );

    lua_settop(L, 0);
    return 0;
}

} // namespace openspace::luascriptfunctions
