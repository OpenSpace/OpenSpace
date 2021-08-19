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

#include <openspace/interaction/scriptcamerastates.h>
#include <openspace/navigation/navigationstate.h>
#include <numeric>

namespace openspace::luascriptfunctions {

int loadNavigationState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadNavigationState");
    const std::string cameraStateFilePath = ghoul::lua::value<std::string>(L);

    if (cameraStateFilePath.empty()) {
        return ghoul::lua::luaError(L, "filepath string is empty");
    }

    global::navigationHandler->loadNavigationState(cameraStateFilePath);
    return 0;
}

int getNavigationState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 0, 1 }, "lua::getNavigationState");
    std::optional<std::string> frame = ghoul::lua::value<std::optional<std::string>>(L);

    interaction::NavigationState state;
    if (frame.has_value()) {
        const SceneGraphNode* referenceFrame = sceneGraphNode(*frame);
        if (!referenceFrame) {
            return ghoul::lua::luaError(
                L,
                fmt::format("Could not find node '{}' as reference frame", *frame)
            );
        }
        state = global::navigationHandler->navigationState(*referenceFrame);
    }
    else {
        state = global::navigationHandler->navigationState();
    }


    const auto pushVector = [](lua_State* s, const glm::dvec3& v) {
        lua_newtable(s);
        ghoul::lua::push(s, 1, v.x);
        lua_rawset(s, -3);
        ghoul::lua::push(s, 2, v.y);
        lua_rawset(s, -3);
        ghoul::lua::push(s, 3, v.z);
        lua_rawset(s, -3);
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
        pushVector(L, *state.up);
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

    return 1;
}

int setNavigationState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setNavigationState");
    ghoul::Dictionary navigationStateDictionary = ghoul::lua::value<ghoul::Dictionary>(L);

    openspace::documentation::TestResult r = openspace::documentation::testSpecification(
        interaction::NavigationState::Documentation(),
        navigationStateDictionary
    );

    if (!r.success) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Could not set camera state: {}", ghoul::to_string(r))
        );
    }

    global::navigationHandler->setNavigationStateNextFrame(
        interaction::NavigationState(navigationStateDictionary)
    );
    return 0;
}

int saveNavigationState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::saveNavigationState");
    auto [path, frame] = ghoul::lua::values<std::string, std::optional<std::string>>(L);
    frame = frame.value_or("");

    if (path.empty()) {
        return ghoul::lua::luaError(L, "Filepath string is empty");
    }

    global::navigationHandler->saveNavigationState(path, *frame);
    return 0;
}

int retargetAnchor(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::retargetAnchor");
    global::navigationHandler->orbitalNavigator().startRetargetAnchor();
    return 0;
}

int retargetAim(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::retargetAim");
    global::navigationHandler->orbitalNavigator().startRetargetAim();
    return 0;
}

int bindJoystickAxis(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 2, 6 }, "lua::bindJoystickAxis");
    auto [axis, axisType, shouldInvert, shouldNormalize, isSticky, sensitivity] =
        ghoul::lua::values<
            int, std::string, std::optional<bool>, std::optional<bool>,
            std::optional<bool>, std::optional<double>
        >(L);
    shouldInvert = shouldInvert.value_or(false);
    shouldNormalize = shouldNormalize.value_or(false);
    isSticky = isSticky.value_or(false);
    sensitivity = sensitivity.value_or(0.0);

    global::navigationHandler->setJoystickAxisMapping(
        axis,
        ghoul::from_string<interaction::JoystickCameraStates::AxisType>(axisType),
        interaction::JoystickCameraStates::AxisInvert(*shouldInvert),
        interaction::JoystickCameraStates::AxisNormalize(*shouldNormalize),
        *isSticky,
        *sensitivity
    );
    return 0;
}

int joystickAxis(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::joystickAxis");
    const int axis = ghoul::lua::value<int>(L);

    using AI = interaction::JoystickCameraStates::AxisInformation;
    AI info = global::navigationHandler->joystickAxisMapping(axis);

    ghoul::lua::push(
        L,
        ghoul::to_string(info.type),
        static_cast<bool>(info.invert),
        static_cast<bool>(info.normalize),
        info.isSticky,
        info.sensitivity
    );
    return 5;
}

int setJoystickAxisDeadzone(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::setJoystickAxisDeadzone");
    auto [axis, deadzone] = ghoul::lua::values<int, float>(L);

    global::navigationHandler->setJoystickAxisDeadzone(axis, deadzone);
    return 0;
}

int joystickAxisDeadzone(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::joystickAxisDeadzone");
    const int axis = ghoul::lua::value<int>(L);

    const float deadzone = global::navigationHandler->joystickAxisDeadzone(axis);
    ghoul::lua::push(L, deadzone);
    return 1;
}

int bindJoystickButton(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 3, 5 }, "lua::bindJoystickButton");
    auto [button, command, documentation, actionStr, isRemote] =
        ghoul::lua::values<
            int, std::string, std::string, std::optional<std::string>, std::optional<bool>
        >(L);
    actionStr = actionStr.value_or("Press");
    isRemote = isRemote.value_or(true);

    interaction::JoystickAction action = 
        ghoul::from_string<interaction::JoystickAction>(*actionStr);

    global::navigationHandler->bindJoystickButtonCommand(
        button,
        command,
        action,
        interaction::JoystickCameraStates::ButtonCommandRemote(*isRemote),
        documentation
    );
    return 0;
}

int clearJoystickButton(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::clearJoystickButton");
    const int button = ghoul::lua::value<int>(L);
    global::navigationHandler->clearJoystickButtonCommand(button);
    return 0;
}

int joystickButton(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::joystickButton");
    const int button = ghoul::lua::value<int>(L);

    const std::vector<std::string>& cmds =
        global::navigationHandler->joystickButtonCommand(button);

    std::string cmd = std::accumulate(
        cmds.cbegin(),
        cmds.cend(),
        std::string(),
        [](const std::string& lhs, const std::string& rhs) {
            return lhs + ";" + rhs;
        }
    );
    ghoul::lua::push(L, cmd);
    return 1;
}

int addGlobalRotation(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addGlobalRotation");
    auto [v1, v2] = ghoul::lua::values<double, double>(L);

    global::navigationHandler->orbitalNavigator().scriptStates().addGlobalRotation(
        glm::dvec2(v1, v2)
    );
    return 0;
}

int addLocalRotation(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addLocalRotation");
    auto [v1, v2] = ghoul::lua::values<double, double>(L);

    global::navigationHandler->orbitalNavigator().scriptStates().addLocalRotation(
        glm::dvec2(v1, v2)
    );
    return 0;
}

int addTruckMovement(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addTruckMovement");
    auto [v1, v2] = ghoul::lua::values<double, double>(L);

    global::navigationHandler->orbitalNavigator().scriptStates().addTruckMovement(
        glm::dvec2(v1, v2)
    );
    return 0;
}

int addLocalRoll(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addLocalRoll");
    auto [v1, v2] = ghoul::lua::values<double, double>(L);

    global::navigationHandler->orbitalNavigator().scriptStates().addLocalRoll(
        glm::dvec2(v1, v2)
    );
    return 0;
}

int addGlobalRoll(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addGlobalRoll");
    auto [v1, v2] = ghoul::lua::values<double, double>(L);

    global::navigationHandler->orbitalNavigator().scriptStates().addGlobalRoll(
        glm::dvec2(v1, v2)
    );
    return 0;
}

} // namespace openspace::luascriptfunctions
