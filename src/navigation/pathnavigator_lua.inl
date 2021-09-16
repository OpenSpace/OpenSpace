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

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/updatestructures.h>
#include <openspace/query/query.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/vector_angle.hpp>

namespace {
    constexpr const double Epsilon = 1e-5;
} // namespace

namespace openspace::luascriptfunctions {

int isFlying(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::isFlying");
    bool hasFinished = global::navigationHandler->pathNavigator().hasFinished();
    ghoul::lua::push(L, !hasFinished);
    return 1;
}

int continuePath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::continuePath");
    global::navigationHandler->pathNavigator().continuePath();
    return 0;
}

int pausePath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::pausePath");
    global::navigationHandler->pathNavigator().pausePath();
    return 0;
}

int stopPath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::stopPath");
    global::navigationHandler->pathNavigator().abortPath();
    return 0;
}

int goTo(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 3 }, "lua::goTo");
    auto [nodeIdentifier, useUpFromTargetOrDuration, duration] = ghoul::lua::values<
        std::string, std::optional<std::variant<bool, double>>, std::optional<double>
    >(L);

    if (useUpFromTargetOrDuration.has_value() &&
        std::holds_alternative<double>(*useUpFromTargetOrDuration)
        && duration.has_value())
    {
        return ghoul::lua::luaError(L, "Duration cannot be specified twice");
    }


    if (!sceneGraphNode(nodeIdentifier)) {
        return ghoul::lua::luaError(L, "Unknown node name: " + nodeIdentifier);
    }

    using namespace std::string_literals;
    ghoul::Dictionary insDict;
    insDict.setValue("TargetType", "Node"s);
    insDict.setValue("Target", nodeIdentifier);
    if (useUpFromTargetOrDuration.has_value()) {
        if (std::holds_alternative<bool>(*useUpFromTargetOrDuration)) {
            insDict.setValue(
                "UseTargetUpDirection",
                std::get<bool>(*useUpFromTargetOrDuration)
            );
        }
        else {
            double d = std::get<double>(*useUpFromTargetOrDuration);
            if (d <= Epsilon) {
                return ghoul::lua::luaError(L, "Duration must be larger than zero");
            }
            insDict.setValue("Duration", d);
        }
    }
    if (duration.has_value()) {
        double d = *duration;
        if (d <= Epsilon) {
            return ghoul::lua::luaError(L, "Duration must be larger than zero");
        }
        insDict.setValue("Duration", d);
    }

    global::navigationHandler->pathNavigator().createPath(insDict);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
    return 0;
}

int goToHeight(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 2, 4 }, "lua::goToHeight");
    auto [nodeIdentifier, height, useUpFromTargetOrDuration, duration] =
        ghoul::lua::values<
            std::string, double, std::optional<std::variant<bool, double>>,
            std::optional<double>
        >(L);


    if (!sceneGraphNode(nodeIdentifier)) {
        return ghoul::lua::luaError(L, "Unknown node name: " + nodeIdentifier);
    }

    using namespace std::string_literals;
    ghoul::Dictionary insDict;
    insDict.setValue("TargetType", "Node"s);
    insDict.setValue("Target", nodeIdentifier);
    insDict.setValue("Height", height);
    if (useUpFromTargetOrDuration.has_value()) {
        if (std::holds_alternative<bool>(*useUpFromTargetOrDuration)) {
            insDict.setValue(
                "UseTargetUpDirection",
                std::get<bool>(*useUpFromTargetOrDuration)
            );
        }
        else {
            double d = std::get<double>(*useUpFromTargetOrDuration);
            if (d <= Epsilon) {
                return ghoul::lua::luaError(L, "Duration must be larger than zero");
            }
            insDict.setValue("Duration", d);
        }
    }
    if (duration.has_value()) {
        double d = *duration;
        if (d <= Epsilon) {
            return ghoul::lua::luaError(L, "Duration must be larger than zero");
        }
        insDict.setValue("Duration", d);
    }

    global::navigationHandler->pathNavigator().createPath(insDict);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }

    return 0;
}

int goToNavigationState(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::goToNavigationState");
    auto [navigationState, duration] =
        ghoul::lua::values<ghoul::Dictionary, std::optional<double>>(L);

    try {
        openspace::documentation::testSpecificationAndThrow(
            interaction::NavigationState::Documentation(),
            navigationState,
            "NavigationState"
        );
    }
    catch (documentation::SpecificationError& e) {
        LERRORC("goToNavigationState", ghoul::to_string(e.result));
        return ghoul::lua::luaError(
            L, fmt::format("Unable to create a path: {}", e.what())
        );
    }

    using namespace std::string_literals;
    ghoul::Dictionary instruction;
    instruction.setValue("TargetType", "NavigationState"s);
    instruction.setValue("NavigationState", navigationState);

    if (duration.has_value()) {
        double d = *duration;
        if (d <= Epsilon) {
            return ghoul::lua::luaError(L, "Duration must be larger than zero");
        }
        instruction.setValue("Duration", d);
    }

    global::navigationHandler->pathNavigator().createPath(instruction);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
    return 0;
}

int createPath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::createPath");
    ghoul::Dictionary dictionary = ghoul::lua::value<ghoul::Dictionary>(L);

    global::navigationHandler->pathNavigator().createPath(dictionary);
    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
    return 0;
}

} // namespace openspace::luascriptfunctions
