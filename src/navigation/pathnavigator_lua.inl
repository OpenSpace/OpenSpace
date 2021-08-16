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
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int continuePath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::continuePath");

    global::navigationHandler->pathNavigator().continuePath();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int pausePath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::pausePath");

    global::navigationHandler->pathNavigator().pausePath();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int stopPath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::stopPath");

    global::navigationHandler->pathNavigator().abortPath();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

// All the goTo function has the same two optional input parameters at the end. The
// purpose of this function is to handle these input parameters and add the result
// to the dictionary specifying the instruction for a camera path.
int handleOptionalGoToParameters(lua_State* L, const int startLocation,
                                 const int nArguments,
                                 ghoul::Dictionary& resultInstruction)
{
    const bool firstIsNumber = (lua_isnumber(L, startLocation) != 0);
    const bool firstIsBool = (lua_isboolean(L, startLocation) != 0);

    if (!(firstIsNumber || firstIsBool)) {
        const char* msg = lua_pushfstring(
            L,
            "%s or %s expected, got %s",
            lua_typename(L, LUA_TNUMBER),
            lua_typename(L, LUA_TBOOLEAN),
            luaL_typename(L, -1)
        );
        return ghoul::lua::luaError(
            L, fmt::format("bad argument #{} ({})", startLocation, msg)
        );
    }

    int location = startLocation;

    if (firstIsBool) {
        const bool useUpFromTarget = (lua_toboolean(L, location) == 1);
        resultInstruction.setValue("UseTargetUpDirection", useUpFromTarget);

        if (nArguments > startLocation) {
            location++;
        }
    }

    if (firstIsNumber || nArguments > startLocation) {
        double duration = ghoul::lua::value<double>(L, location);
        if (duration <= Epsilon) {
            lua_settop(L, 0);
            return ghoul::lua::luaError(L, "Duration must be larger than zero.");
        }
        resultInstruction.setValue("Duration", duration);
    }

    return 0;
}

int goTo(lua_State* L) {
    int nArguments = ghoul::lua::checkArgumentsAndThrow(L, { 1, 3 }, "lua::goTo");

    const std::string& nodeIdentifier = ghoul::lua::value<std::string>(L, 1);

    if (!sceneGraphNode(nodeIdentifier)) {
        lua_settop(L, 0);
        return ghoul::lua::luaError(L, "Unknown node name: " + nodeIdentifier);
    }

    using namespace std::string_literals;
    ghoul::Dictionary insDict;
    insDict.setValue("TargetType", "Node"s);
    insDict.setValue("Target", nodeIdentifier);

    if (nArguments > 1) {
        int result = handleOptionalGoToParameters(L, 2, nArguments, insDict);
        if (result != 0) {
            return result; // An error occurred
        }
    }

    global::navigationHandler->pathNavigator().createPath(insDict);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int goToHeight(lua_State* L) {
    int nArguments = ghoul::lua::checkArgumentsAndThrow(L, { 2, 4 }, "lua::goToHeight");

    const std::string& nodeIdentifier = ghoul::lua::value<std::string>(L, 1);

    if (!sceneGraphNode(nodeIdentifier)) {
        lua_settop(L, 0);
        return ghoul::lua::luaError(L, "Unknown node name: " + nodeIdentifier);
    }

    double height = ghoul::lua::value<double>(L, 2);

    using namespace std::string_literals;
    ghoul::Dictionary insDict;
    insDict.setValue("TargetType", "Node"s);
    insDict.setValue("Target", nodeIdentifier);
    insDict.setValue("Height", height);

    if (nArguments > 2) {
        int result = handleOptionalGoToParameters(L, 3, nArguments, insDict);
        if (result != 0) {
            return result; // An error occurred
        }
    }

    global::navigationHandler->pathNavigator().createPath(insDict);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int generatePath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::generatePath");

    ghoul::Dictionary dictionary;
    ghoul::lua::luaDictionaryFromState(L, dictionary);

    global::navigationHandler->pathNavigator().createPath(dictionary);

    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions
