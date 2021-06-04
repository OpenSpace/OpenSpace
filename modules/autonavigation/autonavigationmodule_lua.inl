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

#include <modules/autonavigation/instruction.h>
#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>
#include <openspace/query/query.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/vector_angle.hpp>

namespace {
    constexpr const char* _loggerCat = "AutoNavigation";
} // namespace

namespace openspace::autonavigation::luascriptfunctions {

const double Epsilon = 1e-12;

int isFlying(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::isFlying");

    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    bool hasFinished = module->AutoNavigationHandler().hasFinished();

    ghoul::lua::push(L, !hasFinished);
    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

//int continuePath(lua_State* L) {
//    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::continuePath");
//
//    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
//    module->AutoNavigationHandler().continuePath();
//
//    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
//    return 0;
//}

int stopPath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::stopPath");

    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    module->AutoNavigationHandler().abortPath();

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
    insDict.setValue("Type", "Node"s);
    insDict.setValue("Target", nodeIdentifier);

    if (nArguments > 1) {
        int result = handleOptionalGoToParameters(L, 2, nArguments, insDict);
        if (result != 0) {
            return result; // An error occurred
        }
    }

    Instruction instruction(insDict);

    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    module->AutoNavigationHandler().createPath(instruction);

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
    insDict.setValue("Type", "Node"s);
    insDict.setValue("Target", nodeIdentifier);
    insDict.setValue("Height", height);

    if (nArguments > 2) {
        int result = handleOptionalGoToParameters(L, 3, nArguments, insDict);
        if (result != 0) {
            return result; // An error occurred
        }
    }

    Instruction instruction(insDict);

    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    module->AutoNavigationHandler().createPath(instruction);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

// @TODO (emmbr 2020-11-06) Ideally, this module shouldn't depend on things from
// Globebrowsing, but we want it for an istallation. Later on, move this functionality
// somewhere else. Maybe combine with the existing "goToGeo" in globebrowsing?
int goToGeo(lua_State* L) {
    int nArguments = ghoul::lua::checkArgumentsAndThrow(L, { 4, 6 }, "lua::goToGeo");

    const std::string& nodeIdentifier = ghoul::lua::value<std::string>(L, 1);
    const SceneGraphNode* n = sceneGraphNode(nodeIdentifier);
    if (!n) {
        lua_settop(L, 0);
        return ghoul::lua::luaError(L, "Unknown globe name: " + nodeIdentifier);
    }

    const double latitude = ghoul::lua::value<double>(L, 2);
    const double longitude = ghoul::lua::value<double>(L, 3);
    const double altitude = ghoul::lua::value<double>(L, 4);

    using RenderableGlobe = openspace::globebrowsing::RenderableGlobe;
    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Identifier must be a RenderableGlobe");
    }

    // Compute the relative position based on the input values
    glm::dvec3 positionModelCoords = global::moduleEngine->module<GlobeBrowsingModule>()
        ->cartesianCoordinatesFromGeo(
        *globe,
        latitude,
        longitude,
        altitude
    );

    using namespace std::string_literals;
    ghoul::Dictionary insDict;
    insDict.setValue("Type", "Node"s);
    insDict.setValue("Target", nodeIdentifier);
    insDict.setValue("Position", positionModelCoords);

    if (nArguments > 4) {
        int result = handleOptionalGoToParameters(L, 5, nArguments, insDict);
        if (result != 0) {
            return result; // An error occurred
        }
    }

    Instruction instruction(insDict);

    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    module->AutoNavigationHandler().createPath(instruction);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int generatePath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::generatePath");

    ghoul::Dictionary dictionary;
    ghoul::lua::luaDictionaryFromState(L, dictionary);
    Instruction instruction(dictionary);

    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    module->AutoNavigationHandler().createPath(instruction);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

// TODO: remove when not needed
// Created for debugging. Access info for rendereable path
int getPathPositions(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::getPathPositions");

    const int pointsPerSegment = (int)ghoul::lua::value<lua_Number>(L, 1);

    // Get sample positions from the current curve
    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    AutoNavigationHandler& handler = module->AutoNavigationHandler();
    std::vector<glm::dvec3> points = handler.curvePositions(pointsPerSegment);

    // Push the points to the Lua stack:
    lua_settop(L, 0);
    const auto pushVector = [](lua_State* L, const glm::dvec3& v) {
        lua_newtable(L);
        ghoul::lua::push(L, 1, v.x);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 2, v.y);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 3, v.z);
        lua_rawset(L, -3);
    };

    lua_newtable(L);
    for (int i = 0; i < points.size(); ++i) {
        ghoul::lua::push(L, i+1);
        pushVector(L, points[i]);
        lua_rawset(L, -3);
    }

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

// TODO: remove when not needed
// Created for debugging. Access info for rendereable path
int getPathOrientations(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::getPathPositions");

    const int pointsPerSegment = (int)ghoul::lua::value<lua_Number>(L, 1);

    // Get sample positions from the current curve
    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    AutoNavigationHandler& handler = module->AutoNavigationHandler();
    std::vector<glm::dquat> orientations = handler.curveOrientations(pointsPerSegment);

    // Push the rotation to the Lua stack:
    lua_settop(L, 0);
    const auto pushVector = [](lua_State* L, const glm::dquat& v) {
        lua_newtable(L);
        ghoul::lua::push(L, 1, v.w);
        lua_rawset(L, -4);
        ghoul::lua::push(L, 2, v.x);
        lua_rawset(L, -4);
        ghoul::lua::push(L, 3, v.y);
        lua_rawset(L, -4);
        ghoul::lua::push(L, 4, v.z);
        lua_rawset(L, -4);
    };

    lua_newtable(L);
    for (int i = 0; i < orientations.size(); ++i) {
        ghoul::lua::push(L, i + 1);
        pushVector(L, orientations[i]);
        lua_rawset(L, -4);
    }

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

// TODO: remove when not needed
// Created for debugging. Access info for rendereable path
int getPathViewDirections(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::getPathViewDirections");

    const int pointsPerSegment = (int)ghoul::lua::value<lua_Number>(L, 1);

    // Get sample positions from the current curve
    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    AutoNavigationHandler& handler = module->AutoNavigationHandler();
    std::vector<glm::dvec3> viewDirections = handler.curveViewDirections(pointsPerSegment);

    // Push the points to the Lua stack:
    lua_settop(L, 0);
    const auto pushVector = [](lua_State* L, const glm::dvec3& v) {
        lua_newtable(L);
        ghoul::lua::push(L, 1, v.x);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 2, v.y);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 3, v.z);
        lua_rawset(L, -3);
    };

    // Push the rotation to the Lua stack:
    lua_newtable(L);
    for (int i = 0; i < viewDirections.size(); ++i) {
        ghoul::lua::push(L, i + 1);
        pushVector(L, viewDirections[i]);
        lua_rawset(L, -3);
    }

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

// TODO: remove when not needed
// Created for debugging. Access info for rendering of control points
int getControlPoints(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getControlPoints");

    // Get sample positions from the current curve
    AutoNavigationModule* module = global::moduleEngine->module<AutoNavigationModule>();
    AutoNavigationHandler& handler = module->AutoNavigationHandler();
    std::vector<glm::dvec3> points = handler.controlPoints();

    // Push the points to the Lua stack:
    lua_settop(L, 0);
    const auto pushVector = [](lua_State* L, const glm::dvec3& v) {
        lua_newtable(L);
        ghoul::lua::push(L, 1, v.x);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 2, v.y);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 3, v.z);
        lua_rawset(L, -3);
    };

    lua_newtable(L);
    for (int i = 0; i < points.size(); ++i) {
        ghoul::lua::push(L, i+1);
        pushVector(L, points[i]);
        lua_rawset(L, -3);
    }

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

} // namespace openspace::autonavigation::luascriptfunctions
