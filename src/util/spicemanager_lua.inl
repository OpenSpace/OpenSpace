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

#include <ghoul/filesystem/filesystem.h>
#include <openspace/engine/globals.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>

namespace openspace::luascriptfunctions {

/**
 * \ingroup LuaScripts
 * loadKernel(string):
 * Loads the provided SPICE kernel by name. The name can contain path tokens, which are
 * automatically resolved.
 */

int loadKernel(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadKernel");
    std::string argument = ghoul::lua::value<std::string>(L);

    if (!std::filesystem::is_regular_file(argument)) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Kernel file '{}' did not exist", argument)
        );
    }
    unsigned int result = SpiceManager::ref().loadKernel(argument);
    ghoul::lua::push(L, result);
    return 1;
}

/**
 * unloadKernel({string, number}):
 * Unloads the provided SPICE kernel. The name can contain path tokens, which are
 * automatically resolved.
 */
int unloadKernel(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::unloadKernel");
    std::variant<std::string, unsigned int> argument = 
        ghoul::lua::value<std::variant<std::string, unsigned int>>(L);

    if (std::holds_alternative<std::string>(argument)) {
        SpiceManager::ref().unloadKernel(std::get<std::string>(argument));
    }
    else {
        SpiceManager::ref().unloadKernel(std::get<unsigned int>(argument));
    }
    return 0;
}

/**
 * spiceBodies():
 * Returns the list of bodies loaded into the spicemanager
 */
int spiceBodies(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::spiceBodies");
    const bool buildInBodies = ghoul::lua::value<bool>(L);

    std::vector<std::pair<int, std::string>> bodies = SpiceManager::ref().spiceBodies(
        buildInBodies
    );

    lua_newtable(L);
    int number = 1;
    for (const std::pair<int, std::string>& body : bodies) {
        lua_newtable(L);
        ghoul::lua::push(L, 1, body.first);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 2, body.second);
        lua_rawset(L, -3);
        lua_rawseti(L, -2, number);
        ++number;
    }
    return 1;
}

// internal function for getSpk and getCk coverages
void buildLuaCoverageStack(lua_State* L,
                           const std::vector<std::pair<double, double>>& coverage)
{
    lua_newtable(L);
    int number = 1;
    for (const std::pair<double, double>& window : coverage) {
        std::string start = SpiceManager::ref().dateFromEphemerisTime(window.first);
        std::string end = SpiceManager::ref().dateFromEphemerisTime(window.second);

        lua_newtable(L);
        ghoul::lua::push(L, 1, start);
        lua_rawset(L, -3);
        ghoul::lua::push(L, 2, end);
        lua_rawset(L, -3);
        lua_rawseti(L, -2, number);
        ++number;
    }
}

/**
 * spkCoverage(string):
 * Returns the spk coverage for given body
 */
int spkCoverage(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::spkCoverage");
    std::string argument = ghoul::lua::value<std::string>(L);

    buildLuaCoverageStack(L, SpiceManager::ref().spkCoverage(argument));
    return 1;
}

/**
 * ckCoverage(string):
 * Returns the spk coverage for given body
 */
int ckCoverage(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::ckCoverage");
    std::string argument = ghoul::lua::value<std::string>(L);

    buildLuaCoverageStack(L, SpiceManager::ref().ckCoverage(argument));
    return 1;
}

/**
 * rotationMatrix({string, string, string}):
 * Returns the rotationMatrix for a given body in a frame of reference at a specific time
 */
int rotationMatrix(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::rotationMatrix");
    auto [body, frame, date] =
        ghoul::lua::values<std::string, std::string, std::string>(L);

    const double ephemerisTime = SpiceManager::ref().ephemerisTimeFromDate(date);
    glm::dmat3 rotationMatrix = SpiceManager::ref().frameTransformationMatrix(
        body,
        frame,
        ephemerisTime
    );
    ghoul::lua::push(L, 1, rotationMatrix);
    return 1;
}

/**
 * position({string, string, string, string}):
 * Returns the position for a given body relative to another body, 
 * in a given frame of reference, at a specific time.
 */
int position(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 4, "lua::position");
    auto [target, observer, frame, date] =
        ghoul::lua::values<std::string, std::string, std::string, std::string>(L);

    const double ephemerisTime = SpiceManager::ref().ephemerisTimeFromDate(date);
    glm::dvec3 postion = SpiceManager::ref().targetPosition(
        target,
        observer,
        frame,
        {},
        ephemerisTime
    );
    ghoul::lua::push(L, 1, postion);

    return 1;
}

} // namespace openspace::luascriptfunctions
