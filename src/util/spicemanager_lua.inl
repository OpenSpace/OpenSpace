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

    bool isString = (lua_isstring(L, 1) == 1);
    if (!isString) {
        LERROR(fmt::format(
            "{}: Expected argument of type 'string'", ghoul::lua::errorLocation(L)
        ));
        return 0;
    }

    std::string argument = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );
    if (!std::filesystem::is_regular_file(argument)) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Kernel file '{}' did not exist", argument)
        );
    }
    unsigned int result = SpiceManager::ref().loadKernel(argument);

    lua_pushnumber(L, result);

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
 * unloadKernel({string, number}):
 * Unloads the provided SPICE kernel. The name can contain path tokens, which are
 * automatically resolved.
 */
int unloadKernel(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadKernel");

    bool isString = (lua_isstring(L, 1) == 1);
    bool isNumber = (lua_isnumber(L, 1) == 1);

    if (!isString && !isNumber) {
        LERRORC(
            "loadKernel",
            fmt::format(
                "{}: Expected argument of type 'string' or 'number'",
                ghoul::lua::errorLocation(L)
            )
        );
        lua_settop(L, 0);
        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
        return 0;
    }

    if (isString) {
        std::string argument = ghoul::lua::value<std::string>(L, 1);
        SpiceManager::ref().unloadKernel(argument);
    }

    if (isNumber) {
        unsigned int argument = ghoul::lua::value<unsigned int>(L, 1);
        SpiceManager::ref().unloadKernel(argument);
    }

    lua_settop(L, 0);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * spiceBodies():
 * Returns the list of bodies loaded into the spicemanager
 */
int spiceBodies(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1,2 }, "lua::getSpiceBodies");
    bool isBool = (lua_isboolean(L, 1) == 1);
    const bool buildInBodies = isBool ? ghoul::lua::value<bool>(L, 1) : false;

    isBool = (lua_isboolean(L, 2) == 1);
    bool printValues = isBool ? ghoul::lua::value<bool>(L, 2) : false;
    lua_settop(L, 0);
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

        if (printValues) {
            LINFO(fmt::format("Body id '{}' and name: {}", body.first, body.second));
        }
    }

    return 1;
}

//internal function for getSpk and getCk coverages
void buildLuaCoverageStack(lua_State* L,
                           const std::vector<std::pair<double, double>>& coverage,
                           bool printValues)
{
    lua_settop(L, 0);
    lua_newtable(L);
    int number = 1;
    for (const std::pair<double, double>& window : coverage) {
        std::string start = SpiceManager::ref().dateFromEphemerisTime(window.first);
        std::string end = SpiceManager::ref().dateFromEphemerisTime(window.second);

        if (printValues) {
            LINFO(fmt::format(
                "Coverage start {} and end: {}",
                SpiceManager::ref().dateFromEphemerisTime(window.first),
                SpiceManager::ref().dateFromEphemerisTime(window.second)
            ));
        }

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
 * getSpkCoverage({string, bool(optional)}):
 * Returns the spk coverage for given body
 */
int spkCoverage(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::getSpkCoverage");
    const bool isString = (lua_isstring(L, 1) == 1);
    const bool isBool = (lua_isboolean(L, 2) == 1);

    if (!isString) {
        LERRORC(
            "getSpkCoverage",
            fmt::format(
                "{}: Expected argument of type 'string'",
                ghoul::lua::errorLocation(L)
            )
        );
        lua_settop(L, 0);
        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
        return 0;
    }

    std::string argument = ghoul::lua::value<std::string>(L, 1);

    bool printValues = isBool ? ghoul::lua::value<bool>(L, 2) : false;
    buildLuaCoverageStack(L, SpiceManager::ref().spkCoverage(argument), printValues);

    return 1;
}

/**
 * getCkCoverage({string, bool(optional)}):
 * Returns the spk coverage for given body
 */
int ckCoverage(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::getCkCoverage");

    const bool isString = (lua_isstring(L, 1) == 1);
    const bool isBool = (lua_isboolean(L, 2) == 1);

    if (!isString) {
        LERRORC(
            "getCkCoverage",
            fmt::format(
                "{}: Expected argument of type 'string'",
                ghoul::lua::errorLocation(L)
            )
        );
        lua_settop(L, 0);
        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
        return 0;
    }

    std::string argument = ghoul::lua::value<std::string>(L, 1);
    bool printValues = isBool ? ghoul::lua::value<bool>(L, 2) : false;
    buildLuaCoverageStack(L, SpiceManager::ref().ckCoverage(argument), printValues);

    return 1;
}

} // namespace openspace::luascriptfunctions
