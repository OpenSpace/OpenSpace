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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/easing.h>
#include <openspace/util/coordinateconversion.h>

namespace openspace::space::luascriptfunctions {

int convertFromRaDec(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 3, 4 }, "lua::convertFromRaDec");

    glm::dvec2 degrees = glm::dvec2(0.0);
    bool isDegrees = true;
    if (lua_type(L, 1) == LUA_TSTRING && lua_type(L, 2) == LUA_TSTRING) {
        std::string s_ra = ghoul::lua::value<std::string>(L, 1);
        std::string s_dec = ghoul::lua::value<std::string>(L, 2);
        degrees = icrsToDecimalDegrees(s_ra, s_dec);
    }
    else if (lua_type(L, 1) == LUA_TNUMBER && lua_type(L, 2) == LUA_TNUMBER) {
        degrees.x = ghoul::lua::value<double>(L, 1);
        degrees.y = ghoul::lua::value<double>(L, 2);
        if (lua_gettop(L) >= 4) {
            isDegrees = ghoul::lua::value<bool>(L, 4);
        }
    }
    else {
        throw ghoul::lua::LuaRuntimeException("lua::convertFromRaDec: Ra and Dec have to "
            "be of the same type, either String or Number"
        );
    }

    double distance = ghoul::lua::value<double>(L, 3);
    lua_settop(L, 0);

    glm::dvec3 pos = icrsToGalacticCartesian(degrees.x, degrees.y, distance, isDegrees);
    ghoul::lua::push(L, pos);

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int convertToRaDec(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::convertToRaDec");

    double x = ghoul::lua::value<double>(L, 1);
    double y = ghoul::lua::value<double>(L, 2);
    double z = ghoul::lua::value<double>(L, 3);
    lua_settop(L, 0);

    glm::dvec3 degrees = galacticCartesianToIcrs(x, y, z);
    std::pair<std::string, std::string> raDecPair
        = decimalDegreesToIcrs(degrees.x, degrees.y);

    ghoul::lua::push(L, raDecPair.first);  // Ra
    ghoul::lua::push(L, raDecPair.second); // Dec
    ghoul::lua::push(L, degrees.z);        // Distance

    ghoul_assert(lua_gettop(L) == 3, "Incorrect number of items left on stack");
    return 3;
}

}  // namespace openspace::space::luascriptfunctions
