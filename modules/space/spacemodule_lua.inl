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

#include <openspace/util/coordinateconversion.h>

namespace openspace::space::luascriptfunctions {

int convertFromRaDec(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::convertFromRaDec");

    glm::dvec2 degrees = glm::dvec2(0.0);
    if (ghoul::lua::hasValue<std::string>(L, 1) &&
        ghoul::lua::hasValue<std::string>(L, 2))
    {
        auto [ra, dec] = ghoul::lua::values<std::string, std::string>(L);
        degrees = icrsToDecimalDegrees(ra, dec);
    }
    else if (ghoul::lua::hasValue<double>(L, 1) && ghoul::lua::hasValue<double>(L, 2)) {
        auto [x, y] = ghoul::lua::values<double, double>(L);
        degrees.x = x;
        degrees.y = y;
    }
    else {
        throw ghoul::lua::LuaRuntimeException(
            "Ra and Dec have to be of the same type, either String or Number"
        );
    }

    double distance = ghoul::lua::value<double>(L);
    glm::dvec3 pos = icrsToGalacticCartesian(degrees.x, degrees.y, distance);
    ghoul::lua::push(L, pos);
    return 1;
}

int convertToRaDec(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::convertToRaDec");
    auto [x, y, z] = ghoul::lua::values<double, double, double>(L);

    glm::dvec3 deg = galacticCartesianToIcrs(x, y, z);
    std::pair<std::string, std::string> raDecPair = decimalDegreesToIcrs(deg.x, deg.y);

    // Ra, Dec, Distance
    ghoul::lua::push(L, raDecPair.first, raDecPair.second, deg.z);
    return 3;
}

}  // namespace openspace::space::luascriptfunctions
