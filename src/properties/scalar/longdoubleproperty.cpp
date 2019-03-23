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

#include <openspace/properties/scalar/longdoubleproperty.h>

#include <ghoul/lua/ghoul_lua.h>

#include <limits>
#include <sstream>

namespace {

long double fromLuaConversion(lua_State* state, bool& success) {
    success = (lua_isnumber(state, -1) == 1);
    if (success) {
        long double val = static_cast<long double>(lua_tonumber(state, -1));
        lua_pop(state, 1);
        return val;
    }
    else {
        return 0l;
    }
}

bool toLuaConversion(lua_State* state, long double value) {
    lua_pushnumber(state, static_cast<lua_Number>(value));
    return true;
}

long double fromStringConversion(const std::string& val, bool& success) {
    std::stringstream s(val);
    long double v;
    s >> v;
    success = !s.fail();
    if (success) {
        return v;
    }
    else {
        throw ghoul::RuntimeError("Conversion error for string: " + val);
    }
}

bool toStringConversion(std::string& outValue, long double inValue) {
    outValue = std::to_string(inValue);
    return true;
}

} // namespace

namespace openspace::properties {

REGISTER_NUMERICALPROPERTY_SOURCE(
    LongDoubleProperty,
    long double,
    0l,
    std::numeric_limits<long double>::lowest(),
    std::numeric_limits<long double>::max(),
    0.01l,
    fromLuaConversion,
    toLuaConversion,
    fromStringConversion,
    toStringConversion,
    LUA_TNUMBER
)

} // namespace openspace::properties
