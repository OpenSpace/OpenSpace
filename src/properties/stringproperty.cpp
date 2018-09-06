/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/properties/stringproperty.h>

#include <ghoul/lua/ghoul_lua.h>

namespace {

std::string fromLuaConversion(lua_State* state, bool& success) {
    success = lua_isstring(state, -1) == 1;
    if (success) {
        return lua_tostring(state, -1);
    }
    else {
        return "";
    }
}

bool toLuaConversion(lua_State* state, const std::string& val) {
    lua_pushstring(state, val.c_str());
    return true;
}

std::string fromStringConversion(std::string val, bool& success) {
    // An incoming string is of the form
    // "value"
    // so we want to remove the leading and trailing " characters
    if (val.size() > 2 && (val[0] == '"' && val[val.size() - 1] == '"')) {
        // Removing the first and last "
        success = true;
        return val.substr(1, val.size() - 2);
    }
    success = false;
    return val;
}

bool toStringConversion(std::string& outValue, std::string inValue) {
    outValue = "\"" + std::move(inValue ) + "\"";
    return true;
}

} // namespace

namespace openspace::properties {

REGISTER_TEMPLATEPROPERTY_SOURCE(
    StringProperty,
    std::string,
    "",
    fromLuaConversion,
    toLuaConversion,
    fromStringConversion,
    toStringConversion,
    LUA_TSTRING
)

} // namespace openspace::properties
