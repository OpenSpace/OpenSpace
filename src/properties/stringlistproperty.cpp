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

#include <openspace/properties/stringlistproperty.h>

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/misc.h>
#include <numeric>

namespace {

std::vector<std::string> fromLuaConversion(lua_State* state, bool& success) {
    if (!lua_istable(state, -1)) {
        success = false;
        return {};
    }

    std::vector<std::string> result;
    lua_pushnil(state);
    while (lua_next(state, -2) != 0) {
        if (lua_isstring(state, -1)) {
            result.emplace_back(lua_tostring(state, -1));
        }
        else {
            success = false;
            return {};
        }
        lua_pop(state, 1);
    }
    success = true;
    return result;
}

bool toLuaConversion(lua_State* state, std::vector<std::string> val) {
    lua_createtable(state, static_cast<int>(val.size()), 0);

    int i = 1;
    for (std::string& v : val) {
        lua_pushnumber(state, i);
        lua_pushstring(state, v.c_str());
        lua_settable(state, -3);
        ++i;
    }

    return true;
}

std::vector<std::string> fromStringConversion(const std::string& val, bool& success) {
    std::vector<std::string> tokens = ghoul::tokenizeString(val, ',');
    for (std::string& token : tokens) {
        // Each incoming string is of the form "value"
        // so we want to remove the leading and trailing " characters
        if (token.size() > 2 && (token[0] == '"' && token[token.size() - 1] == '"')) {
            token = token.substr(1, token.size() - 2);
        }
    }

    success = true;
    return tokens;
}

bool toStringConversion(std::string& outValue, const std::vector<std::string>& inValue) {
    outValue = "";
    for (const std::string& v : inValue) {
        if (&v != &*inValue.cbegin()) {
            outValue += ", " + v;
        }
        else {
            outValue += v;
        }
    }

    // outValue = std::accumulate(
    //     inValue.begin(),
    //     inValue.end(),
    //     std::string(""),
    //     [](std::string lhs, std::string rhs) {
    //         return lhs + "," + "\"" + rhs + "\"";
    //     }
    // );
    return true;
}

} // namespace

namespace openspace::properties {

REGISTER_TEMPLATEPROPERTY_SOURCE(
    StringListProperty,
    std::vector<std::string>,
    std::vector<std::string>(),
    fromLuaConversion,
    toLuaConversion,
    fromStringConversion,
    toStringConversion,
    LUA_TTABLE
)

} // namespace openspace::properties
