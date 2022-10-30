/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/properties/list/stringlistproperty.h>

#include <openspace/json.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/misc.h>

namespace openspace::properties {

StringListProperty::StringListProperty(Property::PropertyInfo info,
                                       std::vector<std::string> values)
    : ListProperty(std::move(info), std::move(values))
{}

std::string_view StringListProperty::className() const {
    return "StringListProperty";
}

int StringListProperty::typeLua() const {
    return LUA_TTABLE;
}

std::vector<std::string> StringListProperty::fromLuaConversion(lua_State* state,
                                                               bool& success) const
{
    if (!lua_istable(state, -1)) {
        success = false;
        LERRORC(className(), "Conversion from Lua failed. The input was not a table");
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

void StringListProperty::toLuaConversion(lua_State* state) const {
    lua_createtable(state, static_cast<int>(_value.size()), 0);

    int i = 1;
    for (const std::string& v : _value) {
        ghoul::lua::push(state, i);
        ghoul::lua::push(state, v.c_str());
        lua_settable(state, -3);
        ++i;
    }
}

std::string StringListProperty::toStringConversion() const {
    nlohmann::json json(_value);
    return json.dump();
}

} // namespace openspace::properties
