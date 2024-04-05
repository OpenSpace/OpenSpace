/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/lua/lua_helper.h>
#include <openspace/json.h>

namespace openspace::properties {

StringProperty::StringProperty(Property::PropertyInfo info, std::string value)
    : TemplateProperty<std::string>(std::move(info), std::move(value))
{}

std::string_view StringProperty::className() const {
    return "StringProperty";
}

int StringProperty::typeLua() const {
    return LUA_TSTRING;
}

std::string StringProperty::fromLuaConversion(lua_State* state) const {
    return ghoul::lua::value<std::string>(state);
}

void StringProperty::toLuaConversion(lua_State* state) const {
    ghoul::lua::push(state, _value);
}

std::string StringProperty::toStringConversion() const {
    nlohmann::json json;
    nlohmann::to_json(json, _value);
    return json.dump();
}

StringProperty::operator std::string_view() {
    return _value;
}

StringProperty::operator std::string_view() const {
    return _value;
}

} // namespace openspace::properties
