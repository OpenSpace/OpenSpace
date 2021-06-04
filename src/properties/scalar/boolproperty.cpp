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

#include <openspace/properties/scalar/boolproperty.h>

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>

namespace openspace::properties {

BoolProperty::BoolProperty(Property::PropertyInfo info, bool value)
    : TemplateProperty<bool>(std::move(info), value)
{}

std::string BoolProperty::className() const {
    return "BoolProperty";
}

int BoolProperty::typeLua() const {
    return LUA_TBOOLEAN;
}

bool BoolProperty::fromLuaConversion(lua_State* state, bool& success) const {
    success = (lua_isboolean(state, -1) == 1);
    return success ? (lua_toboolean(state, -1) == 1) : false;
}

void BoolProperty::toLuaConversion(lua_State* state) const {
    ghoul::lua::push(state, _value);
}

std::string BoolProperty::toStringConversion() const {
    return _value ? "true" : "false";
}

} // namespace openspace::properties
