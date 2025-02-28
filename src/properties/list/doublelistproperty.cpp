/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/properties/list/doublelistproperty.h>

#include <openspace/json.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/stringhelper.h>

namespace openspace::properties {

DoubleListProperty::DoubleListProperty(Property::PropertyInfo info,
                                       std::vector<double> values)
    : ListProperty(std::move(info), std::move(values))
{}

std::string_view DoubleListProperty::className() const {
    return "DoubleListProperty";
}

ghoul::lua::LuaTypes DoubleListProperty::typeLua() const {
    return ghoul::lua::LuaTypes::Table;
}

void DoubleListProperty::getLuaValue(lua_State* state) const {
    ghoul::lua::push(state, _value);
}

std::vector<double> DoubleListProperty::toValue(lua_State* state) const {
    return ghoul::lua::value<std::vector<double>>(state);
}

std::string DoubleListProperty::stringValue() const {
    const nlohmann::json json(_value);
    return json.dump();
}

} // namespace openspace::properties
