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

#include <openspace/properties/vector/uvec2property.h>

#include <openspace/util/json_helper.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>

namespace openspace::properties {

UVec2Property::UVec2Property(Property::PropertyInfo info, glm::uvec2 value,
                             glm::uvec2 minValue, glm::uvec2 maxValue,
                             glm::uvec2 stepValue)
    : NumericalProperty<glm::uvec2>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue)
    )
{}

std::string_view UVec2Property::className() const {
    return "UVec2Property";
}

ghoul::lua::LuaTypes UVec2Property::typeLua() const {
    return ghoul::lua::LuaTypes::Table;
}

void UVec2Property::getLuaValue(lua_State* state) const {
    ghoul::lua::push(state, _value);
}

glm::uvec2 UVec2Property::toValue(lua_State* state) const {
    return ghoul::lua::value<glm::uvec2>(state);
}

std::string UVec2Property::stringValue() const {
    return formatJson(_value);
}

} // namespace openspace::properties
