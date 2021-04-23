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

#include <openspace/properties/vector/uvec4property.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

UVec4Property::UVec4Property(Property::PropertyInfo info, glm::uvec4 value,
                             glm::uvec4 minValue, glm::uvec4 maxValue,
                             glm::uvec4 stepValue)
    : NumericalProperty<glm::uvec4>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue)
    )
{}

UVec4Property::UVec4Property(Property::PropertyInfo info, glm::uvec4 value,
                             glm::uvec4 minValue, glm::uvec4 maxValue,
                             glm::uvec4 stepValue, float exponent)
    : NumericalProperty<glm::uvec4>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue),
        exponent
    )
{}

std::string UVec4Property::className() const {
    return "UVec4Property";
}

int UVec4Property::typeLua() const {
    return LUA_TTABLE;
}

glm::uvec4 UVec4Property::fromLuaConversion(lua_State* state, bool& success) const {
    glm::uvec4 result = glm::uvec4(0);
    lua_pushnil(state);
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::uvec4>::value; ++i) {
        int hasNext = lua_next(state, -2);
        if (hasNext != 1) {
            success = false;
            return glm::uvec4(0);
        }
        if (lua_isnumber(state, -1) != 1) {
            success = false;
            return glm::uvec4(0);
        }
        else {
            result[i] = static_cast<glm::uvec4::value_type>(lua_tonumber(state, -1));
            lua_pop(state, 1);
        }
    }

    // The last accessor argument is still on the stack
    lua_pop(state, 1);
    success = true;
    return result;
}

bool UVec4Property::toLuaConversion(lua_State* state) const {
    lua_newtable(state);
    int number = 1;
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::uvec4>::value; ++i) {
        lua_pushnumber(state, static_cast<lua_Number>(_value[i]));
        lua_rawseti(state, -2, number);
        ++number;
    }
    return true;
}

bool UVec4Property::toStringConversion(std::string& outValue) const {
    outValue = "{";
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::uvec4>::value; ++i) {
        outValue += std::to_string(_value[i]) + ",";
    }
    outValue.pop_back();
    outValue += "}";
    return true;
}

} // namespace openspace::properties
