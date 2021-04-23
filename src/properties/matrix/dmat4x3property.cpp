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

#include <openspace/properties/matrix/dmat4x3property.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

DMat4x3Property::DMat4x3Property(Property::PropertyInfo info, glm::dmat4x3 value,
                                 glm::dmat4x3 minValue, glm::dmat4x3 maxValue,
                                 glm::dmat4x3 stepValue)
    : NumericalProperty<glm::dmat4x3>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue)
    )
{}

DMat4x3Property::DMat4x3Property(Property::PropertyInfo info, glm::dmat4x3 value,
                                 glm::dmat4x3 minValue, glm::dmat4x3 maxValue,
                                 glm::dmat4x3 stepValue, float exponent)
    : NumericalProperty<glm::dmat4x3>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue),
        exponent
    )
{}

std::string DMat4x3Property::className() const {
    return "DMat4x3Property";
}

int DMat4x3Property::typeLua() const {
    return LUA_TTABLE;
}

glm::dmat4x3 DMat4x3Property::fromLuaConversion(lua_State* state, bool& success) const {
    glm::dmat4x3 result;
    lua_pushnil(state);
    int number = 1;
    for (glm::length_t i = 0; i < glm::dmat4x3::row_type::length(); ++i) {
        for (glm::length_t j = 0; j < glm::dmat4x3::col_type::length(); ++j) {
            int hasNext = lua_next(state, -2);
            if (hasNext != 1) {
                success = false;
                return glm::dmat4x3(0.0);
            }
            if (lua_isnumber(state, -1) != 1) {
                success = false;
                return glm::dmat4x3(0.0);
            }
            else {
                result[i][j] = lua_tonumber(state, -1);
                lua_pop(state, 1);
                ++number;
            }
        }
    }
    // The last accessor argument and the table are still on the stack
    lua_pop(state, 1);
    success = true;
    return result;
}

bool DMat4x3Property::toLuaConversion(lua_State* state) const {
    lua_newtable(state);
    int number = 1;
    for (glm::length_t i = 0; i < glm::dmat4x3::row_type::length(); ++i) {
        for (glm::length_t j = 0; j < glm::dmat4x3::col_type::length(); ++j) {
            lua_pushnumber(state, _value[i][j]);
            lua_rawseti(state, -2, number);
            ++number;
        }
    }
    return true;
}

bool DMat4x3Property::toStringConversion(std::string& outValue) const {
    outValue = "[";
    for (glm::length_t i = 0; i < glm::dmat4x3::row_type::length(); ++i) {
        for (glm::length_t j = 0; j < glm::dmat4x3::col_type::length(); ++j) {
            outValue += std::to_string(_value[i][j]) + ",";
        }
    }
    outValue.pop_back();
    outValue += "]";
    return true;
}

}  // namespace openspace::properties
