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

#include <openspace/properties/vector/dvec2property.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

DVec2Property::DVec2Property(Property::PropertyInfo info, glm::dvec2 value,
                             glm::dvec2 minValue, glm::dvec2 maxValue,
                             glm::dvec2 stepValue)
    : NumericalProperty<glm::dvec2>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue)
    )
{}

DVec2Property::DVec2Property(Property::PropertyInfo info, glm::dvec2 value,
                             glm::dvec2 minValue, glm::dvec2 maxValue,
                             glm::dvec2 stepValue, float exponent)
    : NumericalProperty<glm::dvec2>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue),
        exponent
    )
{}

std::string DVec2Property::className() const {
    return "DVec2Property";
}

int DVec2Property::typeLua() const {
    return LUA_TTABLE;
}

glm::dvec2 DVec2Property::fromLuaConversion(lua_State* state, bool& success) const {
    glm::dvec2 result = glm::dvec2(0.0);
    lua_pushnil(state);
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::dvec2>::value; ++i) {
        int hasNext = lua_next(state, -2);
        if (hasNext != 1) {
            success = false;
            return glm::dvec2(0.0);
        }
        if (lua_isnumber(state, -1) != 1) {
            success = false;
            return glm::dvec2(0.0);
        }
        else {
            result[i] = lua_tonumber(state, -1);
            lua_pop(state, 1);
        }
    }

    // The last accessor argument is still on the stack
    lua_pop(state, 1);
    success = true;
    return result;
}

bool DVec2Property::toLuaConversion(lua_State* state) const {
    lua_newtable(state);
    int number = 1;
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::dvec2>::value; ++i) {
        lua_pushnumber(state, _value[i]);
        lua_rawseti(state, -2, number);
        ++number;
    }
    return true;
}

bool DVec2Property::toStringConversion(std::string& outValue) const {
    outValue = "{";
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::dvec2>::value; ++i) {
        outValue += std::to_string(_value[i]) + ",";
    }
    outValue.pop_back();
    outValue += "}";
    return true;
}

} // namespace openspace::properties
