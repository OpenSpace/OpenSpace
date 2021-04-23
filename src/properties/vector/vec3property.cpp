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

#include <openspace/properties/vector/vec3property.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

Vec3Property::Vec3Property(Property::PropertyInfo info, glm::vec3 value,
                           glm::vec3 minValue, glm::vec3 maxValue, glm::vec3 stepValue)
    : NumericalProperty<glm::vec3>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue)
    )
{}

Vec3Property::Vec3Property(Property::PropertyInfo info, glm::vec3 value,
                           glm::vec3 minValue, glm::vec3 maxValue, glm::vec3 stepValue,
                           float exponent)
    : NumericalProperty<glm::vec3>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue),
        exponent
    )
{}

std::string Vec3Property::className() const {
    return "Vec3Property";
}

int Vec3Property::typeLua() const {
    return LUA_TTABLE;
}

glm::vec3 Vec3Property::fromLuaConversion(lua_State* state, bool& success) const {
    glm::vec3 result = glm::vec3(0.f);
    lua_pushnil(state);
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::vec3>::value; ++i) {
        int hasNext = lua_next(state, -2);
        if (hasNext != 1) {
            success = false;
            return glm::vec3(0.f);
        }
        if (lua_isnumber(state, -1) != 1) {
            success = false;
            return glm::vec3(0.f);
        }
        else {
            result[i] = static_cast<glm::vec3::value_type>(lua_tonumber(state, -1));
            lua_pop(state, 1);
        }
    }

    // The last accessor argument is still on the stack
    lua_pop(state, 1);
    success = true;
    return result;
}

bool Vec3Property::toLuaConversion(lua_State* state) const {
    lua_newtable(state);
    int number = 1;
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::vec3>::value; ++i) {
        lua_pushnumber(state, static_cast<lua_Number>(_value[i]));
        lua_rawseti(state, -2, number);
        ++number;
    }
    return true;
}

bool Vec3Property::toStringConversion(std::string& outValue) const {
    outValue = "{";
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::vec3>::value; ++i) {
        outValue += std::to_string(_value[i]) + ",";
    }
    outValue.pop_back();
    outValue += "}";
    return true;
}

} // namespace openspace::properties
