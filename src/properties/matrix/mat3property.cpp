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

#include <openspace/properties/matrix/mat3property.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>

namespace openspace::properties {

Mat3Property::Mat3Property(Property::PropertyInfo info, glm::mat3x3 value,
                           glm::mat3x3 minValue, glm::mat3x3 maxValue,
                           glm::mat3x3 stepValue)
    : NumericalProperty<glm::mat3x3>(
        std::move(info),
        std::move(value),
        std::move(minValue),
        std::move(maxValue),
        std::move(stepValue)
    )
{}

std::string Mat3Property::className() const {
    return "Mat3Property";
}

int Mat3Property::typeLua() const {
    return LUA_TTABLE;
}

glm::mat3x3 Mat3Property::fromLuaConversion(lua_State* state, bool& success) const {
    return ghoul::lua::tryGetValue<glm::mat3x3>(state, success);
}

void Mat3Property::toLuaConversion(lua_State* state) const {
    ghoul::lua::push(state, _value);
}

std::string Mat3Property::toStringConversion() const {
    std::string outValue = "[";
    for (glm::length_t i = 0; i < glm::mat3x3::row_type::length(); ++i) {
        for (glm::length_t j = 0; j < glm::mat3x3::col_type::length(); ++j) {
            outValue += std::to_string(_value[i][j]) + ",";
        }
    }
    outValue.pop_back();
    outValue += "]";
    return outValue;
}

}  // namespace openspace::properties
