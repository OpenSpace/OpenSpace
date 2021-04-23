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

#include <openspace/properties/scalar/longdoubleproperty.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

LongDoubleProperty::LongDoubleProperty(Property::PropertyInfo info, long double value,
                                       long double minValue, long double maxValue,
                                       long double stepValue)
    : NumericalProperty<long double>(
        std::move(info),
        value,
        minValue,
        maxValue,
        stepValue
    )
{}

LongDoubleProperty::LongDoubleProperty(Property::PropertyInfo info, long double value,
                                       long double minValue, long double maxValue,
                                       long double stepValue, float exponent)
    : NumericalProperty<long double>(
        std::move(info),
        value,
        minValue,
        maxValue,
        stepValue,
        exponent
    )
{}

std::string LongDoubleProperty::className() const {
    return "LongDoubleProperty";
}

int LongDoubleProperty::typeLua() const {
    return LUA_TNUMBER;
}

long double LongDoubleProperty::fromLuaConversion(lua_State* state, bool& success) const {
    success = (lua_isnumber(state, -1) == 1);
    if (success) {
        long double val = static_cast<long double>(lua_tonumber(state, -1));
        return val;
    }
    else {
        return 0l;
    }
}

bool LongDoubleProperty::toLuaConversion(lua_State* state) const {
    lua_pushnumber(state, static_cast<lua_Number>(_value));
    return true;
}

bool LongDoubleProperty::toStringConversion(std::string& outValue) const {
    outValue = std::to_string(_value);
    return true;
}

} // namespace openspace::properties
