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

#include <openspace/properties/scalar/longlongproperty.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

LongLongProperty::LongLongProperty(Property::PropertyInfo info, long long value,
                                   long long minValue, long long maxValue,
                                   long long stepValue)
    : NumericalProperty<long long>(
        std::move(info),
        value,
        minValue,
        maxValue,
        stepValue
    )
{}

LongLongProperty::LongLongProperty(Property::PropertyInfo info, long long value,
                                   long long minValue, long long maxValue,
                                   long long stepValue, float exponent)
    : NumericalProperty<long long>(
        std::move(info),
        value,
        minValue,
        maxValue,
        stepValue,
        exponent
    )
{}

std::string LongLongProperty::className() const {
    return "LongLongProperty";
}

int LongLongProperty::typeLua() const {
    return LUA_TNUMBER;
}

long long LongLongProperty::fromLuaConversion(lua_State* state, bool& success) const {
    success = (lua_isnumber(state, -1) == 1);
    if (success) {
        long long val = static_cast<long long>(lua_tonumber(state, -1));
        return val;
    }
    else {
        return 0;
    }
}

bool LongLongProperty::toLuaConversion(lua_State* state) const {
    lua_pushnumber(state, static_cast<lua_Number>(_value));
    return true;
}

bool LongLongProperty::toStringConversion(std::string& outValue) const {
    outValue = std::to_string(_value);
    return true;
}

} // namespace openspace::properties
