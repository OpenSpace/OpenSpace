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

#include <openspace/properties/scalar/uintproperty.h>

#include <openspace/util/json_helper.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

UIntProperty::UIntProperty(Property::PropertyInfo info, unsigned int value,
                           unsigned int minValue, unsigned int maxValue,
                           unsigned int stepValue)
    : NumericalProperty<unsigned int>(
        std::move(info),
        value,
        minValue,
        maxValue,
        stepValue
    )
{}

std::string_view UIntProperty::className() const {
    return "UIntProperty";
}

ghoul::lua::LuaTypes UIntProperty::typeLua() const {
    return ghoul::lua::LuaTypes::Number;
}

void UIntProperty::getLuaValue(lua_State* state) const {
    ghoul::lua::push(state, _value);
}

unsigned int UIntProperty::toValue(lua_State* state) const {
    if (ghoul::lua::hasValue<double>(state)) {
        return static_cast<unsigned int>(ghoul::lua::value<double>(state));
    }
    else if (ghoul::lua::hasValue<unsigned int>(state)) {
        return ghoul::lua::value<unsigned int>(state);
    }
    else {
        throw ghoul::RuntimeError("Error extracting value in UIntProperty");
    }
}

std::string UIntProperty::stringValue() const {
    return formatJson(_value);
}

} // namespace openspace::properties
