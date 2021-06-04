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

#include <openspace/util/json_helper.h>
#include <ghoul/lua/ghoul_lua.h>
#include <glm/ext/matrix_common.hpp>
#include <cmath>

namespace openspace::properties {

template <typename T>
const std::string NumericalProperty<T>::MinimumValueKey = "MinimumValue";

template <typename T>
const std::string NumericalProperty<T>::MaximumValueKey = "MaximumValue";

template <typename T>
const std::string NumericalProperty<T>::SteppingValueKey = "SteppingValue";

template <typename T>
const std::string NumericalProperty<T>::ExponentValueKey = "Exponent";

template <typename T>
NumericalProperty<T>::NumericalProperty(Property::PropertyInfo info, T value,
                                        T minimumValue, T maximumValue, T steppingValue,
                                        float exponent)
    : TemplateProperty<T>(std::move(info), std::move(value))
    , _minimumValue(std::move(minimumValue))
    , _maximumValue(std::move(maximumValue))
    , _stepping(std::move(steppingValue))
    , _exponent(exponent)
{}

template <typename T>
T NumericalProperty<T>::minValue() const {
    return _minimumValue;
}

template <typename T>
void NumericalProperty<T>::setMinValue(T value) {
    _minimumValue = std::move(value);
}

template <typename T>
T NumericalProperty<T>::maxValue() const {
    return _maximumValue;
}

template <typename T>
void NumericalProperty<T>::setMaxValue(T value) {
    _maximumValue = std::move(value);
}

template <typename T>
T NumericalProperty<T>::steppingValue() const {
    return _stepping;
}

template <typename T>
void NumericalProperty<T>::setSteppingValue(T value) {
    _stepping = std::move(value);
}

template <typename T>
float NumericalProperty<T>::exponent() const {
    return _exponent;
}

template <typename T>
void NumericalProperty<T>::setExponent(float exponent) {
    ghoul_assert(std::abs(exponent) > 0.f, "Exponent for property input cannot be zero");
    _exponent = exponent;
}

template <typename T>
std::string NumericalProperty<T>::generateAdditionalJsonDescription() const {
    std::string result = "{ ";
    result +=
      "\"" + MinimumValueKey + "\": " + luaToJson(ghoul::to_string(_minimumValue)) + ",";
    result +=
      "\"" + MaximumValueKey + "\": " + luaToJson(ghoul::to_string(_maximumValue)) + ",";
    result +=
      "\"" + SteppingValueKey + "\": " + luaToJson(ghoul::to_string(_stepping)) + ",";
    result +=
      "\"" + ExponentValueKey + "\": " + luaToJson(ghoul::to_string(_exponent));
    result += " }";
    return result;
}

template <typename T>
std::string NumericalProperty<T>::luaToJson(std::string luaValue) const {
    if (luaValue[0] == '{') {
        luaValue.replace(0, 1, "[");
    }
    if (luaValue[luaValue.size() - 1] == '}') {
        luaValue.replace(luaValue.size() - 1, 1, "]");
    }
    return luaValue;
}

template <typename T>
std::string NumericalProperty<T>::jsonValue() const {
    std::string value = toStringConversion();
    return luaToJson(value);
}

template <typename T>
void NumericalProperty<T>::setInterpolationTarget(std::any value) {
    T v = std::any_cast<T>(std::move(value));

    _interpolationStart = TemplateProperty<T>::_value;
    _interpolationEnd = std::move(v);
}

template <typename T>
void NumericalProperty<T>::setLuaInterpolationTarget(lua_State* state) {
    bool success = false;
    T targetValue = fromLuaConversion(state, success);
    if (success) {
        _interpolationStart = TemplateProperty<T>::_value;
        _interpolationEnd = std::move(targetValue);
    }
}

template <typename T>
void NumericalProperty<T>::interpolateValue(float t,
                                            ghoul::EasingFunc<float> easingFunction)
{
    if (easingFunction) {
        t = easingFunction(t);
    }
    TemplateProperty<T>::setValue(static_cast<T>(
        glm::mix(_interpolationStart, _interpolationEnd, t)
    ));
}

template <typename T>
void NumericalProperty<T>::toLuaConversion(lua_State* state) const {
    ghoul::lua::push(state, TemplateProperty<T>::_value);
}

template <typename T>
std::string NumericalProperty<T>::toStringConversion() const {
    return formatJson(TemplateProperty<T>::_value);
}

} // namespace openspace::properties
