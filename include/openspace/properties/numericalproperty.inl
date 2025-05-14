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

#include <string_view>

namespace {
    constexpr std::string_view MinimumValueKey = "min";
    constexpr std::string_view MaximumValueKey = "max";
    constexpr std::string_view SteppingValueKey = "step";
    constexpr std::string_view ExponentValueKey = "exponent";

    std::string luaToJson(std::string luaValue) {
        if (luaValue[0] == '{') {
            luaValue.replace(0, 1, "[");
        }
        if (luaValue[luaValue.size() - 1] == '}') {
            luaValue.replace(luaValue.size() - 1, 1, "]");
        }
        return luaValue;
    }
} // namespace

namespace openspace::properties {

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
	Property::notifyMetaDataChangeListeners();
}

template <typename T>
T NumericalProperty<T>::maxValue() const {
    return _maximumValue;
}

template <typename T>
void NumericalProperty<T>::setMaxValue(T value) {
    _maximumValue = std::move(value);
	Property::notifyMetaDataChangeListeners();
}

template <typename T>
T NumericalProperty<T>::steppingValue() const {
    return _stepping;
}

template <typename T>
void NumericalProperty<T>::setSteppingValue(T value) {
    _stepping = std::move(value);
	Property::notifyMetaDataChangeListeners();
}

template <typename T>
float NumericalProperty<T>::exponent() const {
    return _exponent;
}

template <typename T>
void NumericalProperty<T>::setExponent(float exponent) {
    ghoul_assert(std::abs(exponent) > 0.f, "Exponent for property input cannot be zero");

    if (!std::is_unsigned<T>::value) {
        auto isValidRange = [](const T& minValue, const T& maxValue) {
            if constexpr (ghoul::isGlmVector<T>() || ghoul::isGlmMatrix<T>()) {
                return glm::all(glm::greaterThanEqual(minValue, T(0))) &&
                    glm::all(glm::greaterThanEqual(maxValue, T(0)));
            }
            else {
                return (minValue >= T(0) && maxValue >= T(0));
            }
        };

        // While the exponential slider does not support ranges with negative values,
        // prevent setting the exponent for such ranges
        // @TODO (2021-06-30, emmbr), remove this check when no longer needed
        ghoul_assert(
            isValidRange(_minimumValue, _maximumValue),
            "Setting exponent for properties with negative values in [min, max] "
            "range is not yet supported"
        );
        if (!isValidRange(_minimumValue, _maximumValue)) {
            exponent = 1.f;
        }
    }

    _exponent = exponent;
    Property::notifyMetaDataChangeListeners();
}

template <typename T>
nlohmann::json NumericalProperty<T>::generateAdditionalJsonDescription() const {
    nlohmann::json result = {
        { MinimumValueKey, nlohmann::json::parse(luaToJson(ghoul::to_string(_minimumValue))) },
        { MaximumValueKey, nlohmann::json::parse(luaToJson(ghoul::to_string(_maximumValue))) },
        { SteppingValueKey, nlohmann::json::parse(luaToJson(ghoul::to_string(_stepping))) },
        { ExponentValueKey, nlohmann::json::parse(luaToJson(ghoul::to_string(_exponent))) }
    };
    return result;
}

template <typename T>
void NumericalProperty<T>::setLuaInterpolationTarget(lua_State* state) {
    T targetValue = toValue(state);
    _interpolationStart = TemplateProperty<T>::_value;
    _interpolationEnd = std::move(targetValue);
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

} // namespace openspace::properties
