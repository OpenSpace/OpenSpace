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

#ifndef __OPENSPACE_CORE___NUMERICALPROPERTY___H__
#define __OPENSPACE_CORE___NUMERICALPROPERTY___H__

#include <openspace/properties/templateproperty.h>
#include <openspace/util/json_helper.h>

namespace openspace::properties {

template <typename T>
class NumericalProperty : public TemplateProperty<T> {
public:
    NumericalProperty(Property::PropertyInfo info, T value, T minimumValue,
        T maximumValue, T steppingValue, float exponent = 1.f);

    T minValue() const;
    void setMinValue(T value);

    T maxValue() const;
    void setMaxValue(T value);

    T steppingValue() const;
    void setSteppingValue(T value);

    float exponent() const;
    void setExponent(float exponent);

    using TemplateProperty<T>::operator=;

    void setLuaInterpolationTarget(lua_State* state) override;

    void interpolateValue(float t,
        ghoul::EasingFunc<float> easingFunc = nullptr) override;

protected:
    nlohmann::json generateAdditionalJsonDescription() const override;
    using TemplateProperty<T>::toValue;

    T _minimumValue = T(0);
    T _maximumValue = T(0);
    T _stepping = T(0);
    float _exponent = 0.f;

    T _interpolationStart = T(0);
    T _interpolationEnd = T(0);
};

} // namespace openspace::properties

#include "openspace/properties/numericalproperty.inl"

#endif // __OPENSPACE_CORE___NUMERICALPROPERTY___H__
