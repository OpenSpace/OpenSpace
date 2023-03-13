/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

namespace openspace::properties {

template <typename T>
class NumericalProperty : public TemplateProperty<T> {
public:
    NumericalProperty(Property::PropertyInfo info, T value, T minimumValue,
        T maximumValue, T steppingValue, float exponent = 1.f);

    virtual std::string_view className() const override = 0;
    virtual int typeLua() const override = 0;

    T minValue() const;
    void setMinValue(T value);

    T maxValue() const;
    void setMaxValue(T value);

    T steppingValue() const;
    void setSteppingValue(T value);

    float exponent() const;
    void setExponent(float exponent);

    std::string jsonValue() const override;

    using TemplateProperty<T>::operator=;

    void setInterpolationTarget(std::any value) override;
    void setLuaInterpolationTarget(lua_State* state) override;

    void interpolateValue(float t,
        ghoul::EasingFunc<float> easingFunc = nullptr) override;

protected:
    static const std::string MinimumValueKey;
    static const std::string MaximumValueKey;
    static const std::string SteppingValueKey;
    static const std::string ExponentValueKey;

    T fromLuaConversion(lua_State* state) const override;
    virtual void toLuaConversion(lua_State* state) const override;
    virtual std::string toStringConversion() const override;

    std::string generateAdditionalJsonDescription() const override;

    /**
     * convert a lua formatted value to a JSON formatted value
     * @param luaValue
     * @return a json formatted string representation of the given lua value
     */
    std::string luaToJson(std::string luaValue) const;

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
