/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __NUMERICALPROPERTY_H__
#define __NUMERICALPROPERTY_H__

#include <openspace/properties/templateproperty.h>

namespace openspace {
namespace properties {

template <typename T>
class NumericalProperty : public TemplateProperty<T> {
public:
    NumericalProperty(std::string identifier, std::string guiName,
        Visibility visibility = Visibility::User);
    NumericalProperty(std::string identifier, std::string guiName, T value,
        Visibility visibility = Visibility::User);
    NumericalProperty(std::string identifier, std::string guiName, T value,
        T minimumValue, T maximumValue, Visibility visibility = Visibility::User);
    NumericalProperty(std::string identifier, std::string guiName, T value,
        T minimumValue, T maximumValue, T steppingValue,
        Visibility visibility = Visibility::User);

    bool getLuaValue(lua_State* state) const override;
    bool setLuaValue(lua_State* state) override;
    int typeLua() const override;

    bool getStringValue(std::string& value) const override;
    bool setStringValue(std::string value) override;

    T minValue() const;
    void setMinValue(T value);

    T maxValue() const;
    void setMaxValue(T value);

    virtual std::string className() const override;

    using TemplateProperty<T>::operator=;

protected:
    static const std::string MinimumValueKey;
    static const std::string MaximumValueKey;
    static const std::string SteppingValueKey;

    std::string generateAdditionalDescription() const;

    T _minimumValue;
    T _maximumValue;
    T _stepping;
};

} // namespace properties
} // namespace openspace

#include "openspace/properties/numericalproperty.inl"

#endif // __NUMERICALPROPERTY_H__
