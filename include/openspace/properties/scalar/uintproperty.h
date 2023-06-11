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

#ifndef __OPENSPACE_CORE___UINTPROPERTY___H__
#define __OPENSPACE_CORE___UINTPROPERTY___H__

#include <openspace/properties/numericalproperty.h>
#include <limits>

namespace openspace::properties {

/**
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type `unsigned int`.
 */
class UIntProperty : public NumericalProperty<unsigned int> {
public:
    UIntProperty(Property::PropertyInfo info, unsigned int value = 0,
        unsigned int minValue = std::numeric_limits<unsigned int>::lowest(),
        unsigned int maxValue = std::numeric_limits<unsigned int>::max(),
        unsigned int stepValue = 1);

    std::string_view className() const override;
    int typeLua() const override;

    using TemplateProperty<unsigned int>::operator=;

private:
    unsigned int fromLuaConversion(lua_State* state) const override;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___UINTPROPERTY___H__
