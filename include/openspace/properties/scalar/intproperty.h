/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_CORE___INTPROPERTY___H__
#define __OPENSPACE_CORE___INTPROPERTY___H__

 /**
 * \file intproperty.h
 *
 * \addtogroup openspace
 * @{
 * \addtogroup properties
 * @{

 * \class IntProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>int</code>.

 * @} @}
 */

#include <openspace/properties/numericalproperty.h>
#include <limits>

namespace openspace::properties {

class IntProperty : public NumericalProperty<int> {
public:
    IntProperty(Property::PropertyInfo info, int value = 0,
        int minValue = std::numeric_limits<int>::lowest(),
        int maxValue = std::numeric_limits<int>::max(), int stepValue = 1);

    std::string className() const override;
    int typeLua() const override;

    using TemplateProperty<int>::operator=;

protected:
    int fromLuaConversion(lua_State* state, bool& success) const override;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___INTPROPERTY___H__
