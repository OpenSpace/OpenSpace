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

#ifndef __OPENSPACE_CORE___FLOATPROPERTY___H__
#define __OPENSPACE_CORE___FLOATPROPERTY___H__

 /**
 * \file floatproperty.h
 *
 * \addtogroup openspace
 * @{
 * \addtogroup properties
 * @{

 * \class FloatProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>float</code>.

 * @} @}
 */

#include <openspace/properties/numericalproperty.h>
#include <limits>

namespace openspace::properties {

class FloatProperty : public NumericalProperty<float> {
public:
    FloatProperty(Property::PropertyInfo info, float value = 0.f,
        float minValue = std::numeric_limits<float>::lowest(),
        float maxValue = std::numeric_limits<float>::max(), float stepValue = 0.01f);

    std::string className() const override;
    int typeLua() const override;

    using TemplateProperty<float>::operator=;

protected:
    float fromLuaConversion(lua_State* state, bool& success) const override;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___FLOATPROPERTY___H__
