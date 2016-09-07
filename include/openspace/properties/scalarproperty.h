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

#ifndef __SCALARPROPERTY_H__
#define __SCALARPROPERTY_H__

 /**
 * \file scalarproperty.h
 *
 * \addtogroup openspace 
 * @{
 * \addtogroup properties 
 * @{
 
 * \class BoolProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>bool</code>.

 * \class CharProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>char</code>.

 * \class SignedCharProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>signed char</code>.

 * \class UCharProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>unsigned char</code>.

 * \class ShortProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>short</code>.

 * \class UShortProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>unsigned short</code>.

 * \class IntProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>int</code>.

 * \class UIntProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>unsigned int</code>.

 * \class LongProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>long</code>.

 * \class ULongProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>unsigned long</code>.

 * \class LongLongProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>long long</code>.

 * \class ULongLongProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>unsigned long long</code>.

 * \class FloatProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>float</code>.

 * \class DoubleProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>double</code>.

 * \class LongDoubleProperty
 * This class is a concrete implementation of openspace::properties::TemplateProperty with
 * the type <code>long double</code>.

 * @} @}
 */

#include "openspace/properties/numericalproperty.h"

namespace openspace {
namespace properties {

REGISTER_TEMPLATEPROPERTY_HEADER(BoolProperty, bool);
REGISTER_NUMERICALPROPERTY_HEADER(CharProperty, char);
//REGISTER_NUMERICALPROPERTY_HEADER(WCharProperty, wchar_t);
REGISTER_NUMERICALPROPERTY_HEADER(SignedCharProperty, signed char);
REGISTER_NUMERICALPROPERTY_HEADER(UCharProperty, unsigned char);
REGISTER_NUMERICALPROPERTY_HEADER(ShortProperty, short);
REGISTER_NUMERICALPROPERTY_HEADER(UShortProperty, unsigned short);
REGISTER_NUMERICALPROPERTY_HEADER(IntProperty, int);
REGISTER_NUMERICALPROPERTY_HEADER(UIntProperty, unsigned int);
REGISTER_NUMERICALPROPERTY_HEADER(LongProperty, long);
REGISTER_NUMERICALPROPERTY_HEADER(ULongProperty, unsigned long);
REGISTER_NUMERICALPROPERTY_HEADER(LongLongProperty, long long);
REGISTER_NUMERICALPROPERTY_HEADER(ULongLongProperty, unsigned long long);
REGISTER_NUMERICALPROPERTY_HEADER(FloatProperty, float);
REGISTER_NUMERICALPROPERTY_HEADER(DoubleProperty, double);
REGISTER_NUMERICALPROPERTY_HEADER(LongDoubleProperty, long double);

} // namespace properties
} // namespace openspace

#endif // __SCALARPROPERTY_H__
