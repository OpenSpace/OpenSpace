/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
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

#ifndef __INTPROPERTY_H__
#define __INTPROPERTY_H__

#include "properties/numericalproperty.h"

namespace openspace {
namespace properties {

REGISTER_NUMERICALPROPERTY_HEADER(CharProperty, char);
//REGISTER_NUMERICALPROPERTY_HEADER(Char16Property, char16_t);
//REGISTER_NUMERICALPROPERTY_HEADER(Char32Property, char32_t);
REGISTER_NUMERICALPROPERTY_HEADER(WCharProperty, wchar_t);
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

#endif // __INTPROPERTY_H__
