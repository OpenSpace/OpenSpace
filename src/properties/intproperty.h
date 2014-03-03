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
//
//template <>
//Property* createPropertyDelegate<TemplateProperty<float>>();

namespace openspace {
namespace properties {

typedef NumericalProperty<int> IntProperty;

template <>
std::string PropertyDelegate<IntProperty>::className();

template <>
std::string PropertyDelegate<TemplateProperty<int>>::className();

//template <>
//std::string classNameDelegate<IntProperty>();

} // namespace properties
} // namespace openspace

//class IntProperty : public NumericalProperty<int> {
//public:
//    IntProperty(const std::string& identifier, const std::string& guiName, int value = 0,
//        int minimumValue = 0, int maximumValue = 100, int stepping = 1);
//
//    using NumericalProperty<int>::operator=;
//
//    std::string className() const;
//};

#endif // __INTPROPERTY_H__
