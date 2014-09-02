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

#ifndef __STRINGPROPERTY_H__
#define __STRINGPROPERTY_H__

#include <openspace/properties/templateproperty.h>

namespace openspace {
namespace properties {

class StringProperty : public TemplateProperty<std::string> {
public:
    StringProperty(std::string identifier, std::string guiName);
    StringProperty(std::string identifier, std::string guiName, std::string value);

    using TemplateProperty<std::string>::operator=;
};

template <>
std::string PropertyDelegate<TemplateProperty<std::string>>::className();

template <>
template <>
std::string PropertyDelegate<TemplateProperty<std::string>>::defaultValue<std::string>();

template <>
template <>
std::string PropertyDelegate<TemplateProperty<std::string>>::fromLuaValue<std::string>(
      lua_State* state, bool& success);

template <>
template <>
bool PropertyDelegate<TemplateProperty<std::string>>::toLuaValue<std::string>(
      lua_State* state, std::string value);

template <>
int PropertyDelegate<TemplateProperty<std::string>>::typeLua();



} // namespace properties
} // namespace openspace

#endif // __STRINGPROPERTY_H__