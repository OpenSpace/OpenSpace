/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_CORE___STRINGLISTPROPERTY___H__
#define __OPENSPACE_CORE___STRINGLISTPROPERTY___H__

#include <openspace/properties/listproperty.h>

#include <string>
#include <vector>

namespace openspace::properties {

class StringListProperty : public ListProperty<std::string> {
public:
    StringListProperty(Property::PropertyInfo info);
    StringListProperty(Property::PropertyInfo info, std::vector<std::string> values);

    using TemplateProperty<std::vector<std::string>>::operator std::vector<std::string>;
    using TemplateProperty<std::vector<std::string>>::operator=;
};

template <>
std::string PropertyDelegate<TemplateProperty<std::vector<std::string>>>::className();

template <>
template <>
std::vector<std::string>
PropertyDelegate<TemplateProperty<std::vector<std::string>>>::fromLuaValue(
    lua_State* state, bool& success);

template <>
template <>
bool PropertyDelegate<TemplateProperty<std::vector<std::string>>>::toLuaValue(
    lua_State* state, const std::vector<std::string>& value);

template <>
int PropertyDelegate<TemplateProperty<std::vector<std::string>>>::typeLua();

template <>
template <>
std::vector<std::string>
PropertyDelegate<TemplateProperty<std::vector<std::string>>>::fromString(
    const std::string& value, bool& success);

template <>
template <>
bool PropertyDelegate<TemplateProperty<std::vector<std::string>>>::toString(
    std::string& outValue, const std::vector<std::string>& inValue);

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___STRINGLISTPROPERTY___H__
