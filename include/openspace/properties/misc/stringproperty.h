/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___STRINGPROPERTY___H__
#define __OPENSPACE_CORE___STRINGPROPERTY___H__

#include <openspace/properties/templateproperty.h>

namespace openspace::properties {

class StringProperty : public TemplateProperty<std::string> {
public:
    StringProperty(Property::PropertyInfo info, std::string value = "");

    std::string_view className() const override final;
    ghoul::lua::LuaTypes typeLua() const override final;

    using TemplateProperty<std::string>::operator=;
    using TemplateProperty<std::string>::value;

    void getLuaValue(lua_State* state) const override final;

    std::string stringValue() const override final;
    operator std::string_view();
    operator std::string_view() const;

private:
    std::string toValue(lua_State* state) const override final;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___STRINGPROPERTY___H__
