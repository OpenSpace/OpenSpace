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

#ifndef __OPENSPACE_CORE___DOUBLELISTPROPERTY___H__
#define __OPENSPACE_CORE___DOUBLELISTPROPERTY___H__

#include <openspace/properties/listproperty.h>

namespace openspace::properties {

class DoubleListProperty : public ListProperty<double> {
public:
    DoubleListProperty(Property::PropertyInfo info,
        std::vector<double> values = std::vector<double>());

    std::string_view className() const override;
    int typeLua() const override;

    using TemplateProperty<std::vector<double>>::operator std::vector<double>;
    using TemplateProperty<std::vector<double>>::operator=;

protected:
    std::vector<double> fromLuaConversion(lua_State* state, bool& success) const override;
    void toLuaConversion(lua_State* state) const override;
    std::string toStringConversion() const override;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___DOUBLELISTPROPERTY___H__
