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

#ifndef __OPENSPACE_CORE___IVEC4PROPERTY___H__
#define __OPENSPACE_CORE___IVEC4PROPERTY___H__

#include <openspace/properties/numericalproperty.h>

#include <ghoul/glm.h>
#include <limits>

namespace openspace::properties {

class IVec4Property : public NumericalProperty<glm::ivec4> {
public:
    IVec4Property(Property::PropertyInfo info, glm::ivec4 value = glm::ivec4(0),
        glm::ivec4 minValue = glm::ivec4(std::numeric_limits<int>::lowest()),
        glm::ivec4 maxValue = glm::ivec4(std::numeric_limits<int>::max()),
        glm::ivec4 stepValue = glm::ivec4(1));

    std::string className() const override;
    int typeLua() const override;

    using TemplateProperty<glm::ivec4>::operator=;

protected:
    glm::ivec4 fromLuaConversion(lua_State* state, bool& success) const override;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___IVEC4PROPERTY___H__
