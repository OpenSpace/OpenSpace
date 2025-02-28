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

#ifndef __OPENSPACE_CORE___UVEC3PROPERTY___H__
#define __OPENSPACE_CORE___UVEC3PROPERTY___H__

#include <openspace/properties/numericalproperty.h>

#include <ghoul/glm.h>
#include <limits>

namespace openspace::properties {

class UVec3Property : public NumericalProperty<glm::uvec3> {
public:
    UVec3Property(Property::PropertyInfo info, glm::uvec3 value = glm::uvec3(0),
        glm::uvec3 minValue = glm::uvec3(std::numeric_limits<unsigned int>::lowest()),
        glm::uvec3 maxValue = glm::uvec3(std::numeric_limits<unsigned int>::max()),
        glm::uvec3 stepValue = glm::uvec3(1));

    std::string_view className() const override final;
    ghoul::lua::LuaTypes typeLua() const override final;

    void getLuaValue(lua_State* state) const override final;

    std::string stringValue() const override final;
    using TemplateProperty<glm::uvec3>::operator=;

private:
    glm::uvec3 toValue(lua_State* state) const override final;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___UVEC3PROPERTY___H__
