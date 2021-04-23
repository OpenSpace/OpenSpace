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

#include <openspace/properties/vector/bvec3property.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

BVec3Property::BVec3Property(Property::PropertyInfo info, glm::bvec3 value)
    : TemplateProperty<glm::bvec3>(info, value)
{}

std::string BVec3Property::className() const {
    return "BVec3Property";
}

int BVec3Property::typeLua() const {
    return LUA_TTABLE;
}

glm::bvec3 BVec3Property::fromLuaConversion(lua_State* state, bool& success) const {
    glm::bvec3 result = glm::bvec3(false);
    lua_pushnil(state);
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::bvec3>::value; ++i) {
        int hasNext = lua_next(state, -2);
        if (hasNext != 1) {
            success = false;
            return glm::bvec3(false);
        }
        if (lua_isboolean(state, -1) != 1) {
            success = false;
            return glm::bvec3(false);
        }
        else {
            result[i] = static_cast<glm::bvec3::value_type>(lua_toboolean(state, -1));
            lua_pop(state, 1);
        }
    }

    // The last accessor argument is still on the stack
    lua_pop(state, 1);
    success = true;
    return result;
}

bool BVec3Property::toLuaConversion(lua_State* state) const {
    lua_newtable(state);
    int number = 1;
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::bvec3>::value; ++i) {
        lua_pushnumber(state, static_cast<lua_Number>(_value[i]));
        lua_setfield(state, -2, std::to_string(number).c_str());
        ++number;
    }
    return true;
}

bool BVec3Property::toStringConversion(std::string& outValue) const {
    outValue = "{";
    for (glm::length_t i = 0; i < ghoul::glm_components<glm::bvec3>::value; ++i) {
        outValue += std::to_string(_value[i]) + ",";
    }
    outValue.pop_back();
    outValue += "}";
    return true;
}

} // namespace openspace::properties
