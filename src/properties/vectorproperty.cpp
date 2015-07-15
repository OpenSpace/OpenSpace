/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include "openspace/properties/vectorproperty.h"

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/glm.h>
#include <ghoul/misc/misc.h>

#include <limits>

using std::numeric_limits;

namespace openspace {
namespace properties {

#define DEFAULT_FROM_LUA_LAMBDA(__TYPE__, __CONVFUNC__, __TESTFUNC__)                    \
    [](lua_State * state, bool& success) -> __TYPE__ {                                   \
        __TYPE__ result;                                                                 \
        lua_pushnil(state);                                                              \
        for (__TYPE__::size_type i = 0; i < result.length(); ++i) {                      \
            int success = lua_next(state, -2);                                           \
            if (success != 1) {                                                          \
                success = false;                                                         \
                return __TYPE__(0);                                                      \
            }                                                                            \
			if (__TESTFUNC__(state, -1) != 1) {                                          \
                success = false;                                                         \
                return __TYPE__(0);                                                      \
            } else {                                                                     \
				result[i] = static_cast<__TYPE__::value_type>(__CONVFUNC__(state, -1));  \
				lua_pop(state, 1);                                                       \
            }                                                                            \
        }                                                                                \
        success = true;                                                                  \
        return result;                                                                   \
    }

#define DEFAULT_TO_LUA_LAMBDA(__TYPE__)                                                  \
    [](lua_State * state, __TYPE__ value) -> bool {                                      \
        lua_newtable(state);                                                             \
        int number = 1;                                                                  \
        for (__TYPE__::size_type i = 0; i < value.length(); ++i) {                       \
            lua_pushnumber(state, static_cast<lua_Number>(value[i]));                    \
            lua_setfield(state, -2, std::to_string(number).c_str());                     \
            ++number;                                                                    \
        }                                                                                \
        return true;                                                                     \
    }

#define DEFAULT_FROM_STRING_LAMBDA(__TYPE__)                                             \
    [](std::string value, bool& success) -> __TYPE__ {                                   \
        __TYPE__ result;                                                                 \
        std::vector<std::string> tokens = ghoul::tokenizeString(value, ',');             \
        if (tokens.size() != result.length()) {                                          \
            success = false;                                                             \
            return result;                                                               \
        }                                                                                \
        for (__TYPE__::size_type i = 0; i < result.length(); ++i) {                      \
                std::stringstream s(tokens[i]);                                          \
                __TYPE__::value_type v;                                                  \
                s >> v;                                                                  \
                if (s.fail()) {                                                          \
                    success = false;                                                     \
                    return result;                                                       \
                }                                                                        \
                else                                                                     \
                    result[i] = v;                                                       \
        }                                                                                \
        success = true;                                                                  \
        return result;                                                                   \
    }

#define DEFAULT_TO_STRING_LAMBDA(__TYPE__)                                               \
    [](std::string& outValue, __TYPE__ inValue) -> bool {                                \
        outValue = "{";                                                                  \
        for (__TYPE__::size_type i = 0; i < inValue.length(); ++i)                       \
            outValue += std::to_string(inValue[i]) + ",";                                \
        outValue.pop_back();                                                             \
        outValue += "}";                                                                 \
        return true;                                                                     \
    }


// Forcing value from int to bool is acceptable here (line 48)
#ifdef WIN32
#pragma warning(disable : 4800)
#endif


REGISTER_TEMPLATEPROPERTY_SOURCE(BVec2Property, glm::bvec2, glm::bvec2(false),
								 DEFAULT_FROM_LUA_LAMBDA(glm::bvec2, lua_toboolean,
														 lua_isboolean),
                                 DEFAULT_TO_LUA_LAMBDA(glm::bvec2),
                                 DEFAULT_FROM_STRING_LAMBDA(glm::bvec2),
                                 DEFAULT_TO_STRING_LAMBDA(glm::bvec2),
                                 LUA_TTABLE);

REGISTER_TEMPLATEPROPERTY_SOURCE(BVec3Property, glm::bvec3, glm::bvec3(false),
                                 DEFAULT_FROM_LUA_LAMBDA(glm::bvec3, lua_toboolean,
                                                         lua_isboolean),
                                 DEFAULT_TO_LUA_LAMBDA(glm::bvec3),
                                 DEFAULT_FROM_STRING_LAMBDA(glm::bvec3),
                                 DEFAULT_TO_STRING_LAMBDA(glm::bvec3),
                                 LUA_TTABLE);

REGISTER_TEMPLATEPROPERTY_SOURCE(BVec4Property, glm::bvec4, glm::bvec4(false),
                                 DEFAULT_FROM_LUA_LAMBDA(glm::bvec4, lua_toboolean,
                                                         lua_isboolean),
                                 DEFAULT_TO_LUA_LAMBDA(glm::bvec4),
                                 DEFAULT_FROM_STRING_LAMBDA(glm::bvec4),
                                 DEFAULT_TO_STRING_LAMBDA(glm::bvec4),
                                 LUA_TTABLE);

#ifdef WIN32
#pragma warning(default : 4800)
#endif


REGISTER_NUMERICALPROPERTY_SOURCE(Vec2Property, glm::vec2, glm::vec2(0),
                                  glm::vec2(numeric_limits<float>::lowest()),
                                  glm::vec2(numeric_limits<float>::max()),
                                  glm::vec2(0.01f),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::vec2, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::vec2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::vec2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::vec2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Vec3Property, glm::vec3, glm::vec3(0),
                                  glm::vec3(numeric_limits<float>::lowest()),
                                  glm::vec3(numeric_limits<float>::max()),
                                  glm::vec3(0.01f),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::vec3, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::vec3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::vec3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::vec3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Vec4Property, glm::vec4, glm::vec4(0),
                                  glm::vec4(numeric_limits<float>::lowest()),
                                  glm::vec4(numeric_limits<float>::max()),
                                  glm::vec4(0.01f),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::vec4, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::vec4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::vec4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::vec4),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DVec2Property, glm::dvec2, glm::dvec2(0),
                                  glm::dvec2(numeric_limits<double>::lowest()),
                                  glm::dvec2(numeric_limits<double>::max()),
                                  glm::dvec2(0.01),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dvec2, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dvec2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dvec2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dvec2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DVec3Property, glm::dvec3, glm::dvec3(0),
                                  glm::dvec3(numeric_limits<double>::lowest()),
                                  glm::dvec3(numeric_limits<double>::max()),
                                  glm::dvec3(0.01),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dvec3, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dvec3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dvec3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dvec3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DVec4Property, glm::dvec4, glm::dvec4(0),
                                  glm::dvec4(numeric_limits<double>::lowest()),
                                  glm::dvec4(numeric_limits<double>::max()),
                                  glm::dvec4(0.01),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dvec4, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dvec4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dvec4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dvec4),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(IVec2Property, glm::ivec2, glm::ivec2(0),
                                  glm::ivec2(numeric_limits<int>::lowest()),
                                  glm::ivec2(numeric_limits<int>::max()), glm::ivec2(1),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::ivec2, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::ivec2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::ivec2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::ivec2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(IVec3Property, glm::ivec3, glm::ivec3(0),
                                  glm::ivec3(numeric_limits<int>::lowest()),
                                  glm::ivec3(numeric_limits<int>::max()), glm::ivec3(1),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::ivec3, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::ivec3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::ivec3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::ivec3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(IVec4Property, glm::ivec4, glm::ivec4(0),
                                  glm::ivec4(numeric_limits<int>::lowest()),
                                  glm::ivec4(numeric_limits<int>::max()), glm::ivec4(1),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::ivec4, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::ivec4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::ivec4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::ivec4),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(UVec2Property, glm::uvec2, glm::uvec2(0),
                                  glm::uvec2(numeric_limits<unsigned int>::lowest()),
                                  glm::uvec2(numeric_limits<unsigned int>::max()),
                                  glm::uvec2(1),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::uvec2, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::uvec2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::uvec2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::uvec2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(UVec3Property, glm::uvec3, glm::uvec3(0),
                                  glm::uvec3(numeric_limits<unsigned int>::lowest()),
                                  glm::uvec3(numeric_limits<unsigned int>::max()),
                                  glm::uvec3(1),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::uvec3, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::uvec3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::uvec3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::uvec3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(UVec4Property, glm::uvec4, glm::uvec4(0),
                                  glm::uvec4(numeric_limits<unsigned int>::lowest()),
                                  glm::uvec4(numeric_limits<unsigned int>::max()),
                                  glm::uvec4(1),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::uvec4, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::uvec4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::uvec4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::uvec4),
                                  LUA_TTABLE);

} // namespace properties
} // namespace openspace
