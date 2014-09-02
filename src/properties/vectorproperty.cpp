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

#include "openspace/properties/vectorproperty.h"

#include <ghoul/lua/ghoul_lua.h>

#include <glm/glm.hpp>

#include <limits>

using std::numeric_limits;

namespace openspace {
namespace properties {

#define DEFAULT_FROM_LUA_LAMBDA_2(TYPE, DEFAULT_VALUE) \
	[](lua_State* state, bool& success) -> TYPE { \
		success = (lua_isnumber(state, -1) == 1) && (lua_isnumber(state, -2) == 1); \
		if (success) \
			return TYPE( \
					static_cast<TYPE::value_type>(lua_tonumber(state, -1)), \
					static_cast<TYPE::value_type>(lua_tonumber(state, -2))); \
		else \
			return DEFAULT_VALUE; \
	}

#define DEFAULT_FROM_LUA_LAMBDA_3(TYPE, DEFAULT_VALUE) \
	[](lua_State* state, bool& success) -> TYPE { \
		success = (lua_isnumber(state, -1) == 1) && \
				  (lua_isnumber(state, -2) == 1) && \
				  (lua_isnumber(state, -3) == 1); \
		if (success) \
			return TYPE( \
				static_cast<TYPE::value_type>(lua_tonumber(state, -1)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -2)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -3))); \
		else \
			return DEFAULT_VALUE; \
}

#define DEFAULT_FROM_LUA_LAMBDA_4(TYPE, DEFAULT_VALUE) \
	[](lua_State* state, bool& success) -> TYPE { \
		success = (lua_isnumber(state, -1) == 1) && \
			      (lua_isnumber(state, -2) == 1) && \
				  (lua_isnumber(state, -3) == 1) && \
				  (lua_isnumber(state, -4) == 1); \
		if (success) \
			return TYPE( \
				static_cast<TYPE::value_type>(lua_tonumber(state, -1)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -2)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -3)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -4))); \
		else \
			return DEFAULT_VALUE; \
}

#define DEFAULT_TO_LUA_LAMBDA_2(TYPE)                                                    \
    [](lua_State* state, TYPE value) -> bool {                                          \
        lua_pushnumber(state, static_cast<lua_Number>(value.x));                         \
        lua_pushnumber(state, static_cast<lua_Number>(value.y));                         \
        return true;                                                                     \
    }

#define DEFAULT_TO_LUA_LAMBDA_3(TYPE)                                                    \
    [](lua_State* state, TYPE value) -> bool {                                          \
        lua_pushnumber(state, static_cast<lua_Number>(value.x));                         \
        lua_pushnumber(state, static_cast<lua_Number>(value.y));                         \
        lua_pushnumber(state, static_cast<lua_Number>(value.z));                         \
        return true;                                                                     \
    \
}

#define DEFAULT_TO_LUA_LAMBDA_4(TYPE)                                                    \
    [](lua_State* state, TYPE value) -> bool {                                          \
        lua_pushnumber(state, static_cast<lua_Number>(value.x));                         \
        lua_pushnumber(state, static_cast<lua_Number>(value.y));                         \
        lua_pushnumber(state, static_cast<lua_Number>(value.z));                         \
        lua_pushnumber(state, static_cast<lua_Number>(value.w));                         \
        return true;                                                                     \
    \
}

REGISTER_TEMPLATEPROPERTY_SOURCE(BVec2Property, glm::bvec2, glm::bvec2(false));
REGISTER_TEMPLATEPROPERTY_SOURCE(BVec3Property, glm::bvec3, glm::bvec3(false));
REGISTER_TEMPLATEPROPERTY_SOURCE(BVec4Property, glm::bvec4, glm::bvec4(false));

REGISTER_NUMERICALPROPERTY_SOURCE(Vec2Property, glm::vec2, glm::vec2(0),
    glm::vec2(numeric_limits<float>::lowest()), glm::vec2(numeric_limits<float>::max()),
    glm::vec2(0.01f), DEFAULT_FROM_LUA_LAMBDA_2(glm::vec2, glm::vec2(0)),
	DEFAULT_TO_LUA_LAMBDA_2(glm::vec2));

REGISTER_NUMERICALPROPERTY_SOURCE(Vec3Property, glm::vec3, glm::vec3(0),
    glm::vec3(numeric_limits<float>::lowest()), glm::vec3(numeric_limits<float>::max()),
    glm::vec3(0.01f), DEFAULT_FROM_LUA_LAMBDA_3(glm::vec3, glm::vec3(0)),
	DEFAULT_TO_LUA_LAMBDA_3(glm::vec3));

REGISTER_NUMERICALPROPERTY_SOURCE(Vec4Property, glm::vec4, glm::vec4(0),
    glm::vec4(numeric_limits<float>::lowest()), glm::vec4(numeric_limits<float>::max()),
    glm::vec4(0.01f), DEFAULT_FROM_LUA_LAMBDA_4(glm::vec4, glm::vec4(0)),
	DEFAULT_TO_LUA_LAMBDA_4(glm::vec4));

REGISTER_NUMERICALPROPERTY_SOURCE(DVec2Property, glm::dvec2, glm::dvec2(0),
    glm::dvec2(numeric_limits<double>::lowest()),
    glm::dvec2(numeric_limits<double>::max()), glm::dvec2(0.01),
	DEFAULT_FROM_LUA_LAMBDA_2(glm::dvec2, glm::dvec2(0)),
	DEFAULT_TO_LUA_LAMBDA_2(glm::dvec2));

REGISTER_NUMERICALPROPERTY_SOURCE(DVec3Property, glm::dvec3, glm::dvec3(0),
    glm::dvec3(numeric_limits<double>::lowest()),
    glm::dvec3(numeric_limits<double>::max()), glm::dvec3(0.01),
	DEFAULT_FROM_LUA_LAMBDA_3(glm::dvec3, glm::dvec3(0)),
	DEFAULT_TO_LUA_LAMBDA_3(glm::dvec3));

REGISTER_NUMERICALPROPERTY_SOURCE(DVec4Property, glm::dvec4, glm::dvec4(0),
    glm::dvec4(numeric_limits<double>::lowest()),
    glm::dvec4(numeric_limits<double>::max()), glm::dvec4(0.01),
	DEFAULT_FROM_LUA_LAMBDA_4(glm::dvec4, glm::dvec4(0)),
	DEFAULT_TO_LUA_LAMBDA_4(glm::dvec4));

REGISTER_NUMERICALPROPERTY_SOURCE(IVec2Property, glm::ivec2, glm::ivec2(0),
    glm::ivec2(numeric_limits<int>::lowest()), glm::ivec2(numeric_limits<int>::max()),
    glm::ivec2(1), DEFAULT_FROM_LUA_LAMBDA_2(glm::ivec2, glm::ivec2(0)),
	DEFAULT_TO_LUA_LAMBDA_2(glm::ivec2));

REGISTER_NUMERICALPROPERTY_SOURCE(IVec3Property, glm::ivec3, glm::ivec3(0),
    glm::ivec3(numeric_limits<int>::lowest()), glm::ivec3(numeric_limits<int>::max()),
    glm::ivec3(1), DEFAULT_FROM_LUA_LAMBDA_3(glm::ivec3, glm::ivec3(0)),
	DEFAULT_TO_LUA_LAMBDA_3(glm::ivec3));

REGISTER_NUMERICALPROPERTY_SOURCE(IVec4Property, glm::ivec4, glm::ivec4(0),
    glm::ivec4(numeric_limits<int>::lowest()), glm::ivec4(numeric_limits<int>::max()),
    glm::ivec4(1), DEFAULT_FROM_LUA_LAMBDA_4(glm::ivec4, glm::ivec4(0)),
	DEFAULT_TO_LUA_LAMBDA_4(glm::ivec4));

REGISTER_NUMERICALPROPERTY_SOURCE(UVec2Property, glm::uvec2, glm::uvec2(0),
    glm::uvec2(numeric_limits<unsigned int>::lowest()),
    glm::uvec2(numeric_limits<unsigned int>::max()), glm::uvec2(1),
	DEFAULT_FROM_LUA_LAMBDA_2(glm::uvec2, glm::uvec2(0)),
	DEFAULT_TO_LUA_LAMBDA_2(glm::uvec2));

REGISTER_NUMERICALPROPERTY_SOURCE(UVec3Property, glm::uvec3, glm::uvec3(0),
    glm::uvec3(numeric_limits<unsigned int>::lowest()),
    glm::uvec3(numeric_limits<unsigned int>::max()), glm::uvec3(1),
	DEFAULT_FROM_LUA_LAMBDA_3(glm::uvec3, glm::uvec3(0)),
	DEFAULT_TO_LUA_LAMBDA_3(glm::uvec3));

REGISTER_NUMERICALPROPERTY_SOURCE(UVec4Property, glm::uvec4, glm::uvec4(0),
    glm::uvec4(numeric_limits<unsigned int>::lowest()),
    glm::uvec4(numeric_limits<unsigned int>::max()), glm::uvec4(1),
	DEFAULT_FROM_LUA_LAMBDA_4(glm::uvec4, glm::uvec4(0)),
	DEFAULT_TO_LUA_LAMBDA_4(glm::uvec4));


} // namespace properties
} // namespace openspace
