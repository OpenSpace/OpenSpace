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

#include <openspace/properties/matrixproperty.h>

#include <limits>

using std::numeric_limits;

namespace openspace {
namespace properties {

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
			return DEFAULT_VALUE;\
	}

#define DEFAULT_FROM_LUA_LAMBDA_6(TYPE, DEFAULT_VALUE) \
	[](lua_State* state, bool& success) -> TYPE { \
		success = (lua_isnumber(state, -1) == 1) && \
				  (lua_isnumber(state, -2) == 1) && \
				  (lua_isnumber(state, -3) == 1) && \
				  (lua_isnumber(state, -4) == 1) && \
				  (lua_isnumber(state, -5) == 1) && \
				  (lua_isnumber(state, -6) == 1); \
		if (success) \
			return TYPE( \
				static_cast<TYPE::value_type>(lua_tonumber(state, -1)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -2)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -3)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -4)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -5)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -6))); \
		else \
			return DEFAULT_VALUE;\
	}

#define DEFAULT_FROM_LUA_LAMBDA_8(TYPE, DEFAULT_VALUE) \
	[](lua_State* state, bool& success) -> TYPE { \
		success = (lua_isnumber(state, -1) == 1) && \
				  (lua_isnumber(state, -2) == 1) && \
				  (lua_isnumber(state, -3) == 1) && \
				  (lua_isnumber(state, -4) == 1) && \
				  (lua_isnumber(state, -5) == 1) && \
				  (lua_isnumber(state, -6) == 1) && \
				  (lua_isnumber(state, -7) == 1) && \
				  (lua_isnumber(state, -8) == 1); \
		if (success) \
			return TYPE( \
				static_cast<TYPE::value_type>(lua_tonumber(state, -1)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -2)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -3)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -4)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -5)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -6)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -7)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -8))); \
		else \
			return DEFAULT_VALUE;\
	}

#define DEFAULT_FROM_LUA_LAMBDA_9(TYPE, DEFAULT_VALUE) \
	[](lua_State* state, bool& success) -> TYPE { \
		success = (lua_isnumber(state, -1) == 1) && \
				  (lua_isnumber(state, -2) == 1) && \
				  (lua_isnumber(state, -3) == 1) && \
				  (lua_isnumber(state, -4) == 1) && \
				  (lua_isnumber(state, -5) == 1) && \
				  (lua_isnumber(state, -6) == 1) && \
				  (lua_isnumber(state, -7) == 1) && \
				  (lua_isnumber(state, -8) == 1) && \
				  (lua_isnumber(state, -9) == 1); \
		if (success) \
			return TYPE( \
				static_cast<TYPE::value_type>(lua_tonumber(state, -1)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -2)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -3)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -4)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -5)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -6)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -7)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -8)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -9))); \
		else \
			return DEFAULT_VALUE;\
	}

#define DEFAULT_FROM_LUA_LAMBDA_12(TYPE, DEFAULT_VALUE) \
	[](lua_State* state, bool& success) -> TYPE { \
		success = (lua_isnumber(state, -1) == 1) && \
				  (lua_isnumber(state, -2) == 1) && \
				  (lua_isnumber(state, -3) == 1) && \
				  (lua_isnumber(state, -4) == 1) && \
				  (lua_isnumber(state, -5) == 1) && \
				  (lua_isnumber(state, -6) == 1) && \
				  (lua_isnumber(state, -7) == 1) && \
				  (lua_isnumber(state, -8) == 1) && \
				  (lua_isnumber(state, -9) == 1) && \
				  (lua_isnumber(state, -10) == 1) && \
				  (lua_isnumber(state, -11) == 1) && \
				  (lua_isnumber(state, -12) == 1); \
		if (success) \
			return TYPE( \
				static_cast<TYPE::value_type>(lua_tonumber(state, -1)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -2)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -3)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -4)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -5)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -6)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -7)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -8)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -9)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -10)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -11)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -12))); \
		else \
			return DEFAULT_VALUE;\
}

#define DEFAULT_FROM_LUA_LAMBDA_16(TYPE, DEFAULT_VALUE) \
	[](lua_State* state, bool& success) -> TYPE { \
		success = (lua_isnumber(state, -1) == 1) && \
				  (lua_isnumber(state, -2) == 1) && \
				  (lua_isnumber(state, -3) == 1) && \
				  (lua_isnumber(state, -4) == 1) && \
				  (lua_isnumber(state, -5) == 1) && \
				  (lua_isnumber(state, -6) == 1) && \
				  (lua_isnumber(state, -7) == 1) && \
				  (lua_isnumber(state, -8) == 1) && \
				  (lua_isnumber(state, -9) == 1) && \
				  (lua_isnumber(state, -10) == 1) && \
				  (lua_isnumber(state, -11) == 1) && \
				  (lua_isnumber(state, -12) == 1) && \
				  (lua_isnumber(state, -13) == 1) && \
				  (lua_isnumber(state, -14) == 1) && \
				  (lua_isnumber(state, -15) == 1) && \
				  (lua_isnumber(state, -16) == 1); \
		if (success) \
			return TYPE( \
				static_cast<TYPE::value_type>(lua_tonumber(state, -1)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -2)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -3)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -4)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -5)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -6)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -7)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -8)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -9)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -10)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -11)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -12)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -13)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -14)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -15)), \
				static_cast<TYPE::value_type>(lua_tonumber(state, -16))); \
		else \
			return DEFAULT_VALUE;\
	}

REGISTER_NUMERICALPROPERTY_SOURCE(Mat2Property, glm::mat2x2, glm::mat2x2(0),
    glm::mat2x2(numeric_limits<float>::lowest()),
    glm::mat2x2(numeric_limits<float>::max()), glm::mat2x2(0.01f),
	DEFAULT_FROM_LUA_LAMBDA_4(glm::mat2x2, glm::mat2x2(0)),
	[](lua_State* state, glm::mat2x2 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat2x3Property, glm::mat2x3, glm::mat2x3(0),
    glm::mat2x3(numeric_limits<float>::lowest()),
    glm::mat2x3(numeric_limits<float>::max()), glm::mat2x3(0.01f),
	DEFAULT_FROM_LUA_LAMBDA_6(glm::mat2x3, glm::mat2x3(0)),
	[](lua_State* state, glm::mat2x3 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat2x4Property, glm::mat2x4, glm::mat2x4(0),
    glm::mat2x4(numeric_limits<float>::lowest()),
    glm::mat2x4(numeric_limits<float>::max()), glm::mat2x4(0.01f),
	DEFAULT_FROM_LUA_LAMBDA_8(glm::mat2x4, glm::mat2x4(0)),
	[](lua_State* state, glm::mat2x4 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][3]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat3x2Property, glm::mat3x2, glm::mat3x2(0),
    glm::mat3x2(numeric_limits<float>::lowest()),
    glm::mat3x2(numeric_limits<float>::max()), glm::mat3x2(0.01f),
	DEFAULT_FROM_LUA_LAMBDA_6(glm::mat3x2, glm::mat3x2(0)),
	[](lua_State* state, glm::mat3x2 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat3Property, glm::mat3x3, glm::mat3x3(0),
    glm::mat3x3(numeric_limits<float>::lowest()),
    glm::mat3x3(numeric_limits<float>::max()), glm::mat3x3(0.01f),
	DEFAULT_FROM_LUA_LAMBDA_9(glm::mat3x3, glm::mat3x3(0)),
	[](lua_State* state, glm::mat3x3 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][2]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat3x4Property, glm::mat3x4, glm::mat3x4(0),
    glm::mat3x4(numeric_limits<float>::lowest()),
    glm::mat3x4(numeric_limits<float>::max()), glm::mat3x4(0.01f),
	DEFAULT_FROM_LUA_LAMBDA_12(glm::mat3x4, glm::mat3x4(0)),
	[](lua_State* state, glm::mat3x4 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][3]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat4x2Property, glm::mat4x2, glm::mat4x2(0),
    glm::mat4x2(numeric_limits<float>::lowest()),
    glm::mat4x2(numeric_limits<float>::max()), glm::mat4x2(0.01f),
	DEFAULT_FROM_LUA_LAMBDA_8(glm::mat4x2, glm::mat4x2(0)),
	[](lua_State* state, glm::mat4x2 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][1]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat4x3Property, glm::mat4x3, glm::mat4x3(0),
    glm::mat4x3(numeric_limits<float>::lowest()),
    glm::mat4x3(numeric_limits<float>::max()), glm::mat4x3(0.01f),
	DEFAULT_FROM_LUA_LAMBDA_12(glm::mat4x3, glm::mat4x3(0)),
	[](lua_State* state, glm::mat4x3 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][2]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat4Property, glm::mat4x4, glm::mat4x4(0),
    glm::mat4x4(numeric_limits<float>::lowest()),
    glm::mat4x4(numeric_limits<float>::max()), glm::mat4x4(0.01f),
	DEFAULT_FROM_LUA_LAMBDA_16(glm::mat4x4, glm::mat4x4(0)),
	[](lua_State* state, glm::mat4x4 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][3]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat2Property, glm::dmat2x2, glm::dmat2x2(0),
    glm::dmat2x2(numeric_limits<double>::lowest()),
    glm::dmat2x2(numeric_limits<double>::max()), glm::dmat2x2(0.01),
	DEFAULT_FROM_LUA_LAMBDA_4(glm::dmat2x2, glm::dmat2x2(0)),
	[](lua_State* state, glm::dmat2x2 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat2x3Property, glm::dmat2x3, glm::dmat2x3(0),
    glm::dmat2x3(numeric_limits<double>::lowest()),
    glm::dmat2x3(numeric_limits<double>::max()), glm::dmat2x3(0.01),
	DEFAULT_FROM_LUA_LAMBDA_6(glm::dmat2x3, glm::dmat2x3(0)),
	[](lua_State* state, glm::dmat2x3 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat2x4Property, glm::dmat2x4, glm::dmat2x4(0),
    glm::dmat2x4(numeric_limits<double>::lowest()),
    glm::dmat2x4(numeric_limits<double>::max()), glm::dmat2x4(0.01),
	DEFAULT_FROM_LUA_LAMBDA_8(glm::dmat2x4, glm::dmat2x4(0)),
	[](lua_State* state, glm::dmat2x4 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][3]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat3x2Property, glm::dmat3x2, glm::dmat3x2(0),
    glm::dmat3x2(numeric_limits<double>::lowest()),
    glm::dmat3x2(numeric_limits<double>::max()), glm::dmat3x2(0.01),
	DEFAULT_FROM_LUA_LAMBDA_6(glm::dmat3x2, glm::dmat3x2(0)),
	[](lua_State* state, glm::dmat3x2 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat3Property, glm::dmat3x3, glm::dmat3x3(0),
    glm::dmat3x3(numeric_limits<double>::lowest()),
    glm::dmat3x3(numeric_limits<double>::max()), glm::dmat3x3(0.01),
	DEFAULT_FROM_LUA_LAMBDA_9(glm::dmat3x3, glm::dmat3x3(0)),
	[](lua_State* state, glm::dmat3x3 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][2]));
		return true;
	}
);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat3x4Property, glm::dmat3x4, glm::dmat3x4(0),
    glm::dmat3x4(numeric_limits<double>::lowest()),
    glm::dmat3x4(numeric_limits<double>::max()), glm::dmat3x4(0.01),
	DEFAULT_FROM_LUA_LAMBDA_12(glm::dmat3x4, glm::dmat3x4(0)),
	[](lua_State* state, glm::dmat3x4 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][3]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat4x2Property, glm::dmat4x2, glm::dmat4x2(0),
    glm::dmat4x2(numeric_limits<double>::lowest()),
    glm::dmat4x2(numeric_limits<double>::max()), glm::dmat4x2(0.01),
	DEFAULT_FROM_LUA_LAMBDA_8(glm::dmat4x2, glm::dmat4x2(0)),
	[](lua_State* state, glm::dmat4x2 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][1]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat4x3Property, glm::dmat4x3, glm::dmat4x3(0),
    glm::dmat4x3(numeric_limits<double>::lowest()),
    glm::dmat4x3(numeric_limits<double>::max()), glm::dmat4x3(0.01),
	DEFAULT_FROM_LUA_LAMBDA_12(glm::dmat4x3, glm::dmat4x3(0)),
	[](lua_State* state, glm::dmat4x3 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][2]));
		return true;
	}
	);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat4Property, glm::dmat4x4, glm::dmat4x4(0),
    glm::dmat4x4(numeric_limits<double>::lowest()),
    glm::dmat4x4(numeric_limits<double>::max()), glm::dmat4x4(0.01),
	DEFAULT_FROM_LUA_LAMBDA_16(glm::dmat4x4, glm::dmat4x4(0)),
	[](lua_State* state, glm::dmat4x4 value) -> bool {
		lua_pushnumber(state, static_cast<lua_Number>(value[0][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[0][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[1][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[2][3]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][0]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][1]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][2]));
		lua_pushnumber(state, static_cast<lua_Number>(value[3][3]));
		return true;
	}
	);

} // namespace properties
} // namespace openspace
