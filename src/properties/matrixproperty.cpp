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

#include <openspace/properties/matrixproperty.h>

#include <ghoul/misc/misc.h>

#include <limits>
#include <sstream>
#include <vector>

using std::numeric_limits;

namespace openspace {
namespace properties {

#define DEFAULT_FROM_LUA_LAMBDA(__TYPE__)                                                \
    [](lua_State* state, bool& success) -> __TYPE__ {                                    \
        __TYPE__ result;                                                                 \
        int number = 1;                                                                  \
        for (__TYPE__::size_type i = 0; i < __TYPE__::row_size(); ++i) {                 \
            for (__TYPE__::size_type j = 0; j < __TYPE__::col_size(); ++j) {             \
                lua_getfield(state, -1, std::to_string(number).c_str());                 \
                if (lua_isnumber(state, -1) != 1) {                                      \
                    success = false;                                                     \
                    return __TYPE__(0);                                                  \
                } else {                                                                 \
                    result[i][j]                                                         \
                          = static_cast<__TYPE__::value_type>(lua_tonumber(state, -1));  \
                    lua_pop(state, 1);                                                   \
                    ++number;                                                            \
                }                                                                        \
            }                                                                            \
        }                                                                                \
        success = true;                                                                  \
        return result;                                                                   \
    }

#define DEFAULT_TO_LUA_LAMBDA(__TYPE__)                                                  \
    [](lua_State* state, __TYPE__ value) -> bool {                                       \
        lua_newtable(state);                                                             \
        int number = 1;                                                                  \
        for (__TYPE__::size_type i = 0; i < __TYPE__::row_size(); ++i) {                 \
            for (__TYPE__::size_type j = 0; j < __TYPE__::col_size(); ++j) {             \
                lua_pushnumber(state, static_cast<lua_Number>(value[i][j]));             \
                lua_setfield(state, -2, std::to_string(number).c_str());                 \
                ++number;                                                                \
            }                                                                            \
        }                                                                                \
        return true;                                                                     \
    }

#define DEFAULT_FROM_STRING_LAMBDA(__TYPE__)                                             \
    [](std::string value, bool& success) -> __TYPE__ {                                   \
        __TYPE__ result;                                                                 \
        std::vector<std::string> tokens = ghoul::tokenizeString(value, ',');             \
        if (tokens.size() != (__TYPE__::row_size() * __TYPE__::col_size())) {            \
            success = false;                                                             \
            return result;                                                               \
        }                                                                                \
        int number = 0;                                                                  \
        for (__TYPE__::size_type i = 0; i < __TYPE__::row_size(); ++i) {                 \
            for (__TYPE__::size_type j = 0; j < __TYPE__::col_size(); ++j) {             \
                std::stringstream s(tokens[number]);                                     \
                __TYPE__::value_type v;                                                  \
                s >> v;                                                                  \
                if (s.fail()) {                                                          \
                    success = false;                                                     \
                    return result;                                                       \
                }                                                                        \
                else {                                                                   \
                    result[i][j] = v;                                                    \
                    ++number;                                                            \
                }                                                                        \
            }                                                                            \
        }                                                                                \
        success = true;                                                                  \
        return result;                                                                   \
    }

#define DEFAULT_TO_STRING_LAMBDA(__TYPE__)                                               \
    [](std::string& outValue, __TYPE__ inValue) -> bool {                                \
        outValue = "";                                                                   \
        for (__TYPE__::size_type i = 0; i < __TYPE__::row_size(); ++i) {                 \
            for (__TYPE__::size_type j = 0; j < __TYPE__::col_size(); ++j) {             \
                outValue += std::to_string(inValue[i][j]) + ",";                         \
            }                                                                            \
        }                                                                                \
        return true;                                                                     \
    }

REGISTER_NUMERICALPROPERTY_SOURCE(Mat2Property, glm::mat2x2, glm::mat2x2(0),
                                  glm::mat2x2(
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest()
								  ),
                                  glm::mat2x2(
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max()
								  ),
                                  glm::mat2x2(
									0.01f, 0.01f,
									0.01f, 0.01f
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::mat2x2),
                                  DEFAULT_TO_LUA_LAMBDA(glm::mat2x2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::mat2x2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::mat2x2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat2x3Property, glm::mat2x3, glm::mat2x3(0),
                                  glm::mat2x3(
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest()
								  ),
                                  glm::mat2x3(
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max()
								  ),
                                  glm::mat2x3(
									0.01f, 0.01f, 0.01f,
									0.01f, 0.01f, 0.01f
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::mat2x3),
                                  DEFAULT_TO_LUA_LAMBDA(glm::mat2x3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::mat2x3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::mat2x3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat2x4Property, glm::mat2x4, glm::mat2x4(0),
                                  glm::mat2x4(
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest()
								  ),
                                  glm::mat2x4(
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max()
								  ),
                                  glm::mat2x4(
									0.01f, 0.01f, 0.01f, 0.01f,
									0.01f, 0.01f, 0.01f, 0.01f
							      ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::mat2x4),
                                  DEFAULT_TO_LUA_LAMBDA(glm::mat2x4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::mat2x4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::mat2x4),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat3x2Property, glm::mat3x2, glm::mat3x2(0),
                                  glm::mat3x2(
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest()
								  ),
                                  glm::mat3x2(
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max()
								  ),
                                  glm::mat3x2(
									0.01f, 0.01f, 0.01f,
									0.01f, 0.01f, 0.01f
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::mat3x2),
                                  DEFAULT_TO_LUA_LAMBDA(glm::mat3x2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::mat3x2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::mat3x2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat3Property, glm::mat3x3, glm::mat3x3(0),
                                  glm::mat3x3(
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest()
								  ),
                                  glm::mat3x3(
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max()
								  ),
                                  glm::mat3x3(
									0.01f, 0.01f, 0.01f,
									0.01f, 0.01f, 0.01f,
									0.01f, 0.01f, 0.01f
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::mat3x3),
                                  DEFAULT_TO_LUA_LAMBDA(glm::mat3x3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::mat3x3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::mat3x3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat3x4Property, glm::mat3x4, glm::mat3x4(0),
                                  glm::mat3x4(
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest()
								  ),
                                  glm::mat3x4(
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max()
								  ),
                                  glm::mat3x4(
									0.01f, 0.01f, 0.01f, 0.01f, 
									0.01f, 0.01f, 0.01f, 0.01f, 
									0.01f, 0.01f, 0.01f, 0.01f
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::mat3x4),
                                  DEFAULT_TO_LUA_LAMBDA(glm::mat3x4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::mat3x4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::mat3x4),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat4x2Property, glm::mat4x2, glm::mat4x2(0),
                                  glm::mat4x2(
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest()
								  ),
                                  glm::mat4x2(
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max()
								  ),
                                  glm::mat4x2(
									0.01f, 0.01f, 0.01f, 0.01f, 
									0.01f, 0.01f, 0.01f, 0.01f
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::mat4x2),
                                  DEFAULT_TO_LUA_LAMBDA(glm::mat4x2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::mat4x2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::mat4x2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat4x3Property, glm::mat4x3, glm::mat4x3(0),
                                  glm::mat4x3(
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest()
								  ),
                                  glm::mat4x3(
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max()
								  ),
                                  glm::mat4x3(
									0.01f, 0.01f, 0.01f, 0.01f, 
									0.01f, 0.01f, 0.01f, 0.01f, 
									0.01f, 0.01f, 0.01f, 0.01f
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::mat4x3),
                                  DEFAULT_TO_LUA_LAMBDA(glm::mat4x3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::mat4x3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::mat4x3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(Mat4Property, glm::mat4x4, glm::mat4x4(0),
                                  glm::mat4x4(
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest(),
									numeric_limits<float>::lowest()
								  ),
                                  glm::mat4x4(
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max(),
									numeric_limits<float>::max()
								  ),
                                  glm::mat4x4(
									0.01f, 0.01f, 0.01f, 0.01f, 
									0.01f, 0.01f, 0.01f, 0.01f, 
									0.01f, 0.01f, 0.01f, 0.01f, 
									0.01f, 0.01f, 0.01f, 0.01f
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::mat4x4),
                                  DEFAULT_TO_LUA_LAMBDA(glm::mat4x4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::mat4x4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::mat4x4),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat2Property, glm::dmat2x2, glm::dmat2x2(0),
                                  glm::dmat2x2(
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest()
								  ),
                                  glm::dmat2x2(
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max()
								  ),
                                  glm::dmat2x2(
									0.01, 0.01, 
									0.01, 0.01
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dmat2x2),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dmat2x2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dmat2x2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dmat2x2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat2x3Property, glm::dmat2x3, glm::dmat2x3(0),
                                  glm::dmat2x3(
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest()
								  ),
                                  glm::dmat2x3(
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max()
								  ),
                                  glm::dmat2x3(
									0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dmat2x3),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dmat2x3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dmat2x3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dmat2x3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat2x4Property, glm::dmat2x4, glm::dmat2x4(0),
                                  glm::dmat2x4(
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest()
								  ),
                                  glm::dmat2x4(
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max()
								  ),
                                  glm::dmat2x4(
									0.01, 0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 0.01
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dmat2x4),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dmat2x4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dmat2x4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dmat2x4),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat3x2Property, glm::dmat3x2, glm::dmat3x2(0),
                                  glm::dmat3x2(
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest()
								  ),
                                  glm::dmat3x2(
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max()
								  ),
                                  glm::dmat3x2(
									0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dmat3x2),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dmat3x2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dmat3x2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dmat3x2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat3Property, glm::dmat3x3, glm::dmat3x3(0),
                                  glm::dmat3x3(
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest()
								  ),
                                  glm::dmat3x3(
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max()
								  ),
                                  glm::dmat3x3(
									0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dmat3x3),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dmat3x3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dmat3x3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dmat3x3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat3x4Property, glm::dmat3x4, glm::dmat3x4(0),
                                  glm::dmat3x4(
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest()
								  ),
                                  glm::dmat3x4(
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max()
								  ),
                                  glm::dmat3x4(
									0.01, 0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 0.01
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dmat3x4),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dmat3x4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dmat3x4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dmat3x4),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat4x2Property, glm::dmat4x2, glm::dmat4x2(0),
                                  glm::dmat4x2(
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest()
								  ),
                                  glm::dmat4x2(
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max()
								  ),
                                  glm::dmat4x2(
									0.01, 0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 0.01
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dmat4x2),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dmat4x2),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dmat4x2),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dmat4x2),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat4x3Property, glm::dmat4x3, glm::dmat4x3(0),
                                  glm::dmat4x3(
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest()
								  ),
                                  glm::dmat4x3(
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max()
								  ),
                                  glm::dmat4x3(
									0.01, 0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 0.01
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dmat4x3),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dmat4x3),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dmat4x3),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dmat4x3),
                                  LUA_TTABLE);

REGISTER_NUMERICALPROPERTY_SOURCE(DMat4Property, glm::dmat4x4, glm::dmat4x4(0),
                                  glm::dmat4x4(
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest(),
									numeric_limits<double>::lowest()
								  ),
                                  glm::dmat4x4(
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max(),
									numeric_limits<double>::max()
								  ),
                                  glm::dmat4x4(
									0.01, 0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 0.01, 
									0.01, 0.01, 0.01, 0.01
								  ),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::dmat4x4),
                                  DEFAULT_TO_LUA_LAMBDA(glm::dmat4x4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::dmat4x4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::dmat4x4),
                                  LUA_TTABLE);

}  // namespace properties
}  // namespace openspace
