/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/properties/matrix/mat4x2property.h>

#include <ghoul/misc/misc.h>

#include <limits>
#include <sstream>
#include <vector>

using std::numeric_limits;

namespace openspace::properties {

#define DEFAULT_FROM_LUA_LAMBDA(__TYPE__)                                                \
    [](lua_State* state, bool& success) -> __TYPE__ {                                    \
        __TYPE__ result;                                                                 \
        int number = 1;                                                                  \
        for (glm::length_t i = 0; i < ghoul::glm_cols<__TYPE__>::value; ++i) {           \
            for (glm::length_t j = 0; j < ghoul::glm_rows<__TYPE__>::value; ++j) {       \
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
        for (glm::length_t i = 0; i < ghoul::glm_cols<__TYPE__>::value; ++i) {           \
            for (glm::length_t j = 0; j < ghoul::glm_rows<__TYPE__>::value; ++j) {       \
                lua_pushnumber(state, static_cast<lua_Number>(value[i][j]));             \
                lua_setfield(state, -2, std::to_string(number).c_str());                 \
                ++number;                                                                \
            }                                                                            \
        }                                                                                \
        return true;                                                                     \
    }

#define DEFAULT_FROM_STRING_LAMBDA(__TYPE__)                                             \
    [](std::string v, bool& success) -> __TYPE__ {                                       \
        __TYPE__ result;                                                                 \
        std::vector<std::string> tokens = ghoul::tokenizeString(v, ',');                 \
        if (tokens.size() !=                                                             \
            (ghoul::glm_rows<__TYPE__>::value * ghoul::glm_cols<__TYPE__>::value))       \
        {                                                                                \
            success = false;                                                             \
            return result;                                                               \
        }                                                                                \
        int number = 0;                                                                  \
        for (glm::length_t i = 0; i < ghoul::glm_cols<__TYPE__>::value; ++i) {           \
            for (glm::length_t j = 0; j < ghoul::glm_rows<__TYPE__>::value; ++j) {       \
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
        for (glm::length_t i = 0; i < ghoul::glm_cols<__TYPE__>::value; ++i) {           \
            for (glm::length_t j = 0; j < ghoul::glm_rows<__TYPE__>::value; ++j) {       \
                outValue += std::to_string(inValue[i][j]) + ",";                         \
            }                                                                            \
            outValue.pop_back();                                                         \
        }                                                                                \
        return true;                                                                     \
    }

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

}  // namespace openspace::properties
