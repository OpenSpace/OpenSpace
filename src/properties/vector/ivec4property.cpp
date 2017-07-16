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

#include <openspace/properties/vector/ivec4property.h>

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/glm.h>
#include <ghoul/misc/misc.h>

#include <limits>

using std::numeric_limits;

namespace openspace::properties {

#define DEFAULT_FROM_LUA_LAMBDA(__TYPE__, __CONVFUNC__, __TESTFUNC__)                    \
    [](lua_State * state, bool& success) -> __TYPE__ {                                   \
        __TYPE__ result;                                                                 \
        lua_pushnil(state);                                                              \
        for (glm::length_t i = 0; i < ghoul::glm_components<__TYPE__>::value; ++i) {     \
            int hasNext = lua_next(state, -2);                                           \
            if (hasNext != 1) {                                                          \
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
        for (glm::length_t i = 0; i < ghoul::glm_components<__TYPE__>::value; ++i) {     \
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
        if (tokens.size() != static_cast<size_t>(result.length())) {                     \
            success = false;                                                             \
            return result;                                                               \
        }                                                                                \
        for (glm::length_t i = 0; i < ghoul::glm_components<__TYPE__>::value; ++i) {     \
                std::stringstream s(tokens[i]);                                          \
                __TYPE__::value_type v;                                                  \
                s >> v;                                                                  \
                if (s.fail()) {                                                          \
                    success = false;                                                     \
                    return result;                                                       \
                }                                                                        \
                else {                                                                   \
                    result[i] = v;                                                       \
                }                                                                        \
        }                                                                                \
        success = true;                                                                  \
        return result;                                                                   \
    }

#define DEFAULT_TO_STRING_LAMBDA(__TYPE__)                                               \
    [](std::string& outValue, __TYPE__ inValue) -> bool {                                \
        outValue = "{";                                                                  \
        for (glm::length_t i = 0; i < ghoul::glm_components<__TYPE__>::value; ++i) {     \
            outValue += std::to_string(inValue[i]) + ",";                                \
        }                                                                                \
        outValue.pop_back();                                                             \
        outValue += "}";                                                                 \
        return true;                                                                     \
    }

REGISTER_NUMERICALPROPERTY_SOURCE(IVec4Property, glm::ivec4, glm::ivec4(0),
                                  glm::ivec4(numeric_limits<int>::lowest()),
                                  glm::ivec4(numeric_limits<int>::max()), glm::ivec4(1),
                                  DEFAULT_FROM_LUA_LAMBDA(glm::ivec4, lua_tonumber,
                                                          lua_isnumber),
                                  DEFAULT_TO_LUA_LAMBDA(glm::ivec4),
                                  DEFAULT_FROM_STRING_LAMBDA(glm::ivec4),
                                  DEFAULT_TO_STRING_LAMBDA(glm::ivec4),
                                  LUA_TTABLE);

} // namespace openspace::properties
