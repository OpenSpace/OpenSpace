/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/properties/scalar/wcharproperty.h>

#include <ghoul/lua/ghoul_lua.h>

#include <limits>
#include <sstream>

using std::numeric_limits;

namespace openspace {
namespace properties {

#define DEFAULT_FROM_LUA_LAMBDA(TYPE, DEFAULT_VALUE)                                     \
    [](lua_State* state, bool& success) -> TYPE {                                        \
        success = (lua_isnumber(state, -1) == 1);                                        \
        if (success) {                                                                   \
            return static_cast<TYPE>(lua_tonumber(state, -1));                           \
        }                                                                                \
        else {                                                                           \
            return DEFAULT_VALUE;                                                        \
        }                                                                                \
    }

#define DEFAULT_TO_LUA_LAMBDA(TYPE)                                                      \
    [](lua_State* state, TYPE value) -> bool {                                           \
        lua_pushnumber(state, static_cast<lua_Number>(value));                           \
        return true;                                                                     \
    }

#define DEFAULT_FROM_STRING_LAMBDA(TYPE, DEFAULT_VALUE)                                  \
    [](std::string value, bool& success) -> TYPE {                                       \
        std::stringstream s(value);                                                      \
        TYPE v;                                                                          \
        s >> v;                                                                          \
        success = !s.fail();                                                             \
        if (success) {                                                                   \
            return v;                                                                    \
        }                                                                                \
    }

//REGISTER_NUMERICALPROPERTY_SOURCE(WCharProperty, wchar_t, wchar_t(0),
//                                  numeric_limits<wchar_t>::lowest(),
//                                  numeric_limits<wchar_t>::max(), wchar_t(1),
//                                  DEFAULT_FROM_LUA_LAMBDA(wchar_t, wchar_t(0)),
//                                  DEFAULT_TO_LUA_LAMBDA(wchar_t),
//                                  DEFAULT_FROM_STRING_LAMBDA(wchar_t, wchar_t(0)),
//                                  DEFAULT_TO_STRING_LAMBDA(wchar_t),
//                                  LUA_TNUMBER);

}  // namespace properties
} // namespace openspace
