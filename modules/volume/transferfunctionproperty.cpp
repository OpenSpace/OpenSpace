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

#include <modules/volume/transferfunctionproperty.h>

#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

#define DEFAULT_FROM_LUA_LAMBDA(TYPE, DEFAULT_VALUE)                                     \
    [](lua_State* state, bool& success) -> TYPE {                                        \
        success = (lua_istable(state, -1) == 1);                                         \
        if(success) {                                                                    \
            lua_pushnil(state);                                                          \
            int index = lua_gettop(state);                                               \
            std::vector<std::string> results;                                            \
            while (lua_next(state, -1)) {                                                \
                auto something  = lua_tostring(state, -1);                               \
                results.push_back(something);                                            \
                lua_pop(state, 1);                                                       \
            }                                                                            \
        }                                                                                \
            return DEFAULT_VALUE;                                                        \
        }

#define DEFAULT_TO_LUA_LAMBDA(TYPE)                                                      \
    [](lua_State* state, TYPE value) -> bool {                                           \
        return true;                                                                     \
    }

#define DEFAULT_FROM_STRING_LAMBDA(TYPE, DEFAULT_VALUE)                                  \
    [](std::string val, bool& success) -> TYPE {                                         \
        TYPE TF(val);                                                                    \
        success = true;                                                                  \
        return TF;                                                                       \
    }

#define DEFAULT_TO_STRING_LAMBDA(TYPE)                                                   \
    [](std::string& outValue, TYPE inValue) -> bool {                                    \
        outValue = inValue.getSerializedToString();                                      \
        return true;                                                                     \
    }

    REGISTER_TEMPLATEPROPERTY_SOURCE(TransferFunctionProperty, volume::TransferFunction, 0,
        DEFAULT_FROM_LUA_LAMBDA(volume::TransferFunction, volume::TransferFunction(0)),
        DEFAULT_TO_LUA_LAMBDA(volume::TransferFunction),
        DEFAULT_FROM_STRING_LAMBDA(volume::TransferFunction, volume::TransferFunction(0)),
        DEFAULT_TO_STRING_LAMBDA(volume::TransferFunction),
        LUA_TTABLE);
} // namespace openspace::properties
