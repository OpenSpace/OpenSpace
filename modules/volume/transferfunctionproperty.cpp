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
    [](lua_State* state, bool leaveOnStack, bool& success) -> TYPE {                     \
        TYPE TF;                                                                         \
        success = true;                                                                  \
        /*TODO: Actually keep envelopes on lua stack*/                                   \
        TF.setEnvelopesFromLua(state);                                                   \
        return TF;                                                                       \
    }

#define DEFAULT_TO_LUA_LAMBDA(TYPE)                                                      \
    [](lua_State* state, TYPE value) -> bool {                                           \
        bool success = value.getEnvelopesToLua(state);                                   \
        return success;                                                                  \
    }

#define DEFAULT_FROM_STRING_LAMBDA(TYPE, DEFAULT_VALUE)                                  \
    [](std::string val, bool& success) -> TYPE {                                         \
        TYPE TF;                                                                         \
        success = TF.setEnvelopesFromString(val);                                        \
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
