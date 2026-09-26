/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/lua/lua_types.h>

#include <ghoul/lua/ghoul_lua.h>
#include <stdexcept>

namespace ghoul::lua {

LuaTypes fromLuaType(int type) {
    switch (type) {
        case LUA_TNONE:          return LuaTypes::None;
        case LUA_TNIL:           return LuaTypes::Nil;
        case LUA_TBOOLEAN:       return LuaTypes::Boolean;
        case LUA_TLIGHTUSERDATA: return LuaTypes::LightUserData;
        case LUA_TNUMBER:        return LuaTypes::Number;
        case LUA_TSTRING:        return LuaTypes::String;
        case LUA_TTABLE:         return LuaTypes::Table;
        case LUA_TFUNCTION:      return LuaTypes::Function;
        case LUA_TUSERDATA:      return LuaTypes::UserData;
        case LUA_TTHREAD:        return LuaTypes::Thread;
        default:                 throw std::logic_error("Lua only has 9 types");
    }
}

bool typeMatch(LuaTypes lhs, LuaTypes rhs) noexcept {
    return (lhs & rhs) != 0;
}

} // namespace ghoul::lua
