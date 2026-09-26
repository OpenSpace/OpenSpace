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

#ifndef __GHOUL___GHOUL_LUA_TYPES___H__
#define __GHOUL___GHOUL_LUA_TYPES___H__

#include <cstdint>

namespace ghoul::lua {

/**
 * Supported Lua types.The values are powers of two in order to be able to combine them to
 * represent function that can take multiple options.
 */
enum LuaTypes : uint16_t {
    None = 0,
    Nil = 1,
    Boolean = 2,
    LightUserData = 4,
    Number = 8,
    String = 16,
    Table = 32,
    Function = 64,
    UserData = 128,
    Thread = 256
};

LuaTypes fromLuaType(int type);

bool typeMatch(LuaTypes lhs, LuaTypes rhs) noexcept;

} // namespace ghoul::lua

#endif // __GHOUL___GHOUL_LUA_TYPES___H__
