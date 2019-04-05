/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/properties/binaryproperty.h>

#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

REGISTER_TEMPLATEPROPERTY_SOURCE(
    BinaryProperty,
    std::vector<char>,
    std::vector<char>(0),
    [](lua_State* state, bool& success) -> std::vector<char> {
        // TODO: Convert from lua
        std::vector<char> result;
        success = true;
        return result;
    },
    [](lua_State* state, std::vector<char>) -> bool {
        // TODO: Convert to lua
        return true;
    },
    [](std::string value, bool& success) -> std::vector<char> {
        // TODO: From json conversion
        std::vector<char> result;
        success = true;
        return result;
    },
    [](std::string& outValue, std::vector<char> inValue) -> bool {
        // TODO: To json conversion
        outValue = "";
        return true;
    },
    LUA_TTABLE
)

} // namespace openspace::properties
