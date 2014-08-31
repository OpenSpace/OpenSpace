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

#ifndef __PROPERTYDELEGATE_H__
#define __PROPERTYDELEGATE_H__

#include <string>

struct lua_State;

namespace openspace {
namespace properties {

template <typename T>
class PropertyDelegate {
public:
    static std::string className();

    template <typename U>
    static U defaultValue();

    template <typename U>
    static U defaultMinimumValue();

    template <typename U>
    static U defaultMaximumValue();

	template <typename U>
	static U fromLuaValue(lua_State* state, bool& success);

	template <typename U>
	static bool toLuaValue(lua_State* state, U value);
};

} // namespace properties
} // namespace openspace

#include <openspace/properties/propertydelegate.inl>

#endif // __PROPERTYDELEGATE_H__
