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

#include "openspace/properties/scalarproperty.h"

#include <ghoul/lua/ghoul_lua.h>

#include <limits>

using std::numeric_limits;

namespace openspace {
namespace properties {

#define DEFAULT_FROM_LUA_LAMBDA(TYPE, DEFAULT_VALUE) \
	[](lua_State* state, bool& success) -> TYPE { \
		success = (lua_isnumber(state, -1) == 1); \
		if (success) \
			return static_cast<TYPE>(lua_tonumber(state, -1)); \
		else \
			return DEFAULT_VALUE; \
	}

#define DEFAULT_TO_LUA_LAMBDA(TYPE) \
	[](lua_State* state, TYPE value) -> bool { \
		lua_pushnumber(state, static_cast<lua_Number>(value)); \
		return true; \
	}

// char16_t and char32_t are not supported on Visual Studio 2013 and are defined to
// be equal to unsigned short and unsigned int which causes a compile error

REGISTER_TEMPLATEPROPERTY_SOURCE(BoolProperty, bool, false);

REGISTER_NUMERICALPROPERTY_SOURCE(CharProperty, char, char(0),
    numeric_limits<char>::lowest(), numeric_limits<char>::max(), char(1),
	DEFAULT_FROM_LUA_LAMBDA(char, char(0)), DEFAULT_TO_LUA_LAMBDA(char));

//REGISTER_NUMERICALPROPERTY_SOURCE(Char16Property, char16_t, char16_t(0),
//    numeric_limits<char16_t>::lowest(), numeric_limits<char16_t>::max(), char16_t(1));

//REGISTER_NUMERICALPROPERTY_SOURCE(Char32Property, char32_t, char32_t(0),
//    numeric_limits<char32_t>::lowest(), numeric_limits<char32_t>::max(), char32_t(1));

REGISTER_NUMERICALPROPERTY_SOURCE(WCharProperty, wchar_t, wchar_t(0),
    numeric_limits<wchar_t>::lowest(), numeric_limits<wchar_t>::max(), wchar_t(1),
	DEFAULT_FROM_LUA_LAMBDA(wchar_t, wchar_t(0)), DEFAULT_TO_LUA_LAMBDA(wchar_t));

REGISTER_NUMERICALPROPERTY_SOURCE(SignedCharProperty, signed char, (signed char)(0),
    numeric_limits<signed char>::lowest(), numeric_limits<signed char>::max(),
    (signed char)0, DEFAULT_FROM_LUA_LAMBDA(signed char, (signed char)(0)),
	DEFAULT_TO_LUA_LAMBDA(signed char));

REGISTER_NUMERICALPROPERTY_SOURCE(UCharProperty, unsigned char, (unsigned char)0,
    numeric_limits<unsigned char>::lowest(), numeric_limits<unsigned char>::max(),
	(unsigned char)1, DEFAULT_FROM_LUA_LAMBDA(unsigned char, (unsigned char)(0)),
	DEFAULT_TO_LUA_LAMBDA(unsigned char));

REGISTER_NUMERICALPROPERTY_SOURCE(ShortProperty, short, short(0),
    numeric_limits<short>::lowest(), numeric_limits<short>::max(), short(1),
	DEFAULT_FROM_LUA_LAMBDA(short, short(0)), DEFAULT_TO_LUA_LAMBDA(short));

REGISTER_NUMERICALPROPERTY_SOURCE(UShortProperty, unsigned short, (unsigned short)(0),
    numeric_limits<unsigned short>::lowest(), numeric_limits<unsigned short>::max(),
    (unsigned short)1, DEFAULT_FROM_LUA_LAMBDA(unsigned short, (unsigned short)(0)),
	DEFAULT_TO_LUA_LAMBDA(unsigned short));

REGISTER_NUMERICALPROPERTY_SOURCE(IntProperty, int, int(0),
    numeric_limits<int>::lowest(), numeric_limits<int>::max(), int(1),
	DEFAULT_FROM_LUA_LAMBDA(int, int(0)), DEFAULT_TO_LUA_LAMBDA(int));

REGISTER_NUMERICALPROPERTY_SOURCE(UIntProperty, unsigned int, (unsigned int)0,
    numeric_limits<unsigned int>::lowest(), numeric_limits<unsigned int>::max(),
    (unsigned int)1, DEFAULT_FROM_LUA_LAMBDA(unsigned int, (unsigned int)(0)),
	DEFAULT_TO_LUA_LAMBDA(unsigned int));

REGISTER_NUMERICALPROPERTY_SOURCE(LongProperty, long, long(0),
    numeric_limits<long>::lowest(), numeric_limits<long>::max(), long(1),
	DEFAULT_FROM_LUA_LAMBDA(long, long(0)), DEFAULT_TO_LUA_LAMBDA(long));

REGISTER_NUMERICALPROPERTY_SOURCE(ULongProperty, unsigned long, (unsigned long)0,
    numeric_limits<unsigned long>::lowest(), numeric_limits<unsigned long>::max(),
    (unsigned long)1, DEFAULT_FROM_LUA_LAMBDA(unsigned long, (unsigned long)(0)),
	DEFAULT_TO_LUA_LAMBDA(unsigned long));

REGISTER_NUMERICALPROPERTY_SOURCE(LongLongProperty, long long, (long long)0,
    numeric_limits<long long>::lowest(), numeric_limits<long long>::max(), (long long)1,
	DEFAULT_FROM_LUA_LAMBDA(long long, (long long)(0)),
	DEFAULT_TO_LUA_LAMBDA(long long));

REGISTER_NUMERICALPROPERTY_SOURCE(ULongLongProperty, unsigned long long,
    (unsigned long long)1, numeric_limits<unsigned long long>::lowest(),
    numeric_limits<unsigned long long>::max(), (unsigned long long)1,
	DEFAULT_FROM_LUA_LAMBDA(unsigned long long, (unsigned long long)(0)),
	DEFAULT_TO_LUA_LAMBDA(unsigned long long));

REGISTER_NUMERICALPROPERTY_SOURCE(FloatProperty, float, 0.f,
    numeric_limits<float>::lowest(), numeric_limits<float>::max(), 0.01f,
	DEFAULT_FROM_LUA_LAMBDA(float, float(0)), DEFAULT_TO_LUA_LAMBDA(float));

REGISTER_NUMERICALPROPERTY_SOURCE(DoubleProperty, double, 0.0,
    numeric_limits<double>::lowest(), numeric_limits<double>::max(), 0.01,
	DEFAULT_FROM_LUA_LAMBDA(double, double(0)), DEFAULT_TO_LUA_LAMBDA(double));

REGISTER_NUMERICALPROPERTY_SOURCE(LongDoubleProperty, long double, (long double)0,
    numeric_limits<long double>::lowest(), numeric_limits<long double>::max(),
    (long double)0.01f,
	DEFAULT_FROM_LUA_LAMBDA(long double, (long double)(0)),
	DEFAULT_TO_LUA_LAMBDA(long double));

} // namespace properties
} // namespace openspace
