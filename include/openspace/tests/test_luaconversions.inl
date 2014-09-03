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

#include "gtest/gtest.h"

#include <ghoul/lua/ghoul_lua.h>
#include <openspace/properties/propertydelegate.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/matrixproperty.h>
#include <openspace/properties/stringproperty.h>

class LuaConversionTest : public testing::Test {
protected:
	lua_State* state;

	LuaConversionTest() {
		state = luaL_newstate();
		luaL_openlibs(state);
    }

	~LuaConversionTest() {
		lua_close(state);
    }

	void reset() {
		lua_close(state);
		state = luaL_newstate();
		luaL_openlibs(state);
	}
};

TEST_F(LuaConversionTest, LuaExecution) {
	int status = luaL_loadstring(state, "");
	EXPECT_EQ(status, LUA_OK);
}

#define CONVERSION_TEST_TEMPLATE(__NAME__, __TYPE__, __VALUE__)                          \
    TEST_F(LuaConversionTest, __NAME__)                                                  \
    {                                                                                    \
        using namespace openspace::properties;                                           \
        bool success                                                                     \
              = PropertyDelegate<TemplateProperty<__TYPE__>>::toLuaValue<__TYPE__>(      \
                    state, __VALUE__);                                                   \
        EXPECT_TRUE(success) << "toLuaValue";                                            \
        __TYPE__ value = __TYPE__(0);                                                    \
        value = PropertyDelegate<TemplateProperty<__TYPE__>>::fromLuaValue<__TYPE__>(    \
              state, success);                                                           \
        EXPECT_TRUE(success) << "fromLuaValue";                                          \
        EXPECT_EQ(value, __VALUE__) << "fromLuaValue";                                   \
    }

#define CONVERSION_TEST_NUMERICAL(__NAME__, __TYPE__, __VALUE__)                         \
    TEST_F(LuaConversionTest, __NAME__)                                                  \
    {                                                                                    \
        using namespace openspace::properties;                                           \
        bool success                                                                     \
              = PropertyDelegate<NumericalProperty<__TYPE__>>::toLuaValue<__TYPE__>(     \
                    state, __VALUE__);                                                   \
        EXPECT_TRUE(success) << "toLuaValue";                                            \
        __TYPE__ value = __TYPE__(0);                                                    \
        value = PropertyDelegate<NumericalProperty<__TYPE__>>::fromLuaValue<__TYPE__>(   \
              state, success);                                                           \
        EXPECT_TRUE(success) << "fromLuaValue";                                          \
        EXPECT_EQ(value, __VALUE__) << "fromLuaValue";                                   \
    }

CONVERSION_TEST_TEMPLATE(Bool, bool, true);

CONVERSION_TEST_NUMERICAL(Char, char, 1);
CONVERSION_TEST_NUMERICAL(WChar, wchar_t, 1);
CONVERSION_TEST_NUMERICAL(SignedChar, signed char, 1);
CONVERSION_TEST_NUMERICAL(UnsignedChar, unsigned char, 1);
CONVERSION_TEST_NUMERICAL(Short, short, 1);
CONVERSION_TEST_NUMERICAL(UnsignedShort, unsigned short, 1);
CONVERSION_TEST_NUMERICAL(Int, int, 1);
CONVERSION_TEST_NUMERICAL(UnsignedInt, unsigned int, 1);
CONVERSION_TEST_NUMERICAL(Long, long, 1);
CONVERSION_TEST_NUMERICAL(UnsignedLong, unsigned long, 1);
CONVERSION_TEST_NUMERICAL(LongLong, long long, 1);
CONVERSION_TEST_NUMERICAL(UnsignedLongLong, unsigned long long, 1);
CONVERSION_TEST_NUMERICAL(Float, float, 1.f);
CONVERSION_TEST_NUMERICAL(Double, double, 1.0);
CONVERSION_TEST_NUMERICAL(LongDouble, long double, 1.0);

CONVERSION_TEST_NUMERICAL(Vec2, glm::vec2, glm::vec2(1.f));
CONVERSION_TEST_NUMERICAL(Vec3, glm::vec3, glm::vec3(1.f));
CONVERSION_TEST_NUMERICAL(Vec4, glm::vec4, glm::vec4(1.f));
CONVERSION_TEST_NUMERICAL(DVec2, glm::dvec2, glm::dvec2(1.0));
CONVERSION_TEST_NUMERICAL(DVec3, glm::dvec3, glm::dvec3(1.0));
CONVERSION_TEST_NUMERICAL(DVec4, glm::dvec4, glm::dvec4(1.0));
CONVERSION_TEST_NUMERICAL(IVec2, glm::ivec2, glm::ivec2(1));
CONVERSION_TEST_NUMERICAL(IVec3, glm::ivec3, glm::ivec3(1));
CONVERSION_TEST_NUMERICAL(IVec4, glm::ivec4, glm::ivec4(1));
CONVERSION_TEST_NUMERICAL(UVec2, glm::uvec2, glm::uvec2(1));
CONVERSION_TEST_NUMERICAL(UVec3, glm::uvec3, glm::uvec3(1));
CONVERSION_TEST_NUMERICAL(UVec4, glm::uvec4, glm::uvec4(1));

CONVERSION_TEST_NUMERICAL(Mat2x2, glm::mat2x2, glm::mat2x2(1.f));
CONVERSION_TEST_NUMERICAL(Mat2x3, glm::mat2x3, glm::mat2x3(1.f));
CONVERSION_TEST_NUMERICAL(Mat2x4, glm::mat2x4, glm::mat2x4(1.f));
CONVERSION_TEST_NUMERICAL(Mat3x2, glm::mat3x2, glm::mat3x2(1.f));
CONVERSION_TEST_NUMERICAL(Mat3x3, glm::mat3x3, glm::mat3x3(1.f));
CONVERSION_TEST_NUMERICAL(Mat3x4, glm::mat3x4, glm::mat3x4(1.f));
CONVERSION_TEST_NUMERICAL(Mat4x2, glm::mat4x2, glm::mat4x2(1.f));
CONVERSION_TEST_NUMERICAL(Mat4x3, glm::mat4x3, glm::mat4x3(1.f));
CONVERSION_TEST_NUMERICAL(Mat4x4, glm::mat4x4, glm::mat4x4(1.f));
CONVERSION_TEST_NUMERICAL(DMat2x2, glm::dmat2x2, glm::dmat2x2(1.f));
CONVERSION_TEST_NUMERICAL(DMat2x3, glm::dmat2x3, glm::dmat2x3(1.f));
CONVERSION_TEST_NUMERICAL(DMat2x4, glm::dmat2x4, glm::dmat2x4(1.f));
CONVERSION_TEST_NUMERICAL(DMat3x2, glm::dmat3x2, glm::dmat3x2(1.f));
CONVERSION_TEST_NUMERICAL(DMat3x3, glm::dmat3x3, glm::dmat3x3(1.f));
CONVERSION_TEST_NUMERICAL(DMat3x4, glm::dmat3x4, glm::dmat3x4(1.f));
CONVERSION_TEST_NUMERICAL(DMat4x2, glm::dmat4x2, glm::dmat4x2(1.f));
CONVERSION_TEST_NUMERICAL(DMat4x3, glm::dmat4x3, glm::dmat4x3(1.f));
CONVERSION_TEST_NUMERICAL(DMat4x4, glm::dmat4x4, glm::dmat4x4(1.f));

TEST_F(LuaConversionTest, String)
{
	using namespace openspace::properties;
	bool success
		  = PropertyDelegate<TemplateProperty<std::string>>::toLuaValue<std::string>(
				state, "value");
	EXPECT_TRUE(success) << "toLuaValue";
	std::string value = "";
	value = PropertyDelegate<TemplateProperty<std::string>>::fromLuaValue<std::string>(
		  state, success);
	EXPECT_TRUE(success) << "fromLuaValue";
	EXPECT_EQ(value, "value") << "fromLuaValue";
}