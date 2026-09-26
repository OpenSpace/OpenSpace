/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
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

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_template_test_macros.hpp>
#include <ghoul/glm.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <random>
#include <iostream>

namespace {
    constexpr int NumberFuzzTests = 1000;
} // namespace

TEST_CASE("LuaConversion: LuaExecution", "[luaconversion]") {
    lua_State* state = luaL_newstate();
    luaL_openlibs(state);

    const int status = luaL_loadstring(state, "");
    CHECK(status == LUA_OK);

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion", "[luaconversion]", bool, char, signed char,
                   unsigned char, short, unsigned short, int, unsigned int, long,
                   unsigned long, long long, unsigned long long, float, double,
                   long double)
{
    using T = TestType;

    lua_State* state = luaL_newstate();
    luaL_openlibs(state);

    const T val(1);

    ghoul::lua::push(state, val);

    const T value = ghoul::lua::value<T>(state);
    CHECK(value == Catch::Approx(val));

    lua_close(state);
}

TEST_CASE("LuaConversion: Const char*", "[luaconversion]") {
    lua_State* state = luaL_newstate();

    ghoul::lua::push(state, "value");

    const char* value = ghoul::lua::value<const char*>(state);
    CHECK(std::string_view(value) == "value");

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: String", "[luaconversion]", std::string,
                   std::filesystem::path, std::string_view)
{
    using T = TestType;

    lua_State* state = luaL_newstate();

    ghoul::lua::push(state, T("value"));

    const T value = ghoul::lua::value<T>(state);
    CHECK(value == "value");

    lua_close(state);
}

TEST_CASE("LuaConversion: String", "[luaconversion]") {
    lua_State* state = luaL_newstate();

    ghoul::lua::push(state, "value");

    const std::string value = ghoul::lua::value<std::string>(state);
    CHECK(value == "value");

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion - Glm Types", "[luaconversion]", glm::vec2, glm::vec3,
                   glm::vec4, glm::dvec2, glm::dvec3, glm::dvec4, glm::ivec2, glm::ivec3,
                   glm::ivec4, glm::uvec2, glm::uvec3, glm::uvec4, glm::mat2x2,
                   glm::mat2x3, glm::mat2x4, glm::mat3x2, glm::mat3x3, glm::mat3x4,
                   glm::mat4x2, glm::mat4x3, glm::mat4x4, glm::dmat2x2, glm::dmat2x3,
                   glm::dmat2x4, glm::dmat3x2, glm::dmat3x3, glm::dmat3x4, glm::dmat4x2,
                   glm::dmat4x3, glm::dmat4x4)
{
    using T = TestType;

    lua_State* state = luaL_newstate();
    luaL_openlibs(state);

    const T val(1);

    ghoul::lua::push(state, val);

    const T value = ghoul::lua::value<T>(state);
    CHECK(ghoul::to_string(value) == ghoul::to_string(val));
    CHECK(value == val);

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion Fuzz <short", "[luaconversion]", char, signed char,
                   unsigned char)
{
    using T = TestType;

    lua_State* state = luaL_newstate();

    std::mt19937 gen(1337);
    std::uniform_int_distribution<short> dis(
        std::numeric_limits<T>::lowest(),
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion Fuzz >short", "[luaconversion]", short, unsigned short,
                   int, unsigned int)
{
    using T = TestType;

    lua_State* state = luaL_newstate();

    std::mt19937 gen(1337);
    std::uniform_int_distribution<T> dis(
        std::numeric_limits<T>::lowest(),
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion Fuzz Limited Signed", "[luaconversion]", long,
                   long long)
{
    using T = TestType;
    lua_State* state = luaL_newstate();

    std::mt19937 gen(1337);
    std::uniform_int_distribution<> dis(
        // We need to limit the range of values as Lua uses 'doubles' to store, and some
        // values will not be representable
        std::numeric_limits<int>::lowest(),
        std::numeric_limits<int>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion Fuzz Limited Unsigned", "[luaconversion]",
                   unsigned long, unsigned long long)
{
    using T = TestType;
    lua_State* state = luaL_newstate();

    std::mt19937 gen(1337);
    std::uniform_int_distribution<unsigned int> dis(
        // We need to limit the range of values as Lua uses 'doubles' to store, and some
        // values will not be representable
        std::numeric_limits<unsigned int>::lowest(),
        std::numeric_limits<unsigned int>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion Float Fuzz", "[luaconversion]", float, double,
                   long double)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<T> dis(
        0.f,
        std::numeric_limits<T>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(value == Catch::Approx(val));

        if (typeid(T) == typeid(long double)) {
            if (Catch::Approx(value) != std::numeric_limits<long double>::infinity()) {
                CHECK(value == Catch::Approx(val));
            }
        }
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Vec2 Float Fuzz", "[luaconversion]", glm::vec2,
                   glm::dvec2)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    // @TODO: update range to avoid infinity
    std::mt19937 gen(1337);
    std::uniform_real_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Vec2 Fuzz", "[luaconversion]", glm::ivec2, glm::uvec2)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Vec3 Float Fuzz", "[luaconversion]", glm::vec3,
                   glm::dvec3)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Vec3 Fuzz", "[luaconversion]", glm::ivec3, glm::uvec3)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Vec4 Float Fuzz", "[luaconversion]", glm::vec4,
                   glm::dvec4)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Vec4 Fuzz", "[luaconversion]", glm::ivec4, glm::uvec4)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_int_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Mat2x2 Fuzz", "[luaconversion]", glm::mat2x2,
                   glm::dmat2x2)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Mat3x3 Fuzz", "[luaconversion]", glm::mat3x3,
                   glm::dmat3x3)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen),
            dis(gen), dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Mat4x4 Fuzz", "[luaconversion]", glm::mat4x4,
                   glm::dmat4x4)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen),
            dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen),
            dis(gen), dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Mat2x3 & Mat3x2 Fuzz", "[luaconversion]", glm::mat2x3,
                   glm::dmat2x3, glm::mat3x2, glm::dmat3x2)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Mat3x4 & Mat4x3 Fuzz", "[luaconversion]", glm::mat3x4,
                   glm::dmat3x4, glm::mat4x3, glm::dmat4x3)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen),
            dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEMPLATE_TEST_CASE("LuaConversion: Mat2x4 & Mat4x2 Fuzz", "[luaconversion]", glm::mat2x4,
                   glm::dmat2x4, glm::mat4x2, glm::dmat4x2)
{
    lua_State* state = luaL_newstate();

    using T = TestType;

    std::mt19937 gen(1337);
    std::uniform_real_distribution<typename T::value_type> dis(
        std::numeric_limits<typename T::value_type>::min(),
        std::numeric_limits<typename T::value_type>::max()
    );

    for (int i = 0; i < NumberFuzzTests; i++) {
        const T val = T(dis(gen), dis(gen), dis(gen), dis(gen), dis(gen), dis(gen),
            dis(gen), dis(gen));

        ghoul::lua::push(state, val);

        const T value = ghoul::lua::value<T>(state);
        CHECK(ghoul::to_string(value) == ghoul::to_string(val));
        CHECK(value == val);
    }

    lua_close(state);
}

TEST_CASE("LuaConversion: Variant", "[luaconversion]") {
    lua_State* state = luaL_newstate();

    using T1 = std::variant<double, std::string, glm::ivec2>;
    {
        ghoul::lua::push(state, "abc");
        T1 v = ghoul::lua::value<T1>(state);
        REQUIRE(std::holds_alternative<std::string>(v));
        CHECK(std::get<std::string>(v) == "abc");
    }
    {
        ghoul::lua::push(state, 2.2);
        T1 v = ghoul::lua::value<T1>(state);
        REQUIRE(std::holds_alternative<double>(v));
        CHECK(std::get<double>(v) == 2.2);
    }
    {
        ghoul::lua::push(state, glm::ivec2(3, 4));
        T1 v = ghoul::lua::value<T1>(state);
        REQUIRE(std::holds_alternative<glm::ivec2>(v));
        CHECK(std::get<glm::ivec2>(v) == glm::ivec2(3, 4));
    }

    using T2 = std::variant<ghoul::Dictionary, int, bool>;
    {
        ghoul::Dictionary d;
        d.setValue("a", 1.1);
        ghoul::lua::push(state, d);
        T2 v = ghoul::lua::value<T2>(state);
        REQUIRE(std::holds_alternative<ghoul::Dictionary>(v));
        REQUIRE(std::get<ghoul::Dictionary>(v).hasValue<double>("a"));
        CHECK(std::get<ghoul::Dictionary>(v).value<double>("a") == 1.1);
    }
    {
        ghoul::lua::push(state, 2);
        T2 v = ghoul::lua::value<T2>(state);
        REQUIRE(std::holds_alternative<int>(v));
        CHECK(std::get<int>(v) == 2);
    }
    {
        ghoul::lua::push(state, true);
        T2 v = ghoul::lua::value<T2>(state);
        REQUIRE(std::holds_alternative<bool>(v));
        CHECK(std::get<bool>(v) == true);
    }

    using T3 = std::variant<std::string, double, glm::ivec2>;
    {
        ghoul::lua::push(state, "abc");
        T3 v = ghoul::lua::value<T3>(state);
        REQUIRE(std::holds_alternative<std::string>(v));
        CHECK(std::get<std::string>(v) == "abc");
    }
    {
        ghoul::lua::push(state, 2.2);
        T3 v = ghoul::lua::value<T3>(state);
        REQUIRE(std::holds_alternative<double>(v));
        CHECK(std::get<double>(v) == 2.2);
    }
    {
        ghoul::lua::push(state, glm::ivec2(3, 4));
        T3 v = ghoul::lua::value<T3>(state);
        REQUIRE(std::holds_alternative<glm::ivec2>(v));
        CHECK(std::get<glm::ivec2>(v) == glm::ivec2(3, 4));
    }

    lua_close(state);
}

TEST_CASE("LuaConversion: StringMap", "[luaconversion]") {
    lua_State* state = luaL_newstate();

    {
        using T = std::map<std::string, double>;

        ghoul::Dictionary d;
        d.setValue("a", 1.1);
        d.setValue("b", 2.2);
        d.setValue("c", 3.3);

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == 1.1);
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == 2.2);
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == 3.3);
    }
    {
        using T = std::map<std::string, float>;

        ghoul::Dictionary d;
        d.setValue("a", 1.1);
        d.setValue("b", 2.2);
        d.setValue("c", 3.3);

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == 1.1f);
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == 2.2f);
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == 3.3f);
    }
    {
        using T = std::map<std::string, std::filesystem::path>;
        using namespace std::string_literals;
        using namespace std::string_view_literals;

        ghoul::Dictionary d;
        d.setValue("a", "abc"s);
        d.setValue("b", "def"s);
        d.setValue("c", "ghi"s);

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == "abc"sv);
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == "def"sv);
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == "ghi"sv);
    }
    {
        using T = std::map<std::string, std::string>;
        using namespace std::string_literals;
        using namespace std::string_view_literals;

        ghoul::Dictionary d;
        d.setValue("a", "abc"s);
        d.setValue("b", "def"s);
        d.setValue("c", "ghi"s);

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == "abc"sv);
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == "def"sv);
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == "ghi"sv);
    }
    {
        using T = std::map<std::string, glm::vec2>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dvec2(1.1, 2.2));
        d.setValue("b", glm::dvec2(3.3, 4.4));
        d.setValue("c", glm::dvec2(5.5, 6.6));

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::vec2(1.1f, 2.2f));
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == glm::vec2(3.3f, 4.4f));
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == glm::vec2(5.5f, 6.6f));
    }
    {
        using T = std::map<std::string, glm::vec3>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dvec3(1.1, 2.2, 3.3));
        d.setValue("b", glm::dvec3(4.4, 5.5, 6.6));
        d.setValue("c", glm::dvec3(7.7, 8.8, 9.9));

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::vec3(1.1f, 2.2f, 3.3f));
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == glm::vec3(4.4f, 5.5f, 6.6f));
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == glm::vec3(7.7f, 8.8f, 9.9f));
    }
    {
        using T = std::map<std::string, glm::vec4>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dvec4(1.1, 2.2, 3.3, 4.4));
        d.setValue("b", glm::dvec4(5.5, 6.6, 7.7, 8.8));
        d.setValue("c", glm::dvec4(9.9, 10.10, 11.11, 12.12));

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
    }
    {
        using T = std::map<std::string, glm::ivec2>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dvec2(1.0, 2.0));
        d.setValue("b", glm::dvec2(3.0, 4.0));
        d.setValue("c", glm::dvec2(5.0, 6.0));

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::ivec2(1, 2));
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == glm::ivec2(3, 4));
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == glm::ivec2(5, 6));
    }
    {
        using T = std::map<std::string, glm::ivec3>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dvec3(1.0, 2.0, 3.0));
        d.setValue("b", glm::dvec3(4.0, 5.0, 6.0));
        d.setValue("c", glm::dvec3(7.0, 8.0, 9.0));

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::ivec3(1, 2, 3));
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == glm::ivec3(4, 5, 6));
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == glm::ivec3(7, 8, 9));
    }
    {
        using T = std::map<std::string, glm::ivec4>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dvec4(1.0, 2.0, 3.0, 4.0));
        d.setValue("b", glm::dvec4(5.0, 6.0, 7.0, 8.0));
        d.setValue("c", glm::dvec4(9.0, 10.0, 11.0, 12.0));

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::ivec4(1, 2, 3, 4));
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == glm::ivec4(5, 6, 7, 8));
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == glm::ivec4(9, 10, 11, 12));
    }
    {
        using T = std::map<std::string, glm::mat2x2>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
        d.setValue("b", glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
        d.setValue("c", glm::dmat2x2(9.9, 10.10, 11.11, 12.12));

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
    }
    {
        using T = std::map<std::string, glm::mat2x3>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
        d.setValue("b", glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
        d.setValue("c", glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18));

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
    }
    {
        using T = std::map<std::string, glm::mat2x4>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dmat2x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
        d.setValue(
            "b",
            glm::dmat2x4(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
        );
        d.setValue(
            "c",
            glm::dmat2x4(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
        );

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::mat2x4(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f));
        REQUIRE(v.find("b") != v.end());
        CHECK(
            v["b"] ==
            glm::mat2x4(9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f)
        );
        REQUIRE(v.find("c") != v.end());
        CHECK(
            v["c"] ==
            glm::mat2x4(17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f)
        );
    }
    {
        using T = std::map<std::string, glm::mat3x2>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dmat3x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
        d.setValue("b", glm::dmat3x2(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
        d.setValue("c", glm::dmat3x2(13.13, 14.14, 15.15, 16.16, 17.17, 18.18));

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
    }
    {
        using T = std::map<std::string, glm::mat3x3>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dmat3x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9));
        d.setValue(
            "b",
            glm::dmat3x3(10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
        );
        d.setValue(
            "c",
            glm::dmat3x3(19.19, 20.20, 21.21, 22.22, 23.23, 24.24, 25.25, 26.26, 27.27)
        );

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(
            v["a"] == glm::mat3x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f)
        );
        REQUIRE(v.find("b") != v.end());
        CHECK(
            v["b"] ==
            glm::mat3x3(
                10.10f, 11.11f, 12.12f,
                13.13f, 14.14f, 15.15f,
                16.16f, 17.17f, 18.18f
            )
        );
        REQUIRE(v.find("c") != v.end());
        CHECK(
            v["c"] ==
            glm::mat3x3(
                19.19f, 20.20f, 21.21f,
                22.22f, 23.23f, 24.24f,
                25.25f, 26.26f, 27.27f
            )
        );
    }
    {
        using T = std::map<std::string, glm::mat3x4>;

        ghoul::Dictionary d;
        d.setValue(
            "a",
            glm::dmat3x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12)
        );
        d.setValue(
            "b",
            glm::dmat3x4(
                13.13, 14.14, 15.15,
                16.16, 17.17, 18.18,
                19.19, 20.20, 21.21,
                22.22, 23.23, 24.24
            )
        );
        d.setValue(
            "c",
            glm::dmat3x4(
                25.25, 26.26, 27.27,
                28.28, 29.29, 30.30,
                31.31, 32.32, 33.33,
                34.34, 35.35, 36.36
            )
        );

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(
            v["a"] ==
            glm::mat3x4(
                1.1f, 2.2f, 3.3f,
                4.4f, 5.5f, 6.6f,
                7.7f, 8.8f, 9.9f,
                10.10f, 11.11f, 12.12f
            )
        );
        REQUIRE(v.find("b") != v.end());
        CHECK(
            v["b"] ==
            glm::mat3x4(
                13.13f, 14.14f, 15.15f,
                16.16f, 17.17f, 18.18f,
                19.19f, 20.20f, 21.21f,
                22.22f, 23.23f, 24.24f
            )
        );
        REQUIRE(v.find("c") != v.end());
        CHECK(
            v["c"] ==
            glm::mat3x4(
                25.25f, 26.26f, 27.27f,
                28.28f, 29.29f, 30.30f,
                31.31f, 32.32f, 33.33f,
                34.34f, 35.35f, 36.36f
            )
        );
    }
    {
        using T = std::map<std::string, glm::mat4x2>;

        ghoul::Dictionary d;
        d.setValue("a", glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
        d.setValue(
            "b",
            glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
        );
        d.setValue(
            "c",
            glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
        );

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == glm::mat4x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f));
        REQUIRE(v.find("b") != v.end());
        CHECK(
            v["b"] ==
            glm::mat4x2(9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f)
        );
        REQUIRE(v.find("c") != v.end());
        CHECK(
            v["c"] ==
            glm::mat4x2(17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f)
        );
    }
    {
        using T = std::map<std::string, glm::mat4x3>;

        ghoul::Dictionary d;
        d.setValue(
            "a",
            glm::dmat4x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12)
        );
        d.setValue(
            "b",
            glm::dmat4x3(
                13.13, 14.14, 15.15, 16.16,
                17.17, 18.18, 19.19, 20.20,
                21.21, 22.22, 23.23, 24.24
            )
        );
        d.setValue(
            "c",
            glm::dmat4x3(
                25.25, 26.26, 27.27, 28.28,
                29.29, 30.30, 31.31, 32.32,
                33.33, 34.34, 35.35, 36.36
            )
        );

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(
            v["a"] ==
            glm::mat4x3(
                1.1f, 2.2f, 3.3f, 4.4f,
                5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f
            )
        );
        REQUIRE(v.find("b") != v.end());
        CHECK(
            v["b"] ==
            glm::mat4x3(
                13.13f, 14.14f, 15.15f, 16.16f,
                17.17f, 18.18f, 19.19f, 20.20f,
                21.21f, 22.22f, 23.23f, 24.24f
            )
        );
        REQUIRE(v.find("c") != v.end());
        CHECK(
            v["c"] ==
            glm::mat4x3(
                25.25f, 26.26f, 27.27f, 28.28f,
                29.29f, 30.30f, 31.31f, 32.32f,
                33.33f, 34.34f, 35.35f, 36.36f
            )
        );
    }
    {
        using T = std::map<std::string, glm::mat4x4>;

        ghoul::Dictionary d;
        d.setValue(
            "a",
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4,
                5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12,
                13.13, 14.14, 15.15, 16.16
            )
        );
        d.setValue(
            "b",
            glm::dmat4x4(
                17.17, 18.18, 19.19, 20.20,
                21.21, 22.22, 23.23, 24.24,
                25.25, 26.26, 27.27, 28.28,
                29.29, 30.30, 31.31, 32.32
            )
        );
        d.setValue(
            "c",
            glm::dmat4x4(
                33.33, 34.34, 35.35, 36.36,
                37.37, 38.38, 39.39, 40.40,
                41.41, 42.42, 43.43, 44.44,
                45.45, 46.46, 47.47, 48.48
            )
        );

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(
            v["a"] ==
            glm::mat4x4(
                1.1f, 2.2f, 3.3f, 4.4f,
                5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f,
                13.13f, 14.14f, 15.15f, 16.16f
            )
        );
        REQUIRE(v.find("b") != v.end());
        CHECK(
            v["b"] ==
            glm::mat4x4(
                17.17f, 18.18f, 19.19f, 20.20f,
                21.21f, 22.22f, 23.23f, 24.24f,
                25.25f, 26.26f, 27.27f, 28.28f,
                29.29f, 30.30f, 31.31f, 32.32f
            )
        );
        REQUIRE(v.find("c") != v.end());
        CHECK(
            v["c"] ==
            glm::mat4x4(
                33.33f, 34.34f, 35.35f, 36.36f,
                37.37f, 38.38f, 39.39f, 40.40f,
                41.41f, 42.42f, 43.43f, 44.44f,
                45.45f, 46.46f, 47.47f, 48.48f
            )
        );
    }
    {
        using T = std::map<std::string, int>;

        ghoul::Dictionary d;
        d.setValue("a", 1.0);
        d.setValue("b", 2.0);
        d.setValue("c", 3.0);

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == 1);
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == 2);
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == 3);
    }
    {
        using T = std::map<std::string, ghoul::Dictionary>;
        using namespace std::string_literals;
        using namespace std::string_view_literals;

        ghoul::Dictionary d;
        {
            ghoul::Dictionary e;
            e.setValue("foo", "bar"s);
            d.setValue("a", e);
        }
        {
            ghoul::Dictionary e;
            e.setValue("bar", "foo"s);
            d.setValue("b", e);
        }

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 2);
        REQUIRE(v.find("a") != v.end());
        {
            const ghoul::Dictionary e = v["a"];
            REQUIRE(e.hasKey("foo"));
            REQUIRE(e.hasValue<std::string>("foo"));
            CHECK(e.value<std::string>("foo") == "bar"sv);
        }
        {
            const ghoul::Dictionary e = v["b"];
            REQUIRE(e.hasKey("bar"));
            REQUIRE(e.hasValue<std::string>("bar"));
            CHECK(e.value<std::string>("bar") == "foo"sv);
        }
    }
    {
        using T = std::map<std::string, std::vector<int>>;

        ghoul::Dictionary d;
        d.setValue("a", std::vector<int>{ 1, 2, 3 });
        d.setValue("b", std::vector<int>{ 4, 5, 6 });
        d.setValue("c", std::vector<int>{ 7, 8, 9 });

        ghoul::lua::push(state, d);
        T v = ghoul::lua::value<T>(state);

        REQUIRE(v.size() == 3);
        REQUIRE(v.find("a") != v.end());
        CHECK(v["a"] == std::vector<int>{ 1, 2, 3 });
        REQUIRE(v.find("b") != v.end());
        CHECK(v["b"] == std::vector<int>{ 4, 5, 6 });
        REQUIRE(v.find("c") != v.end());
        CHECK(v["c"] == std::vector<int>{ 7, 8, 9 });
    }
    lua_close(state);
}

TEST_CASE("LuaConversion: Vector", "[luaconversion]") {
    lua_State* state = luaL_newstate();

    {
        std::vector<double> d = { 1.1, 2.2, 3.3 };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<double>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<float> d = { 1.1f, 2.2f, 3.3f };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<float>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<int> d = { 1, 2, 3 };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<int>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        using namespace std::string_literals;

        std::vector<std::string> d = { "abc"s, "def"s, "ghi"s };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<std::string>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        using namespace std::string_literals;

        std::vector<std::filesystem::path> d = { "abc"s, "def"s, "ghi"s };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<std::filesystem::path>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::vec2> d = {
            glm::vec2(1.1f, 2.2f),
            glm::vec2(3.3f, 4.4f),
            glm::vec2(5.5f, 6.6f)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::vec2>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::vec3> d = {
            glm::vec3(1.1f, 2.2f, 3.3f),
            glm::vec3(4.4f, 5.5f, 6.6f),
            glm::vec3(7.7f, 8.8f, 9.9f)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::vec3>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::vec4> d = {
            glm::vec4(1.1f, 2.2f, 3.3f, 4.4f),
            glm::vec4(5.5f, 6.6f, 7.7f, 8.8f),
            glm::vec4(9.9f, 10.10f, 11.11f, 12.12f)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::vec4>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::ivec2> d = {
            glm::ivec2(1, 2),
            glm::ivec2(3, 4),
            glm::ivec2(5, 6)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::ivec2>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::ivec3> d = {
            glm::ivec3(1, 2, 3),
            glm::ivec3(4, 5, 6),
            glm::ivec3(7, 8, 9)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::ivec3>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::ivec4> d = {
            glm::ivec4(1, 2, 3, 4),
            glm::ivec4(5, 6, 7, 8),
            glm::ivec4(9, 10, 11, 12)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::ivec4>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::uvec2> d = {
            glm::uvec2(1, 2),
            glm::uvec2(3, 4),
            glm::uvec2(5, 6)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::uvec2>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::uvec3> d = {
            glm::uvec3(1, 2, 3),
            glm::uvec3(4, 5, 6),
            glm::uvec3(7, 8, 9)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::uvec3>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::uvec4> d = {
            glm::uvec4(1, 2, 3, 4),
            glm::uvec4(5, 6, 7, 8),
            glm::uvec4(9, 10, 11, 12)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::uvec4>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::mat2x2> d = {
            glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f),
            glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f),
            glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::mat2x2>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::mat2x3> d = {
            glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f),
            glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f),
            glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::mat2x3>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::mat2x4> d = {
            glm::mat2x4(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f),
            glm::mat2x4(9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f),
            glm::mat2x4(17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::mat2x4>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::mat3x2> d = {
            glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f),
            glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f),
            glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::mat3x2>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::mat3x3> d = {
            glm::mat3x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f),
            glm::mat3x3(
                10.10f, 11.11f, 12.12f,
                13.13f, 14.14f, 15.15f,
                16.16f, 17.17f, 18.18f
            ),
            glm::mat3x3(
                19.19f, 20.20f, 21.21f,
                22.22f, 23.23f, 24.24f,
                25.25f, 26.26f, 27.27f
            )
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::mat3x3>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::mat3x4> d = {
            glm::mat3x4(
                1.1f, 2.2f, 3.3f,
                4.4f, 5.5f, 6.6f,
                7.7f, 8.8f, 9.9f,
                10.10f, 11.11f, 12.12f
            ),
            glm::mat3x4(
                13.13f, 14.14f, 15.15f,
                16.16f, 17.17f, 18.18f,
                19.19f, 20.20f, 21.21f,
                22.22f, 23.23f, 24.24f
            ),
            glm::mat3x4(
                25.25f, 26.26f, 27.27f,
                28.28f, 29.29f, 30.30f,
                31.31f, 32.32f, 33.33f,
                34.34f, 35.35f, 36.36f
            )
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::mat3x4>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::mat4x2> d = {
            glm::mat4x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f),
            glm::mat4x2(9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f),
            glm::mat4x2(17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::mat4x2>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::mat4x3> d = {
            glm::mat4x3(
                1.1f, 2.2f, 3.3f, 4.4f,
                5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f
            ),
            glm::mat4x3(
                13.13f, 14.14f, 15.15f, 16.16f,
                17.17f, 18.18f, 19.19f, 20.20f,
                21.21f, 22.22f, 23.23f, 24.24f
            ),
            glm::mat4x3(
                25.25f, 26.26f, 27.27f, 28.28f,
                29.29f, 30.30f, 31.31f, 32.32f,
                33.33f, 34.34f, 35.35f, 36.36f
            )
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::mat4x3>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::mat4x4> d = {
            glm::mat4x4(
                1.1f, 2.2f, 3.3f, 4.4f,
                5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f,
                13.13f, 14.14f, 15.15f, 16.16f
            ),
            glm::mat4x4(
                17.17f, 18.18f, 19.19f, 20.20f,
                21.21f, 22.22f, 23.23f, 24.24f,
                25.25f, 26.26f, 27.27f, 28.28f,
                29.29f, 30.30f, 31.31f, 32.32f
            ),
            glm::mat4x4(
                33.33f, 34.34f, 35.35f, 36.36f,
                37.37f, 38.38f, 39.39f, 40.40f,
                41.41f, 42.42f, 43.43f, 44.44f,
                45.45f, 46.46f, 47.47f, 48.48f
            )
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::mat4x4>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::dmat2x2> d = {
            glm::dmat2x2(1.1, 2.2, 3.3, 4.4),
            glm::dmat2x2(5.5, 6.6, 7.7, 8.8),
            glm::dmat2x2(9.9, 10.10, 11.11, 12.12)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::dmat2x2>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::dmat2x3> d = {
            glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6),
            glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12),
            glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::dmat2x3>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::dmat2x4> d = {
            glm::dmat2x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8),
            glm::dmat2x4(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16),
            glm::dmat2x4(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::dmat2x4>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::dmat3x2> d = {
            glm::dmat3x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6),
            glm::dmat3x2(7.7, 8.8, 9.9, 10.10, 11.11, 12.12),
            glm::dmat3x2(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::dmat3x2>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::dmat3x3> d = {
            glm::dmat3x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9),
            glm::dmat3x3(
                10.10, 11.11, 12.12,
                13.13, 14.14, 15.15,
                16.16, 17.17, 18.18
            ),
            glm::dmat3x3(
                19.19, 20.20, 21.21,
                22.22, 23.23, 24.24,
                25.25, 26.26, 27.27
            )
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::dmat3x3>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::dmat3x4> d = {
            glm::dmat3x4(
                1.1, 2.2, 3.3,
                4.4, 5.5, 6.6,
                7.7, 8.8, 9.9,
                10.10, 11.11, 12.12
            ),
            glm::dmat3x4(
                13.13, 14.14, 15.15,
                16.16, 17.17, 18.18,
                19.19, 20.20, 21.21,
                22.22, 23.23, 24.24
            ),
            glm::dmat3x4(
                25.25, 26.26, 27.27,
                28.28, 29.29, 30.30,
                31.31, 32.32, 33.33,
                34.34, 35.35, 36.36
            )
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::dmat3x4>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::dmat4x2> d = {
            glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8),
            glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16),
            glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::dmat4x2>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::dmat4x3> d = {
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4,
                5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12
            ),
            glm::dmat4x3(
                13.13, 14.14, 15.15, 16.16,
                17.17, 18.18, 19.19, 20.20,
                21.21, 22.22, 23.23, 24.24
            ),
            glm::dmat4x3(
                25.25, 26.26, 27.27, 28.28,
                29.29, 30.30, 31.31, 32.32,
                33.33, 34.34, 35.35, 36.36
            )
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::dmat4x3>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }
    {
        std::vector<glm::dmat4x4> d = {
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4,
                5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12,
                13.13, 14.14, 15.15, 16.16
            ),
            glm::dmat4x4(
                17.17, 18.18, 19.19, 20.20,
                21.21, 22.22, 23.23, 24.24,
                25.25, 26.26, 27.27, 28.28,
                29.29, 30.30, 31.31, 32.32
            ),
            glm::dmat4x4(
                33.33, 34.34, 35.35, 36.36,
                37.37, 38.38, 39.39, 40.40,
                41.41, 42.42, 43.43, 44.44,
                45.45, 46.46, 47.47, 48.48
            )
        };
        ghoul::lua::push(state, d);

        auto v = ghoul::lua::value<std::vector<glm::dmat4x4>>(state);
        REQUIRE(v.size() == 3);
        CHECK(v == d);
    }

    lua_close(state);
}
