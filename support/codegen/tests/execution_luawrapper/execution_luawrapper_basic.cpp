/*****************************************************************************************
 *                                                                                       *
 * OpenSpace Codegen                                                                     *
 *                                                                                       *
 * Copyright (c) 2021-2026                                                               *
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

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_exception.hpp>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/lua/lua_helper.h>
#include <openspace/misc/dictionary.h>
#include <openspace/scripting/lualibrary.h>
#include <optional>

using Function = openspace::LuaLibrary::Function;

namespace {
    bool ranTestFunc = false;
    [[codegen::luawrap]] void testFunc() {
        ranTestFunc = true;
    }

    bool ranTestFunc2 = false;
    int testFunc2Value = -1;
    [[codegen::luawrap]] void testFunc2(int arg) {
        testFunc2Value = arg;
        ranTestFunc2 = true;
    }

    bool ranTestFunc3 = false;
    int testFunc3Value1 = -1;
    std::string testFunc3Value2;
    [[codegen::luawrap]] void testFunc3(int arg1, std::string arg2) {
        testFunc3Value1 = arg1;
        testFunc3Value2 = std::move(arg2);
        ranTestFunc3 = true;
    }

    bool ranTestFunc4 = false;
    int testFunc4Value1 = -1;
    double testFunc4Value2 = -1.0;
    [[codegen::luawrap]] void testFunc4(int arg1, std::optional<double> arg2) {
        testFunc4Value1 = arg1;
        testFunc4Value2 = arg2.value_or(testFunc4Value2);
        ranTestFunc4 = true;
    }

    bool ranTestFunc5 = false;
    [[codegen::luawrap]] int testFunc5() {
        ranTestFunc5 = true;
        return 5;
    }

    bool ranTestFunc6 = false;
    [[codegen::luawrap]] std::tuple<int, double> testFunc6() {
        ranTestFunc6 = true;
        return { 5, 6.0 };
    }

    bool ranTestFunc7 = false;
    [[codegen::luawrap]] void testFunc7() {
        ranTestFunc7 = true;
        throw ghoul::lua::LuaError("Thrown Lua error message");
    }

    bool ranTestFunc8 = false;
    [[codegen::luawrap("abcFunc")]] void testFunc8() {
        ranTestFunc8 = true;
    }

    void resetTestRuns() {
        ranTestFunc = false;

        ranTestFunc2 = false;
        testFunc2Value = -1;

        ranTestFunc3 = false;
        testFunc3Value1 = -1;
        testFunc3Value2.clear();

        ranTestFunc4 = false;
        testFunc4Value1 = -1;
        testFunc4Value2 = -1.0;

        ranTestFunc5 = false;
        ranTestFunc6 = false;
        ranTestFunc7 = false;
        ranTestFunc8 = false;
    }
} // namespace
#include "execution_luawrapper_basic_codegen.cpp"


TEST_CASE("Execution/LuaWrapper:  Basic", "[Execution][LuaWrapper]") {
    resetTestRuns();

    SECTION("TestFunc") {
        Function func = codegen::lua::TestFunc;
        CHECK(func.name == "testFunc");
        CHECK(func.arguments.empty());
        CHECK(func.returnType.empty());
        CHECK(func.helpText.empty());
        REQUIRE(func.function);

        lua_State* state = luaL_newstate();
        REQUIRE(state);
        func.function(state);
        CHECK(ranTestFunc);
        CHECK(lua_gettop(state) == 0);
        lua_close(state);
    }

    SECTION("TestFunc2") {
        Function func = codegen::lua::TestFunc2;
        CHECK(func.name == "testFunc2");
        REQUIRE(func.arguments.size() == 1);
        CHECK(func.arguments[0].name == "arg");
        CHECK(func.arguments[0].type == "Integer");
        CHECK(func.returnType.empty());
        CHECK(func.helpText.empty());
        REQUIRE(func.function);

        lua_State* state = luaL_newstate();
        REQUIRE(state);
        ghoul::lua::push(state, 2);
        func.function(state);
        CHECK(ranTestFunc2);
        CHECK(testFunc2Value == 2);
        CHECK(lua_gettop(state) == 0);
        lua_close(state);
    }

    SECTION("TestFunc3") {
        Function func = codegen::lua::TestFunc3;
        CHECK(func.name == "testFunc3");
        REQUIRE(func.arguments.size() == 2);
        CHECK(func.arguments[0].name == "arg1");
        CHECK(func.arguments[0].type == "Integer");
        CHECK(func.arguments[1].name == "arg2");
        CHECK(func.arguments[1].type == "String");
        CHECK(func.returnType.empty());
        CHECK(func.helpText.empty());
        REQUIRE(func.function);

        lua_State* state = luaL_newstate();
        REQUIRE(state);
        ghoul::lua::push(state, 2, "test");
        func.function(state);
        CHECK(ranTestFunc3);
        CHECK(testFunc3Value1 == 2);
        CHECK(testFunc3Value2 == "test");
        CHECK(lua_gettop(state) == 0);
        lua_close(state);
    }

    SECTION("TestFunc4/All Arguments") {
        Function func = codegen::lua::TestFunc4;
        CHECK(func.name == "testFunc4");
        REQUIRE(func.arguments.size() == 2);
        CHECK(func.arguments[0].name == "arg1");
        CHECK(func.arguments[0].type == "Integer");
        CHECK(func.arguments[1].name == "arg2");
        CHECK(func.arguments[1].type == "Number?");
        CHECK(func.returnType.empty());
        CHECK(func.helpText.empty());
        REQUIRE(func.function);

        lua_State* state = luaL_newstate();
        REQUIRE(state);
        ghoul::lua::push(state, 2, 3.0);
        func.function(state);
        CHECK(ranTestFunc4);
        CHECK(testFunc4Value1 == 2);
        CHECK(testFunc4Value2 == 3.0);
        CHECK(lua_gettop(state) == 0);
        lua_close(state);
    }

    SECTION("TestFunc4/No Optional") {
        Function func = codegen::lua::TestFunc4;
        CHECK(func.name == "testFunc4");
        REQUIRE(func.arguments.size() == 2);
        CHECK(func.arguments[0].name == "arg1");
        CHECK(func.arguments[0].type == "Integer");
        CHECK(func.arguments[1].name == "arg2");
        CHECK(func.arguments[1].type == "Number?");
        CHECK(func.returnType.empty());
        CHECK(func.helpText.empty());
        REQUIRE(func.function);

        lua_State* state = luaL_newstate();
        REQUIRE(state);
        ghoul::lua::push(state, 2);
        func.function(state);
        CHECK(ranTestFunc4);
        CHECK(testFunc4Value1 == 2);
        CHECK(testFunc4Value2 == -1.0);
        CHECK(lua_gettop(state) == 0);
        lua_close(state);
    }

    SECTION("Basic/TestFunc5") {
        Function func = codegen::lua::TestFunc5;
        CHECK(func.name == "testFunc5");
        CHECK(func.arguments.empty());
        CHECK(func.returnType == "Integer");
        CHECK(func.helpText.empty());
        REQUIRE(func.function);

        lua_State* state = luaL_newstate();
        REQUIRE(state);
        const int nResult = func.function(state);
        CHECK(ranTestFunc5);
        CHECK(lua_gettop(state) == 1);
        REQUIRE(nResult == 1);
        const int value = ghoul::lua::value<int>(state);
        CHECK(value == 5);
        lua_close(state);
    }

    SECTION("Basic/TestFunc6") {
        Function func = codegen::lua::TestFunc6;
        CHECK(func.name == "testFunc6");
        CHECK(func.arguments.empty());
        CHECK(func.returnType == "(Integer, Number)");
        CHECK(func.helpText.empty());
        REQUIRE(func.function);

        lua_State* state = luaL_newstate();
        REQUIRE(state);
        const int nResult = func.function(state);
        CHECK(ranTestFunc6);
        CHECK(lua_gettop(state) == 1);
        REQUIRE(nResult == 1);
        const auto [res1, res2] = ghoul::lua::value<std::tuple<int, double>>(state);
        CHECK(res1 == 5);
        CHECK(res2 == 6.0);
        lua_close(state);
    }

    SECTION("Basic/TestFunc7") {
        auto panicFunc = [](lua_State* L) -> int {
            CHECK(ranTestFunc7);
            const std::string msg = lua_tostring(L, -1);
            CHECK(msg == "Thrown Lua error message");
            lua_close(L);
            throw std::runtime_error("excepted exception");
        };

        Function func = codegen::lua::TestFunc7;
        CHECK(func.name == "testFunc7");
        CHECK(func.arguments.empty());
        CHECK(func.returnType.empty());
        CHECK(func.helpText.empty());
        REQUIRE(func.function);

        lua_State* state = luaL_newstate();
        REQUIRE(state);
        lua_atpanic(state, panicFunc);

        CHECK_THROWS_MATCHES(
            func.function(state),
            std::runtime_error, Catch::Matchers::Message("excepted exception")
        );
    }

    SECTION("Basic/TestFunc8") {
        Function func = codegen::lua::TestFunc8;
        CHECK(func.name == "abcFunc");
        CHECK(func.arguments.empty());
        CHECK(func.returnType.empty());
        CHECK(func.helpText.empty());
        REQUIRE(func.function);

        lua_State* state = luaL_newstate();
        REQUIRE(state);
        func.function(state);
        CHECK(lua_gettop(state) == 0);
        lua_close(state);
    }
}
