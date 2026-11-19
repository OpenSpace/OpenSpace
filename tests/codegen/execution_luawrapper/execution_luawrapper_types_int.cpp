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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/lua/lua_helper.h>
#include <openspace/misc/dictionary.h>
#include <openspace/scripting/lualibrary.h>
#include <array>
#include <optional>

using namespace openspace;

namespace {
    [[codegen::luawrap]] void funcInt(int arg) {
        CHECK(arg == 1);
    }

    [[codegen::luawrap]] void funcIntDefaulted(int arg = 2) {
        CHECK(arg == 2);
    }

    [[codegen::luawrap]] void funcIntDefaultedCheck(bool isDefaulted, int arg = 2) {
        if (isDefaulted) {
            CHECK(arg == 2);
        }
        else {
            CHECK(arg == 3);
        }
    }

    [[codegen::luawrap]] void funcIntMap(std::map<std::string, int> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == 1);
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == 2);
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == 3);
    }

    [[codegen::luawrap]] void funcIntOptional(std::optional<int> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == 1);
    }

    [[codegen::luawrap]] void funcIntOptionalNullopt(std::optional<int> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcIntVector(std::vector<int> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == 1);
        CHECK(arg[1] == 2);
        CHECK(arg[2] == 3);
    }

    [[codegen::luawrap]] void funcIntArray1(std::array<int, 1> arg) {
        CHECK(arg[0] == 2);
    }

    [[codegen::luawrap]] void funcIntArray2(std::array<int, 5> arg) {
        CHECK(arg[0] == 2);
        CHECK(arg[1] == 4);
        CHECK(arg[2] == 6);
        CHECK(arg[3] == 8);
        CHECK(arg[4] == 10);
    }

    [[codegen::luawrap]] void funcIntArray3(std::array<int, 10> arg) {
        CHECK(arg[0] == 2);
        CHECK(arg[1] == 4);
        CHECK(arg[2] == 6);
        CHECK(arg[3] == 8);
        CHECK(arg[4] == 10);
        CHECK(arg[5] == 12);
        CHECK(arg[6] == 14);
        CHECK(arg[7] == 16);
        CHECK(arg[8] == 18);
        CHECK(arg[9] == 20);
    }

    [[codegen::luawrap]] int returnInt() {
        return 1;
    }

    [[codegen::luawrap]] std::map<std::string, int> returnIntMap() {
        return {
            { "key1", 1 },
            { "key2", 2 },
            { "key3", 3 }
        };
    }

    [[codegen::luawrap]] std::optional<int> returnIntOptional() {
        return 1;
    }

    [[codegen::luawrap]] std::optional<int> returnIntOptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<int> returnIntVector() {
        return { 1, 2, 3 };
    }
} // namespace
#include "execution_luawrapper_types_int_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  int", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncInt;
    CHECK(func.name == "funcInt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Integer");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, 1);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  intDefaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIntDefaulted;
    CHECK(func.name == "funcIntDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Integer?");
    CHECK(func.arguments[0].defaultValue == "2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, 2);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  intDefaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIntDefaulted;
    CHECK(func.name == "funcIntDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Integer?");
    CHECK(func.arguments[0].defaultValue == "2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  intDefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIntDefaultedCheck;
    CHECK(func.name == "funcIntDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Integer?");
    CHECK(func.arguments[1].defaultValue == "2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, 3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  intDefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIntDefaultedCheck;
    CHECK(func.name == "funcIntDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Integer?");
    CHECK(func.arguments[1].defaultValue == "2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, true);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  intMap", "[Execution][LuaWrapper]") {
    using namespace std::string_literals;

    LuaLibrary::Function func = codegen::lua::FuncIntMap;
    CHECK(func.name == "funcIntMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> Integer");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, "key1", 1);
    lua_rawset(state, -3);
    lua::push(state, "key2", 2);
    lua_rawset(state, -3);
    lua::push(state, "key3", 3);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  intOptional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIntOptional;
    CHECK(func.name == "funcIntOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Integer?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, 1);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  intOptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIntOptionalNullopt;
    CHECK(func.name == "funcIntOptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Integer?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  intVector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIntVector;
    CHECK(func.name == "funcIntVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Integer[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, 1);
    lua_rawset(state, -3);
    lua::push(state, 2, 2);
    lua_rawset(state, -3);
    lua::push(state, 3, 3);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  intArray1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIntArray1;
    CHECK(func.name == "funcIntArray1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Integer[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, 2);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(state, std::array<int, 1> { 2 });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  intArray2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIntArray2;
    CHECK(func.name == "funcIntArray2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Integer[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, 2);
    lua_rawset(state, -3);
    lua::push(state, 2, 4);
    lua_rawset(state, -3);
    lua::push(state, 3, 6);
    lua_rawset(state, -3);
    lua::push(state, 4, 8);
    lua_rawset(state, -3);
    lua::push(state, 5, 10);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(state, std::array<int, 5> { 2, 4, 6, 8, 10 });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  intArray3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIntArray3;
    CHECK(func.name == "funcIntArray3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Integer[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, 2);
    lua_rawset(state, -3);
    lua::push(state, 2, 4);
    lua_rawset(state, -3);
    lua::push(state, 3, 6);
    lua_rawset(state, -3);
    lua::push(state, 4, 8);
    lua_rawset(state, -3);
    lua::push(state, 5, 10);
    lua_rawset(state, -3);
    lua::push(state, 6, 12);
    lua_rawset(state, -3);
    lua::push(state, 7, 14);
    lua_rawset(state, -3);
    lua::push(state, 8, 16);
    lua_rawset(state, -3);
    lua::push(state, 9, 18);
    lua_rawset(state, -3);
    lua::push(state, 10, 20);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(state, std::array<int, 10> { 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  int", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnInt;
    CHECK(func.name == "returnInt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Integer");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const int val = lua::value<int>(state);
    CHECK(val == 1);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  intMap", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnIntMap;
    CHECK(func.name == "returnIntMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> Integer");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, int> val = lua::value<std::map<std::string, int>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == 1);
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == 2);
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == 3);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  intOptional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnIntOptional;
    CHECK(func.name == "returnIntOptional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Integer?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const int val = lua::value<int>(state);
    CHECK(val == 1);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  intOptional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnIntOptionalNullopt;
    CHECK(func.name == "returnIntOptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Integer?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  intVector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnIntVector;
    CHECK(func.name == "returnIntVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Integer[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<int> val = lua::value<std::vector<int>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == 1);
    CHECK(val[1] == 2);
    CHECK(val[2] == 3);
    lua_close(state);
}
