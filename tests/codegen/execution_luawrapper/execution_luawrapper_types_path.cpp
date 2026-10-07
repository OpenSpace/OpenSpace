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
    [[codegen::luawrap]] void funcPath(std::filesystem::path arg) {
        CHECK(arg == std::filesystem::path("abc"));
    }

    [[codegen::luawrap]] void funcPathDefaulted(std::filesystem::path arg = std::filesystem::path("abc")) {
        CHECK(arg == std::filesystem::path("abc"));
    }

    [[codegen::luawrap]] void funcPathDefaultedCheck(bool isDefaulted, std::filesystem::path arg = std::filesystem::path("abc")) {
        if (isDefaulted) {
            CHECK(arg == std::filesystem::path("abc"));
        }
        else {
            CHECK(arg == std::filesystem::path("def"));
        }
    }

    [[codegen::luawrap]] void funcPathMap(std::map<std::string, std::filesystem::path> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == std::filesystem::path("abc"));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == std::filesystem::path("def"));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == std::filesystem::path("ghi"));
    }

    [[codegen::luawrap]] void funcPathOptional(std::optional<std::filesystem::path> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == "abc");
    }

    [[codegen::luawrap]] void funcPathOptionalNullopt(std::optional<std::filesystem::path> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcPathVector(std::vector<std::filesystem::path> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == std::filesystem::path("abc"));
        CHECK(arg[1] == std::filesystem::path("def"));
        CHECK(arg[2] == std::filesystem::path("ghi"));
    }

    [[codegen::luawrap]] void funcPathArray1(std::array<std::filesystem::path, 1> arg) {
        CHECK(arg[0] == "abc");
    }

    [[codegen::luawrap]] void funcPathArray2(std::array<std::filesystem::path, 5> arg) {
        CHECK(arg[0] == "abc");
        CHECK(arg[1] == "def");
        CHECK(arg[2] == "ghi");
        CHECK(arg[3] == "jkl");
        CHECK(arg[4] == "mno");
    }

    [[codegen::luawrap]] void funcPathArray3(std::array<std::filesystem::path, 10> arg) {
        CHECK(arg[0] == "abc");
        CHECK(arg[1] == "def");
        CHECK(arg[2] == "ghi");
        CHECK(arg[3] == "jkl");
        CHECK(arg[4] == "mno");
        CHECK(arg[5] == "pqr");
        CHECK(arg[6] == "stu");
        CHECK(arg[7] == "vwx");
        CHECK(arg[8] == "yzz");
        CHECK(arg[9] == "ABC");
    }

    [[codegen::luawrap]] std::filesystem::path returnPath() {
        return std::filesystem::path("abc");
    }

    [[codegen::luawrap]] std::map<std::string, std::filesystem::path> returnPathMap() {
        return {
            { "key1", std::filesystem::path("abc") },
            { "key2", std::filesystem::path("def") },
            { "key3", std::filesystem::path("ghi") }
        };
    }

    [[codegen::luawrap]] std::optional<std::filesystem::path> returnPathOptional() {
        return std::filesystem::path("abc");
    }

    [[codegen::luawrap]] std::optional<std::filesystem::path> returnPathOptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<std::filesystem::path> returnPathVector() {
        return {
            std::filesystem::path("abc"),
            std::filesystem::path("def"),
            std::filesystem::path("ghi")
        };
    }
} // namespace
#include "execution_luawrapper_types_path_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  path", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncPath;
    CHECK(func.name == "funcPath");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Path");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::filesystem::path("abc"));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  pathDefaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncPathDefaulted;
    CHECK(func.name == "funcPathDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Path?");
    CHECK(func.arguments[0].defaultValue == "std::filesystem::path(\"abc\")");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::filesystem::path("abc"));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  pathDefaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncPathDefaulted;
    CHECK(func.name == "funcPathDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Path?");
    CHECK(func.arguments[0].defaultValue == "std::filesystem::path(\"abc\")");
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
    "Execution/LuaWrapper/Arguments:  pathDefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncPathDefaultedCheck;
    CHECK(func.name == "funcPathDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Path?");
    CHECK(func.arguments[1].defaultValue == "std::filesystem::path(\"abc\")");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, std::filesystem::path("def"));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  pathDefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncPathDefaultedCheck;
    CHECK(func.name == "funcPathDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Path?");
    CHECK(func.arguments[1].defaultValue == "std::filesystem::path(\"abc\")");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  pathMap", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncPathMap;
    CHECK(func.name == "funcPathMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> Path");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, "key1", std::filesystem::path("abc"));
    lua_rawset(state, -3);
    lua::push(state, "key2", std::filesystem::path("def"));
    lua_rawset(state, -3);
    lua::push(state, "key3", std::filesystem::path("ghi"));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  pathOptional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncPathOptional;
    CHECK(func.name == "funcPathOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Path?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::filesystem::path("abc"));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  pathOptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncPathOptionalNullopt;
    CHECK(func.name == "funcPathOptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Path?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  pathVector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncPathVector;
    CHECK(func.name == "funcPathVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Path[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, std::filesystem::path("abc"));
    lua_rawset(state, -3);
    lua::push(state, 2, std::filesystem::path("def"));
    lua_rawset(state, -3);
    lua::push(state, 3, std::filesystem::path("ghi"));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  pathArray1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncPathArray1;
    CHECK(func.name == "funcPathArray1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Path[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, "abc");
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  pathArray2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncPathArray2;
    CHECK(func.name == "funcPathArray2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Path[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, "abc");
    lua_rawset(state, -3);
    lua::push(state, 2, "def");
    lua_rawset(state, -3);
    lua::push(state, 3, "ghi");
    lua_rawset(state, -3);
    lua::push(state, 4, "jkl");
    lua_rawset(state, -3);
    lua::push(state, 5, "mno");
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  pathArray3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncPathArray3;
    CHECK(func.name == "funcPathArray3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Path[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, "abc");
    lua_rawset(state, -3);
    lua::push(state, 2, "def");
    lua_rawset(state, -3);
    lua::push(state, 3, "ghi");
    lua_rawset(state, -3);
    lua::push(state, 4, "jkl");
    lua_rawset(state, -3);
    lua::push(state, 5, "mno");
    lua_rawset(state, -3);
    lua::push(state, 6, "pqr");
    lua_rawset(state, -3);
    lua::push(state, 7, "stu");
    lua_rawset(state, -3);
    lua::push(state, 8, "vwx");
    lua_rawset(state, -3);
    lua::push(state, 9, "yzz");
    lua_rawset(state, -3);
    lua::push(state, 10, "ABC");
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  path", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnPath;
    CHECK(func.name == "returnPath");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Path");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::filesystem::path val = lua::value<std::filesystem::path>(state);
    CHECK(val == std::filesystem::path("abc"));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  pathMap", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnPathMap;
    CHECK(func.name == "returnPathMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> Path");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, std::filesystem::path> val =
        lua::value<std::map<std::string, std::filesystem::path>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == std::filesystem::path("abc"));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == std::filesystem::path("def"));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == std::filesystem::path("ghi"));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  pathOptional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnPathOptional;
    CHECK(func.name == "returnPathOptional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Path?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::filesystem::path val = lua::value<std::filesystem::path>(state);
    CHECK(val == std::filesystem::path("abc"));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  pathOptional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnPathOptionalNullopt;
    CHECK(func.name == "returnPathOptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Path?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  pathVector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnPathVector;
    CHECK(func.name == "returnPathVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Path[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<std::filesystem::path> val =
        lua::value<std::vector<std::filesystem::path>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == std::filesystem::path("abc"));
    CHECK(val[1] == std::filesystem::path("def"));
    CHECK(val[2] == std::filesystem::path("ghi"));
    lua_close(state);
}
