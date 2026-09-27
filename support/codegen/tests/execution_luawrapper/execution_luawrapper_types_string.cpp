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

using Function = openspace::LuaLibrary::Function;

namespace {
    [[codegen::luawrap]] void funcString(std::string arg) {
        CHECK(arg == "abc");
    }

    [[codegen::luawrap]] void funcStringDefaulted(std::string arg = "abc") {
        CHECK(arg == "abc");
    }

    [[codegen::luawrap]] void funcStringDefaultedCheck(bool isDefaulted, std::string arg = "abc") {
        if (isDefaulted) {
            CHECK(arg == "abc");
        }
        else {
            CHECK(arg == "def");
        }
    }

    [[codegen::luawrap]] void funcStringMap(std::map<std::string, std::string> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == "abc");
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == "def");
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == "ghi");
    }

    [[codegen::luawrap]] void funcStringOptional(std::optional<std::string> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == "abc");
    }

    [[codegen::luawrap]] void funcStringOptionalNullopt(std::optional<std::string> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcStringVector(std::vector<std::string> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == "abc");
        CHECK(arg[1] == "def");
        CHECK(arg[2] == "ghi");
    }

    [[codegen::luawrap]] void funcStringArray1(std::array<std::string, 1> arg) {
        CHECK(arg[0] == "abc");
    }

    [[codegen::luawrap]] void funcStringArray2(std::array<std::string, 5> arg) {
        CHECK(arg[0] == "abc");
        CHECK(arg[1] == "def");
        CHECK(arg[2] == "ghi");
        CHECK(arg[3] == "jkl");
        CHECK(arg[4] == "mno");
    }

    [[codegen::luawrap]] void funcStringArray3(std::array<std::string, 10> arg) {
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

    [[codegen::luawrap]] std::string returnString() {
        return "abc";
    }

    [[codegen::luawrap]] std::map<std::string, std::string> returnStringMap() {
        return {
            { "key1", "abc" },
            { "key2", "def" },
            { "key3", "ghi" }
        };
    }

    [[codegen::luawrap]] std::optional<std::string> returnStringOptional() {
        return "abc";
    }

    [[codegen::luawrap]] std::optional<std::string> returnStringOptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<std::string> returnStringVector() {
        return { "abc", "def", "ghi" };
    }
} // namespace
#include "execution_luawrapper_types_string_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  string", "[Execution][LuaWrapper]") {
    using namespace std::string_literals;

    Function func = codegen::lua::FuncString;
    CHECK(func.name == "funcString");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, "abc"s);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  stringDefaulted with value",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncStringDefaulted;
    CHECK(func.name == "funcStringDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String?");
    CHECK(func.arguments[0].defaultValue == "\"abc\"");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, "abc"s);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  stringDefaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncStringDefaulted;
    CHECK(func.name == "funcStringDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String?");
    CHECK(func.arguments[0].defaultValue == "\"abc\"");
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
    "Execution/LuaWrapper/Arguments:  stringDefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncStringDefaultedCheck;
    CHECK(func.name == "funcStringDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "String?");
    CHECK(func.arguments[1].defaultValue == "\"abc\"");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, "def"s);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  stringDefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncStringDefaultedCheck;
    CHECK(func.name == "funcStringDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "String?");
    CHECK(func.arguments[1].defaultValue == "\"abc\"");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, true);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  stringMap", "[Execution][LuaWrapper]") {
    using namespace std::string_literals;

    Function func = codegen::lua::FuncStringMap;
    CHECK(func.name == "funcStringMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> String");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", "abc"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", "def"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", "ghi"s);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  stringOptional", "[Execution][LuaWrapper]") {
    using namespace std::string_literals;

    Function func = codegen::lua::FuncStringOptional;
    CHECK(func.name == "funcStringOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, "abc"s);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  stringOptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncStringOptionalNullopt;
    CHECK(func.name == "funcStringOptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  stringVector", "[Execution][LuaWrapper]") {
    using namespace std::string_literals;

    Function func = codegen::lua::FuncStringVector;
    CHECK(func.name == "funcStringVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, "abc"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, "def"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, "ghi"s);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  stringArray1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncStringArray1;
    CHECK(func.name == "funcStringArray1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, "abc");
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  stringArray2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncStringArray2;
    CHECK(func.name == "funcStringArray2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, "abc");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, "def");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, "ghi");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, "jkl");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, "mno");
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  stringArray3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncStringArray3;
    CHECK(func.name == "funcStringArray3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, "abc");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, "def");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, "ghi");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, "jkl");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, "mno");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, "pqr");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, "stu");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, "vwx");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, "yzz");
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, "ABC");
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  string", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnString;
    CHECK(func.name == "returnString");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::string val = ghoul::lua::value<std::string>(state);
    CHECK(val == "abc");
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  stringMap", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnStringMap;
    CHECK(func.name == "returnStringMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> String");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, std::string> val =
        ghoul::lua::value<std::map<std::string, std::string>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == "abc");
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == "def");
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == "ghi");
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  stringOptional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnStringOptional;
    CHECK(func.name == "returnStringOptional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::string val = ghoul::lua::value<std::string>(state);
    CHECK(val == "abc");
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  stringOptional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnStringOptionalNullopt;
    CHECK(func.name == "returnStringOptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  stringVector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnStringVector;
    CHECK(func.name == "returnStringVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<std::string> val = ghoul::lua::value<std::vector<std::string>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == "abc");
    CHECK(val[1] == "def");
    CHECK(val[2] == "ghi");
    lua_close(state);
}
