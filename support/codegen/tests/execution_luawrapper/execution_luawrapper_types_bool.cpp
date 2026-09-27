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
    [[codegen::luawrap]] void funcBool(bool arg) {
        CHECK(arg == true);
    }

    [[codegen::luawrap]] void funcBoolDefaulted(bool arg = true) {
        CHECK(arg == true);
    }

    [[codegen::luawrap]] void funcBoolDefaultedCheck(bool isDefaulted, bool arg = true) {
        if (isDefaulted) {
            CHECK(arg == true);
        }
        else {
            CHECK(arg == false);
        }
    }

    [[codegen::luawrap]] void funcBoolMap(std::map<std::string, bool> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == true);
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == false);
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == false);
    }

    [[codegen::luawrap]] void funcBoolOptional(std::optional<bool> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == true);
    }

    [[codegen::luawrap]] void funcBoolOptionalNullopt(std::optional<bool> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcBoolVector(std::vector<bool> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == true);
        CHECK(arg[1] == false);
        CHECK(arg[2] == false);
    }

    [[codegen::luawrap]] void funcBoolArray1(std::array<bool, 1> arg) {
        CHECK(arg[0] == true);
    }

    [[codegen::luawrap]] void funcBoolArray2(std::array<bool, 5> arg) {
        CHECK(arg[0] == true);
        CHECK(arg[1] == false);
        CHECK(arg[2] == true);
        CHECK(arg[3] == false);
        CHECK(arg[4] == true);
    }

    [[codegen::luawrap]] void funcBoolArray3(std::array<bool, 10> arg) {
        CHECK(arg[0] == true);
        CHECK(arg[1] == false);
        CHECK(arg[2] == true);
        CHECK(arg[3] == false);
        CHECK(arg[4] == true);
        CHECK(arg[5] == true);
        CHECK(arg[6] == false);
        CHECK(arg[7] == true);
        CHECK(arg[8] == false);
        CHECK(arg[9] == true);
    }

    [[codegen::luawrap]] bool returnBool() {
        return true;
    }

    [[codegen::luawrap]] std::map<std::string, bool> returnBoolMap() {
        return {
            { "key1", true },
            { "key2", false },
            { "key3", true }
        };
    }

    [[codegen::luawrap]] std::optional<bool> returnBoolOptional() {
        return true;
    }

    [[codegen::luawrap]] std::optional<bool> returnBoolOptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<bool> returnBoolVector() {
        return { false, true, true };
    }
} // namespace
#include "execution_luawrapper_types_bool_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  bool", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncBool;
    CHECK(func.name == "funcBool");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean");
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

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  boolDefaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncBoolDefaulted;
    CHECK(func.name == "funcBoolDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean?");
    CHECK(func.arguments[0].defaultValue == "true");
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

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  boolDefaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncBoolDefaulted;
    CHECK(func.name == "funcBoolDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean?");
    CHECK(func.arguments[0].defaultValue == "true");
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
    "Execution/LuaWrapper/Arguments:  boolDefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncBoolDefaultedCheck;
    CHECK(func.name == "funcBoolDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Boolean?");
    CHECK(func.arguments[1].defaultValue == "true");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, false);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  boolDefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncBoolDefaultedCheck;
    CHECK(func.name == "funcBoolDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Boolean?");
    CHECK(func.arguments[1].defaultValue == "true");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  boolMap", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncBoolMap;
    CHECK(func.name == "funcBoolMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> Boolean");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", true);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", false);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", false);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  boolOptional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncBoolOptional;
    CHECK(func.name == "funcBoolOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean?");
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

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  funcBoolOptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncBoolOptionalNullopt;
    CHECK(func.name == "funcBoolOptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  boolVector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncBoolVector;
    CHECK(func.name == "funcBoolVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, true);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, false);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, false);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  boolArray1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncBoolArray1;
    CHECK(func.name == "funcBoolArray1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, true);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<bool, 1> { true });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  boolArray2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncBoolArray2;
    CHECK(func.name == "funcBoolArray2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, true);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, false);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, true);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, false);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, true);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<bool, 5> { true, false, true, false, true });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  boolArray3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncBoolArray3;
    CHECK(func.name == "funcBoolArray3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, true);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, false);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, true);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, false);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, true);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, true);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, false);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, true);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, false);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, true);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<bool, 10> {
            true, false, true, false, true, true, false, true, false, true
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  bool", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnBool;
    CHECK(func.name == "returnBool");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Boolean");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const bool val = ghoul::lua::value<bool>(state);
    CHECK(val == true);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  boolMap", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnBoolMap;
    CHECK(func.name == "returnBoolMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> Boolean");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, bool> val =
        ghoul::lua::value<std::map<std::string, bool>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == true);
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == false);
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == true);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  boolOptional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnBoolOptional;
    CHECK(func.name == "returnBoolOptional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Boolean?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const bool val = ghoul::lua::value<bool>(state);
    CHECK(val == true);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  boolOptional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnBoolOptionalNullopt;
    CHECK(func.name == "returnBoolOptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Boolean?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  boolVector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnBoolVector;
    CHECK(func.name == "returnBoolVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Boolean[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<bool> val = ghoul::lua::value<std::vector<bool>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == false);
    CHECK(val[1] == true);
    CHECK(val[2] == true);
    lua_close(state);
}
