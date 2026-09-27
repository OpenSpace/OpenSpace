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
    [[codegen::luawrap]] void funcDouble(double arg) {
        CHECK(arg == 1.1);
    }

    [[codegen::luawrap]] void funcDoubleDefaulted(double arg = 2.2) {
        CHECK(arg == 2.2);
    }

    [[codegen::luawrap]] void funcDoubleDefaultedCheck(bool isDefaulted, double arg = 2.2) {
        if (isDefaulted) {
            CHECK(arg == 2.2);
        }
        else {
            CHECK(arg == 3.3);
        }
    }

    [[codegen::luawrap]] void funcDoubleMap(std::map<std::string, double> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == 1.1);
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == 2.2);
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == 3.3);
    }

    [[codegen::luawrap]] void funcDoubleOptional(std::optional<double> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == 1.1);
    }

    [[codegen::luawrap]] void funcDoubleOptionalNullopt(std::optional<double> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDoubleVector(std::vector<double> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == 1.1);
        CHECK(arg[1] == 2.2);
        CHECK(arg[2] == 3.3);
    }

    [[codegen::luawrap]] void funcDoubleArray1(std::array<double, 1> arg) {
        CHECK(arg[0] == 1.1);
    }

    [[codegen::luawrap]] void funcDoubleArray2(std::array<double, 5> arg) {
        CHECK(arg[0] == 1.1);
        CHECK(arg[1] == 2.2);
        CHECK(arg[2] == 3.3);
        CHECK(arg[3] == 4.4);
        CHECK(arg[4] == 5.5);
    }

    [[codegen::luawrap]] void funcDoubleArray3(std::array<double, 10> arg) {
        CHECK(arg[0] == 1.1);
        CHECK(arg[1] == 2.2);
        CHECK(arg[2] == 3.3);
        CHECK(arg[3] == 4.4);
        CHECK(arg[4] == 5.5);
        CHECK(arg[5] == 6.6);
        CHECK(arg[6] == 7.7);
        CHECK(arg[7] == 8.8);
        CHECK(arg[8] == 9.9);
        CHECK(arg[9] == 10.10);
    }

    [[codegen::luawrap]] double returnDouble() {
        return 1.1;
    }

    [[codegen::luawrap]] std::map<std::string, double> returnDoubleMap() {
        return {
            { "key1", 1.1 },
            { "key2", 2.2 },
            { "key3", 3.3 }
        };
    }

    [[codegen::luawrap]] std::optional<double> returnDoubleOptional() {
        return 1.1;
    }

    [[codegen::luawrap]] std::optional<double> returnDoubleOptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<double> returnDoubleVector() {
        return { 1.1, 2.2, 3.3 };
    }
} // namespace
#include "execution_luawrapper_types_double_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  double", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDouble;
    CHECK(func.name == "funcDouble");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, 1.1);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  doubleDefaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDoubleDefaulted;
    CHECK(func.name == "funcDoubleDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number?");
    CHECK(func.arguments[0].defaultValue == "2.2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, 2.2);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  doubleDefaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDoubleDefaulted;
    CHECK(func.name == "funcDoubleDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number?");
    CHECK(func.arguments[0].defaultValue == "2.2");
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
    "Execution/LuaWrapper/Arguments:  doubleDefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDoubleDefaultedCheck;
    CHECK(func.name == "funcDoubleDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Number?");
    CHECK(func.arguments[1].defaultValue == "2.2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, 3.3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  doubleDefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDoubleDefaultedCheck;
    CHECK(func.name == "funcDoubleDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Number?");
    CHECK(func.arguments[1].defaultValue == "2.2");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  doubleMap", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDoubleMap;
    CHECK(func.name == "funcDoubleMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> Number");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", 1.1);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", 2.2);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", 3.3);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  doubleOptional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDoubleOptional;
    CHECK(func.name == "funcDoubleOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, 1.1);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  doubleOptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDoubleOptionalNullopt;
    CHECK(func.name == "funcDoubleOptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  doubleVector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDoubleVector;
    CHECK(func.name == "funcDoubleVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, 1.1);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, 2.2);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, 3.3);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  doubleArray1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDoubleArray1;
    CHECK(func.name == "funcDoubleArray1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, 1.1);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<double, 1> { 1.1 } );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  doubleArray2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDoubleArray2;
    CHECK(func.name == "funcDoubleArray2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, 1.1);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, 2.2);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, 3.3);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, 4.4);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, 5.5);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<double, 5> { 1.1, 2.2, 3.3, 4.4, 5.5 });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  doubleArray3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDoubleArray3;
    CHECK(func.name == "funcDoubleArray3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, 1.1);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, 2.2);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, 3.3);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, 4.4);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, 5.5);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, 6.6);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, 7.7);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, 8.8);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, 9.9);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, 10.10);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<double, 10> { 1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10 });
    func.function(state);
    CHECK(lua_gettop(state) == 0);


    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  double", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDouble;
    CHECK(func.name == "returnDouble");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const double val = ghoul::lua::value<double>(state);
    CHECK(val == 1.1);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  doubleMap", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDoubleMap;
    CHECK(func.name == "returnDoubleMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> Number");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::map<std::string, double>& val =
        ghoul::lua::value<std::map<std::string, double>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == 1.1);
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == 2.2);
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == 3.3);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  doubleOptional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDoubleOptional;
    CHECK(func.name == "returnDoubleOptional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const double val = ghoul::lua::value<double>(state);
    CHECK(val == 1.1);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  doubleOptional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDoubleOptionalNullopt;
    CHECK(func.name == "returnDoubleOptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  doubleVector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDoubleVector;
    CHECK(func.name == "returnDoubleVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<double> val = ghoul::lua::value<std::vector<double>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == 1.1);
    CHECK(val[1] == 2.2);
    CHECK(val[2] == 3.3);
    lua_close(state);
}
