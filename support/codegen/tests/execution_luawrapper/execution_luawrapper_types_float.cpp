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
    [[codegen::luawrap]] void funcFloat(float arg) {
        CHECK(arg == 1.1f);
    }

    [[codegen::luawrap]] void funcFloatDefaulted(float arg = 1.1f) {
        CHECK(arg == 1.1f);
    }

    [[codegen::luawrap]] void funcFloatDefaultedCheck(bool isDefaulted, float arg = 2.2f) {
        if (isDefaulted) {
            CHECK(arg == 2.2f);
        }
        else {
            CHECK(arg == 3.3f);
        }
    }

    [[codegen::luawrap]] void funcFloatMap(std::map<std::string, float> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == 1.1f);
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == 2.2f);
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == 3.3f);
    }

    [[codegen::luawrap]] void funcFloatOptional(std::optional<float> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == 1.1f);
    }

    [[codegen::luawrap]] void funcFloatOptionalNullopt(std::optional<float> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcFloatVector(std::vector<float> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == 1.1f);
        CHECK(arg[1] == 2.2f);
        CHECK(arg[2] == 3.3f);
    }

    [[codegen::luawrap]] void funcFloatArray1(std::array<float, 1> arg) {
        CHECK(arg[0] == 1.1f);
    }

    [[codegen::luawrap]] void funcFloatArray2(std::array<float, 5> arg) {
        CHECK(arg[0] == 1.1f);
        CHECK(arg[1] == 2.2f);
        CHECK(arg[2] == 3.3f);
        CHECK(arg[3] == 4.4f);
        CHECK(arg[4] == 5.5f);
    }

    [[codegen::luawrap]] void funcFloatArray3(std::array<float, 10> arg) {
        CHECK(arg[0] == 1.1f);
        CHECK(arg[1] == 2.2f);
        CHECK(arg[2] == 3.3f);
        CHECK(arg[3] == 4.4f);
        CHECK(arg[4] == 5.5f);
        CHECK(arg[5] == 6.6f);
        CHECK(arg[6] == 7.7f);
        CHECK(arg[7] == 8.8f);
        CHECK(arg[8] == 9.9f);
        CHECK(arg[9] == 10.10f);
    }

    [[codegen::luawrap]] float returnFloat() {
        return 1.1f;
    }

    [[codegen::luawrap]] std::map<std::string, float> returnFloatMap() {
        return {
            { "key1", 1.1f },
            { "key2", 2.2f },
            { "key3", 3.3f }
        };
    }

    [[codegen::luawrap]] std::optional<float> returnFloatOptional() {
        return 1.1f;
    }

    [[codegen::luawrap]] std::optional<float> returnFloatOptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<float> returnFloatVector() {
        return { 1.1f, 2.2f, 3.3f };
    }
} // namespace
#include "execution_luawrapper_types_float_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  float", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncFloat;
    CHECK(func.name == "funcFloat");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, 1.1f);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  floatDefaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncFloatDefaulted;
    CHECK(func.name == "funcFloatDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number?");
    CHECK(func.arguments[0].defaultValue == "1.1f");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, 1.1f);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  floatDefaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncFloatDefaulted;
    CHECK(func.name == "funcFloatDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number?");
    CHECK(func.arguments[0].defaultValue == "1.1f");
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
    "Execution/LuaWrapper/Arguments:  floatDefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncFloatDefaultedCheck;
    CHECK(func.name == "funcFloatDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Number?");
    CHECK(func.arguments[1].defaultValue == "2.2f");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, 3.3f);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  floatDefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncFloatDefaultedCheck;
    CHECK(func.name == "funcFloatDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Number?");
    CHECK(func.arguments[1].defaultValue == "2.2f");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  floatMap", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncFloatMap;
    CHECK(func.name == "funcFloatMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> Number");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", 1.1f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", 2.2f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", 3.3f);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  floatOptional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncFloatOptional;
    CHECK(func.name == "funcFloatOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, 1.1f);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  floatOptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncFloatOptionalNullopt;
    CHECK(func.name == "funcFloatOptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  floatVector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncFloatVector;
    CHECK(func.name == "funcFloatVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, 1.1f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, 2.2f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, 3.3f);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  floatArray1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncFloatArray1;
    CHECK(func.name == "funcFloatArray1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, 1.1f);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<float, 1> { 1.1f });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  floatArray2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncFloatArray2;
    CHECK(func.name == "funcFloatArray2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, 1.1f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, 2.2f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, 3.3f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, 4.4f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, 5.5f);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<float, 5> { 1.1f, 2.2f, 3.3f, 4.4f, 5.5f });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  floatArray3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncFloatArray3;
    CHECK(func.name == "funcFloatArray3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, 1.1f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, 2.2f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, 3.3f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, 4.4f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, 5.5f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, 6.6f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, 7.7f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, 8.8f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, 9.9f);
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, 10.10f);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<float, 10> {
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  float", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnFloat;
    CHECK(func.name == "returnFloat");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const float val = ghoul::lua::value<float>(state);
    CHECK(val == 1.1f);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  floatMap", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnFloatMap;
    CHECK(func.name == "returnFloatMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> Number");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, float> val =
        ghoul::lua::value<std::map<std::string, float>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == 1.1f);
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == 2.2f);
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == 3.3f);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  floatOptional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnFloatOptional;
    CHECK(func.name == "returnFloatOptional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const float val = ghoul::lua::value<float>(state);
    CHECK(val == 1.1f);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  floatOptional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnFloatOptionalNullopt;
    CHECK(func.name == "returnFloatOptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Return:  floatVector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnFloatVector;
    CHECK(func.name == "returnFloatVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<float> val = ghoul::lua::value<std::vector<float>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == 1.1f);
    CHECK(val[1] == 2.2f);
    CHECK(val[2] == 3.3f);
    lua_close(state);
}
