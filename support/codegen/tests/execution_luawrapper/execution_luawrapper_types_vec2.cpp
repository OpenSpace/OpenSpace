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
    [[codegen::luawrap]] void funcVec2(glm::vec2 arg) {
        CHECK(arg == glm::vec2(1.1f, 2.2f));
    }

    [[codegen::luawrap]] void funcVec2Defaulted(glm::vec2 arg = glm::vec2(1.1f, 2.2f)) {
        CHECK(arg == glm::vec2(1.1f, 2.2f));
    }

    [[codegen::luawrap]] void funcVec2DefaultedCheck(bool isDefaulted, glm::vec2 arg = glm::vec2(1.1f, 2.2f)) {
        if (isDefaulted) {
            CHECK(arg == glm::vec2(1.1f, 2.2f));
        }
        else {
            CHECK(arg == glm::vec2(3.3f, 4.4f));
        }
    }

    [[codegen::luawrap]] void funcVec2Map(std::map<std::string, glm::vec2> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::vec2(1.1f, 2.2f));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::vec2(3.3f, 4.4f));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::vec2(5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcVec2Optional(std::optional<glm::vec2> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::vec2(1.1f, 2.2f));
    }

    [[codegen::luawrap]] void funcVec2OptionalNullopt(std::optional<glm::vec2> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcVec2Vector(std::vector<glm::vec2> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::vec2(1.1f, 2.2f));
        CHECK(arg[1] == glm::vec2(3.3f, 4.4f));
        CHECK(arg[2] == glm::vec2(5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcVec2Array1(std::array<glm::vec2, 1> arg) {
        CHECK(arg[0] == glm::vec2(1.1f, 2.2f));
    }

    [[codegen::luawrap]] void funcVec2Array2(std::array<glm::vec2, 5> arg) {
        CHECK(arg[0] == glm::vec2(1.1f, 2.2f));
        CHECK(arg[1] == glm::vec2(3.3f, 4.4f));
        CHECK(arg[2] == glm::vec2(5.5f, 6.6f));
        CHECK(arg[3] == glm::vec2(7.7f, 8.8f));
        CHECK(arg[4] == glm::vec2(9.9f, 10.10f));
    }

    [[codegen::luawrap]] void funcVec2Array3(std::array<glm::vec2, 10> arg) {
        CHECK(arg[0] == glm::vec2(1.1f, 2.2f));
        CHECK(arg[1] == glm::vec2(3.3f, 4.4f));
        CHECK(arg[2] == glm::vec2(5.5f, 6.6f));
        CHECK(arg[3] == glm::vec2(7.7f, 8.8f));
        CHECK(arg[4] == glm::vec2(9.9f, 10.10f));
        CHECK(arg[5] == glm::vec2(11.11f, 12.12f));
        CHECK(arg[6] == glm::vec2(13.13f, 14.14f));
        CHECK(arg[7] == glm::vec2(15.15f, 16.16f));
        CHECK(arg[8] == glm::vec2(17.17f, 18.18f));
        CHECK(arg[9] == glm::vec2(19.19f, 20.20f));
    }

    [[codegen::luawrap]] glm::vec2 returnVec2() {
        return glm::vec2(1.1f, 2.2f);
    }

    [[codegen::luawrap]] std::map<std::string, glm::vec2> returnVec2Map() {
        return {
            { "key1", glm::vec2(1.1f, 2.2f) },
            { "key2", glm::vec2(3.3f, 4.4f) },
            { "key3", glm::vec2(5.5f, 6.6f) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::vec2> returnVec2Optional() {
        return glm::vec2(1.1f, 2.2f);
    }

    [[codegen::luawrap]] std::optional<glm::vec2> returnVec2OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::vec2> returnVec2Vector() {
        return {
            glm::vec2(1.1f, 2.2f),
            glm::vec2(3.3f, 4.4f),
            glm::vec2(5.5f, 6.6f)
        };
    }
} // namespace
#include "execution_luawrapper_types_vec2_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  vec2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec2;
    CHECK(func.name == "funcVec2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::vec2(1.1f, 2.2f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec2Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec2Defaulted;
    CHECK(func.name == "funcVec2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2?");
    CHECK(func.arguments[0].defaultValue == "glm::vec2(1.1f, 2.2f)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::vec2(1.1f, 2.2f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec2Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec2Defaulted;
    CHECK(func.name == "funcVec2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2?");
    CHECK(func.arguments[0].defaultValue == "glm::vec2(1.1f, 2.2f)");
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
    "Execution/LuaWrapper/Arguments:  vec2DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec2DefaultedCheck;
    CHECK(func.name == "funcVec2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec2?");
    CHECK(func.arguments[1].defaultValue == "glm::vec2(1.1f, 2.2f)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, glm::vec2(3.3f, 4.4f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec2DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec2DefaultedCheck;
    CHECK(func.name == "funcVec2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec2?");
    CHECK(func.arguments[1].defaultValue == "glm::vec2(1.1f, 2.2f)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  vec2Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec2Map;
    CHECK(func.name == "funcVec2Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> vec2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, "key1", glm::vec2(1.1f, 2.2f));
    lua_rawset(state, -3);
    lua::push(state, "key2", glm::vec2(3.3f, 4.4f));
    lua_rawset(state, -3);
    lua::push(state, "key3", glm::vec2(5.5f, 6.6f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec2Optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec2Optional;
    CHECK(func.name == "funcVec2Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::vec2(1.1f, 2.2f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec2OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec2OptionalNullopt;
    CHECK(func.name == "funcVec2OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec2Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec2Vector;
    CHECK(func.name == "funcVec2Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::vec2(1.1f, 2.2f));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::vec2(3.3f, 4.4f));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::vec2(5.5f, 6.6f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec2Array1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec2Array1;
    CHECK(func.name == "funcVec2Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::vec2(1.1f, 2.2f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec2Array2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec2Array2;
    CHECK(func.name == "funcVec2Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::vec2(1.1f, 2.2f));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::vec2(3.3f, 4.4f));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::vec2(5.5f, 6.6f));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::vec2(7.7f, 8.8f));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::vec2(9.9f, 10.10f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec2Array3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec2Array3;
    CHECK(func.name == "funcVec2Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::vec2(1.1f, 2.2f));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::vec2(3.3f, 4.4f));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::vec2(5.5f, 6.6f));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::vec2(7.7f, 8.8f));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::vec2(9.9f, 10.10f));
    lua_rawset(state, -3);
    lua::push(state, 6, glm::vec2(11.11f, 12.12f));
    lua_rawset(state, -3);
    lua::push(state, 7, glm::vec2(13.13f, 14.14f));
    lua_rawset(state, -3);
    lua::push(state, 8, glm::vec2(15.15f, 16.16f));
    lua_rawset(state, -3);
    lua::push(state, 9, glm::vec2(17.17, 18.18));
    lua_rawset(state, -3);
    lua::push(state, 10, glm::vec2(19.19, 20.20));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  vec2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVec2;
    CHECK(func.name == "returnVec2");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::vec2 val = lua::value<glm::vec2>(state);
    CHECK(val == glm::vec2(1.1f, 2.2f));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  vec2Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVec2Map;
    CHECK(func.name == "returnVec2Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> vec2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::vec2> val =
        lua::value<std::map<std::string, glm::vec2>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::vec2(1.1f, 2.2f));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::vec2(3.3f, 4.4f));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::vec2(5.5f, 6.6f));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  vec2Optional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnVec2Optional;
    CHECK(func.name == "returnVec2Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::vec2 val = lua::value<glm::vec2>(state);
    CHECK(val == glm::vec2(1.1f, 2.2));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  vec2Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnVec2OptionalNullopt;
    CHECK(func.name == "returnVec2OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  vec2Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVec2Vector;
    CHECK(func.name == "returnVec2Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec2[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::vec2> val = lua::value<std::vector<glm::vec2>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::vec2(1.1f, 2.2f));
    CHECK(val[1] == glm::vec2(3.3f, 4.4f));
    CHECK(val[2] == glm::vec2(5.5f, 6.6f));
    lua_close(state);
}
