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
    [[codegen::luawrap]] void funcVec3(glm::vec3 arg) {
        CHECK(arg == glm::vec3(1.1f, 2.2f, 3.3f));
    }

    [[codegen::luawrap]] void funcVec3Defaulted(glm::vec3 arg = glm::vec3(1.1f, 2.2f, 3.3f)) {
        CHECK(arg == glm::vec3(1.1f, 2.2f, 3.3f));
    }

    [[codegen::luawrap]] void funcVec3DefaultedCheck(bool isDefaulted, glm::vec3 arg = glm::vec3(1.1f, 2.2f, 3.3f)) {
        if (isDefaulted) {
            CHECK(arg == glm::vec3(1.1f, 2.2f, 3.3f));
        }
        else {
            CHECK(arg == glm::vec3(4.4f, 5.5f, 6.6f));
        }
    }

    [[codegen::luawrap]] void funcVec3Map(std::map<std::string, glm::vec3> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::vec3(1.1f, 2.2f, 3.3f));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::vec3(4.4f, 5.5f, 6.6f));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::vec3(7.7f, 8.8f, 9.9f));
    }

    [[codegen::luawrap]] void funcVec3Optional(std::optional<glm::vec3> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::vec3(1.1f, 2.2f, 3.3f));
    }

    [[codegen::luawrap]] void funcVec3OptionalNullopt(std::optional<glm::vec3> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcVec3Vector(std::vector<glm::vec3> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::vec3(1.1f, 2.2f, 3.3f));
        CHECK(arg[1] == glm::vec3(4.4f, 5.5f, 6.6f));
        CHECK(arg[2] == glm::vec3(7.7f, 8.8f, 9.9f));
    }

    [[codegen::luawrap]] void funcVec3Array1(std::array<glm::vec3, 1> arg) {
        CHECK(arg[0] == glm::vec3(1.1f, 2.2f, 3.3f));
    }

    [[codegen::luawrap]] void funcVec3Array2(std::array<glm::vec3, 5> arg) {
        CHECK(arg[0] == glm::vec3(1.1f, 2.2f, 3.3f));
        CHECK(arg[1] == glm::vec3(4.4f, 5.5f, 6.6f));
        CHECK(arg[2] == glm::vec3(7.7f, 8.8f, 9.9f));
        CHECK(arg[3] == glm::vec3(10.10f, 11.11f, 12.12f));
        CHECK(arg[4] == glm::vec3(13.13f, 14.14f, 15.15f));
    }

    [[codegen::luawrap]] void funcVec3Array3(std::array<glm::vec3, 10> arg) {
        CHECK(arg[0] == glm::vec3(1.1, 2.2, 3.3));
        CHECK(arg[1] == glm::vec3(4.4, 5.5, 6.6));
        CHECK(arg[2] == glm::vec3(7.7, 8.8, 9.9));
        CHECK(arg[3] == glm::vec3(10.10, 11.11, 12.12));
        CHECK(arg[4] == glm::vec3(13.13, 14.14, 15.15));
        CHECK(arg[5] == glm::vec3(16.16, 17.17, 18.18));
        CHECK(arg[6] == glm::vec3(19.19, 20.20, 21.21));
        CHECK(arg[7] == glm::vec3(22.22, 23.23, 24.24));
        CHECK(arg[8] == glm::vec3(25.25, 26.26, 27.27));
        CHECK(arg[9] == glm::vec3(28.28, 29.29, 30.30));
    }

    [[codegen::luawrap]] glm::vec3 returnVec3() {
        return glm::vec3(1.1f, 2.2f, 3.3f);
    }

    [[codegen::luawrap]] std::map<std::string, glm::vec3> returnVec3Map() {
        return {
            { "key1", glm::vec3(1.1f, 2.2f, 3.3f) },
            { "key2", glm::vec3(4.4f, 5.5f, 6.6f) },
            { "key3", glm::vec3(7.7f, 8.8f, 9.9f) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::vec3> returnVec3Optional() {
        return glm::vec3(1.1f, 2.2f, 3.3f);
    }

    [[codegen::luawrap]] std::optional<glm::vec3> returnVec3OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::vec3> returnVec3Vector() {
        return {
            glm::vec3(1.1f, 2.2f, 3.3f),
            glm::vec3(4.4f, 5.5f, 6.6f),
            glm::vec3(7.7f, 8.8f, 9.9f)
        };
    }
} // namespace
#include "execution_luawrapper_types_vec3_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  vec3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec3;
    CHECK(func.name == "funcVec3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::vec3(1.1f, 2.2f, 3.3f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec3Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec3Defaulted;
    CHECK(func.name == "funcVec3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3?");
    CHECK(func.arguments[0].defaultValue == "glm::vec3(1.1f, 2.2f, 3.3f)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::vec3(1.1f, 2.2f, 3.3f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec3Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec3Defaulted;
    CHECK(func.name == "funcVec3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3?");
    CHECK(func.arguments[0].defaultValue == "glm::vec3(1.1f, 2.2f, 3.3f)");
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
    "Execution/LuaWrapper/Arguments:  vec3DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec3DefaultedCheck;
    CHECK(func.name == "funcVec3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec3?");
    CHECK(func.arguments[1].defaultValue == "glm::vec3(1.1f, 2.2f, 3.3f)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, glm::vec3(4.4f, 5.5f, 6.6f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec3DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec3DefaultedCheck;
    CHECK(func.name == "funcVec3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec3?");
    CHECK(func.arguments[1].defaultValue == "glm::vec3(1.1f, 2.2f, 3.3f)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  vec3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec3Map;
    CHECK(func.name == "funcVec3Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> vec3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, "key1", glm::vec3(1.1f, 2.2f, 3.3f));
    lua_rawset(state, -3);
    lua::push(state, "key2", glm::vec3(4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    lua::push(state, "key3", glm::vec3(7.7f, 8.8f, 9.9f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec3Optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec3Optional;
    CHECK(func.name == "funcVec3Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::vec3(1.1f, 2.2f, 3.3f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec3OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVec3OptionalNullopt;
    CHECK(func.name == "funcVec3OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec3Vector;
    CHECK(func.name == "funcVec3Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::vec3(1.1f, 2.2f, 3.3f));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::vec3(4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::vec3(7.7f, 8.8f, 9.9f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec3Array1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec3Array1;
    CHECK(func.name == "funcVec3Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::vec3(1.1f, 2.2f, 3.3f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec3Array2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec3Array2;
    CHECK(func.name == "funcVec3Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::vec3(1.1f, 2.2f, 3.3f));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::vec3(4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::vec3(7.7f, 8.8f, 9.9f));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::vec3(10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::vec3(13.13f, 14.14f, 15.15f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec3Array3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVec3Array3;
    CHECK(func.name == "funcVec3Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::vec3(1.1f, 2.2f, 3.3f));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::vec3(4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::vec3(7.7f, 8.8f, 9.9f));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::vec3(10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::vec3(13.13f, 14.14f, 15.15f));
    lua_rawset(state, -3);
    lua::push(state, 6, glm::vec3(16.16f, 17.17f, 18.18f));
    lua_rawset(state, -3);
    lua::push(state, 7, glm::vec3(19.19f, 20.20f, 21.21f));
    lua_rawset(state, -3);
    lua::push(state, 8, glm::vec3(22.22f, 23.23f, 24.24f));
    lua_rawset(state, -3);
    lua::push(state, 9, glm::vec3(25.25f, 26.26f, 27.27f));
    lua_rawset(state, -3);
    lua::push(state, 10, glm::vec3(28.28f, 29.29f, 30.30f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  vec3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVec3;
    CHECK(func.name == "returnVec3");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::vec3 val = lua::value<glm::vec3>(state);
    CHECK(val == glm::vec3(1.1f, 2.2f, 3.3f));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  vec3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVec3Map;
    CHECK(func.name == "returnVec3Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> vec3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::vec3> val =
        lua::value<std::map<std::string, glm::vec3>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::vec3(1.1f, 2.2f, 3.3f));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::vec3(4.4f, 5.5f, 6.6f));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::vec3(7.7f, 8.8f, 9.9f));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  vec3Optional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnVec3Optional;
    CHECK(func.name == "returnVec3Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::vec3 val = lua::value<glm::vec3>(state);
    CHECK(val == glm::vec3(1.1f, 2.2f, 3.3f));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  vec3Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnVec3OptionalNullopt;
    CHECK(func.name == "returnVec3OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  vec3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVec3Vector;
    CHECK(func.name == "returnVec3Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec3[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::vec3> val = lua::value<std::vector<glm::vec3>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::vec3(1.1f, 2.2f, 3.3f));
    CHECK(val[1] == glm::vec3(4.4f, 5.5f, 6.6f));
    CHECK(val[2] == glm::vec3(7.7f, 8.8f, 9.9f));
    lua_close(state);
}
