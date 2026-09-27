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
    [[codegen::luawrap]] void funcVec4(glm::vec4 arg) {
        CHECK(arg == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    }

    [[codegen::luawrap]] void funcVec4Defaulted(glm::vec4 arg = glm::vec4(1.1f, 2.2f, 3.3f, 4.4f)) {
        CHECK(arg == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    }

    [[codegen::luawrap]] void funcVec4DefaultedCheck(bool isDefaulted, glm::vec4 arg = glm::vec4(1.1f, 2.2f, 3.3f, 4.4f)) {
        if (isDefaulted) {
            CHECK(arg == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
        }
        else {
            CHECK(arg == glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
        }
    }

    [[codegen::luawrap]] void funcVec4Map(std::map<std::string, glm::vec4> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
    }

    [[codegen::luawrap]] void funcVec4Optional(std::optional<glm::vec4> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    }

    [[codegen::luawrap]] void funcVec4OptionalNullopt(std::optional<glm::vec4> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcVec4Vector(std::vector<glm::vec4> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
        CHECK(arg[1] == glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
        CHECK(arg[2] == glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
    }

    [[codegen::luawrap]] void funcVec4Array1(std::array<glm::vec4, 1> arg) {
        CHECK(arg[0] == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    }

    [[codegen::luawrap]] void funcVec4Array2(std::array<glm::vec4, 5> arg) {
        CHECK(arg[0] == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
        CHECK(arg[1] == glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
        CHECK(arg[2] == glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[3] == glm::vec4(13.13f, 14.14f, 15.15f, 16.16f));
        CHECK(arg[4] == glm::vec4(17.17f, 18.18f, 19.19f, 20.20f));
    }

    [[codegen::luawrap]] void funcVec4Array3(std::array<glm::vec4, 10> arg) {
        CHECK(arg[0] == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
        CHECK(arg[1] == glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
        CHECK(arg[2] == glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[3] == glm::vec4(13.13f, 14.14f, 15.15f, 16.16f));
        CHECK(arg[4] == glm::vec4(17.17f, 18.18f, 19.19f, 20.20f));
        CHECK(arg[5] == glm::vec4(21.21f, 22.22f, 23.23f, 24.24f));
        CHECK(arg[6] == glm::vec4(25.25f, 26.26f, 27.27f, 28.28f));
        CHECK(arg[7] == glm::vec4(29.29f, 30.30f, 31.31f, 32.32f));
        CHECK(arg[8] == glm::vec4(33.33f, 34.34f, 35.35f, 36.36f));
        CHECK(arg[9] == glm::vec4(37.37f, 38.38f, 39.39f, 40.40f));
    }

    [[codegen::luawrap]] glm::vec4 returnVec4() {
        return glm::vec4(1.1f, 2.2f, 3.3f, 4.4f);
    }

    [[codegen::luawrap]] std::map<std::string, glm::vec4> returnVec4Map() {
        return {
            { "key1", glm::vec4(1.1f, 2.2f, 3.3f, 4.4f) },
            { "key2", glm::vec4(5.5f, 6.6f, 7.7f, 8.8f) },
            { "key3", glm::vec4(9.9f, 10.10f, 11.11f, 12.12f) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::vec4> returnVec4Optional() {
        return glm::vec4(1.1f, 2.2f, 3.3f, 4.4f);
    }

    [[codegen::luawrap]] std::optional<glm::vec4> returnVec4OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::vec4> returnVec4Vector() {
        return {
            glm::vec4(1.1f, 2.2f, 3.3f, 4.4f),
            glm::vec4(5.5f, 6.6f, 7.7f, 8.8f),
            glm::vec4(9.9f, 10.10f, 11.11f, 12.12f)
        };
    }
} // namespace
#include "execution_luawrapper_types_vec4_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  vec4", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncVec4;
    CHECK(func.name == "funcVec4");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec4Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncVec4Defaulted;
    CHECK(func.name == "funcVec4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4?");
    CHECK(func.arguments[0].defaultValue == "glm::vec4(1.1f, 2.2f, 3.3f, 4.4f)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec4Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncVec4Defaulted;
    CHECK(func.name == "funcVec4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4?");
    CHECK(func.arguments[0].defaultValue == "glm::vec4(1.1f, 2.2f, 3.3f, 4.4f)");
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
    "Execution/LuaWrapper/Arguments:  vec4DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncVec4DefaultedCheck;
    CHECK(func.name == "funcVec4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec4?");
    CHECK(func.arguments[1].defaultValue == "glm::vec4(1.1f, 2.2f, 3.3f, 4.4f)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec4DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncVec4DefaultedCheck;
    CHECK(func.name == "funcVec4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec4?");
    CHECK(func.arguments[1].defaultValue == "glm::vec4(1.1f, 2.2f, 3.3f, 4.4f)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  vec4Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncVec4Map;
    CHECK(func.name == "funcVec4Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> vec4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec4Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncVec4Optional;
    CHECK(func.name == "funcVec4Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  vec4OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncVec4OptionalNullopt;
    CHECK(func.name == "funcVec4OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec4Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncVec4Vector;
    CHECK(func.name == "funcVec4Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec4Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncVec4Array1;
    CHECK(func.name == "funcVec4Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec4Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncVec4Array2;
    CHECK(func.name == "funcVec4Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::vec4(13.13f, 14.14f, 15.15f, 16.16f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::vec4(17.17f, 18.18f, 19.19f, 20.20f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  vec4Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncVec4Array3;
    CHECK(func.name == "funcVec4Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::vec4(13.13f, 14.14f, 15.15f, 16.16f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::vec4(17.17f, 18.18f, 19.19f, 20.20f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, glm::vec4(21.21f, 22.22f, 23.23f, 24.24f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, glm::vec4(25.25f, 26.26f, 27.27f, 28.28f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, glm::vec4(29.29f, 30.30f, 31.31f, 32.32f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, glm::vec4(33.33f, 34.34f, 35.35f, 36.36f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, glm::vec4(37.37f, 38.38f, 39.39f, 40.40f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  vec4", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnVec4;
    CHECK(func.name == "returnVec4");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::vec4 val = ghoul::lua::value<glm::vec4>(state);
    CHECK(val == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  vec4Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnVec4Map;
    CHECK(func.name == "returnVec4Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> vec4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::vec4> val =
        ghoul::lua::value<std::map<std::string, glm::vec4>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  vec4Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnVec4Optional;
    CHECK(func.name == "returnVec4Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec4?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::vec4 val = ghoul::lua::value<glm::vec4>(state);
    CHECK(val == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  vec4Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnVec4OptionalNullopt;
    CHECK(func.name == "returnVec4OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec4?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  vec4Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnVec4Vector;
    CHECK(func.name == "returnVec4Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec4[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::vec4> val = ghoul::lua::value<std::vector<glm::vec4>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::vec4(1.1f, 2.2f, 3.3f, 4.4f));
    CHECK(val[1] == glm::vec4(5.5f, 6.6f, 7.7f, 8.8f));
    CHECK(val[2] == glm::vec4(9.9f, 10.10f, 11.11f, 12.12f));
    lua_close(state);
}
