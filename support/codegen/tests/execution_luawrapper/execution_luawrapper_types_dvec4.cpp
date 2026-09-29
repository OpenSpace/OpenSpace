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
    [[codegen::luawrap]] void funcDvec4(glm::dvec4 arg) {
        CHECK(arg == glm::dvec4(1.1, 2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDvec4Defaulted(glm::dvec4 arg = glm::dvec4(1.1, 2.2, 3.3, 4.4)) {
        CHECK(arg == glm::dvec4(1.1, 2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDvec4DefaultedCheck(bool isDefaulted, glm::dvec4 arg = glm::dvec4(1.1, 2.2, 3.3, 4.4)) {
        if (isDefaulted) {
            CHECK(arg == glm::dvec4(1.1, 2.2, 3.3, 4.4));
        }
        else {
            CHECK(arg == glm::dvec4(5.5, 6.6, 7.7, 8.8));
        }
    }

    [[codegen::luawrap]] void funcDvec4Map(std::map<std::string, glm::dvec4> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::dvec4(1.1, 2.2, 3.3, 4.4));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::dvec4(5.5, 6.6, 7.7, 8.8));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::dvec4(9.9, 10.10, 11.11, 12.12));
    }

    [[codegen::luawrap]] void funcDvec4Optional(std::optional<glm::dvec4> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::dvec4(1.1, 2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDvec4OptionalNullopt(std::optional<glm::dvec4> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDvec4Vector(std::vector<glm::dvec4> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::dvec4(1.1, 2.2, 3.3, 4.4));
        CHECK(arg[1] == glm::dvec4(5.5, 6.6, 7.7, 8.8));
        CHECK(arg[2] == glm::dvec4(9.9, 10.10, 11.11, 12.12));
    }

    [[codegen::luawrap]] void funcDvec4Array1(std::array<glm::dvec4, 1> arg) {
        CHECK(arg[0] == glm::dvec4(1.1, 2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDvec4Array2(std::array<glm::dvec4, 5> arg) {
        CHECK(arg[0] == glm::dvec4(1.1, 2.2, 3.3, 4.4));
        CHECK(arg[1] == glm::dvec4(5.5, 6.6, 7.7, 8.8));
        CHECK(arg[2] == glm::dvec4(9.9, 10.10, 11.11, 12.12));
        CHECK(arg[3] == glm::dvec4(13.13, 14.14, 15.15, 16.16));
        CHECK(arg[4] == glm::dvec4(17.17, 18.18, 19.19, 20.20));
    }

    [[codegen::luawrap]] void funcDvec4Array3(std::array<glm::dvec4, 10> arg) {
        CHECK(arg[0] == glm::dvec4(1.1, 2.2, 3.3, 4.4));
        CHECK(arg[1] == glm::dvec4(5.5, 6.6, 7.7, 8.8));
        CHECK(arg[2] == glm::dvec4(9.9, 10.10, 11.11, 12.12));
        CHECK(arg[3] == glm::dvec4(13.13, 14.14, 15.15, 16.16));
        CHECK(arg[4] == glm::dvec4(17.17, 18.18, 19.19, 20.20));
        CHECK(arg[5] == glm::dvec4(21.21, 22.22, 23.23, 24.24));
        CHECK(arg[6] == glm::dvec4(25.25, 26.26, 27.27, 28.28));
        CHECK(arg[7] == glm::dvec4(29.29, 30.30, 31.31, 32.32));
        CHECK(arg[8] == glm::dvec4(33.33, 34.34, 35.35, 36.36));
        CHECK(arg[9] == glm::dvec4(37.37, 38.38, 39.39, 40.40));
    }

    [[codegen::luawrap]] glm::dvec4 returnDvec4() {
        return glm::dvec4(1.1, 2.2, 3.3, 4.4);
    }

    [[codegen::luawrap]] std::map<std::string, glm::dvec4> returnDvec4Map() {
        return {
            { "key1", glm::dvec4(1.1, 2.2, 3.3, 4.4) },
            { "key2", glm::dvec4(5.5, 6.6, 7.7, 8.8) },
            { "key3", glm::dvec4(9.9, 10.10, 11.11, 12.12) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::dvec4> returnDvec4Optional() {
        return glm::dvec4(1.1, 2.2, 3.3, 4.4);
    }

    [[codegen::luawrap]] std::optional<glm::dvec4> returnDvec4OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::dvec4> returnDvec4Vector() {
        return {
            glm::dvec4(1.1, 2.2, 3.3, 4.4),
            glm::dvec4(5.5, 6.6, 7.7, 8.8),
            glm::dvec4(9.9, 10.10, 11.11, 12.12)
        };
    }
} // namespace
#include "execution_luawrapper_types_dvec4_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  dvec4", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDvec4;
    CHECK(func.name == "funcDvec4");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::dvec4(1.1, 2.2, 3.3, 4.4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec4Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDvec4Defaulted;
    CHECK(func.name == "funcDvec4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4?");
    CHECK(func.arguments[0].defaultValue == "glm::dvec4(1.1, 2.2, 3.3, 4.4)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::dvec4(1.1, 2.2, 3.3, 4.4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec4Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDvec4Defaulted;
    CHECK(func.name == "funcDvec4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4?");
    CHECK(func.arguments[0].defaultValue == "glm::dvec4(1.1, 2.2, 3.3, 4.4)");
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
    "Execution/LuaWrapper/Arguments:  dvec4DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDvec4DefaultedCheck;
    CHECK(func.name == "funcDvec4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec4?");
    CHECK(func.arguments[1].defaultValue == "glm::dvec4(1.1, 2.2, 3.3, 4.4)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, glm::dvec4(5.5, 6.6, 7.7, 8.8));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec4DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDvec4DefaultedCheck;
    CHECK(func.name == "funcDvec4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec4?");
    CHECK(func.arguments[1].defaultValue == "glm::dvec4(1.1, 2.2, 3.3, 4.4)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec4Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDvec4Map;
    CHECK(func.name == "funcDvec4Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> vec4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, "key1", glm::dvec4(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua::push(state, "key2", glm::dvec4(5.5, 6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua::push(state, "key3", glm::dvec4(9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec4Optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDvec4Optional;
    CHECK(func.name == "funcDvec4Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::dvec4(1.1, 2.2, 3.3, 4.4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec4OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDvec4OptionalNullopt;
    CHECK(func.name == "funcDvec4OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec4Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDvec4Vector;
    CHECK(func.name == "funcDvec4Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::dvec4(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::dvec4(5.5, 6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::dvec4(9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec4Array1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDvec4Array1;
    CHECK(func.name == "funcDvec4Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::dvec4(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(state, std::array<glm::dvec4, 1> { glm::dvec4(1.1, 2.2, 3.3, 4.4) });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec4Array2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDvec4Array2;
    CHECK(func.name == "funcDvec4Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::dvec4(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::dvec4(5.5, 6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::dvec4(9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::dvec4(13.13, 14.14, 15.15, 16.16));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::dvec4(17.17, 18.18, 19.19, 20.20));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dvec4, 5> {
            glm::dvec4(1.1, 2.2, 3.3, 4.4),
            glm::dvec4(5.5, 6.6, 7.7, 8.8),
            glm::dvec4(9.9, 10.10, 11.11, 12.12),
            glm::dvec4(13.13, 14.14, 15.15, 16.16),
            glm::dvec4(17.17, 18.18, 19.19, 20.20)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec4Array3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDvec4Array3;
    CHECK(func.name == "funcDvec4Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec4[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::dvec4(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::dvec4(5.5, 6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::dvec4(9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::dvec4(13.13, 14.14, 15.15, 16.16));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::dvec4(17.17, 18.18, 19.19, 20.20));
    lua_rawset(state, -3);
    lua::push(state, 6, glm::dvec4(21.21, 22.22, 23.23, 24.24));
    lua_rawset(state, -3);
    lua::push(state, 7, glm::dvec4(25.25, 26.26, 27.27, 28.28));
    lua_rawset(state, -3);
    lua::push(state, 8, glm::dvec4(29.29, 30.30, 31.31, 32.32));
    lua_rawset(state, -3);
    lua::push(state, 9, glm::dvec4(33.33, 34.34, 35.35, 36.36));
    lua_rawset(state, -3);
    lua::push(state, 10, glm::dvec4(37.37, 38.38, 39.39, 40.40));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dvec4, 10> {
            glm::dvec4(1.1, 2.2, 3.3, 4.4),
            glm::dvec4(5.5, 6.6, 7.7, 8.8),
            glm::dvec4(9.9, 10.10, 11.11, 12.12),
            glm::dvec4(13.13, 14.14, 15.15, 16.16),
            glm::dvec4(17.17, 18.18, 19.19, 20.20),
            glm::dvec4(21.21, 22.22, 23.23, 24.24),
            glm::dvec4(25.25, 26.26, 27.27, 28.28),
            glm::dvec4(29.29, 30.30, 31.31, 32.32),
            glm::dvec4(33.33, 34.34, 35.35, 36.36),
            glm::dvec4(37.37, 38.38, 39.39, 40.40)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dvec4", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDvec4;
    CHECK(func.name == "returnDvec4");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dvec4 val = lua::value<glm::dvec4>(state);
    CHECK(val == glm::dvec4(1.1, 2.2, 3.3, 4.4));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dvec4Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDvec4Map;
    CHECK(func.name == "returnDvec4Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> vec4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::dvec4> val =
        lua::value<std::map<std::string, glm::dvec4>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::dvec4(1.1, 2.2, 3.3, 4.4));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::dvec4(5.5, 6.6, 7.7, 8.8));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::dvec4(9.9, 10.10, 11.11, 12.12));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dvec4Optional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnDvec4Optional;
    CHECK(func.name == "returnDvec4Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec4?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dvec4 val = lua::value<glm::dvec4>(state);
    CHECK(val == glm::dvec4(1.1, 2.2, 3.3, 4.4));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dvec4Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnDvec4OptionalNullopt;
    CHECK(func.name == "returnDvec4OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Return:  dvec4Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDvec4Vector;
    CHECK(func.name == "returnDvec4Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec4[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::dvec4> val = lua::value<std::vector<glm::dvec4>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::dvec4(1.1, 2.2, 3.3, 4.4));
    CHECK(val[1] == glm::dvec4(5.5, 6.6, 7.7, 8.8));
    CHECK(val[2] == glm::dvec4(9.9, 10.10, 11.11, 12.12));
    lua_close(state);
}
