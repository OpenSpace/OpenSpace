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
    [[codegen::luawrap]] void funcDMat2x2(glm::dmat2x2 arg) {
        CHECK(arg == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDMat2x2Defaulted(glm::dmat2x2 arg = glm::dmat2x2(1.1, 2.2, 3.3, 4.4)) {
        CHECK(arg == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDMat2x2DefaultedCheck(bool isDefaulted, glm::dmat2x2 arg = glm::dmat2x2(1.1, 2.2, 3.3, 4.4)) {
        if (isDefaulted) {
            CHECK(arg == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
        }
        else {
            CHECK(arg == glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
        }
    }

    [[codegen::luawrap]] void funcDMat2x2Map(std::map<std::string, glm::dmat2x2> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
    }

    [[codegen::luawrap]] void funcDMat2x2Optional(std::optional<glm::dmat2x2> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDMat2x2OptionalNullopt(std::optional<glm::dmat2x2> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDMat2x2Vector(std::vector<glm::dmat2x2> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
        CHECK(arg[1] == glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
        CHECK(arg[2] == glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
    }

    [[codegen::luawrap]] void funcDMat2x2Array1(std::array<glm::dmat2x2, 1> arg) {
        CHECK(arg[0] == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDMat2x2Array2(std::array<glm::dmat2x2, 5> arg) {
        CHECK(arg[0] == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
        CHECK(arg[1] == glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
        CHECK(arg[2] == glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
        CHECK(arg[3] == glm::dmat2x2(13.13, 14.14, 15.15, 16.16));
        CHECK(arg[4] == glm::dmat2x2(17.17, 18.18, 19.19, 20.20));
    }

    [[codegen::luawrap]] void funcDMat2x2Array3(std::array<glm::dmat2x2, 10> arg) {
        CHECK(arg[0] == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
        CHECK(arg[1] == glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
        CHECK(arg[2] == glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
        CHECK(arg[3] == glm::dmat2x2(13.13, 14.14, 15.15, 16.16));
        CHECK(arg[4] == glm::dmat2x2(17.17, 18.18, 19.19, 20.20));
        CHECK(arg[5] == glm::dmat2x2(21.21, 22.22, 23.23, 24.24));
        CHECK(arg[6] == glm::dmat2x2(25.25, 26.26, 27.27, 28.28));
        CHECK(arg[7] == glm::dmat2x2(29.29, 30.30, 31.31, 32.32));
        CHECK(arg[8] == glm::dmat2x2(33.33, 34.34, 35.35, 36.36));
        CHECK(arg[9] == glm::dmat2x2(37.37, 38.38, 39.39, 40.40));
    }

    [[codegen::luawrap]] glm::dmat2x2 returnDMat2x2() {
        return glm::dmat2x2(1.1, 2.2, 3.3, 4.4);
    }

    [[codegen::luawrap]] std::map<std::string, glm::dmat2x2> returnDMat2x2Map() {
        return {
            { "key1", glm::dmat2x2(1.1, 2.2, 3.3, 4.4) },
            { "key2", glm::dmat2x2(5.5, 6.6, 7.7, 8.8) },
            { "key3", glm::dmat2x2(9.9, 10.10, 11.11, 12.12) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::dmat2x2> returnDMat2x2Optional() {
        return glm::dmat2x2(1.1, 2.2, 3.3, 4.4);
    }

    [[codegen::luawrap]] std::optional<glm::dmat2x2> returnDMat2x2OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::dmat2x2> returnDMat2x2Vector() {
        return {
            glm::dmat2x2(1.1, 2.2, 3.3, 4.4),
            glm::dmat2x2(5.5, 6.6, 7.7, 8.8),
            glm::dmat2x2(9.9, 10.10, 11.11, 12.12)
        };
    }
} // namespace
#include "execution_luawrapper_types_dmat2x2_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2;
    CHECK(func.name == "funcDMat2x2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat2x2Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2Defaulted;
    CHECK(func.name == "funcDMat2x2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2?");
    CHECK(func.arguments[0].defaultValue == "glm::dmat2x2(1.1, 2.2, 3.3, 4.4)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat2x2Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2Defaulted;
    CHECK(func.name == "funcDMat2x2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2?");
    CHECK(func.arguments[0].defaultValue == "glm::dmat2x2(1.1, 2.2, 3.3, 4.4)");
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
    "Execution/LuaWrapper/Arguments:  dmat2x2DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2DefaultedCheck;
    CHECK(func.name == "funcDMat2x2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat2x2?");
    CHECK(func.arguments[1].defaultValue == "glm::dmat2x2(1.1, 2.2, 3.3, 4.4)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat2x2DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2DefaultedCheck;
    CHECK(func.name == "funcDMat2x2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat2x2?");
    CHECK(func.arguments[1].defaultValue == "glm::dmat2x2(1.1, 2.2, 3.3, 4.4)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x2Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2Map;
    CHECK(func.name == "funcDMat2x2Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> mat2x2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, "key1", glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua::push(state, "key2", glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua::push(state, "key3", glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x2Optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2Optional;
    CHECK(func.name == "funcDMat2x2Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat2x2OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2OptionalNullopt;
    CHECK(func.name == "funcDMat2x2OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x2Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2Vector;
    CHECK(func.name == "funcDMat2x2Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x2Array1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2Array1;
    CHECK(func.name == "funcDMat2x2Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dmat2x2, 1> { glm::dmat2x2(1.1, 2.2, 3.3, 4.4) }
    );
    func.function(state);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x2Array2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2Array2;
    CHECK(func.name == "funcDMat2x2Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::dmat2x2(13.13, 14.14, 15.15, 16.16));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::dmat2x2(17.17, 18.18, 19.19, 20.20));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dmat2x2, 5> {
            glm::dmat2x2(1.1, 2.2, 3.3, 4.4),
            glm::dmat2x2(5.5, 6.6, 7.7, 8.8),
            glm::dmat2x2(9.9, 10.10, 11.11, 12.12),
            glm::dmat2x2(13.13, 14.14, 15.15, 16.16),
            glm::dmat2x2(17.17, 18.18, 19.19, 20.20)
        }
    );
    func.function(state);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x2Array3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat2x2Array3;
    CHECK(func.name == "funcDMat2x2Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::dmat2x2(13.13, 14.14, 15.15, 16.16));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::dmat2x2(17.17, 18.18, 19.19, 20.20));
    lua_rawset(state, -3);
    lua::push(state, 6, glm::dmat2x2(21.21, 22.22, 23.23, 24.24));
    lua_rawset(state, -3);
    lua::push(state, 7, glm::dmat2x2(25.25, 26.26, 27.27, 28.28));
    lua_rawset(state, -3);
    lua::push(state, 8, glm::dmat2x2(29.29, 30.30, 31.31, 32.32));
    lua_rawset(state, -3);
    lua::push(state, 9, glm::dmat2x2(33.33, 34.34, 35.35, 36.36));
    lua_rawset(state, -3);
    lua::push(state, 10, glm::dmat2x2(37.37, 38.38, 39.39, 40.40));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dmat2x2, 10> {
            glm::dmat2x2(1.1, 2.2, 3.3, 4.4),
            glm::dmat2x2(5.5, 6.6, 7.7, 8.8),
            glm::dmat2x2(9.9, 10.10, 11.11, 12.12),
            glm::dmat2x2(13.13, 14.14, 15.15, 16.16),
            glm::dmat2x2(17.17, 18.18, 19.19, 20.20),
            glm::dmat2x2(21.21, 22.22, 23.23, 24.24),
            glm::dmat2x2(25.25, 26.26, 27.27, 28.28),
            glm::dmat2x2(29.29, 30.30, 31.31, 32.32),
            glm::dmat2x2(33.33, 34.34, 35.35, 36.36),
            glm::dmat2x2(37.37, 38.38, 39.39, 40.40)
        }
    );
    func.function(state);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat2x2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDMat2x2;
    CHECK(func.name == "returnDMat2x2");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat2x2 val = lua::value<glm::dmat2x2>(state);
    CHECK(val == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat2x2Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDMat2x2Map;
    CHECK(func.name == "returnDMat2x2Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat2x2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::dmat2x2> val =
        lua::value<std::map<std::string, glm::dmat2x2>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat2x2Optional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnDMat2x2Optional;
    CHECK(func.name == "returnDMat2x2Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat2x2 val = lua::value<glm::dmat2x2>(state);
    CHECK(val == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat2x2Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnDMat2x2OptionalNullopt;
    CHECK(func.name == "returnDMat2x2OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat2x2Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDMat2x2Vector;
    CHECK(func.name == "returnDMat2x2Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x2[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::dmat2x2> val = lua::value<std::vector<glm::dmat2x2>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    CHECK(val[1] == glm::dmat2x2(5.5, 6.6, 7.7, 8.8));
    CHECK(val[2] == glm::dmat2x2(9.9, 10.10, 11.11, 12.12));
    lua_close(state);
}
