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
    [[codegen::luawrap]] void funcDMat2x3(glm::dmat2x3 arg) {
        CHECK(arg == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    }

    [[codegen::luawrap]] void funcDMat2x3Defaulted(glm::dmat2x3 arg = glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)) {
        CHECK(arg == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    }

    [[codegen::luawrap]] void funcDMat2x3DefaultedCheck(bool isDefaulted, glm::dmat2x3 arg = glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)) {
        if (isDefaulted) {
            CHECK(arg == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
        }
        else {
            CHECK(arg == glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
        }
    }

    [[codegen::luawrap]] void funcDMat2x3Map(std::map<std::string, glm::dmat2x3> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(
            arg.find("key2")->second ==
            glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12)
        );
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(
            arg.find("key3")->second ==
            glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
        );
    }

    [[codegen::luawrap]] void funcDMat2x3Optional(std::optional<glm::dmat2x3> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    }

    [[codegen::luawrap]] void funcDMat2x3OptionalNullopt(std::optional<glm::dmat2x3> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDMat2x3Vector(std::vector<glm::dmat2x3> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
        CHECK(arg[1] == glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
        CHECK(arg[2] == glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18));
    }

    [[codegen::luawrap]] void funcDMat2x3Array1(std::array<glm::dmat2x3, 1> arg) {
        CHECK(arg[0] == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    }

    [[codegen::luawrap]] void funcDMat2x3Array2(std::array<glm::dmat2x3, 5> arg) {
        CHECK(arg[0] == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
        CHECK(arg[1] == glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
        CHECK(arg[2] == glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18));
        CHECK(arg[3] == glm::dmat2x3(19.19, 20.20, 21.21, 22.22, 23.23, 24.24));
        CHECK(arg[4] == glm::dmat2x3(25.25, 26.26, 27.27, 28.28, 29.29, 30.30));
    }

    [[codegen::luawrap]] void funcDMat2x3Array3(std::array<glm::dmat2x3, 10> arg) {
        CHECK(arg[0] == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
        CHECK(arg[1] == glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
        CHECK(arg[2] == glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18));
        CHECK(arg[3] == glm::dmat2x3(19.19, 20.20, 21.21, 22.22, 23.23, 24.24));
        CHECK(arg[4] == glm::dmat2x3(25.25, 26.26, 27.27, 28.28, 29.29, 30.30));
        CHECK(arg[5] == glm::dmat2x3(31.31, 32.32, 33.33, 34.34, 35.35, 36.36));
        CHECK(arg[6] == glm::dmat2x3(37.37, 38.38, 39.39, 40.40, 41.41, 42.42));
        CHECK(arg[7] == glm::dmat2x3(43.43, 44.44, 45.45, 46.46, 47.47, 48.48));
        CHECK(arg[8] == glm::dmat2x3(49.49, 50.50, 51.51, 52.52, 53.53, 54.54));
        CHECK(arg[9] == glm::dmat2x3(55.55, 56.56, 57.57, 58.58, 59.59, 60.60));
    }

    [[codegen::luawrap]] glm::dmat2x3 returnDMat2x3() {
        return glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6);
    }

    [[codegen::luawrap]] std::map<std::string, glm::dmat2x3> returnDMat2x3Map() {
        return {
            { "key1", glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6) },
            { "key2", glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12) },
            { "key3", glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::dmat2x3> returnDMat2x3Optional() {
        return glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6);
    }

    [[codegen::luawrap]] std::optional<glm::dmat2x3> returnDMat2x3OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::dmat2x3> returnDMat2x3Vector() {
        return {
            glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6),
            glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12),
            glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
        };
    }
} // namespace
#include "execution_luawrapper_types_dmat2x3_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat2x3;
    CHECK(func.name == "funcDMat2x3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat2x3Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat2x3Defaulted;
    CHECK(func.name == "funcDMat2x3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3?");
    CHECK(func.arguments[0].defaultValue == "glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat2x3Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat2x3Defaulted;
    CHECK(func.name == "funcDMat2x3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3?");
    CHECK(func.arguments[0].defaultValue == "glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)");
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
    "Execution/LuaWrapper/Arguments:  dmat2x3DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat2x3DefaultedCheck;
    CHECK(func.name == "funcDMat2x3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat2x3?");
    CHECK(func.arguments[1].defaultValue == "glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat2x3DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat2x3DefaultedCheck;
    CHECK(func.name == "funcDMat2x3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat2x3?");
    CHECK(func.arguments[1].defaultValue == "glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x3Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat2x3Map;
    CHECK(func.name == "funcDMat2x3Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> mat2x3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        "key3",
        glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x3Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat2x3Optional;
    CHECK(func.name == "funcDMat2x3Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat2x3OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat2x3OptionalNullopt;
    CHECK(func.name == "funcDMat2x3OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x3Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat2x3Vector;
    CHECK(func.name == "funcDMat2x3Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x3Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat2x3Array1;
    CHECK(func.name == "funcDMat2x3Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dmat2x3, 1> {
            glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x3Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat2x3Array2;
    CHECK(func.name == "funcDMat2x3Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        4,
        glm::dmat2x3(19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        5,
        glm::dmat2x3(25.25, 26.26, 27.27, 28.28, 29.29, 30.30)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dmat2x3, 5> {
            glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6),
            glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12),
            glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18),
            glm::dmat2x3(19.19, 20.20, 21.21, 22.22, 23.23, 24.24),
            glm::dmat2x3(25.25, 26.26, 27.27, 28.28, 29.29, 30.30)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat2x3Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat2x3Array3;
    CHECK(func.name == "funcDMat2x3Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        4,
        glm::dmat2x3(19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        5,
        glm::dmat2x3(25.25, 26.26, 27.27, 28.28, 29.29, 30.30)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        6,
        glm::dmat2x3(31.31, 32.32, 33.33, 34.34, 35.35, 36.36)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        7,
        glm::dmat2x3(37.37, 38.38, 39.39, 40.40, 41.41, 42.42)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        8,
        glm::dmat2x3(43.43, 44.44, 45.45, 46.46, 47.47, 48.48)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        9,
        glm::dmat2x3(49.49, 50.50, 51.51, 52.52, 53.53, 54.54)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        10,
        glm::dmat2x3(55.55, 56.56, 57.57, 58.58, 59.59, 60.60)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dmat2x3, 10> {
            glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6),
            glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12),
            glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18),
            glm::dmat2x3(19.19, 20.20, 21.21, 22.22, 23.23, 24.24),
            glm::dmat2x3(25.25, 26.26, 27.27, 28.28, 29.29, 30.30),
            glm::dmat2x3(31.31, 32.32, 33.33, 34.34, 35.35, 36.36),
            glm::dmat2x3(37.37, 38.38, 39.39, 40.40, 41.41, 42.42),
            glm::dmat2x3(43.43, 44.44, 45.45, 46.46, 47.47, 48.48),
            glm::dmat2x3(49.49, 50.50, 51.51, 52.52, 53.53, 54.54),
            glm::dmat2x3(55.55, 56.56, 57.57, 58.58, 59.59, 60.60)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat2x3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDMat2x3;
    CHECK(func.name == "returnDMat2x3");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat2x3 val = ghoul::lua::value<glm::dmat2x3>(state);
    CHECK(val == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat2x3Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDMat2x3Map;
    CHECK(func.name == "returnDMat2x3Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat2x3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::dmat2x3> val =
        ghoul::lua::value<std::map<std::string, glm::dmat2x3>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
    REQUIRE(val.find("key3") != val.end());
    CHECK(
        val.find("key3")->second ==
        glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18)
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat2x3Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDMat2x3Optional;
    CHECK(func.name == "returnDMat2x3Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat2x3 val = ghoul::lua::value<glm::dmat2x3>(state);
    CHECK(val == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat2x3Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDMat2x3OptionalNullopt;
    CHECK(func.name == "returnDMat2x3OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat2x3Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDMat2x3Vector;
    CHECK(func.name == "returnDMat2x3Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x3[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::dmat2x3> val = ghoul::lua::value<std::vector<glm::dmat2x3>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::dmat2x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6));
    CHECK(val[1] == glm::dmat2x3(7.7, 8.8, 9.9, 10.10, 11.11, 12.12));
    CHECK(val[2] == glm::dmat2x3(13.13, 14.14, 15.15, 16.16, 17.17, 18.18));
    lua_close(state);
}
