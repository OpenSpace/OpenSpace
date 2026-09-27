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
    [[codegen::luawrap]] void funcDMat4x2(glm::dmat4x2 arg) {
        CHECK(arg == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    }

    [[codegen::luawrap]] void funcDMat4x2Defaulted(glm::dmat4x2 arg = glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)) {
        CHECK(arg == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    }

    [[codegen::luawrap]] void funcDMat4x2DefaultedCheck(bool isDefaulted, glm::dmat4x2 arg = glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)) {
        if (isDefaulted) {
            CHECK(arg == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
        }
        else {
            CHECK(
                arg ==
                glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
            );
        }
    }

    [[codegen::luawrap]] void funcDMat4x2Map(std::map<std::string, glm::dmat4x2> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(
            arg.find("key1")->second ==
            glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
        );
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(
            arg.find("key2")->second ==
            glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
        );
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(
            arg.find("key3")->second ==
            glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
        );
    }

    [[codegen::luawrap]] void funcDMat4x2Optional(std::optional<glm::dmat4x2> arg) {
        REQUIRE(arg.has_value());
        CHECK(
            *arg ==
            glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
        );
    }

    [[codegen::luawrap]] void funcDMat4x2OptionalNullopt(std::optional<glm::dmat4x2> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDMat4x2Vector(std::vector<glm::dmat4x2> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
        CHECK(
            arg[1] ==
            glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
        );
        CHECK(
            arg[2] ==
            glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
        );
    }

    [[codegen::luawrap]] void funcDMat4x2Array1(std::array<glm::dmat4x2, 1> arg) {
        CHECK(arg[0] == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    }

    [[codegen::luawrap]] void funcDMat4x2Array2(std::array<glm::dmat4x2, 5> arg) {
        CHECK(arg[0] == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
        CHECK(arg[1] == glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16));
        CHECK(arg[2] == glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24));
        CHECK(arg[3] == glm::dmat4x2(25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32));
        CHECK(arg[4] == glm::dmat4x2(33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40));
    }

    [[codegen::luawrap]] void funcDMat4x2Array3(std::array<glm::dmat4x2, 10> arg) {
        CHECK(arg[0] == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
        CHECK(arg[1] == glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16));
        CHECK(arg[2] == glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24));
        CHECK(arg[3] == glm::dmat4x2(25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32));
        CHECK(arg[4] == glm::dmat4x2(33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40));
        CHECK(arg[5] == glm::dmat4x2(41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48));
        CHECK(arg[6] == glm::dmat4x2(49.49, 50.50, 51.51, 52.52, 53.53, 54.54, 55.55, 56.56));
        CHECK(arg[7] == glm::dmat4x2(57.57, 58.58, 59.59, 60.60, 61.61, 62.62, 63.63, 64.64));
        CHECK(arg[8] == glm::dmat4x2(65.65, 66.66, 67.67, 68.68, 69.69, 70.70, 71.71, 72.72));
        CHECK(arg[9] == glm::dmat4x2(73.73, 74.74, 75.75, 76.76, 77.77, 78.78, 79.79, 80.80));
    }

    [[codegen::luawrap]] glm::dmat4x2 returnDMat4x2() {
        return glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8);
    }

    [[codegen::luawrap]] std::map<std::string, glm::dmat4x2> returnDMat4x2Map() {
        return {
            {
                "key1",
                glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
            },
            {
                "key2",
                glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
            },
            {
                "key3",
                glm::dmat4x2(
                    17.17, 18.18, 19.19, 20.20,
                    21.21, 22.22, 23.23, 24.24
                )
            }
        };
    }

    [[codegen::luawrap]] std::optional<glm::dmat4x2> returnDMat4x2Optional() {
        return glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8);
    }

    [[codegen::luawrap]] std::optional<glm::dmat4x2> returnDMat4x2OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::dmat4x2> returnDMat4x2Vector() {
        return {
            glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8),
            glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16),
            glm::dmat4x2(
                17.17, 18.18, 19.19, 20.20,
                21.21, 22.22, 23.23, 24.24
            )
        };
    }
} // namespace
#include "execution_luawrapper_types_dmat4x2_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat4x2;
    CHECK(func.name == "funcDMat4x2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x2Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat4x2Defaulted;
    CHECK(func.name == "funcDMat4x2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x2?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x2Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat4x2Defaulted;
    CHECK(func.name == "funcDMat4x2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x2?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)"
    );
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
    "Execution/LuaWrapper/Arguments:  dmat4x2DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat4x2DefaultedCheck;
    CHECK(func.name == "funcDMat4x2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x2?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(
        state,
        false,
        glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x2DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat4x2DefaultedCheck;
    CHECK(func.name == "funcDMat4x2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x2?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)"
    );
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat4x2Map;
    CHECK(func.name == "funcDMat4x2Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> mat4x2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        "key1",
        glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        "key2",
        glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        "key3",
        glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x2Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat4x2Optional;
    CHECK(func.name == "funcDMat4x2Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x2OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDMat4x2OptionalNullopt;
    CHECK(func.name == "funcDMat4x2OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat4x2Vector;
    CHECK(func.name == "funcDMat4x2Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x2[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        1,
        glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        2,
        glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x2Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat4x2Array1;
    CHECK(func.name == "funcDMat4x2Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x2[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        1,
        glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dmat4x2, 1> {
            glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x2Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat4x2Array2;
    CHECK(func.name == "funcDMat4x2Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x2[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        1,
        glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        2,
        glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        4,
        glm::dmat4x2(25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        5,
        glm::dmat4x2(33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dmat4x2, 5> {
            glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8),
            glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16),
            glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24),
            glm::dmat4x2(25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32),
            glm::dmat4x2(33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x2Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDMat4x2Array3;
    CHECK(func.name == "funcDMat4x2Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x2[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        1,
        glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        2,
        glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        4,
        glm::dmat4x2(25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        5,
        glm::dmat4x2(33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        6,
        glm::dmat4x2(41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        7,
        glm::dmat4x2(49.49, 50.50, 51.51, 52.52, 53.53, 54.54, 55.55, 56.56)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        8,
        glm::dmat4x2(57.57, 58.58, 59.59, 60.60, 61.61, 62.62, 63.63, 64.64)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        9,
        glm::dmat4x2(65.65, 66.66, 67.67, 68.68, 69.69, 70.70, 71.71, 72.72)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        10,
        glm::dmat4x2(73.73, 74.74, 75.75, 76.76, 77.77, 78.78, 79.79, 80.80)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dmat4x2, 10> {
            glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8),
            glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16),
            glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24),
            glm::dmat4x2(25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32),
            glm::dmat4x2(33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40),
            glm::dmat4x2(41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48),
            glm::dmat4x2(49.49, 50.50, 51.51, 52.52, 53.53, 54.54, 55.55, 56.56),
            glm::dmat4x2(57.57, 58.58, 59.59, 60.60, 61.61, 62.62, 63.63, 64.64),
            glm::dmat4x2(65.65, 66.66, 67.67, 68.68, 69.69, 70.70, 71.71, 72.72),
            glm::dmat4x2(73.73, 74.74, 75.75, 76.76, 77.77, 78.78, 79.79, 80.80)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);


    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat4x2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDMat4x2;
    CHECK(func.name == "returnDMat4x2");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat4x2 val = ghoul::lua::value<glm::dmat4x2>(state);
    CHECK(val == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat4x2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDMat4x2Map;
    CHECK(func.name == "returnDMat4x2Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat4x2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::dmat4x2> val =
        ghoul::lua::value<std::map<std::string, glm::dmat4x2>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(
        val.find("key1")->second ==
        glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8)
    );
    REQUIRE(val.find("key2") != val.end());
    CHECK(
        val.find("key2")->second ==
        glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)
    );
    REQUIRE(val.find("key3") != val.end());
    CHECK(
        val.find("key3")->second ==
        glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24)
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat4x2Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDMat4x2Optional;
    CHECK(func.name == "returnDMat4x2Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat4x2 val = ghoul::lua::value<glm::dmat4x2>(state);
    CHECK(val == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat4x2Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDMat4x2OptionalNullopt;
    CHECK(func.name == "returnDMat4x2OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat4x2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDMat4x2Vector;
    CHECK(func.name == "returnDMat4x2Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x2[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::dmat4x2> val = ghoul::lua::value<std::vector<glm::dmat4x2>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::dmat4x2(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8));
    CHECK(val[1] == glm::dmat4x2(9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16));
    CHECK(val[2] == glm::dmat4x2(17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24));
    lua_close(state);
}
