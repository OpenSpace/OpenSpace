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
    [[codegen::luawrap]] void funcDMat4x3(glm::dmat4x3 arg) {
        CHECK(
            arg ==
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
                7.7, 8.8, 9.9, 10.10, 11.11, 12.12
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x3Defaulted(glm::dmat4x3 arg = glm::dmat4x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12)) {
        CHECK(
            arg ==
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
                7.7, 8.8, 9.9, 10.10, 11.11, 12.12
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x3DefaultedCheck(bool isDefaulted, glm::dmat4x3 arg = glm::dmat4x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12)) {
        if (isDefaulted) {
            CHECK(
                arg ==
                glm::dmat4x3(
                    1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
                    7.7, 8.8, 9.9, 10.10, 11.11, 12.12
                )
            );
        }
        else {
            CHECK(
                arg ==
                glm::dmat4x3(
                    13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
                    19.19, 20.20, 21.21, 22.22, 23.23, 24.24
                )
            );
        }
    }

    [[codegen::luawrap]] void funcDMat4x3Map(std::map<std::string, glm::dmat4x3> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(
            arg.find("key1")->second ==
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
                7.7, 8.8, 9.9, 10.10, 11.11, 12.12
            )
        );
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(
            arg.find("key2")->second ==
            glm::dmat4x3(
                13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
                19.19, 20.20, 21.21, 22.22, 23.23, 24.24
            )
        );
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(
            arg.find("key3")->second ==
            glm::dmat4x3(
                25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
                31.31, 32.32, 33.33, 34.34, 35.35, 36.36
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x3Optional(std::optional<glm::dmat4x3> arg) {
        REQUIRE(arg.has_value());
        CHECK(
            *arg ==
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
                7.7, 8.8, 9.9, 10.10, 11.11, 12.12
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x3OptionalNullopt(std::optional<glm::dmat4x3> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDMat4x3Vector(std::vector<glm::dmat4x3> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(
            arg[0] ==
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
                7.7, 8.8, 9.9, 10.10, 11.11, 12.12
            )
        );
        CHECK(
            arg[1] ==
            glm::dmat4x3(
                13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
                19.19, 20.20, 21.21, 22.22, 23.23, 24.24
            )
        );
        CHECK(
            arg[2] ==
            glm::dmat4x3(
                25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
                31.31, 32.32, 33.33, 34.34, 35.35, 36.36
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x3Array1(std::array<glm::dmat4x3, 1> arg) {
        CHECK(arg[0] == glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        ));
    }

    [[codegen::luawrap]] void funcDMat4x3Array2(std::array<glm::dmat4x3, 5> arg) {
        CHECK(arg[0] == glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        ));
        CHECK(arg[1] == glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
            19.19, 20.20, 21.21, 22.22, 23.23, 24.24
        ));
        CHECK(arg[2] == glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
            31.31, 32.32, 33.33, 34.34, 35.35, 36.36
        ));
        CHECK(arg[3] == glm::dmat4x3(
            37.37, 38.38, 39.39, 40.40, 41.41, 42.42,
            43.43, 44.44, 45.45, 46.46, 47.47, 48.48
        ));
        CHECK(arg[4] == glm::dmat4x3(
            49.49, 50.50, 51.51, 52.52, 53.53, 54.54,
            55.55, 56.56, 57.57, 58.58, 59.59, 60.60
        ));
    }

    [[codegen::luawrap]] void funcDMat4x3Array3(std::array<glm::dmat4x3, 10> arg) {
        CHECK(arg[0] == glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        ));
        CHECK(arg[1] == glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
            19.19, 20.20, 21.21, 22.22, 23.23, 24.24
        ));
        CHECK(arg[2] == glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
            31.31, 32.32, 33.33, 34.34, 35.35, 36.36
        ));
        CHECK(arg[3] == glm::dmat4x3(
            37.37, 38.38, 39.39, 40.40, 41.41, 42.42,
            43.43, 44.44, 45.45, 46.46, 47.47, 48.48
        ));
        CHECK(arg[4] == glm::dmat4x3(
            49.49, 50.50, 51.51, 52.52, 53.53, 54.54,
            55.55, 56.56, 57.57, 58.58, 59.59, 60.60
        ));
        CHECK(arg[5] == glm::dmat4x3(
            61.61, 62.62, 63.63, 64.64, 65.65, 66.66,
            67.67, 68.68, 69.69, 70.70, 71.71, 72.72
        ));
        CHECK(arg[6] == glm::dmat4x3(
            73.73, 74.74, 75.75, 76.76, 77.77, 78.78,
            79.79, 80.80, 81.81, 82.82, 83.83, 84.84
        ));
        CHECK(arg[7] == glm::dmat4x3(
            85.85, 86.86, 87.87, 88.88, 89.89, 90.90,
            91.91, 92.92, 93.93, 94.94, 95.95, 96.96
        ));
        CHECK(arg[8] == glm::dmat4x3(
            97.97, 98.98, 99.99, 100.100, 101.101, 102.102,
            103.103, 104.104, 105.105, 106.106, 107.107, 108.108
        ));
        CHECK(arg[9] == glm::dmat4x3(
            109.109, 110.110, 111.111, 112.112, 113.113, 114.114,
            115.115, 116.116, 117.117, 118.118, 119.119, 120.120
        ));
    }

    [[codegen::luawrap]] glm::dmat4x3 returnDMat4x3() {
        return glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        );
    }

    [[codegen::luawrap]] std::map<std::string, glm::dmat4x3> returnDMat4x3Map() {
        return {
            {
                "key1",
                glm::dmat4x3(
                    1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
                    7.7, 8.8, 9.9, 10.10, 11.11, 12.12
                )
            },
            {
                "key2",
                glm::dmat4x3(
                    13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
                    19.19, 20.20, 21.21, 22.22, 23.23, 24.24
                )
            },
            {
                "key3",
                glm::dmat4x3(
                    25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
                    31.31, 32.32, 33.33, 34.34, 35.35, 36.36
                )
            }
        };
    }

    [[codegen::luawrap]] std::optional<glm::dmat4x3> returnDMat4x3Optional() {
        return glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        );
    }

    [[codegen::luawrap]] std::optional<glm::dmat4x3> returnDMat4x3OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::dmat4x3> returnDMat4x3Vector() {
        return {
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
                7.7, 8.8, 9.9, 10.10, 11.11, 12.12
            ),
            glm::dmat4x3(
                13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
                19.19, 20.20, 21.21, 22.22, 23.23, 24.24
            ),
            glm::dmat4x3(
                25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
                31.31, 32.32, 33.33, 34.34, 35.35, 36.36
            )
        };
    }
} // namespace
#include "execution_luawrapper_types_dmat4x3_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3;
    CHECK(func.name == "funcDMat4x3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
            7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x3Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3Defaulted;
    CHECK(func.name == "funcDMat4x3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::dmat4x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, "
        "10.10, 11.11, 12.12)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
            7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x3Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3Defaulted;
    CHECK(func.name == "funcDMat4x3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::dmat4x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, "
        "10.10, 11.11, 12.12)"
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
    "Execution/LuaWrapper/Arguments:  dmat4x3DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3DefaultedCheck;
    CHECK(func.name == "funcDMat4x3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x3?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::dmat4x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, "
        "10.10, 11.11, 12.12)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        false,
        glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
            19.19, 20.20, 21.21, 22.22, 23.23, 24.24
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x3DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3DefaultedCheck;
    CHECK(func.name == "funcDMat4x3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x3?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::dmat4x3(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, "
        "10.10, 11.11, 12.12)"
    );
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3Map;
    CHECK(func.name == "funcDMat4x3Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> mat4x3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        "key1",
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
            7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        "key2",
        glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
            19.19, 20.20, 21.21, 22.22, 23.23, 24.24
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        "key3",
        glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
            31.31, 32.32, 33.33, 34.34, 35.35, 36.36
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x3Optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3Optional;
    CHECK(func.name == "funcDMat4x3Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
            7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x3OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3OptionalNullopt;
    CHECK(func.name == "funcDMat4x3OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3Vector;
    CHECK(func.name == "funcDMat4x3Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        1,
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
            7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
            19.19, 20.20, 21.21, 22.22, 23.23, 24.24
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
            31.31, 32.32, 33.33, 34.34, 35.35, 36.36
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x3Array1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3Array1;
    CHECK(func.name == "funcDMat4x3Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        1,
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dmat4x3, 1> {
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
            )
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x3Array2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3Array2;
    CHECK(func.name == "funcDMat4x3Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        1,
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
            19.19, 20.20, 21.21, 22.22, 23.23, 24.24
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
            31.31, 32.32, 33.33, 34.34, 35.35, 36.36
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        4,
        glm::dmat4x3(
            37.37, 38.38, 39.39, 40.40, 41.41, 42.42,
            43.43, 44.44, 45.45, 46.46, 47.47, 48.48
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        5,
        glm::dmat4x3(
            49.49, 50.50, 51.51, 52.52, 53.53, 54.54,
            55.55, 56.56, 57.57, 58.58, 59.59, 60.60
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dmat4x3, 5> {
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
            ),
            glm::dmat4x3(
                13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
                19.19, 20.20, 21.21, 22.22, 23.23, 24.24
            ),
            glm::dmat4x3(
                25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
                31.31, 32.32, 33.33, 34.34, 35.35, 36.36
            ),
            glm::dmat4x3(
                37.37, 38.38, 39.39, 40.40, 41.41, 42.42,
                43.43, 44.44, 45.45, 46.46, 47.47, 48.48
            ),
            glm::dmat4x3(
                49.49, 50.50, 51.51, 52.52, 53.53, 54.54,
                55.55, 56.56, 57.57, 58.58, 59.59, 60.60
            )
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x3Array3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x3Array3;
    CHECK(func.name == "funcDMat4x3Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        1,
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
            19.19, 20.20, 21.21, 22.22, 23.23, 24.24
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
            31.31, 32.32, 33.33, 34.34, 35.35, 36.36
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        4,
        glm::dmat4x3(
            37.37, 38.38, 39.39, 40.40, 41.41, 42.42,
            43.43, 44.44, 45.45, 46.46, 47.47, 48.48
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        5,
        glm::dmat4x3(
            49.49, 50.50, 51.51, 52.52, 53.53, 54.54,
            55.55, 56.56, 57.57, 58.58, 59.59, 60.60
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        6,
        glm::dmat4x3(
            61.61, 62.62, 63.63, 64.64, 65.65, 66.66,
            67.67, 68.68, 69.69, 70.70, 71.71, 72.72
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        7,
        glm::dmat4x3(
            73.73, 74.74, 75.75, 76.76, 77.77, 78.78,
            79.79, 80.80, 81.81, 82.82, 83.83, 84.84
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        8,
        glm::dmat4x3(
            85.85, 86.86, 87.87, 88.88, 89.89, 90.90,
            91.91, 92.92, 93.93, 94.94, 95.95, 96.96
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        9,
        glm::dmat4x3(
            97.97, 98.98, 99.99, 100.100, 101.101, 102.102,
            103.103, 104.104, 105.105, 106.106, 107.107, 108.108
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        10,
        glm::dmat4x3(
            109.109, 110.110, 111.111, 112.112, 113.113, 114.114,
            115.115, 116.116, 117.117, 118.118, 119.119, 120.120
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dmat4x3, 10> {
            glm::dmat4x3(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12
            ),
            glm::dmat4x3(
                13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
                19.19, 20.20, 21.21, 22.22, 23.23, 24.24
            ),
            glm::dmat4x3(
                25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
                31.31, 32.32, 33.33, 34.34, 35.35, 36.36
            ),
            glm::dmat4x3(
                37.37, 38.38, 39.39, 40.40, 41.41, 42.42,
                43.43, 44.44, 45.45, 46.46, 47.47, 48.48
            ),
            glm::dmat4x3(
                49.49, 50.50, 51.51, 52.52, 53.53, 54.54,
                55.55, 56.56, 57.57, 58.58, 59.59, 60.60
            ),
            glm::dmat4x3(
                61.61, 62.62, 63.63, 64.64, 65.65, 66.66,
                67.67, 68.68, 69.69, 70.70, 71.71, 72.72
            ),
            glm::dmat4x3(
                73.73, 74.74, 75.75, 76.76, 77.77, 78.78,
                79.79, 80.80, 81.81, 82.82, 83.83, 84.84
            ),
            glm::dmat4x3(
                85.85, 86.86, 87.87, 88.88, 89.89, 90.90,
                91.91, 92.92, 93.93, 94.94, 95.95, 96.96
            ),
            glm::dmat4x3(
                97.97, 98.98, 99.99, 100.100, 101.101, 102.102,
                103.103, 104.104, 105.105, 106.106, 107.107, 108.108
            ),
            glm::dmat4x3(
                109.109, 110.110, 111.111, 112.112, 113.113, 114.114,
                115.115, 116.116, 117.117, 118.118, 119.119, 120.120
            )
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);


    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat4x3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x3;
    CHECK(func.name == "returnDMat4x3");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat4x3 val = lua::value<glm::dmat4x3>(state);
    CHECK(
        val ==
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
            7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat4x3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x3Map;
    CHECK(func.name == "returnDMat4x3Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat4x3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::dmat4x3> val =
        lua::value<std::map<std::string, glm::dmat4x3>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(
        val.find("key1")->second ==
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
            7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    REQUIRE(val.find("key2") != val.end());
    CHECK(
        val.find("key2")->second ==
        glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
            19.19, 20.20, 21.21, 22.22, 23.23, 24.24
        )
    );
    REQUIRE(val.find("key3") != val.end());
    CHECK(
        val.find("key3")->second ==
        glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
            31.31, 32.32, 33.33, 34.34, 35.35, 36.36
        )
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat4x3Optional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x3Optional;
    CHECK(func.name == "returnDMat4x3Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat4x3 val = lua::value<glm::dmat4x3>(state);
    CHECK(
        val ==
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
            7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat4x3Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x3OptionalNullopt;
    CHECK(func.name == "returnDMat4x3OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat4x3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x3Vector;
    CHECK(func.name == "returnDMat4x3Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x3[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::dmat4x3> val = lua::value<std::vector<glm::dmat4x3>>(state);
    REQUIRE(val.size() == 3);
    CHECK(
        val[0] ==
        glm::dmat4x3(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6,
            7.7, 8.8, 9.9, 10.10, 11.11, 12.12
        )
    );
    CHECK(
        val[1] ==
        glm::dmat4x3(
            13.13, 14.14, 15.15, 16.16, 17.17, 18.18,
            19.19, 20.20, 21.21, 22.22, 23.23, 24.24
        )
    );
    CHECK(
        val[2] ==
        glm::dmat4x3(
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30,
            31.31, 32.32, 33.33, 34.34, 35.35, 36.36
        )
    );
    lua_close(state);
}
