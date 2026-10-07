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
    [[codegen::luawrap]] void funcDMat4x4(glm::dmat4x4 arg) {
        CHECK(
            arg ==
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x4Defaulted(glm::dmat4x4 arg = glm::dmat4x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)) {
        CHECK(
            arg ==
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x4DefaultedCheck(bool isDefaulted, glm::dmat4x4 arg = glm::dmat4x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)) {
        if (isDefaulted) {
            CHECK(
                arg ==
                glm::dmat4x4(
                    1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
                    9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
                )
            );
        }
        else {
            CHECK(
                arg ==
                glm::dmat4x4(
                    17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
                    25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
                )
            );
        }
    }

    [[codegen::luawrap]] void funcDMat4x4Map(std::map<std::string, glm::dmat4x4> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(
            arg.find("key1")->second ==
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
            )
        );
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(
            arg.find("key2")->second ==
            glm::dmat4x4(
                17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
                25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
            )
        );
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(
            arg.find("key3")->second ==
            glm::dmat4x4(
                33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40,
                41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x4Optional(std::optional<glm::dmat4x4> arg) {
        REQUIRE(arg.has_value());
        CHECK(
            *arg ==
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x4OptionalNullopt(std::optional<glm::dmat4x4> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDMat4x4Vector(std::vector<glm::dmat4x4> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(
            arg[0] ==
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
            )
        );
        CHECK(
            arg[1] ==
            glm::dmat4x4(
                17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
                25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
            )
        );
        CHECK(
            arg[2] ==
            glm::dmat4x4(
                33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40,
                41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48
            )
        );
    }

    [[codegen::luawrap]] void funcDMat4x4Array1(std::array<glm::dmat4x4, 1> arg) {
        CHECK(arg[0] == glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4,
            5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12,
            13.13, 14.14, 15.15, 16.16
        ));
    }

    [[codegen::luawrap]] void funcDMat4x4Array2(std::array<glm::dmat4x4, 5> arg) {
        CHECK(arg[0] == glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4,
            5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12,
            13.13, 14.14, 15.15, 16.16
        ));
        CHECK(arg[1] == glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20,
            21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28,
            29.29, 30.30, 31.31, 32.32
        ));
        CHECK(arg[2] == glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36,
            37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44,
            45.45, 46.46, 47.47, 48.48
        ));
        CHECK(arg[3] == glm::dmat4x4(
            49.49, 50.50, 51.51, 52.52,
            53.53, 54.54, 55.55, 56.56,
            57.57, 58.58, 59.59, 60.60,
            61.61, 62.62, 63.63, 64.64
        ));
        CHECK(arg[4] == glm::dmat4x4(
            65.65, 66.66, 67.67, 68.68,
            69.69, 70.70, 71.71, 72.72,
            73.73, 74.74, 75.75, 76.76,
            77.77, 78.78, 79.79, 80.80
        ));
    }

    [[codegen::luawrap]] void funcDMat4x4Array3(std::array<glm::dmat4x4, 10> arg) {
        CHECK(arg[0] == glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4,
            5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12,
            13.13, 14.14, 15.15, 16.16
        ));
        CHECK(arg[1] == glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20,
            21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28,
            29.29, 30.30, 31.31, 32.32
        ));
        CHECK(arg[2] == glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36,
            37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44,
            45.45, 46.46, 47.47, 48.48
        ));
        CHECK(arg[3] == glm::dmat4x4(
            49.49, 50.50, 51.51, 52.52,
            53.53, 54.54, 55.55, 56.56,
            57.57, 58.58, 59.59, 60.60,
            61.61, 62.62, 63.63, 64.64
        ));
        CHECK(arg[4] == glm::dmat4x4(
            65.65, 66.66, 67.67, 68.68,
            69.69, 70.70, 71.71, 72.72,
            73.73, 74.74, 75.75, 76.76,
            77.77, 78.78, 79.79, 80.80
        ));
        CHECK(arg[5] == glm::dmat4x4(
            81.81, 82.82, 83.83, 84.84,
            85.85, 86.86, 87.87, 88.88,
            89.89, 90.90, 91.91, 92.92,
            93.93, 94.94, 95.95, 96.96
        ));
        CHECK(arg[6] == glm::dmat4x4(
            97.97, 98.98, 99.99, 100.100,
            101.101, 102.102, 103.103, 104.104,
            105.105, 106.106, 107.107, 108.108,
            109.109, 110.110, 111.111, 112.112
        ));
        CHECK(arg[7] == glm::dmat4x4(
            113.113, 114.114, 115.115, 116.116,
            117.117, 118.118, 119.119, 120.120,
            121.121, 122.122, 123.123, 124.124,
            125.125, 126.126, 127.127, 128.128
        ));
        CHECK(arg[8] == glm::dmat4x4(
            129.129, 130.130, 131.131, 132.132,
            133.133, 134.134, 135.135, 136.136,
            137.137, 138.138, 139.139, 140.140,
            141.141, 142.142, 143.143, 144.144
        ));
        CHECK(arg[9] == glm::dmat4x4(
            145.145, 146.146, 147.147, 148.148,
            149.149, 150.150, 151.151, 152.152,
            153.153, 154.154, 155.155, 156.156,
            157.157, 158.158, 159.159, 160.160
        ));
    }

    [[codegen::luawrap]] glm::dmat4x4 returnDMat4x4() {
        return glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        );
    }

    [[codegen::luawrap]] std::map<std::string, glm::dmat4x4> returnDMat4x4Map() {
        return {
            {
                "key1",
                glm::dmat4x4(
                    1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
                    9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
                )
            },
            {
                "key2",
                glm::dmat4x4(
                    17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
                    25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
                )
            },
            {
                "key3",
                glm::dmat4x4(
                    33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40,
                    41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48
                )
            }
        };
    }

    [[codegen::luawrap]] std::optional<glm::dmat4x4> returnDMat4x4Optional() {
        return glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        );
    }

    [[codegen::luawrap]] std::optional<glm::dmat4x4> returnDMat4x4OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::dmat4x4> returnDMat4x4Vector() {
        return {
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
            ),
            glm::dmat4x4(
                17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
                25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
            ),
            glm::dmat4x4(
                33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40,
                41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48
            )
        };
    }
} // namespace
#include "execution_luawrapper_types_dmat4x4_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x4", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4;
    CHECK(func.name == "funcDMat4x4");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x4Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4Defaulted;
    CHECK(func.name == "funcDMat4x4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::dmat4x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, "
        "10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x4Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4Defaulted;
    CHECK(func.name == "funcDMat4x4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::dmat4x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, "
        "10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)"
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
    "Execution/LuaWrapper/Arguments:  dmat4x4DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4DefaultedCheck;
    CHECK(func.name == "funcDMat4x4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x4?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::dmat4x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, "
        "10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        false,
        glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x4DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4DefaultedCheck;
    CHECK(func.name == "funcDMat4x4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x4?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::dmat4x4(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, "
        "10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16)"
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x4Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4Map;
    CHECK(func.name == "funcDMat4x4Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> mat4x4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        "key1",
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        "key2",
        glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        "key3",
        glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x4Optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4Optional;
    CHECK(func.name == "funcDMat4x4Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dmat4x4OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4OptionalNullopt;
    CHECK(func.name == "funcDMat4x4OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x4Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4Vector;
    CHECK(func.name == "funcDMat4x4Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        1,
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x4Array1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4Array1;
    CHECK(func.name == "funcDMat4x4Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        1,
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4,
            5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12,
            13.13, 14.14, 15.15, 16.16
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dmat4x4, 1> {
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4,
                5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12,
                13.13, 14.14, 15.15, 16.16
            )
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x4Array2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4Array2;
    CHECK(func.name == "funcDMat4x4Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        1,
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4,
            5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12,
            13.13, 14.14, 15.15, 16.16
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20,
            21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28,
            29.29, 30.30, 31.31, 32.32
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36,
            37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44,
            45.45, 46.46, 47.47, 48.48
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        4,
        glm::dmat4x4(
            49.49, 50.50, 51.51, 52.52,
            53.53, 54.54, 55.55, 56.56,
            57.57, 58.58, 59.59, 60.60,
            61.61, 62.62, 63.63, 64.64
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        5,
        glm::dmat4x4(
            65.65, 66.66, 67.67, 68.68,
            69.69, 70.70, 71.71, 72.72,
            73.73, 74.74, 75.75, 76.76,
            77.77, 78.78, 79.79, 80.80
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dmat4x4, 5> {
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4,
                5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12,
                13.13, 14.14, 15.15, 16.16
            ),
            glm::dmat4x4(
                17.17, 18.18, 19.19, 20.20,
                21.21, 22.22, 23.23, 24.24,
                25.25, 26.26, 27.27, 28.28,
                29.29, 30.30, 31.31, 32.32
            ),
            glm::dmat4x4(
                33.33, 34.34, 35.35, 36.36,
                37.37, 38.38, 39.39, 40.40,
                41.41, 42.42, 43.43, 44.44,
                45.45, 46.46, 47.47, 48.48
            ),
            glm::dmat4x4(
                49.49, 50.50, 51.51, 52.52,
                53.53, 54.54, 55.55, 56.56,
                57.57, 58.58, 59.59, 60.60,
                61.61, 62.62, 63.63, 64.64
            ),
            glm::dmat4x4(
                65.65, 66.66, 67.67, 68.68,
                69.69, 70.70, 71.71, 72.72,
                73.73, 74.74, 75.75, 76.76,
                77.77, 78.78, 79.79, 80.80
            )
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dmat4x4Array3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncDMat4x4Array3;
    CHECK(func.name == "funcDMat4x4Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        1,
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4,
            5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12,
            13.13, 14.14, 15.15, 16.16
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20,
            21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28,
            29.29, 30.30, 31.31, 32.32
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36,
            37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44,
            45.45, 46.46, 47.47, 48.48
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        4,
        glm::dmat4x4(
            49.49, 50.50, 51.51, 52.52,
            53.53, 54.54, 55.55, 56.56,
            57.57, 58.58, 59.59, 60.60,
            61.61, 62.62, 63.63, 64.64
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        5,
        glm::dmat4x4(
            65.65, 66.66, 67.67, 68.68,
            69.69, 70.70, 71.71, 72.72,
            73.73, 74.74, 75.75, 76.76,
            77.77, 78.78, 79.79, 80.80
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        6,
        glm::dmat4x4(
            81.81, 82.82, 83.83, 84.84,
            85.85, 86.86, 87.87, 88.88,
            89.89, 90.90, 91.91, 92.92,
            93.93, 94.94, 95.95, 96.96
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        7,
        glm::dmat4x4(
            97.97, 98.98, 99.99, 100.100,
            101.101, 102.102, 103.103, 104.104,
            105.105, 106.106, 107.107, 108.108,
            109.109, 110.110, 111.111, 112.112
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        8,
        glm::dmat4x4(
            113.113, 114.114, 115.115, 116.116,
            117.117, 118.118, 119.119, 120.120,
            121.121, 122.122, 123.123, 124.124,
            125.125, 126.126, 127.127, 128.128
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        9,
        glm::dmat4x4(
            129.129, 130.130, 131.131, 132.132,
            133.133, 134.134, 135.135, 136.136,
            137.137, 138.138, 139.139, 140.140,
            141.141, 142.142, 143.143, 144.144
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        10,
        glm::dmat4x4(
            145.145, 146.146, 147.147, 148.148,
            149.149, 150.150, 151.151, 152.152,
            153.153, 154.154, 155.155, 156.156,
            157.157, 158.158, 159.159, 160.160
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::dmat4x4, 10> {
            glm::dmat4x4(
                1.1, 2.2, 3.3, 4.4,
                5.5, 6.6, 7.7, 8.8,
                9.9, 10.10, 11.11, 12.12,
                13.13, 14.14, 15.15, 16.16
            ),
            glm::dmat4x4(
                17.17, 18.18, 19.19, 20.20,
                21.21, 22.22, 23.23, 24.24,
                25.25, 26.26, 27.27, 28.28,
                29.29, 30.30, 31.31, 32.32
            ),
            glm::dmat4x4(
                33.33, 34.34, 35.35, 36.36,
                37.37, 38.38, 39.39, 40.40,
                41.41, 42.42, 43.43, 44.44,
                45.45, 46.46, 47.47, 48.48
            ),
            glm::dmat4x4(
                49.49, 50.50, 51.51, 52.52,
                53.53, 54.54, 55.55, 56.56,
                57.57, 58.58, 59.59, 60.60,
                61.61, 62.62, 63.63, 64.64
            ),
            glm::dmat4x4(
                65.65, 66.66, 67.67, 68.68,
                69.69, 70.70, 71.71, 72.72,
                73.73, 74.74, 75.75, 76.76,
                77.77, 78.78, 79.79, 80.80
            ),
            glm::dmat4x4(
                81.81, 82.82, 83.83, 84.84,
                85.85, 86.86, 87.87, 88.88,
                89.89, 90.90, 91.91, 92.92,
                93.93, 94.94, 95.95, 96.96
            ),
            glm::dmat4x4(
                97.97, 98.98, 99.99, 100.100,
                101.101, 102.102, 103.103, 104.104,
                105.105, 106.106, 107.107, 108.108,
                109.109, 110.110, 111.111, 112.112
            ),
            glm::dmat4x4(
                113.113, 114.114, 115.115, 116.116,
                117.117, 118.118, 119.119, 120.120,
                121.121, 122.122, 123.123, 124.124,
                125.125, 126.126, 127.127, 128.128
            ),
            glm::dmat4x4(
                129.129, 130.130, 131.131, 132.132,
                133.133, 134.134, 135.135, 136.136,
                137.137, 138.138, 139.139, 140.140,
                141.141, 142.142, 143.143, 144.144
            ),
            glm::dmat4x4(
                145.145, 146.146, 147.147, 148.148,
                149.149, 150.150, 151.151, 152.152,
                153.153, 154.154, 155.155, 156.156,
                157.157, 158.158, 159.159, 160.160
            )
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);


    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat4x4", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x4;
    CHECK(func.name == "returnDMat4x4");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat4x4 val = lua::value<glm::dmat4x4>(state);
    CHECK(
        val ==
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        )
    );
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat4x4Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x4Map;
    CHECK(func.name == "returnDMat4x4Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat4x4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::dmat4x4> val =
        lua::value<std::map<std::string, glm::dmat4x4>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(
        val.find("key1")->second ==
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        )
    );
    REQUIRE(val.find("key2") != val.end());
    CHECK(
        val.find("key2")->second ==
        glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
        )
    );
    REQUIRE(val.find("key3") != val.end());
    CHECK(
        val.find("key3")->second ==
        glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48
        )
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat4x4Optional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x4Optional;
    CHECK(func.name == "returnDMat4x4Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x4?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dmat4x4 val = lua::value<glm::dmat4x4>(state);
    CHECK(
        val ==
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        )
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dmat4x4Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x4OptionalNullopt;
    CHECK(func.name == "returnDMat4x4OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x4?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dmat4x4Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnDMat4x4Vector;
    CHECK(func.name == "returnDMat4x4Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x4[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::dmat4x4> val = lua::value<std::vector<glm::dmat4x4>>(state);
    REQUIRE(val.size() == 3);
    CHECK(
        val[0] ==
        glm::dmat4x4(
            1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8,
            9.9, 10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.16
        )
    );
    CHECK(
        val[1] ==
        glm::dmat4x4(
            17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23, 24.24,
            25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32
        )
    );
    CHECK(
        val[2] ==
        glm::dmat4x4(
            33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40,
            41.41, 42.42, 43.43, 44.44, 45.45, 46.46, 47.47, 48.48
        )
    );
    lua_close(state);
}
