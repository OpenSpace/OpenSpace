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
    [[codegen::luawrap]] void funcMat4x4(glm::mat4x4 arg) {
        CHECK(
            arg ==
            glm::mat4x4(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x4Defaulted(glm::mat4x4 arg = glm::mat4x4(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f)) {
        CHECK(
            arg ==
            glm::mat4x4(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x4DefaultedCheck(bool isDefaulted, glm::mat4x4 arg = glm::mat4x4(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f)) {
        if (isDefaulted) {
            CHECK(
                arg ==
                glm::mat4x4(
                    1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
                    9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
                )
            );
        }
        else {
            CHECK(
                arg ==
                glm::mat4x4(
                    17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
                    25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
                )
            );
        }
    }

    [[codegen::luawrap]] void funcMat4x4Map(std::map<std::string, glm::mat4x4> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(
            arg.find("key1")->second ==
            glm::mat4x4(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
            )
        );
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(
            arg.find("key2")->second ==
            glm::mat4x4(
                17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
                25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
            )
        );
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(
            arg.find("key3")->second ==
            glm::mat4x4(
                33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38, 39.39f, 40.40f,
                41.41f, 42.42f, 43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x4Optional(std::optional<glm::mat4x4> arg) {
        REQUIRE(arg.has_value());
        CHECK(
            *arg ==
            glm::mat4x4(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x4OptionalNullopt(std::optional<glm::mat4x4> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcMat4x4Vector(std::vector<glm::mat4x4> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(
            arg[0] ==
            glm::mat4x4(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
            )
        );
        CHECK(
            arg[1] ==
            glm::mat4x4(
                17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
                25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
            )
        );
        CHECK(
            arg[2] ==
            glm::mat4x4(
                33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38f, 39.39f, 40.40f,
                41.41f, 42.42f, 43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x4Array1(std::array<glm::mat4x4, 1> arg) {
        CHECK(arg[0] == glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f,
            5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f,
            13.13f, 14.14f, 15.15f, 16.16f
        ));
    }

    [[codegen::luawrap]] void funcMat4x4Array2(std::array<glm::mat4x4, 5> arg) {
        CHECK(arg[0] == glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f,
            5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f,
            13.13f, 14.14f, 15.15f, 16.16f
        ));
        CHECK(arg[1] == glm::mat4x4(
            17.17f, 18.18f, 19.19f, 20.20f,
            21.21f, 22.22f, 23.23f, 24.24f,
            25.25f, 26.26f, 27.27f, 28.28f,
            29.29f, 30.30f, 31.31f, 32.32f
        ));
        CHECK(arg[2] == glm::mat4x4(
            33.33f, 34.34f, 35.35f, 36.36f,
            37.37f, 38.38f, 39.39f, 40.40f,
            41.41f, 42.42f, 43.43f, 44.44f,
            45.45f, 46.46f, 47.47f, 48.48f
        ));
        CHECK(arg[3] == glm::mat4x4(
            49.49f, 50.50f, 51.51f, 52.52f,
            53.53f, 54.54f, 55.55f, 56.56f,
            57.57f, 58.58f, 59.59f, 60.60f,
            61.61f, 62.62f, 63.63f, 64.64f
        ));
        CHECK(arg[4] == glm::mat4x4(
            65.65f, 66.66f, 67.67f, 68.68f,
            69.69f, 70.70f, 71.71f, 72.72f,
            73.73f, 74.74f, 75.75f, 76.76f,
            77.77f, 78.78f, 79.79f, 80.80f
        ));
    }

    [[codegen::luawrap]] void funcMat4x4Array3(std::array<glm::mat4x4, 10> arg) {
        CHECK(arg[0] == glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f,
            5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f,
            13.13f, 14.14f, 15.15f, 16.16f
        ));
        CHECK(arg[1] == glm::mat4x4(
            17.17f, 18.18f, 19.19f, 20.20f,
            21.21f, 22.22f, 23.23f, 24.24f,
            25.25f, 26.26f, 27.27f, 28.28f,
            29.29f, 30.30f, 31.31f, 32.32f
        ));
        CHECK(arg[2] == glm::mat4x4(
            33.33f, 34.34f, 35.35f, 36.36f,
            37.37f, 38.38f, 39.39f, 40.40f,
            41.41f, 42.42f, 43.43f, 44.44f,
            45.45f, 46.46f, 47.47f, 48.48f
        ));
        CHECK(arg[3] == glm::mat4x4(
            49.49f, 50.50f, 51.51f, 52.52f,
            53.53f, 54.54f, 55.55f, 56.56f,
            57.57f, 58.58f, 59.59f, 60.60f,
            61.61f, 62.62f, 63.63f, 64.64f
        ));
        CHECK(arg[4] == glm::mat4x4(
            65.65f, 66.66f, 67.67f, 68.68f,
            69.69f, 70.70f, 71.71f, 72.72f,
            73.73f, 74.74f, 75.75f, 76.76f,
            77.77f, 78.78f, 79.79f, 80.80f
        ));
        CHECK(arg[5] == glm::mat4x4(
            81.81f, 82.82f, 83.83f, 84.84f,
            85.85f, 86.86f, 87.87f, 88.88f,
            89.89f, 90.90f, 91.91f, 92.92f,
            93.93f, 94.94f, 95.95f, 96.96f
        ));
        CHECK(arg[6] == glm::mat4x4(
            97.97f, 98.98f, 99.99f, 100.100f,
            101.101f, 102.102f, 103.103f, 104.104f,
            105.105f, 106.106f, 107.107f, 108.108f,
            109.109f, 110.110f, 111.111f, 112.112f
        ));
        CHECK(arg[7] == glm::mat4x4(
            113.113f, 114.114f, 115.115f, 116.116f,
            117.117f, 118.118f, 119.119f, 120.120f,
            121.121f, 122.122f, 123.123f, 124.124f,
            125.125f, 126.126f, 127.127f, 128.128f
        ));
        CHECK(arg[8] == glm::mat4x4(
            129.129f, 130.130f, 131.131f, 132.132f,
            133.133f, 134.134f, 135.135f, 136.136f,
            137.137f, 138.138f, 139.139f, 140.140f,
            141.141f, 142.142f, 143.143f, 144.144f
        ));
        CHECK(arg[9] == glm::mat4x4(
            145.145f, 146.146f, 147.147f, 148.148f,
            149.149f, 150.150f, 151.151f, 152.152f,
            153.153f, 154.154f, 155.155f, 156.156f,
            157.157f, 158.158f, 159.159f, 160.160f
        ));
    }

    [[codegen::luawrap]] glm::mat4x4 returnMat4x4() {
        return glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        );
    }

    [[codegen::luawrap]] std::map<std::string, glm::mat4x4> returnMat4x4Map() {
        return {
            {
                "key1",
                glm::mat4x4(
                    1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
                    9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
                )
            },
            {
                "key2",
                glm::mat4x4(
                    17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
                    25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
                )
            },
            {
                "key3",
                glm::mat4x4(
                    33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38f, 39.39f, 40.40f,
                    41.41f, 42.42f, 43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
                )
            }
        };
    }

    [[codegen::luawrap]] std::optional<glm::mat4x4> returnMat4x4Optional() {
        return glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        );
    }

    [[codegen::luawrap]] std::optional<glm::mat4x4> returnMat4x4OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::mat4x4> returnMat4x4Vector() {
        return {
            glm::mat4x4(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
                9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
            ),
            glm::mat4x4(
                17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
                25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
            ),
            glm::mat4x4(
                33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38f, 39.39f, 40.40f,
                41.41f, 42.42f, 43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
            )
        };
    }
} // namespace
#include "execution_luawrapper_types_mat4x4_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x4", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat4x4;
    CHECK(func.name == "funcMat4x4");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(
        state,
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat4x4Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat4x4Defaulted;
    CHECK(func.name == "funcMat4x4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::mat4x4(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, "
        "10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(
        state,
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat4x4Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat4x4Defaulted;
    CHECK(func.name == "funcMat4x4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::mat4x4(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, "
        "10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f)"
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
    "Execution/LuaWrapper/Arguments:  mat4x4DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat4x4DefaultedCheck;
    CHECK(func.name == "funcMat4x4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x4?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::mat4x4(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, "
        "10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(
        state,
        false,
        glm::mat4x4(
            17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat4x4DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat4x4DefaultedCheck;
    CHECK(func.name == "funcMat4x4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x4?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::mat4x4(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, "
        "10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f)"
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

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x4Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat4x4Map;
    CHECK(func.name == "funcMat4x4Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> mat4x4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        "key1",
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        "key2",
        glm::mat4x4(
            17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        "key3",
        glm::mat4x4(
            33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38, 39.39f, 40.40f,
            41.41f, 42.42f, 43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x4Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat4x4Optional;
    CHECK(func.name == "funcMat4x4Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(
        state,
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat4x4OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat4x4OptionalNullopt;
    CHECK(func.name == "funcMat4x4OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x4Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat4x4Vector;
    CHECK(func.name == "funcMat4x4Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        1,
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        2,
        glm::mat4x4(
            17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::mat4x4(
            33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38f, 39.39f, 40.40f,
            41.41f, 42.42f, 43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x4Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat4x4Array1;
    CHECK(func.name == "funcMat4x4Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        1,
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f,
            5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f,
            13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x4Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat4x4Array2;
    CHECK(func.name == "funcMat4x4Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        1,
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f,
            5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f,
            13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        2,
        glm::mat4x4(
            17.17f, 18.18f, 19.19f, 20.20f,
            21.21f, 22.22f, 23.23f, 24.24f,
            25.25f, 26.26f, 27.27f, 28.28f,
            29.29f, 30.30f, 31.31f, 32.32f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::mat4x4(
            33.33f, 34.34f, 35.35f, 36.36f,
            37.37f, 38.38f, 39.39f, 40.40f,
            41.41f, 42.42f, 43.43f, 44.44f,
            45.45f, 46.46f, 47.47f, 48.48f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        4,
        glm::mat4x4(
            49.49f, 50.50f, 51.51f, 52.52f,
            53.53f, 54.54f, 55.55f, 56.56f,
            57.57f, 58.58f, 59.59f, 60.60f,
            61.61f, 62.62f, 63.63f, 64.64f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        5,
        glm::mat4x4(
            65.65f, 66.66f, 67.67f, 68.68f,
            69.69f, 70.70f, 71.71f, 72.72f,
            73.73f, 74.74f, 75.75f, 76.76f,
            77.77f, 78.78f, 79.79f, 80.80f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x4Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat4x4Array3;
    CHECK(func.name == "funcMat4x4Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x4[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        1,
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f,
            5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f,
            13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        2,
        glm::mat4x4(
            17.17f, 18.18f, 19.19f, 20.20f,
            21.21f, 22.22f, 23.23f, 24.24f,
            25.25f, 26.26f, 27.27f, 28.28f,
            29.29f, 30.30f, 31.31f, 32.32f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::mat4x4(
            33.33f, 34.34f, 35.35f, 36.36f,
            37.37f, 38.38f, 39.39f, 40.40f,
            41.41f, 42.42f, 43.43f, 44.44f,
            45.45f, 46.46f, 47.47f, 48.48f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        4,
        glm::mat4x4(
            49.49f, 50.50f, 51.51f, 52.52f,
            53.53f, 54.54f, 55.55f, 56.56f,
            57.57f, 58.58f, 59.59f, 60.60f,
            61.61f, 62.62f, 63.63f, 64.64f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        5,
        glm::mat4x4(
            65.65f, 66.66f, 67.67f, 68.68f,
            69.69f, 70.70f, 71.71f, 72.72f,
            73.73f, 74.74f, 75.75f, 76.76f,
            77.77f, 78.78f, 79.79f, 80.80f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        6,
        glm::mat4x4(
            81.81f, 82.82f, 83.83f, 84.84f,
            85.85f, 86.86f, 87.87f, 88.88f,
            89.89f, 90.90f, 91.91f, 92.92f,
            93.93f, 94.94f, 95.95f, 96.96f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        7,
        glm::mat4x4(
            97.97f, 98.98f, 99.99f, 100.100f,
            101.101f, 102.102f, 103.103f, 104.104f,
            105.105f, 106.106f, 107.107f, 108.108f,
            109.109f, 110.110f, 111.111f, 112.112f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        8,
        glm::mat4x4(
            113.113f, 114.114f, 115.115f, 116.116f,
            117.117f, 118.118f, 119.119f, 120.120f,
            121.121f, 122.122f, 123.123f, 124.124f,
            125.125f, 126.126f, 127.127f, 128.128f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        9,
        glm::mat4x4(
            129.129f, 130.130f, 131.131f, 132.132f,
            133.133f, 134.134f, 135.135f, 136.136f,
            137.137f, 138.138f, 139.139f, 140.140f,
            141.141f, 142.142f, 143.143f, 144.144f
        )
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        10,
        glm::mat4x4(
            145.145f, 146.146f, 147.147f, 148.148f,
            149.149f, 150.150f, 151.151f, 152.152f,
            153.153f, 154.154f, 155.155f, 156.156f,
            157.157f, 158.158f, 159.159f, 160.160f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat4x4", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnMat4x4;
    CHECK(func.name == "returnMat4x4");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat4x4 val = ghoul::lua::value<glm::mat4x4>(state);
    CHECK(
        val ==
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat4x4Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnMat4x4Map;
    CHECK(func.name == "returnMat4x4Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat4x4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::mat4x4> val =
        ghoul::lua::value<std::map<std::string, glm::mat4x4>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(
        val.find("key1")->second ==
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    REQUIRE(val.find("key2") != val.end());
    CHECK(
        val.find("key2")->second ==
        glm::mat4x4(
            17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
        )
    );
    REQUIRE(val.find("key3") != val.end());
    CHECK(
        val.find("key3")->second ==
        glm::mat4x4(
            33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38, 39.39f, 40.40f,
            41.41f, 42.42f, 43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
        )
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat4x4Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnMat4x4Optional;
    CHECK(func.name == "returnMat4x4Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x4?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat4x4 val = ghoul::lua::value<glm::mat4x4>(state);
    CHECK(
        val ==
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat4x4Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnMat4x4OptionalNullopt;
    CHECK(func.name == "returnMat4x4OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Return:  mat4x4Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnMat4x4Vector;
    CHECK(func.name == "returnMat4x4Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x4[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::mat4x4> val = ghoul::lua::value<std::vector<glm::mat4x4>>(state);
    REQUIRE(val.size() == 3);
    CHECK(
        val[0] ==
        glm::mat4x4(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f,
            9.9f, 10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f, 16.16f
        )
    );
    CHECK(
        val[1] ==
        glm::mat4x4(
            17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f,
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f
        )
    );
    CHECK(
        val[2] ==
        glm::mat4x4(
            33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38f, 39.39f, 40.40f,
            41.41f, 42.42f, 43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
        )
    );
    lua_close(state);
}
