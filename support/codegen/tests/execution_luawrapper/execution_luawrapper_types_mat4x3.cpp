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
    [[codegen::luawrap]] void funcMat4x3(glm::mat4x3 arg) {
        CHECK(
            arg ==
            glm::mat4x3(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
                7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x3Defaulted(glm::mat4x3 arg = glm::mat4x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)) {
        CHECK(
            arg ==
            glm::mat4x3(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
                7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x3DefaultedCheck(bool isDefaulted, glm::mat4x3 arg = glm::mat4x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)) {
        if (isDefaulted) {
            CHECK(
                arg ==
                glm::mat4x3(
                    1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
                    7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
                )
            );
        }
        else {
            CHECK(
                arg ==
                glm::mat4x3(
                    13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
                    19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
                )
            );
        }
    }

    [[codegen::luawrap]] void funcMat4x3Map(std::map<std::string, glm::mat4x3> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(
            arg.find("key1")->second ==
            glm::mat4x3(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
                7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
            )
        );
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(
            arg.find("key2")->second ==
            glm::mat4x3(
                13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
                19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
            )
        );
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(
            arg.find("key3")->second ==
            glm::mat4x3(
                25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
                31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x3Optional(std::optional<glm::mat4x3> arg) {
        REQUIRE(arg.has_value());
        CHECK(
            *arg ==
            glm::mat4x3(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
                7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x3OptionalNullopt(std::optional<glm::mat4x3> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcMat4x3Vector(std::vector<glm::mat4x3> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(
            arg[0] ==
            glm::mat4x3(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
                7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
            )
        );
        CHECK(
            arg[1] ==
            glm::mat4x3(
                13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
                19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
            )
        );
        CHECK(
            arg[2] ==
            glm::mat4x3(
                25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
                31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
            )
        );
    }

    [[codegen::luawrap]] void funcMat4x3Array1(std::array<glm::mat4x3, 1> arg) {
        CHECK(arg[0] == glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        ));
    }

    [[codegen::luawrap]] void funcMat4x3Array2(std::array<glm::mat4x3, 5> arg) {
        CHECK(arg[0] == glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        ));
        CHECK(arg[1] == glm::mat4x3(
            13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
            19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
        ));
        CHECK(arg[2] == glm::mat4x3(
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
            31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
        ));
        CHECK(arg[3] == glm::mat4x3(
            37.37f, 38.38f, 39.39f, 40.40f, 41.41f, 42.42f,
            43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
        ));
        CHECK(arg[4] == glm::mat4x3(
            49.49f, 50.50f, 51.51f, 52.52f, 53.53f, 54.54f,
            55.55f, 56.56f, 57.57f, 58.58f, 59.59f, 60.60f
        ));
    }

    [[codegen::luawrap]] void funcMat4x3Array3(std::array<glm::mat4x3, 10> arg) {
        CHECK(arg[0] == glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        ));
        CHECK(arg[1] == glm::mat4x3(
            13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
            19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
        ));
        CHECK(arg[2] == glm::mat4x3(
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
            31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
        ));
        CHECK(arg[3] == glm::mat4x3(
            37.37f, 38.38f, 39.39f, 40.40f, 41.41f, 42.42f,
            43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
        ));
        CHECK(arg[4] == glm::mat4x3(
            49.49f, 50.50f, 51.51f, 52.52f, 53.53f, 54.54f,
            55.55f, 56.56f, 57.57f, 58.58f, 59.59f, 60.60f
        ));
        CHECK(arg[5] == glm::mat4x3(
            61.61f, 62.62f, 63.63f, 64.64f, 65.65f, 66.66f,
            67.67f, 68.68f, 69.69f, 70.70f, 71.71f, 72.72f
        ));
        CHECK(arg[6] == glm::mat4x3(
            73.73f, 74.74f, 75.75f, 76.76f, 77.77f, 78.78f,
            79.79f, 80.80f, 81.81f, 82.82f, 83.83f, 84.84f
        ));
        CHECK(arg[7] == glm::mat4x3(
            85.85f, 86.86f, 87.87f, 88.88f, 89.89f, 90.90f,
            91.91f, 92.92f, 93.93f, 94.94f, 95.95f, 96.96f
        ));
        CHECK(arg[8] == glm::mat4x3(
            97.97f, 98.98f, 99.99f, 100.100f, 101.101f, 102.102f,
            103.103f, 104.104f, 105.105f, 106.106f, 107.107f, 108.108f
        ));
        CHECK(arg[9] == glm::mat4x3(
            109.109f, 110.110f, 111.111f, 112.112f, 113.113f, 114.114f,
            115.115f, 116.116f, 117.117f, 118.118f, 119.119f, 120.120f
        ));
    }

    [[codegen::luawrap]] glm::mat4x3 returnMat4x3() {
        return glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        );
    }

    [[codegen::luawrap]] std::map<std::string, glm::mat4x3> returnMat4x3Map() {
        return {
            {
                "key1",
                glm::mat4x3(
                    1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
                    7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
                )
            },
            {
                "key2",
                glm::mat4x3(
                    13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
                    19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
                )
            },
            {
                "key3",
                glm::mat4x3(
                    25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
                    31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
                )
            }
        };
    }

    [[codegen::luawrap]] std::optional<glm::mat4x3> returnMat4x3Optional() {
        return glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        );
    }

    [[codegen::luawrap]] std::optional<glm::mat4x3> returnMat4x3OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::mat4x3> returnMat4x3Vector() {
        return {
            glm::mat4x3(
                1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
                7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
            ),
            glm::mat4x3(
                13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
                19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
            ),
            glm::mat4x3(
                25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
                31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
            )
        };
    }
} // namespace
#include "execution_luawrapper_types_mat4x3_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat4x3;
    CHECK(func.name == "funcMat4x3");
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
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat4x3Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat4x3Defaulted;
    CHECK(func.name == "funcMat4x3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::mat4x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, "
        "10.10f, 11.11f, 12.12f)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat4x3Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat4x3Defaulted;
    CHECK(func.name == "funcMat4x3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat4x3?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::mat4x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, "
        "10.10f, 11.11f, 12.12f)"
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
    "Execution/LuaWrapper/Arguments:  mat4x3DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat4x3DefaultedCheck;
    CHECK(func.name == "funcMat4x3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x3?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::mat4x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, "
        "10.10f, 11.11f, 12.12f)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(
        state,
        false,
        glm::mat4x3(
            13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
            19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat4x3DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat4x3DefaultedCheck;
    CHECK(func.name == "funcMat4x3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat4x3?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::mat4x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, "
        "10.10f, 11.11f, 12.12f)"
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

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat4x3Map;
    CHECK(func.name == "funcMat4x3Map");
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
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        "key2",
        glm::mat4x3(
            13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
            19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        "key3",
        glm::mat4x3(
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
            31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x3Optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat4x3Optional;
    CHECK(func.name == "funcMat4x3Optional");
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
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat4x3OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat4x3OptionalNullopt;
    CHECK(func.name == "funcMat4x3OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat4x3Vector;
    CHECK(func.name == "funcMat4x3Vector");
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
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::mat4x3(
            13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
            19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::mat4x3(
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
            31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x3Array1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat4x3Array1;
    CHECK(func.name == "funcMat4x3Array1");
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
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x3Array2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat4x3Array2;
    CHECK(func.name == "funcMat4x3Array2");
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
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::mat4x3(
            13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
            19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::mat4x3(
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
            31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        4,
        glm::mat4x3(
            37.37f, 38.38f, 39.39f, 40.40f, 41.41f, 42.42f,
            43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        5,
        glm::mat4x3(
            49.49f, 50.50f, 51.51f, 52.52f, 53.53f, 54.54f,
            55.55f, 56.56f, 57.57f, 58.58f, 59.59f, 60.60f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat4x3Array3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat4x3Array3;
    CHECK(func.name == "funcMat4x3Array3");
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
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::mat4x3(
            13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
            19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::mat4x3(
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
            31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        4,
        glm::mat4x3(
            37.37f, 38.38f, 39.39f, 40.40f, 41.41f, 42.42f,
            43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        5,
        glm::mat4x3(
            49.49f, 50.50f, 51.51f, 52.52f, 53.53f, 54.54f,
            55.55f, 56.56f, 57.57f, 58.58f, 59.59f, 60.60f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        6,
        glm::mat4x3(
            61.61f, 62.62f, 63.63f, 64.64f, 65.65f, 66.66f,
            67.67f, 68.68f, 69.69f, 70.70f, 71.71f, 72.72f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        7,
        glm::mat4x3(
            73.73f, 74.74f, 75.75f, 76.76f, 77.77f, 78.78f,
            79.79f, 80.80f, 81.81f, 82.82f, 83.83f, 84.84f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        8,
        glm::mat4x3(
            85.85f, 86.86f, 87.87f, 88.88f, 89.89f, 90.90f,
            91.91f, 92.92f, 93.93f, 94.94f, 95.95f, 96.96f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        9,
        glm::mat4x3(
            97.97f, 98.98f, 99.99f, 100.100f, 101.101f, 102.102f,
            103.103f, 104.104f, 105.105f, 106.106f, 107.107f, 108.108f
        )
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        10,
        glm::mat4x3(
            109.109f, 110.110f, 111.111f, 112.112f, 113.113f, 114.114f,
            115.115f, 116.116f, 117.117f, 118.118f, 119.119f, 120.120f
        )
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat4x3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnMat4x3;
    CHECK(func.name == "returnMat4x3");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat4x3 val = lua::value<glm::mat4x3>(state);
    CHECK(
        val ==
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat4x3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnMat4x3Map;
    CHECK(func.name == "returnMat4x3Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat4x3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::mat4x3> val =
        lua::value<std::map<std::string, glm::mat4x3>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(
        val.find("key1")->second ==
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    REQUIRE(val.find("key2") != val.end());
    CHECK(
        val.find("key2")->second ==
        glm::mat4x3(
            13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
            19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
        )
    );
    REQUIRE(val.find("key3") != val.end());
    CHECK(
        val.find("key3")->second ==
        glm::mat4x3(
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
            31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
        )
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat4x3Optional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnMat4x3Optional;
    CHECK(func.name == "returnMat4x3Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat4x3 val = lua::value<glm::mat4x3>(state);
    CHECK(
        val ==
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat4x3Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnMat4x3OptionalNullopt;
    CHECK(func.name == "returnMat4x3OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Return:  mat4x3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnMat4x3Vector;
    CHECK(func.name == "returnMat4x3Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat4x3[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::mat4x3> val = lua::value<std::vector<glm::mat4x3>>(state);
    REQUIRE(val.size() == 3);
    CHECK(
        val[0] ==
        glm::mat4x3(
            1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f
        )
    );
    CHECK(
        val[1] ==
        glm::mat4x3(
            13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f,
            19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f
        )
    );
    CHECK(
        val[2] ==
        glm::mat4x3(
            25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f,
            31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f
        )
    );
    lua_close(state);
}
