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
    [[codegen::luawrap]] void funcMat2x3(glm::mat2x3 arg) {
        CHECK(arg == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcMat2x3Defaulted(glm::mat2x3 arg = glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)) {
        CHECK(arg == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcMat2x3DefaultedCheck(bool isDefaulted, glm::mat2x3 arg = glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)) {
        if (isDefaulted) {
            CHECK(arg == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        }
        else {
            CHECK(arg == glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        }
    }

    [[codegen::luawrap]] void funcMat2x3Map(std::map<std::string, glm::mat2x3> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(
            arg.find("key1")->second ==
            glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)
        );
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(
            arg.find("key2")->second ==
            glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)
        );
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(
            arg.find("key3")->second ==
            glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
        );
    }

    [[codegen::luawrap]] void funcMat2x3Optional(std::optional<glm::mat2x3> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcMat2x3OptionalNullopt(std::optional<glm::mat2x3> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcMat2x3Vector(std::vector<glm::mat2x3> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        CHECK(arg[1] == glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[2] == glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
    }

    [[codegen::luawrap]] void funcMat2x3Array1(std::array<glm::mat2x3, 1> arg) {
        CHECK(arg[0] == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcMat2x3Array2(std::array<glm::mat2x3, 5> arg) {
        CHECK(arg[0] == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        CHECK(arg[1] == glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[2] == glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
        CHECK(arg[3] == glm::mat2x3(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f));
        CHECK(arg[4] == glm::mat2x3(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f));
    }

    [[codegen::luawrap]] void funcMat2x3Array3(std::array<glm::mat2x3, 10> arg) {
        CHECK(arg[0] == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        CHECK(arg[1] == glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[2] == glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
        CHECK(arg[3] == glm::mat2x3(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f));
        CHECK(arg[4] == glm::mat2x3(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f));
        CHECK(arg[5] == glm::mat2x3(31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f));
        CHECK(arg[6] == glm::mat2x3(37.37f, 38.38f, 39.39f, 40.40f, 41.41f, 42.42f));
        CHECK(arg[7] == glm::mat2x3(43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f));
        CHECK(arg[8] == glm::mat2x3(49.49f, 50.50f, 51.51f, 52.52f, 53.53f, 54.54f));
        CHECK(arg[9] == glm::mat2x3(55.55f, 56.56f, 57.57f, 58.58f, 59.59f, 60.60f));
    }

    [[codegen::luawrap]] glm::mat2x3 returnMat2x3() {
        return glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f);
    }

    [[codegen::luawrap]] std::map<std::string, glm::mat2x3> returnMat2x3Map() {
        return {
            { "key1", glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f) },
            { "key2", glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f) },
            { "key3", glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::mat2x3> returnMat2x3Optional() {
        return glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f);
    }

    [[codegen::luawrap]] std::optional<glm::mat2x3> returnMat2x3OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::mat2x3> returnMat2x3Vector() {
        return {
            glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f),
            glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f),
            glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
        };
    }
} // namespace
#include "execution_luawrapper_types_mat2x3_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat2x3;
    CHECK(func.name == "funcMat2x3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat2x3Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat2x3Defaulted;
    CHECK(func.name == "funcMat2x3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat2x3Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat2x3Defaulted;
    CHECK(func.name == "funcMat2x3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)"
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
    "Execution/LuaWrapper/Arguments:  mat2x3DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat2x3DefaultedCheck;
    CHECK(func.name == "funcMat2x3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat2x3?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat2x3DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat2x3DefaultedCheck;
    CHECK(func.name == "funcMat2x3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat2x3?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)"
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

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat2x3Map;
    CHECK(func.name == "funcMat2x3Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> mat2x3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        "key1",
        glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        "key2",
        glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        "key3",
        glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x3Optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat2x3Optional;
    CHECK(func.name == "funcMat2x3Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat2x3OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncMat2x3OptionalNullopt;
    CHECK(func.name == "funcMat2x3OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat2x3Vector;
    CHECK(func.name == "funcMat2x3Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(
        state,
        1,
        glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        2,
        glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x3Array1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat2x3Array1;
    CHECK(func.name == "funcMat2x3Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::mat2x3, 1> { glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f) }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x3Array2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat2x3Array2;
    CHECK(func.name == "funcMat2x3Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        4,
        glm::mat2x3(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        5,
        glm::mat2x3(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::mat2x3, 5> {
            glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f),
            glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f),
            glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f),
            glm::mat2x3(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f),
            glm::mat2x3(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x3Array3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncMat2x3Array3;
    CHECK(func.name == "funcMat2x3Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x3[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    lua::push(
        state,
        3,
        glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        4,
        glm::mat2x3(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        5,
        glm::mat2x3(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        6,
        glm::mat2x3(31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        7,
        glm::mat2x3(37.37f, 38.38f, 39.39f, 40.40f, 41.41f, 42.42f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        8,
        glm::mat2x3(43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        9,
        glm::mat2x3(49.49f, 50.50f, 51.51f, 52.52f, 53.53f, 54.54f)
    );
    lua_rawset(state, -3);
    lua::push(
        state,
        10,
        glm::mat2x3(55.55f, 56.56f, 57.57f, 58.58f, 59.59f, 60.60f)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::mat2x3, 10> {
            glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f),
            glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f),
            glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f),
            glm::mat2x3(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f),
            glm::mat2x3(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f),
            glm::mat2x3(31.31f, 32.32f, 33.33f, 34.34f, 35.35f, 36.36f),
            glm::mat2x3(37.37f, 38.38f, 39.39f, 40.40f, 41.41f, 42.42f),
            glm::mat2x3(43.43f, 44.44f, 45.45f, 46.46f, 47.47f, 48.48f),
            glm::mat2x3(49.49f, 50.50f, 51.51f, 52.52f, 53.53f, 54.54f),
            glm::mat2x3(55.55f, 56.56f, 57.57f, 58.58f, 59.59f, 60.60f)

        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat2x3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnMat2x3;
    CHECK(func.name == "returnMat2x3");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat2x3 val = lua::value<glm::mat2x3>(state);
    CHECK(val == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat2x3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnMat2x3Map;
    CHECK(func.name == "returnMat2x3Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat2x3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::mat2x3> val =
        lua::value<std::map<std::string, glm::mat2x3>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    REQUIRE(val.find("key2") != val.end());
    CHECK(
        val.find("key2")->second == glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)
    );
    REQUIRE(val.find("key3") != val.end());
    CHECK(
        val.find("key3")->second ==
        glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat2x3Optional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnMat2x3Optional;
    CHECK(func.name == "returnMat2x3Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat2x3 val = lua::value<glm::mat2x3>(state);
    CHECK(val == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat2x3Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnMat2x3OptionalNullopt;
    CHECK(func.name == "returnMat2x3OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Return:  mat2x3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnMat2x3Vector;
    CHECK(func.name == "returnMat2x3Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x3[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::mat2x3> val = lua::value<std::vector<glm::mat2x3>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::mat2x3(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    CHECK(val[1] == glm::mat2x3(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
    CHECK(val[2] == glm::mat2x3(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
    lua_close(state);
}
