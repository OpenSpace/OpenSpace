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
    [[codegen::luawrap]] void funcMat3x2(glm::mat3x2 arg) {
        CHECK(arg == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcMat3x2Defaulted(glm::mat3x2 arg = glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)) {
        CHECK(arg == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcMat3x2DefaultedCheck(bool isDefaulted, glm::mat3x2 arg = glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)) {
        if (isDefaulted) {
            CHECK(arg == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        }
        else {
            CHECK(arg == glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        }
    }

    [[codegen::luawrap]] void funcMat3x2Map(std::map<std::string, glm::mat3x2> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(
            arg.find("key1")->second ==
            glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)
        );
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(
            arg.find("key2")->second ==
            glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)
        );
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(
            arg.find("key3")->second ==
            glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
        );
    }

    [[codegen::luawrap]] void funcMat3x2Optional(std::optional<glm::mat3x2> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcMat3x2OptionalNullopt(std::optional<glm::mat3x2> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcMat3x2Vector(std::vector<glm::mat3x2> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        CHECK(arg[1] == glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[2] == glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
    }

    [[codegen::luawrap]] void funcMat3x2Array1(std::array<glm::mat3x2, 1> arg) {
        CHECK(arg[0] == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    }

    [[codegen::luawrap]] void funcMat3x2Array2(std::array<glm::mat3x2, 5> arg) {
        CHECK(arg[0] == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        CHECK(arg[1] == glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[2] == glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
        CHECK(arg[3] == glm::mat3x2(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f));
        CHECK(arg[4] == glm::mat3x2(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f));
    }

    [[codegen::luawrap]] void funcMat3x2Array3(std::array<glm::mat3x2, 10> arg) {
        CHECK(arg[0] == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
        CHECK(arg[1] == glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[2] == glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
        CHECK(arg[3] == glm::mat3x2(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f));
        CHECK(arg[4] == glm::mat3x2(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f));
        CHECK(arg[5] == glm::mat3x2(21.21f, 22.22f, 23.23f, 24.24f, 25.25f, 26.26f));
        CHECK(arg[6] == glm::mat3x2(27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f));
        CHECK(arg[7] == glm::mat3x2(33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38f));
        CHECK(arg[8] == glm::mat3x2(39.39f, 40.40f, 41.41f, 42.42f, 43.43f, 44.44f));
        CHECK(arg[9] == glm::mat3x2(45.45f, 46.46f, 47.47f, 48.48f, 49.49f, 50.50f));
    }

    [[codegen::luawrap]] glm::mat3x2 returnMat3x2() {
        return glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f);
    }

    [[codegen::luawrap]] std::map<std::string, glm::mat3x2> returnMat3x2Map() {
        return {
            { "key1", glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f) },
            { "key2", glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f) },
            { "key3", glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::mat3x2> returnMat3x2Optional() {
        return glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f);
    }

    [[codegen::luawrap]] std::optional<glm::mat3x2> returnMat3x2OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::mat3x2> returnMat3x2Vector() {
        return {
            glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f),
            glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f),
            glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
        };
    }
} // namespace
#include "execution_luawrapper_types_mat3x2_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  mat3x2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat3x2;
    CHECK(func.name == "funcMat3x2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat3x2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat3x2Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat3x2Defaulted;
    CHECK(func.name == "funcMat3x2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat3x2?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat3x2Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat3x2Defaulted;
    CHECK(func.name == "funcMat3x2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat3x2?");
    CHECK(
        func.arguments[0].defaultValue ==
        "glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)"
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
    "Execution/LuaWrapper/Arguments:  mat3x2DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat3x2DefaultedCheck;
    CHECK(func.name == "funcMat3x2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat3x2?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)"
    );
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat3x2DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat3x2DefaultedCheck;
    CHECK(func.name == "funcMat3x2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat3x2?");
    CHECK(
        func.arguments[1].defaultValue ==
        "glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)"
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

TEST_CASE("Execution/LuaWrapper/Arguments:  mat3x2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat3x2Map;
    CHECK(func.name == "funcMat3x2Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> mat3x2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        "key1",
        glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        "key2",
        glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        "key3",
        glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat3x2Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat3x2Optional;
    CHECK(func.name == "funcMat3x2Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat3x2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat3x2OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat3x2OptionalNullopt;
    CHECK(func.name == "funcMat3x2OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat3x2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat3x2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat3x2Vector;
    CHECK(func.name == "funcMat3x2Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat3x2[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(
        state,
        1,
        glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        2,
        glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat3x2Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat3x2Array1;
    CHECK(func.name == "funcMat3x2Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat3x2[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat3x2Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat3x2Array2;
    CHECK(func.name == "funcMat3x2Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat3x2[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        4,
        glm::mat3x2(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        5,
        glm::mat3x2(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat3x2Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat3x2Array3;
    CHECK(func.name == "funcMat3x2Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat3x2[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        3,
        glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        4,
        glm::mat3x2(19.19f, 20.20f, 21.21f, 22.22f, 23.23f, 24.24f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        5,
        glm::mat3x2(25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        6,
        glm::mat3x2(21.21f, 22.22f, 23.23f, 24.24f, 25.25f, 26.26f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        7,
        glm::mat3x2(27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        8,
        glm::mat3x2(33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        9,
        glm::mat3x2(39.39f, 40.40f, 41.41f, 42.42f, 43.43f, 44.44f)
    );
    lua_rawset(state, -3);
    ghoul::lua::push(
        state,
        10,
        glm::mat3x2(45.45f, 46.46f, 47.47f, 48.48f, 49.49f, 50.50f)
    );
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat3x2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnMat3x2;
    CHECK(func.name == "returnMat3x2");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat3x2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat3x2 val = ghoul::lua::value<glm::mat3x2>(state);
    CHECK(val == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat3x2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnMat3x2Map;
    CHECK(func.name == "returnMat3x2Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat3x2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::mat3x2> val =
        ghoul::lua::value<std::map<std::string, glm::mat3x2>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    REQUIRE(val.find("key2") != val.end());
    CHECK(
        val.find("key2")->second ==
        glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f)
    );
    REQUIRE(val.find("key3") != val.end());
    CHECK(
        val.find("key3")->second ==
        glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f)
    );
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat3x2Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnMat3x2Optional;
    CHECK(func.name == "returnMat3x2Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat3x2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat3x2 val = ghoul::lua::value<glm::mat3x2>(state);
    CHECK(val == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat3x2Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnMat3x2OptionalNullopt;
    CHECK(func.name == "returnMat3x2OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat3x2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat3x2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnMat3x2Vector;
    CHECK(func.name == "returnMat3x2Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat3x2[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::mat3x2> val = ghoul::lua::value<std::vector<glm::mat3x2>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::mat3x2(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f));
    CHECK(val[1] == glm::mat3x2(7.7f, 8.8f, 9.9f, 10.10f, 11.11f, 12.12f));
    CHECK(val[2] == glm::mat3x2(13.13f, 14.14f, 15.15f, 16.16f, 17.17f, 18.18f));
    lua_close(state);
}
