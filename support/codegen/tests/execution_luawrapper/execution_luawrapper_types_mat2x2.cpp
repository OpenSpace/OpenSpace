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
    [[codegen::luawrap]] void funcMat2x2(glm::mat2x2 arg) {
        CHECK(arg == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    }

    [[codegen::luawrap]] void funcMat2x2Defaulted(glm::mat2x2 arg = glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f)) {
        CHECK(arg == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    }

    [[codegen::luawrap]] void funcMat2x2DefaultedCheck(bool isDefaulted, glm::mat2x2 arg = glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f)) {
        if (isDefaulted) {
            CHECK(arg == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
        }
        else {
            CHECK(arg == glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
        }
    }

    [[codegen::luawrap]] void funcMat2x2Map(std::map<std::string, glm::mat2x2> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
    }

    [[codegen::luawrap]] void funcMat2x2Optional(std::optional<glm::mat2x2> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    }

    [[codegen::luawrap]] void funcMat2x2OptionalNullopt(std::optional<glm::mat2x2> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcMat2x2Vector(std::vector<glm::mat2x2> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
        CHECK(arg[1] == glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
        CHECK(arg[2] == glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
    }

    [[codegen::luawrap]] void funcMat2x2Array1(std::array<glm::mat2x2, 1> arg) {
        CHECK(arg[0] == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    }

    [[codegen::luawrap]] void funcMat2x2Array2(std::array<glm::mat2x2, 5> arg) {
        CHECK(arg[0] == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
        CHECK(arg[1] == glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
        CHECK(arg[2] == glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[3] == glm::mat2x2(13.13f, 14.14f, 15.15f, 16.16f));
        CHECK(arg[4] == glm::mat2x2(17.17f, 18.18f, 19.19f, 20.20f));
    }

    [[codegen::luawrap]] void funcMat2x2Array3(std::array<glm::mat2x2, 10> arg) {
        CHECK(arg[0] == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
        CHECK(arg[1] == glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
        CHECK(arg[2] == glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
        CHECK(arg[3] == glm::mat2x2(13.13f, 14.14f, 15.15f, 16.16f));
        CHECK(arg[4] == glm::mat2x2(17.17f, 18.18f, 19.19f, 20.20f));
        CHECK(arg[5] == glm::mat2x2(21.21f, 22.22f, 23.23f, 24.24f));
        CHECK(arg[6] == glm::mat2x2(25.25f, 26.26f, 27.27f, 28.28f));
        CHECK(arg[7] == glm::mat2x2(29.29f, 30.30f, 31.31f, 32.32f));
        CHECK(arg[8] == glm::mat2x2(33.33f, 34.34f, 35.35f, 36.36f));
        CHECK(arg[9] == glm::mat2x2(37.37f, 38.38f, 39.39f, 40.40f));
    }

    [[codegen::luawrap]] glm::mat2x2 returnMat2x2() {
        return glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f);
    }

    [[codegen::luawrap]] std::map<std::string, glm::mat2x2> returnMat2x2Map() {
        return {
            { "key1", glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f) },
            { "key2", glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f) },
            { "key3", glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::mat2x2> returnMat2x2Optional() {
        return glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f);
    }

    [[codegen::luawrap]] std::optional<glm::mat2x2> returnMat2x2OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::mat2x2> returnMat2x2Vector() {
        return {
            glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f),
            glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f),
            glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f)
        };
    }
} // namespace
#include "execution_luawrapper_types_mat2x2_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat2x2;
    CHECK(func.name == "funcMat2x2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat2x2Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat2x2Defaulted;
    CHECK(func.name == "funcMat2x2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2?");
    CHECK(func.arguments[0].defaultValue == "glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat2x2Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat2x2Defaulted;
    CHECK(func.name == "funcMat2x2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2?");
    CHECK(func.arguments[0].defaultValue == "glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f)");
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
    "Execution/LuaWrapper/Arguments:  mat2x2DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat2x2DefaultedCheck;
    CHECK(func.name == "funcMat2x2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat2x2?");
    CHECK(func.arguments[1].defaultValue == "glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat2x2DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat2x2DefaultedCheck;
    CHECK(func.name == "funcMat2x2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "mat2x2?");
    CHECK(func.arguments[1].defaultValue == "glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat2x2Map;
    CHECK(func.name == "funcMat2x2Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> mat2x2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x2Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat2x2Optional;
    CHECK(func.name == "funcMat2x2Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  mat2x2OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMat2x2OptionalNullopt;
    CHECK(func.name == "funcMat2x2OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat2x2Vector;
    CHECK(func.name == "funcMat2x2Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x2Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat2x2Array1;
    CHECK(func.name == "funcMat2x2Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::mat2x2, 1> {
            glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x2Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat2x2Array2;
    CHECK(func.name == "funcMat2x2Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::mat2x2(13.13f, 14.14f, 15.15f, 16.16f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::mat2x2(17.17f, 18.18f, 19.19f, 20.20f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::mat2x2, 5> {
            glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f),
            glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f),
            glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f),
            glm::mat2x2(13.13f, 14.14f, 15.15f, 16.16f),
            glm::mat2x2(17.17f, 18.18f, 19.19f, 20.20f)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  mat2x2Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncMat2x2Array3;
    CHECK(func.name == "funcMat2x2Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "mat2x2[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::mat2x2(13.13f, 14.14f, 15.15f, 16.16f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::mat2x2(17.17f, 18.18f, 19.19f, 20.20f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, glm::mat2x2(21.21f, 22.22f, 23.23f, 24.24f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, glm::mat2x2(25.25f, 26.26f, 27.27f, 28.28f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, glm::mat2x2(29.29f, 30.30f, 31.31f, 32.32f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, glm::mat2x2(33.33f, 34.34f, 35.35f, 36.36f));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, glm::mat2x2(37.37f, 38.38f, 39.39f, 40.40f));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::mat2x2, 10> {
            glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f),
            glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f),
            glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f),
            glm::mat2x2(13.13f, 14.14f, 15.15f, 16.16f),
            glm::mat2x2(17.17f, 18.18f, 19.19f, 20.20f),
            glm::mat2x2(21.21f, 22.22f, 23.23f, 24.24f),
            glm::mat2x2(25.25f, 26.26f, 27.27f, 28.28f),
            glm::mat2x2(29.29f, 30.30f, 31.31f, 32.32f),
            glm::mat2x2(33.33f, 34.34f, 35.35f, 36.36f),
            glm::mat2x2(37.37f, 38.38f, 39.39f, 40.40f)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat2x2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnMat2x2;
    CHECK(func.name == "returnMat2x2");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat2x2 val = ghoul::lua::value<glm::mat2x2>(state);
    CHECK(val == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  mat2x2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnMat2x2Map;
    CHECK(func.name == "returnMat2x2Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> mat2x2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::mat2x2> val =
        ghoul::lua::value<std::map<std::string, glm::mat2x2>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat2x2Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnMat2x2Optional;
    CHECK(func.name == "returnMat2x2Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::mat2x2 val = ghoul::lua::value<glm::mat2x2>(state);
    CHECK(val == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  mat2x2Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnMat2x2OptionalNullopt;
    CHECK(func.name == "returnMat2x2OptionalNullopt");
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

TEST_CASE("Execution/LuaWrapper/Return:  mat2x2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnMat2x2Vector;
    CHECK(func.name == "returnMat2x2Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "mat2x2[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::mat2x2> val = ghoul::lua::value<std::vector<glm::mat2x2>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::mat2x2(1.1f, 2.2f, 3.3f, 4.4f));
    CHECK(val[1] == glm::mat2x2(5.5f, 6.6f, 7.7f, 8.8f));
    CHECK(val[2] == glm::mat2x2(9.9f, 10.10f, 11.11f, 12.12f));
    lua_close(state);
}
