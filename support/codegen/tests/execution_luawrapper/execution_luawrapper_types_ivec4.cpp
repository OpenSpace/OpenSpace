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
    [[codegen::luawrap]] void funcIvec4(glm::ivec4 arg) {
        CHECK(arg == glm::ivec4(1, 2, 3, 4));
    }

    [[codegen::luawrap]] void funcIvec4Defaulted(glm::ivec4 arg = glm::ivec4(1, 2, 3, 4)) {
        CHECK(arg == glm::ivec4(1, 2, 3, 4));
    }

    [[codegen::luawrap]] void funcIvec4DefaultedCheck(bool isDefaulted, glm::ivec4 arg = glm::ivec4(1, 2, 3, 4)) {
        if (isDefaulted) {
            CHECK(arg == glm::ivec4(1, 2, 3, 4));
        }
        else {
            CHECK(arg == glm::ivec4(5, 6, 7, 8));
        }
    }

    [[codegen::luawrap]] void funcIvec4Map(std::map<std::string, glm::ivec4> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::ivec4(1, 2, 3, 4));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::ivec4(5, 6, 7, 8));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::ivec4(9, 10, 11, 12));
    }

    [[codegen::luawrap]] void funcIvec4Optional(std::optional<glm::ivec4> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::ivec4(1, 2, 3, 4));
    }

    [[codegen::luawrap]] void funcIvec4OptionalNullopt(std::optional<glm::ivec4> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcIvec4Vector(std::vector<glm::ivec4> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::ivec4(1, 2, 3, 4));
        CHECK(arg[1] == glm::ivec4(5, 6, 7, 8));
        CHECK(arg[2] == glm::ivec4(9, 10, 11, 12));
    }

    [[codegen::luawrap]] void funcIvec4Array1(std::array<glm::ivec4, 1> arg) {
        CHECK(arg[0] == glm::ivec4(1, 2, 3, 4));
    }

    [[codegen::luawrap]] void funcIvec4Array2(std::array<glm::ivec4, 5> arg) {
        CHECK(arg[0] == glm::ivec4(1, 2, 3, 4));
        CHECK(arg[1] == glm::ivec4(5, 6, 7, 8));
        CHECK(arg[2] == glm::ivec4(9, 10, 11, 12));
        CHECK(arg[3] == glm::ivec4(13, 14, 15, 16));
        CHECK(arg[4] == glm::ivec4(17, 18, 19, 20));
    }

    [[codegen::luawrap]] void funcIvec4Array3(std::array<glm::ivec4, 10> arg) {
        CHECK(arg[0] == glm::ivec4(1, 2, 3, 4));
        CHECK(arg[1] == glm::ivec4(5, 6, 7, 8));
        CHECK(arg[2] == glm::ivec4(9, 10, 11, 12));
        CHECK(arg[3] == glm::ivec4(13, 14, 15, 16));
        CHECK(arg[4] == glm::ivec4(17, 18, 19, 20));
        CHECK(arg[5] == glm::ivec4(21, 22, 23, 24));
        CHECK(arg[6] == glm::ivec4(25, 26, 27, 28));
        CHECK(arg[7] == glm::ivec4(29, 30, 31, 32));
        CHECK(arg[8] == glm::ivec4(33, 34, 35, 36));
        CHECK(arg[9] == glm::ivec4(37, 38, 39, 40));
    }

    [[codegen::luawrap]] glm::ivec4 returnIvec4() {
        return glm::ivec4(1, 2, 3, 4);
    }

    [[codegen::luawrap]] std::map<std::string, glm::ivec4> returnIvec4Map() {
        return {
            { "key1", glm::ivec4(1, 2, 3, 4) },
            { "key2", glm::ivec4(5, 6, 7, 8) },
            { "key3", glm::ivec4(9, 10, 11, 12) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::ivec4> returnIvec4Optional() {
        return glm::ivec4(1, 2, 3, 4);
    }

    [[codegen::luawrap]] std::optional<glm::ivec4> returnIvec4OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::ivec4> returnIvec4Vector() {
        return {
            glm::ivec4(1, 2, 3, 4),
            glm::ivec4(5, 6, 7, 8),
            glm::ivec4(9, 10, 11, 12)
        };
    }
} // namespace
#include "execution_luawrapper_types_ivec4_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  ivec4", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec4;
    CHECK(func.name == "funcIvec4");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::ivec4(1, 2, 3, 4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec4Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec4Defaulted;
    CHECK(func.name == "funcIvec4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec4?");
    CHECK(func.arguments[0].defaultValue == "glm::ivec4(1, 2, 3, 4)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::ivec4(1, 2, 3, 4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec4Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec4Defaulted;
    CHECK(func.name == "funcIvec4Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec4?");
    CHECK(func.arguments[0].defaultValue == "glm::ivec4(1, 2, 3, 4)");
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
    "Execution/LuaWrapper/Arguments:  ivec4DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec4DefaultedCheck;
    CHECK(func.name == "funcIvec4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "ivec4?");
    CHECK(func.arguments[1].defaultValue == "glm::ivec4(1, 2, 3, 4)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, glm::ivec4(5, 6, 7, 8));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec4DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec4DefaultedCheck;
    CHECK(func.name == "funcIvec4DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "ivec4?");
    CHECK(func.arguments[1].defaultValue == "glm::ivec4(1, 2, 3, 4)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec4Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec4Map;
    CHECK(func.name == "funcIvec4Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> ivec4");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", glm::ivec4(1, 2, 3, 4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", glm::ivec4(5, 6, 7, 8));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", glm::ivec4(9, 10, 11, 12));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec4Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec4Optional;
    CHECK(func.name == "funcIvec4Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec4?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::ivec4(1, 2, 3, 4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec4OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec4OptionalNullopt;
    CHECK(func.name == "funcIvec4OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec4?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec4Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec4Vector;
    CHECK(func.name == "funcIvec4Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec4[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::ivec4(1, 2, 3, 4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::ivec4(5, 6, 7, 8));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::ivec4(9, 10, 11, 12));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec4Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec4Array1;
    CHECK(func.name == "funcIvec4Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec4[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::ivec4(1, 2, 3, 4));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<glm::ivec4, 1> { glm::ivec4(1, 2, 3, 4) });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec4Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec4Array2;
    CHECK(func.name == "funcIvec4Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec4[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::ivec4(1, 2, 3, 4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::ivec4(5, 6, 7, 8));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::ivec4(9, 10, 11, 12));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::ivec4(13, 14, 15, 16));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::ivec4(17, 18, 19, 20));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::ivec4, 5> {
            glm::ivec4(1, 2, 3, 4),
            glm::ivec4(5, 6, 7, 8),
            glm::ivec4(9, 10, 11, 12),
            glm::ivec4(13, 14, 15, 16),
            glm::ivec4(17, 18, 19, 20)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec4Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec4Array3;
    CHECK(func.name == "funcIvec4Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec4[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::ivec4(1, 2, 3, 4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::ivec4(5, 6, 7, 8));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::ivec4(9, 10, 11, 12));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::ivec4(13, 14, 15, 16));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::ivec4(17, 18, 19, 20));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, glm::ivec4(21, 22, 23, 24));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, glm::ivec4(25, 26, 27, 28));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, glm::ivec4(29, 30, 31, 32));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, glm::ivec4(33, 34, 35, 36));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, glm::ivec4(37, 38, 39, 40));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::ivec4, 10> {
            glm::ivec4(1, 2, 3, 4),
            glm::ivec4(5, 6, 7, 8),
            glm::ivec4(9, 10, 11, 12),
            glm::ivec4(13, 14, 15, 16),
            glm::ivec4(17, 18, 19, 20),
            glm::ivec4(21, 22, 23, 24),
            glm::ivec4(25, 26, 27, 28),
            glm::ivec4(29, 30, 31, 32),
            glm::ivec4(33, 34, 35, 36),
            glm::ivec4(37, 38, 39, 40)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  ivec4", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnIvec4;
    CHECK(func.name == "returnIvec4");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::ivec4 val = ghoul::lua::value<glm::ivec4>(state);
    CHECK(val == glm::ivec4(1, 2, 3, 4));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  ivec4Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnIvec4Map;
    CHECK(func.name == "returnIvec4Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> ivec4");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::ivec4> val =
        ghoul::lua::value<std::map<std::string, glm::ivec4>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::ivec4(1, 2, 3, 4));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::ivec4(5, 6, 7, 8));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::ivec4(9, 10, 11, 12));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  ivec4Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnIvec4Optional;
    CHECK(func.name == "returnIvec4Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec4?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::ivec4 val = ghoul::lua::value<glm::ivec4>(state);
    CHECK(val == glm::ivec4(1, 2, 3, 4));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  ivec4Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnIvec4OptionalNullopt;
    CHECK(func.name == "returnIvec4OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec4?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  ivec4Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnIvec4Vector;
    CHECK(func.name == "returnIvec4Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec4[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::ivec4> val = ghoul::lua::value<std::vector<glm::ivec4>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::ivec4(1, 2, 3, 4));
    CHECK(val[1] == glm::ivec4(5, 6, 7, 8));
    CHECK(val[2] == glm::ivec4(9, 10, 11, 12));
    lua_close(state);
}
