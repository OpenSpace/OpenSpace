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
    [[codegen::luawrap]] void funcIvec3(glm::ivec3 arg) {
        CHECK(arg == glm::ivec3(1, 2, 3));
    }

    [[codegen::luawrap]] void funcIvec3Defaulted(glm::ivec3 arg = glm::ivec3(1, 2, 3)) {
        CHECK(arg == glm::ivec3(1, 2, 3));
    }

    [[codegen::luawrap]] void funcIvec3DefaultedCheck(bool isDefaulted, glm::ivec3 arg = glm::ivec3(1, 2, 3)) {
        if (isDefaulted) {
            CHECK(arg == glm::ivec3(1, 2, 3));
        }
        else {
            CHECK(arg == glm::ivec3(4, 5, 6));
        }
    }

    [[codegen::luawrap]] void funcIvec3Map(std::map<std::string, glm::ivec3> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::ivec3(1, 2, 3));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::ivec3(4, 5, 6));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::ivec3(7, 8, 9));
    }

    [[codegen::luawrap]] void funcIvec3Optional(std::optional<glm::ivec3> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::ivec3(1, 2, 3));
    }

    [[codegen::luawrap]] void funcIvec3OptionalNullopt(std::optional<glm::ivec3> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcIvec3Vector(std::vector<glm::ivec3> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::ivec3(1, 2, 3));
        CHECK(arg[1] == glm::ivec3(4, 5, 6));
        CHECK(arg[2] == glm::ivec3(7, 8, 9));
    }

    [[codegen::luawrap]] void funcIvec3Array1(std::array<glm::ivec3, 1> arg) {
        CHECK(arg[0] == glm::ivec3(1, 2, 3));
    }

    [[codegen::luawrap]] void funcIvec3Array2(std::array<glm::ivec3, 5> arg) {
        CHECK(arg[0] == glm::ivec3(1, 2, 3));
        CHECK(arg[1] == glm::ivec3(4, 5, 6));
        CHECK(arg[2] == glm::ivec3(7, 8, 9));
        CHECK(arg[3] == glm::ivec3(10, 11, 12));
        CHECK(arg[4] == glm::ivec3(13, 14, 15));
    }

    [[codegen::luawrap]] void funcIvec3Array3(std::array<glm::ivec3, 10> arg) {
        CHECK(arg[0] == glm::ivec3(1, 2, 3));
        CHECK(arg[1] == glm::ivec3(4, 5, 6));
        CHECK(arg[2] == glm::ivec3(7, 8, 9));
        CHECK(arg[3] == glm::ivec3(10, 11, 12));
        CHECK(arg[4] == glm::ivec3(13, 14, 15));
        CHECK(arg[5] == glm::ivec3(16, 17, 18));
        CHECK(arg[6] == glm::ivec3(19, 20, 21));
        CHECK(arg[7] == glm::ivec3(22, 23, 24));
        CHECK(arg[8] == glm::ivec3(25, 26, 27));
        CHECK(arg[9] == glm::ivec3(28, 29, 30));
    }

    [[codegen::luawrap]] glm::ivec3 returnIvec3() {
        return glm::ivec3(1, 2, 3);
    }

    [[codegen::luawrap]] std::map<std::string, glm::ivec3> returnIvec3Map() {
        return {
            { "key1", glm::ivec3(1, 2, 3) },
            { "key2", glm::ivec3(4, 5, 6) },
            { "key3", glm::ivec3(7, 8, 9) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::ivec3> returnIvec3Optional() {
        return glm::ivec3(1, 2, 3);
    }

    [[codegen::luawrap]] std::optional<glm::ivec3> returnIvec3OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::ivec3> returnIvec3Vector() {
        return {
            glm::ivec3(1, 2, 3),
            glm::ivec3(4, 5, 6),
            glm::ivec3(7, 8, 9)
        };
    }
} // namespace
#include "execution_luawrapper_types_ivec3_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  ivec3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIvec3;
    CHECK(func.name == "funcIvec3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::ivec3(1, 2, 3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec3Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIvec3Defaulted;
    CHECK(func.name == "funcIvec3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec3?");
    CHECK(func.arguments[0].defaultValue == "glm::ivec3(1, 2, 3)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::ivec3(1, 2, 3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec3Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIvec3Defaulted;
    CHECK(func.name == "funcIvec3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec3?");
    CHECK(func.arguments[0].defaultValue == "glm::ivec3(1, 2, 3)");
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
    "Execution/LuaWrapper/Arguments:  ivec3DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIvec3DefaultedCheck;
    CHECK(func.name == "funcIvec3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "ivec3?");
    CHECK(func.arguments[1].defaultValue == "glm::ivec3(1, 2, 3)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, glm::ivec3(4, 5, 6));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec3DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIvec3DefaultedCheck;
    CHECK(func.name == "funcIvec3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "ivec3?");
    CHECK(func.arguments[1].defaultValue == "glm::ivec3(1, 2, 3)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIvec3Map;
    CHECK(func.name == "funcIvec3Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> ivec3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, "key1", glm::ivec3(1, 2, 3));
    lua_rawset(state, -3);
    lua::push(state, "key2", glm::ivec3(4, 5, 6));
    lua_rawset(state, -3);
    lua::push(state, "key3", glm::ivec3(7, 8, 9));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec3Optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIvec3Optional;
    CHECK(func.name == "funcIvec3Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::ivec3(1, 2, 3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec3OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncIvec3OptionalNullopt;
    CHECK(func.name == "funcIvec3OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIvec3Vector;
    CHECK(func.name == "funcIvec3Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec3[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::ivec3(1, 2, 3));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::ivec3(4, 5, 6));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::ivec3(7, 8, 9));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec3Array1", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIvec3Array1;
    CHECK(func.name == "funcIvec3Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec3[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::ivec3(1, 2, 3));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(state, std::array<glm::ivec3, 1> { glm::ivec3(1, 2, 3) });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec3Array2", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIvec3Array2;
    CHECK(func.name == "funcIvec3Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec3[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::ivec3(1, 2, 3));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::ivec3(4, 5, 6));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::ivec3(7, 8, 9));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::ivec3(10, 11, 12));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::ivec3(13, 14, 15));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::ivec3, 5> {
            glm::ivec3(1, 2, 3),
            glm::ivec3(4, 5, 6),
            glm::ivec3(7, 8, 9),
            glm::ivec3(10, 11, 12),
            glm::ivec3(13, 14, 15)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec3Array3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIvec3Array3;
    CHECK(func.name == "funcIvec3Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec3[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    lua::push(state, 1, glm::ivec3(1, 2, 3));
    lua_rawset(state, -3);
    lua::push(state, 2, glm::ivec3(4, 5, 6));
    lua_rawset(state, -3);
    lua::push(state, 3, glm::ivec3(7, 8, 9));
    lua_rawset(state, -3);
    lua::push(state, 4, glm::ivec3(10, 11, 12));
    lua_rawset(state, -3);
    lua::push(state, 5, glm::ivec3(13, 14, 15));
    lua_rawset(state, -3);
    lua::push(state, 6, glm::ivec3(16, 17, 18));
    lua_rawset(state, -3);
    lua::push(state, 7, glm::ivec3(19, 20, 21));
    lua_rawset(state, -3);
    lua::push(state, 8, glm::ivec3(22, 23, 24));
    lua_rawset(state, -3);
    lua::push(state, 9, glm::ivec3(25, 26, 27));
    lua_rawset(state, -3);
    lua::push(state, 10, glm::ivec3(28, 29, 30));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(
        state,
        std::array<glm::ivec3, 10> {
            glm::ivec3(1, 2, 3),
            glm::ivec3(4, 5, 6),
            glm::ivec3(7, 8, 9),
            glm::ivec3(10, 11, 12),
            glm::ivec3(13, 14, 15),
            glm::ivec3(16, 17, 18),
            glm::ivec3(19, 20, 21),
            glm::ivec3(22, 23, 24),
            glm::ivec3(25, 26, 27),
            glm::ivec3(28, 29, 30)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  ivec3", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnIvec3;
    CHECK(func.name == "returnIvec3");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::ivec3 val = lua::value<glm::ivec3>(state);
    CHECK(val == glm::ivec3(1, 2, 3));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  ivec3Map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnIvec3Map;
    CHECK(func.name == "returnIvec3Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> ivec3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::ivec3> val =
        lua::value<std::map<std::string, glm::ivec3>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::ivec3(1, 2, 3));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::ivec3(4, 5, 6));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::ivec3(7, 8, 9));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  ivec3Optional with value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnIvec3Optional;
    CHECK(func.name == "returnIvec3Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::ivec3 val = lua::value<glm::ivec3>(state);
    CHECK(val == glm::ivec3(1, 2, 3));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  ivec3Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnIvec3OptionalNullopt;
    CHECK(func.name == "returnIvec3OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  ivec3Vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnIvec3Vector;
    CHECK(func.name == "returnIvec3Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec3[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::ivec3> val = lua::value<std::vector<glm::ivec3>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::ivec3(1, 2, 3));
    CHECK(val[1] == glm::ivec3(4, 5, 6));
    CHECK(val[2] == glm::ivec3(7, 8, 9));
    lua_close(state);
}
