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
    [[codegen::luawrap]] void funcIvec2(glm::ivec2 arg) {
        CHECK(arg == glm::ivec2(1, 2));
    }

    [[codegen::luawrap]] void funcIvec2Defaulted(glm::ivec2 arg = glm::ivec2(1, 2)) {
        CHECK(arg == glm::ivec2(1, 2));
    }

    [[codegen::luawrap]] void funcIvec2DefaultedCheck(bool isDefaulted, glm::ivec2 arg = glm::ivec2(1, 2)) {
        if (isDefaulted) {
            CHECK(arg == glm::ivec2(1, 2));
        }
        else {
            CHECK(arg == glm::ivec2(2, 3));
        }
    }

    [[codegen::luawrap]] void funcIvec2Map(std::map<std::string, glm::ivec2> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::ivec2(1, 2));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::ivec2(3, 4));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::ivec2(5, 6));
    }

    [[codegen::luawrap]] void funcIvec2Optional(std::optional<glm::ivec2> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::ivec2(1, 2));
    }

    [[codegen::luawrap]] void funcIvec2OptionalNullopt(std::optional<glm::ivec2> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcIvec2Vector(std::vector<glm::ivec2> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::ivec2(1, 2));
        CHECK(arg[1] == glm::ivec2(3, 4));
        CHECK(arg[2] == glm::ivec2(5, 6));
    }

    [[codegen::luawrap]] void funcIvec2Array1(std::array<glm::ivec2, 1> arg) {
        CHECK(arg[0] == glm::ivec2(1, 2));
    }

    [[codegen::luawrap]] void funcIvec2Array2(std::array<glm::ivec2, 5> arg) {
        CHECK(arg[0] == glm::ivec2(1, 2));
        CHECK(arg[1] == glm::ivec2(3, 4));
        CHECK(arg[2] == glm::ivec2(5, 6));
        CHECK(arg[3] == glm::ivec2(7, 8));
        CHECK(arg[4] == glm::ivec2(9, 10));
    }

    [[codegen::luawrap]] void funcIvec2Array3(std::array<glm::ivec2, 10> arg) {
        CHECK(arg[0] == glm::ivec2(1, 2));
        CHECK(arg[1] == glm::ivec2(3, 4));
        CHECK(arg[2] == glm::ivec2(5, 6));
        CHECK(arg[3] == glm::ivec2(7, 8));
        CHECK(arg[4] == glm::ivec2(9, 10));
        CHECK(arg[5] == glm::ivec2(11, 12));
        CHECK(arg[6] == glm::ivec2(13, 14));
        CHECK(arg[7] == glm::ivec2(15, 16));
        CHECK(arg[8] == glm::ivec2(17, 18));
        CHECK(arg[9] == glm::ivec2(19, 20));
    }

    [[codegen::luawrap]] glm::ivec2 returnIvec2() {
        return glm::ivec2(1, 2);
    }

    [[codegen::luawrap]] std::map<std::string, glm::ivec2> returnIvec2Map() {
        return {
            { "key1", glm::ivec2(1, 2) },
            { "key2", glm::ivec2(3, 4) },
            { "key3", glm::ivec2(5, 6) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::ivec2> returnIvec2Optional() {
        return glm::ivec2(1, 2);
    }

    [[codegen::luawrap]] std::optional<glm::ivec2> returnIvec2OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::ivec2> returnIvec2Vector() {
        return { glm::ivec2(1, 2), glm::ivec2(3, 4), glm::ivec2(5, 6) };
    }
} // namespace
#include "execution_luawrapper_types_ivec2_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  ivec2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec2;
    CHECK(func.name == "funcIvec2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::ivec2(1, 2));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec2Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec2Defaulted;
    CHECK(func.name == "funcIvec2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec2?");
    CHECK(func.arguments[0].defaultValue == "glm::ivec2(1, 2)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::ivec2(1, 2));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec2Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec2Defaulted;
    CHECK(func.name == "funcIvec2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec2?");
    CHECK(func.arguments[0].defaultValue == "glm::ivec2(1, 2)");
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
    "Execution/LuaWrapper/Arguments:  ivec2DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec2DefaultedCheck;
    CHECK(func.name == "funcIvec2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "ivec2?");
    CHECK(func.arguments[1].defaultValue == "glm::ivec2(1, 2)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, glm::ivec2(2, 3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec2DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec2DefaultedCheck;
    CHECK(func.name == "funcIvec2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "ivec2?");
    CHECK(func.arguments[1].defaultValue == "glm::ivec2(1, 2)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec2Map;
    CHECK(func.name == "funcIvec2Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> ivec2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", glm::ivec2(1, 2));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", glm::ivec2(3, 4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", glm::ivec2(5, 6));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec2Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec2Optional;
    CHECK(func.name == "funcIvec2Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::ivec2(1, 2));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  ivec2OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncIvec2OptionalNullopt;
    CHECK(func.name == "funcIvec2OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec2Vector;
    CHECK(func.name == "funcIvec2Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec2[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::ivec2(1, 2));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::ivec2(3, 4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::ivec2(5, 6));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec2Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec2Array1;
    CHECK(func.name == "funcIvec2Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec2[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::ivec2(1, 2));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<glm::ivec2, 1> { glm::ivec2(1, 2) });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec2Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec2Array2;
    CHECK(func.name == "funcIvec2Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec2[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::ivec2(1, 2));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::ivec2(3, 4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::ivec2(5, 6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::ivec2(7, 8));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::ivec2(9, 10));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::ivec2, 5> {
            glm::ivec2(1, 2),
            glm::ivec2(3, 4),
            glm::ivec2(5, 6),
            glm::ivec2(7, 8),
            glm::ivec2(9, 10)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  ivec2Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncIvec2Array3;
    CHECK(func.name == "funcIvec2Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "ivec2[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::ivec2(1, 2));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::ivec2(3, 4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::ivec2(5, 6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::ivec2(7, 8));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::ivec2(9, 10));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, glm::ivec2(11, 12));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, glm::ivec2(13, 14));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, glm::ivec2(15, 16));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, glm::ivec2(17, 18));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, glm::ivec2(19, 20));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::ivec2, 10> {
            glm::ivec2(1, 2),
            glm::ivec2(3, 4),
            glm::ivec2(5, 6),
            glm::ivec2(7, 8),
            glm::ivec2(9, 10),
            glm::ivec2(11, 12),
            glm::ivec2(13, 14),
            glm::ivec2(15, 16),
            glm::ivec2(17, 18),
            glm::ivec2(19, 20)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);


    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  ivec2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnIvec2;
    CHECK(func.name == "returnIvec2");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::ivec2 val = ghoul::lua::value<glm::ivec2>(state);
    CHECK(val == glm::ivec2(1, 2));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  ivec2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnIvec2Map;
    CHECK(func.name == "returnIvec2Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> ivec2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::ivec2> val =
        ghoul::lua::value<std::map<std::string, glm::ivec2>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::ivec2(1, 2));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::ivec2(3, 4));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::ivec2(5, 6));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  ivec2Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnIvec2Optional;
    CHECK(func.name == "returnIvec2Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::ivec2 val = ghoul::lua::value<glm::ivec2>(state);
    CHECK(val == glm::ivec2(1, 2));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  ivec2Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnIvec2OptionalNullopt;
    CHECK(func.name == "returnIvec2OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  ivec2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnIvec2Vector;
    CHECK(func.name == "returnIvec2Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "ivec2[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector val = ghoul::lua::value<std::vector<glm::ivec2>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::ivec2(1, 2));
    CHECK(val[1] == glm::ivec2(3, 4));
    CHECK(val[2] == glm::ivec2(5, 6));
    lua_close(state);
}
