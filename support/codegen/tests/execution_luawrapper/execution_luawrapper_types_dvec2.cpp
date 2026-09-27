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
    [[codegen::luawrap]] void funcDvec2(glm::dvec2 arg) {
        CHECK(arg == glm::dvec2(1.1, 2.2));
    }

    [[codegen::luawrap]] void funcDvec2Defaulted(glm::dvec2 arg = glm::dvec2(1.1, 2.2)) {
        CHECK(arg == glm::dvec2(1.1, 2.2));
    }

    [[codegen::luawrap]] void funcDvec2DefaultedCheck(bool isDefaulted, glm::dvec2 arg = glm::dvec2(1.1, 2.2)) {
        if (isDefaulted) {
            CHECK(arg == glm::dvec2(1.1, 2.2));
        }
        else {
            CHECK(arg == glm::dvec2(3.3, 4.4));
        }
    }

    [[codegen::luawrap]] void funcDvec2Map(std::map<std::string, glm::dvec2> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::dvec2(1.1, 2.2));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::dvec2(3.3, 4.4));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::dvec2(5.5, 6.6));
    }

    [[codegen::luawrap]] void funcDvec2Optional(std::optional<glm::dvec2> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::dvec2(1.1, 2.2));
    }

    [[codegen::luawrap]] void funcDvec2OptionalNullopt(std::optional<glm::dvec2> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDvec2Vector(std::vector<glm::dvec2> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::dvec2(1.1, 2.2));
        CHECK(arg[1] == glm::dvec2(3.3, 4.4));
        CHECK(arg[2] == glm::dvec2(5.5, 6.6));
    }

    [[codegen::luawrap]] void funcDvec2Array1(std::array<glm::dvec2, 1> arg) {
        CHECK(arg[0] == glm::dvec2(1.1, 2.2));
    }

    [[codegen::luawrap]] void funcDvec2Array2(std::array<glm::dvec2, 5> arg) {
        CHECK(arg[0] == glm::dvec2(1.1, 2.2));
        CHECK(arg[1] == glm::dvec2(3.3, 4.4));
        CHECK(arg[2] == glm::dvec2(5.5, 6.6));
        CHECK(arg[3] == glm::dvec2(7.7, 8.8));
        CHECK(arg[4] == glm::dvec2(9.9, 10.10));
    }

    [[codegen::luawrap]] void funcDvec2Array3(std::array<glm::dvec2, 10> arg) {
        CHECK(arg[0] == glm::dvec2(1.1, 2.2));
        CHECK(arg[1] == glm::dvec2(3.3, 4.4));
        CHECK(arg[2] == glm::dvec2(5.5, 6.6));
        CHECK(arg[3] == glm::dvec2(7.7, 8.8));
        CHECK(arg[4] == glm::dvec2(9.9, 10.10));
        CHECK(arg[5] == glm::dvec2(11.11, 12.12));
        CHECK(arg[6] == glm::dvec2(13.13, 14.14));
        CHECK(arg[7] == glm::dvec2(15.15, 16.16));
        CHECK(arg[8] == glm::dvec2(17.17, 18.18));
        CHECK(arg[9] == glm::dvec2(19.19, 20.20));
    }

    [[codegen::luawrap]] glm::dvec2 returnDvec2() {
        return glm::dvec2(1.1, 2.2);
    }

    [[codegen::luawrap]] std::map<std::string, glm::dvec2> returnDvec2Map() {
        return {
            { "key1", glm::dvec2(1.1, 2.2) },
            { "key2", glm::dvec2(3.3, 4.4) },
            { "key3", glm::dvec2(5.5, 6.6) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::dvec2> returnDvec2Optional() {
        return glm::dvec2(1.1, 2.2);
    }

    [[codegen::luawrap]] std::optional<glm::dvec2> returnDvec2OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::dvec2> returnDvec2Vector() {
        return {
            glm::dvec2(1.1, 2.2),
            glm::dvec2(3.3, 4.4),
            glm::dvec2(5.5, 6.6)
        };
    }
} // namespace
#include "execution_luawrapper_types_dvec2_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  dvec2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec2;
    CHECK(func.name == "funcDvec2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dvec2(1.1, 2.2));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec2Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec2Defaulted;
    CHECK(func.name == "funcDvec2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2?");
    CHECK(func.arguments[0].defaultValue == "glm::dvec2(1.1, 2.2)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dvec2(1.1, 2.2));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec2Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec2Defaulted;
    CHECK(func.name == "funcDvec2Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2?");
    CHECK(func.arguments[0].defaultValue == "glm::dvec2(1.1, 2.2)");
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
    "Execution/LuaWrapper/Arguments:  dvec2DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec2DefaultedCheck;
    CHECK(func.name == "funcDvec2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec2?");
    CHECK(func.arguments[1].defaultValue == "glm::dvec2(1.1, 2.2)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, glm::dvec2(3.3, 4.4));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec2DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec2DefaultedCheck;
    CHECK(func.name == "funcDvec2DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec2?");
    CHECK(func.arguments[1].defaultValue == "glm::dvec2(1.1, 2.2)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec2Map;
    CHECK(func.name == "funcDvec2Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> vec2");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", glm::dvec2(1.1, 2.2));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", glm::dvec2(3.3, 4.4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", glm::dvec2(5.5, 6.6));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec2Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec2Optional;
    CHECK(func.name == "funcDvec2Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dvec2(1.1, 2.2));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec2OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec2OptionalNullopt;
    CHECK(func.name == "funcDvec2OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec2Vector;
    CHECK(func.name == "funcDvec2Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dvec2(1.1, 2.2));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::dvec2(3.3, 4.4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::dvec2(5.5, 6.6));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec2Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec2Array1;
    CHECK(func.name == "funcDvec2Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dvec2(1.1, 2.2));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<glm::dvec2, 1> { glm::dvec2(1.1, 2.2) });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec2Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec2Array2;
    CHECK(func.name == "funcDvec2Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dvec2(1.1, 2.2));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::dvec2(3.3, 4.4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::dvec2(5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::dvec2(7.7, 8.8));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::dvec2(9.9, 10.10));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dvec2, 5> {
            glm::dvec2(1.1, 2.2),
            glm::dvec2(3.3, 4.4),
            glm::dvec2(5.5, 6.6),
            glm::dvec2(7.7, 8.8),
            glm::dvec2(9.9, 10.10)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec24Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec2Array3;
    CHECK(func.name == "funcDvec2Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec2[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dvec2(1.1, 2.2));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::dvec2(3.3, 4.4));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::dvec2(5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::dvec2(7.7, 8.8));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::dvec2(9.9, 10.10));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, glm::dvec2(11.11, 12.12));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, glm::dvec2(13.13, 14.14));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, glm::dvec2(15.15, 16.16));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, glm::dvec2(17.17, 18.18));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, glm::dvec2(19.19, 20.20));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dvec2, 10> {
            glm::dvec2(1.1, 2.2),
            glm::dvec2(3.3, 4.4),
            glm::dvec2(5.5, 6.6),
            glm::dvec2(7.7, 8.8),
            glm::dvec2(9.9, 10.10),
            glm::dvec2(11.11, 12.12),
            glm::dvec2(13.13, 14.14),
            glm::dvec2(15.15, 16.16),
            glm::dvec2(17.17, 18.18),
            glm::dvec2(19.19, 20.20)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dvec2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDvec2;
    CHECK(func.name == "returnDvec2");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    ghoul::lua::value<glm::dvec2>(state);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dvec2Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDvec2Map;
    CHECK(func.name == "returnDvec2Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> vec2");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::dvec2> val =
        ghoul::lua::value<std::map<std::string, glm::dvec2>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::dvec2(1.1, 2.2));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::dvec2(3.3, 4.4));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::dvec2(5.5, 6.6));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dvec2Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDvec2Optional;
    CHECK(func.name == "returnDvec2Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dvec2 val = ghoul::lua::value<glm::dvec2>(state);
    CHECK(val == glm::dvec2(1.1, 2.2));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dvec2Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDvec2OptionalNullopt;
    CHECK(func.name == "returnDvec2OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec2?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dvec2Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDvec2Vector;
    CHECK(func.name == "returnDvec2Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec2[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::dvec2> val = ghoul::lua::value<std::vector<glm::dvec2>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::dvec2(1.1, 2.2));
    CHECK(val[1] == glm::dvec2(3.3, 4.4));
    CHECK(val[2] == glm::dvec2(5.5, 6.6));
    lua_close(state);
}
