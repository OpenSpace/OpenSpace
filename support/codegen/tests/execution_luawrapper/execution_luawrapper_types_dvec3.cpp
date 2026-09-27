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
    [[codegen::luawrap]] void funcDvec3(glm::dvec3 arg) {
        CHECK(arg == glm::dvec3(1.1, 2.2, 3.3));
    }

    [[codegen::luawrap]] void funcDvec3Defaulted(glm::dvec3 arg = glm::dvec3(1.1, 2.2, 3.3)) {
        CHECK(arg == glm::dvec3(1.1, 2.2, 3.3));
    }

    [[codegen::luawrap]] void funcDvec3DefaultedCheck(bool isDefaulted, glm::dvec3 arg = glm::dvec3(1.1, 2.2, 3.3)) {
        if (isDefaulted) {
            CHECK(arg == glm::dvec3(1.1, 2.2, 3.3));
        }
        else {
            CHECK(arg == glm::dvec3(4.4, 5.5, 6.6));
        }
    }

    [[codegen::luawrap]] void funcDvec3Map(std::map<std::string, glm::dvec3> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second == glm::dvec3(1.1, 2.2, 3.3));
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second == glm::dvec3(4.4, 5.5, 6.6));
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second == glm::dvec3(7.7, 8.8, 9.9));
    }

    [[codegen::luawrap]] void funcDvec3Optional(std::optional<glm::dvec3> arg) {
        REQUIRE(arg.has_value());
        CHECK(*arg == glm::dvec3(1.1, 2.2, 3.3));
    }

    [[codegen::luawrap]] void funcDvec3OptionalNullopt(std::optional<glm::dvec3> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDvec3Vector(std::vector<glm::dvec3> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0] == glm::dvec3(1.1, 2.2, 3.3));
        CHECK(arg[1] == glm::dvec3(4.4, 5.5, 6.6));
        CHECK(arg[2] == glm::dvec3(7.7, 8.8, 9.9));
    }

    [[codegen::luawrap]] void funcDvec3Array1(std::array<glm::dvec3, 1> arg) {
        CHECK(arg[0] == glm::dvec3(1.1, 2.2, 3.3));
    }

    [[codegen::luawrap]] void funcDvec3Array2(std::array<glm::dvec3, 5> arg) {
        CHECK(arg[0] == glm::dvec3(1.1, 2.2, 3.3));
        CHECK(arg[1] == glm::dvec3(4.4, 5.5, 6.6));
        CHECK(arg[2] == glm::dvec3(7.7, 8.8, 9.9));
        CHECK(arg[3] == glm::dvec3(10.10, 11.11, 12.12));
        CHECK(arg[4] == glm::dvec3(13.13, 14.14, 15.15));
    }

    [[codegen::luawrap]] void funcDvec3Array3(std::array<glm::dvec3, 10> arg) {
        CHECK(arg[0] == glm::dvec3(1.1, 2.2, 3.3));
        CHECK(arg[1] == glm::dvec3(4.4, 5.5, 6.6));
        CHECK(arg[2] == glm::dvec3(7.7, 8.8, 9.9));
        CHECK(arg[3] == glm::dvec3(10.10, 11.11, 12.12));
        CHECK(arg[4] == glm::dvec3(13.13, 14.14, 15.15));
        CHECK(arg[5] == glm::dvec3(16.16, 17.17, 18.18));
        CHECK(arg[6] == glm::dvec3(19.19, 20.20, 21.21));
        CHECK(arg[7] == glm::dvec3(22.22, 23.23, 24.24));
        CHECK(arg[8] == glm::dvec3(25.25, 26.26, 27.27));
        CHECK(arg[9] == glm::dvec3(28.28, 29.29, 30.30));
    }

    [[codegen::luawrap]] glm::dvec3 returnDvec3() {
        return glm::dvec3(1.1, 2.2, 3.3);
    }

    [[codegen::luawrap]] std::map<std::string, glm::dvec3> returnDvec3Map() {
        return {
            { "key1", glm::dvec3(1.1, 2.2, 3.3) },
            { "key2", glm::dvec3(4.4, 5.5, 6.6) },
            { "key3", glm::dvec3(7.7, 8.8, 9.9) }
        };
    }

    [[codegen::luawrap]] std::optional<glm::dvec3> returnDvec3Optional() {
        return glm::dvec3(1.1, 2.2, 3.3);
    }

    [[codegen::luawrap]] std::optional<glm::dvec3> returnDvec3OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<glm::dvec3> returnDvec3Vector() {
        return {
            glm::dvec3(1.1, 2.2, 3.3),
            glm::dvec3(4.4, 5.5, 6.6),
            glm::dvec3(7.7, 8.8, 9.9)
        };
    }
} // namespace
#include "execution_luawrapper_types_dvec3_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  dvec3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec3;
    CHECK(func.name == "funcDvec3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dvec3(1.1, 2.2, 3.3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec3Defaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec3Defaulted;
    CHECK(func.name == "funcDvec3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3?");
    CHECK(func.arguments[0].defaultValue == "glm::dvec3(1.1, 2.2, 3.3)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dvec3(1.1, 2.2, 3.3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec3Defaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec3Defaulted;
    CHECK(func.name == "funcDvec3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3?");
    CHECK(func.arguments[0].defaultValue == "glm::dvec3(1.1, 2.2, 3.3)");
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
    "Execution/LuaWrapper/Arguments:  dvec3DefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec3DefaultedCheck;
    CHECK(func.name == "funcDvec3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec3?");
    CHECK(func.arguments[1].defaultValue == "glm::dvec3(1.1, 2.2, 3.3)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false, glm::dvec3(4.4, 5.5, 6.6));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec3DefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec3DefaultedCheck;
    CHECK(func.name == "funcDvec3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "vec3?");
    CHECK(func.arguments[1].defaultValue == "glm::dvec3(1.1, 2.2, 3.3)");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec3Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec3Map;
    CHECK(func.name == "funcDvec3Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> vec3");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "key1", glm::dvec3(1.1, 2.2, 3.3));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key2", glm::dvec3(4.4, 5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, "key3", glm::dvec3(7.7, 8.8, 9.9));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec3Optional", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec3Optional;
    CHECK(func.name == "funcDvec3Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, glm::dvec3(1.1, 2.2, 3.3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dvec3OptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDvec3OptionalNullopt;
    CHECK(func.name == "funcDvec3OptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec3Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec3Vector;
    CHECK(func.name == "funcDvec3Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dvec3(1.1, 2.2, 3.3));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::dvec3(4.4, 5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::dvec3(7.7, 8.8, 9.9));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec3Array1", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec3Array1;
    CHECK(func.name == "funcDvec3Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dvec3(1.1, 2.2, 3.3));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, std::array<glm::dvec3, 1> { glm::dvec3(1.1, 2.2, 3.3) });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec3Array2", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec3Array2;
    CHECK(func.name == "funcDvec3Array2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dvec3(1.1, 2.2, 3.3));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::dvec3(4.4, 5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::dvec3(7.7, 8.8, 9.9));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::dvec3(10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::dvec3(13.13, 14.14, 15.15));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dvec3, 5> {
            glm::dvec3(1.1, 2.2, 3.3),
            glm::dvec3(4.4, 5.5, 6.6),
            glm::dvec3(7.7, 8.8, 9.9),
            glm::dvec3(10.10, 11.11, 12.12),
            glm::dvec3(13.13, 14.14, 15.15)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dvec3Array3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncDvec3Array3;
    CHECK(func.name == "funcDvec3Array3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "vec3[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1, glm::dvec3(1.1, 2.2, 3.3));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 2, glm::dvec3(4.4, 5.5, 6.6));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 3, glm::dvec3(7.7, 8.8, 9.9));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 4, glm::dvec3(10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 5, glm::dvec3(13.13, 14.14, 15.15));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 6, glm::dvec3(16.16, 17.17, 18.18));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 7, glm::dvec3(19.19, 20.20, 21.21));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 8, glm::dvec3(22.22, 23.23, 24.24));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 9, glm::dvec3(25.25, 26.26, 27.27));
    lua_rawset(state, -3);
    ghoul::lua::push(state, 10, glm::dvec3(28.28, 29.29, 30.30));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(
        state,
        std::array<glm::dvec3, 10> {
            glm::dvec3(1.1, 2.2, 3.3),
            glm::dvec3(4.4, 5.5, 6.6),
            glm::dvec3(7.7, 8.8, 9.9),
            glm::dvec3(10.10, 11.11, 12.12),
            glm::dvec3(13.13, 14.14, 15.15),
            glm::dvec3(16.16, 17.17, 18.18),
            glm::dvec3(19.19, 20.20, 21.21),
            glm::dvec3(22.22, 23.23, 24.24),
            glm::dvec3(25.25, 26.26, 27.27),
            glm::dvec3(28.28, 29.29, 30.30)
        }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);


    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dvec3", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDvec3;
    CHECK(func.name == "returnDvec3");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dvec3 val = ghoul::lua::value<glm::dvec3>(state);
    CHECK(val == glm::dvec3(1.1, 2.2, 3.3));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dvec3Map", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDvec3Map;
    CHECK(func.name == "returnDvec3Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> vec3");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, glm::dvec3> val =
        ghoul::lua::value<std::map<std::string, glm::dvec3>>(state);
    CHECK(val.size() == 3);
    REQUIRE(val.find("key1") != val.end());
    CHECK(val.find("key1")->second == glm::dvec3(1.1, 2.2, 3.3));
    REQUIRE(val.find("key2") != val.end());
    CHECK(val.find("key2")->second == glm::dvec3(4.4, 5.5, 6.6));
    REQUIRE(val.find("key3") != val.end());
    CHECK(val.find("key3")->second == glm::dvec3(7.7, 8.8, 9.9));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dvec3Optional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDvec3Optional;
    CHECK(func.name == "returnDvec3Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    glm::dvec3 val = ghoul::lua::value<glm::dvec3>(state);
    CHECK(val == glm::dvec3(1.1, 2.2, 3.3));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dvec3Optional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDvec3OptionalNullopt;
    CHECK(func.name == "returnDvec3OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec3?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dvec3Vector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDvec3Vector;
    CHECK(func.name == "returnDvec3Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "vec3[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<glm::dvec3> val = ghoul::lua::value<std::vector<glm::dvec3>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == glm::dvec3(1.1, 2.2, 3.3));
    CHECK(val[1] == glm::dvec3(4.4, 5.5, 6.6));
    CHECK(val[2] == glm::dvec3(7.7, 8.8, 9.9));
    lua_close(state);
}
