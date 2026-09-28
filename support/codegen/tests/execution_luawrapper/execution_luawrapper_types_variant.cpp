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
    [[codegen::luawrap]] void funcVariantBool(std::variant<bool, int> arg) {
        REQUIRE(std::holds_alternative<bool>(arg));
        CHECK(std::get<bool>(arg) == true);
    }

    [[codegen::luawrap]] void funcVariantInt(std::variant<bool, int> arg) {
        REQUIRE(std::holds_alternative<int>(arg));
        CHECK(std::get<int>(arg) == 2);
    }

    [[codegen::luawrap]] void funcVariantDoubleDvec3StringDouble(std::variant<double, glm::dvec3, std::string> arg) {
        REQUIRE(std::holds_alternative<double>(arg));
        CHECK(std::get<double>(arg) == 2.2);
    }

    [[codegen::luawrap]] void funcVariantDoubleDvec3StringDvec3(std::variant<double, glm::dvec3, std::string> arg) {
        REQUIRE(std::holds_alternative<glm::dvec3>(arg));
        CHECK(std::get<glm::dvec3>(arg) == glm::dvec3(1.1, 2.2, 3.3));
    }

    [[codegen::luawrap]] void funcVariantDoubleDvec3StringString(std::variant<double, glm::dvec3, std::string> arg) {
        REQUIRE(std::holds_alternative<std::string>(arg));
        CHECK(std::get<std::string>(arg) == "abc");
    }

    [[codegen::luawrap]] std::variant<bool, int> returnVariantBoolIntBool() {
        return true;
    }

    [[codegen::luawrap]] std::variant<bool, int> returnVariantBoolIntInt() {
        return 1;
    }

    [[codegen::luawrap]] std::variant<double, glm::dvec3, std::string> returnVariantDvec3FloatStringDouble() {
        return 1.1;
    }

    [[codegen::luawrap]] std::variant<double, glm::dvec3, std::string> returnVariantDvec3FloatStringDvec3() {
        return glm::dvec3(1.1, 2.2, 3.3);
    }

    [[codegen::luawrap]] std::variant<double, glm::dvec3, std::string> returnVariantDvec3FloatStringString() {
        return "abc";
    }
} // namespace
#include "execution_luawrapper_types_variant_codegen.cpp"


TEST_CASE(
    "Execution/LuaWrapper/Arguments:  variant(bool,int) / bool",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVariantBool;
    CHECK(func.name == "funcVariantBool");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean | Integer");
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

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  variant(bool,int) / int",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVariantInt;
    CHECK(func.name == "funcVariantInt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Boolean | Integer");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, 2);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  variant(double, vec3, string) / double",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVariantDoubleDvec3StringDouble;
    CHECK(func.name == "funcVariantDoubleDvec3StringDouble");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number | vec3 | String");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, 2.2);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  variant(double, vec3, string) / vec3",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::FuncVariantDoubleDvec3StringDvec3;
    CHECK(func.name == "funcVariantDoubleDvec3StringDvec3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number | vec3 | String");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, glm::dvec3(1.1, 2.2, 3.3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  variant(double, vec3, string) / string",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    LuaLibrary::Function func = codegen::lua::FuncVariantDoubleDvec3StringString;
    CHECK(func.name == "funcVariantDoubleDvec3StringString");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Number | vec3 | String");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, "abc"s);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  variant(bool,int) / bool",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnVariantBoolIntBool;
    CHECK(func.name == "returnVariantBoolIntBool");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Boolean | Integer");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::variant<bool, int> val = lua::value<std::variant<bool, int>>(state);
    REQUIRE(std::holds_alternative<bool>(val));
    CHECK(std::get<bool>(val) == true);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  variant(bool,int) / int",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnVariantBoolIntInt;
    CHECK(func.name == "returnVariantBoolIntInt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Boolean | Integer");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::variant<bool, int> val = lua::value<std::variant<bool, int>>(state);
    REQUIRE(std::holds_alternative<int>(val));
    CHECK(std::get<int>(val) == 1);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  variant(double, dvec3, string) / double",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnVariantDvec3FloatStringDouble;
    CHECK(func.name == "returnVariantDvec3FloatStringDouble");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number | vec3 | String");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::variant<double, glm::dvec3, std::string> val =
        lua::value<std::variant<double, glm::dvec3, std::string>>(state);
    REQUIRE(std::holds_alternative<double>(val));
    CHECK(std::get<double>(val) == 1.1);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  variant(double, dvec3, string) / dvec3",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnVariantDvec3FloatStringDvec3;
    CHECK(func.name == "returnVariantDvec3FloatStringDvec3");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number | vec3 | String");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::variant<double, glm::dvec3, std::string> val =
        lua::value<std::variant<double, glm::dvec3, std::string>>(state);
    REQUIRE(std::holds_alternative<glm::dvec3>(val));
    CHECK(std::get<glm::dvec3>(val) == glm::dvec3(1.1, 2.2, 3.3));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  variant(double, dvec3, string) / string",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnVariantDvec3FloatStringString;
    CHECK(func.name == "returnVariantDvec3FloatStringString");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Number | vec3 | String");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::variant<double, glm::dvec3, std::string> val =
        lua::value<std::variant<double, glm::dvec3, std::string>>(state);
    REQUIRE(std::holds_alternative<std::string>(val));
    CHECK(std::get<std::string>(val) == "abc");
    lua_close(state);
}
