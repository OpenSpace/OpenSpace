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
    [[codegen::luawrap]] void funcDictionary(ghoul::Dictionary arg) {
        REQUIRE(arg.hasValue<double>("a"));
        CHECK(arg.value<double>("a") == 1.1);
        REQUIRE(arg.hasValue<std::string>("b"));
        CHECK(arg.value<std::string>("b") == "abc");
        REQUIRE(arg.hasValue<glm::dvec3>("c"));
        CHECK(arg.value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDictionaryDefaulted(ghoul::Dictionary arg = ghoul::Dictionary()) {
        CHECK(arg == ghoul::Dictionary());
    }

    [[codegen::luawrap]] void funcDictionaryDefaultedCheck(bool isDefaulted, ghoul::Dictionary arg = ghoul::Dictionary()) {
        if (isDefaulted) {
            CHECK(arg == ghoul::Dictionary());
        }
        else {
            REQUIRE(arg.hasValue<double>("a"));
            CHECK(arg.value<double>("a") == 1.1);
            REQUIRE(arg.hasValue<std::string>("b"));
            CHECK(arg.value<std::string>("b") == "abc");
            REQUIRE(arg.hasValue<glm::dvec3>("c"));
            CHECK(arg.value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
        }
    }

    [[codegen::luawrap]] void funcDictionaryMap(std::map<std::string, ghoul::Dictionary> arg) {
        CHECK(arg.size() == 3);
        {
            auto it = arg.find("key1");
            REQUIRE(it != arg.end());
            CHECK(it->second.hasValue<double>("a"));
            CHECK(it->second.value<double>("a") == 1.1);
            REQUIRE(it->second.hasValue<std::string>("b"));
            CHECK(it->second.value<std::string>("b") == "abc");
            REQUIRE(it->second.hasValue<glm::dvec3>("c"));
            CHECK(it->second.value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
        }
        {
            auto it = arg.find("key2");
            REQUIRE(it != arg.end());
            CHECK(it->second.hasValue<double>("a"));
            CHECK(it->second.value<double>("a") == 5.5);
            REQUIRE(it->second.hasValue<std::string>("b"));
            CHECK(it->second.value<std::string>("b") == "def");
            REQUIRE(it->second.hasValue<glm::dvec3>("c"));
            CHECK(it->second.value<glm::dvec3>("c") == glm::dvec3(6.6, 7.7, 8.8));
        }
        {
            auto it = arg.find("key3");
            REQUIRE(it != arg.end());
            CHECK(it->second.hasValue<double>("a"));
            CHECK(it->second.value<double>("a") == 9.9);
            REQUIRE(it->second.hasValue<std::string>("b"));
            CHECK(it->second.value<std::string>("b") == "ghi");
            REQUIRE(it->second.hasValue<glm::dvec3>("c"));
            CHECK(it->second.value<glm::dvec3>("c") == glm::dvec3(10.10, 11.11, 12.12));
        }
    }

    [[codegen::luawrap]] void funcDictionaryOptional(std::optional<ghoul::Dictionary> arg) {
        REQUIRE(arg.has_value());
        REQUIRE(arg->hasValue<double>("a"));
        CHECK(arg->value<double>("a") == 1.1);
        REQUIRE(arg->hasValue<std::string>("b"));
        CHECK(arg->value<std::string>("b") == "abc");
        REQUIRE(arg->hasValue<glm::dvec3>("c"));
        CHECK(arg->value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDictionaryOptionalNullopt(std::optional<ghoul::Dictionary> arg) {
        CHECK(!arg.has_value());
    }

    [[codegen::luawrap]] void funcDictionaryVector(std::vector<ghoul::Dictionary> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0].hasValue<double>("a"));
        CHECK(arg[0].value<double>("a") == 1.1);
        REQUIRE(arg[0].hasValue<std::string>("b"));
        CHECK(arg[0].value<std::string>("b") == "abc");
        REQUIRE(arg[0].hasValue<glm::dvec3>("c"));
        CHECK(arg[0].value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
        CHECK(arg[1].hasValue<double>("a"));
        CHECK(arg[1].value<double>("a") == 5.5);
        REQUIRE(arg[1].hasValue<std::string>("b"));
        CHECK(arg[1].value<std::string>("b") == "def");
        REQUIRE(arg[1].hasValue<glm::dvec3>("c"));
        CHECK(arg[1].value<glm::dvec3>("c") == glm::dvec3(6.6, 7.7, 8.8));
        CHECK(arg[2].hasValue<double>("a"));
        CHECK(arg[2].value<double>("a") == 9.9);
        REQUIRE(arg[2].hasValue<std::string>("b"));
        CHECK(arg[2].value<std::string>("b") == "ghi");
        REQUIRE(arg[2].hasValue<glm::dvec3>("c"));
        CHECK(arg[2].value<glm::dvec3>("c") == glm::dvec3(10.10, 11.11, 12.12));
    }

    [[codegen::luawrap]] void funcDictionaryArray1(std::array<ghoul::Dictionary, 1> arg) {
        CHECK(arg[0].hasValue<double>("a"));
        CHECK(arg[0].value<double>("a") == 1.1);
        REQUIRE(arg[0].hasValue<std::string>("b"));
        CHECK(arg[0].value<std::string>("b") == "abc");
        REQUIRE(arg[0].hasValue<glm::dvec3>("c"));
        CHECK(arg[0].value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
    }

    [[codegen::luawrap]] void funcDictionaryArray2(std::array<ghoul::Dictionary, 5> arg) {
        CHECK(arg[0].hasValue<double>("a"));
        CHECK(arg[0].value<double>("a") == 1.1);
        REQUIRE(arg[0].hasValue<std::string>("b"));
        CHECK(arg[0].value<std::string>("b") == "abc");
        REQUIRE(arg[0].hasValue<glm::dvec3>("c"));
        CHECK(arg[0].value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
        CHECK(arg[1].hasValue<double>("a"));
        CHECK(arg[1].value<double>("a") == 5.5);
        REQUIRE(arg[1].hasValue<std::string>("b"));
        CHECK(arg[1].value<std::string>("b") == "def");
        REQUIRE(arg[1].hasValue<glm::dvec3>("c"));
        CHECK(arg[1].value<glm::dvec3>("c") == glm::dvec3(6.6, 7.7, 8.8));
        CHECK(arg[2].hasValue<double>("a"));
        CHECK(arg[2].value<double>("a") == 9.9);
        REQUIRE(arg[2].hasValue<std::string>("b"));
        CHECK(arg[2].value<std::string>("b") == "ghi");
        REQUIRE(arg[2].hasValue<glm::dvec3>("c"));
        CHECK(arg[2].value<glm::dvec3>("c") == glm::dvec3(10.10, 11.11, 12.12));
        CHECK(arg[3].hasValue<double>("a"));
        CHECK(arg[3].value<double>("a") == 13.13);
        REQUIRE(arg[3].hasValue<std::string>("b"));
        CHECK(arg[3].value<std::string>("b") == "jkl");
        REQUIRE(arg[3].hasValue<glm::dvec3>("c"));
        CHECK(arg[3].value<glm::dvec3>("c") == glm::dvec3(14.14, 15.15, 16.16));
        CHECK(arg[4].hasValue<double>("a"));
        CHECK(arg[4].value<double>("a") == 17.17);
        REQUIRE(arg[4].hasValue<std::string>("b"));
        CHECK(arg[4].value<std::string>("b") == "mno");
        REQUIRE(arg[4].hasValue<glm::dvec3>("c"));
        CHECK(arg[4].value<glm::dvec3>("c") == glm::dvec3(18.18, 19.19, 20.20));
    }

    [[codegen::luawrap]] void funcDictionaryArray3(std::array<ghoul::Dictionary, 10> arg)
    {
        CHECK(arg[0].hasValue<double>("a"));
        CHECK(arg[0].value<double>("a") == 1.1);
        REQUIRE(arg[0].hasValue<std::string>("b"));
        CHECK(arg[0].value<std::string>("b") == "abc");
        REQUIRE(arg[0].hasValue<glm::dvec3>("c"));
        CHECK(arg[0].value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
        
        CHECK(arg[1].hasValue<double>("a"));
        CHECK(arg[1].value<double>("a") == 5.5);
        REQUIRE(arg[1].hasValue<std::string>("b"));
        CHECK(arg[1].value<std::string>("b") == "def");
        REQUIRE(arg[1].hasValue<glm::dvec3>("c"));
        CHECK(arg[1].value<glm::dvec3>("c") == glm::dvec3(6.6, 7.7, 8.8));
        
        CHECK(arg[2].hasValue<double>("a"));
        CHECK(arg[2].value<double>("a") == 9.9);
        REQUIRE(arg[2].hasValue<std::string>("b"));
        CHECK(arg[2].value<std::string>("b") == "ghi");
        REQUIRE(arg[2].hasValue<glm::dvec3>("c"));
        CHECK(arg[2].value<glm::dvec3>("c") == glm::dvec3(10.10, 11.11, 12.12));
        
        CHECK(arg[3].hasValue<double>("a"));
        CHECK(arg[3].value<double>("a") == 13.13);
        REQUIRE(arg[3].hasValue<std::string>("b"));
        CHECK(arg[3].value<std::string>("b") == "jkl");
        REQUIRE(arg[3].hasValue<glm::dvec3>("c"));
        CHECK(arg[3].value<glm::dvec3>("c") == glm::dvec3(14.14, 15.15, 16.16));
        
        CHECK(arg[4].hasValue<double>("a"));
        CHECK(arg[4].value<double>("a") == 17.17);
        REQUIRE(arg[4].hasValue<std::string>("b"));
        CHECK(arg[4].value<std::string>("b") == "mno");
        REQUIRE(arg[4].hasValue<glm::dvec3>("c"));
        CHECK(arg[4].value<glm::dvec3>("c") == glm::dvec3(18.18, 19.19, 20.20));
        
        CHECK(arg[5].hasValue<double>("a"));
        CHECK(arg[5].value<double>("a") == 21.21);
        REQUIRE(arg[5].hasValue<std::string>("b"));
        CHECK(arg[5].value<std::string>("b") == "pqr");
        REQUIRE(arg[5].hasValue<glm::dvec3>("c"));
        CHECK(arg[5].value<glm::dvec3>("c") == glm::dvec3(22.22, 23.23, 24.24));
        
        CHECK(arg[6].hasValue<double>("a"));
        CHECK(arg[6].value<double>("a") == 25.25);
        REQUIRE(arg[6].hasValue<std::string>("b"));
        CHECK(arg[6].value<std::string>("b") == "stu");
        REQUIRE(arg[6].hasValue<glm::dvec3>("c"));
        CHECK(arg[6].value<glm::dvec3>("c") == glm::dvec3(26.26, 27.27, 28.28));
        
        CHECK(arg[7].hasValue<double>("a"));
        CHECK(arg[7].value<double>("a") == 29.29);
        REQUIRE(arg[7].hasValue<std::string>("b"));
        CHECK(arg[7].value<std::string>("b") == "vwx");
        REQUIRE(arg[7].hasValue<glm::dvec3>("c"));
        CHECK(arg[7].value<glm::dvec3>("c") == glm::dvec3(30.30, 31.31, 32.32));
        
        CHECK(arg[8].hasValue<double>("a"));
        CHECK(arg[8].value<double>("a") == 33.33);
        REQUIRE(arg[8].hasValue<std::string>("b"));
        CHECK(arg[8].value<std::string>("b") == "yzz");
        REQUIRE(arg[8].hasValue<glm::dvec3>("c"));
        CHECK(arg[8].value<glm::dvec3>("c") == glm::dvec3(34.34, 35.35, 36.36));
        
        CHECK(arg[9].hasValue<double>("a"));
        CHECK(arg[9].value<double>("a") == 37.37);
        REQUIRE(arg[9].hasValue<std::string>("b"));
        CHECK(arg[9].value<std::string>("b") == "ABC");
        REQUIRE(arg[9].hasValue<glm::dvec3>("c"));
        CHECK(arg[9].value<glm::dvec3>("c") == glm::dvec3(38.38, 39.39, 40.40));
    }

    [[codegen::luawrap]] ghoul::Dictionary returnDictionary() {
        using namespace std::string_literals;

        ghoul::Dictionary d;
        d.setValue("a", 1.1);
        d.setValue("b", "abc"s);
        d.setValue("c", glm::dvec3(2.2, 3.3, 4.4));
        return d;
    }

    [[codegen::luawrap]] std::map<std::string, ghoul::Dictionary> returnDictionaryMap() {
        using namespace std::string_literals;

        ghoul::Dictionary d1;
        d1.setValue("a", 1.1);
        d1.setValue("b", "abc"s);
        d1.setValue("c", glm::dvec3(2.2, 3.3, 4.4));

        ghoul::Dictionary d2;
        d2.setValue("a", 5.5);
        d2.setValue("b", "def"s);
        d2.setValue("c", glm::dvec3(6.6, 7.7, 8.8));

        ghoul::Dictionary d3;
        d3.setValue("a", 9.9);
        d3.setValue("b", "ghi"s);
        d3.setValue("c", glm::dvec3(10.10, 11.11, 12.12));

        return {
            { "key1", d1 },
            { "key2", d2 },
            { "key3", d3 }
        };
    }

    [[codegen::luawrap]] std::optional<ghoul::Dictionary> returnDictionaryOptional() {
        using namespace std::string_literals;

        ghoul::Dictionary d;
        d.setValue("a", 1.1);
        d.setValue("b", "abc"s);
        d.setValue("c", glm::dvec3(2.2, 3.3, 4.4));
        return d;
    }

    [[codegen::luawrap]] std::optional<ghoul::Dictionary> returnDictionaryOptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::vector<ghoul::Dictionary> returnDictionaryVector() {
        using namespace std::string_literals;

        ghoul::Dictionary d1;
        d1.setValue("a", 1.1);
        d1.setValue("b", "abc"s);
        d1.setValue("c", glm::dvec3(2.2, 3.3, 4.4));

        ghoul::Dictionary d2;
        d2.setValue("a", 5.5);
        d2.setValue("b", "def"s);
        d2.setValue("c", glm::dvec3(6.6, 7.7, 8.8));

        ghoul::Dictionary d3;
        d3.setValue("a", 9.9);
        d3.setValue("b", "ghi"s);
        d3.setValue("c", glm::dvec3(10.10, 11.11, 12.12));

        return { d1, d2, d3 };
    }
} // namespace
#include "execution_luawrapper_types_dictionary_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  dictionary", "[Execution][LuaWrapper]") {
    using namespace std::string_literals;

    Function func = codegen::lua::FuncDictionary;
    CHECK(func.name == "funcDictionary");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Table");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 1.1);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "abc"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dictionaryDefaulted with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDictionaryDefaulted;
    CHECK(func.name == "funcDictionaryDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Table?");
    CHECK(func.arguments[0].defaultValue == "ghoul::Dictionary()");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dictionaryDefaulted w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDictionaryDefaulted;
    CHECK(func.name == "funcDictionaryDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Table?");
    CHECK(func.arguments[0].defaultValue == "ghoul::Dictionary()");
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
    "Execution/LuaWrapper/Arguments:  dictionaryDefaultedCheck with value",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncDictionaryDefaultedCheck;
    CHECK(func.name == "funcDictionaryDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Table?");
    CHECK(func.arguments[1].defaultValue == "ghoul::Dictionary()");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, false);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 1.1);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "abc"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dictionaryDefaultedCheck w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDictionaryDefaultedCheck;
    CHECK(func.name == "funcDictionaryDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Table?");
    CHECK(func.arguments[1].defaultValue == "ghoul::Dictionary()");
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

TEST_CASE("Execution/LuaWrapper/Arguments:  dictionaryMap", "[Execution][LuaWrapper]") {
    using namespace std::string_literals;

    Function func = codegen::lua::FuncDictionaryMap;
    CHECK(func.name == "funcDictionaryMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> Table");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    {
        ghoul::lua::push(state, "key1");
        lua_newtable(state);
        ghoul::lua::push(state, "a"s, 1.1);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "b"s, "abc"s);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "c"s, glm::dvec3(2.2, 3.3, 4.4));
        lua_rawset(state, -3);
    }
    lua_rawset(state, -3);
    {
        ghoul::lua::push(state, "key2");
        lua_newtable(state);
        ghoul::lua::push(state, "a"s, 5.5);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "b"s, "def"s);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "c"s, glm::dvec3(6.6, 7.7, 8.8));
        lua_rawset(state, -3);
    }
    lua_rawset(state, -3);
    {
        ghoul::lua::push(state, "key3");
        lua_newtable(state);
        ghoul::lua::push(state, "a"s, 9.9);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "b"s, "ghi"s);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "c"s, glm::dvec3(10.10, 11.11, 12.12));
        lua_rawset(state, -3);
    }
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dictionaryOptional",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncDictionaryOptional;
    CHECK(func.name == "funcDictionaryOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Table?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    {
        lua_newtable(state);
        ghoul::lua::push(state, "a"s, 1.1);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "b"s, "abc"s);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "c"s, glm::dvec3(2.2, 3.3, 4.4));
        lua_rawset(state, -3);
    }
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  dictionaryOptionalNullopt",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncDictionaryOptionalNullopt;
    CHECK(func.name == "funcDictionaryOptionalNullopt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Table?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dictionaryVector", "[Execution][LuaWrapper]")
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncDictionaryVector;
    CHECK(func.name == "funcDictionaryVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Table[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    {
        ghoul::lua::push(state, 1);
        lua_newtable(state);
        ghoul::lua::push(state, "a"s, 1.1);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "b"s, "abc"s);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "c"s, glm::dvec3(2.2, 3.3, 4.4));
        lua_rawset(state, -3);
    }
    lua_rawset(state, -3);
    {
        ghoul::lua::push(state, 2);
        lua_newtable(state);
        ghoul::lua::push(state, "a"s, 5.5);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "b"s, "def"s);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "c"s, glm::dvec3(6.6, 7.7, 8.8));
        lua_rawset(state, -3);
    }
    lua_rawset(state, -3);
    {
        ghoul::lua::push(state, 3);
        lua_newtable(state);
        ghoul::lua::push(state, "a"s, 9.9);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "b"s, "ghi"s);
        lua_rawset(state, -3);
        ghoul::lua::push(state, "c"s, glm::dvec3(10.10, 11.11, 12.12));
        lua_rawset(state, -3);
    }
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dictionaryArray1", "[Execution][LuaWrapper]")
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncDictionaryArray1;
    CHECK(func.name == "funcDictionaryArray1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Table[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);
    ghoul::lua::push(state, 1);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 1.1);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "abc"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua_rawset(state, -3);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::Dictionary d1;
    d1.setValue("a", 1.1);
    d1.setValue("b", "abc"s);
    d1.setValue("c", glm::dvec3(2.2, 3.3, 4.4));
    ghoul::lua::push(state, std::array<ghoul::Dictionary, 1> { d1 });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dictionaryArray2", "[Execution][LuaWrapper]")
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncDictionaryArray2;
    CHECK(func.name == "funcDictionaryArray2");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Table[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);

    ghoul::lua::push(state, 1);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 1.1);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "abc"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 2);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 5.5);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "def"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 3);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 9.9);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "ghi"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 4);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 13.13);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "jkl"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(14.14, 15.15, 16.16));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 5);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 17.17);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "mno"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(18.18, 19.19, 20.20));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    func.function(state);
    CHECK(lua_gettop(state) == 0);


    ghoul::Dictionary d1;
    d1.setValue("a", 1.1);
    d1.setValue("b", "abc"s);
    d1.setValue("c", glm::dvec3(2.2, 3.3, 4.4));
    ghoul::Dictionary d2;
    d2.setValue("a", 5.5);
    d2.setValue("b", "def"s);
    d2.setValue("c", glm::dvec3(6.6, 7.7, 8.8));
    ghoul::Dictionary d3;
    d3.setValue("a", 9.9);
    d3.setValue("b", "ghi"s);
    d3.setValue("c", glm::dvec3(10.10, 11.11, 12.12));
    ghoul::Dictionary d4;
    d4.setValue("a", 13.13);
    d4.setValue("b", "jkl"s);
    d4.setValue("c", glm::dvec3(14.14, 15.15, 16.16));
    ghoul::Dictionary d5;
    d5.setValue("a", 17.17);
    d5.setValue("b", "mno"s);
    d5.setValue("c", glm::dvec3(18.18, 19.19, 20.20));
    ghoul::lua::push(state, std::array<ghoul::Dictionary, 5> { d1, d2, d3, d4, d5 });
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  dictionaryArray3", "[Execution][LuaWrapper]")
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncDictionaryArray3;
    CHECK(func.name == "funcDictionaryArray3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Table[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua_newtable(state);

    ghoul::lua::push(state, 1);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 1.1);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "abc"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(2.2, 3.3, 4.4));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 2);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 5.5);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "def"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(6.6, 7.7, 8.8));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 3);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 9.9);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "ghi"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(10.10, 11.11, 12.12));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 4);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 13.13);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "jkl"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(14.14, 15.15, 16.16));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 5);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 17.17);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "mno"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(18.18, 19.19, 20.20));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 6);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 21.21);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "pqr"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(22.22, 23.23, 24.24));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 7);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 25.25);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "stu"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(26.26, 27.27, 28.28));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 8);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 29.29);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "vwx"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(30.30, 31.31, 32.32));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 9);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 33.33);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "yzz"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(34.34, 35.35, 36.36));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    ghoul::lua::push(state, 10);
    lua_newtable(state);
    ghoul::lua::push(state, "a"s, 37.37);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "b"s, "ABC"s);
    lua_rawset(state, -3);
    ghoul::lua::push(state, "c"s, glm::dvec3(38.38, 39.39, 40.40));
    lua_rawset(state, -3);
    lua_rawset(state, -3);

    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::Dictionary d1;
    d1.setValue("a", 1.1);
    d1.setValue("b", "abc"s);
    d1.setValue("c", glm::dvec3(2.2, 3.3, 4.4));
    ghoul::Dictionary d2;
    d2.setValue("a", 5.5);
    d2.setValue("b", "def"s);
    d2.setValue("c", glm::dvec3(6.6, 7.7, 8.8));
    ghoul::Dictionary d3;
    d3.setValue("a", 9.9);
    d3.setValue("b", "ghi"s);
    d3.setValue("c", glm::dvec3(10.10, 11.11, 12.12));
    ghoul::Dictionary d4;
    d4.setValue("a", 13.13);
    d4.setValue("b", "jkl"s);
    d4.setValue("c", glm::dvec3(14.14, 15.15, 16.16));
    ghoul::Dictionary d5;
    d5.setValue("a", 17.17);
    d5.setValue("b", "mno"s);
    d5.setValue("c", glm::dvec3(18.18, 19.19, 20.20));
    ghoul::Dictionary d6;
    d6.setValue("a", 21.21);
    d6.setValue("b", "pqr"s);
    d6.setValue("c", glm::dvec3(22.22, 23.23, 24.24));
    ghoul::Dictionary d7;
    d7.setValue("a", 25.25);
    d7.setValue("b", "stu"s);
    d7.setValue("c", glm::dvec3(26.26, 27.27, 28.28));
    ghoul::Dictionary d8;
    d8.setValue("a", 29.29);
    d8.setValue("b", "vwx"s);
    d8.setValue("c", glm::dvec3(30.30, 31.31, 32.32));
    ghoul::Dictionary d9;
    d9.setValue("a", 33.33);
    d9.setValue("b", "yzz"s);
    d9.setValue("c", glm::dvec3(34.34, 35.35, 36.36));
    ghoul::Dictionary d10;
    d10.setValue("a", 37.37);
    d10.setValue("b", "ABC"s);
    d10.setValue("c", glm::dvec3(38.38, 39.39, 40.40));
    ghoul::lua::push(
        state,
        std::array<ghoul::Dictionary, 10> { d1, d2, d3, d4, d5, d6, d7, d8, d9, d10 }
    );
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dictionary", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDictionary;
    CHECK(func.name == "returnDictionary");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Table");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const ghoul::Dictionary val = ghoul::lua::value<ghoul::Dictionary>(state);
    REQUIRE(val.hasValue<double>("a"));
    CHECK(val.value<double>("a") == 1.1);
    REQUIRE(val.hasValue<std::string>("b"));
    CHECK(val.value<std::string>("b") == "abc");
    REQUIRE(val.hasValue<glm::dvec3>("c"));
    CHECK(val.value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dictionaryMap", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDictionaryMap;
    CHECK(func.name == "returnDictionaryMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> Table");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, ghoul::Dictionary> val =
        ghoul::lua::value<std::map<std::string, ghoul::Dictionary>>(state);
    CHECK(val.size() == 3);
    {
        auto it = val.find("key1");
        REQUIRE(it != val.end());
        CHECK(it->second.hasValue<double>("a"));
        CHECK(it->second.value<double>("a") == 1.1);
        REQUIRE(it->second.hasValue<std::string>("b"));
        CHECK(it->second.value<std::string>("b") == "abc");
        REQUIRE(it->second.hasValue<glm::dvec3>("c"));
        CHECK(it->second.value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
    }
    {
        auto it = val.find("key2");
        REQUIRE(it != val.end());
        CHECK(it->second.hasValue<double>("a"));
        CHECK(it->second.value<double>("a") == 5.5);
        REQUIRE(it->second.hasValue<std::string>("b"));
        CHECK(it->second.value<std::string>("b") == "def");
        REQUIRE(it->second.hasValue<glm::dvec3>("c"));
        CHECK(it->second.value<glm::dvec3>("c") == glm::dvec3(6.6, 7.7, 8.8));
    }
    {
        auto it = val.find("key3");
        REQUIRE(it != val.end());
        CHECK(it->second.hasValue<double>("a"));
        CHECK(it->second.value<double>("a") == 9.9);
        REQUIRE(it->second.hasValue<std::string>("b"));
        CHECK(it->second.value<std::string>("b") == "ghi");
        REQUIRE(it->second.hasValue<glm::dvec3>("c"));
        CHECK(it->second.value<glm::dvec3>("c") == glm::dvec3(10.10, 11.11, 12.12));
    }
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dictionaryOptional with value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDictionaryOptional;
    CHECK(func.name == "returnDictionaryOptional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Table?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const ghoul::Dictionary val = ghoul::lua::value<ghoul::Dictionary>(state);
    REQUIRE(val.hasValue<double>("a"));
    CHECK(val.value<double>("a") == 1.1);
    REQUIRE(val.hasValue<std::string>("b"));
    CHECK(val.value<std::string>("b") == "abc");
    REQUIRE(val.hasValue<glm::dvec3>("c"));
    CHECK(val.value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  dictionaryOptional w/o value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::ReturnDictionaryOptionalNullopt;
    CHECK(func.name == "returnDictionaryOptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Table?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  dictionaryVector", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::ReturnDictionaryVector;
    CHECK(func.name == "returnDictionaryVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Table[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<ghoul::Dictionary> val =
        ghoul::lua::value<std::vector<ghoul::Dictionary>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0].hasValue<double>("a"));
    CHECK(val[0].value<double>("a") == 1.1);
    REQUIRE(val[0].hasValue<std::string>("b"));
    CHECK(val[0].value<std::string>("b") == "abc");
    REQUIRE(val[0].hasValue<glm::dvec3>("c"));
    CHECK(val[0].value<glm::dvec3>("c") == glm::dvec3(2.2, 3.3, 4.4));
    CHECK(val[1].hasValue<double>("a"));
    CHECK(val[1].value<double>("a") == 5.5);
    REQUIRE(val[1].hasValue<std::string>("b"));
    CHECK(val[1].value<std::string>("b") == "def");
    REQUIRE(val[1].hasValue<glm::dvec3>("c"));
    CHECK(val[1].value<glm::dvec3>("c") == glm::dvec3(6.6, 7.7, 8.8));
    CHECK(val[2].hasValue<double>("a"));
    CHECK(val[2].value<double>("a") == 9.9);
    REQUIRE(val[2].hasValue<std::string>("b"));
    CHECK(val[2].value<std::string>("b") == "ghi");
    REQUIRE(val[2].hasValue<glm::dvec3>("c"));
    CHECK(val[2].value<glm::dvec3>("c") == glm::dvec3(10.10, 11.11, 12.12));
    lua_close(state);
}
