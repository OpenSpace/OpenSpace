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
#include <optional>

using Function = openspace::LuaLibrary::Function;

namespace {
    enum class [[codegen::enum]] Enum {
        A,
        B,
        C
    };

    [[codegen::luawrap]] void funcVoid() {
    }

    [[codegen::luawrap]] void funcSingleArgument(Enum e) {
        CHECK(e == Enum::B);
    }

    [[codegen::luawrap]] void funcMultipleArguments(Enum e, Enum f) {
        CHECK(e == Enum::B);
        CHECK(f == Enum::C);
    }

    [[codegen::luawrap]] void funcOptionalMultiple(bool hasValue, Enum e,
                                                    std::optional<Enum> f)
    {
        CHECK(e == Enum::B);

        if (hasValue) {
            REQUIRE(f.has_value());
            CHECK(*f == Enum::C);
        }
        else {
            CHECK(!f.has_value());
        }
    }

    [[codegen::luawrap]] void funcVectorArguments(int n, std::vector<Enum> es) {
        assert(n == 0 || n == 1 || n == 3);

        if (n == 0) {
            CHECK(es.empty());
        }
        else if (n == 1) {
            REQUIRE(es.size() == 1);
            CHECK(es[0] == Enum::B);
        }
        else if (n == 3) {
            REQUIRE(es.size() == 3);
            CHECK(es[0] == Enum::A);
            CHECK(es[1] == Enum::B);
            CHECK(es[2] == Enum::C);
        }
    }

    [[codegen::luawrap]] void funcOptionalVectorArguments(bool hasValue,
                                                          std::optional<int> n,
                                                      std::optional<std::vector<Enum>> es)
    {
        if (hasValue) {
            REQUIRE(n.has_value());
            assert(n == 0 || n == 1 || n == 3);

            REQUIRE(es.has_value());
            if (n == 0) {
                CHECK(es->empty());
            }
            else if (n == 1) {
                REQUIRE(es->size() == 1);
                CHECK(es->at(0) == Enum::B);
            }
            else if (n == 3) {
                REQUIRE(es->size() == 3);
                CHECK(es->at(0) == Enum::A);
                CHECK(es->at(1) == Enum::B);
                CHECK(es->at(2) == Enum::C);
            }
        }
    }

    [[codegen::luawrap]] void funcMapArguments(std::map<std::string, Enum> es) {
        REQUIRE(es.size() == 3);

        std::vector<std::string> keys;
        keys.reserve(es.size());
        for (const std::pair<const std::string, Enum>& p : es) {
            keys.push_back(p.first);
        }

        CHECK(keys[0] == "first");
        CHECK(es["first"] == Enum::A);

        CHECK(keys[1] == "second");
        CHECK(es["second"] == Enum::B);

        CHECK(keys[2] == "third");
        CHECK(es["third"] == Enum::C);
    }

    [[codegen::luawrap]] Enum funcReturnValue(int v) {
        assert(v == 1 || v == 2 || v == 3);

        if (v == 1) {
            return Enum::A;
        }
        else if (v == 2) {
            return Enum::B;
        }
        else if (v == 3) {
            return Enum::C;
        }

        throw std::logic_error("Missing case label");
    }
} // namespace
#include "execution_luawrapper_arguments_enums_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments-Enums:  void", "[Execution][LuaWrapper]") {
    Function func = codegen::lua::FuncVoid;
    CHECK(func.name == "funcVoid");
    CHECK(func.arguments.empty());
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
    "Execution/LuaWrapper/Arguments-Enums:  One Parameter",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncSingleArgument;
    CHECK(func.name == "funcSingleArgument");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "e");
    CHECK(func.arguments[0].type == "Enum");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, "B");
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Enums:  Two Parameters",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncMultipleArguments;
    CHECK(func.name == "funcMultipleArguments");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "e");
    CHECK(func.arguments[0].type == "Enum");
    CHECK(func.arguments[1].name == "f");
    CHECK(func.arguments[1].type == "Enum");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, "B", "C");
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Enums:  Two Parameters w/ optional",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncOptionalMultiple;
    CHECK(func.name == "funcOptionalMultiple");
    REQUIRE(func.arguments.size() == 3);
    CHECK(func.arguments[0].name == "hasValue");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "e");
    CHECK(func.arguments[1].type == "Enum");
    CHECK(func.arguments[2].name == "f");
    CHECK(func.arguments[2].type == "Enum?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, true, "B", "C");
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, false, "B");
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Enums:  Vector Arguments",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncVectorArguments;
    CHECK(func.name == "funcVectorArguments");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "n");
    CHECK(func.arguments[0].type == "Integer");
    CHECK(func.arguments[1].name == "es");
    CHECK(func.arguments[1].type == "Enum[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    std::vector<std::string> es;

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, 0, es);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    es.emplace_back("B");
    ghoul::lua::push(state, 1, es);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    es.clear();
    es.emplace_back("A");
    es.emplace_back("B");
    es.emplace_back("C");
    ghoul::lua::push(state, 3, es);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Enums:  OptionalVector Arguments",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncOptionalVectorArguments;
    CHECK(func.name == "funcOptionalVectorArguments");
    REQUIRE(func.arguments.size() == 3);
    CHECK(func.arguments[0].name == "hasValue");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "n");
    CHECK(func.arguments[1].type == "Integer?");
    CHECK(func.arguments[2].name == "es");
    CHECK(func.arguments[2].type == "Enum[]?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    std::vector<std::string> es;

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, true, 0, es);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    es.emplace_back("B");
    ghoul::lua::push(state, true, 1, es);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    es.clear();
    es.emplace_back("A");
    es.emplace_back("B");
    es.emplace_back("C");
    ghoul::lua::push(state, true, 3, es);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Enums:  Map Arguments",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncMapArguments;
    CHECK(func.name == "funcMapArguments");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "es");
    CHECK(func.arguments[0].type == "String -> Enum");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::Dictionary es;
    es.setValue("first", "A"s);
    es.setValue("second", "B"s);
    es.setValue("third", "C"s);

    ghoul::lua::push(state, es);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Enums:  Return value",
    "[Execution][LuaWrapper]"
)
{
    Function func = codegen::lua::FuncReturnValue;
    CHECK(func.name == "funcReturnValue");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "v");
    CHECK(func.arguments[0].type == "Integer");
    CHECK(func.returnType == "Enum");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, 1);
    func.function(state);
    CHECK(lua_gettop(state) == 1);
    CHECK(ghoul::lua::value<std::string>(state) == "A");

    ghoul::lua::push(state, 2);
    func.function(state);
    CHECK(lua_gettop(state) == 1);
    CHECK(ghoul::lua::value<std::string>(state) == "B");

    ghoul::lua::push(state, 3);
    func.function(state);
    CHECK(lua_gettop(state) == 1);
    CHECK(ghoul::lua::value<std::string>(state) == "C");
    lua_close(state);
}
