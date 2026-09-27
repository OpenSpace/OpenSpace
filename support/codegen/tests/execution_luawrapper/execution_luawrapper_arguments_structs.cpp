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
#include <catch2/catch_approx.hpp>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/lua/lua_helper.h>
#include <openspace/misc/dictionary.h>
#include <openspace/scripting/lualibrary.h>
#include <optional>

using Function = openspace::LuaLibrary::Function;

namespace {
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void funcVoid() {
    }

    [[codegen::luawrap]] void funcSingleArgument(Parameter p) {
        CHECK(p.a == 1);
        CHECK(p.b == 2.2f);
        CHECK(p.c == "3.3");
    }

    [[codegen::luawrap]] void funcOptionalArgument(bool hasValue,
                                                   std::optional<Parameter> p)
    {
        if (hasValue) {
            REQUIRE(p.has_value());
            CHECK(p->a == 1);
            CHECK(p->b == 2.2f);
            CHECK(p->c == "3.3");
        }
        else {
            CHECK(!p.has_value());
        }
    }

    [[codegen::luawrap]] void funcMultipleArguments(Parameter p, Parameter q) {
        CHECK(p.a == 1);
        CHECK(p.b == 2.2f);
        CHECK(p.c == "3.3");

        CHECK(q.a == 4);
        CHECK(q.b == 5.5f);
        CHECK(q.c == "6.6");
    }

    [[codegen::luawrap]] void funcOptionalMultiple(bool hasValue, Parameter p,
                                                   std::optional<Parameter> q)
    {
        CHECK(p.a == 1);
        CHECK(p.b == 2.2f);
        CHECK(p.c == "3.3");

        if (hasValue) {
            REQUIRE(q.has_value());
            CHECK(q->a == 4);
            CHECK(q->b == 5.5f);
            CHECK(q->c == "6.6");
        }
        else {
            CHECK(!q.has_value());
        }
    }

    [[codegen::luawrap]] void funcVectorArguments(int n, std::vector<Parameter> ps) {
        assert(n == 0 || n == 1 || n == 3);

        if (n == 0) {
            CHECK(ps.empty());
        }
        else if (n == 1) {
            REQUIRE(ps.size() == 1);
            CHECK(ps[0].a == 1);
            CHECK(ps[0].b == 2.2f);
            CHECK(ps[0].c == "3.3");
        }
        else if (n == 3) {
            REQUIRE(ps.size() == 3);
            CHECK(ps[0].a == 1);
            CHECK(ps[0].b == 2.2f);
            CHECK(ps[0].c == "3.3");
            CHECK(ps[1].a == 4);
            CHECK(ps[1].b == 5.5f);
            CHECK(ps[1].c == "6.6");
            CHECK(ps[2].a == 7);
            CHECK(ps[2].b == 8.8f);
            CHECK(ps[2].c == "9.9");
        }
    }

    [[codegen::luawrap]] void funcOptionalVectorArguments(bool hasValue,
                                                          std::optional<int> n,
                                                 std::optional<std::vector<Parameter>> ps)
    {
        if (hasValue) {
            REQUIRE(n.has_value());
            assert(n == 0 || n == 1 || n == 3);

            REQUIRE(ps.has_value());
            if (n == 0) {
                CHECK(ps->empty());
            }
            else if (n == 1) {
                REQUIRE(ps->size() == 1);
                CHECK(ps->at(0).a == 1);
                CHECK(ps->at(0).b == 2.2f);
                CHECK(ps->at(0).c == "3.3");
            }
            else if (n == 3) {
                REQUIRE(ps->size() == 3);
                CHECK(ps->at(0).a == 1);
                CHECK(ps->at(0).b == 2.2f);
                CHECK(ps->at(0).c == "3.3");
                CHECK(ps->at(1).a == 4);
                CHECK(ps->at(1).b == 5.5f);
                CHECK(ps->at(1).c == "6.6");
                CHECK(ps->at(2).a == 7);
                CHECK(ps->at(2).b == 8.8f);
                CHECK(ps->at(2).c == "9.9");
            }
        }
        else {
            CHECK(!n.has_value());
            CHECK(!ps.has_value());
        }
    }

    [[codegen::luawrap]] void funcMapArguments(std::map<std::string, Parameter> ps) {
        REQUIRE(ps.size() == 3);

        std::vector<std::string> keys;
        keys.reserve(ps.size());
        for (const std::pair<const std::string, Parameter>& p : ps) {
            keys.push_back(p.first);
        }

        CHECK(keys[0] == "first");
        CHECK(ps["first"].a == 1);
        CHECK(ps["first"].b == Catch::Approx(2.2));
        CHECK(ps["first"].c == "3.3");

        CHECK(keys[1] == "second");
        CHECK(ps["second"].a == 4);
        CHECK(ps["second"].b == Catch::Approx(5.5));
        CHECK(ps["second"].c == "6.6");

        CHECK(keys[2] == "third");
        CHECK(ps["third"].a == 7);
        CHECK(ps["third"].b == Catch::Approx(8.8));
        CHECK(ps["third"].c == "9.9");
    }

    [[codegen::luawrap]] Parameter funcReturnValue(int a, float b, std::string c) {
        Parameter p = {
            .a = a,
            .b = b,
            .c = std::move(c)
        };
        return p;
    }
} // namespace
#include "execution_luawrapper_arguments_structs_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments-Structs:  void", "[Execution][LuaWrapper]") {
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
    "Execution/LuaWrapper/Arguments-Structs:  One Parameter",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncSingleArgument;
    CHECK(func.name == "funcSingleArgument");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "p");
    CHECK(func.arguments[0].type == "Parameter");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::Dictionary p;
    p.setValue("A", 1);
    p.setValue("B", 2.2);
    p.setValue("C", "3.3"s);
    ghoul::lua::push(state, p);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Structs:  One optional Parameter",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncOptionalArgument;
    CHECK(func.name == "funcOptionalArgument");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "hasValue");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "p");
    CHECK(func.arguments[1].type == "Parameter?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::Dictionary p;
    p.setValue("A", 1);
    p.setValue("B", 2.2);
    p.setValue("C", "3.3"s);
    ghoul::lua::push(state, true, p);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, false);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Structs:  Two Parameters",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncMultipleArguments;
    CHECK(func.name == "funcMultipleArguments");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "p");
    CHECK(func.arguments[0].type == "Parameter");
    CHECK(func.arguments[1].name == "q");
    CHECK(func.arguments[1].type == "Parameter");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::Dictionary p;
    p.setValue("A", 1);
    p.setValue("B", 2.2);
    p.setValue("C", "3.3"s);
    ghoul::Dictionary q;
    q.setValue("A", 4);
    q.setValue("B", 5.5);
    q.setValue("C", "6.6"s);
    ghoul::lua::push(state, p, q);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments-Structs:  Two Parameters w/optional") {
    using namespace std::string_literals;

    Function func = codegen::lua::FuncOptionalMultiple;
    CHECK(func.name == "funcOptionalMultiple");
    REQUIRE(func.arguments.size() == 3);
    CHECK(func.arguments[0].name == "hasValue");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "p");
    CHECK(func.arguments[1].type == "Parameter");
    CHECK(func.arguments[2].name == "q");
    CHECK(func.arguments[2].type == "Parameter?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::Dictionary p;
    p.setValue("A", 1);
    p.setValue("B", 2.2);
    p.setValue("C", "3.3"s);
    ghoul::Dictionary q;
    q.setValue("A", 4);
    q.setValue("B", 5.5);
    q.setValue("C", "6.6"s);
    ghoul::lua::push(state, true, p, q);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, false, p);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Structs:  Vector Arguments",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncVectorArguments;
    CHECK(func.name == "funcVectorArguments");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "n");
    CHECK(func.arguments[0].type == "Integer");
    CHECK(func.arguments[1].name == "ps");
    CHECK(func.arguments[1].type == "Parameter[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    std::vector<ghoul::Dictionary> ps;
    ghoul::Dictionary p;
    p.setValue("A", 1);
    p.setValue("B", 2.2);
    p.setValue("C", "3.3"s);
    ps.push_back(p);

    ghoul::Dictionary p2;
    p2.setValue("A", 4);
    p2.setValue("B", 5.5);
    p2.setValue("C", "6.6"s);
    ps.push_back(p2);

    ghoul::Dictionary p3;
    p3.setValue("A", 7);
    p3.setValue("B", 8.8);
    p3.setValue("C", "9.9"s);
    ps.push_back(p3);
    ghoul::lua::push(state, 3, ps);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ps.clear();
    ps.push_back(p);
    ghoul::lua::push(state, 1, ps);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ps.clear();
    ghoul::lua::push(state, 0, ps);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Structs:  OptionalVector Arguments",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncOptionalVectorArguments;
    CHECK(func.name == "funcOptionalVectorArguments");
    REQUIRE(func.arguments.size() == 3);
    CHECK(func.arguments[0].name == "hasValue");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "n");
    CHECK(func.arguments[1].type == "Integer?");
    CHECK(func.arguments[2].name == "ps");
    CHECK(func.arguments[2].type == "Parameter[]?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    std::vector<ghoul::Dictionary> ps;
    ghoul::Dictionary p;
    p.setValue("A", 1);
    p.setValue("B", 2.2);
    p.setValue("C", "3.3"s);
    ps.push_back(p);

    ghoul::Dictionary p2;
    p2.setValue("A", 4);
    p2.setValue("B", 5.5);
    p2.setValue("C", "6.6"s);
    ps.push_back(p2);

    ghoul::Dictionary p3;
    p3.setValue("A", 7);
    p3.setValue("B", 8.8);
    p3.setValue("C", "9.9"s);
    ps.push_back(p3);
    ghoul::lua::push(state, true, 3, ps);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ps.clear();
    ps.push_back(p);
    ghoul::lua::push(state, true, 1, ps);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ps.clear();
    ghoul::lua::push(state, true, 0, ps);
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    ghoul::lua::push(state, false);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Structs:  Map Arguments",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncMapArguments;
    CHECK(func.name == "funcMapArguments");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "ps");
    CHECK(func.arguments[0].type == "String -> Parameter");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::Dictionary ps;

    ghoul::Dictionary p;
    p.setValue("A", 1);
    p.setValue("B", 2.2);
    p.setValue("C", "3.3"s);
    ps.setValue("first", p);

    ghoul::Dictionary p2;
    p2.setValue("A", 4);
    p2.setValue("B", 5.5);
    p2.setValue("C", "6.6"s);
    ps.setValue("second", p2);

    ghoul::Dictionary p3;
    p3.setValue("A", 7);
    p3.setValue("B", 8.8);
    p3.setValue("C", "9.9"s);
    ps.setValue("third", p3);

    ghoul::lua::push(state, ps);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments-Structs:  Return value",
    "[Execution][LuaWrapper]"
)
{
    using namespace std::string_literals;

    Function func = codegen::lua::FuncReturnValue;
    CHECK(func.name == "funcReturnValue");
    REQUIRE(func.arguments.size() == 3);
    CHECK(func.arguments[0].name == "a");
    CHECK(func.arguments[0].type == "Integer");
    CHECK(func.arguments[1].name == "b");
    CHECK(func.arguments[1].type == "Number");
    CHECK(func.arguments[2].name == "c");
    CHECK(func.arguments[2].type == "String");
    CHECK(func.returnType == "Parameter");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    ghoul::lua::push(state, 1, 2.2, "3.3"s);
    func.function(state);
    CHECK(lua_gettop(state) == 1);

    const ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(state);
    REQUIRE(d.hasValue<double>("a"));
    CHECK(d.value<double>("a") == 1);
    REQUIRE(d.hasValue<double>("b"));
    CHECK(d.value<double>("b") == Catch::Approx(2.2));
    REQUIRE(d.hasValue<std::string>("c"));
    CHECK(d.value<std::string>("c") == "3.3");
}
