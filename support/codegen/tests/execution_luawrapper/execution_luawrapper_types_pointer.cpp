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

using namespace openspace;

namespace {
    struct Foo {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void funcVoidArg(void* arg) {
        REQUIRE(arg != nullptr);
        CHECK(arg == reinterpret_cast<void*>(1));
    }

    [[codegen::luawrap]] void funcIntArg(int* arg) {
        REQUIRE(arg != nullptr);
        CHECK(*arg == 1);
    }

    [[codegen::luawrap]] void funcStringArg(std::string* arg) {
        REQUIRE(arg != nullptr);
        CHECK(*arg == "abc");
    }

    [[codegen::luawrap]] void funcStructArg(Foo* arg) {
        REQUIRE(arg != nullptr);
        CHECK(arg->a == 1);
        CHECK(arg->b == 2.f);
        CHECK(arg->c == "abc");
    }

    [[codegen::luawrap]] void funcStruct2Arg(Foo** arg) {
        REQUIRE(arg != nullptr);
        Foo* f1 = arg[0];
        CHECK(f1->a == 1);
        CHECK(f1->b == 2.f);
        CHECK(f1->c == "abc");

        Foo* f2 = arg[1];
        CHECK(f2->a == 3);
        CHECK(f2->b == 4.f);
        CHECK(f2->c == "def");

        Foo* f3 = arg[2];
        CHECK(f3->a == 5);
        CHECK(f3->b == 6.f);
        CHECK(f3->c == "ghi");
    }

    [[codegen::luawrap]] void funcVoidDefaulted(bool isDefaulted, void* arg = nullptr) {
        if (isDefaulted) {
            CHECK(arg == nullptr);
        }
        else {
            CHECK(arg != nullptr);
            CHECK(arg == reinterpret_cast<void*>(1));
        }
    }

    [[codegen::luawrap]] void funcIntDefaulted(bool isDefaulted, int* arg = nullptr) {
        if (isDefaulted) {
            CHECK(arg == nullptr);
        }
        else {
            CHECK(arg != nullptr);
            CHECK(*arg == 1);
        }
    }

    [[codegen::luawrap]] void funcStringDefaulted(bool isDefaulted,
                                                  std::string* arg = nullptr)
    {
        if (isDefaulted) {
            CHECK(arg == nullptr);
        }
        else {
            CHECK(arg != nullptr);
            CHECK(*arg == "abc");
        }
    }

    [[codegen::luawrap]] void funcStructDefaulted(bool isDefaulted, Foo* arg = nullptr) {
        if (isDefaulted) {
            CHECK(arg == nullptr);
        }
        else {
            CHECK(arg != nullptr);
            CHECK(arg->a == 1);
            CHECK(arg->b == 2.f);
            CHECK(arg->c == "abc");
        }
    }

    [[codegen::luawrap]] void funcStruct2Defaulted(bool isDefaulted,
                                                   Foo** arg = nullptr)
    {
        if (isDefaulted) {
            CHECK(arg == nullptr);
        }
        else {
            CHECK(arg != nullptr);
            Foo* f1 = arg[0];
            CHECK(f1->a == 1);
            CHECK(f1->b == 2.f);
            CHECK(f1->c == "abc");

            Foo* f2 = arg[1];
            CHECK(f2->a == 3);
            CHECK(f2->b == 4.f);
            CHECK(f2->c == "def");

            Foo* f3 = arg[2];
            CHECK(f3->a == 5);
            CHECK(f3->b == 6.f);
            CHECK(f3->c == "ghi");
        }
    }

    [[codegen::luawrap]] void funcVoidMap(std::map<std::string, void*> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        REQUIRE(arg.find("key1")->second != nullptr);
        CHECK(arg.find("key1")->second == reinterpret_cast<void*>(1));

        REQUIRE(arg.find("key2") != arg.end());
        REQUIRE(arg.find("key2")->second != nullptr);
        CHECK(arg.find("key2")->second == reinterpret_cast<void*>(2));

        REQUIRE(arg.find("key3") != arg.end());
        REQUIRE(arg.find("key3")->second != nullptr);
        CHECK(arg.find("key3")->second == reinterpret_cast<void*>(3));
    }

    [[codegen::luawrap]] void funcIntMap(std::map<std::string, int*> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        REQUIRE(arg.find("key1")->second != nullptr);
        CHECK(*arg.find("key1")->second == 1);

        REQUIRE(arg.find("key2") != arg.end());
        REQUIRE(arg.find("key2")->second != nullptr);
        CHECK(*arg.find("key2")->second == 2);

        REQUIRE(arg.find("key3") != arg.end());
        REQUIRE(arg.find("key3")->second != nullptr);
        CHECK(*arg.find("key3")->second == 3);
    }

    [[codegen::luawrap]] void funcStringMap(std::map<std::string, std::string*> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        REQUIRE(arg.find("key1")->second != nullptr);
        CHECK(*arg.find("key1")->second == "abc");

        REQUIRE(arg.find("key2") != arg.end());
        REQUIRE(arg.find("key2")->second != nullptr);
        CHECK(*arg.find("key2")->second == "def");

        REQUIRE(arg.find("key3") != arg.end());
        REQUIRE(arg.find("key3")->second != nullptr);
        CHECK(*arg.find("key3")->second == "ghi");
    }

    [[codegen::luawrap]] void funcStructMap(std::map<std::string, Foo*> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        REQUIRE(arg.find("key1")->second != nullptr);
        CHECK(arg.find("key1")->second->a == 1);
        CHECK(arg.find("key1")->second->b == 2.f);
        CHECK(arg.find("key1")->second->c == "abc");

        REQUIRE(arg.find("key2") != arg.end());
        REQUIRE(arg.find("key2")->second != nullptr);
        CHECK(arg.find("key2")->second->a == 3);
        CHECK(arg.find("key2")->second->b == 4.f);
        CHECK(arg.find("key2")->second->c == "def");

        REQUIRE(arg.find("key3") != arg.end());
        REQUIRE(arg.find("key3")->second != nullptr);
        CHECK(arg.find("key3")->second->a == 5);
        CHECK(arg.find("key3")->second->b == 6.f);
        CHECK(arg.find("key3")->second->c == "ghi");
    }

    [[codegen::luawrap]] void funcStruct2Map(std::map<std::string, Foo**> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(arg.find("key1")->second != nullptr);
        CHECK(arg.find("key1")->second[0] != nullptr);
        REQUIRE(arg.find("key1")->second[0]->a == 1);
        REQUIRE(arg.find("key1")->second[0]->b == 2.f);
        REQUIRE(arg.find("key1")->second[0]->c == "abc");
        CHECK(arg.find("key1")->second[1] != nullptr);
        REQUIRE(arg.find("key1")->second[1]->a == 3);
        REQUIRE(arg.find("key1")->second[1]->b == 4.f);
        REQUIRE(arg.find("key1")->second[1]->c == "def");
        CHECK(arg.find("key1")->second[2] != nullptr);
        REQUIRE(arg.find("key1")->second[2]->a == 5);
        REQUIRE(arg.find("key1")->second[2]->b == 6.f);
        REQUIRE(arg.find("key1")->second[2]->c == "ghi");

        REQUIRE(arg.find("key2") != arg.end());
        CHECK(arg.find("key2")->second != nullptr);
        CHECK(arg.find("key2")->second[0] != nullptr);
        REQUIRE(arg.find("key2")->second[0]->a == 7);
        REQUIRE(arg.find("key2")->second[0]->b == 8.f);
        REQUIRE(arg.find("key2")->second[0]->c == "jkl");
        CHECK(arg.find("key2")->second[1] != nullptr);
        REQUIRE(arg.find("key2")->second[1]->a == 9);
        REQUIRE(arg.find("key2")->second[1]->b == 10.f);
        REQUIRE(arg.find("key2")->second[1]->c == "mno");
        CHECK(arg.find("key2")->second[2] != nullptr);
        REQUIRE(arg.find("key2")->second[2]->a == 11);
        REQUIRE(arg.find("key2")->second[2]->b == 12.f);
        REQUIRE(arg.find("key2")->second[2]->c == "pqr");

        REQUIRE(arg.find("key3") != arg.end());
        CHECK(arg.find("key3")->second != nullptr);
        CHECK(arg.find("key3")->second[0] != nullptr);
        REQUIRE(arg.find("key3")->second[0]->a == 13);
        REQUIRE(arg.find("key3")->second[0]->b == 14.f);
        REQUIRE(arg.find("key3")->second[0]->c == "stu");
        CHECK(arg.find("key3")->second[1] != nullptr);
        REQUIRE(arg.find("key3")->second[1]->a == 15);
        REQUIRE(arg.find("key3")->second[1]->b == 16.f);
        REQUIRE(arg.find("key3")->second[1]->c == "vwx");
        CHECK(arg.find("key3")->second[2] != nullptr);
        REQUIRE(arg.find("key3")->second[2]->a == 17);
        REQUIRE(arg.find("key3")->second[2]->b == 18.f);
        REQUIRE(arg.find("key3")->second[2]->c == "yzz");
    }

    [[codegen::luawrap]] void funcVoidOptional(std::optional<void*> arg) {
        REQUIRE(arg.has_value());
        REQUIRE(*arg != nullptr);
        CHECK(*arg == reinterpret_cast<void*>(1));
    }

    [[codegen::luawrap]] void funcIntOptional(std::optional<int*> arg) {
        REQUIRE(arg.has_value());
        REQUIRE(*arg != nullptr);
        CHECK(**arg == 1);
    }

    [[codegen::luawrap]] void funcStringOptional(std::optional<std::string*> arg) {
        REQUIRE(arg.has_value());
        REQUIRE(*arg != nullptr);
        CHECK(**arg == "abc");
    }

    [[codegen::luawrap]] void funcStructOptional(std::optional<Foo*> arg) {
        REQUIRE(arg.has_value());
        REQUIRE(*arg != nullptr);
        CHECK((*arg)->a == 1);
        CHECK((*arg)->b == 2.f);
        CHECK((*arg)->c == "abc");
    }

    [[codegen::luawrap]] void funcStruct2Optional(std::optional<Foo**> arg) {
        REQUIRE(arg.has_value());
        REQUIRE(*arg != nullptr);
        Foo* f1 = (*arg)[0];
        CHECK(f1->a == 1);
        CHECK(f1->b == 2.f);
        CHECK(f1->c == "abc");

        Foo* f2 = (*arg)[1];
        CHECK(f2->a == 3);
        CHECK(f2->b == 4.f);
        CHECK(f2->c == "def");

        Foo* f3 = (*arg)[2];
        CHECK(f3->a == 5);
        CHECK(f3->b == 6.f);
        CHECK(f3->c == "ghi");
    }

    [[codegen::luawrap]] void funcVoidVector(std::vector<void*> arg) {
        REQUIRE(arg.size() == 3);
        REQUIRE(arg[0] != nullptr);
        CHECK(arg[0] == reinterpret_cast<void*>(1));

        REQUIRE(arg[1] != nullptr);
        CHECK(arg[1] == reinterpret_cast<void*>(2));

        REQUIRE(arg[2] != nullptr);
        CHECK(arg[2] == reinterpret_cast<void*>(3));
    }

    [[codegen::luawrap]] void funcIntVector(std::vector<int*> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(*arg[0] == 1);
        CHECK(*arg[1] == 2);
        CHECK(*arg[2] == 3);
    }

    [[codegen::luawrap]] void funcStringVector(std::vector<std::string*> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(*arg[0] == "abc");
        CHECK(*arg[1] == "def");
        CHECK(*arg[2] == "ghi");
    }

    [[codegen::luawrap]] void funcStructVector(std::vector<Foo*> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0]->a == 1);
        CHECK(arg[0]->b == 2.f);
        CHECK(arg[0]->c == "abc");

        CHECK(arg[1]->a == 3);
        CHECK(arg[1]->b == 4.f);
        CHECK(arg[1]->c == "def");

        CHECK(arg[2]->a == 5);
        CHECK(arg[2]->b == 6.f);
        CHECK(arg[2]->c == "ghi");
    }

    [[codegen::luawrap]] void funcStruct2Vector(std::vector<Foo**> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(arg[0][0]->a == 1);
        CHECK(arg[0][0]->b == 2.f);
        CHECK(arg[0][0]->c == "abc");
        CHECK(arg[0][1]->a == 3);
        CHECK(arg[0][1]->b == 4.f);
        CHECK(arg[0][1]->c == "def");
        CHECK(arg[0][2]->a == 5);
        CHECK(arg[0][2]->b == 6.f);
        CHECK(arg[0][2]->c == "ghi");

        CHECK(arg[1][0]->a == 7);
        CHECK(arg[1][0]->b == 8.f);
        CHECK(arg[1][0]->c == "jkl");
        CHECK(arg[1][1]->a == 9);
        CHECK(arg[1][1]->b == 10.f);
        CHECK(arg[1][1]->c == "mno");
        CHECK(arg[1][2]->a == 11);
        CHECK(arg[1][2]->b == 12.f);
        CHECK(arg[1][2]->c == "pqr");

        CHECK(arg[2][0]->a == 13);
        CHECK(arg[2][0]->b == 14.f);
        CHECK(arg[2][0]->c == "stu");
        CHECK(arg[2][1]->a == 15);
        CHECK(arg[2][1]->b == 16.f);
        CHECK(arg[2][1]->c == "vwx");
        CHECK(arg[2][2]->a == 17);
        CHECK(arg[2][2]->b == 18.f);
        CHECK(arg[2][2]->c == "yzz");
    }

    [[codegen::luawrap]] void* returnVoid() {
        return reinterpret_cast<void*>(1);
    }

    [[codegen::luawrap]] int* returnInt() {
        return new int(1);
    }

    [[codegen::luawrap]] std::string* returnString() {
        return new std::string("foo");
    }

    [[codegen::luawrap]] Foo* returnStruct() {
        return new Foo {
            .a = 1,
            .b = 2.f,
            .c = "abc"
        };
    }

    [[codegen::luawrap]] Foo** returnStruct2() {
        Foo** fs = new Foo*[3];

        fs[0] = new Foo {
            .a = 1,
            .b = 2.f,
            .c = "abc"
        };

        fs[1] = new Foo {
            .a = 3,
            .b = 4.f,
            .c = "def"
        };

        fs[2] = new Foo {
            .a = 5,
            .b = 6.f,
            .c = "ghi"
        };

        return fs;
    }

    [[codegen::luawrap]] std::map<std::string, void*> returnVoidMap() {
        return {
            { "key1", reinterpret_cast<void*>(1) },
            { "key2", reinterpret_cast<void*>(2) },
            { "key3", reinterpret_cast<void*>(3) }
        };
    }

    [[codegen::luawrap]] std::map<std::string, int*> returnIntMap() {
        return {
            { "key1", new int(1) },
            { "key2", new int(2) },
            { "key3", new int(3) }
        };
    }

    [[codegen::luawrap]] std::map<std::string, std::string*> returnStringMap() {
        return {
            { "key1", new std::string("abc") },
            { "key2", new std::string("def") },
            { "key3", new std::string("ghi") }
        };
    }

    [[codegen::luawrap]] std::map<std::string, Foo*> returnStructMap() {
        return {
            { "key1", new Foo { .a = 1, .b = 2.f, .c = "abc" } },
            { "key2", new Foo { .a = 3, .b = 4.f, .c = "def" } },
            { "key3", new Foo { .a = 5, .b = 6.f, .c = "ghi" } }
        };
    }

    [[codegen::luawrap]] std::map<std::string, Foo**> returnStruct2Map() {
        Foo** fs1 = new Foo*[3];
        fs1[0] = new Foo {
            .a = 1,
            .b = 2.f,
            .c = "abc"
        };

        fs1[1] = new Foo {
            .a = 3,
            .b = 4.f,
            .c = "def"
        };

        fs1[2] = new Foo {
            .a = 5,
            .b = 6.f,
            .c = "ghi"
        };

        Foo** fs2 = new Foo*[3];
        fs2[0] = new Foo {
            .a = 7,
            .b = 8.f,
            .c = "jkl"
        };

        fs2[1] = new Foo {
            .a = 9,
            .b = 10.f,
            .c = "mno"
        };

        fs2[2] = new Foo {
            .a = 11,
            .b = 12.f,
            .c = "pqr"
        };

        Foo** fs3 = new Foo*[3];
        fs3[0] = new Foo {
            .a = 13,
            .b = 14.f,
            .c = "stu"
        };

        fs3[1] = new Foo {
            .a = 15,
            .b = 16.f,
            .c = "vwx"
        };

        fs3[2] = new Foo {
            .a = 17,
            .b = 18.f,
            .c = "yzz"
        };

        return {
            { "key1", fs1 },
            { "key2", fs2 },
            { "key3", fs3 }
        };
    }

    [[codegen::luawrap]] std::optional<void*> returnVoidOptional(bool shouldReturn) {
        if (shouldReturn) {
            return reinterpret_cast<void*>(1);
        }
        else {
            return std::nullopt;
        }
    }

    [[codegen::luawrap]] std::optional<int*> returnIntOptional(bool shouldReturn) {
        if (shouldReturn) {
            return new int(1);
        }
        else {
            return std::nullopt;
        }
    }

    [[codegen::luawrap]] std::optional<std::string*> returnStringOptional(
                                                                        bool shouldReturn)
    {
        if (shouldReturn) {
            return new std::string("abc");
        }
        else {
            return std::nullopt;
        }
    }

    [[codegen::luawrap]] std::optional<Foo*> returnStructOptional(bool shouldReturn) {
        if (shouldReturn) {
            return new Foo {
                .a = 1,
                .b = 2.f,
                .c = "abc"
            };
        }
        else {
            return std::nullopt;
        }
    }

    [[codegen::luawrap]] std::optional<Foo**> returnStruct2Optional(bool shouldReturn) {
        if (shouldReturn) {
            Foo** fs = new Foo*[3];

            fs[0] = new Foo {
                .a = 1,
                .b = 2.f,
                .c = "abc"
            };

            fs[1] = new Foo {
                .a = 3,
                .b = 4.f,
                .c = "def"
            };

            fs[2] = new Foo {
                .a = 5,
                .b = 6.f,
                .c = "ghi"
            };

            return fs;
        }
        else {
            return std::nullopt;
        }
    }

    [[codegen::luawrap]] std::vector<void*> returnVoidVector() {
        return {
            reinterpret_cast<void*>(1),
            reinterpret_cast<void*>(2),
            reinterpret_cast<void*>(3)
        };
    }

    [[codegen::luawrap]] std::vector<int*> returnIntVector() {
        return {
            new int(1),
            new int(2),
            new int(3)
        };
    }

    [[codegen::luawrap]] std::vector<std::string*> returnStringVector() {
        return {
            new std::string("abc"),
            new std::string("def"),
            new std::string("ghi")
        };
    }

    [[codegen::luawrap]] std::vector<Foo*> returnStructVector() {
        return {
            new Foo { .a = 1, .b = 2.f, .c = "abc" },
            new Foo { .a = 3, .b = 4.f, .c = "def" },
            new Foo { .a = 5, .b = 6.f, .c = "ghi" }
        };
    }

    [[codegen::luawrap]] std::vector<Foo**> returnStruct2Vector() {
        Foo** fs1 = new Foo*[3];
        fs1[0] = new Foo{
            .a = 1,
            .b = 2.f,
            .c = "abc"
        };

        fs1[1] = new Foo{
            .a = 3,
            .b = 4.f,
            .c = "def"
        };

        fs1[2] = new Foo{
            .a = 5,
            .b = 6.f,
            .c = "ghi"
        };

        Foo** fs2 = new Foo*[3];
        fs2[0] = new Foo{
            .a = 7,
            .b = 8.f,
            .c = "jkl"
        };

        fs2[1] = new Foo{
            .a = 9,
            .b = 10.f,
            .c = "mno"
        };

        fs2[2] = new Foo{
            .a = 11,
            .b = 12.f,
            .c = "pqr"
        };

        Foo** fs3 = new Foo*[3];
        fs3[0] = new Foo{
            .a = 13,
            .b = 14.f,
            .c = "stu"
        };

        fs3[1] = new Foo{
            .a = 15,
            .b = 16.f,
            .c = "vwx"
        };

        fs3[2] = new Foo{
            .a = 17,
            .b = 18.f,
            .c = "yzz"
        };

        return { fs1, fs2, fs3 };
    }
} // namespace
#include "execution_luawrapper_types_pointer_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments:  void*", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVoidArg;
    CHECK(func.name == "funcVoidArg");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "void*");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, reinterpret_cast<void*>(1));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  int*", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIntArg;
    CHECK(func.name == "funcIntArg");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "int*");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    int v = 1;
    lua::push(state, &v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  std::string*", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncStringArg;
    CHECK(func.name == "funcStringArg");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "std::string*");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    std::string v = "abc";
    lua::push(state, &v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  Foo*", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncStructArg;
    CHECK(func.name == "funcStructArg");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Foo*");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo v = Foo{ 1, 2.f, "abc" };
    lua::push(state, &v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  Foo**", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncStruct2Arg;
    CHECK(func.name == "funcStruct2Arg");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Foo**");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo** v = new Foo*[3];
    v[0] = new Foo {
        .a = 1,
        .b = 2.f,
        .c = "abc"
    };

    v[1] = new Foo {
        .a = 3,
        .b = 4.f,
        .c = "def"
    };

    v[2] = new Foo {
        .a = 5,
        .b = 6.f,
        .c = "ghi"
    };

    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  void* optional with value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncVoidDefaulted;
    CHECK(func.name == "funcVoidDefaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "void*?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, reinterpret_cast<void*>(1));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  void* optional w/o value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncVoidDefaulted;
    CHECK(func.name == "funcVoidDefaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "void*?");
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
    "Execution/LuaWrapper/Arguments:  int* optional with value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncIntDefaulted;
    CHECK(func.name == "funcIntDefaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "int*?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    int v = 1;
    lua::push(state, false, &v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  int* optional w/o value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncIntDefaulted;
    CHECK(func.name == "funcIntDefaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "int*?");
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
    "Execution/LuaWrapper/Arguments:  std::string* optional with value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStringDefaulted;
    CHECK(func.name == "funcStringDefaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "std::string*?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    std::string v = "abc";
    lua::push(state, false, &v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  std::string* optional w/o value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStringDefaulted;
    CHECK(func.name == "funcStringDefaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "std::string*?");
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
    "Execution/LuaWrapper/Arguments:  Foo* optional with value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStructDefaulted;
    CHECK(func.name == "funcStructDefaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Foo*?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo v = Foo {
        .a = 1,
        .b = 2.f,
        .c = "abc"
    };
    lua::push(state, false, &v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  Foo* optional w/o value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStructDefaulted;
    CHECK(func.name == "funcStructDefaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Foo*?");
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
    "Execution/LuaWrapper/Arguments:  Foo** optional with value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStruct2Defaulted;
    CHECK(func.name == "funcStruct2Defaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Foo**?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo** v = new Foo*[3];
    v[0] = new Foo{
        .a = 1,
        .b = 2.f,
        .c = "abc"
    };

    v[1] = new Foo{
        .a = 3,
        .b = 4.f,
        .c = "def"
    };

    v[2] = new Foo{
        .a = 5,
        .b = 6.f,
        .c = "ghi"
    };
    lua::push(state, false, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  Foo** optional w/o value",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStruct2Defaulted;
    CHECK(func.name == "funcStruct2Defaulted");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "Foo**?");
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
    "Execution/LuaWrapper/Arguments:  void* map",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncVoidMap;
    CHECK(func.name == "funcVoidMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> void*");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::map<std::string, void*> v = {
        { "key1", reinterpret_cast<void*>(1) },
        { "key2", reinterpret_cast<void*>(2) },
        { "key3", reinterpret_cast<void*>(3) }
    };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  int* map",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncIntMap;
    CHECK(func.name == "funcIntMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> int*");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    int v1 = 1;
    int v2 = 2;
    int v3 = 3;
    const std::map<std::string, void*> v = {
        { "key1", &v1 },
        { "key2", &v2 },
        { "key3", &v3 }
    };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  std::string* map",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStringMap;
    CHECK(func.name == "funcStringMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> std::string*");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    std::string v1 = "abc";
    std::string v2 = "def";
    std::string v3 = "ghi";
    const std::map<std::string, std::string*> v = {
        { "key1", &v1 },
        { "key2", &v2 },
        { "key3", &v3 }
    };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  Foo* map",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStructMap;
    CHECK(func.name == "funcStructMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> Foo*");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo v1 = Foo { .a = 1, .b = 2.f, .c = "abc" };
    Foo v2 = Foo { .a = 3, .b = 4.f, .c = "def" };
    Foo v3 = Foo { .a = 5, .b = 6.f, .c = "ghi" };
    const std::map<std::string, Foo*> v = {
        { "key1", &v1 },
        { "key2", &v2 },
        { "key3", &v3 }
    };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  Foo** map",
    "[Execution][LuaWrapper]")
{
    // NOLINTBEGIN(cppcoreguidelines-avoid-c-arrays,modernize-avoid-c-arrays)
    LuaLibrary::Function func = codegen::lua::FuncStruct2Map;
    CHECK(func.name == "funcStruct2Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> Foo**");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo v11 = Foo { .a = 1, .b = 2.f, .c = "abc" };
    Foo v12 = Foo { .a = 3, .b = 4.f, .c = "def" };
    Foo v13 = Foo { .a = 5, .b = 6.f, .c = "ghi" };
    Foo* v1[3] = { &v11, &v12, &v13 };

    Foo v21 = Foo { .a = 7, .b = 8.f, .c = "jkl" };
    Foo v22 = Foo { .a = 9, .b = 10.f, .c = "mno" };
    Foo v23 = Foo { .a = 11, .b = 12.f, .c = "pqr" };
    Foo* v2[3] = { &v21, &v22, &v23 };

    Foo v31 = Foo { .a = 13, .b = 14.f, .c = "stu" };
    Foo v32 = Foo { .a = 15, .b = 16.f, .c = "vwx" };
    Foo v33 = Foo { .a = 17, .b = 18.f, .c = "yzz" };
    Foo* v3[3] = { &v31, &v32, &v33 };
    const std::map<std::string, Foo**> v = {
        { "key1", v1 },
        { "key2", v2 },
        { "key3", v3 }
    };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
    // NOLINTEND(cppcoreguidelines-avoid-c-arrays,modernize-avoid-c-arrays)
}

TEST_CASE("Execution/LuaWrapper/Arguments:  void* optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncVoidOptional;
    CHECK(func.name == "funcVoidOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "void*?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, reinterpret_cast<void*>(1));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  int* optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncIntOptional;
    CHECK(func.name == "funcIntOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "int*?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    int v = 1;
    lua::push(state, &v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  std::string* optional",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStringOptional;
    CHECK(func.name == "funcStringOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "std::string*?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    std::string v = "abc";
    lua::push(state, &v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  Foo* optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncStructOptional;
    CHECK(func.name == "funcStructOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Foo*?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo v = Foo{ 1, 2.f, "abc" };
    lua::push(state, &v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments:  Foo** optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::FuncStruct2Optional;
    CHECK(func.name == "funcStruct2Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Foo**?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo** v = new Foo*[3];
    v[0] = new Foo {
        .a = 1,
        .b = 2.f,
        .c = "abc"
    };

    v[1] = new Foo {
        .a = 3,
        .b = 4.f,
        .c = "def"
    };

    v[2] = new Foo {
        .a = 5,
        .b = 6.f,
        .c = "ghi"
    };

    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  void* vector",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncVoidVector;
    CHECK(func.name == "funcVoidVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "void*[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::vector<void*> v = {
        reinterpret_cast<void*>(1),
        reinterpret_cast<void*>(2),
        reinterpret_cast<void*>(3)
    };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  int* vector",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncIntVector;
    CHECK(func.name == "funcIntVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "int*[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    int v1 = 1;
    int v2 = 2;
    int v3 = 3;
    const std::vector<void*> v = { &v1, &v2, &v3 };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  std::string* vector",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStringVector;
    CHECK(func.name == "funcStringVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "std::string*[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    std::string v1 = "abc";
    std::string v2 = "def";
    std::string v3 = "ghi";
    const std::vector<std::string*> v = { &v1, &v2, &v3 };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  Foo* vector",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStructVector;
    CHECK(func.name == "funcStructVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Foo*[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo v1 = Foo { .a = 1, .b = 2.f, .c = "abc" };
    Foo v2 = Foo { .a = 3, .b = 4.f, .c = "def" };
    Foo v3 = Foo { .a = 5, .b = 6.f, .c = "ghi" };
    const std::vector<Foo*> v = { &v1, &v2, &v3 };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments:  Foo** vector",
    "[Execution][LuaWrapper]")
{
    LuaLibrary::Function func = codegen::lua::FuncStruct2Vector;
    CHECK(func.name == "funcStruct2Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "Foo**[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    Foo v11 = Foo { .a = 1, .b = 2.f, .c = "abc" };
    Foo v12 = Foo { .a = 3, .b = 4.f, .c = "def" };
    Foo v13 = Foo { .a = 5, .b = 6.f, .c = "ghi" };
    Foo* v1[3] = { &v11, &v12, &v13 };

    Foo v21 = Foo { .a = 7, .b = 8.f, .c = "jkl" };
    Foo v22 = Foo { .a = 9, .b = 10.f, .c = "mno" };
    Foo v23 = Foo { .a = 11, .b = 12.f, .c = "pqr" };
    Foo* v2[3] = { &v21, &v22, &v23 };

    Foo v31 = Foo { .a = 13, .b = 14.f, .c = "stu" };
    Foo v32 = Foo { .a = 15, .b = 16.f, .c = "vwx" };
    Foo v33 = Foo { .a = 17, .b = 18.f, .c = "yzz" };
    Foo* v3[3] = { &v31, &v32, &v33 };
    const std::vector<Foo**> v = { v1, v2, v3 };
    lua::push(state, v);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  void*", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVoid;
    CHECK(func.name == "returnVoid");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "void*");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    void* val = lua::value<void*>(state);
    CHECK(val == reinterpret_cast<void*>(1));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  int*", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnInt;
    CHECK(func.name == "returnInt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "int*");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    int* val = lua::value<int*>(state);
    CHECK(*val == 1);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  std::string*", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnString;
    CHECK(func.name == "returnString");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "std::string*");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::string* val = lua::value<std::string*>(state);
    CHECK(*val == "foo");
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  Foo*", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnStruct;
    CHECK(func.name == "returnStruct");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Foo*");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    Foo* val = lua::value<Foo*>(state);
    CHECK(val->a == 1);
    CHECK(val->b == 2.f);
    CHECK(val->c == "abc");
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  Foo**", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnStruct2;
    CHECK(func.name == "returnStruct2");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Foo**");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    Foo** val = lua::value<Foo**>(state);
    CHECK(val[0]->a == 1);
    CHECK(val[0]->b == 2.f);
    CHECK(val[0]->c == "abc");
    CHECK(val[1]->a == 3);
    CHECK(val[1]->b == 4.f);
    CHECK(val[1]->c == "def");
    CHECK(val[2]->a == 5);
    CHECK(val[2]->b == 6.f);
    CHECK(val[2]->c == "ghi");
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  void* map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVoidMap;
    CHECK(func.name == "returnVoidMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> void*");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, void*> val =
        lua::value<std::map<std::string, void*>>(state);
    REQUIRE(val.size() == 3);
    REQUIRE(val.contains("key1"));
    CHECK(val["key1"] == reinterpret_cast<void*>(1));
    REQUIRE(val.contains("key2"));
    CHECK(val["key2"] == reinterpret_cast<void*>(2));
    REQUIRE(val.contains("key3"));
    CHECK(val["key3"] == reinterpret_cast<void*>(3));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  int* map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnIntMap;
    CHECK(func.name == "returnIntMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> int*");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, int*> val =
        lua::value<std::map<std::string, int*>>(state);
    REQUIRE(val.size() == 3);
    REQUIRE(val.contains("key1"));
    CHECK(*val["key1"] == 1);
    REQUIRE(val.contains("key2"));
    CHECK(*val["key2"] == 2);
    REQUIRE(val.contains("key3"));
    CHECK(*val["key3"] == 3);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  std::string* map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnStringMap;
    CHECK(func.name == "returnStringMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> std::string*");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, std::string*> val =
        lua::value<std::map<std::string, std::string*>>(state);
    REQUIRE(val.size() == 3);
    REQUIRE(val.contains("key1"));
    CHECK(*val["key1"] == "abc");
    REQUIRE(val.contains("key2"));
    CHECK(*val["key2"] == "def");
    REQUIRE(val.contains("key3"));
    CHECK(*val["key3"] == "ghi");
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  Foo* map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnStructMap;
    CHECK(func.name == "returnStructMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> Foo*");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, Foo*> val =
        lua::value<std::map<std::string, Foo*>>(state);
    REQUIRE(val.size() == 3);
    REQUIRE(val.contains("key1"));
    CHECK(val["key1"]->a == 1);
    CHECK(val["key1"]->b == 2.f);
    CHECK(val["key1"]->c == "abc");
    REQUIRE(val.contains("key2"));
    CHECK(val["key2"]->a == 3);
    CHECK(val["key2"]->b == 4.f);
    CHECK(val["key2"]->c == "def");
    REQUIRE(val.contains("key3"));
    CHECK(val["key3"]->a == 5);
    CHECK(val["key3"]->b == 6.f);
    CHECK(val["key3"]->c == "ghi");
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  Foo** map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnStruct2Map;
    CHECK(func.name == "returnStruct2Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> Foo**");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::map<std::string, Foo**> val =
        lua::value<std::map<std::string, Foo**>>(state);
    REQUIRE(val.size() == 3);
    REQUIRE(val.contains("key1"));
    CHECK(val["key1"][0]->a == 1);
    CHECK(val["key1"][0]->b == 2.f);
    CHECK(val["key1"][0]->c == "abc");
    CHECK(val["key1"][1]->a == 3);
    CHECK(val["key1"][1]->b == 4.f);
    CHECK(val["key1"][1]->c == "def");
    CHECK(val["key1"][2]->a == 5);
    CHECK(val["key1"][2]->b == 6.f);
    CHECK(val["key1"][2]->c == "ghi");
    REQUIRE(val.contains("key2"));
    CHECK(val["key2"][0]->a == 7);
    CHECK(val["key2"][0]->b == 8.f);
    CHECK(val["key2"][0]->c == "jkl");
    CHECK(val["key2"][1]->a == 9);
    CHECK(val["key2"][1]->b == 10.f);
    CHECK(val["key2"][1]->c == "mno");
    CHECK(val["key2"][2]->a == 11);
    CHECK(val["key2"][2]->b == 12.f);
    CHECK(val["key2"][2]->c == "pqr");
    REQUIRE(val.contains("key3"));
    CHECK(val["key3"][0]->a == 13);
    CHECK(val["key3"][0]->b == 14.f);
    CHECK(val["key3"][0]->c == "stu");
    CHECK(val["key3"][1]->a == 15);
    CHECK(val["key3"][1]->b == 16.f);
    CHECK(val["key3"][1]->c == "vwx");
    CHECK(val["key3"][2]->a == 17);
    CHECK(val["key3"][2]->b == 18.f);
    CHECK(val["key3"][2]->c == "yzz");
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  void* optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVoidOptional;
    CHECK(func.name == "returnVoidOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "shouldReturn");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.returnType == "void*?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);

    lua::push(state, true);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::optional<void*> val1 = lua::value<std::optional<void*>>(state);
    REQUIRE(val1.has_value());
    CHECK(*val1 == reinterpret_cast<void*>(1));

    lua::push(state, false);
    func.function(state);
    REQUIRE(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  int* optional", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnIntOptional;
    CHECK(func.name == "returnIntOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "shouldReturn");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.returnType == "int*?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);

    lua::push(state, true);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::optional<int*> val1 = lua::value<std::optional<int*>>(state);
    REQUIRE(val1.has_value());
    int* v = *val1;
    CHECK(*v == 1);

    lua::push(state, false);
    func.function(state);
    REQUIRE(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  std::string* optional",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnStringOptional;
    CHECK(func.name == "returnStringOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "shouldReturn");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.returnType == "std::string*?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);

    lua::push(state, true);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::optional<std::string*> val1 =
        lua::value<std::optional<std::string*>>(state);
    REQUIRE(val1.has_value());
    std::string* v = *val1;
    CHECK(*v == "abc");

    lua::push(state, false);
    func.function(state);
    REQUIRE(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  Foo* optional",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnStructOptional;
    CHECK(func.name == "returnStructOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "shouldReturn");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.returnType == "Foo*?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);

    lua::push(state, true);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::optional<Foo*> val1 = lua::value<std::optional<Foo*>>(state);
    REQUIRE(val1.has_value());
    Foo* v = *val1;
    CHECK(v->a == 1);
    CHECK(v->b == 2.f);
    CHECK(v->c == "abc");

    lua::push(state, false);
    func.function(state);
    REQUIRE(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  Foo** optional",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnStruct2Optional;
    CHECK(func.name == "returnStruct2Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "shouldReturn");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.returnType == "Foo**?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);

    lua::push(state, true);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::optional<Foo**> val1 = lua::value<std::optional<Foo**>>(state);
    REQUIRE(val1.has_value());
    Foo** v = *val1;
    CHECK(v[0]->a == 1);
    CHECK(v[0]->b == 2.f);
    CHECK(v[0]->c == "abc");
    CHECK(v[1]->a == 3);
    CHECK(v[1]->b == 4.f);
    CHECK(v[1]->c == "def");
    CHECK(v[2]->a == 5);
    CHECK(v[2]->b == 6.f);
    CHECK(v[2]->c == "ghi");

    lua::push(state, false);
    func.function(state);
    REQUIRE(lua_gettop(state) == 0);

    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  void* vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnVoidVector;
    CHECK(func.name == "returnVoidVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "void*[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<void*> val = lua::value<std::vector<void*>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0] == reinterpret_cast<void*>(1));
    CHECK(val[1] == reinterpret_cast<void*>(2));
    CHECK(val[2] == reinterpret_cast<void*>(3));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  int* vector", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnIntVector;
    CHECK(func.name == "returnIntVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "int*[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<int*> val = lua::value<std::vector<int*>>(state);
    REQUIRE(val.size() == 3);
    CHECK(*val[0] == 1);
    CHECK(*val[1] == 2);
    CHECK(*val[2] == 3);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  std::string* vector",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnStringVector;
    CHECK(func.name == "returnStringVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "std::string*[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<std::string*> val = lua::value<std::vector<std::string*>>(state);
    REQUIRE(val.size() == 3);
    CHECK(*val[0] == "abc");
    CHECK(*val[1] == "def");
    CHECK(*val[2] == "ghi");
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  Foo* vector",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnStructVector;
    CHECK(func.name == "returnStructVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Foo*[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);

    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<Foo*> val = lua::value<std::vector<Foo*>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0]->a == 1);
    CHECK(val[0]->b == 2.f);
    CHECK(val[0]->c == "abc");
    CHECK(val[1]->a == 3);
    CHECK(val[1]->b == 4.f);
    CHECK(val[1]->c == "def");
    CHECK(val[2]->a == 5);
    CHECK(val[2]->b == 6.f);
    CHECK(val[2]->c == "ghi");
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  Foo** vector",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnStruct2Vector;
    CHECK(func.name == "returnStruct2Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "Foo**[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);

    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    std::vector<Foo**> val = lua::value<std::vector<Foo**>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val[0][0]->a == 1);
    CHECK(val[0][0]->b == 2.f);
    CHECK(val[0][0]->c == "abc");
    CHECK(val[0][1]->a == 3);
    CHECK(val[0][1]->b == 4.f);
    CHECK(val[0][1]->c == "def");
    CHECK(val[0][2]->a == 5);
    CHECK(val[0][2]->b == 6.f);
    CHECK(val[0][2]->c == "ghi");
    CHECK(val[1][0]->a == 7);
    CHECK(val[1][0]->b == 8.f);
    CHECK(val[1][0]->c == "jkl");
    CHECK(val[1][1]->a == 9);
    CHECK(val[1][1]->b == 10.f);
    CHECK(val[1][1]->c == "mno");
    CHECK(val[1][2]->a == 11);
    CHECK(val[1][2]->b == 12.f);
    CHECK(val[1][2]->c == "pqr");
    CHECK(val[2][0]->a == 13);
    CHECK(val[2][0]->b == 14.f);
    CHECK(val[2][0]->c == "stu");
    CHECK(val[2][1]->a == 15);
    CHECK(val[2][1]->b == 16.f);
    CHECK(val[2][1]->c == "vwx");
    CHECK(val[2][2]->a == 17);
    CHECK(val[2][2]->b == 18.f);
    CHECK(val[2][2]->c == "yzz");
    lua_close(state);
}
