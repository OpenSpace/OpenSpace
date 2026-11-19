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

#include "codegen.h"
#include "parsing.h"
#include "types.h"

TEST_CASE("Parsing/LuaWrapper/Arguments-Struct:  void", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void func() {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        CHECK(s->variables[0]->name == "a");
        CHECK(generateTypename(s->variables[0]->type) == "int");

        CHECK(s->variables[1]->name == "b");
        CHECK(generateTypename(s->variables[1]->type) == "float");

        CHECK(s->variables[2]->name == "c");
        CHECK(generateTypename(s->variables[2]->type) == "std::string");
    }
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        CHECK(f->arguments.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE(
    "Parsing/LuaWrapper/Arguments-Struct:  as single parameter",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void func(Parameter p) {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        CHECK(s->variables[0]->name == "a");
        CHECK(generateTypename(s->variables[0]->type) == "int");

        CHECK(s->variables[1]->name == "b");
        CHECK(generateTypename(s->variables[1]->type) == "float");

        CHECK(s->variables[2]->name == "c");
        CHECK(generateTypename(s->variables[2]->type) == "std::string");
    }
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "p");
        REQUIRE(v->type->isCustomType());
        CustomType* t = static_cast<CustomType*>(v->type);
        REQUIRE(t);
        REQUIRE(t->type);
        CHECK(t->type->name == "Parameter");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.key.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.inrange.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.notinlist.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE(
    "Parsing/LuaWrapper/Arguments-Struct:  as optional single parameter",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void func(std::optional<Parameter> p) {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        CHECK(s->variables[0]->name == "a");
        CHECK(generateTypename(s->variables[0]->type) == "int");

        CHECK(s->variables[1]->name == "b");
        CHECK(generateTypename(s->variables[1]->type) == "float");

        CHECK(s->variables[2]->name == "c");
        CHECK(generateTypename(s->variables[2]->type) == "std::string");
    }
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "p");
        REQUIRE(v->type->isOptionalType());
        OptionalType* t = static_cast<OptionalType*>(v->type);
        REQUIRE(t);

        REQUIRE(t->type->isCustomType());
        CustomType* tt = static_cast<CustomType*>(t->type);
        REQUIRE(tt);
        REQUIRE(tt->type);
        CHECK(tt->type->name == "Parameter");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.key.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.inrange.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.notinlist.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE(
    "Parsing/LuaWrapper/Arguments-Struct:  as multiple parameter",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void func(Parameter p, Parameter q) {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        CHECK(s->variables[0]->name == "a");
        CHECK(generateTypename(s->variables[0]->type) == "int");

        CHECK(s->variables[1]->name == "b");
        CHECK(generateTypename(s->variables[1]->type) == "float");

        CHECK(s->variables[2]->name == "c");
        CHECK(generateTypename(s->variables[2]->type) == "std::string");
    }
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 2);

        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "p");
            REQUIRE(v->type->isCustomType());
            CustomType* t = static_cast<CustomType*>(v->type);
            REQUIRE(t);
            REQUIRE(t->type);
            CHECK(t->type->name == "Parameter");

            CHECK(v->attributes.annotation.empty());
            CHECK(v->attributes.key.empty());
            CHECK(v->attributes.inlist.empty());
            CHECK(v->attributes.inrange.empty());
            CHECK(v->attributes.less.empty());
            CHECK(v->attributes.lessequal.empty());
            CHECK(v->attributes.greater.empty());
            CHECK(v->attributes.greaterequal.empty());
            CHECK(v->attributes.notinlist.empty());
            CHECK(v->attributes.reference.empty());
            CHECK(v->attributes.unequal.empty());
        }
        {
            Variable* v = f->arguments[1];
            REQUIRE(v);
            CHECK(v->name == "q");
            REQUIRE(v->type->isCustomType());
            CustomType* t = static_cast<CustomType*>(v->type);
            REQUIRE(t);
            REQUIRE(t->type);
            CHECK(t->type->name == "Parameter");

            CHECK(v->attributes.annotation.empty());
            CHECK(v->attributes.key.empty());
            CHECK(v->attributes.inlist.empty());
            CHECK(v->attributes.inrange.empty());
            CHECK(v->attributes.less.empty());
            CHECK(v->attributes.lessequal.empty());
            CHECK(v->attributes.greater.empty());
            CHECK(v->attributes.greaterequal.empty());
            CHECK(v->attributes.notinlist.empty());
            CHECK(v->attributes.reference.empty());
            CHECK(v->attributes.unequal.empty());
        }
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE(
    "Parsing/LuaWrapper/Arguments-Struct:  as multiple parameter w/ optional",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void func(Parameter p, std::optional<Parameter> q) {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        CHECK(s->variables[0]->name == "a");
        CHECK(generateTypename(s->variables[0]->type) == "int");

        CHECK(s->variables[1]->name == "b");
        CHECK(generateTypename(s->variables[1]->type) == "float");

        CHECK(s->variables[2]->name == "c");
        CHECK(generateTypename(s->variables[2]->type) == "std::string");
    }
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 2);

        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "p");
            REQUIRE(v->type->isCustomType());
            CustomType* t = static_cast<CustomType*>(v->type);
            REQUIRE(t);
            REQUIRE(t->type);
            CHECK(t->type->name == "Parameter");

            CHECK(v->attributes.annotation.empty());
            CHECK(v->attributes.key.empty());
            CHECK(v->attributes.inlist.empty());
            CHECK(v->attributes.inrange.empty());
            CHECK(v->attributes.less.empty());
            CHECK(v->attributes.lessequal.empty());
            CHECK(v->attributes.greater.empty());
            CHECK(v->attributes.greaterequal.empty());
            CHECK(v->attributes.notinlist.empty());
            CHECK(v->attributes.reference.empty());
            CHECK(v->attributes.unequal.empty());
        }
        {
            Variable* v = f->arguments[1];
            REQUIRE(v);
            CHECK(v->name == "q");
            REQUIRE(v->type->isOptionalType());
            OptionalType* t = static_cast<OptionalType*>(v->type);
            REQUIRE(t);

            REQUIRE(t->type->isCustomType());
            CustomType* tt = static_cast<CustomType*>(t->type);
            REQUIRE(tt);
            REQUIRE(tt->type);
            CHECK(tt->type->name == "Parameter");


            CHECK(v->attributes.annotation.empty());
            CHECK(v->attributes.key.empty());
            CHECK(v->attributes.inlist.empty());
            CHECK(v->attributes.inrange.empty());
            CHECK(v->attributes.less.empty());
            CHECK(v->attributes.lessequal.empty());
            CHECK(v->attributes.greater.empty());
            CHECK(v->attributes.greaterequal.empty());
            CHECK(v->attributes.notinlist.empty());
            CHECK(v->attributes.reference.empty());
            CHECK(v->attributes.unequal.empty());
        }
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE(
    "Parsing/LuaWrapper/Arguments-Struct:  as parameter vector",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void func(std::vector<Parameter> p) {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        CHECK(s->variables[0]->name == "a");
        CHECK(generateTypename(s->variables[0]->type) == "int");

        CHECK(s->variables[1]->name == "b");
        CHECK(generateTypename(s->variables[1]->type) == "float");

        CHECK(s->variables[2]->name == "c");
        CHECK(generateTypename(s->variables[2]->type) == "std::string");
    }
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "p");
        REQUIRE(v->type->isVectorType());
        VectorType* t = static_cast<VectorType*>(v->type);
        REQUIRE(t->type->isCustomType());
        CustomType* tt = static_cast<CustomType*>(t->type);
        REQUIRE(tt);
        REQUIRE(tt->type);
        CHECK(tt->type->name == "Parameter");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.key.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.inrange.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.notinlist.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE(
    "Parsing/LuaWrapper/Arguments-Struct:  as optional parameter vector",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void func(std::optional<std::vector<Parameter>> p) {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        CHECK(s->variables[0]->name == "a");
        CHECK(generateTypename(s->variables[0]->type) == "int");

        CHECK(s->variables[1]->name == "b");
        CHECK(generateTypename(s->variables[1]->type) == "float");

        CHECK(s->variables[2]->name == "c");
        CHECK(generateTypename(s->variables[2]->type) == "std::string");
    }
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "p");
        REQUIRE(v->type->isOptionalType());
        OptionalType* t = static_cast<OptionalType*>(v->type);
        REQUIRE(t->type->isVectorType());
        VectorType* tt = static_cast<VectorType*>(t->type);
        REQUIRE(tt->type->isCustomType());
        CustomType* ttt = static_cast<CustomType*>(tt->type);
        REQUIRE(ttt);
        REQUIRE(ttt->type);
        CHECK(ttt->type->name == "Parameter");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.key.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.inrange.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.notinlist.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Arguments-Struct:  as map", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void func(std::map<std::string, Parameter> p) {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        CHECK(s->variables[0]->name == "a");
        CHECK(generateTypename(s->variables[0]->type) == "int");

        CHECK(s->variables[1]->name == "b");
        CHECK(generateTypename(s->variables[1]->type) == "float");

        CHECK(s->variables[2]->name == "c");
        CHECK(generateTypename(s->variables[2]->type) == "std::string");
    }
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "p");
        REQUIRE(v->type->isMapType());
        MapType* t = static_cast<MapType*>(v->type);
        CHECK(generateTypename(t->keyType) == "std::string");
        REQUIRE(t->valueType->isCustomType());
        CustomType* tt = static_cast<CustomType*>(t->valueType);
        REQUIRE(tt);
        REQUIRE(tt->type);
        CHECK(tt->type->name == "Parameter");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.key.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.inrange.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.notinlist.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Arguments-Struct:  as array", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] void func1(std::array<Parameter, 1> p) {
    }

    [[codegen::luawrap]] void func2(std::array<Parameter, 5> p) {
    }

    [[codegen::luawrap]] void func3(std::array<Parameter, 10> p) {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        CHECK(s->variables[0]->name == "a");
        CHECK(generateTypename(s->variables[0]->type) == "int");

        CHECK(s->variables[1]->name == "b");
        CHECK(generateTypename(s->variables[1]->type) == "float");

        CHECK(s->variables[2]->name == "c");
        CHECK(generateTypename(s->variables[2]->type) == "std::string");
    }
    REQUIRE(code.luaWrapperFunctions.size() == 3);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func1");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "p");
        REQUIRE(v->type->isArrayType());
        ArrayType* at = static_cast<ArrayType*>(v->type);
        REQUIRE(at->type->tag == VariableType::Tag::CustomType);
        CHECK(at->size == 1);
        CustomType* ct = static_cast<CustomType*>(at->type);
        REQUIRE(ct);
        REQUIRE(ct->type);
        CHECK(ct->type->name == "Parameter");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.key.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.inrange.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.notinlist.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
    }
    {
        Function* f = code.luaWrapperFunctions[1];
        REQUIRE(f);

        CHECK(f->functionName == "func2");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "p");
        REQUIRE(v->type->isArrayType());
        ArrayType* at = static_cast<ArrayType*>(v->type);
        REQUIRE(at->type->tag == VariableType::Tag::CustomType);
        CHECK(at->size == 5);
        CustomType* ct = static_cast<CustomType*>(at->type);
        REQUIRE(ct);
        REQUIRE(ct->type);
        CHECK(ct->type->name == "Parameter");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.key.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.inrange.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.notinlist.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
    }
    {
        Function* f = code.luaWrapperFunctions[2];
        REQUIRE(f);

        CHECK(f->functionName == "func3");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "p");
        REQUIRE(v->type->isArrayType());
        ArrayType* at = static_cast<ArrayType*>(v->type);
        REQUIRE(at->type->tag == VariableType::Tag::CustomType);
        CHECK(at->size == 10);
        CustomType* ct = static_cast<CustomType*>(at->type);
        REQUIRE(ct);
        REQUIRE(ct->type);
        CHECK(ct->type->name == "Parameter");

        CHECK(v->attributes.annotation.empty());
        CHECK(v->attributes.key.empty());
        CHECK(v->attributes.inlist.empty());
        CHECK(v->attributes.inrange.empty());
        CHECK(v->attributes.less.empty());
        CHECK(v->attributes.lessequal.empty());
        CHECK(v->attributes.greater.empty());
        CHECK(v->attributes.greaterequal.empty());
        CHECK(v->attributes.notinlist.empty());
        CHECK(v->attributes.reference.empty());
        CHECK(v->attributes.unequal.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE(
    "Parsing/LuaWrapper/Arguments-Struct:  as return value",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(P)]] Parameter {
        int a;
        float b;
        std::string c;
    };

    [[codegen::luawrap]] Parameter func() {
    }
)";

    Code code = parse(Source);
    CHECK(code.enums.empty());

    {
        REQUIRE(code.structs.size() == 1);
        Struct* s = code.structs[0];
        REQUIRE(s);

        CHECK(s->name == "Parameter");
        REQUIRE(s->variables.size() == 3);
        {
            Variable* v = s->variables[0];
            REQUIRE(v);
            CHECK(v->name == "a");
            CHECK(generateTypename(v->type) == "int");

            CHECK(v->attributes.annotation.empty());
            CHECK(v->attributes.key.empty());
            CHECK(v->attributes.inlist.empty());
            CHECK(v->attributes.inrange.empty());
            CHECK(v->attributes.less.empty());
            CHECK(v->attributes.lessequal.empty());
            CHECK(v->attributes.greater.empty());
            CHECK(v->attributes.greaterequal.empty());
            CHECK(v->attributes.notinlist.empty());
            CHECK(v->attributes.reference.empty());
            CHECK(v->attributes.unequal.empty());
        }

        {
            Variable* v = s->variables[1];
            REQUIRE(v);
            CHECK(v->name == "b");
            CHECK(generateTypename(v->type) == "float");

            CHECK(v->attributes.annotation.empty());
            CHECK(v->attributes.key.empty());
            CHECK(v->attributes.inlist.empty());
            CHECK(v->attributes.inrange.empty());
            CHECK(v->attributes.less.empty());
            CHECK(v->attributes.lessequal.empty());
            CHECK(v->attributes.greater.empty());
            CHECK(v->attributes.greaterequal.empty());
            CHECK(v->attributes.notinlist.empty());
            CHECK(v->attributes.reference.empty());
            CHECK(v->attributes.unequal.empty());
        }

        {
            Variable* v = s->variables[2];
            REQUIRE(v);
            CHECK(v->name == "c");
            CHECK(generateTypename(v->type) == "std::string");

            CHECK(v->attributes.annotation.empty());
            CHECK(v->attributes.key.empty());
            CHECK(v->attributes.inlist.empty());
            CHECK(v->attributes.inrange.empty());
            CHECK(v->attributes.less.empty());
            CHECK(v->attributes.lessequal.empty());
            CHECK(v->attributes.greater.empty());
            CHECK(v->attributes.greaterequal.empty());
            CHECK(v->attributes.notinlist.empty());
            CHECK(v->attributes.reference.empty());
            CHECK(v->attributes.unequal.empty());
        }
    }
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());

        REQUIRE(f->returnValue->isCustomType());
        CustomType* t = static_cast<CustomType*>(f->returnValue);
        REQUIRE(t);
        REQUIRE(t->type);
        CHECK(t->type->name == "Parameter");

        REQUIRE(f->arguments.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
