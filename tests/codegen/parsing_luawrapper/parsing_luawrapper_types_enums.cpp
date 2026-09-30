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

TEST_CASE("Parsing/LuaWrapper/Arguments-Enums:  void", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    enum class [[codegen::enum]] Enum {
        A,
        B,
        C
    };

    [[codegen::luawrap]] void func() {
    }
)";

    Code code = parse(Source);
    {
        REQUIRE(code.enums.size() == 1);
        Enum* e = code.enums[0];
        REQUIRE(e);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "A");
        CHECK(e->elements[1]->name == "B");
        CHECK(e->elements[2]->name == "C");
    }
    CHECK(code.structs.empty());
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
    "Parsing/LuaWrapper/Arguments-Enums:  as single parameter",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    enum class [[codegen::enum]] Enum {
        A,
        B,
        C
    };

    [[codegen::luawrap]] void func(Enum e) {
    }
)";

    Code code = parse(Source);
    {
        REQUIRE(code.enums.size() == 1);
        Enum* e = code.enums[0];
        REQUIRE(e);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "A");
        CHECK(e->elements[1]->name == "B");
        CHECK(e->elements[2]->name == "C");
    }
    CHECK(code.structs.empty());
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        CHECK(v->name == "e");
        REQUIRE(v->type->isCustomType());
        CustomType* t = static_cast<CustomType*>(v->type);
        CHECK(t->name == "Enum");
        REQUIRE(t->type->type == StackElement::Type::Enum);

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
    "Parsing/LuaWrapper/Arguments-Enums:  as double parameter",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    enum class [[codegen::enum]] Enum {
        A,
        B,
        C
    };

    [[codegen::luawrap]] void func(Enum e, Enum f) {
    }
)";

    Code code = parse(Source);
    {
        REQUIRE(code.enums.size() == 1);
        Enum* e = code.enums[0];
        REQUIRE(e);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "A");
        CHECK(e->elements[1]->name == "B");
        CHECK(e->elements[2]->name == "C");
    }
    CHECK(code.structs.empty());
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
            CHECK(v->name == "e");
            REQUIRE(v->type->isCustomType());
            CustomType* t = static_cast<CustomType*>(v->type);
            CHECK(t->name == "Enum");
            REQUIRE(t->type->type == StackElement::Type::Enum);

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
            CHECK(v->name == "f");
            REQUIRE(v->type->isCustomType());
            CustomType* t = static_cast<CustomType*>(v->type);
            CHECK(t->name == "Enum");
            REQUIRE(t->type->type == StackElement::Type::Enum);

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
    "Parsing/LuaWrapper/Arguments-Enums:  as optional parameter",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    enum class [[codegen::enum]] Enum {
        A,
        B,
        C
    };

    [[codegen::luawrap]] void func(std::optional<Enum> e) {
    }
)";

    Code code = parse(Source);
    {
        REQUIRE(code.enums.size() == 1);
        Enum* e = code.enums[0];
        REQUIRE(e);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "A");
        CHECK(e->elements[1]->name == "B");
        CHECK(e->elements[2]->name == "C");
    }
    CHECK(code.structs.empty());
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        CHECK(v->name == "e");
        REQUIRE(v->type->isOptionalType());
        OptionalType* t = static_cast<OptionalType*>(v->type);
        REQUIRE(t->type->isCustomType());
        CustomType* tt = static_cast<CustomType*>(t->type);
        CHECK(tt->name == "Enum");
        REQUIRE(tt->type->type == StackElement::Type::Enum);

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
    "Parsing/LuaWrapper/Arguments-Enums:  as vector parameter",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    enum class [[codegen::enum]] Enum {
        A,
        B,
        C
    };

    [[codegen::luawrap]] void func(std::vector<Enum> e) {
    }
)";

    Code code = parse(Source);
    {
        REQUIRE(code.enums.size() == 1);
        Enum* e = code.enums[0];
        REQUIRE(e);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "A");
        CHECK(e->elements[1]->name == "B");
        CHECK(e->elements[2]->name == "C");
    }
    CHECK(code.structs.empty());
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        Variable* v = f->arguments[0];
        CHECK(v->name == "e");
        REQUIRE(v->type->isVectorType());
        VectorType* t = static_cast<VectorType*>(v->type);
        REQUIRE(t->type->isCustomType());
        CustomType* tt = static_cast<CustomType*>(t->type);
        CHECK(tt->name == "Enum");
        REQUIRE(tt->type->type == StackElement::Type::Enum);

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
    "Parsing/LuaWrapper/Arguments-Enums:  as array parameter",
    "[Parsing][LuaWrapper]")
{
    constexpr std::string_view Source = R"(
    enum class [[codegen::enum]] Enum {
        A,
        B,
        C
    };

    [[codegen::luawrap]] void func1(std::array<Enum, 1> arg) {
    }

    [[codegen::luawrap]] void func2(std::array<Enum, 5> arg) {
    }

    [[codegen::luawrap]] void func3(std::array<Enum, 10> arg) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    {
        REQUIRE(code.enums.size() == 1);
        Enum* e = code.enums[0];
        REQUIRE(e);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "A");
        CHECK(e->elements[1]->name == "B");
        CHECK(e->elements[2]->name == "C");
    }
    REQUIRE(code.luaWrapperFunctions.size() == 3);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func1");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::ArrayType);
            ArrayType* at = static_cast<ArrayType*>(v->type);
            CustomType* tt = static_cast<CustomType*>(at->type);
            CHECK(at->size == 1);
            CHECK(tt->name == "Enum");
            REQUIRE(tt->type->type == StackElement::Type::Enum);

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
        Function* f = code.luaWrapperFunctions[1];
        REQUIRE(f);

        CHECK(f->functionName == "func2");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::ArrayType);
            ArrayType* at = static_cast<ArrayType*>(v->type);
            CustomType* tt = static_cast<CustomType*>(at->type);
            CHECK(at->size == 5);
            CHECK(tt->name == "Enum");
            REQUIRE(tt->type->type == StackElement::Type::Enum);

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
        Function* f = code.luaWrapperFunctions[2];
        REQUIRE(f);

        CHECK(f->functionName == "func3");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::ArrayType);
            ArrayType* at = static_cast<ArrayType*>(v->type);
            CustomType* tt = static_cast<CustomType*>(at->type);
            CHECK(at->size == 10);
            CHECK(tt->name == "Enum");
            REQUIRE(tt->type->type == StackElement::Type::Enum);

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

TEST_CASE("Parsing/LuaWrapper/Arguments-Enums:  as return value", "[Parsing][LuaWrapper]")
{
    constexpr std::string_view Source = R"(
    enum class [[codegen::enum]] Enum {
        A,
        B,
        C
    };

    [[codegen::luawrap]] Enum func() {
    }
)";

    Code code = parse(Source);
    {
        REQUIRE(code.enums.size() == 1);
        Enum* e = code.enums[0];
        REQUIRE(e);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "A");
        CHECK(e->elements[1]->name == "B");
        CHECK(e->elements[2]->name == "C");
    }
    CHECK(code.structs.empty());
    {
        REQUIRE(code.luaWrapperFunctions.size() == 1);
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "func");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue->isCustomType());
        CustomType* t = static_cast<CustomType*>(f->returnValue);
        CHECK(t->name == "Enum");
        REQUIRE(t->type->type == StackElement::Type::Enum);
        CHECK(f->arguments.empty());
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Arguments-Enum:  array return", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    enum class [[codegen::enum]] Enum {
        A,
        B,
        C
    };

    [[codegen::luawrap]] std::array<Enum, 1> foo1() {
        return {};
    }

    [[codegen::luawrap]] std::array<Enum, 5> foo2() {
        return {};
    }

    [[codegen::luawrap]] std::array<Enum, 10> foo3() {
        return {};
    }
)";

    Code code = parse(Source);
    {
        REQUIRE(code.enums.size() == 1);
        Enum* e = code.enums[0];
        REQUIRE(e);
        REQUIRE(e->elements.size() == 3);
        CHECK(e->elements[0]->name == "A");
        CHECK(e->elements[1]->name == "B");
        CHECK(e->elements[2]->name == "C");
    }
    CHECK(code.structs.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 3);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "foo1");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::CustomType);
        CHECK(at->size == 1);
        CustomType* t = static_cast<CustomType*>(at->type);
        CHECK(t->name == "Enum");
    }
    {
        Function* f = code.luaWrapperFunctions[1];
        REQUIRE(f);

        CHECK(f->functionName == "foo2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::CustomType);
        CHECK(at->size == 5);
        CustomType* t = static_cast<CustomType*>(at->type);
        CHECK(t->name == "Enum");
        REQUIRE(t->type->type == StackElement::Type::Enum);
    }
    {
        Function* f = code.luaWrapperFunctions[2];
        REQUIRE(f);

        CHECK(f->functionName == "foo3");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::CustomType);
        CHECK(at->size == 10);
        CustomType* t = static_cast<CustomType*>(at->type);
        CHECK(t->name == "Enum");
        REQUIRE(t->type->type == StackElement::Type::Enum);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
