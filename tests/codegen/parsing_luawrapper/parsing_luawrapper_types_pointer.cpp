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

TEST_CASE("Parsing/LuaWrapper/Arguments:  Pointer", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};

    [[codegen::luawrap]] void funcVoid(void* arg) {
    }

    [[codegen::luawrap]] void funcInt(int* arg) {
    }

    [[codegen::luawrap]] void funcString(std::string* arg) {
    }

    [[codegen::luawrap]] void funcStruct(Foo* arg) {
    }

    [[codegen::luawrap]] void funcStruct2(Foo** arg) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 5);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(v->type);
            CHECK(generateTypename(pt) == "void*");

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

        CHECK(f->functionName == "funcInt");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(v->type);
            CHECK(generateTypename(pt) == "int*");

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

        CHECK(f->functionName == "funcString");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(v->type);
            CHECK(generateTypename(pt) == "std::string*");

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
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(v->type);
            CHECK(generateTypename(pt) == "Foo*");

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
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct2");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(v->type);
            CHECK(generateTypename(pt) == "Foo**");

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

TEST_CASE("Parsing/LuaWrapper/Arguments:  Pointer defaulted", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};

    [[codegen::luawrap]] void funcVoid(void* arg = nullptr) {
    }

    [[codegen::luawrap]] void funcInt(int* arg = nullptr) {
    }

    [[codegen::luawrap]] void funcString(std::string* arg = nullptr) {
    }

    [[codegen::luawrap]] void funcStruct(Foo* arg = nullptr) {
    }

    [[codegen::luawrap]] void funcStruct2(Foo** arg = nullptr) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 5);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "void*");

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

        CHECK(f->functionName == "funcInt");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "int*");

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

        CHECK(f->functionName == "funcString");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "std::string*");

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
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "Foo*");

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
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct2");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "Foo**");

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

TEST_CASE("Parsing/LuaWrapper/Arguments:  Pointer map", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};

    [[codegen::luawrap]] void funcVoid(std::map<std::string, void*> arg) {
    }

    [[codegen::luawrap]] void funcInt(std::map<std::string, int*> arg) {
    }

    [[codegen::luawrap]] void funcString(std::map<std::string, std::string*> arg) {
    }

    [[codegen::luawrap]] void funcStruct(std::map<std::string, Foo*> arg) {
    }

    [[codegen::luawrap]] void funcStruct2(std::map<std::string, Foo**> arg) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 5);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::MapType);
            MapType* mt = static_cast<MapType*>(v->type);
            REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
            CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
            REQUIRE(mt->valueType->isPointerType());
            PointerType* pt = static_cast<PointerType*>(mt->valueType);
            CHECK(generateTypename(pt) == "void*");

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

        CHECK(f->functionName == "funcInt");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::MapType);
            MapType* mt = static_cast<MapType*>(v->type);
            REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
            CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
            REQUIRE(mt->valueType->isPointerType());
            PointerType* pt = static_cast<PointerType*>(mt->valueType);
            CHECK(generateTypename(pt) == "int*");

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

        CHECK(f->functionName == "funcString");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::MapType);
            MapType* mt = static_cast<MapType*>(v->type);
            REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
            CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
            REQUIRE(mt->valueType->isPointerType());
            PointerType* pt = static_cast<PointerType*>(mt->valueType);
            CHECK(generateTypename(pt) == "std::string*");

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
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::MapType);
            MapType* mt = static_cast<MapType*>(v->type);
            REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
            CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
            REQUIRE(mt->valueType->isPointerType());
            PointerType* pt = static_cast<PointerType*>(mt->valueType);
            CHECK(generateTypename(pt) == "Foo*");

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
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct2");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::MapType);
            MapType* mt = static_cast<MapType*>(v->type);
            REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
            CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
            REQUIRE(mt->valueType->isPointerType());
            PointerType* pt = static_cast<PointerType*>(mt->valueType);
            CHECK(generateTypename(pt) == "Foo**");

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

TEST_CASE("Parsing/LuaWrapper/Arguments:  Pointer optional", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};

    [[codegen::luawrap]] void funcVoid(std::optional<void*> arg) {
    }

    [[codegen::luawrap]] void funcInt(std::optional<int*> arg) {
    }

    [[codegen::luawrap]] void funcString(std::optional<std::string*> arg) {
    }

    [[codegen::luawrap]] void funcStruct(std::optional<Foo*> arg) {
    }

    [[codegen::luawrap]] void funcStruct2(std::optional<Foo**> arg) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 5);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "void*");

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

        CHECK(f->functionName == "funcInt");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "int*");

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

        CHECK(f->functionName == "funcString");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "std::string*");

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
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "Foo*");

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
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct2");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::OptionalType);
            OptionalType* ot = static_cast<OptionalType*>(v->type);
            REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(ot->type);
            CHECK(generateTypename(pt) == "Foo**");

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

TEST_CASE("Parsing/LuaWrapper/Arguments:  Pointer vector", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};

    [[codegen::luawrap]] void funcVoid(std::vector<void*> arg) {
    }

    [[codegen::luawrap]] void funcInt(std::vector<int*> arg) {
    }

    [[codegen::luawrap]] void funcString(std::vector<std::string*> arg) {
    }

    [[codegen::luawrap]] void funcStruct(std::vector<Foo*> arg) {
    }

    [[codegen::luawrap]] void funcStruct2(std::vector<Foo**> arg) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 5);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::VectorType);
            VectorType* vt = static_cast<VectorType*>(v->type);
            REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(vt->type);
            CHECK(generateTypename(pt) == "void*");

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

        CHECK(f->functionName == "funcInt");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::VectorType);
            VectorType* vt = static_cast<VectorType*>(v->type);
            REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(vt->type);
            CHECK(generateTypename(pt) == "int*");

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

        CHECK(f->functionName == "funcString");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::VectorType);
            VectorType* vt = static_cast<VectorType*>(v->type);
            REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(vt->type);
            CHECK(generateTypename(pt) == "std::string*");

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
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::VectorType);
            VectorType* vt = static_cast<VectorType*>(v->type);
            REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(vt->type);
            CHECK(generateTypename(pt) == "Foo*");

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
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct2");
        CHECK(f->documentation.empty());
        CHECK(f->returnValue == nullptr);
        REQUIRE(f->arguments.size() == 1);
        {
            Variable* v = f->arguments[0];
            REQUIRE(v);
            CHECK(v->name == "arg");
            REQUIRE(v->type);
            REQUIRE(v->type->tag == VariableType::Tag::VectorType);
            VectorType* vt = static_cast<VectorType*>(v->type);
            REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
            PointerType* pt = static_cast<PointerType*>(vt->type);
            CHECK(generateTypename(pt) == "Foo**");

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

TEST_CASE("Parsing/LuaWrapper/Arguments:  Pointer array", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};
    
    [[codegen::luawrap]] void funcVoid1(std::array<void*, 1> arg) {
    }

    [[codegen::luawrap]] void funcVoid2(std::array<void*, 5> arg) {
    }

    [[codegen::luawrap]] void funcVoid3(std::array<void*, 10> arg) {
    }

    [[codegen::luawrap]] void funcInt1(std::array<int*, 1> arg) {
    }

    [[codegen::luawrap]] void funcInt2(std::array<int*, 5> arg) {
    }

    [[codegen::luawrap]] void funcInt3(std::array<int*, 10> arg) {
    }

    [[codegen::luawrap]] void funcString1(std::array<std::string*, 1> arg) {
    }

    [[codegen::luawrap]] void funcString2(std::array<std::string*, 5> arg) {
    }

    [[codegen::luawrap]] void funcString3(std::array<std::string*, 10> arg) {
    }

    [[codegen::luawrap]] void funcFoo1(std::array<Foo*, 1> arg) {
    }

    [[codegen::luawrap]] void funcFoo2(std::array<Foo*, 5> arg) {
    }

    [[codegen::luawrap]] void funcFoo3(std::array<Foo*, 10> arg) {
    }

    [[codegen::luawrap]] void funcFooS1(std::array<Foo**, 1> arg) {
    }

    [[codegen::luawrap]] void funcFooS2(std::array<Foo**, 5> arg) {
    }

    [[codegen::luawrap]] void funcFooS3(std::array<Foo**, 10> arg) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 15);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid1");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 1);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "void");

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

        CHECK(f->functionName == "funcVoid2");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 5);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "void");

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

        CHECK(f->functionName == "funcVoid3");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 10);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "void");

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
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt1");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 1);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "int");

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
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt2");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 5);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "int");

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
        Function* f = code.luaWrapperFunctions[5];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt3");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 10);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "int");

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
        Function* f = code.luaWrapperFunctions[6];
        REQUIRE(f);

        CHECK(f->functionName == "funcString1");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 1);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "std::string");

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
        Function* f = code.luaWrapperFunctions[7];
        REQUIRE(f);

        CHECK(f->functionName == "funcString2");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 5);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "std::string");

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
        Function* f = code.luaWrapperFunctions[8];
        REQUIRE(f);

        CHECK(f->functionName == "funcString3");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 10);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "std::string");

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
        Function* f = code.luaWrapperFunctions[9];
        REQUIRE(f);

        CHECK(f->functionName == "funcFoo1");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 1);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "Foo");

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
        Function* f = code.luaWrapperFunctions[10];
        REQUIRE(f);

        CHECK(f->functionName == "funcFoo2");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 5);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "Foo");

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
        Function* f = code.luaWrapperFunctions[11];
        REQUIRE(f);

        CHECK(f->functionName == "funcFoo3");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 10);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "Foo");

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
        Function* f = code.luaWrapperFunctions[12];
        REQUIRE(f);

        CHECK(f->functionName == "funcFooS1");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 1);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "Foo*");

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
        Function* f = code.luaWrapperFunctions[13];
        REQUIRE(f);

        CHECK(f->functionName == "funcFooS2");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 5);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "Foo*");

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
        Function* f = code.luaWrapperFunctions[14];
        REQUIRE(f);

        CHECK(f->functionName == "funcFooS3");
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
            REQUIRE(at->type->tag == VariableType::Tag::PointerType);
            CHECK(at->size == 10);
            PointerType* pt = static_cast<PointerType*>(at->type);
            CHECK(pt->type == "Foo*");

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

TEST_CASE("Parsing/LuaWrapper/Return:  pointer", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};

    [[codegen::luawrap]] void* funcVoid() {
    }

    [[codegen::luawrap]] int* funcInt() {
    }

    [[codegen::luawrap]] std::string* funcString() {
    }

    [[codegen::luawrap]] Foo* funcStruct() {
    }

    [[codegen::luawrap]] Foo** funcStruct2() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 5);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(rt);
        CHECK(generateTypename(pt) == "void*");
    }
    {
        Function* f = code.luaWrapperFunctions[1];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(rt);
        CHECK(generateTypename(pt) == "int*");
    }
    {
        Function* f = code.luaWrapperFunctions[2];
        REQUIRE(f);

        CHECK(f->functionName == "funcString");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(rt);
        CHECK(generateTypename(pt) == "std::string*");
    }
    {
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(rt);
        CHECK(generateTypename(pt) == "Foo*");
    }
    {
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(rt);
        CHECK(generateTypename(pt) == "Foo**");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Return:  pointer map", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};

    [[codegen::luawrap]] std::map<std::string, void*> funcVoid() {
    }

    [[codegen::luawrap]] std::map<std::string, int*> funcInt() {
    }

    [[codegen::luawrap]] std::map<std::string, std::string*> funcString() {
    }

    [[codegen::luawrap]] std::map<std::string, Foo*> funcStruct() {
    }

    [[codegen::luawrap]] std::map<std::string, Foo**> funcStruct2() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 5);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::MapType);
        MapType* mt = static_cast<MapType*>(rt);
        REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
        CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
        CHECK(mt->valueType->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(mt->valueType);
        CHECK(generateTypename(pt) == "void*");
    }
    {
        Function* f = code.luaWrapperFunctions[1];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::MapType);
        MapType* mt = static_cast<MapType*>(rt);
        REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
        CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
        CHECK(mt->valueType->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(mt->valueType);
        CHECK(generateTypename(pt) == "int*");
    }
    {
        Function* f = code.luaWrapperFunctions[2];
        REQUIRE(f);

        CHECK(f->functionName == "funcString");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::MapType);
        MapType* mt = static_cast<MapType*>(rt);
        REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
        CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
        CHECK(mt->valueType->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(mt->valueType);
        CHECK(generateTypename(pt) == "std::string*");
    }
    {
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::MapType);
        MapType* mt = static_cast<MapType*>(rt);
        REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
        CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
        CHECK(mt->valueType->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(mt->valueType);
        CHECK(generateTypename(pt) == "Foo*");
    }
    {
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::MapType);
        MapType* mt = static_cast<MapType*>(rt);
        REQUIRE(mt->keyType->tag == VariableType::Tag::BasicType);
        CHECK(static_cast<BasicType*>(mt->keyType)->type == BasicType::Type::String);
        CHECK(mt->valueType->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(mt->valueType);
        CHECK(generateTypename(pt) == "Foo**");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Return:  pointer optional", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};

    [[codegen::luawrap]] std::optional<void*> funcVoid() {
    }

    [[codegen::luawrap]] std::optional<int*> funcInt() {
    }

    [[codegen::luawrap]] std::optional<std::string*> funcString() {
    }

    [[codegen::luawrap]] std::optional<Foo*> funcStruct() {
    }

    [[codegen::luawrap]] std::optional<Foo**> funcStruct2() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 5);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::OptionalType);
        OptionalType* ot = static_cast<OptionalType*>(rt);
        REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(ot->type);
        CHECK(generateTypename(pt) == "void*");
    }
    {
        Function* f = code.luaWrapperFunctions[1];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::OptionalType);
        OptionalType* ot = static_cast<OptionalType*>(rt);
        REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(ot->type);
        CHECK(generateTypename(pt) == "int*");
    }
    {
        Function* f = code.luaWrapperFunctions[2];
        REQUIRE(f);

        CHECK(f->functionName == "funcString");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::OptionalType);
        OptionalType* ot = static_cast<OptionalType*>(rt);
        REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(ot->type);
        CHECK(generateTypename(pt) == "std::string*");
    }
    {
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::OptionalType);
        OptionalType* ot = static_cast<OptionalType*>(rt);
        REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(ot->type);
        CHECK(generateTypename(pt) == "Foo*");
    }
    {
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::OptionalType);
        OptionalType* ot = static_cast<OptionalType*>(rt);
        REQUIRE(ot->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(ot->type);
        CHECK(generateTypename(pt) == "Foo**");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Return:  pointer vector", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    struct Foo {};

    [[codegen::luawrap]] std::vector<void*> funcVoid() {
    }

    [[codegen::luawrap]] std::vector<int*> funcInt() {
    }

    [[codegen::luawrap]] std::vector<std::string*> funcString() {
    }

    [[codegen::luawrap]] std::vector<Foo*> funcStruct() {
    }

    [[codegen::luawrap]] std::vector<Foo**> funcStruct2() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 5);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::VectorType);
        VectorType* vt = static_cast<VectorType*>(rt);
        REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(vt->type);
        CHECK(generateTypename(pt) == "void*");
    }
    {
        Function* f = code.luaWrapperFunctions[1];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::VectorType);
        VectorType* vt = static_cast<VectorType*>(rt);
        REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(vt->type);
        CHECK(generateTypename(pt) == "int*");
    }
    {
        Function* f = code.luaWrapperFunctions[2];
        REQUIRE(f);

        CHECK(f->functionName == "funcString");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::VectorType);
        VectorType* vt = static_cast<VectorType*>(rt);
        REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(vt->type);
        CHECK(generateTypename(pt) == "std::string*");
    }
    {
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::VectorType);
        VectorType* vt = static_cast<VectorType*>(rt);
        REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(vt->type);
        CHECK(generateTypename(pt) == "Foo*");
    }
    {
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcStruct2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        REQUIRE(rt->tag == VariableType::Tag::VectorType);
        VectorType* vt = static_cast<VectorType*>(rt);
        REQUIRE(vt->type->tag == VariableType::Tag::PointerType);
        PointerType* pt = static_cast<PointerType*>(vt->type);
        CHECK(generateTypename(pt) == "Foo**");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Return:  pointer array", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] std::array<void*, 1> funcVoid1() {
        return {};
    }

    [[codegen::luawrap]] std::array<void*, 5> funcVoid2() {
        return {};
    }

    [[codegen::luawrap]] std::array<void*, 10> funcVoid3() {
        return {};
    }

    [[codegen::luawrap]] std::array<int*, 1> funcInt1() {
        return {};
    }

    [[codegen::luawrap]] std::array<int*, 5> funcInt2() {
        return {};
    }

    [[codegen::luawrap]] std::array<int*, 10> funcInt3() {
        return {};
    }

    [[codegen::luawrap]] std::array<std::string*, 1> funcString1() {
        return {};
    }

    [[codegen::luawrap]] std::array<std::string*, 5> funcString2() {
        return {};
    }

    [[codegen::luawrap]] std::array<std::string*, 10> funcString3() {
        return {};
    }

    [[codegen::luawrap]] std::array<Foo*, 1> funcFoo1() {
        return {};
    }

    [[codegen::luawrap]] std::array<Foo*, 5> funcFoo2() {
        return {};
    }

    [[codegen::luawrap]] std::array<Foo*, 10> funcFoo3() {
        return {};
    }

    [[codegen::luawrap]] std::array<Foo**, 1> funcFooS1() {
        return {};
    }

    [[codegen::luawrap]] std::array<Foo**, 5> funcFooS2() {
        return {};
    }

    [[codegen::luawrap]] std::array<Foo**, 10> funcFooS3() {
        return {};
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 15);
    {
        Function* f = code.luaWrapperFunctions[0];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid1");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 1);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "void");
    }
    {
        Function* f = code.luaWrapperFunctions[1];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 5);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "void");
    }
    {
        Function* f = code.luaWrapperFunctions[2];
        REQUIRE(f);

        CHECK(f->functionName == "funcVoid3");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 10);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "void");
    }
    {
        Function* f = code.luaWrapperFunctions[3];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt1");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 1);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "int");
    }
    {
        Function* f = code.luaWrapperFunctions[4];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 5);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "int");
    }
    {
        Function* f = code.luaWrapperFunctions[5];
        REQUIRE(f);

        CHECK(f->functionName == "funcInt3");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 10);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "int");
    }
    {
        Function* f = code.luaWrapperFunctions[6];
        REQUIRE(f);

        CHECK(f->functionName == "funcString1");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 1);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "std::string");
    }
    {
        Function* f = code.luaWrapperFunctions[7];
        REQUIRE(f);

        CHECK(f->functionName == "funcString2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 5);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "std::string");
    }
    {
        Function* f = code.luaWrapperFunctions[8];
        REQUIRE(f);

        CHECK(f->functionName == "funcString3");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 10);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "std::string");
    }
    {
        Function* f = code.luaWrapperFunctions[9];
        REQUIRE(f);

        CHECK(f->functionName == "funcFoo1");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 1);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "Foo");
    }
    {
        Function* f = code.luaWrapperFunctions[10];
        REQUIRE(f);

        CHECK(f->functionName == "funcFoo2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 5);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "Foo");
    }
    {
        Function* f = code.luaWrapperFunctions[11];
        REQUIRE(f);

        CHECK(f->functionName == "funcFoo3");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 10);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "Foo");
    }
    {
        Function* f = code.luaWrapperFunctions[12];
        REQUIRE(f);

        CHECK(f->functionName == "funcFooS1");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 1);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "Foo*");
    }
    {
        Function* f = code.luaWrapperFunctions[13];
        REQUIRE(f);

        CHECK(f->functionName == "funcFooS2");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 5);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "Foo*");
    }
    {
        Function* f = code.luaWrapperFunctions[14];
        REQUIRE(f);

        CHECK(f->functionName == "funcFooS3");
        CHECK(f->documentation.empty());
        CHECK(f->arguments.empty());
        VariableType* rt = f->returnValue;
        CHECK(rt->tag == VariableType::Tag::ArrayType);
        ArrayType* at = static_cast<ArrayType*>(rt);
        REQUIRE(at->type->tag == VariableType::Tag::PointerType);
        CHECK(at->size == 10);
        PointerType* pt = static_cast<PointerType*>(at->type);
        CHECK(pt->type == "Foo*");
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
