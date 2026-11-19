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

TEST_CASE("Parsing/LuaWrapper/Arguments:  Variant<bool, int>", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] void func(std::variant<bool, int> arg) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions.front();
    REQUIRE(f);

    CHECK(f->functionName == "func");
    CHECK(f->documentation.empty());
    CHECK(f->returnValue == nullptr);
    REQUIRE(f->arguments.size() == 1);
    {
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "arg");
        REQUIRE(v->type);
        REQUIRE(v->type->tag == VariableType::Tag::VariantType);
        VariantType* ot = static_cast<VariantType*>(v->type);
        REQUIRE(ot->types.size() == 2);
        {
            VariableType* t = ot->types[0];
            REQUIRE(t->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(t);
            REQUIRE(bt->type == BasicType::Type::Bool);
        }
        {
            VariableType* t = ot->types[1];
            REQUIRE(t->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(t);
            REQUIRE(bt->type == BasicType::Type::Int);
        }

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
    "Parsing/LuaWrapper/Arguments:  Variant<double, float, string>",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] void func(std::variant<double, float, std::string> arg) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions.front();
    REQUIRE(f);

    CHECK(f->functionName == "func");
    CHECK(f->documentation.empty());
    CHECK(f->returnValue == nullptr);
    REQUIRE(f->arguments.size() == 1);
    {
        Variable* v = f->arguments[0];
        REQUIRE(v);
        CHECK(v->name == "arg");
        REQUIRE(v->type);
        REQUIRE(v->type->tag == VariableType::Tag::VariantType);
        VariantType* ot = static_cast<VariantType*>(v->type);
        REQUIRE(ot->types.size() == 3);
        {
            VariableType* t = ot->types[0];
            REQUIRE(t->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(t);
            REQUIRE(bt->type == BasicType::Type::Double);
        }
        {
            VariableType* t = ot->types[1];
            REQUIRE(t->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(t);
            REQUIRE(bt->type == BasicType::Type::Float);
        }
        {
            VariableType* t = ot->types[2];
            REQUIRE(t->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(t);
            REQUIRE(bt->type == BasicType::Type::String);
        }

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

TEST_CASE("Parsing/LuaWrapper/Arguments:  variant array", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] void func1(std::array<std::variant<double, float, std::string>, 1> arg) {
    }

    [[codegen::luawrap]] void func2(std::array<std::variant<double, float, std::string>, 5> arg) {
    }

    [[codegen::luawrap]] void func3(std::array<std::variant<double, float, std::string>, 10> arg) {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
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
            REQUIRE(at->type->tag == VariableType::Tag::VariantType);
            CHECK(at->size == 1);
            VariantType* ot = static_cast<VariantType*>(at->type);
            REQUIRE(ot->types.size() == 3);
            {
                VariableType* t = ot->types[0];
                REQUIRE(t->tag == VariableType::Tag::BasicType);
                BasicType* bt = static_cast<BasicType*>(t);
                REQUIRE(bt->type == BasicType::Type::Double);
            }
            {
                VariableType* t = ot->types[1];
                REQUIRE(t->tag == VariableType::Tag::BasicType);
                BasicType* bt = static_cast<BasicType*>(t);
                REQUIRE(bt->type == BasicType::Type::Float);
            }
            {
                VariableType* t = ot->types[2];
                REQUIRE(t->tag == VariableType::Tag::BasicType);
                BasicType* bt = static_cast<BasicType*>(t);
                REQUIRE(bt->type == BasicType::Type::String);
            }

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
            REQUIRE(at->type->tag == VariableType::Tag::VariantType);
            CHECK(at->size == 5);
            VariantType* ot = static_cast<VariantType*>(at->type);
            REQUIRE(ot->types.size() == 3);
            {
                VariableType* t = ot->types[0];
                REQUIRE(t->tag == VariableType::Tag::BasicType);
                BasicType* bt = static_cast<BasicType*>(t);
                REQUIRE(bt->type == BasicType::Type::Double);
            }
            {
                VariableType* t = ot->types[1];
                REQUIRE(t->tag == VariableType::Tag::BasicType);
                BasicType* bt = static_cast<BasicType*>(t);
                REQUIRE(bt->type == BasicType::Type::Float);
            }
            {
                VariableType* t = ot->types[2];
                REQUIRE(t->tag == VariableType::Tag::BasicType);
                BasicType* bt = static_cast<BasicType*>(t);
                REQUIRE(bt->type == BasicType::Type::String);
            }
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
            REQUIRE(at->type->tag == VariableType::Tag::VariantType);
            CHECK(at->size == 10);
            VariantType* ot = static_cast<VariantType*>(at->type);
            REQUIRE(ot->types.size() == 3);
            {
                VariableType* t = ot->types[0];
                REQUIRE(t->tag == VariableType::Tag::BasicType);
                BasicType* bt = static_cast<BasicType*>(t);
                REQUIRE(bt->type == BasicType::Type::Double);
            }
            {
                VariableType* t = ot->types[1];
                REQUIRE(t->tag == VariableType::Tag::BasicType);
                BasicType* bt = static_cast<BasicType*>(t);
                REQUIRE(bt->type == BasicType::Type::Float);
            }
            {
                VariableType* t = ot->types[2];
                REQUIRE(t->tag == VariableType::Tag::BasicType);
                BasicType* bt = static_cast<BasicType*>(t);
                REQUIRE(bt->type == BasicType::Type::String);
            }
        }
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Return:  variant bool", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] std::variant<bool> foo() {
        return true;
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->arguments.empty());
    VariableType* rt = f->returnValue;
    CHECK(rt->tag == VariableType::Tag::VariantType);
    VariantType* vt = static_cast<VariantType*>(rt);
    REQUIRE(vt->types.size() == 1);
    VariableType* v = vt->types[0];
    REQUIRE(v);
    REQUIRE(v->tag == VariableType::Tag::BasicType);
    BasicType* bt = static_cast<BasicType*>(v);
    CHECK(bt->type == BasicType::Type::Bool);

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Return:  variant int double", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] std::variant<int, double> foo() {
        return 1;
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->arguments.empty());
    VariableType* rt = f->returnValue;
    CHECK(rt->tag == VariableType::Tag::VariantType);
    VariantType* vt = static_cast<VariantType*>(rt);
    REQUIRE(vt->types.size() == 2);
    {
        VariableType* v = vt->types[0];
        REQUIRE(v);
        REQUIRE(v->tag == VariableType::Tag::BasicType);
        BasicType* bt = static_cast<BasicType*>(v);
        CHECK(bt->type == BasicType::Type::Int);
    }
    {
        VariableType* v = vt->types[1];
        REQUIRE(v);
        REQUIRE(v->tag == VariableType::Tag::BasicType);
        BasicType* bt = static_cast<BasicType*>(v);
        CHECK(bt->type == BasicType::Type::Double);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE(
    "Parsing/LuaWrapper/Return:  variant float string path",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] std::variant<float, std::string, std::filesystem::path> foo() {
        return 1.f;
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->arguments.empty());
    VariableType* rt = f->returnValue;
    CHECK(rt->tag == VariableType::Tag::VariantType);
    VariantType* vt = static_cast<VariantType*>(rt);
    REQUIRE(vt->types.size() == 3);
    {
        VariableType* v = vt->types[0];
        REQUIRE(v);
        REQUIRE(v->tag == VariableType::Tag::BasicType);
        BasicType* bt = static_cast<BasicType*>(v);
        CHECK(bt->type == BasicType::Type::Float);
    }
    {
        VariableType* v = vt->types[1];
        REQUIRE(v);
        REQUIRE(v->tag == VariableType::Tag::BasicType);
        BasicType* bt = static_cast<BasicType*>(v);
        CHECK(bt->type == BasicType::Type::String);
    }
    {
        VariableType* v = vt->types[2];
        REQUIRE(v);
        REQUIRE(v->tag == VariableType::Tag::BasicType);
        BasicType* bt = static_cast<BasicType*>(v);
        CHECK(bt->type == BasicType::Type::Path);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Return:  variant array", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] std::array<std::variant<float, std::string, std::filesystem::path>, 1> foo1() {
        return {};
    }

    [[codegen::luawrap]] std::array<std::variant<float, std::string, std::filesystem::path>, 5> foo2() {
        return {};
    }

    [[codegen::luawrap]] std::array<std::variant<float, std::string, std::filesystem::path>, 10> foo3() {
        return {};
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
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
        CHECK(at->type->tag == VariableType::Tag::VariantType);
        CHECK(at->size == 1);
        VariantType* vt = static_cast<VariantType*>(at->type);
        REQUIRE(vt->types.size() == 3);
        {
            VariableType* v = vt->types[0];
            REQUIRE(v);
            REQUIRE(v->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(v);
            CHECK(bt->type == BasicType::Type::Float);
        }
        {
            VariableType* v = vt->types[1];
            REQUIRE(v);
            REQUIRE(v->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(v);
            CHECK(bt->type == BasicType::Type::String);
        }
        {
            VariableType* v = vt->types[2];
            REQUIRE(v);
            REQUIRE(v->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(v);
            CHECK(bt->type == BasicType::Type::Path);
        }
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
        CHECK(at->type->tag == VariableType::Tag::VariantType);
        CHECK(at->size == 5);
        VariantType* vt = static_cast<VariantType*>(at->type);
        REQUIRE(vt->types.size() == 3);
        {
            VariableType* v = vt->types[0];
            REQUIRE(v);
            REQUIRE(v->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(v);
            CHECK(bt->type == BasicType::Type::Float);
        }
        {
            VariableType* v = vt->types[1];
            REQUIRE(v);
            REQUIRE(v->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(v);
            CHECK(bt->type == BasicType::Type::String);
        }
        {
            VariableType* v = vt->types[2];
            REQUIRE(v);
            REQUIRE(v->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(v);
            CHECK(bt->type == BasicType::Type::Path);
        }
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
        CHECK(at->type->tag == VariableType::Tag::VariantType);
        CHECK(at->size == 10);
        VariantType* vt = static_cast<VariantType*>(at->type);
        REQUIRE(vt->types.size() == 3);
        {
            VariableType* v = vt->types[0];
            REQUIRE(v);
            REQUIRE(v->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(v);
            CHECK(bt->type == BasicType::Type::Float);
        }
        {
            VariableType* v = vt->types[1];
            REQUIRE(v);
            REQUIRE(v->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(v);
            CHECK(bt->type == BasicType::Type::String);
        }
        {
            VariableType* v = vt->types[2];
            REQUIRE(v);
            REQUIRE(v->tag == VariableType::Tag::BasicType);
            BasicType* bt = static_cast<BasicType*>(v);
            CHECK(bt->type == BasicType::Type::Path);
        }
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
