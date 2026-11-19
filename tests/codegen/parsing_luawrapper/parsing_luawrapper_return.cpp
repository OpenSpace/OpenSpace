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

TEST_CASE("Parsing/LuaWrapper/Return:  tuple bool", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] std::tuple<bool> foo() {
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
    CHECK(rt->tag == VariableType::Tag::TupleType);
    TupleType* vt = static_cast<TupleType*>(rt);
    REQUIRE(vt->types.size() == 1);
    VariableType* v = vt->types[0];
    REQUIRE(v);
    REQUIRE(v->tag == VariableType::Tag::BasicType);
    BasicType* bt = static_cast<BasicType*>(v);
    CHECK(bt->type == BasicType::Type::Bool);

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Return:  tuple int double", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] std::tuple<int, double> foo() {
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
    CHECK(rt->tag == VariableType::Tag::TupleType);
    TupleType* vt = static_cast<TupleType*>(rt);
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

TEST_CASE("Parsing/LuaWrapper/Return:  tuple float string path", "[Parsing][LuaWrapper]")
{
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] std::tuple<float, std::string, std::filesystem::path> foo() {
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
    CHECK(rt->tag == VariableType::Tag::TupleType);
    TupleType* vt = static_cast<TupleType*>(rt);
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
