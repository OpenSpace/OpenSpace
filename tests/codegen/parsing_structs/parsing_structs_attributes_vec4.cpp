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

TEST_CASE("Parsing/Structs/Attributes/Vec4") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(Attributes)]] Parameters {
        // inRangeValueVec4 documentation
        glm::vec4 inRangeValueVec4 [[codegen::inrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // inRangeValueVec4Optional documentation
        std::optional<glm::vec4> inRangeValueVec4Optional
            [[codegen::inrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // inRangeValueVec4Vector documentation
        std::vector<glm::vec4> inRangeValueVec4Vector
            [[codegen::inrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // notInRangeValueVec4 documentation
        glm::vec4 notInRangeValueVec4
            [[codegen::notinrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // notInRangeValueVec4Optional documentation
        std::optional<glm::vec4> notInRangeValueVec4Optional
            [[codegen::notinrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // notInRangeValueVec4Vector documentation
        std::vector<glm::vec4> notInRangeValueVec4Vector
            [[codegen::notinrange(glm::vec4(1.f), glm::vec4(2.f))]];

        // lessValueVec4 documentation
        glm::vec4 lessValueVec4 [[codegen::less(glm::vec4(1.f))]];

        // lessValueVec4Optional documentation
        std::optional<glm::vec4> lessValueVec4Optional [[codegen::less(glm::vec4(1.f))]];

        // lessValueVec4Vector documentation
        std::vector<glm::vec4> lessValueVec4Vector [[codegen::less(glm::vec4(1.f))]];

        // lessEqualValueVec4 documentation
        glm::vec4 lessEqualValueVec4 [[codegen::lessequal(glm::vec4(1.f))]];

        // lessEqualValueVec4Optional documentation
        std::optional<glm::vec4> lessEqualValueVec4Optional
            [[codegen::lessequal(glm::vec4(1.f))]];

        // lessEqualValueVec4Vector documentation
        std::vector<glm::vec4> lessEqualValueVec4Vector
            [[codegen::lessequal(glm::vec4(1.f))]];

        // greaterValueVec4 documentation
        glm::vec4 greaterValueVec4 [[codegen::greater(glm::vec4(1.f))]];

        // greaterValueVec4Optional documentation
        std::optional<glm::vec4> greaterValueVec4Optional
            [[codegen::greater(glm::vec4(1.f))]];

        // greaterValueVec4Vector documentation
        std::vector<glm::vec4> greaterValueVec4Vector
            [[codegen::greater(glm::vec4(1.f))]];

        // greaterEqualValueVec4 documentation
        glm::vec4 greaterEqualValueVec4 [[codegen::greaterequal(glm::vec4(1.f))]];

        // greaterEqualValueVec4Optional documentation
        std::optional<glm::vec4> greaterEqualValueVec4Optional
            [[codegen::greaterequal(glm::vec4(1.f))]];

        // greaterEqualValueVec4Vector documentation
        std::vector<glm::vec4> greaterEqualValueVec4Vector
            [[codegen::greaterequal(glm::vec4(1.f))]];

        // unequalValueVec4 documentation
        glm::vec4 unequalValueVec4 [[codegen::unequal(glm::vec4(1.f))]];

        // unequalValueVec4Optional documentation
        std::optional<glm::vec4> unequalValueVec4Optional
            [[codegen::unequal(glm::vec4(1.f))]];

        // unequalValueVec4Vector documentation
        std::vector<glm::vec4> unequalValueVec4Vector
            [[codegen::unequal(glm::vec4(1.f))]];

        // inRangeValueVec4Private documentation
        glm::vec4 inRangeValueVec4Private [[codegen::inrange(glm::vec4(1.f), glm::vec4(2.f)), codegen::private()]];

        // inRangeValueVec4OptionalPrivate documentation
        std::optional<glm::vec4> inRangeValueVec4OptionalPrivate
            [[codegen::inrange(glm::vec4(1.f), glm::vec4(2.f)), codegen::private()]];

        // inRangeValueVec4VectorPrivate documentation
        std::vector<glm::vec4> inRangeValueVec4VectorPrivate
            [[codegen::inrange(glm::vec4(1.f), glm::vec4(2.f)), codegen::private()]];

        // notInRangeValueVec4Private documentation
        glm::vec4 notInRangeValueVec4Private
            [[codegen::notinrange(glm::vec4(1.f), glm::vec4(2.f)), codegen::private()]];

        // notInRangeValueVec4OptionalPrivate documentation
        std::optional<glm::vec4> notInRangeValueVec4OptionalPrivate
            [[codegen::notinrange(glm::vec4(1.f), glm::vec4(2.f)), codegen::private()]];

        // notInRangeValueVec4VectorPrivate documentation
        std::vector<glm::vec4> notInRangeValueVec4VectorPrivate
            [[codegen::notinrange(glm::vec4(1.f), glm::vec4(2.f)), codegen::private()]];

        // lessValueVec4Private documentation
        glm::vec4 lessValueVec4Private [[codegen::less(glm::vec4(1.f)), codegen::private()]];

        // lessValueVec4OptionalPrivate documentation
        std::optional<glm::vec4> lessValueVec4OptionalPrivate [[codegen::less(glm::vec4(1.f)), codegen::private()]];

        // lessValueVec4VectorPrivate documentation
        std::vector<glm::vec4> lessValueVec4VectorPrivate [[codegen::less(glm::vec4(1.f)), codegen::private()]];

        // lessEqualValueVec4Private documentation
        glm::vec4 lessEqualValueVec4Private [[codegen::lessequal(glm::vec4(1.f)), codegen::private()]];

        // lessEqualValueVec4OptionalPrivate documentation
        std::optional<glm::vec4> lessEqualValueVec4OptionalPrivate
            [[codegen::lessequal(glm::vec4(1.f)), codegen::private()]];

        // lessEqualValueVec4VectorPrivate documentation
        std::vector<glm::vec4> lessEqualValueVec4VectorPrivate
            [[codegen::lessequal(glm::vec4(1.f)), codegen::private()]];

        // greaterValueVec4Private documentation
        glm::vec4 greaterValueVec4Private [[codegen::greater(glm::vec4(1.f)), codegen::private()]];

        // greaterValueVec4OptionalPrivate documentation
        std::optional<glm::vec4> greaterValueVec4OptionalPrivate
            [[codegen::greater(glm::vec4(1.f)), codegen::private()]];

        // greaterValueVec4VectorPrivate documentation
        std::vector<glm::vec4> greaterValueVec4VectorPrivate
            [[codegen::greater(glm::vec4(1.f)), codegen::private()]];

        // greaterEqualValueVec4Private documentation
        glm::vec4 greaterEqualValueVec4Private [[codegen::greaterequal(glm::vec4(1.f)), codegen::private()]];

        // greaterEqualValueVec4OptionalPrivate documentation
        std::optional<glm::vec4> greaterEqualValueVec4OptionalPrivate
            [[codegen::greaterequal(glm::vec4(1.f)), codegen::private()]];

        // greaterEqualValueVec4VectorPrivate documentation
        std::vector<glm::vec4> greaterEqualValueVec4VectorPrivate
            [[codegen::greaterequal(glm::vec4(1.f)), codegen::private()]];

        // unequalValueVec4Private documentation
        glm::vec4 unequalValueVec4Private [[codegen::unequal(glm::vec4(1.f)), codegen::private()]];

        // unequalValueVec4OptionalPrivate documentation
        std::optional<glm::vec4> unequalValueVec4OptionalPrivate
            [[codegen::unequal(glm::vec4(1.f)), codegen::private()]];

        // unequalValueVec4VectorPrivate documentation
        std::vector<glm::vec4> unequalValueVec4VectorPrivate
            [[codegen::unequal(glm::vec4(1.f)), codegen::private()]];
})";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    CHECK(code.enums.empty());
    CHECK(code.luaWrapperFunctions.empty());
    Struct* s = code.structs.front();
    REQUIRE(s);

    CHECK(s->name == "Parameters");
    CHECK(s->comment.empty());
    CHECK(s->attributes.dictionary == "Attributes");
    CHECK(s->attributes.noExhaustive);
    CHECK(s->parent == nullptr);
    CHECK(s->children.empty());
    REQUIRE(s->variables.size() == 42);

    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "inRangeValueVec4");
        CHECK(var->key == "\"InRangeValueVec4\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "inRangeValueVec4 documentation");
        CHECK(var->attributes.inrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[1];
        REQUIRE(var);
        CHECK(var->name == "inRangeValueVec4Optional");
        CHECK(var->key == "\"InRangeValueVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "inRangeValueVec4Optional documentation");
        CHECK(var->attributes.inrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[2];
        REQUIRE(var);
        CHECK(var->name == "inRangeValueVec4Vector");
        CHECK(var->key == "\"InRangeValueVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "inRangeValueVec4Vector documentation");
        CHECK(var->attributes.inrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[3];
        REQUIRE(var);
        CHECK(var->name == "notInRangeValueVec4");
        CHECK(var->key == "\"NotInRangeValueVec4\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "notInRangeValueVec4 documentation");
        CHECK(var->attributes.notinrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[4];
        REQUIRE(var);
        CHECK(var->name == "notInRangeValueVec4Optional");
        CHECK(var->key == "\"NotInRangeValueVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "notInRangeValueVec4Optional documentation");
        CHECK(var->attributes.notinrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[5];
        REQUIRE(var);
        CHECK(var->name == "notInRangeValueVec4Vector");
        CHECK(var->key == "\"NotInRangeValueVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "notInRangeValueVec4Vector documentation");
        CHECK(var->attributes.notinrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "lessValueVec4");
        CHECK(var->key == "\"LessValueVec4\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "lessValueVec4 documentation");
        CHECK(var->attributes.less == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[7];
        REQUIRE(var);
        CHECK(var->name == "lessValueVec4Optional");
        CHECK(var->key == "\"LessValueVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "lessValueVec4Optional documentation");
        CHECK(var->attributes.less == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[8];
        REQUIRE(var);
        CHECK(var->name == "lessValueVec4Vector");
        CHECK(var->key == "\"LessValueVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "lessValueVec4Vector documentation");
        CHECK(var->attributes.less == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[9];
        REQUIRE(var);
        CHECK(var->name == "lessEqualValueVec4");
        CHECK(var->key == "\"LessEqualValueVec4\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "lessEqualValueVec4 documentation");
        CHECK(var->attributes.lessequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[10];
        REQUIRE(var);
        CHECK(var->name == "lessEqualValueVec4Optional");
        CHECK(var->key == "\"LessEqualValueVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "lessEqualValueVec4Optional documentation");
        CHECK(var->attributes.lessequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[11];
        REQUIRE(var);
        CHECK(var->name == "lessEqualValueVec4Vector");
        CHECK(var->key == "\"LessEqualValueVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "lessEqualValueVec4Vector documentation");
        CHECK(var->attributes.lessequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[12];
        REQUIRE(var);
        CHECK(var->name == "greaterValueVec4");
        CHECK(var->key == "\"GreaterValueVec4\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "greaterValueVec4 documentation");
        CHECK(var->attributes.greater == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[13];
        REQUIRE(var);
        CHECK(var->name == "greaterValueVec4Optional");
        CHECK(var->key == "\"GreaterValueVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "greaterValueVec4Optional documentation");
        CHECK(var->attributes.greater == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[14];
        REQUIRE(var);
        CHECK(var->name == "greaterValueVec4Vector");
        CHECK(var->key == "\"GreaterValueVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "greaterValueVec4Vector documentation");
        CHECK(var->attributes.greater == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[15];
        REQUIRE(var);
        CHECK(var->name == "greaterEqualValueVec4");
        CHECK(var->key == "\"GreaterEqualValueVec4\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "greaterEqualValueVec4 documentation");
        CHECK(var->attributes.greaterequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[16];
        REQUIRE(var);
        CHECK(var->name == "greaterEqualValueVec4Optional");
        CHECK(var->key == "\"GreaterEqualValueVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "greaterEqualValueVec4Optional documentation");
        CHECK(var->attributes.greaterequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[17];
        REQUIRE(var);
        CHECK(var->name == "greaterEqualValueVec4Vector");
        CHECK(var->key == "\"GreaterEqualValueVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "greaterEqualValueVec4Vector documentation");
        CHECK(var->attributes.greaterequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[18];
        REQUIRE(var);
        CHECK(var->name == "unequalValueVec4");
        CHECK(var->key == "\"UnequalValueVec4\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "unequalValueVec4 documentation");
        CHECK(var->attributes.unequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "unequalValueVec4Optional");
        CHECK(var->key == "\"UnequalValueVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "unequalValueVec4Optional documentation");
        CHECK(var->attributes.unequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "unequalValueVec4Vector");
        CHECK(var->key == "\"UnequalValueVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "unequalValueVec4Vector documentation");
        CHECK(var->attributes.unequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "inRangeValueVec4Private");
        CHECK(var->key == "\"InRangeValueVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "inRangeValueVec4Private documentation");
        CHECK(var->attributes.inrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "inRangeValueVec4OptionalPrivate");
        CHECK(var->key == "\"InRangeValueVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "inRangeValueVec4OptionalPrivate documentation");
        CHECK(var->attributes.inrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "inRangeValueVec4VectorPrivate");
        CHECK(var->key == "\"InRangeValueVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "inRangeValueVec4VectorPrivate documentation");
        CHECK(var->attributes.inrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "notInRangeValueVec4Private");
        CHECK(var->key == "\"NotInRangeValueVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "notInRangeValueVec4Private documentation");
        CHECK(var->attributes.notinrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "notInRangeValueVec4OptionalPrivate");
        CHECK(var->key == "\"NotInRangeValueVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "notInRangeValueVec4OptionalPrivate documentation");
        CHECK(var->attributes.notinrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "notInRangeValueVec4VectorPrivate");
        CHECK(var->key == "\"NotInRangeValueVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "notInRangeValueVec4VectorPrivate documentation");
        CHECK(var->attributes.notinrange == "glm::vec4(1.f), glm::vec4(2.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "lessValueVec4Private");
        CHECK(var->key == "\"LessValueVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "lessValueVec4Private documentation");
        CHECK(var->attributes.less == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "lessValueVec4OptionalPrivate");
        CHECK(var->key == "\"LessValueVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "lessValueVec4OptionalPrivate documentation");
        CHECK(var->attributes.less == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "lessValueVec4VectorPrivate");
        CHECK(var->key == "\"LessValueVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "lessValueVec4VectorPrivate documentation");
        CHECK(var->attributes.less == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "lessEqualValueVec4Private");
        CHECK(var->key == "\"LessEqualValueVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "lessEqualValueVec4Private documentation");
        CHECK(var->attributes.lessequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "lessEqualValueVec4OptionalPrivate");
        CHECK(var->key == "\"LessEqualValueVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "lessEqualValueVec4OptionalPrivate documentation");
        CHECK(var->attributes.lessequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "lessEqualValueVec4VectorPrivate");
        CHECK(var->key == "\"LessEqualValueVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "lessEqualValueVec4VectorPrivate documentation");
        CHECK(var->attributes.lessequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "greaterValueVec4Private");
        CHECK(var->key == "\"GreaterValueVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "greaterValueVec4Private documentation");
        CHECK(var->attributes.greater == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "greaterValueVec4OptionalPrivate");
        CHECK(var->key == "\"GreaterValueVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "greaterValueVec4OptionalPrivate documentation");
        CHECK(var->attributes.greater == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "greaterValueVec4VectorPrivate");
        CHECK(var->key == "\"GreaterValueVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "greaterValueVec4VectorPrivate documentation");
        CHECK(var->attributes.greater == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "greaterEqualValueVec4Private");
        CHECK(var->key == "\"GreaterEqualValueVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "greaterEqualValueVec4Private documentation");
        CHECK(var->attributes.greaterequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "greaterEqualValueVec4OptionalPrivate");
        CHECK(var->key == "\"GreaterEqualValueVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "greaterEqualValueVec4OptionalPrivate documentation");
        CHECK(var->attributes.greaterequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[38];
        REQUIRE(var);
        CHECK(var->name == "greaterEqualValueVec4VectorPrivate");
        CHECK(var->key == "\"GreaterEqualValueVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "greaterEqualValueVec4VectorPrivate documentation");
        CHECK(var->attributes.greaterequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[39];
        REQUIRE(var);
        CHECK(var->name == "unequalValueVec4Private");
        CHECK(var->key == "\"UnequalValueVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::vec4");
        CHECK(var->comment == "unequalValueVec4Private documentation");
        CHECK(var->attributes.unequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[40];
        REQUIRE(var);
        CHECK(var->name == "unequalValueVec4OptionalPrivate");
        CHECK(var->key == "\"UnequalValueVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::vec4>");
        CHECK(var->comment == "unequalValueVec4OptionalPrivate documentation");
        CHECK(var->attributes.unequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    {
        Variable* var = s->variables[41];
        REQUIRE(var);
        CHECK(var->name == "unequalValueVec4VectorPrivate");
        CHECK(var->key == "\"UnequalValueVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::vec4>");
        CHECK(var->comment == "unequalValueVec4VectorPrivate documentation");
        CHECK(var->attributes.unequal == "glm::vec4(1.f)");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.key.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(var->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
