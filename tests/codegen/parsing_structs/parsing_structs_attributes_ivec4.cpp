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

TEST_CASE("Parsing/Structs/Attributes/IVec4") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(Attributes)]] Parameters {
        // inRangeValueIVec4 documentation
        glm::ivec4 inRangeValueIVec4 [[codegen::inrange(glm::ivec4(1), glm::ivec4(2))]];

        // inRangeValueIVec4Optional documentation
        std::optional<glm::ivec4> inRangeValueIVec4Optional
            [[codegen::inrange(glm::ivec4(1), glm::ivec4(2))]];

        // inRangeValueIVec4Vector documentation
        std::vector<glm::ivec4> inRangeValueIVec4Vector
            [[codegen::inrange(glm::ivec4(1), glm::ivec4(2))]];

        // notInRangeValueIVec4 documentation
        glm::ivec4 notInRangeValueIVec4
            [[codegen::notinrange(glm::ivec4(1), glm::ivec4(2))]];

        // notInRangeValueIVec4Optional documentation
        std::optional<glm::ivec4> notInRangeValueIVec4Optional
            [[codegen::notinrange(glm::ivec4(1), glm::ivec4(2))]];

        // notInRangeValueIVec4Vector documentation
        std::vector<glm::ivec4> notInRangeValueIVec4Vector
            [[codegen::notinrange(glm::ivec4(1), glm::ivec4(2))]];

        // lessValueIVec4 documentation
        glm::ivec4 lessValueIVec4 [[codegen::less(glm::ivec4(1))]];

        // lessValueIVec4Optional documentation
        std::optional<glm::ivec4> lessValueIVec4Optional [[codegen::less(glm::ivec4(1))]];

        // lessValueIVec4Vector documentation
        std::vector<glm::ivec4> lessValueIVec4Vector [[codegen::less(glm::ivec4(1))]];

        // lessEqualValueIVec4 documentation
        glm::ivec4 lessEqualValueIVec4 [[codegen::lessequal(glm::ivec4(1))]];

        // lessEqualValueIVec4Optional documentation
        std::optional<glm::ivec4> lessEqualValueIVec4Optional
            [[codegen::lessequal(glm::ivec4(1))]];

        // lessEqualValueIVec4Vector documentation
        std::vector<glm::ivec4> lessEqualValueIVec4Vector
            [[codegen::lessequal(glm::ivec4(1))]];

        // greaterValueIVec4 documentation
        glm::ivec4 greaterValueIVec4 [[codegen::greater(glm::ivec4(1))]];

        // greaterValueIVec4Optional documentation
        std::optional<glm::ivec4> greaterValueIVec4Optional
            [[codegen::greater(glm::ivec4(1))]];

        // greaterValueIVec4Vector documentation
        std::vector<glm::ivec4> greaterValueIVec4Vector
            [[codegen::greater(glm::ivec4(1))]];

        // greaterEqualValueIVec4 documentation
        glm::ivec4 greaterEqualValueIVec4 [[codegen::greaterequal(glm::ivec4(1))]];

        // greaterEqualValueIVec4Optional documentation
        std::optional<glm::ivec4> greaterEqualValueIVec4Optional
            [[codegen::greaterequal(glm::ivec4(1))]];

        // greaterEqualValueIVec4Vector documentation
        std::vector<glm::ivec4> greaterEqualValueIVec4Vector
            [[codegen::greaterequal(glm::ivec4(1))]];

        // unequalValueIVec4 documentation
        glm::ivec4 unequalValueIVec4 [[codegen::unequal(glm::ivec4(1))]];

        // unequalValueIVec4Optional documentation
        std::optional<glm::ivec4> unequalValueIVec4Optional
            [[codegen::unequal(glm::ivec4(1))]];

        // unequalValueIVec4Vector documentation
        std::vector<glm::ivec4> unequalValueIVec4Vector
            [[codegen::unequal(glm::ivec4(1))]];

        // inRangeValueIVec4Private documentation
        glm::ivec4 inRangeValueIVec4Private [[codegen::inrange(glm::ivec4(1), glm::ivec4(2)), codegen::private()]];

        // inRangeValueIVec4OptionalPrivate documentation
        std::optional<glm::ivec4> inRangeValueIVec4OptionalPrivate
            [[codegen::inrange(glm::ivec4(1), glm::ivec4(2)), codegen::private()]];

        // inRangeValueIVec4VectorPrivate documentation
        std::vector<glm::ivec4> inRangeValueIVec4VectorPrivate
            [[codegen::inrange(glm::ivec4(1), glm::ivec4(2)), codegen::private()]];

        // notInRangeValueIVec4Private documentation
        glm::ivec4 notInRangeValueIVec4Private
            [[codegen::notinrange(glm::ivec4(1), glm::ivec4(2)), codegen::private()]];

        // notInRangeValueIVec4OptionalPrivate documentation
        std::optional<glm::ivec4> notInRangeValueIVec4OptionalPrivate
            [[codegen::notinrange(glm::ivec4(1), glm::ivec4(2)), codegen::private()]];

        // notInRangeValueIVec4VectorPrivate documentation
        std::vector<glm::ivec4> notInRangeValueIVec4VectorPrivate
            [[codegen::notinrange(glm::ivec4(1), glm::ivec4(2)), codegen::private()]];

        // lessValueIVec4Private documentation
        glm::ivec4 lessValueIVec4Private [[codegen::less(glm::ivec4(1)), codegen::private()]];

        // lessValueIVec4OptionalPrivate documentation
        std::optional<glm::ivec4> lessValueIVec4OptionalPrivate [[codegen::less(glm::ivec4(1)), codegen::private()]];

        // lessValueIVec4VectorPrivate documentation
        std::vector<glm::ivec4> lessValueIVec4VectorPrivate [[codegen::less(glm::ivec4(1)), codegen::private()]];

        // lessEqualValueIVec4Private documentation
        glm::ivec4 lessEqualValueIVec4Private [[codegen::lessequal(glm::ivec4(1)), codegen::private()]];

        // lessEqualValueIVec4OptionalPrivate documentation
        std::optional<glm::ivec4> lessEqualValueIVec4OptionalPrivate
            [[codegen::lessequal(glm::ivec4(1)), codegen::private()]];

        // lessEqualValueIVec4VectorPrivate documentation
        std::vector<glm::ivec4> lessEqualValueIVec4VectorPrivate
            [[codegen::lessequal(glm::ivec4(1)), codegen::private()]];

        // greaterValueIVec4Private documentation
        glm::ivec4 greaterValueIVec4Private [[codegen::greater(glm::ivec4(1)), codegen::private()]];

        // greaterValueIVec4OptionalPrivate documentation
        std::optional<glm::ivec4> greaterValueIVec4OptionalPrivate
            [[codegen::greater(glm::ivec4(1)), codegen::private()]];

        // greaterValueIVec4VectorPrivate documentation
        std::vector<glm::ivec4> greaterValueIVec4VectorPrivate
            [[codegen::greater(glm::ivec4(1)), codegen::private()]];

        // greaterEqualValueIVec4Private documentation
        glm::ivec4 greaterEqualValueIVec4Private [[codegen::greaterequal(glm::ivec4(1)), codegen::private()]];

        // greaterEqualValueIVec4OptionalPrivate documentation
        std::optional<glm::ivec4> greaterEqualValueIVec4OptionalPrivate
            [[codegen::greaterequal(glm::ivec4(1)), codegen::private()]];

        // greaterEqualValueIVec4VectorPrivate documentation
        std::vector<glm::ivec4> greaterEqualValueIVec4VectorPrivate
            [[codegen::greaterequal(glm::ivec4(1)), codegen::private()]];

        // unequalValueIVec4Private documentation
        glm::ivec4 unequalValueIVec4Private [[codegen::unequal(glm::ivec4(1)), codegen::private()]];

        // unequalValueIVec4OptionalPrivate documentation
        std::optional<glm::ivec4> unequalValueIVec4OptionalPrivate
            [[codegen::unequal(glm::ivec4(1)), codegen::private()]];

        // unequalValueIVec4VectorPrivate documentation
        std::vector<glm::ivec4> unequalValueIVec4VectorPrivate
            [[codegen::unequal(glm::ivec4(1)), codegen::private()]];
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
        CHECK(var->name == "inRangeValueIVec4");
        CHECK(var->key == "\"InRangeValueIVec4\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "inRangeValueIVec4 documentation");
        CHECK(var->attributes.inrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "inRangeValueIVec4Optional");
        CHECK(var->key == "\"InRangeValueIVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "inRangeValueIVec4Optional documentation");
        CHECK(var->attributes.inrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "inRangeValueIVec4Vector");
        CHECK(var->key == "\"InRangeValueIVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "inRangeValueIVec4Vector documentation");
        CHECK(var->attributes.inrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "notInRangeValueIVec4");
        CHECK(var->key == "\"NotInRangeValueIVec4\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "notInRangeValueIVec4 documentation");
        CHECK(var->attributes.notinrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "notInRangeValueIVec4Optional");
        CHECK(var->key == "\"NotInRangeValueIVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "notInRangeValueIVec4Optional documentation");
        CHECK(var->attributes.notinrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "notInRangeValueIVec4Vector");
        CHECK(var->key == "\"NotInRangeValueIVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "notInRangeValueIVec4Vector documentation");
        CHECK(var->attributes.notinrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "lessValueIVec4");
        CHECK(var->key == "\"LessValueIVec4\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "lessValueIVec4 documentation");
        CHECK(var->attributes.less == "glm::ivec4(1)");

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
        CHECK(var->name == "lessValueIVec4Optional");
        CHECK(var->key == "\"LessValueIVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "lessValueIVec4Optional documentation");
        CHECK(var->attributes.less == "glm::ivec4(1)");

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
        CHECK(var->name == "lessValueIVec4Vector");
        CHECK(var->key == "\"LessValueIVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "lessValueIVec4Vector documentation");
        CHECK(var->attributes.less == "glm::ivec4(1)");

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
        CHECK(var->name == "lessEqualValueIVec4");
        CHECK(var->key == "\"LessEqualValueIVec4\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "lessEqualValueIVec4 documentation");
        CHECK(var->attributes.lessequal == "glm::ivec4(1)");

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
        CHECK(var->name == "lessEqualValueIVec4Optional");
        CHECK(var->key == "\"LessEqualValueIVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "lessEqualValueIVec4Optional documentation");
        CHECK(var->attributes.lessequal == "glm::ivec4(1)");

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
        CHECK(var->name == "lessEqualValueIVec4Vector");
        CHECK(var->key == "\"LessEqualValueIVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "lessEqualValueIVec4Vector documentation");
        CHECK(var->attributes.lessequal == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterValueIVec4");
        CHECK(var->key == "\"GreaterValueIVec4\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "greaterValueIVec4 documentation");
        CHECK(var->attributes.greater == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterValueIVec4Optional");
        CHECK(var->key == "\"GreaterValueIVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "greaterValueIVec4Optional documentation");
        CHECK(var->attributes.greater == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterValueIVec4Vector");
        CHECK(var->key == "\"GreaterValueIVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "greaterValueIVec4Vector documentation");
        CHECK(var->attributes.greater == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterEqualValueIVec4");
        CHECK(var->key == "\"GreaterEqualValueIVec4\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "greaterEqualValueIVec4 documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterEqualValueIVec4Optional");
        CHECK(var->key == "\"GreaterEqualValueIVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "greaterEqualValueIVec4Optional documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterEqualValueIVec4Vector");
        CHECK(var->key == "\"GreaterEqualValueIVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "greaterEqualValueIVec4Vector documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec4(1)");

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
        CHECK(var->name == "unequalValueIVec4");
        CHECK(var->key == "\"UnequalValueIVec4\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "unequalValueIVec4 documentation");
        CHECK(var->attributes.unequal == "glm::ivec4(1)");

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
        CHECK(var->name == "unequalValueIVec4Optional");
        CHECK(var->key == "\"UnequalValueIVec4Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "unequalValueIVec4Optional documentation");
        CHECK(var->attributes.unequal == "glm::ivec4(1)");

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
        CHECK(var->name == "unequalValueIVec4Vector");
        CHECK(var->key == "\"UnequalValueIVec4Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "unequalValueIVec4Vector documentation");
        CHECK(var->attributes.unequal == "glm::ivec4(1)");

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
        CHECK(var->name == "inRangeValueIVec4Private");
        CHECK(var->key == "\"InRangeValueIVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "inRangeValueIVec4Private documentation");
        CHECK(var->attributes.inrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "inRangeValueIVec4OptionalPrivate");
        CHECK(var->key == "\"InRangeValueIVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "inRangeValueIVec4OptionalPrivate documentation");
        CHECK(var->attributes.inrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "inRangeValueIVec4VectorPrivate");
        CHECK(var->key == "\"InRangeValueIVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "inRangeValueIVec4VectorPrivate documentation");
        CHECK(var->attributes.inrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "notInRangeValueIVec4Private");
        CHECK(var->key == "\"NotInRangeValueIVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "notInRangeValueIVec4Private documentation");
        CHECK(var->attributes.notinrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "notInRangeValueIVec4OptionalPrivate");
        CHECK(var->key == "\"NotInRangeValueIVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "notInRangeValueIVec4OptionalPrivate documentation");
        CHECK(var->attributes.notinrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "notInRangeValueIVec4VectorPrivate");
        CHECK(var->key == "\"NotInRangeValueIVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "notInRangeValueIVec4VectorPrivate documentation");
        CHECK(var->attributes.notinrange == "glm::ivec4(1), glm::ivec4(2)");

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
        CHECK(var->name == "lessValueIVec4Private");
        CHECK(var->key == "\"LessValueIVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "lessValueIVec4Private documentation");
        CHECK(var->attributes.less == "glm::ivec4(1)");

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
        CHECK(var->name == "lessValueIVec4OptionalPrivate");
        CHECK(var->key == "\"LessValueIVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "lessValueIVec4OptionalPrivate documentation");
        CHECK(var->attributes.less == "glm::ivec4(1)");

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
        CHECK(var->name == "lessValueIVec4VectorPrivate");
        CHECK(var->key == "\"LessValueIVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "lessValueIVec4VectorPrivate documentation");
        CHECK(var->attributes.less == "glm::ivec4(1)");

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
        CHECK(var->name == "lessEqualValueIVec4Private");
        CHECK(var->key == "\"LessEqualValueIVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "lessEqualValueIVec4Private documentation");
        CHECK(var->attributes.lessequal == "glm::ivec4(1)");

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
        CHECK(var->name == "lessEqualValueIVec4OptionalPrivate");
        CHECK(var->key == "\"LessEqualValueIVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "lessEqualValueIVec4OptionalPrivate documentation");
        CHECK(var->attributes.lessequal == "glm::ivec4(1)");

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
        CHECK(var->name == "lessEqualValueIVec4VectorPrivate");
        CHECK(var->key == "\"LessEqualValueIVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "lessEqualValueIVec4VectorPrivate documentation");
        CHECK(var->attributes.lessequal == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterValueIVec4Private");
        CHECK(var->key == "\"GreaterValueIVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "greaterValueIVec4Private documentation");
        CHECK(var->attributes.greater == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterValueIVec4OptionalPrivate");
        CHECK(var->key == "\"GreaterValueIVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "greaterValueIVec4OptionalPrivate documentation");
        CHECK(var->attributes.greater == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterValueIVec4VectorPrivate");
        CHECK(var->key == "\"GreaterValueIVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "greaterValueIVec4VectorPrivate documentation");
        CHECK(var->attributes.greater == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterEqualValueIVec4Private");
        CHECK(var->key == "\"GreaterEqualValueIVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "greaterEqualValueIVec4Private documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterEqualValueIVec4OptionalPrivate");
        CHECK(var->key == "\"GreaterEqualValueIVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "greaterEqualValueIVec4OptionalPrivate documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec4(1)");

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
        CHECK(var->name == "greaterEqualValueIVec4VectorPrivate");
        CHECK(var->key == "\"GreaterEqualValueIVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "greaterEqualValueIVec4VectorPrivate documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec4(1)");

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
        CHECK(var->name == "unequalValueIVec4Private");
        CHECK(var->key == "\"UnequalValueIVec4Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec4");
        CHECK(var->comment == "unequalValueIVec4Private documentation");
        CHECK(var->attributes.unequal == "glm::ivec4(1)");

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
        CHECK(var->name == "unequalValueIVec4OptionalPrivate");
        CHECK(var->key == "\"UnequalValueIVec4OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec4>");
        CHECK(var->comment == "unequalValueIVec4OptionalPrivate documentation");
        CHECK(var->attributes.unequal == "glm::ivec4(1)");

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
        CHECK(var->name == "unequalValueIVec4VectorPrivate");
        CHECK(var->key == "\"UnequalValueIVec4VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec4>");
        CHECK(var->comment == "unequalValueIVec4VectorPrivate documentation");
        CHECK(var->attributes.unequal == "glm::ivec4(1)");

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
