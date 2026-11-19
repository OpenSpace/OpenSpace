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

TEST_CASE("Parsing/Structs/Attributes/IVec2") {
    constexpr std::string_view Source = R"(
    struct [[codegen::Dictionary(Attributes)]] Parameters {
        // inRangeValueIVec2 documentation
        glm::ivec2 inRangeValueIVec2 [[codegen::inrange(glm::ivec2(1), glm::ivec2(2))]];

        // inRangeValueIVec2Optional documentation
        std::optional<glm::ivec2> inRangeValueIVec2Optional
            [[codegen::inrange(glm::ivec2(1), glm::ivec2(2))]];

        // inRangeValueIVec2Vector documentation
        std::vector<glm::ivec2> inRangeValueIVec2Vector
            [[codegen::inrange(glm::ivec2(1), glm::ivec2(2))]];

        // notInRangeValueIVec2 documentation
        glm::ivec2 notInRangeValueIVec2
            [[codegen::notinrange(glm::ivec2(1), glm::ivec2(2))]];

        // notInRangeValueIVec2Optional documentation
        std::optional<glm::ivec2> notInRangeValueIVec2Optional
            [[codegen::notinrange(glm::ivec2(1), glm::ivec2(2))]];

        // notInRangeValueIVec2Vector documentation
        std::vector<glm::ivec2> notInRangeValueIVec2Vector
            [[codegen::notinrange(glm::ivec2(1), glm::ivec2(2))]];

        // lessValueIVec2 documentation
        glm::ivec2 lessValueIVec2 [[codegen::less(glm::ivec2(1))]];

        // lessValueIVec2Optional documentation
        std::optional<glm::ivec2> lessValueIVec2Optional
            [[codegen::less(glm::ivec2(1))]];

        // lessValueIVec2Vector documentation
        std::vector<glm::ivec2> lessValueIVec2Vector [[codegen::less(glm::ivec2(1))]];

        // lessEqualValueIVec2 documentation
        glm::ivec2 lessEqualValueIVec2 [[codegen::lessequal(glm::ivec2(1))]];

        // lessEqualValueIVec2Optional documentation
        std::optional<glm::ivec2> lessEqualValueIVec2Optional
            [[codegen::lessequal(glm::ivec2(1))]];

        // lessEqualValueIVec2Vector documentation
        std::vector<glm::ivec2> lessEqualValueIVec2Vector
            [[codegen::lessequal(glm::ivec2(1))]];

        // greaterValueIVec2 documentation
        glm::ivec2 greaterValueIVec2 [[codegen::greater(glm::ivec2(1))]];

        // greaterValueIVec2Optional documentation
        std::optional<glm::ivec2> greaterValueIVec2Optional
            [[codegen::greater(glm::ivec2(1))]];

        // greaterValueIVec2Vector documentation
        std::vector<glm::ivec2> greaterValueIVec2Vector
            [[codegen::greater(glm::ivec2(1))]];

        // greaterEqualValueIVec2 documentation
        glm::ivec2 greaterEqualValueIVec2 [[codegen::greaterequal(glm::ivec2(1))]];

        // greaterEqualValueIVec2Optional documentation
        std::optional<glm::ivec2> greaterEqualValueIVec2Optional
            [[codegen::greaterequal(glm::ivec2(1))]];

        // greaterEqualValueIVec2Vector documentation
        std::vector<glm::ivec2> greaterEqualValueIVec2Vector
            [[codegen::greaterequal(glm::ivec2(1))]];

        // unequalValueIVec2 documentation
        glm::ivec2 unequalValueIVec2 [[codegen::unequal(glm::ivec2(1))]];

        // unequalValueIVec2Optional documentation
        std::optional<glm::ivec2> unequalValueIVec2Optional
            [[codegen::unequal(glm::ivec2(1))]];

        // unequalValueIVec2Vector documentation
        std::vector<glm::ivec2> unequalValueIVec2Vector
            [[codegen::unequal(glm::ivec2(1))]];

        // inRangeValueIVec2Private documentation
        glm::ivec2 inRangeValueIVec2Private [[codegen::inrange(glm::ivec2(1), glm::ivec2(2)), codegen::private()]];

        // inRangeValueIVec2OptionalPrivate documentation
        std::optional<glm::ivec2> inRangeValueIVec2OptionalPrivate
            [[codegen::inrange(glm::ivec2(1), glm::ivec2(2)), codegen::private()]];

        // inRangeValueIVec2VectorPrivate documentation
        std::vector<glm::ivec2> inRangeValueIVec2VectorPrivate
            [[codegen::inrange(glm::ivec2(1), glm::ivec2(2)), codegen::private()]];

        // notInRangeValueIVec2Private documentation
        glm::ivec2 notInRangeValueIVec2Private
            [[codegen::notinrange(glm::ivec2(1), glm::ivec2(2)), codegen::private()]];

        // notInRangeValueIVec2OptionalPrivate documentation
        std::optional<glm::ivec2> notInRangeValueIVec2OptionalPrivate
            [[codegen::notinrange(glm::ivec2(1), glm::ivec2(2)), codegen::private()]];

        // notInRangeValueIVec2VectorPrivate documentation
        std::vector<glm::ivec2> notInRangeValueIVec2VectorPrivate
            [[codegen::notinrange(glm::ivec2(1), glm::ivec2(2)), codegen::private()]];

        // lessValueIVec2Private documentation
        glm::ivec2 lessValueIVec2Private [[codegen::less(glm::ivec2(1)), codegen::private()]];

        // lessValueIVec2OptionalPrivate documentation
        std::optional<glm::ivec2> lessValueIVec2OptionalPrivate
            [[codegen::less(glm::ivec2(1)), codegen::private()]];

        // lessValueIVec2VectorPrivate documentation
        std::vector<glm::ivec2> lessValueIVec2VectorPrivate [[codegen::less(glm::ivec2(1)), codegen::private()]];

        // lessEqualValueIVec2Private documentation
        glm::ivec2 lessEqualValueIVec2Private [[codegen::lessequal(glm::ivec2(1)), codegen::private()]];

        // lessEqualValueIVec2OptionalPrivate documentation
        std::optional<glm::ivec2> lessEqualValueIVec2OptionalPrivate
            [[codegen::lessequal(glm::ivec2(1)), codegen::private()]];

        // lessEqualValueIVec2VectorPrivate documentation
        std::vector<glm::ivec2> lessEqualValueIVec2VectorPrivate
            [[codegen::lessequal(glm::ivec2(1)), codegen::private()]];

        // greaterValueIVec2Private documentation
        glm::ivec2 greaterValueIVec2Private [[codegen::greater(glm::ivec2(1)), codegen::private()]];

        // greaterValueIVec2OptionalPrivate documentation
        std::optional<glm::ivec2> greaterValueIVec2OptionalPrivate
            [[codegen::greater(glm::ivec2(1)), codegen::private()]];

        // greaterValueIVec2VectorPrivate documentation
        std::vector<glm::ivec2> greaterValueIVec2VectorPrivate
            [[codegen::greater(glm::ivec2(1)), codegen::private()]];

        // greaterEqualValueIVec2Private documentation
        glm::ivec2 greaterEqualValueIVec2Private [[codegen::greaterequal(glm::ivec2(1)), codegen::private()]];

        // greaterEqualValueIVec2OptionalPrivate documentation
        std::optional<glm::ivec2> greaterEqualValueIVec2OptionalPrivate
            [[codegen::greaterequal(glm::ivec2(1)), codegen::private()]];

        // greaterEqualValueIVec2VectorPrivate documentation
        std::vector<glm::ivec2> greaterEqualValueIVec2VectorPrivate
            [[codegen::greaterequal(glm::ivec2(1)), codegen::private()]];

        // unequalValueIVec2Private documentation
        glm::ivec2 unequalValueIVec2Private [[codegen::unequal(glm::ivec2(1)), codegen::private()]];

        // unequalValueIVec2OptionalPrivate documentation
        std::optional<glm::ivec2> unequalValueIVec2OptionalPrivate
            [[codegen::unequal(glm::ivec2(1)), codegen::private()]];

        // unequalValueIVec2VectorPrivate documentation
        std::vector<glm::ivec2> unequalValueIVec2VectorPrivate
            [[codegen::unequal(glm::ivec2(1)), codegen::private()]];
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
        CHECK(var->name == "inRangeValueIVec2");
        CHECK(var->key == "\"InRangeValueIVec2\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "inRangeValueIVec2 documentation");
        CHECK(var->attributes.inrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "inRangeValueIVec2Optional");
        CHECK(var->key == "\"InRangeValueIVec2Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "inRangeValueIVec2Optional documentation");
        CHECK(var->attributes.inrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "inRangeValueIVec2Vector");
        CHECK(var->key == "\"InRangeValueIVec2Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "inRangeValueIVec2Vector documentation");
        CHECK(var->attributes.inrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "notInRangeValueIVec2");
        CHECK(var->key == "\"NotInRangeValueIVec2\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "notInRangeValueIVec2 documentation");
        CHECK(var->attributes.notinrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "notInRangeValueIVec2Optional");
        CHECK(var->key == "\"NotInRangeValueIVec2Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "notInRangeValueIVec2Optional documentation");
        CHECK(var->attributes.notinrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "notInRangeValueIVec2Vector");
        CHECK(var->key == "\"NotInRangeValueIVec2Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "notInRangeValueIVec2Vector documentation");
        CHECK(var->attributes.notinrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "lessValueIVec2");
        CHECK(var->key == "\"LessValueIVec2\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "lessValueIVec2 documentation");
        CHECK(var->attributes.less == "glm::ivec2(1)");

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
        CHECK(var->name == "lessValueIVec2Optional");
        CHECK(var->key == "\"LessValueIVec2Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "lessValueIVec2Optional documentation");
        CHECK(var->attributes.less == "glm::ivec2(1)");

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
        CHECK(var->name == "lessValueIVec2Vector");
        CHECK(var->key == "\"LessValueIVec2Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "lessValueIVec2Vector documentation");
        CHECK(var->attributes.less == "glm::ivec2(1)");

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
        CHECK(var->name == "lessEqualValueIVec2");
        CHECK(var->key == "\"LessEqualValueIVec2\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "lessEqualValueIVec2 documentation");
        CHECK(var->attributes.lessequal == "glm::ivec2(1)");

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
        CHECK(var->name == "lessEqualValueIVec2Optional");
        CHECK(var->key == "\"LessEqualValueIVec2Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "lessEqualValueIVec2Optional documentation");
        CHECK(var->attributes.lessequal == "glm::ivec2(1)");

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
        CHECK(var->name == "lessEqualValueIVec2Vector");
        CHECK(var->key == "\"LessEqualValueIVec2Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "lessEqualValueIVec2Vector documentation");
        CHECK(var->attributes.lessequal == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterValueIVec2");
        CHECK(var->key == "\"GreaterValueIVec2\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "greaterValueIVec2 documentation");
        CHECK(var->attributes.greater == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterValueIVec2Optional");
        CHECK(var->key == "\"GreaterValueIVec2Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "greaterValueIVec2Optional documentation");
        CHECK(var->attributes.greater == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterValueIVec2Vector");
        CHECK(var->key == "\"GreaterValueIVec2Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "greaterValueIVec2Vector documentation");
        CHECK(var->attributes.greater == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterEqualValueIVec2");
        CHECK(var->key == "\"GreaterEqualValueIVec2\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "greaterEqualValueIVec2 documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterEqualValueIVec2Optional");
        CHECK(var->key == "\"GreaterEqualValueIVec2Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "greaterEqualValueIVec2Optional documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterEqualValueIVec2Vector");
        CHECK(var->key == "\"GreaterEqualValueIVec2Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "greaterEqualValueIVec2Vector documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec2(1)");

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
        CHECK(var->name == "unequalValueIVec2");
        CHECK(var->key == "\"UnequalValueIVec2\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "unequalValueIVec2 documentation");
        CHECK(var->attributes.unequal == "glm::ivec2(1)");

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
        CHECK(var->name == "unequalValueIVec2Optional");
        CHECK(var->key == "\"UnequalValueIVec2Optional\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "unequalValueIVec2Optional documentation");
        CHECK(var->attributes.unequal == "glm::ivec2(1)");

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
        CHECK(var->name == "unequalValueIVec2Vector");
        CHECK(var->key == "\"UnequalValueIVec2Vector\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "unequalValueIVec2Vector documentation");
        CHECK(var->attributes.unequal == "glm::ivec2(1)");

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
        CHECK(var->name == "inRangeValueIVec2Private");
        CHECK(var->key == "\"InRangeValueIVec2Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "inRangeValueIVec2Private documentation");
        CHECK(var->attributes.inrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "inRangeValueIVec2OptionalPrivate");
        CHECK(var->key == "\"InRangeValueIVec2OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "inRangeValueIVec2OptionalPrivate documentation");
        CHECK(var->attributes.inrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "inRangeValueIVec2VectorPrivate");
        CHECK(var->key == "\"InRangeValueIVec2VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "inRangeValueIVec2VectorPrivate documentation");
        CHECK(var->attributes.inrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "notInRangeValueIVec2Private");
        CHECK(var->key == "\"NotInRangeValueIVec2Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "notInRangeValueIVec2Private documentation");
        CHECK(var->attributes.notinrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "notInRangeValueIVec2OptionalPrivate");
        CHECK(var->key == "\"NotInRangeValueIVec2OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "notInRangeValueIVec2OptionalPrivate documentation");
        CHECK(var->attributes.notinrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "notInRangeValueIVec2VectorPrivate");
        CHECK(var->key == "\"NotInRangeValueIVec2VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "notInRangeValueIVec2VectorPrivate documentation");
        CHECK(var->attributes.notinrange == "glm::ivec2(1), glm::ivec2(2)");

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
        CHECK(var->name == "lessValueIVec2Private");
        CHECK(var->key == "\"LessValueIVec2Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "lessValueIVec2Private documentation");
        CHECK(var->attributes.less == "glm::ivec2(1)");

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
        CHECK(var->name == "lessValueIVec2OptionalPrivate");
        CHECK(var->key == "\"LessValueIVec2OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "lessValueIVec2OptionalPrivate documentation");
        CHECK(var->attributes.less == "glm::ivec2(1)");

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
        CHECK(var->name == "lessValueIVec2VectorPrivate");
        CHECK(var->key == "\"LessValueIVec2VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "lessValueIVec2VectorPrivate documentation");
        CHECK(var->attributes.less == "glm::ivec2(1)");

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
        CHECK(var->name == "lessEqualValueIVec2Private");
        CHECK(var->key == "\"LessEqualValueIVec2Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "lessEqualValueIVec2Private documentation");
        CHECK(var->attributes.lessequal == "glm::ivec2(1)");

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
        CHECK(var->name == "lessEqualValueIVec2OptionalPrivate");
        CHECK(var->key == "\"LessEqualValueIVec2OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "lessEqualValueIVec2OptionalPrivate documentation");
        CHECK(var->attributes.lessequal == "glm::ivec2(1)");

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
        CHECK(var->name == "lessEqualValueIVec2VectorPrivate");
        CHECK(var->key == "\"LessEqualValueIVec2VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "lessEqualValueIVec2VectorPrivate documentation");
        CHECK(var->attributes.lessequal == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterValueIVec2Private");
        CHECK(var->key == "\"GreaterValueIVec2Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "greaterValueIVec2Private documentation");
        CHECK(var->attributes.greater == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterValueIVec2OptionalPrivate");
        CHECK(var->key == "\"GreaterValueIVec2OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "greaterValueIVec2OptionalPrivate documentation");
        CHECK(var->attributes.greater == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterValueIVec2VectorPrivate");
        CHECK(var->key == "\"GreaterValueIVec2VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "greaterValueIVec2VectorPrivate documentation");
        CHECK(var->attributes.greater == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterEqualValueIVec2Private");
        CHECK(var->key == "\"GreaterEqualValueIVec2Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "greaterEqualValueIVec2Private documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterEqualValueIVec2OptionalPrivate");
        CHECK(var->key == "\"GreaterEqualValueIVec2OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "greaterEqualValueIVec2OptionalPrivate documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec2(1)");

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
        CHECK(var->name == "greaterEqualValueIVec2VectorPrivate");
        CHECK(var->key == "\"GreaterEqualValueIVec2VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "greaterEqualValueIVec2VectorPrivate documentation");
        CHECK(var->attributes.greaterequal == "glm::ivec2(1)");

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
        CHECK(var->name == "unequalValueIVec2Private");
        CHECK(var->key == "\"UnequalValueIVec2Private\"");
        CHECK(generateTypename(var->type) == "glm::ivec2");
        CHECK(var->comment == "unequalValueIVec2Private documentation");
        CHECK(var->attributes.unequal == "glm::ivec2(1)");

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
        CHECK(var->name == "unequalValueIVec2OptionalPrivate");
        CHECK(var->key == "\"UnequalValueIVec2OptionalPrivate\"");
        CHECK(generateTypename(var->type) == "std::optional<glm::ivec2>");
        CHECK(var->comment == "unequalValueIVec2OptionalPrivate documentation");
        CHECK(var->attributes.unequal == "glm::ivec2(1)");

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
        CHECK(var->name == "unequalValueIVec2VectorPrivate");
        CHECK(var->key == "\"UnequalValueIVec2VectorPrivate\"");
        CHECK(generateTypename(var->type) == "std::vector<glm::ivec2>");
        CHECK(var->comment == "unequalValueIVec2VectorPrivate documentation");
        CHECK(var->attributes.unequal == "glm::ivec2(1)");

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
