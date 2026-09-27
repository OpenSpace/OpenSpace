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

TEST_CASE("Parsing: Comments", "[Parsing][Misc]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Comments)]] Parameters {
    // multi
    // line
    // commenting
    int multiLineCommenting;

    // multi line simple variable def
    float
    multiLineSimpleVariableDef;

    // multi
    // line
    // commenting
    // and
    // multi line variable def
    std::string
    multiLineCommentAndDef;

    // misaligned
// commenting
        // all over
    // the place
bool misalignedIndent;

    // multiline
    // comment with
    // attribute
    int multiLineCommentAttribute
    [[codegen::inrange(2, 3)]];

    std::string multilineinlist [[codegen::inlist(A, "Very", Long, "List", Of, Variables,
"And", "String Literals", That, Will, Cover,
    And, Span, Multiple,
Lines, With, Weird,
                Indentations)]];

    std::string newLineAnnotation
        [[codegen::annotation(
            "some-annotation-that-is-very-long"
        )]];

    // newLine2Annotation documentation
    std::string newLine2Annotation [[codegen::annotation("A long string that starts here"
        "and covers"
        "multiple lines breaks, because someone really has a"
        "lot to say"
    )]];

    // This value has a " in the comment which might cause it to break?
    std::vector<std::string> quoteInComment;

    // What about " second quote " though?
    int twoQuotesInComment;

    // [[codegen::verbatim(ABC)]]
    int verbatimComment;

    // [[codegen::verbatim(ABC)]]
    // direct comment
    int verbatimAndDirectComment;

    // direct comment
    // [[codegen::verbatim(ABC)]]
    int directAndVerbatimComment;

    // direct comment 1
    // [[codegen::verbatim(ABC)]]
    // direct comment 2
    int sandwichedComment;

    std::vector<
        int
    > vectorWithLineBreak1;
    std::vector<int
    > vectorWithLineBreak2;
    std::vector<
        int> vectorWithLineBreak3;

    std::array<
        std::string,
        5
    > arrayWithLineBreak1;
    std::array<
        std::string, 5
    > arrayWithLineBreak2;
    std::array<
        std::string, 5> arrayWithLineBreak3;
    std::array<std::string, 5
    > arrayWithLineBreak4;
    std::array<std::string,
        5
    > arrayWithLineBreak5;
    std::array<std::string,
        5> arrayWithLineBreak6;

    std::optional<
        double
    > optionalWithLineBreak1;
    std::optional<double
    > optionalWithLineBreak2;
    std::optional<
        double> optionalWithLineBreak3;

    std::map<
        std::string,
        bool
    > mapWithLineBreak1;
    std::map<
        std::string, bool
    > mapWithLineBreak2;
    std::map<
        std::string, bool> mapWithLineBreak3;
    std::map<std::string, bool
    > mapWithLineBreak4;
    std::map<std::string,
        bool
    > mapWithLineBreak5;
    std::map<std::string,
        bool> mapWithLineBreak6;

    std::variant<
        glm::vec2,
        std::filesystem::path
    > variantWithLineBreak1;
    std::variant<
        glm::vec2, std::filesystem::path
    > variantWithLineBreak2;
    std::variant<
        glm::vec2, std::filesystem::path> variantWithLineBreak3;
    std::variant<glm::vec2, std::filesystem::path
    > variantWithLineBreak4;
    std::variant<glm::vec2,
        std::filesystem::path
    > variantWithLineBreak5;
    std::variant<glm::vec2,
        std::filesystem::path> variantWithLineBreak6;

    std::tuple<
        glm::mat2x2,
        glm::dmat4x4,
        std::string
    > tupleWithLineBreak1;
    std::tuple<
        glm::mat2x2, glm::dmat4x4, std::string
    > tupleWithLineBreak2;
    std::tuple<
        glm::mat2x2, glm::dmat4x4, std::string> tupleWithLineBreak3;
    std::tuple<glm::mat2x2,
        glm::dmat4x4,
        std::string
    > tupleWithLineBreak4;
    std::tuple<glm::mat2x2, glm::dmat4x4,
        std::string
    > tupleWithLineBreak5;
    std::tuple<glm::mat2x2, glm::dmat4x4,
    std::string> tupleWithLineBreak6;
    std::tuple<glm::mat2x2,
        glm::dmat4x4,
        std::string> tupleWithLineBreak7;
    std::tuple<glm::mat2x2,
        glm::dmat4x4,
        std::string
    > tupleWithLineBreak8;
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    CHECK(code.enums.empty());
    CHECK(code.luaWrapperFunctions.empty());
    Struct* s = code.structs.front();
    REQUIRE(s);

    REQUIRE(s->variables.size() == 46);
    {
        Variable* var = s->variables[0];
        REQUIRE(var);
        CHECK(var->name == "multiLineCommenting");
        CHECK(generateTypename(var->type) == "int");
        CHECK(var->comment == "multi line commenting");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "multiLineSimpleVariableDef");
        CHECK(generateTypename(var->type) == "float");
        CHECK(var->comment == "multi line simple variable def");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "multiLineCommentAndDef");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "multi line commenting and multi line variable def");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "misalignedIndent");
        CHECK(generateTypename(var->type) == "bool");
        CHECK(var->comment == "misaligned commenting all over the place");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[4];
        REQUIRE(var);
        CHECK(var->name == "multiLineCommentAttribute");
        CHECK(generateTypename(var->type) == "int");
        CHECK(var->comment == "multiline comment with attribute");
        CHECK(var->attributes.inrange == "2, 3");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
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
        Variable* var = s->variables[5];
        REQUIRE(var);
        CHECK(var->name == "multilineinlist");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment.empty());
        CHECK(
            var->attributes.inlist ==
            "A, \"Very\", Long, \"List\", Of, Variables, \"And\", \"String Literals\", "
            "That, Will, Cover, And, Span, Multiple, Lines, With, Weird, Indentations"
        );

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[6];
        REQUIRE(var);
        CHECK(var->name == "newLineAnnotation");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment.empty());
        CHECK(var->attributes.annotation == "\"some-annotation-that-is-very-long\"");

        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "newLine2Annotation");
        CHECK(generateTypename(var->type) == "std::string");
        CHECK(var->comment == "newLine2Annotation documentation");
        CHECK(var->attributes.annotation == "\"A long string that starts here and covers multiple lines breaks, because someone really has a lot to say\"");

        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "quoteInComment");
        CHECK(generateTypename(var->type) == "std::vector<std::string>");
        CHECK(var->comment == "This value has a \" in the comment which might cause it to break?");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "twoQuotesInComment");
        CHECK(generateTypename(var->type) == "int");
        CHECK(var->comment == "What about \" second quote \" though?");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "verbatimComment");
        CHECK(generateTypename(var->type) == "int");
        CHECK(var->comment == "[[codegen::verbatim(ABC)]]");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "verbatimAndDirectComment");
        CHECK(generateTypename(var->type) == "int");
        CHECK(var->comment == "[[codegen::verbatim(ABC)]] direct comment");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "directAndVerbatimComment");
        CHECK(generateTypename(var->type) == "int");
        CHECK(var->comment == "direct comment [[codegen::verbatim(ABC)]]");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "sandwichedComment");
        CHECK(generateTypename(var->type) == "int");
        CHECK(var->comment == "direct comment 1 [[codegen::verbatim(ABC)]] direct comment 2");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "vectorWithLineBreak1");
        CHECK(generateTypename(var->type) == "std::vector<int>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "vectorWithLineBreak2");
        CHECK(generateTypename(var->type) == "std::vector<int>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "vectorWithLineBreak3");
        CHECK(generateTypename(var->type) == "std::vector<int>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "arrayWithLineBreak1");
        CHECK(generateTypename(var->type) == "std::array<std::string, 5>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        CHECK(var->name == "arrayWithLineBreak2");
        CHECK(generateTypename(var->type) == "std::array<std::string, 5>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[19];
        REQUIRE(var);
        CHECK(var->name == "arrayWithLineBreak3");
        CHECK(generateTypename(var->type) == "std::array<std::string, 5>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[20];
        REQUIRE(var);
        CHECK(var->name == "arrayWithLineBreak4");
        CHECK(generateTypename(var->type) == "std::array<std::string, 5>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[21];
        REQUIRE(var);
        CHECK(var->name == "arrayWithLineBreak5");
        CHECK(generateTypename(var->type) == "std::array<std::string, 5>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[22];
        REQUIRE(var);
        CHECK(var->name == "arrayWithLineBreak6");
        CHECK(generateTypename(var->type) == "std::array<std::string, 5>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[23];
        REQUIRE(var);
        CHECK(var->name == "optionalWithLineBreak1");
        CHECK(generateTypename(var->type) == "std::optional<double>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[24];
        REQUIRE(var);
        CHECK(var->name == "optionalWithLineBreak2");
        CHECK(generateTypename(var->type) == "std::optional<double>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[25];
        REQUIRE(var);
        CHECK(var->name == "optionalWithLineBreak3");
        CHECK(generateTypename(var->type) == "std::optional<double>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[26];
        REQUIRE(var);
        CHECK(var->name == "mapWithLineBreak1");
        CHECK(generateTypename(var->type) == "std::map<std::string, bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[27];
        REQUIRE(var);
        CHECK(var->name == "mapWithLineBreak2");
        CHECK(generateTypename(var->type) == "std::map<std::string, bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[28];
        REQUIRE(var);
        CHECK(var->name == "mapWithLineBreak3");
        CHECK(generateTypename(var->type) == "std::map<std::string, bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[29];
        REQUIRE(var);
        CHECK(var->name == "mapWithLineBreak4");
        CHECK(generateTypename(var->type) == "std::map<std::string, bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[30];
        REQUIRE(var);
        CHECK(var->name == "mapWithLineBreak5");
        CHECK(generateTypename(var->type) == "std::map<std::string, bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[31];
        REQUIRE(var);
        CHECK(var->name == "mapWithLineBreak6");
        CHECK(generateTypename(var->type) == "std::map<std::string, bool>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[32];
        REQUIRE(var);
        CHECK(var->name == "variantWithLineBreak1");
        CHECK(generateTypename(var->type) == "std::variant<glm::vec2, std::filesystem::path>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[33];
        REQUIRE(var);
        CHECK(var->name == "variantWithLineBreak2");
        CHECK(generateTypename(var->type) == "std::variant<glm::vec2, std::filesystem::path>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[34];
        REQUIRE(var);
        CHECK(var->name == "variantWithLineBreak3");
        CHECK(generateTypename(var->type) == "std::variant<glm::vec2, std::filesystem::path>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[35];
        REQUIRE(var);
        CHECK(var->name == "variantWithLineBreak4");
        CHECK(generateTypename(var->type) == "std::variant<glm::vec2, std::filesystem::path>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[36];
        REQUIRE(var);
        CHECK(var->name == "variantWithLineBreak5");
        CHECK(generateTypename(var->type) == "std::variant<glm::vec2, std::filesystem::path>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[37];
        REQUIRE(var);
        CHECK(var->name == "variantWithLineBreak6");
        CHECK(generateTypename(var->type) == "std::variant<glm::vec2, std::filesystem::path>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[38];
        REQUIRE(var);
        CHECK(var->name == "tupleWithLineBreak1");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, glm::dmat4x4, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[39];
        REQUIRE(var);
        CHECK(var->name == "tupleWithLineBreak2");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, glm::dmat4x4, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[40];
        REQUIRE(var);
        CHECK(var->name == "tupleWithLineBreak3");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, glm::dmat4x4, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
        CHECK(var->attributes.notinrange.empty());
        CHECK(var->attributes.reference.empty());
        CHECK(var->attributes.unequal.empty());
        CHECK(!var->attributes.isColor);
        CHECK(!var->attributes.isDirectory);
        CHECK(!var->attributes.isDateTime);
        CHECK(!var->attributes.isIdentifier);
        CHECK(!var->attributes.mustBeNotEmpty);
        CHECK(!var->attributes.isPrivate);

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[41];
        REQUIRE(var);
        CHECK(var->name == "tupleWithLineBreak4");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, glm::dmat4x4, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[42];
        REQUIRE(var);
        CHECK(var->name == "tupleWithLineBreak5");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, glm::dmat4x4, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[43];
        REQUIRE(var);
        CHECK(var->name == "tupleWithLineBreak6");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, glm::dmat4x4, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[44];
        REQUIRE(var);
        CHECK(var->name == "tupleWithLineBreak7");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, glm::dmat4x4, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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
        Variable* var = s->variables[45];
        REQUIRE(var);
        CHECK(var->name == "tupleWithLineBreak8");
        CHECK(generateTypename(var->type) == "std::tuple<glm::mat2x2, glm::dmat4x4, std::string>");

        CHECK(var->attributes.annotation.empty());
        CHECK(var->attributes.greater.empty());
        CHECK(var->attributes.greaterequal.empty());
        CHECK(var->attributes.inlist.empty());
        CHECK(var->attributes.less.empty());
        CHECK(var->attributes.lessequal.empty());
        CHECK(var->attributes.inrange.empty());
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

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing: Multiple", "[Parsing][Misc]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(P1)]] Param1 {
    int abc;
};
struct [[codegen::Dictionary(P2)]] Param2 {
    std::string def;
};
struct [[codegen::Dictionary(P3)]] Param3 {
    double ghi;
};
struct [[codegen::Dictionary(P4)]] Param4 {
    ghoul::Dictionary jkl;
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 4);
    REQUIRE(code.enums.empty());

    {
        Struct* s = code.structs[0];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 1);
        CHECK(s->variables[0]->name == "abc");

        CHECK(s->variables[0]->attributes.annotation.empty());
        CHECK(s->variables[0]->attributes.greater.empty());
        CHECK(s->variables[0]->attributes.greaterequal.empty());
        CHECK(s->variables[0]->attributes.inlist.empty());
        CHECK(s->variables[0]->attributes.less.empty());
        CHECK(s->variables[0]->attributes.lessequal.empty());
        CHECK(s->variables[0]->attributes.inrange.empty());
        CHECK(s->variables[0]->attributes.notinrange.empty());
        CHECK(s->variables[0]->attributes.reference.empty());
        CHECK(s->variables[0]->attributes.unequal.empty());
        CHECK(!s->variables[0]->attributes.isColor);
        CHECK(!s->variables[0]->attributes.isDirectory);
        CHECK(!s->variables[0]->attributes.isDateTime);
        CHECK(!s->variables[0]->attributes.isIdentifier);
        CHECK(!s->variables[0]->attributes.mustBeNotEmpty);
        CHECK(!s->variables[0]->attributes.isPrivate);
    }

    {
        Struct* s = code.structs[1];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 1);
        CHECK(s->variables[0]->name == "def");

        CHECK(s->variables[0]->attributes.annotation.empty());
        CHECK(s->variables[0]->attributes.greater.empty());
        CHECK(s->variables[0]->attributes.greaterequal.empty());
        CHECK(s->variables[0]->attributes.inlist.empty());
        CHECK(s->variables[0]->attributes.less.empty());
        CHECK(s->variables[0]->attributes.lessequal.empty());
        CHECK(s->variables[0]->attributes.inrange.empty());
        CHECK(s->variables[0]->attributes.notinrange.empty());
        CHECK(s->variables[0]->attributes.reference.empty());
        CHECK(s->variables[0]->attributes.unequal.empty());
        CHECK(!s->variables[0]->attributes.isColor);
        CHECK(!s->variables[0]->attributes.isDirectory);
        CHECK(!s->variables[0]->attributes.isDateTime);
        CHECK(!s->variables[0]->attributes.isIdentifier);
        CHECK(!s->variables[0]->attributes.mustBeNotEmpty);
        CHECK(!s->variables[0]->attributes.isPrivate);
    }

    {
        Struct* s = code.structs[2];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 1);
        CHECK(s->variables[0]->name == "ghi");

        CHECK(s->variables[0]->attributes.annotation.empty());
        CHECK(s->variables[0]->attributes.greater.empty());
        CHECK(s->variables[0]->attributes.greaterequal.empty());
        CHECK(s->variables[0]->attributes.inlist.empty());
        CHECK(s->variables[0]->attributes.less.empty());
        CHECK(s->variables[0]->attributes.lessequal.empty());
        CHECK(s->variables[0]->attributes.inrange.empty());
        CHECK(s->variables[0]->attributes.notinrange.empty());
        CHECK(s->variables[0]->attributes.reference.empty());
        CHECK(s->variables[0]->attributes.unequal.empty());
        CHECK(!s->variables[0]->attributes.isColor);
        CHECK(!s->variables[0]->attributes.isDirectory);
        CHECK(!s->variables[0]->attributes.isDateTime);
        CHECK(!s->variables[0]->attributes.isIdentifier);
        CHECK(!s->variables[0]->attributes.mustBeNotEmpty);
        CHECK(!s->variables[0]->attributes.isPrivate);
    }

    {
        Struct* s = code.structs[3];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 1);
        CHECK(s->variables[0]->name == "jkl");

        CHECK(s->variables[0]->attributes.annotation.empty());
        CHECK(s->variables[0]->attributes.greater.empty());
        CHECK(s->variables[0]->attributes.greaterequal.empty());
        CHECK(s->variables[0]->attributes.inlist.empty());
        CHECK(s->variables[0]->attributes.less.empty());
        CHECK(s->variables[0]->attributes.lessequal.empty());
        CHECK(s->variables[0]->attributes.inrange.empty());
        CHECK(s->variables[0]->attributes.notinrange.empty());
        CHECK(s->variables[0]->attributes.reference.empty());
        CHECK(s->variables[0]->attributes.unequal.empty());
        CHECK(!s->variables[0]->attributes.isColor);
        CHECK(!s->variables[0]->attributes.isDirectory);
        CHECK(!s->variables[0]->attributes.isDateTime);
        CHECK(!s->variables[0]->attributes.isIdentifier);
        CHECK(!s->variables[0]->attributes.mustBeNotEmpty);
        CHECK(!s->variables[0]->attributes.isPrivate);
    }

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing: Struct Comments", "[Parsing][Misc]") {
    constexpr std::string_view Source = R"(
// Some documentation for the first struct
struct [[codegen::Dictionary(P1)]] Param1 {
    int abc;
};

// Some documentation for the second struct
struct [[codegen::Dictionary(P2)]] Param2 {
    int abc;
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 2);

    {
        Struct* s = code.structs[0];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 1);
        CHECK(s->variables[0]->name == "abc");

        CHECK(s->comment == "Some documentation for the first struct");

        CHECK(s->variables[0]->attributes.annotation.empty());
        CHECK(s->variables[0]->attributes.greater.empty());
        CHECK(s->variables[0]->attributes.greaterequal.empty());
        CHECK(s->variables[0]->attributes.inlist.empty());
        CHECK(s->variables[0]->attributes.less.empty());
        CHECK(s->variables[0]->attributes.lessequal.empty());
        CHECK(s->variables[0]->attributes.inrange.empty());
        CHECK(s->variables[0]->attributes.notinrange.empty());
        CHECK(s->variables[0]->attributes.reference.empty());
        CHECK(s->variables[0]->attributes.unequal.empty());
        CHECK(!s->variables[0]->attributes.isColor);
        CHECK(!s->variables[0]->attributes.isDirectory);
        CHECK(!s->variables[0]->attributes.isDateTime);
        CHECK(!s->variables[0]->attributes.isIdentifier);
        CHECK(!s->variables[0]->attributes.mustBeNotEmpty);
        CHECK(!s->variables[0]->attributes.isPrivate);
    }

    {
        Struct* s = code.structs[1];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 1);
        CHECK(s->variables[0]->name == "abc");

        CHECK(s->comment == "Some documentation for the second struct");
    }
}

TEST_CASE("Parsing: Struct Comments With Multiple Paragraphs", "[Parsing][Misc]") {
    constexpr std::string_view Source = R"(
// Some brief documentation for the struct
//
// Some more details for the struct
struct [[codegen::Dictionary(P)]] Param {
    int abc;
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);

    {
        Struct* s = code.structs[0];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 1);
        CHECK(s->variables[0]->name == "abc");

        std::string expected = R"(Some brief documentation for the struct

Some more details for the struct)";
        CHECK(s->comment == expected);

        CHECK(s->variables[0]->attributes.annotation.empty());
        CHECK(s->variables[0]->attributes.greater.empty());
        CHECK(s->variables[0]->attributes.greaterequal.empty());
        CHECK(s->variables[0]->attributes.inlist.empty());
        CHECK(s->variables[0]->attributes.less.empty());
        CHECK(s->variables[0]->attributes.lessequal.empty());
        CHECK(s->variables[0]->attributes.inrange.empty());
        CHECK(s->variables[0]->attributes.notinrange.empty());
        CHECK(s->variables[0]->attributes.reference.empty());
        CHECK(s->variables[0]->attributes.unequal.empty());
        CHECK(!s->variables[0]->attributes.isColor);
        CHECK(!s->variables[0]->attributes.isDirectory);
        CHECK(!s->variables[0]->attributes.isDateTime);
        CHECK(!s->variables[0]->attributes.isIdentifier);
        CHECK(!s->variables[0]->attributes.mustBeNotEmpty);
        CHECK(!s->variables[0]->attributes.isPrivate);
    }
}

TEST_CASE("Parsing: Struct Comments With Multiple Paragraphs - Block comments", "[Parsing][Misc]") {
    constexpr std::string_view Source = R"(
/*
 * Some brief documentation for the struct.
 *
 * Some more details for the struct
 */

struct [[codegen::Dictionary(P)]] Param {
    int abc;
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);

    {
        Struct* s = code.structs[0];
        REQUIRE(s);
        REQUIRE(s->variables.size() == 1);
        CHECK(s->variables[0]->name == "abc");

        std::string expected = R"(Some brief documentation for the struct.

Some more details for the struct)";
        CHECK(s->comment == expected);

        CHECK(s->variables[0]->attributes.annotation.empty());
        CHECK(s->variables[0]->attributes.greater.empty());
        CHECK(s->variables[0]->attributes.greaterequal.empty());
        CHECK(s->variables[0]->attributes.inlist.empty());
        CHECK(s->variables[0]->attributes.less.empty());
        CHECK(s->variables[0]->attributes.lessequal.empty());
        CHECK(s->variables[0]->attributes.inrange.empty());
        CHECK(s->variables[0]->attributes.notinrange.empty());
        CHECK(s->variables[0]->attributes.reference.empty());
        CHECK(s->variables[0]->attributes.unequal.empty());
        CHECK(!s->variables[0]->attributes.isColor);
        CHECK(!s->variables[0]->attributes.isDirectory);
        CHECK(!s->variables[0]->attributes.isDateTime);
        CHECK(!s->variables[0]->attributes.isIdentifier);
        CHECK(!s->variables[0]->attributes.mustBeNotEmpty);
        CHECK(!s->variables[0]->attributes.isPrivate);
    }
}

TEST_CASE("Parsing: Empty Struct", "[Parsing][Misc]") {
    constexpr std::string_view Source = R"(
// Struct A with empty line
struct [[codegen::Dictionary(A)]] ParamA {

};

// Struct B without empty line
struct [[codegen::Dictionary(B)]] ParamB {
};

// Struct C on same line
struct [[codegen::Dictionary(C)]] ParamC {};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 3);

    {
        Struct* s = code.structs[0];
        REQUIRE(s);
        CHECK(s->name == "ParamA");
        CHECK(s->comment == "Struct A with empty line");
        CHECK(s->attributes.dictionary == "A");
        CHECK(s->attributes.noExhaustive);
        CHECK(s->parent == nullptr);
        CHECK(s->children.empty());
        CHECK(s->variables.empty());
    }
    {
        Struct* s = code.structs[1];
        REQUIRE(s);
        CHECK(s->name == "ParamB");
        CHECK(s->comment == "Struct B without empty line");
        CHECK(s->attributes.dictionary == "B");
        CHECK(s->attributes.noExhaustive);
        CHECK(s->parent == nullptr);
        CHECK(s->children.empty());
        CHECK(s->variables.empty());
    }
    {
        Struct* s = code.structs[2];
        REQUIRE(s);
        CHECK(s->name == "ParamC");
        CHECK(s->comment == "Struct C on same line");
        CHECK(s->attributes.dictionary == "C");
        CHECK(s->attributes.noExhaustive);
        CHECK(s->parent == nullptr);
        CHECK(s->children.empty());
        CHECK(s->variables.empty());
    }
}

TEST_CASE("Parsing: Multiple attributes", "[Parsing][Misc]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(A)]] P1 {
    std::string type
        [[codegen::annotation("Must name a valid type"), codegen::private()]];
};

struct [[codegen::Dictionary(A)]] P2 {
    std::string type
        [[codegen::annotation("Must name a valid type"),
          codegen::private()]];
};

struct [[codegen::Dictionary(A)]] P3 {
    std::string type
        [[
          codegen::annotation("Must name a valid type"),
          codegen::private()]];
};

struct [[codegen::Dictionary(A)]] P4 {
    std::string type
        [[
          codegen::annotation("Must name a valid type"),
          codegen::private()
        ]];
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 4);

    {
        Struct* s = code.structs[0];
        REQUIRE(s);
        CHECK(s->name == "P1");
        CHECK(s->comment == "");
        CHECK(s->attributes.dictionary == "A");
        CHECK(s->attributes.noExhaustive);
        CHECK(s->parent == nullptr);
        CHECK(s->children.empty());
        REQUIRE(s->variables.size() == 1);
        Variable* v = s->variables.front();
        CHECK(v->attributes.annotation == "\"Must name a valid type\"");
        CHECK(v->attributes.isPrivate);
    }
    {
        Struct* s = code.structs[1];
        REQUIRE(s);
        CHECK(s->name == "P2");
        CHECK(s->comment == "");
        CHECK(s->attributes.dictionary == "A");
        CHECK(s->attributes.noExhaustive);
        CHECK(s->parent == nullptr);
        CHECK(s->children.empty());
        REQUIRE(s->variables.size() == 1);
        Variable* v = s->variables.front();
        CHECK(v->attributes.annotation == "\"Must name a valid type\"");
        CHECK(v->attributes.isPrivate);
    }
    {
        Struct* s = code.structs[2];
        REQUIRE(s);
        CHECK(s->name == "P3");
        CHECK(s->comment == "");
        CHECK(s->attributes.dictionary == "A");
        CHECK(s->attributes.noExhaustive);
        CHECK(s->parent == nullptr);
        CHECK(s->children.empty());
        REQUIRE(s->variables.size() == 1);
        Variable* v = s->variables.front();
        CHECK(v->attributes.annotation == "\"Must name a valid type\"");
        CHECK(v->attributes.isPrivate);
    }
    {
        Struct* s = code.structs[3];
        REQUIRE(s);
        CHECK(s->name == "P4");
        CHECK(s->comment == "");
        CHECK(s->attributes.dictionary == "A");
        CHECK(s->attributes.noExhaustive);
        CHECK(s->parent == nullptr);
        CHECK(s->children.empty());
        REQUIRE(s->variables.size() == 1);
        Variable* v = s->variables.front();
        CHECK(v->attributes.annotation == "\"Must name a valid type\"");
        CHECK(v->attributes.isPrivate);
    }
}
