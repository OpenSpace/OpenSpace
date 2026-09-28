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
#include <openspace/misc/dictionary.h>
#include <array>
#include <optional>
#include <variant>
#include <vector>

using namespace openspace;

namespace {
    const std::string A = "A";
    const std::string Long = "Long";
    const std::string Of = "Of";
    const std::string Variables = "Variables";
    const std::string That = "That";
    const std::string Will = "Will";
    const std::string Cover = "Cover";
    const std::string And = "And";
    const std::string Span = "Span";
    const std::string Multiple = "Multiple";
    const std::string Lines = "Lines";
    const std::string With = "With";
    const std::string Weird = "Weird";
    const std::string Indentations = "Indentations";

    struct [[codegen::Dictionary(Other)]] Parameters {
        int abc;

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

    // Struct A with empty line
    struct [[codegen::Dictionary(A)]] ParamA {

    };

    // Struct B without empty line
    struct [[codegen::Dictionary(B)]] ParamB {
    };

    // Struct C on same line
    struct [[codegen::Dictionary(C)]] ParamC {};
} // namespace
#include "execution_structs_other_codegen.cpp"


TEST_CASE("Execution/Structs/Other:  Documentation Id", "[Execution][Structs]") {
    using namespace openspace;
    const Documentation doc1 = codegen::doc<Parameters>("");
    CHECK(doc1.id.empty());

    const Documentation doc2 = codegen::doc<Parameters>("abc");
    CHECK(doc2.id == "abc");
}
