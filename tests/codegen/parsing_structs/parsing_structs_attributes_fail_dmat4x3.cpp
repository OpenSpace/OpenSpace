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
#include <catch2/matchers/catch_matchers_exception.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

#include "codegen.h"
#include "parsing.h"
#include "types.h"

namespace CM = Catch::Matchers;

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::annotation(ABC)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'annotation'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::annotation(ABC)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'annotation'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::annotation(ABC)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'annotation'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::inlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'inlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::inlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'inlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::inlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'inlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  inrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::inrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'inrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector inrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::inrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'inrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional inrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::inrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'inrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  less") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::less(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'less'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector less") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::less(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'less'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional less") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::less(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'less'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  lessequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::lessequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'lessequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector lessequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::lessequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'lessequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional lessequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::lessequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'lessequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  greater") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::greater(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'greater'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector greater") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::greater(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'greater'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional greater") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::greater(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'greater'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  greaterequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::greaterequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'greaterequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector greaterequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::greaterequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'greaterequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional greaterequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::greaterequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'greaterequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  notinlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::notinlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'notinlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector notinlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::notinlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'notinlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional notinlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::notinlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'notinlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  notinrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::notinrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'notinrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector notinrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::notinrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'notinrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional notinrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::notinrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'notinrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  reference") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector reference") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional reference") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::unequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'unequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::unequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'unequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::unequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'unequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  color") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector color") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional color") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  directory") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
glm::dmat4x3 v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'directory'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  vector directory") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<glm::dmat4x3> v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'directory'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/DMat4x3:  optional directory") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<glm::dmat4x3> v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'glm::dmat4x3' does not support attribute 'directory'")
    );
}
