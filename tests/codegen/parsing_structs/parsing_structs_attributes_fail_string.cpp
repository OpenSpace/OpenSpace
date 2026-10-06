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

TEST_CASE("Parsing/Structs/Attributes/Error/String:  inrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::inrange(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'inrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector inrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::inrange(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'inrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional inrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::inrange(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'inrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  less") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::less(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'less'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector less") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::less(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'less'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional less") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::less(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'less'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  lessequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::lessequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'lessequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector lessequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::lessequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'lessequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional lessequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::lessequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'lessequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  greater") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::greater(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'greater'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector greater") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::greater(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'greater'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional greater") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::greater(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'greater'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  greaterequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::greaterequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'greaterequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector greaterequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::greaterequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'greaterequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional greaterequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::greaterequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'greaterequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  notinrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::notinrange(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'notinrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector notinrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::notinrange(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'notinrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional notinrange") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::notinrange(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'notinrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  reference") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector reference") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional reference") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  color") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector color") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional color") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  directory") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'directory'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector directory") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'directory'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional directory") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'std::string' does not support attribute 'directory'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  annotation inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::annotation("abc"), codegen::inlist({"abc", "def" })]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the annotation attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector annotation inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::annotation("abc"), codegen::inlist({"abc", "def" })]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the annotation attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional annotation inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::annotation("abc"), codegen::inlist({"abc", "def" })]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the annotation attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional vector annotation inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::vector<std::string>> v [[codegen::annotation("abc"), codegen::inlist({"abc", "def" })]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the annotation attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  annotation unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::annotation("abc"), codegen::unequal("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the annotation attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector annotation unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::annotation("abc"), codegen::unequal("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the annotation attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional annotation unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::annotation("abc"), codegen::unequal("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the annotation attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional vector annotation unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::vector<std::string>> v [[codegen::annotation("abc"), codegen::unequal("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the annotation attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  annotation notempty") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::annotation("abc"), codegen::notempty()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector annotation notempty") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::annotation("abc"), codegen::notempty()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional annotation notempty") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::annotation("abc"), codegen::notempty()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional vector annotation notempty") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::vector<std::string>> v [[codegen::annotation("abc"), codegen::notempty()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  notempty inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::notempty(), codegen::inlist({"abc", "def"})]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector notempty inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::notempty(), codegen::inlist({"abc", "def"})]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional notempty inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::notempty(), codegen::inlist({"abc", "def"})]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional vector notempty inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::vector<std::string>> v [[codegen::notempty(), codegen::inlist({"abc", "def"})]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  notempty unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::notempty(), codegen::unequal("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector notempty unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::notempty(), codegen::unequal("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional notempty unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::notempty(), codegen::unequal("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional vector notempty unequal") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::vector<std::string>> v [[codegen::notempty(), codegen::unequal("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  notempty annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::notempty(), codegen::annotation("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  vector notempty annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<std::string> v [[codegen::notempty(), codegen::annotation("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional notempty annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::string> v [[codegen::notempty(), codegen::annotation("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  optional vector notempty annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<std::vector<std::string>> v [[codegen::notempty(), codegen::annotation("abc")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("With the notempty attribute, no other attribute can be used")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/String:  notempty wrong parameter") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::string v [[codegen::notempty("bla")]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Boolean attribute needs to be empty, 'true', or 'false'")
    );
}
