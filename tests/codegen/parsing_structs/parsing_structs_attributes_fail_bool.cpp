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

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  annotation", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::annotation(ABC)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type 'bool' does not support attribute 'annotation'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  vector annotation",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::annotation(ABC)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type 'bool' does not support attribute 'annotation'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional annotation",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::annotation(ABC)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type 'bool' does not support attribute 'annotation'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  inlist",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::inlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type 'bool' does not support attribute 'inlist'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  vector inlist",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::inlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type 'bool' does not support attribute 'inlist'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional inlist",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::inlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'inlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  inrange", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::inrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'inrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  vector inrange", "[Parsing][Structs]")
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::inrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'inrange'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional inrange",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::inrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'inrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  less", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::less(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'less'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  vector less", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::less(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'less'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  optional less", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::less(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'less'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  lessequal", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::lessequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'lessequal'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  vector lessequal",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::lessequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'lessequal'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional lessequal",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::lessequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'lessequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  greater", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::greater(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'greater'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  vector greater", "[Parsing][Structs]")
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::greater(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'greater'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional greater",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::greater(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'greater'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  greaterequal", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::greaterequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type 'bool' does not support attribute 'greaterequal'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  vector greaterequal",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::greaterequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type 'bool' does not support attribute 'greaterequal'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional greaterequal",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::greaterequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type 'bool' does not support attribute 'greaterequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  notinlist", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::notinlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'notinlist'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  vector notinlist",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::notinlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'notinlist'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional notinlist",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::notinlist(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'notinlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  notinrange", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::notinrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'notinrange'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  vector notinrange",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::notinrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'notinrange'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional notinrange",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::notinrange(1, 2)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'notinrange'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  reference", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'reference'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  vector reference",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'reference'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional reference",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  unequal", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::unequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'unequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  vector unequal", "[Parsing][Structs]")
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::unequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'unequal'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional unequal",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::unequal(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'unequal'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  color", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  vector color", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  optional color", "[Parsing][Structs]")
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Bool:  directory", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
bool v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'directory'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  vector directory",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<bool> v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'directory'")
    );
}

TEST_CASE(
    "Parsing/Structs/Attributes/Error/Bool:  optional directory",
    "[Parsing][Structs]"
)
{
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<bool> v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'bool' does not support attribute 'directory'")
    );
}
