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

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
double v [[codegen::annotation(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'annotation'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  vector annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<double> v [[codegen::annotation(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'annotation'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  optional annotation") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<double> v [[codegen::annotation(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'annotation'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
double v [[codegen::inlist(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'inlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  vector inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<double> v [[codegen::inlist(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'inlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  optional inlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<double> v [[codegen::inlist(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'inlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  notinlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
double v [[codegen::notinlist(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'notinlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  vector notinlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<double> v [[codegen::notinlist(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'notinlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  optional notinlist") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<double> v [[codegen::notinlist(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'notinlist'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  reference") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
double v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  vector reference") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<double> v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  optional reference") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<double> v [[codegen::reference(1)]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'reference'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  color") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
double v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  vector color") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<double> v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  optional color") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<double> v [[codegen::color()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'color'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  directory") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
double v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'directory'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  vector directory") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::vector<double> v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'directory'")
    );
}

TEST_CASE("Parsing/Structs/Attributes/Error/Double:  optional directory") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
std::optional<double> v [[codegen::directory()]];
};)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Type 'double' does not support attribute 'directory'")
    );
}
