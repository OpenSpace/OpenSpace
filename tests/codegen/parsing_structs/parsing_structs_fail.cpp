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

TEST_CASE("Parsing/Structs/Error:  Extra \" in root name", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary("Wrong")]] Parameters {
    int dummy;
}
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("Root struct name must not be enclosed")
    );
}

TEST_CASE("Parsing/Structs/Error:  Missing description", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error)]] Parameters {
    // [[codegen::verbatim]]
    int error;
};
)";
    const Code code = parse(Source);
    CHECK_THROWS_MATCHES(
        generateResult(code),
        CodegenError, CM::StartsWith("Malformed codegen::verbatim")
    );
}

TEST_CASE("Parsing/Structs/Error:  Missing space", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(struct [[codegen::Dictionary(Name)]] Parameters{
};)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("Missing space or struct name before")
    );
}

TEST_CASE("Parsing/Structs/Error:  Missing variant closing", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error)]] Parameters {
    std::variant<int, float error;
};
)";
    CHECK_THROWS_MATCHES(
        [Source](){
            const Code code = parse(Source);
            generateResult(code);
        }(),
        CodegenError, CM::StartsWith("Unbalanced number of < > brackets")
    );
}

TEST_CASE("Parsing/Structs/Error:  Unknown type", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error)]] Parameters {
    std::list<int> error;
};
)";
    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError,
        CM::StartsWith("Type detected that codegen doesn't know how to handle")
    );
}

TEST_CASE("Parsing/Structs/Error:  Missing struct", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
Type [[codegen::Dictionary(Error)]] Parameters {
    std::string str;
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("Could not find 'struct' before '[[codegen")
    );
}

TEST_CASE("Parsing/Structs/Error:  Unterminated attribute", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error)]] Parameters {
    int val [[codegen::inrange(1.0, 2.0]];
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError,
        CM::StartsWith("Unterminated attribute brackets")
    );
}

TEST_CASE("Parsing/Structs/Error:  Unknown attribute (variable)", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error)]] Parameters {
    int val [[codegen::unknown_key(parameter)]];
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("Unknown attribute 'unknown_key' in attribute")
    );
}

TEST_CASE("Parsing/Structs/Error:  Unterminated dictionary", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error]] Parameters {
    int val;
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError,
        CM::StartsWith("Unterminated attribute brackets")
    );
}

TEST_CASE("Parsing/Structs/Error:  Unterminated attribute brackets", "[Parsing][Structs]")
{
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error)]] Parameters {
    enum class A {
        Value1 [[codegen::key("KeyA"),
        Value2
    };
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("Unterminated attribute brackets")
    );
}

TEST_CASE("Parsing/Structs/Error:  Unknown attribute (struct)", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error)]] Parameters {
    struct [[codegen::unknown_key()]] P {
    };
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError,
        CM::StartsWith("Unknown attribute 'unknown_key' in struct definition")
    );
}

TEST_CASE("Parsing/Structs/Error:  Unknown attribute (enum)", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error)]] Parameters {
    enum class E {
        Value [[codegen::unknown_key()]],
        Value2
    };
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("Unrecognized attribute 'unknown_key' found")
    );
}

TEST_CASE("Parsing/Structs/Error:  Empty root name 1", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary()]] Parameters {
    int val;
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("No name specified for root struct")
    );
}

TEST_CASE("Parsing/Structs/Error:  Empty root name 2", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary]] Parameters {
    int val;
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("No name specified for root struct")
    );
}

TEST_CASE("Parsing/Structs/Error:  Missing closing }", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Error)]] Parameters {
    int val;
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("Could not find closing } of root struct")
    );
}

TEST_CASE("Parsing/Structs/Error:  Wrong order", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct Parameters [[codegen::Dictionary(abc)]] {
    int value;
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError,
        CM::StartsWith("Only 'struct' can appear directly before [[codegen::Dictionary")
    );
}

TEST_CASE("Parsing/Structs/Error:  Block comments", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(abc)]] Parameters {
    /* evil block comment */
    int value;
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("Block comments are not allowed")
    );
}

TEST_CASE("Parsing/Structs/Error:  Duplicate root struct", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(S1)]] Parameters {
    struct [[codegen::Dictionary(S2)]] Sub {
        int value;
    };
    Sub s;
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError,
        CM::StartsWith("Found nested structs annotated with the Dictionary attribute.")
    );
}

TEST_CASE("Parsing/Structs/Error:  Old-style enum", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(abc)]] Parameters {
    enum ABC {
        value1,
        value2
    };
    ABC abc;
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError,
        CM::StartsWith("Old-style 'enum' not supported. Use 'enum class' instead")
    );
}

TEST_CASE("Parsing/Structs/Error:  Default argument in variable", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(D)]] P {
   int value1;
   float v2 = 2.f;
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("Found '=' in variable definition")
    );
}

TEST_CASE("Parsing/Structs/Error:  Misshapen boolean value", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
int v [[codegen::color(1)]];
};)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Boolean attribute needs to be")
    );
}

TEST_CASE("Parsing/Structs/Error:  Illegal pointer type", "[Parsing][Structs]") {
    constexpr std::string_view S = R"(struct [[codegen::Dictionary(D)]] P {
void* v;
};)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Illegal pointer type in struct definition")
    );
}
