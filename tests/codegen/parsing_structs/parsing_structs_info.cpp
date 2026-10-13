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

TEST_CASE("Parsing/Structs: Info attribute", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Info)]] Parameters {
    int infoOnly [[codegen::Info(ABC)]];

    // direct comment
    int infoAndComment [[codegen::Info(ABC)]];

    int infoComplex [[codegen::Info( ns::Foo )]];
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    Struct* s = code.structs.front();
    REQUIRE(s->variables.size() == 3);

    CHECK(s->variables[0]->attributes.info == "ABC");
    CHECK(s->variables[0]->comment.empty());
    CHECK(s->variables[1]->attributes.info == "ABC");
    CHECK(s->variables[1]->comment == "direct comment");
    CHECK(s->variables[2]->attributes.info == "ns::Foo");

    const std::string r = generateResult(code);
    CHECK_THAT(
        r,
        CM::ContainsSubstring(
            "Optional::No,Private::No,std::string(ABC.description));"
        )
    );
    CHECK_THAT(
        r,
        CM::ContainsSubstring(
            "Optional::No,Private::No,"
            "std::string(ABC.description) + \" \" + \"direct comment\");"
        )
    );
    CHECK_THAT(
        r,
        CM::ContainsSubstring(
            "Optional::No,Private::No,std::string(ns::Foo.description));"
        )
    );
}

TEST_CASE("Parsing/Structs: Info attribute combined with inrange", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Info)]] Parameters {
    std::optional<float> sunAngularSize [[codegen::Info(SunAngularSize), codegen::inrange(0.0, 180.0)]];

    // direct comment
    std::optional<float> withComment [[codegen::inrange(0.0, 180.0), codegen::Info(SunAngularSize)]];
};
)";

    Code code = parse(Source);
    REQUIRE(code.structs.size() == 1);
    Struct* s = code.structs.front();
    REQUIRE(s->variables.size() == 2);

    {
        Variable* var = s->variables[0];
        CHECK(var->name == "sunAngularSize");
        CHECK(var->key == "\"SunAngularSize\"");
        CHECK(generateTypename(var->type) == "std::optional<float>");
        CHECK(var->comment.empty());
        CHECK(var->attributes.info == "SunAngularSize");
        CHECK(var->attributes.inrange == "0.0, 180.0");
    }
    {
        Variable* var = s->variables[1];
        CHECK(var->name == "withComment");
        CHECK(var->comment == "direct comment");
        CHECK(var->attributes.info == "SunAngularSize");
        CHECK(var->attributes.inrange == "0.0, 180.0");
    }

    const std::string r = generateResult(code);
    INFO(r);
    CHECK_THAT(r, CM::ContainsSubstring("InRangeVerifier"));
    CHECK_THAT(
        r,
        CM::ContainsSubstring(
            "Optional::Yes,Private::No,std::string(SunAngularSize.description));"
        )
    );
    CHECK_THAT(
        r,
        CM::ContainsSubstring(
            "Optional::Yes,Private::No,"
            "std::string(SunAngularSize.description) + \" \" + \"direct comment\");"
        )
    );
}

TEST_CASE("Parsing/Structs/Error: Empty Info attribute", "[Parsing][Structs]") {
    constexpr std::string_view Source = R"(
struct [[codegen::Dictionary(Info)]] Parameters {
    int value [[codegen::Info()]];
};
)";

    CHECK_THROWS_MATCHES(
        parse(Source),
        CodegenError, CM::StartsWith("codegen::Info requires a non-empty argument")
    );
}
