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

TEST_CASE("Parsing/LuaWrapper/Error:  No variable name provided", "[Parsing][LuaWrapper]")
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] void foo(int) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError, CM::StartsWith("Parameter 0 of function 'foo' has no name")
    );
}

TEST_CASE("Parsing/LuaWrapper/Error:  Unhandled return type", "[Parsing][LuaWrapper]") {
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] std::chrono::time_point foo() {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type detected that codegen doesn't know how to handle: 'std::chrono::time_point'")
    );
}

TEST_CASE(
    "Parsing/LuaWrapper/Error:  Unhandled argument type first argument",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] void foo(std::list<int> abc) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type detected that codegen doesn't know how to handle: 'std::list<int>'")
    );
}

TEST_CASE(
    "Parsing/LuaWrapper/Error:  Unhandled argument type second argument",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] void foo(float abc, std::list<int> def) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type detected that codegen doesn't know how to handle: 'std::list<int>'")
    );
}

TEST_CASE(
    "Parsing/LuaWrapper/Error:  Reference type for argument",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] void foo(float& abc) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Illegal reference type found: float&")
    );
}

TEST_CASE(
    "Parsing/LuaWrapper/Error:  Const reference type for argument",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] void foo(const float& abc) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Illegal reference type 'const float&' found in function 'foo'")
    );
}

TEST_CASE(
    "Parsing/LuaWrapper/Error:  Const pointer type for argument",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] void foo(const float* abc) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Illegal pointer type 'const float*' found in function 'foo'")
    );
}

TEST_CASE(
    "Parsing/LuaWrapper/Error:  Uppercase function name",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] void Foo(const float* abc) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Marked functions must not start with an uppercase letter")
    );
}

TEST_CASE(
    "Parsing/LuaWrapper/Error:  Unsupported type in variant/1",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] void foo(std::variant<unsigned int, float> arg) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type detected that codegen doesn't know how to handle: 'unsigned int'")
    );
}

TEST_CASE(
    "Parsing/LuaWrapper/Error:  Unsupported type in variant/2",
    "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap]] void foo(std::variant<float, unsigned int> arg) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Type detected that codegen doesn't know how to handle: 'unsigned int'")
    );
}

TEST_CASE("Parsing/LuaWrapper/Error:  Empty custom name/1", "[Parsing][LuaWrapper]") {
    constexpr std::string_view S = R"(
    [[codegen::luawrap()]] void foo(std::variant<float, unsigned int> arg) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Error in custom name for luawrap function. Provided name was empty")
    );
}

TEST_CASE("Parsing/LuaWrapper/Error:  Empty custom name/2", "[Parsing][LuaWrapper]") {
    constexpr std::string_view S = R"(
    [[codegen::luawrap("")]] void foo(std::variant<float, unsigned int> arg) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Error in custom name for luawrap function. Provided name was empty")
    );
}

TEST_CASE("Parsing/LuaWrapper/Error:  No \" around custom name", "[Parsing][LuaWrapper]")
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap(abc)]] void foo(std::variant<float, int> arg) {
    }
)";

    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Error in custom name for luawrap function. Provided name must be enclosed")
    );
}

TEST_CASE(
    "Parsing/LuaWrapper/Error:  Unterminated luawrap marker", "[Parsing][LuaWrapper]"
)
{
    constexpr std::string_view S = R"(
    [[codegen::luawrap] void foo() {
    }
)";
    CHECK_THROWS_MATCHES(
        generateResult(parse(S)),
        CodegenError,
        CM::StartsWith("Error parsing root function, unterminated codegen marker. Missing ']'")
    );
}
