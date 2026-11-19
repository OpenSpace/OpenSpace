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

TEST_CASE("Parsing/LuaWrapper/Comments:  No Comments", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Direct Comments/1", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    /*
     * Some example documentation
     * that covers a few lines.
     * And another one for good measure
     */
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(
        f->documentation ==
        "Some example documentation that covers a few lines. "
        "And another one for good measure"
    );
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Direct Comments/2", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    // Some example documentation
    // that covers a few lines.
    // And another one for good measure
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(
        f->documentation ==
        "Some example documentation that covers a few lines. "
        "And another one for good measure"
    );
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Comment Separated/1", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    /*
     * Some example documentation
     * that covers a few lines.
     * And another one for good measure
     */
    void someOtherFunction();

    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Comment Separated/2", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    // First lines that shouldn't be included
    // in the documentation

    // Some example documentation
    // that covers a few lines.
    // And another one for good measure
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(
        f->documentation ==
        "Some example documentation that covers a few lines. "
        "And another one for good measure"
    );
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

// Only // in the line
TEST_CASE("Parsing/LuaWrapper/Comments:  Empty comment", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    //
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

// The line has a space after the //
TEST_CASE("Parsing/LuaWrapper/Comments:  Empty comment/2", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    // 
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Empty comment/3", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    //
    //
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Empty comment/4", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    /**
     */
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Empty comment/5", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    /**
     *
     */
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation.empty());
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Empty line block/1", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    /**
     * First line of comment
     *
     * Second after an empty line
     */
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation == "First line of comment\n\nSecond after an empty line");
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Empty line block/2", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    /**
     *
     * First line of comment
     *
     * Second after an empty line
     */
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation == "First line of comment\n\nSecond after an empty line");
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Empty line block/3", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    /**
     *
     * First line of comment
     *
     * Second after an empty line
     *
     */
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation == "First line of comment\n\nSecond after an empty line");
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Empty line regular/1", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    // First line of comment
    //
    // Second after an empty line
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation == "First line of comment\n\nSecond after an empty line");
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Empty line regular/2", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    //
    // First line of comment
    //
    // Second after an empty line
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation == "First line of comment\n\nSecond after an empty line");
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}

TEST_CASE("Parsing/LuaWrapper/Comments:  Empty line regular/3", "[Parsing][LuaWrapper]") {
    constexpr std::string_view Source = R"(
    //
    // First line of comment
    //
    // Second after an empty line
    //
    [[codegen::luawrap]] void foo() {
    }
)";

    Code code = parse(Source);
    CHECK(code.structs.empty());
    CHECK(code.enums.empty());
    REQUIRE(code.luaWrapperFunctions.size() == 1);
    Function* f = code.luaWrapperFunctions[0];
    REQUIRE(f);

    CHECK(f->functionName == "foo");
    CHECK(f->documentation == "First line of comment\n\nSecond after an empty line");
    CHECK(f->returnValue == nullptr);
    CHECK(f->arguments.empty());

    const std::string r = generateResult(code);
    CHECK(!r.empty());
}
