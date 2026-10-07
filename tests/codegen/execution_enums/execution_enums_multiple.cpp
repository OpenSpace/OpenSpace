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
#include <optional>
#include <variant>
#include <vector>

namespace {
    enum class [[codegen::stringify()]] Enum1 {
        Value1,
        value2,
        Value3
    };

    enum class [[codegen::stringify()]] Enum2 {
        Val4,
        Val5
    };

    enum class [[codegen::stringify()]] Enum3 {
        Val6
    };
} // namespace
#include "execution_enums_multiple_codegen.cpp"


TEST_CASE("Execution/Enums/Multiple:  From String", "[Execution][Enums]") {
    Enum1 v1 = codegen::fromString<Enum1>("Value1");
    CHECK(v1 == Enum1::Value1);
    Enum1 v2 = codegen::fromString<Enum1>("value2");
    CHECK(v2 == Enum1::value2);
    Enum1 v3 = codegen::fromString<Enum1>("Value3");
    CHECK(v3 == Enum1::Value3);

    Enum2 v4 = codegen::fromString<Enum2>("Val4");
    CHECK(v4 == Enum2::Val4);
    Enum2 v5 = codegen::fromString<Enum2>("Val5");
    CHECK(v5 == Enum2::Val5);

    Enum3 v6 = codegen::fromString<Enum3>("Val6");
    CHECK(v6 == Enum3::Val6);
}

TEST_CASE("Execution/Enums/Multiple:  To String", "[Execution][Enums]") {
    std::string_view v1 = codegen::toString(Enum1::Value1);
    CHECK(v1 == "Value1");
    std::string_view v2 = codegen::toString(Enum1::value2);
    CHECK(v2 == "value2");
    std::string_view v3 = codegen::toString(Enum1::Value3);
    CHECK(v3 == "Value3");

    std::string_view v4 = codegen::toString(Enum2::Val4);
    CHECK(v4 == "Val4");
    std::string_view v5 = codegen::toString(Enum2::Val5);
    CHECK(v5 == "Val5");

    std::string_view v6 = codegen::toString(Enum3::Val6);
    CHECK(v6 == "Val6");
}
