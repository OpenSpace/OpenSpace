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
    enum class Mapped2 {
        Val4,
        Val5
    };

    enum class Mapped3 {
        Val6
    };

    enum class [[codegen::stringify()]] Enum1 {
        Value1,
        value2,
        Value3
    };

    enum class [[codegen::stringify(), codegen::map(Mapped2)]] Enum2 {
        Val4,
        Val5
    };

    enum class [[codegen::map(Mapped3)]] Enum3 {
        Val6
    };
} // namespace
#include "execution_enums_mapping_codegen.cpp"


TEST_CASE("Execution/Enums/Mapping:  From String", "[Execution][Enums]") {
    Enum1 v1 = codegen::fromString<Enum1>("Value1");
    CHECK(v1 == Enum1::Value1);
    Enum1 v2 = codegen::fromString<Enum1>("value2");
    CHECK(v2 == Enum1::value2);
    Enum1 v3 = codegen::fromString<Enum1>("Value3");
    CHECK(v3 == Enum1::Value3);

    Enum2 v4 = codegen::fromString<Enum2>("Val4");
    CHECK(v4 == Enum2::Val4);
    Mapped2 mv4 = codegen::map<Mapped2>(v4);
    CHECK(mv4 == Mapped2::Val4);
    Enum2 v5 = codegen::fromString<Enum2>("Val5");
    CHECK(v5 == Enum2::Val5);
    Mapped2 mv5 = codegen::map<Mapped2>(v5);
    CHECK(mv5 == Mapped2::Val5);

    Mapped3 mv6 = codegen::map<Mapped3>(Enum3::Val6);
    CHECK(mv6 == Mapped3::Val6);
}

TEST_CASE("Execution/Enums/Mapping:  To String", "[Execution][Enums]") {
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
}
