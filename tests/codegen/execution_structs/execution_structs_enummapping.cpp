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

using namespace openspace;

namespace {
    enum class MappedEnumA {
        Value1,
        value2,
        Value3
    };

    struct [[codegen::Dictionary(EnumMapping)]] Parameters {
        // enum A documentation
        enum class [[codegen::map(MappedEnumA)]] A {
            Value1,
            value2,
            Value3
        };
        // variable enumAValue documentation
        A enumAValue;
    };
} // namespace
#include "execution_structs_enummapping_codegen.cpp"


TEST_CASE("Execution/Structs/EnumMapping:  1", "[Execution][Structs]") {
    using namespace std::string_literals;

    Dictionary d;
    d.setValue("EnumAValue", "Value1"s);

    const Parameters p = codegen::bake<Parameters>(d);
    CHECK(p.enumAValue == Parameters::A::Value1);

    MappedEnumA ma = codegen::map<MappedEnumA>(p.enumAValue);
    CHECK(ma == MappedEnumA::Value1);
}

TEST_CASE("Execution/Structs/EnumMapping:  2", "[Execution][Structs]") {
    using namespace std::string_literals;

    Dictionary d;
    d.setValue("EnumAValue", "value2"s);

    const Parameters p = codegen::bake<Parameters>(d);
    CHECK(p.enumAValue == Parameters::A::value2);

    MappedEnumA ma = codegen::map<MappedEnumA>(p.enumAValue);
    CHECK(ma == MappedEnumA::value2);
}

TEST_CASE("Execution/Structs/EnumMapping:  3", "[Execution][Structs]") {
    using namespace std::string_literals;

    Dictionary d;
    d.setValue("EnumAValue", "Value3"s);

    const Parameters p = codegen::bake<Parameters>(d);
    CHECK(p.enumAValue == Parameters::A::Value3);

    MappedEnumA ma = codegen::map<MappedEnumA>(p.enumAValue);
    CHECK(ma == MappedEnumA::Value3);
}
