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
#include <vector>

namespace {
    struct [[codegen::Dictionary(Enums)]] Parameters {
        // enum A documentation
        enum class A {
            Value1,
            value2,
            Value3
        };
        // variable enumAValue documentation
        A enumAValue;

        // enum B documentation
        enum class B {
            Value1,
            value2,
            Value3
        };
        // variable enumBValue documentation
        std::optional<B> enumBValue;

        // enum C documentation
        enum class C {
            Value1,
            value2,
            Value3
        };
        // variable enumCValue documentation
        std::vector<C> enumCValue;

        enum class D {
            VeryLongValueThatIsSoLongWithAnEvenLongerKeyWhichNeedsToBe
                [[codegen::key("On a second line or else one line would be too long")]],
            ValueB,
            SecondVeryLongValueThatWillCheckIfWeDetectPropertyThatWeHave
                [[codegen::key("a continuation line as the last element of the enum")]]
        };
        // variable enumDValue documentation
        D enumDValue;

        enum class E {
            Value1,
            Value2,
            Value3,
            Value4
        };
        // variable enumEValue documentation
        E enumEValue;
    };
} // namespace
#include "execution_structs_enums_codegen.cpp"


TEST_CASE("Execution/Structs/Enums:  Bake", "[Execution][Structs]") {
    using namespace std::string_literals;

    ghoul::Dictionary d;
    d.setValue("EnumAValue", "Value1"s);
    d.setValue("EnumBValue", "value2"s);

    {
        ghoul::Dictionary e;
        e.setValue("1", "Value3"s);
        e.setValue("2", "value2"s);
        e.setValue("3", "Value1"s);
        d.setValue("EnumCValue", e);
    }
    d.setValue("EnumDValue", "On a second line or else one line would be too long"s);
    d.setValue("EnumEValue", "Value2"s);

    const Parameters p = codegen::bake<Parameters>(d);
    CHECK(p.enumAValue == Parameters::A::Value1);
    REQUIRE(p.enumBValue.has_value());
    CHECK(*p.enumBValue == Parameters::B::value2);
    CHECK(p.enumCValue.size() == 3);
    CHECK(
        p.enumCValue ==
        std::vector {
            Parameters::C::Value3,
            Parameters::C::value2,
            Parameters::C::Value1
        }
    );
    CHECK(
        p.enumDValue ==
        Parameters::D::VeryLongValueThatIsSoLongWithAnEvenLongerKeyWhichNeedsToBe
    );
    CHECK(p.enumEValue == Parameters::E::Value2);

    d.setValue("EnumDValue", "a continuation line as the last element of the enum"s);

    const Parameters q = codegen::bake<Parameters>(d);
    CHECK(
        q.enumDValue ==
        Parameters::D::SecondVeryLongValueThatWillCheckIfWeDetectPropertyThatWeHave
    );
}

TEST_CASE("Execution/Structs/Enums:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    using namespace std::string_literals;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 5);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "EnumAValue");
        CHECK(!e.optional);
        CHECK(e.documentation == "variable enumAValue documentation");
        CHECK(e.verifier->type() == "String");
        StringInListVerifier* v = dynamic_cast<StringInListVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector{ "Value1"s, "value2"s, "Value3"s });
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "EnumBValue");
        CHECK(e.optional);
        CHECK(e.documentation == "variable enumBValue documentation");
        CHECK(e.verifier->type() == "String");
        StringInListVerifier* v = dynamic_cast<StringInListVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->values == std::vector{ "Value1"s, "value2"s, "Value3"s });
    }
    {
        const DocumentationEntry& e = doc.entries[2];
        CHECK(e.key == "EnumCValue");
        CHECK(!e.optional);
        CHECK(e.documentation == "variable enumCValue documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].optional);
        CHECK(v->documentations[0].documentation == "enum C documentation");
        StringInListVerifier* w = dynamic_cast<StringInListVerifier*>(
            v->documentations[0].verifier.get()
        );
        REQUIRE(w);
        CHECK(w->values == std::vector{ "Value1"s, "value2"s, "Value3"s });
    }
    {
        const DocumentationEntry& e = doc.entries[3];
        CHECK(e.key == "EnumDValue");
        CHECK(!e.optional);
        CHECK(e.documentation == "variable enumDValue documentation");
        StringInListVerifier* sil = dynamic_cast<StringInListVerifier*>(e.verifier.get());
        REQUIRE(sil);
        CHECK(
            sil->values ==
            std::vector<std::string>{
                "On a second line or else one line would be too long",
                "ValueB",
                "a continuation line as the last element of the enum"
            }
        );
    }
    {
        const DocumentationEntry& e = doc.entries[4];
        CHECK(e.key == "EnumEValue");
        CHECK(!e.optional);
        CHECK(e.documentation == "variable enumEValue documentation");
        StringInListVerifier* sil = dynamic_cast<StringInListVerifier*>(e.verifier.get());
        REQUIRE(sil);
        CHECK(
            sil->values ==
            std::vector<std::string> { "Value1", "Value2", "Value3", "Value4" }
        );
    }
}

TEST_CASE("Execution/Structs/Enums:  String", "[Execution][Structs]") {
    //Parameters::E e1 = codegen::fromString<Parameters::E>("Value1");
    //CHECK(e1 == Parameters::E::Value1);
}
