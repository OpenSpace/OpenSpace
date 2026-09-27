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
#include <filesystem>
#include <fstream>
#include <optional>
#include <variant>
#include <vector>

namespace {
    struct [[codegen::Dictionary(Variant)]] Parameters {
        // boolDouble value documentation
        std::variant<bool, double> boolDoubleValue;

        // floatString value documentation
        std::variant<float, std::string> floatStringValue;

        // ivecs value documentation
        std::variant<glm::ivec2, glm::ivec3, glm::ivec4> ivecsValue;

        // vecmat1 value documentation
        std::variant<glm::dvec2, glm::dvec3, glm::dvec4, glm::mat2x3, glm::mat4x2,
            glm::dmat4x4> vecmat1;

        // vecmat2 value documentation
        std::variant<glm::mat2x2, glm::dmat3x3, glm::mat2x4> vecmat2;

        // rest value documentation
        std::variant<glm::vec2, glm::vec3, glm::vec4, glm::mat2x3, glm::mat2x4,
            glm::mat3x3, glm::mat3x4, glm::mat4x4> restValue;

        // optional documentation
        std::optional<std::variant<bool, int>> optionalValue;

        // variant vector documentation
        std::variant<std::vector<std::string>, std::string> variantVector;

        // variant vector 2 documentation
        std::variant<std::string, std::vector<std::string>> variantVector2;

        // multiple variant a documentation
        std::variant<int, std::string> multipleVariantA;

        // multiple variant b documentation
        std::variant<std::string, int> multipleVariantB;

        // multiple variant a2 documentation
        std::variant<int, std::string> multipleVariantA2;

        // multiple variant b2 documentation
        std::optional<std::variant<std::string, int>> multipleVariantB2;

        enum class A {
            Value1,
            Value2,
            Value3
        };
        // variant enum float documentation
        std::variant<A, float> variantEnumFloat;

        enum class B {
            Value1,
            Value2,
            Value3
        };
        // optional variant enum float documentation
        std::optional<std::variant<B, float>> optionalVariantEnumFloat;

        // variant path vector path documentation
        std::variant<
            std::filesystem::path, std::vector<std::filesystem::path>
        > variantPath;

        // variant vector path path documentation
        std::variant<
            std::vector<std::filesystem::path>, std::filesystem::path
        > variantPath2;

        // variantStringVectorC struct
        struct C {
            // variantStringVectorC inner
            std::variant<std::string, std::vector<std::string>> var;
        };
        // variantStringVectorC variable
        C variantStringVectorC;

        // variantStringVectorC struct
        struct D {
            // variantStringVectorD inner
            std::variant<std::string, std::vector<std::string>> var;
        };
        // variantStringVectorD variable
        D variantStringVectorD;

        // variantStringVector
        std::variant<std::string, std::vector<std::string>> variantStringVector;
    };
} // namespace
#include "execution_structs_variant_codegen.cpp"


TEST_CASE("Execution/Structs/Variant:  Bake", "[Execution][Structs]") {
    using namespace std::string_literals;

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    std::filesystem::path tmpFile = (path / "codegen_execution_variants.txt");
    {
        std::ofstream f(tmpFile);
        f << "unit test";
    }

    std::filesystem::path tmpFile2 = (path / "codegen_execution_variants_2.txt");
    {
        std::ofstream f(tmpFile2);
        f << "unit test";
    }

    {
        ghoul::Dictionary d1;
        d1.setValue("BoolDoubleValue", false);
        d1.setValue("FloatStringValue", 2.0);
        d1.setValue("IvecsValue", glm::dvec2(1.0, 2.0));
        d1.setValue("Vecmat1", glm::dvec2(3.1, 3.2));
        d1.setValue("Vecmat2", glm::dmat2x2(4.1, 4.2, 4.3, 4.4));
        d1.setValue(
            "RestValue",
            glm::dmat3x4(5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9, 5.10, 5.11, 5.12)
        );
        d1.setValue("OptionalValue", 5.0);
        d1.setValue("VariantVector", "abc"s);
        d1.setValue("VariantVector2", "def"s);
        d1.setValue("MultipleVariantA", 2.0);
        d1.setValue("MultipleVariantB", 2.0);
        d1.setValue("MultipleVariantA2", 2.0);
        d1.setValue("VariantEnumFloat", "Value2"s);
        d1.setValue("OptionalVariantEnumFloat", "Value2"s);
        d1.setValue("VariantPath", tmpFile);
        {
            ghoul::Dictionary e;
            e.setValue("1", tmpFile);
            e.setValue("2", tmpFile2);
            d1.setValue("VariantPath2", e);
        }
        {
            ghoul::Dictionary e;
            e.setValue("Var", "Value"s);
            d1.setValue("VariantStringVectorC", e);
        }
        {
            ghoul::Dictionary e;
            ghoul::Dictionary f;
            f.setValue("1", "Abc"s);
            f.setValue("2", "Def"s);
            e.setValue("Var", f);
            d1.setValue("VariantStringVectorD", e);
        }
        d1.setValue("VariantStringVector", "ghi"s);

        const Parameters p1 = codegen::bake<Parameters>(d1);
        CHECK(std::get<bool>(p1.boolDoubleValue) == false);
        CHECK(std::get<float>(p1.floatStringValue) == 2.f);
        CHECK(std::get<glm::ivec2>(p1.ivecsValue) == glm::ivec2(1, 2));
        CHECK(std::get<glm::dvec2>(p1.vecmat1) == glm::dvec2(3.1, 3.2));
        CHECK(std::get<glm::mat2x2>(p1.vecmat2) == glm::mat2x2(4.1f, 4.2f, 4.3f, 4.4f));
        CHECK(
            std::get<glm::mat3x4>(p1.restValue) ==
            glm::mat3x4(
                5.1f, 5.2f, 5.3f, 5.4f, 5.5f, 5.6f,
                5.7f, 5.8f, 5.9f, 5.10f, 5.11f, 5.12f
            )
        );
        REQUIRE(p1.optionalValue.has_value());
        CHECK(std::get<int>(*p1.optionalValue) == 5);
        REQUIRE(std::holds_alternative<std::string>(p1.variantVector));
        CHECK(std::get<std::string>(p1.variantVector) == "abc");
        REQUIRE(std::holds_alternative<std::string>(p1.variantVector2));
        CHECK(std::get<std::string>(p1.variantVector2) == "def");
        REQUIRE(std::holds_alternative<int>(p1.multipleVariantA));
        CHECK(std::get<int>(p1.multipleVariantA) == 2);
        REQUIRE(std::holds_alternative<int>(p1.multipleVariantB));
        CHECK(std::get<int>(p1.multipleVariantB) == 2);
        REQUIRE(std::holds_alternative<int>(p1.multipleVariantA2));
        CHECK(std::get<int>(p1.multipleVariantA2) == 2);
        REQUIRE(std::holds_alternative<Parameters::A>(p1.variantEnumFloat));
        CHECK(std::get<Parameters::A>(p1.variantEnumFloat) == Parameters::A::Value2);
        REQUIRE(p1.optionalVariantEnumFloat.has_value());
        REQUIRE(std::holds_alternative<Parameters::B>(*p1.optionalVariantEnumFloat));
        CHECK(
            std::get<Parameters::B>(*p1.optionalVariantEnumFloat) ==
            Parameters::B::Value2
        );
        REQUIRE(std::holds_alternative<std::filesystem::path>(p1.variantPath));
        CHECK(std::get<std::filesystem::path>(p1.variantPath) == tmpFile);
        REQUIRE(
            std::holds_alternative<std::vector<std::filesystem::path>>(p1.variantPath2)
        );
        REQUIRE(
            std::get<std::vector<std::filesystem::path>>(p1.variantPath2).size() == 2
        );
        CHECK(
            std::get<std::vector<std::filesystem::path>>(p1.variantPath2)[0] == tmpFile
        );
        CHECK(
            std::get<std::vector<std::filesystem::path>>(p1.variantPath2)[1] == tmpFile2
        );
        REQUIRE(std::holds_alternative<std::string>(p1.variantStringVectorC.var));
        CHECK(
            std::get<std::string>(p1.variantStringVectorC.var) == "Value"
        );
        REQUIRE(std::holds_alternative<std::vector<std::string>>(p1.variantStringVectorD.var));
        REQUIRE(
            std::get<std::vector<std::string>>(p1.variantStringVectorD.var).size() == 2
        );
        CHECK(
            std::get<std::vector<std::string>>(p1.variantStringVectorD.var)[0] == "Abc"
        );
        CHECK(
            std::get<std::vector<std::string>>(p1.variantStringVectorD.var)[1] == "Def"
        );
        REQUIRE(std::holds_alternative<std::string>(p1.variantStringVector));
        CHECK(std::get<std::string>(p1.variantStringVector) == "ghi");
    }

    {
        ghoul::Dictionary d2;
        d2.setValue("BoolDoubleValue", 6.0);
        d2.setValue("FloatStringValue", "abc"s);
        d2.setValue("IvecsValue", glm::dvec3(7.0, 8.0, 9.0));
        d2.setValue("Vecmat1", glm::dvec3(10.1, 10.2, 10.3));
        d2.setValue(
            "Vecmat2",
            glm::dmat3x3(11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8, 11.9)
        );
        d2.setValue(
            "RestValue",
            glm::dmat3x4(
                12.1, 12.2, 12.3, 12.4, 12.5, 12.6,
                12.7, 12.8, 12.9, 12.10, 12.11, 12.12
            )
        );
        {
            ghoul::Dictionary e;
            e.setValue("1", "abc"s);
            e.setValue("2", "def"s);
            e.setValue("3", "ghi"s);
            e.setValue("4", "jkl"s);
            d2.setValue("VariantVector", e);
        }
        {
            ghoul::Dictionary e;
            e.setValue("1", "mno"s);
            e.setValue("2", "pqr"s);
            e.setValue("3", "stu"s);
            e.setValue("4", "vwx"s);
            d2.setValue("VariantVector2", e);
        }
        d2.setValue("MultipleVariantA", "a"s);
        d2.setValue("MultipleVariantB", "a"s);
        d2.setValue("MultipleVariantA2", "a"s);
        d2.setValue("VariantEnumFloat", 2.0);
        d2.setValue("OptionalVariantEnumFloat", 2.0);
        {
            ghoul::Dictionary e;
            e.setValue("1", tmpFile);
            e.setValue("2", tmpFile2);
            d2.setValue("VariantPath", e);
        }
        d2.setValue("VariantPath2", tmpFile);
        {
            ghoul::Dictionary e;
            e.setValue("Var", "Value"s);
            d2.setValue("VariantStringVectorC", e);
        }
        {
            ghoul::Dictionary e;
            ghoul::Dictionary f;
            f.setValue("1", "Abc"s);
            f.setValue("2", "Def"s);
            e.setValue("Var", f);
            d2.setValue("VariantStringVectorD", e);
        }
        d2.setValue("VariantStringVector", "ghi"s);

        const Parameters p2 = codegen::bake<Parameters>(d2);
        CHECK(std::get<double>(p2.boolDoubleValue) == 6.0);
        CHECK(std::get<std::string>(p2.floatStringValue) == "abc");
        CHECK(std::get<glm::ivec3>(p2.ivecsValue) == glm::ivec3(7, 8, 9));
        CHECK(std::get<glm::dvec3>(p2.vecmat1) == glm::dvec3(10.1, 10.2, 10.3));
        CHECK(
            std::get<glm::dmat3x3>(p2.vecmat2) ==
            glm::dmat3x3(11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8, 11.9)
        );
        CHECK(
            std::get<glm::mat3x4>(p2.restValue) ==
            glm::mat3x4(
                12.1f, 12.2f, 12.3f, 12.4f, 12.5f, 12.6f,
                12.7f, 12.8f, 12.9f, 12.10f, 12.11f, 12.12f
            )
        );
        CHECK(!p2.optionalValue.has_value());
        REQUIRE(std::holds_alternative<std::vector<std::string>>(p2.variantVector));
        CHECK(
            std::get<std::vector<std::string>>(p2.variantVector) ==
            std::vector<std::string>{ "abc", "def", "ghi", "jkl" }
        );
        REQUIRE(std::holds_alternative<std::vector<std::string>>(p2.variantVector2));
        CHECK(
            std::get<std::vector<std::string>>(p2.variantVector2) ==
            std::vector<std::string>{ "mno", "pqr", "stu", "vwx" }
        );
        REQUIRE(std::holds_alternative<std::string>(p2.multipleVariantA));
        CHECK(std::get<std::string>(p2.multipleVariantA) == "a");
        REQUIRE(std::holds_alternative<std::string>(p2.multipleVariantB));
        CHECK(std::get<std::string>(p2.multipleVariantB) == "a");
        REQUIRE(std::holds_alternative<std::string>(p2.multipleVariantA2));
        CHECK(std::get<std::string>(p2.multipleVariantA2) == "a");
        REQUIRE(std::holds_alternative<float>(p2.variantEnumFloat));
        CHECK(std::get<float>(p2.variantEnumFloat) == 2.f);
        REQUIRE(p2.optionalVariantEnumFloat.has_value());
        REQUIRE(std::holds_alternative<float>(*p2.optionalVariantEnumFloat));
        CHECK(std::get<float>(*p2.optionalVariantEnumFloat) == 2.f);
        REQUIRE(
            std::holds_alternative<std::vector<std::filesystem::path>>(p2.variantPath)
        );
        REQUIRE(
            std::get<std::vector<std::filesystem::path>>(p2.variantPath).size() == 2
        );
        CHECK(
            std::get<std::vector<std::filesystem::path>>(p2.variantPath)[0] == tmpFile
        );
        CHECK(
            std::get<std::vector<std::filesystem::path>>(p2.variantPath)[1] == tmpFile2
        );
        REQUIRE(std::holds_alternative<std::filesystem::path>(p2.variantPath2));
        CHECK(std::get<std::filesystem::path>(p2.variantPath2) == tmpFile);
    }

    {
        ghoul::Dictionary d3;
        d3.setValue("BoolDoubleValue", 13.1);
        d3.setValue("FloatStringValue", "abc"s);
        d3.setValue("IvecsValue", glm::dvec4(14.0, 15.0, 16.0, 17.0));
        d3.setValue("Vecmat1", glm::dvec4(18.1, 18.2, 18.3, 18.4));
        d3.setValue(
            "Vecmat2",
            glm::dmat2x4(19.1, 19.2, 19.3, 19.4, 19.5, 19.6, 19.7, 19.8)
        );
        d3.setValue("RestValue", glm::dmat2x3(20.1, 20.2, 20.3, 20.4, 20.5, 20.6));
        d3.setValue("OptionalValue", true);
        d3.setValue("VariantVector", "abc"s);
        d3.setValue("VariantVector2", "def"s);
        d3.setValue("MultipleVariantA", 1.0);
        d3.setValue("MultipleVariantB", 1.0);
        d3.setValue("MultipleVariantA2", 1.0);
        d3.setValue("VariantEnumFloat", "Value1"s);
        d3.setValue("OptionalVariantEnumFloat", "Value1"s);
        d3.setValue("VariantPath", tmpFile);
        {
            ghoul::Dictionary e;
            e.setValue("1", tmpFile);
            e.setValue("2", tmpFile2);
            d3.setValue("VariantPath2", e);
        }
        {
            ghoul::Dictionary e;
            e.setValue("Var", "Value"s);
            d3.setValue("VariantStringVectorC", e);
        }
        {
            ghoul::Dictionary e;
            ghoul::Dictionary f;
            f.setValue("1", "Abc"s);
            f.setValue("2", "Def"s);
            e.setValue("Var", f);
            d3.setValue("VariantStringVectorD", e);
        }
        d3.setValue("VariantStringVector", "ghi"s);

        const Parameters p3 = codegen::bake<Parameters>(d3);
        CHECK(std::get<double>(p3.boolDoubleValue) == 13.1);
        CHECK(std::get<std::string>(p3.floatStringValue) == "abc");
        CHECK(std::get<glm::ivec4>(p3.ivecsValue) == glm::ivec4(14, 15, 16, 17));
        CHECK(std::get<glm::dvec4>(p3.vecmat1) == glm::dvec4(18.1, 18.2, 18.3, 18.4));
        CHECK(
            std::get<glm::mat2x4>(p3.vecmat2) ==
            glm::mat2x4(19.1f, 19.2f, 19.3f, 19.4f, 19.5f, 19.6f, 19.7f, 19.8f)
        );
        CHECK(
            std::get<glm::mat2x3>(p3.restValue) ==
            glm::mat2x3(20.1f, 20.2f, 20.3f, 20.4f, 20.5f, 20.6f)
        );
        REQUIRE(p3.optionalValue.has_value());
        CHECK(std::get<bool>(*p3.optionalValue) == true);
        REQUIRE(std::holds_alternative<std::string>(p3.variantVector));
        CHECK(std::get<std::string>(p3.variantVector) == "abc");
        REQUIRE(std::holds_alternative<std::string>(p3.variantVector2));
        CHECK(std::get<std::string>(p3.variantVector2) == "def");
        REQUIRE(std::holds_alternative<int>(p3.multipleVariantA));
        CHECK(std::get<int>(p3.multipleVariantA) == 1);
        REQUIRE(std::holds_alternative<int>(p3.multipleVariantB));
        CHECK(std::get<int>(p3.multipleVariantB) == 1);
        REQUIRE(std::holds_alternative<int>(p3.multipleVariantA2));
        CHECK(std::get<int>(p3.multipleVariantA2) == 1);
        REQUIRE(std::holds_alternative<Parameters::A>(p3.variantEnumFloat));
        CHECK(std::get<Parameters::A>(p3.variantEnumFloat) == Parameters::A::Value1);
        REQUIRE(p3.optionalVariantEnumFloat.has_value());
        REQUIRE(std::holds_alternative<Parameters::B>(*p3.optionalVariantEnumFloat));
        CHECK(
            std::get<Parameters::B>(*p3.optionalVariantEnumFloat) ==
            Parameters::B::Value1
        );
    }

    {
        ghoul::Dictionary d4;
        d4.setValue("BoolDoubleValue", 20.0);
        d4.setValue("FloatStringValue", "abc"s);
        d4.setValue("IvecsValue", glm::dvec2(21.0, 22.0));
        d4.setValue(
            "Vecmat1",
            glm::dmat4x4(
                23.1, 23.2, 23.3, 23.4, 23.5, 23.6, 23.7, 23.8,
                23.9, 23.10, 23.11, 23.12, 23.13, 23.14, 23.15, 23.16
            )
        );
        d4.setValue("Vecmat2", glm::dmat2x2(24.1, 24.2, 24.3, 24.4));
        d4.setValue(
            "RestValue",
            glm::dmat3x4(
                25.1, 25.2, 25.3, 25.4, 25.5, 25.6, 25.7, 25.8, 25.9, 25.10, 25.11, 25.12
            )
        );
        d4.setValue("VariantVector", "abc"s);
        d4.setValue("VariantVector2", "def"s);
        d4.setValue("MultipleVariantA", "abc"s);
        d4.setValue("MultipleVariantB", "def"s);
        d4.setValue("MultipleVariantA2", "ghi"s);
        d4.setValue("VariantEnumFloat", 5.0);
        d4.setValue("OptionalVariantEnumFloat", 5.0);
        d4.setValue("VariantPath", tmpFile);
        {
            ghoul::Dictionary e;
            e.setValue("1", tmpFile);
            e.setValue("2", tmpFile2);
            d4.setValue("VariantPath2", e);
        }
        {
            ghoul::Dictionary e;
            e.setValue("Var", "Value"s);
            d4.setValue("VariantStringVectorC", e);
        }
        {
            ghoul::Dictionary e;
            ghoul::Dictionary f;
            f.setValue("1", "Abc"s);
            f.setValue("2", "Def"s);
            e.setValue("Var", f);
            d4.setValue("VariantStringVectorD", e);
        }
        d4.setValue("VariantStringVector", "ghi"s);

        const Parameters p4 = codegen::bake<Parameters>(d4);
        CHECK(std::get<double>(p4.boolDoubleValue) == 20);
        CHECK(std::get<std::string>(p4.floatStringValue) == "abc");
        CHECK(std::get<glm::ivec2>(p4.ivecsValue) == glm::ivec2(21, 22));
        CHECK(
            std::get<glm::dmat4x4>(p4.vecmat1) ==
            glm::dmat4x4(
                23.1, 23.2, 23.3, 23.4, 23.5, 23.6, 23.7, 23.8,
                23.9, 23.10, 23.11, 23.12, 23.13, 23.14, 23.15, 23.16
            )
        );
        CHECK(
            std::get<glm::mat2x2>(p4.vecmat2) == glm::mat2x2(24.1f, 24.2f, 24.3f, 24.4f)
        );
        CHECK(
            std::get<glm::mat3x4>(p4.restValue) ==
            glm::mat3x4(
                25.1f, 25.2f, 25.3f, 25.4f, 25.5f, 25.6f,
                25.7f, 25.8f, 25.9f, 25.10f, 25.11f, 25.12f
            )
        );
        CHECK(!p4.optionalValue.has_value());
        REQUIRE(std::holds_alternative<std::string>(p4.variantVector));
        CHECK(std::get<std::string>(p4.variantVector) == "abc");
        REQUIRE(std::holds_alternative<std::string>(p4.variantVector2));
        CHECK(std::get<std::string>(p4.variantVector2) == "def");
        REQUIRE(std::holds_alternative<std::string>(p4.multipleVariantA));
        CHECK(std::get<std::string>(p4.multipleVariantA) == "abc");
        REQUIRE(std::holds_alternative<std::string>(p4.multipleVariantB));
        CHECK(std::get<std::string>(p4.multipleVariantB) == "def");
        REQUIRE(std::holds_alternative<std::string>(p4.multipleVariantA2));
        CHECK(std::get<std::string>(p4.multipleVariantA2) == "ghi");
        REQUIRE(std::holds_alternative<float>(p4.variantEnumFloat));
        CHECK(std::get<float>(p4.variantEnumFloat) == 5.f);
        REQUIRE(p4.optionalVariantEnumFloat.has_value());
        REQUIRE(std::holds_alternative<float>(*p4.optionalVariantEnumFloat));
        CHECK(std::get<float>(*p4.optionalVariantEnumFloat) == 5.f);
    }
}

TEST_CASE("Execution/Structs/Variant:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 20);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "BoolDoubleValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "boolDouble value documentation");
        CHECK(e.verifier->type() == "Boolean, or Double");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "Boolean");
        CHECK(dynamic_cast<BoolVerifier*>(v->values[0].get()));
        CHECK(v->values[1]->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(v->values[1].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "FloatStringValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "floatString value documentation");
        CHECK(e.verifier->type() == "Double, or String");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(v->values[0].get()));
        CHECK(v->values[1]->type() == "String");
        StringVerifier* w = dynamic_cast<StringVerifier*>(v->values[1].get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[2];
        CHECK(e.key == "IvecsValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "ivecs value documentation");
        CHECK(e.verifier->type() == "Vector2<int>, Vector3<int>, or Vector4<int>");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 3);
        CHECK(v->values[0]->type() == "Vector2<int>");
        CHECK(dynamic_cast<Vector2Verifier<int>*>(v->values[0].get()));
        CHECK(v->values[1]->type() == "Vector3<int>");
        CHECK(dynamic_cast<Vector3Verifier<int>*>(v->values[1].get()));
        CHECK(v->values[2]->type() == "Vector4<int>");
        CHECK(dynamic_cast<Vector4Verifier<int>*>(v->values[2].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[3];
        CHECK(e.key == "Vecmat1");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vecmat1 value documentation");
        CHECK(
            e.verifier->type() ==
            "Vector2<double>, Vector3<double>, Vector4<double>, Matrix2x3<double>, "
            "Matrix4x2<double>, or Matrix4x4<double>"
        );
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 6);
        CHECK(v->values[0]->type() == "Vector2<double>");
        CHECK(dynamic_cast<Vector2Verifier<double>*>(v->values[0].get()));
        CHECK(v->values[1]->type() == "Vector3<double>");
        CHECK(dynamic_cast<Vector3Verifier<double>*>(v->values[1].get()));
        CHECK(v->values[2]->type() == "Vector4<double>");
        CHECK(dynamic_cast<Vector4Verifier<double>*>(v->values[2].get()));
        CHECK(v->values[3]->type() == "Matrix2x3<double>");
        CHECK(dynamic_cast<Matrix2x3Verifier<double>*>(v->values[3].get()));
        CHECK(v->values[4]->type() == "Matrix4x2<double>");
        CHECK(dynamic_cast<Matrix4x2Verifier<double>*>(v->values[4].get()));
        CHECK(v->values[5]->type() == "Matrix4x4<double>");
        CHECK(dynamic_cast<Matrix4x4Verifier<double>*>(v->values[5].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[4];
        CHECK(e.key == "Vecmat2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vecmat2 value documentation");
        CHECK(
            e.verifier->type() ==
            "Matrix2x2<double>, Matrix3x3<double>, or Matrix2x4<double>"
        );
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 3);
        CHECK(v->values[0]->type() == "Matrix2x2<double>");
        CHECK(dynamic_cast<Matrix2x2Verifier<double>*>(v->values[0].get()));
        CHECK(v->values[1]->type() == "Matrix3x3<double>");
        CHECK(dynamic_cast<Matrix3x3Verifier<double>*>(v->values[1].get()));
        CHECK(v->values[2]->type() == "Matrix2x4<double>");
        CHECK(dynamic_cast<Matrix2x4Verifier<double>*>(v->values[2].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[5];
        CHECK(e.key == "RestValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "rest value documentation");
        CHECK(
            e.verifier->type() ==
            "Vector2<double>, Vector3<double>, Vector4<double>, Matrix2x3<double>, "
            "Matrix2x4<double>, Matrix3x3<double>, Matrix3x4<double>, or "
            "Matrix4x4<double>"
        );
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 8);
        CHECK(v->values[0]->type() == "Vector2<double>");
        CHECK(dynamic_cast<Vector2Verifier<double>*>(v->values[0].get()));
        CHECK(v->values[1]->type() == "Vector3<double>");
        CHECK(dynamic_cast<Vector3Verifier<double>*>(v->values[1].get()));
        CHECK(v->values[2]->type() == "Vector4<double>");
        CHECK(dynamic_cast<Vector4Verifier<double>*>(v->values[2].get()));
        CHECK(v->values[3]->type() == "Matrix2x3<double>");
        CHECK(dynamic_cast<Matrix2x3Verifier<double>*>(v->values[3].get()));
        CHECK(v->values[4]->type() == "Matrix2x4<double>");
        CHECK(dynamic_cast<Matrix2x4Verifier<double>*>(v->values[4].get()));
        CHECK(v->values[5]->type() == "Matrix3x3<double>");
        CHECK(dynamic_cast<Matrix3x3Verifier<double>*>(v->values[5].get()));
        CHECK(v->values[6]->type() == "Matrix3x4<double>");
        CHECK(dynamic_cast<Matrix3x4Verifier<double>*>(v->values[6].get()));
        CHECK(v->values[7]->type() == "Matrix4x4<double>");
        CHECK(dynamic_cast<Matrix4x4Verifier<double>*>(v->values[7].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[6];
        CHECK(e.key == "OptionalValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optional documentation");
        CHECK(e.verifier->type() == "Boolean, or Integer");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "Boolean");
        CHECK(dynamic_cast<BoolVerifier*>(v->values[0].get()));
        CHECK(v->values[1]->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(v->values[1].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[7];
        CHECK(e.key == "VariantVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variant vector documentation");
        CHECK(e.verifier->type() == "List of strings, or String");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "List of strings");
        StringListVerifier* w = dynamic_cast<StringListVerifier*>(v->values[0].get());
        REQUIRE(w);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].verifier->type() == "String");
        StringVerifier* u =
            dynamic_cast<StringVerifier*>(w->documentations[0].verifier.get());
        REQUIRE(u);
        CHECK(u->mustBeNotEmpty() == false);
        CHECK(v->values[1]->type() == "String");
        StringVerifier* x = dynamic_cast<StringVerifier*>(v->values[1].get());
        REQUIRE(x);
        CHECK(x->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[8];
        CHECK(e.key == "VariantVector2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variant vector 2 documentation");
        CHECK(e.verifier->type() == "String, or List of strings");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "String");
        StringVerifier* w = dynamic_cast<StringVerifier*>(v->values[0].get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);
        CHECK(v->values[1]->type() == "List of strings");
        StringListVerifier* u = dynamic_cast<StringListVerifier*>(v->values[1].get());
        REQUIRE(u);
        REQUIRE(u->documentations.size() == 1);
        CHECK(u->documentations[0].key == "*");
        CHECK(!u->documentations[0].optional);
        CHECK(u->documentations[0].verifier->type() == "String");
        StringVerifier* x =
            dynamic_cast<StringVerifier*>(u->documentations[0].verifier.get());
        REQUIRE(x);
        CHECK(x->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[9];
        CHECK(e.key == "MultipleVariantA");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "multiple variant a documentation");
        CHECK(e.verifier->type() == "Integer, or String");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(v->values[0].get()));
        CHECK(v->values[1]->type() == "String");
        StringVerifier* w = dynamic_cast<StringVerifier*>(v->values[1].get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[10];
        CHECK(e.key == "MultipleVariantB");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "multiple variant b documentation");
        CHECK(e.verifier->type() == "String, or Integer");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "String");
        StringVerifier* w = dynamic_cast<StringVerifier*>(v->values[0].get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);
        CHECK(v->values[1]->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(v->values[1].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[11];
        CHECK(e.key == "MultipleVariantA2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "multiple variant a2 documentation");
        CHECK(e.verifier->type() == "Integer, or String");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(v->values[0].get()));
        CHECK(v->values[1]->type() == "String");
        StringVerifier* w = dynamic_cast<StringVerifier*>(v->values[1].get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[12];
        CHECK(e.key == "MultipleVariantB2");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "multiple variant b2 documentation");
        CHECK(e.verifier->type() == "String, or Integer");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "String");
        StringVerifier* w = dynamic_cast<StringVerifier*>(v->values[0].get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);
        CHECK(v->values[1]->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(v->values[1].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[13];
        CHECK(e.key == "VariantEnumFloat");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variant enum float documentation");
        CHECK(e.verifier->type() == "String, or Double");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "String");
        StringInListVerifier* w = dynamic_cast<StringInListVerifier*>(v->values[0].get());
        REQUIRE(w);
        CHECK(w->values == std::vector<std::string>{ "Value1", "Value2", "Value3" });
        CHECK(v->values[1]->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(v->values[1].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[14];
        CHECK(e.key == "OptionalVariantEnumFloat");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optional variant enum float documentation");
        CHECK(e.verifier->type() == "String, or Double");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "String");
        StringInListVerifier* w = dynamic_cast<StringInListVerifier*>(v->values[0].get());
        REQUIRE(w);
        CHECK(w->values == std::vector<std::string>{ "Value1", "Value2", "Value3" });
        CHECK(v->values[1]->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(v->values[1].get()));
    }
    {
        const DocumentationEntry& e = doc.entries[15];
        CHECK(e.key == "VariantPath");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variant path vector path documentation");
        CHECK(e.verifier->type() == "File, or Table");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(v->values[0].get());
        REQUIRE(w);
        CHECK(v->values[1]->type() == "Table");
        TableVerifier* w2 = dynamic_cast<TableVerifier*>(v->values[1].get());
        REQUIRE(w2->documentations.size() == 1);
        CHECK(w2->documentations[0].key == "*");
        CHECK(w2->documentations[0].verifier->type() == "File");
        CHECK(dynamic_cast<FileVerifier*>(w2->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[16];
        CHECK(e.key == "VariantPath2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variant vector path path documentation");
        CHECK(e.verifier->type() == "Table, or File");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "Table");
        TableVerifier* w2 = dynamic_cast<TableVerifier*>(v->values[0].get());
        REQUIRE(w2->documentations.size() == 1);
        CHECK(w2->documentations[0].key == "*");
        CHECK(w2->documentations[0].verifier->type() == "File");
        CHECK(dynamic_cast<FileVerifier*>(w2->documentations[0].verifier.get()));
        CHECK(v->values[1]->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(v->values[1].get());
        REQUIRE(w);
    }
    {
        const DocumentationEntry& e = doc.entries[17];
        CHECK(e.key == "VariantStringVectorC");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variantStringVectorC variable");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "Var");
        CHECK(!v->documentations[0].isPrivate);
        CHECK(v->documentations[0].verifier->type() == "String, or List of strings");
        OrVerifier* w = dynamic_cast<OrVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        REQUIRE(w->values.size() == 2);
        CHECK(w->values[0]->type() == "String");
        StringVerifier* u = dynamic_cast<StringVerifier*>(w->values[0].get());
        REQUIRE(u);
        CHECK(w->values[1]->type() == "List of strings");
        StringListVerifier* x = dynamic_cast<StringListVerifier*>(w->values[1].get());
        REQUIRE(x);
        REQUIRE(x->documentations.size() == 1);
        CHECK(x->documentations[0].key == "*");
        CHECK(!x->documentations[0].optional);
        CHECK(x->documentations[0].verifier->type() == "String");
    }
    {
        const DocumentationEntry& e = doc.entries[18];
        CHECK(e.key == "VariantStringVectorD");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variantStringVectorD variable");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "Var");
        CHECK(!v->documentations[0].isPrivate);
        CHECK(v->documentations[0].verifier->type() == "String, or List of strings");
        OrVerifier* w = dynamic_cast<OrVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        REQUIRE(w->values.size() == 2);
        CHECK(w->values[0]->type() == "String");
        StringVerifier* u = dynamic_cast<StringVerifier*>(w->values[0].get());
        REQUIRE(u);
        CHECK(w->values[1]->type() == "List of strings");
        StringListVerifier* x = dynamic_cast<StringListVerifier*>(w->values[1].get());
        REQUIRE(x);
        REQUIRE(x->documentations.size() == 1);
        CHECK(x->documentations[0].key == "*");
        CHECK(!x->documentations[0].optional);
        CHECK(x->documentations[0].verifier->type() == "String");
    }
    {
        const DocumentationEntry& e = doc.entries[19];
        CHECK(e.key == "VariantStringVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variantStringVector");
        CHECK(e.verifier->type() == "String, or List of strings");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "String");
        StringVerifier* w = dynamic_cast<StringVerifier*>(v->values[0].get());
        REQUIRE(w);
        CHECK(v->values[1]->type() == "List of strings");
        StringListVerifier* u = dynamic_cast<StringListVerifier*>(v->values[1].get());
        REQUIRE(u);
        REQUIRE(u->documentations.size() == 1);
        CHECK(u->documentations[0].key == "*");
        CHECK(!u->documentations[0].optional);
        CHECK(u->documentations[0].verifier->type() == "String");
    }
}
