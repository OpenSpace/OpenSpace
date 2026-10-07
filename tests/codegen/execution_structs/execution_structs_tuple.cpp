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
#include <tuple>
#include <vector>

using namespace openspace;

namespace {
    struct [[codegen::Dictionary(Tuple)]] Parameters {
        // boolDouble value documentation
        std::tuple<bool, double> boolDoubleValue;

        // floatString value documentation
        std::tuple<float, std::string> floatStringValue;

        // ivecs value documentation
        std::tuple<glm::ivec2, glm::ivec3, glm::ivec4> ivecsValue;

        // vecmat1 value documentation
        std::tuple<glm::dvec2, glm::dvec3, glm::dvec4, glm::dmat2x3, glm::dmat4x2,
            glm::dmat4x4> vecmat1;

        // vecmat2 value documentation
        std::tuple<glm::dmat2x2, glm::dmat3x3, glm::dmat2x4> vecmat2;

        // rest value documentation
        std::tuple<glm::vec2, glm::vec3, glm::vec4, glm::mat2x3, glm::mat2x4,
            glm::mat3x3, glm::mat3x4, glm::mat4x4> restValue;

        // optional documentation
        std::optional<std::tuple<bool, int>> optionalValue;

        // tuple vector documentation
        std::tuple<std::vector<std::string>, std::string> tupleVector;

        // tuple vector 2 documentation
        std::tuple<std::string, std::vector<std::string>> tupleVector2;

        // multiple tuple a documentation
        std::tuple<int, std::string> multipleTupleA;

        // multiple tuple b documentation
        std::tuple<std::string, int> multipleTupleB;

        // multiple tuple a2 documentation
        std::tuple<int, std::string> multipleTupleA2;

        // multiple tuple b2 documentation
        std::optional<std::tuple<std::string, int>> multipleTupleB2;

        enum class A {
            Value1,
            Value2,
            Value3
        };
        // tuple enum float documentation
        std::tuple<A, float> tupleEnumFloat;

        enum class B {
            Value1,
            Value2,
            Value3
        };
        // optional tuple enum float documentation
        std::optional<std::tuple<B, float>> optionalTupleEnumFloat;
    };
} // namespace
#include "execution_structs_tuple_codegen.cpp"


TEST_CASE("Execution/Structs/Tuple:  Bake", "[Execution][Structs]") {
    using namespace std::string_literals;

    Dictionary d;
    {
        Dictionary e;
        e.setValue("1", true);
        e.setValue("2", 1.2);
        d.setValue("BoolDoubleValue", e);
    }
    {
        Dictionary e;
        e.setValue("1", 2.2);
        e.setValue("2", "abc"s);
        d.setValue("FloatStringValue", e);
    }
    {
        Dictionary e;
        e.setValue("1", glm::dvec2(1.0, 2.0));
        e.setValue("2", glm::dvec3(3.0, 4.0, 5.0));
        e.setValue("3", glm::dvec4(6.0, 7.0, 8.0, 9.0));
        d.setValue("IvecsValue", e);
    }
    {
        Dictionary e;
        e.setValue("1", glm::dvec2(1.1, 2.2));
        e.setValue("2", glm::dvec3(3.3, 4.4, 5.5));
        e.setValue("3", glm::dvec4(6.6, 7.7, 8.8, 9.9));
        e.setValue("4", glm::dmat2x3(10.10, 11.11, 12.12, 13.13, 14.14, 15.15));
        e.setValue("5", glm::dmat4x2(16.16, 17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23));
        e.setValue("6", glm::dmat4x4(24.24, 25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32, 33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39));
        d.setValue("Vecmat1", e);
    }
    {
        Dictionary e;
        e.setValue("1", glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
        e.setValue("2", glm::dmat3x3(5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12, 13.13));
        e.setValue("3", glm::dmat2x4(14.14, 15.15, 16.16, 17.17, 18.18, 19.19, 20.20, 21.21));
        d.setValue("Vecmat2", e);
    }
    {
        Dictionary e;
        e.setValue("1", glm::dvec2(1.1, 2.2));
        e.setValue("2", glm::dvec3(3.3, 4.4, 5.5));
        e.setValue("3", glm::dvec4(6.6, 7.7, 8.8, 9.9));
        e.setValue("4", glm::dmat2x3(10.10, 11.11, 12.12, 13.13, 14.14, 15.15));
        e.setValue("5", glm::dmat2x4(16.16, 17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23));
        e.setValue("6", glm::dmat3x3(24.24, 25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32));
        e.setValue("7", glm::dmat3x4(33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39, 40.40, 41.41, 42.42, 43.43, 44.44));
        e.setValue("8", glm::dmat4x4(45.45, 46.46, 47.47, 48.48, 49.49, 50.50, 51.51, 52.52, 53.53, 54.54, 55.55, 56.56, 57.57, 58.58, 59.59, 60.60));
        d.setValue("RestValue", e);
    }
    {
        Dictionary e;
        {
            Dictionary f;
            f.setValue("1", "abc"s);
            f.setValue("2", "def"s);
            f.setValue("3", "ghi"s);
            e.setValue("1", f);
        }
        e.setValue("2", "jkl"s);
        d.setValue("TupleVector", e);
    }
    {
        Dictionary e;
        e.setValue("1", "abc"s);
        {
            Dictionary f;
            f.setValue("1", "def"s);
            f.setValue("2", "ghi"s);
            f.setValue("3", "jkl"s);
            e.setValue("2", f);
        }
        d.setValue("TupleVector2", e);
    }
    {
        Dictionary e;
        e.setValue("1", 1.0);
        e.setValue("2", "abc"s);
        d.setValue("MultipleTupleA", e);
    }
    {
        Dictionary e;
        e.setValue("1", "abc"s);
        e.setValue("2", 1.0);
        d.setValue("MultipleTupleB", e);
    }
    {
        Dictionary e;
        e.setValue("1", 1.0);
        e.setValue("2", "abc"s);
        d.setValue("MultipleTupleA2", e);
    }
    {
        Dictionary e;
        e.setValue("1", "Value2"s);
        e.setValue("2", 1.1);
        d.setValue("TupleEnumFloat", e);
    }

    const Parameters p1 = codegen::bake<Parameters>(d);
    CHECK(std::get<0>(p1.boolDoubleValue) == true);
    CHECK(std::get<1>(p1.boolDoubleValue) == 1.2);
    CHECK(std::get<0>(p1.floatStringValue) == 2.2f);
    CHECK(std::get<1>(p1.floatStringValue) == "abc"s);
    CHECK(std::get<0>(p1.ivecsValue) == glm::ivec2(1, 2));
    CHECK(std::get<1>(p1.ivecsValue) == glm::ivec3(3, 4, 5));
    CHECK(std::get<2>(p1.ivecsValue) == glm::ivec4(6, 7, 8, 9));
    CHECK(std::get<0>(p1.vecmat1) == glm::dvec2(1.1, 2.2));
    CHECK(std::get<1>(p1.vecmat1) == glm::dvec3(3.3, 4.4, 5.5));
    CHECK(std::get<2>(p1.vecmat1) == glm::dvec4(6.6, 7.7, 8.8, 9.9));
    CHECK(std::get<3>(p1.vecmat1) == glm::dmat2x3(10.10, 11.11, 12.12, 13.13, 14.14, 15.15));
    CHECK(std::get<4>(p1.vecmat1) == glm::dmat4x2(16.16, 17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23));
    CHECK(std::get<5>(p1.vecmat1) == glm::dmat4x4(24.24, 25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32, 33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39));
    CHECK(std::get<0>(p1.vecmat2) == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    CHECK(std::get<1>(p1.vecmat2) == glm::dmat3x3(5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12, 13.13));
    CHECK(std::get<2>(p1.vecmat2) == glm::dmat2x4(14.14, 15.15, 16.16, 17.17, 18.18, 19.19, 20.20, 21.21));
    CHECK(std::get<0>(p1.restValue) == glm::vec2(1.1f, 2.2f));
    CHECK(std::get<1>(p1.restValue) == glm::vec3(3.3f, 4.4f, 5.5f));
    CHECK(std::get<2>(p1.restValue) == glm::vec4(6.6f, 7.7f, 8.8f, 9.9f));
    CHECK(std::get<3>(p1.restValue) == glm::mat2x3(10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f));
    CHECK(std::get<4>(p1.restValue) == glm::mat2x4(16.16f, 17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f));
    CHECK(std::get<5>(p1.restValue) == glm::mat3x3(24.24f, 25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f));
    CHECK(std::get<6>(p1.restValue) == glm::mat3x4(33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38f, 39.39f, 40.40f, 41.41f, 42.42f, 43.43f, 44.44f));
    CHECK(std::get<7>(p1.restValue) == glm::mat4x4(45.45f, 46.46f, 47.47f, 48.48f, 49.49f, 50.50f, 51.51f, 52.52f, 53.53f, 54.54f, 55.55f, 56.56f, 57.57f, 58.58f, 59.59f, 60.60f));
    CHECK(!p1.optionalValue.has_value());
    CHECK(std::get<0>(p1.tupleVector) == std::vector{ "abc"s, "def"s, "ghi"s });
    CHECK(std::get<1>(p1.tupleVector) == "jkl"s);
    CHECK(std::get<0>(p1.tupleVector2) == "abc"s);
    CHECK(std::get<1>(p1.tupleVector2) == std::vector{ "def"s, "ghi"s, "jkl"s });
    CHECK(std::get<0>(p1.multipleTupleA) == 1);
    CHECK(std::get<1>(p1.multipleTupleA) == "abc"s);
    CHECK(std::get<0>(p1.multipleTupleB) == "abc"s);
    CHECK(std::get<1>(p1.multipleTupleB) == 1);
    CHECK(std::get<0>(p1.multipleTupleA2) == 1);
    CHECK(std::get<1>(p1.multipleTupleA2) == "abc"s);
    CHECK(!p1.multipleTupleB2.has_value());
    CHECK(std::get<0>(p1.tupleEnumFloat) == Parameters::A::Value2);
    CHECK(std::get<1>(p1.tupleEnumFloat) == 1.1f);
    CHECK(!p1.optionalTupleEnumFloat.has_value());

    {
        Dictionary e;
        e.setValue("1", true);
        e.setValue("2", 2.0);
        d.setValue("OptionalValue", e);
    }
    {
        Dictionary e;
        e.setValue("1", "abc"s);
        e.setValue("2", 2.0);
        d.setValue("MultipleTupleB2", e);
    }
    {
        Dictionary e;
        e.setValue("1", "Value3"s);
        e.setValue("2", 1.1);
        d.setValue("OptionalTupleEnumFloat", e);
    }

    const Parameters p2 = codegen::bake<Parameters>(d);
    CHECK(std::get<0>(p2.boolDoubleValue) == true);
    CHECK(std::get<1>(p2.boolDoubleValue) == 1.2);
    CHECK(std::get<0>(p2.floatStringValue) == 2.2f);
    CHECK(std::get<1>(p2.floatStringValue) == "abc"s);
    CHECK(std::get<0>(p2.ivecsValue) == glm::ivec2(1, 2));
    CHECK(std::get<1>(p2.ivecsValue) == glm::ivec3(3, 4, 5));
    CHECK(std::get<2>(p2.ivecsValue) == glm::ivec4(6, 7, 8, 9));
    CHECK(std::get<0>(p2.vecmat1) == glm::dvec2(1.1, 2.2));
    CHECK(std::get<1>(p2.vecmat1) == glm::dvec3(3.3, 4.4, 5.5));
    CHECK(std::get<2>(p2.vecmat1) == glm::dvec4(6.6, 7.7, 8.8, 9.9));
    CHECK(std::get<3>(p2.vecmat1) == glm::dmat2x3(10.10, 11.11, 12.12, 13.13, 14.14, 15.15));
    CHECK(std::get<4>(p2.vecmat1) == glm::dmat4x2(16.16, 17.17, 18.18, 19.19, 20.20, 21.21, 22.22, 23.23));
    CHECK(std::get<5>(p2.vecmat1) == glm::dmat4x4(24.24, 25.25, 26.26, 27.27, 28.28, 29.29, 30.30, 31.31, 32.32, 33.33, 34.34, 35.35, 36.36, 37.37, 38.38, 39.39));
    CHECK(std::get<0>(p2.vecmat2) == glm::dmat2x2(1.1, 2.2, 3.3, 4.4));
    CHECK(std::get<1>(p2.vecmat2) == glm::dmat3x3(5.5, 6.6, 7.7, 8.8, 9.9, 10.10, 11.11, 12.12, 13.13));
    CHECK(std::get<2>(p2.vecmat2) == glm::dmat2x4(14.14, 15.15, 16.16, 17.17, 18.18, 19.19, 20.20, 21.21));
    CHECK(std::get<0>(p2.restValue) == glm::vec2(1.1f, 2.2f));
    CHECK(std::get<1>(p2.restValue) == glm::vec3(3.3f, 4.4f, 5.5f));
    CHECK(std::get<2>(p2.restValue) == glm::vec4(6.6f, 7.7f, 8.8f, 9.9f));
    CHECK(std::get<3>(p2.restValue) == glm::mat2x3(10.10f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f));
    CHECK(std::get<4>(p2.restValue) == glm::mat2x4(16.16f, 17.17f, 18.18f, 19.19f, 20.20f, 21.21f, 22.22f, 23.23f));
    CHECK(std::get<5>(p2.restValue) == glm::mat3x3(24.24f, 25.25f, 26.26f, 27.27f, 28.28f, 29.29f, 30.30f, 31.31f, 32.32f));
    CHECK(std::get<6>(p2.restValue) == glm::mat3x4(33.33f, 34.34f, 35.35f, 36.36f, 37.37f, 38.38f, 39.39f, 40.40f, 41.41f, 42.42f, 43.43f, 44.44f));
    CHECK(std::get<7>(p2.restValue) == glm::mat4x4(45.45f, 46.46f, 47.47f, 48.48f, 49.49f, 50.50f, 51.51f, 52.52f, 53.53f, 54.54f, 55.55f, 56.56f, 57.57f, 58.58f, 59.59f, 60.60f));
    REQUIRE(p2.optionalValue.has_value());
    CHECK(std::get<0>(*p2.optionalValue) == true);
    CHECK(std::get<1>(*p2.optionalValue) == 2);
    CHECK(std::get<0>(p2.tupleVector) == std::vector{ "abc"s, "def"s, "ghi"s });
    CHECK(std::get<1>(p2.tupleVector) == "jkl"s);
    CHECK(std::get<0>(p2.tupleVector2) == "abc"s);
    CHECK(std::get<1>(p2.tupleVector2) == std::vector{ "def"s, "ghi"s, "jkl"s });
    CHECK(std::get<0>(p2.multipleTupleA) == 1);
    CHECK(std::get<1>(p2.multipleTupleA) == "abc"s);
    CHECK(std::get<0>(p2.multipleTupleB) == "abc"s);
    CHECK(std::get<1>(p2.multipleTupleB) == 1);
    CHECK(std::get<0>(p2.multipleTupleA2) == 1);
    CHECK(std::get<1>(p2.multipleTupleA2) == "abc"s);
    REQUIRE(p2.multipleTupleB2.has_value());
    CHECK(std::get<0>(*p2.multipleTupleB2) == "abc"s);
    CHECK(std::get<1>(*p2.multipleTupleB2) == 2);
    CHECK(std::get<0>(p2.tupleEnumFloat) == Parameters::A::Value2);
    CHECK(std::get<1>(p2.tupleEnumFloat) == 1.1f);
    REQUIRE(p2.optionalTupleEnumFloat.has_value());
    CHECK(std::get<0>(*p2.optionalTupleEnumFloat) == Parameters::B::Value3);
    CHECK(std::get<1>(*p2.optionalTupleEnumFloat) == 1.1f);
}

TEST_CASE("Execution/Structs/Tuple:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 15);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "BoolDoubleValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "boolDouble value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            BoolVerifier* w = dynamic_cast<BoolVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Boolean");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVerifier* w = dynamic_cast<DoubleVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Double");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "FloatStringValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "floatString value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVerifier* w = dynamic_cast<DoubleVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Double");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            StringVerifier* w = dynamic_cast<StringVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "String");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[2];
        CHECK(e.key == "IvecsValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "ivecs value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 3);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            IntVector2Verifier* w = dynamic_cast<IntVector2Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Vector2<int>");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            IntVector3Verifier* w = dynamic_cast<IntVector3Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Vector3<int>");
        }
        {
            const DocumentationEntry& f = v->documentations[2];
            CHECK(f.key == "3");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            IntVector4Verifier* w = dynamic_cast<IntVector4Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Vector4<int>");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[3];
        CHECK(e.key == "Vecmat1");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vecmat1 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 6);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVector2Verifier* w = dynamic_cast<DoubleVector2Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Vector2<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVector3Verifier* w = dynamic_cast<DoubleVector3Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Vector3<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[2];
            CHECK(f.key == "3");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVector4Verifier* w = dynamic_cast<DoubleVector4Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Vector4<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[3];
            CHECK(f.key == "4");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix2x3Verifier* w = dynamic_cast<DoubleMatrix2x3Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix2x3<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[4];
            CHECK(f.key == "5");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix4x2Verifier* w = dynamic_cast<DoubleMatrix4x2Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix4x2<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[5];
            CHECK(f.key == "6");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix4x4Verifier* w = dynamic_cast<DoubleMatrix4x4Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix4x4<double>");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[4];
        CHECK(e.key == "Vecmat2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vecmat2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 3);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix2Verifier* w = dynamic_cast<DoubleMatrix2Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix2x2<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix3Verifier* w = dynamic_cast<DoubleMatrix3Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix3x3<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[2];
            CHECK(f.key == "3");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix2x4Verifier* w = dynamic_cast<DoubleMatrix2x4Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix2x4<double>");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[5];
        CHECK(e.key == "RestValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "rest value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 8);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVector2Verifier* w = dynamic_cast<DoubleVector2Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Vector2<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVector3Verifier* w = dynamic_cast<DoubleVector3Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Vector3<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[2];
            CHECK(f.key == "3");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVector4Verifier* w = dynamic_cast<DoubleVector4Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Vector4<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[3];
            CHECK(f.key == "4");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix2x3Verifier* w = dynamic_cast<DoubleMatrix2x3Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix2x3<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[4];
            CHECK(f.key == "5");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix2x4Verifier* w = dynamic_cast<DoubleMatrix2x4Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix2x4<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[5];
            CHECK(f.key == "6");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix3Verifier* w = dynamic_cast<DoubleMatrix3Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix3x3<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[6];
            CHECK(f.key == "7");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix3x4Verifier* w = dynamic_cast<DoubleMatrix3x4Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix3x4<double>");
        }
        {
            const DocumentationEntry& f = v->documentations[7];
            CHECK(f.key == "8");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleMatrix4x4Verifier* w = dynamic_cast<DoubleMatrix4x4Verifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Matrix4x4<double>");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[6];
        CHECK(e.key == "OptionalValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optional documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            BoolVerifier* w = dynamic_cast<BoolVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Boolean");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            IntVerifier* w = dynamic_cast<IntVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Integer");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[7];
        CHECK(e.key == "TupleVector");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "tuple vector documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            TableVerifier* t = dynamic_cast<TableVerifier*>(f.verifier.get());
            REQUIRE(t);
            REQUIRE(t->documentations.size() == 1);
            CHECK(t->documentations[0].key == "*");
            CHECK(t->documentations[0].verifier->type() == "String");
            StringVerifier* w = dynamic_cast<StringVerifier*>(
                t->documentations[0].verifier.get()
            );
            REQUIRE(w);
            CHECK(!w->mustBeNotEmpty());
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            StringVerifier* w = dynamic_cast<StringVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "String");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[8];
        CHECK(e.key == "TupleVector2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "tuple vector 2 documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            StringVerifier* w = dynamic_cast<StringVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "String");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            TableVerifier* t = dynamic_cast<TableVerifier*>(f.verifier.get());
            REQUIRE(t);
            REQUIRE(t->documentations.size() == 1);
            CHECK(t->documentations[0].key == "*");
            CHECK(t->documentations[0].verifier->type() == "String");
            StringVerifier* w = dynamic_cast<StringVerifier*>(
                t->documentations[0].verifier.get()
                );
            REQUIRE(w);
            CHECK(!w->mustBeNotEmpty());
        }
    }
    {
        const DocumentationEntry& e = doc.entries[9];
        CHECK(e.key == "MultipleTupleA");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "multiple tuple a documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            IntVerifier* w = dynamic_cast<IntVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Integer");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            StringVerifier* w = dynamic_cast<StringVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "String");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[10];
        CHECK(e.key == "MultipleTupleB");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "multiple tuple b documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            StringVerifier* w = dynamic_cast<StringVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "String");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            IntVerifier* w = dynamic_cast<IntVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Integer");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[11];
        CHECK(e.key == "MultipleTupleA2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "multiple tuple a2 documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            IntVerifier* w = dynamic_cast<IntVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Integer");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            StringVerifier* w = dynamic_cast<StringVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "String");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[12];
        CHECK(e.key == "MultipleTupleB2");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "multiple tuple b2 documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            StringVerifier* w = dynamic_cast<StringVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "String");
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            IntVerifier* w = dynamic_cast<IntVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Integer");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[13];
        CHECK(e.key == "TupleEnumFloat");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "tuple enum float documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            using namespace std::string_literals;

            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            StringInListVerifier* w = dynamic_cast<StringInListVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "String");
            CHECK(w->values == std::vector{ "Value1"s, "Value2"s, "Value3"s });
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVerifier* w = dynamic_cast<DoubleVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Double");
        }
    }
    {
        const DocumentationEntry& e = doc.entries[14];
        CHECK(e.key == "OptionalTupleEnumFloat");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optional tuple enum float documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        {
            using namespace std::string_literals;

            const DocumentationEntry& f = v->documentations[0];
            CHECK(f.key == "1");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            StringInListVerifier* w = dynamic_cast<StringInListVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "String");
            CHECK(w->values == std::vector{ "Value1"s, "Value2"s, "Value3"s });
        }
        {
            const DocumentationEntry& f = v->documentations[1];
            CHECK(f.key == "2");
            CHECK(!f.optional);
            CHECK(!f.isPrivate);
            CHECK(f.documentation.empty());
            DoubleVerifier* w = dynamic_cast<DoubleVerifier*>(f.verifier.get());
            REQUIRE(w);
            CHECK(w->type() == "Double");
        }
    }
}
