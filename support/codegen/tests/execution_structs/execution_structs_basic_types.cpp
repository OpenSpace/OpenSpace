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

namespace {
    struct [[codegen::Dictionary(BasicTypes)]] Parameters {
        // bool value documentation
        bool boolValue;

        // int value documentation
        int intValue;

        // int value 2 documentation
        int intValue2;

        // double value documentation
        double doubleValue;

        // float value documentation
        float floatValue;

        // string value documentation
        std::string stringValue;

        // string not empty value documentation
        std::string stringNotEmptyValue [[codegen::notempty()]];

        // path value documentation
        std::filesystem::path pathValue;

        // directory value documentation
        std::filesystem::path directoryValue [[codegen::directory()]];

        // ivec2 value documentation
        glm::ivec2 ivec2Value;

        // ivec3 value documentation
        glm::ivec3 ivec3Value;

        // ivec4 value documentation
        glm::ivec4 ivec4Value;

        // dvec2 value documentation
        glm::dvec2 dvec2Value;

        // dvec3 value documentation
        glm::dvec3 dvec3Value;

        // dvec4 value documentation
        glm::dvec4 dvec4Value;

        // vec2 value documentation
        glm::vec2 vec2Value;

        // vec3 value documentation
        glm::vec3 vec3Value;

        // vec4 value documentation
        glm::vec4 vec4Value;

        // mat2x2 value documentation
        glm::mat2x2 mat2x2Value;

        // mat2 value documentation
        glm::mat2 mat2Value;

        // mat2x3 value documentation
        glm::mat2x3 mat2x3Value;

        // mat2x4 value documentation
        glm::mat2x4 mat2x4Value;

        // mat3x2 value documentation
        glm::mat3x2 mat3x2Value;

        // mat3x3 value documentation
        glm::mat3x3 mat3x3Value;

        // mat3 value documentation
        glm::mat3 mat3Value;

        // mat3x4 value documentation
        glm::mat3x4 mat3x4Value;

        // mat4x2 value documentation
        glm::mat4x2 mat4x2Value;

        // mat4x3 value documentation
        glm::mat4x3 mat4x3Value;

        // mat4x4 value documentation
        glm::mat4x4 mat4x4Value;

        // mat4 value documentation
        glm::mat4 mat4Value;

        // dmat2x2 value documentation
        glm::dmat2x2 dmat2x2Value;

        // dmat2 value documentation
        glm::dmat2 dmat2Value;

        // dmat2x3 value documentation
        glm::dmat2x3 dmat2x3Value;

        // dmat2x4 value documentation
        glm::dmat2x4 dmat2x4Value;

        // dmat3x2 value documentation
        glm::dmat3x2 dmat3x2Value;

        // dmat3x3 value documentation
        glm::dmat3x3 dmat3x3Value;

        // dmat3 value documentation
        glm::dmat3 dmat3Value;

        // dmat3x4 value documentation
        glm::dmat3x4 dmat3x4Value;

        // dmat4x2 value documentation
        glm::dmat4x2 dmat4x2Value;

        // dmat4x3 value documentation
        glm::dmat4x3 dmat4x3Value;

        // dmat4x4 value documentation
        glm::dmat4x4 dmat4x4Value;

        // dmat4 value documentation
        glm::dmat4 dmat4Value;

        // dict value documentation
        ghoul::Dictionary dictValue;
    };
} // namespace
#include "execution_structs_basic_types_codegen.cpp"


TEST_CASE("Execution/Structs/Basic/Types:  Bake", "[Execution][Structs]") {
    using namespace std::string_literals;

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    std::filesystem::path tmpFile = (path / "codegen_execution_basic_types.txt");
    {
        std::ofstream f(tmpFile);
        f << "unit test";
    }

    std::filesystem::path tmpFolder = (path / "codegen_execution_basic_types");
    std::filesystem::create_directories(tmpFolder);


    ghoul::Dictionary d;
    d.setValue("BoolValue", true);
    d.setValue("IntValue", 1.0);
    d.setValue("IntValue2", 2);
    d.setValue("DoubleValue", 3.0);
    d.setValue("FloatValue", 4.0);
    d.setValue("StringValue", "abc"s);
    d.setValue("StringNotEmptyValue", "def"s);
    d.setValue("PathValue", tmpFile);
    d.setValue("DirectoryValue", tmpFolder);
    d.setValue("Ivec2Value", glm::dvec2(5.0, 6.0));
    d.setValue("Ivec3Value", glm::dvec3(7.0, 8.0, 9.0));
    d.setValue("Ivec4Value", glm::dvec4(10.0, 11.0, 12.0, 13.0));
    d.setValue("Dvec2Value", glm::dvec2(14.1, 14.2));
    d.setValue("Dvec3Value", glm::dvec3(15.1, 15.2, 15.3));
    d.setValue("Dvec4Value", glm::dvec4(16.1, 16.2, 16.3, 16.4));
    d.setValue("Vec2Value", glm::dvec2(17.1, 17.2));
    d.setValue("Vec3Value", glm::dvec3(18.1, 18.2, 18.3));
    d.setValue("Vec4Value", glm::dvec4(19.1, 19.2, 19.3, 19.4));
    d.setValue("Mat2x2Value", glm::dmat2x2(20.1, 20.2, 20.3, 20.4));
    d.setValue("Mat2Value", glm::dmat2x2(120.1, 120.2, 120.3, 120.4));
    d.setValue("Mat2x3Value", glm::dmat2x3(21.1, 21.2, 21.3, 21.4, 21.5, 21.6));
    d.setValue(
        "Mat2x4Value",
        glm::dmat2x4(22.1, 22.2, 22.3, 22.4, 22.5, 22.6, 22.7, 22.8)
    );
    d.setValue("Mat3x2Value", glm::dmat3x2(23.1, 23.2, 23.3, 23.4, 23.5, 23.6));
    d.setValue(
        "Mat3x3Value",
        glm::dmat3x3(24.1, 24.2, 24.3, 24.4, 24.5, 24.6, 24.7, 24.8, 24.9)
    );
    d.setValue(
        "Mat3Value",
        glm::dmat3x3(124.1, 124.2, 124.3, 124.4, 124.5, 124.6, 124.7, 124.8, 124.9)
    );
    d.setValue(
        "Mat3x4Value",
        glm::dmat3x4(
            25.1, 25.2, 25.3, 25.4, 25.5, 25.6, 25.7, 25.8, 25.9, 25.10, 25.11, 25.12
        )
    );
    d.setValue(
        "Mat4x2Value",
        glm::dmat4x2(26.1, 26.2, 26.3, 26.4, 26.5, 26.6, 26.7, 26.8)
    );
    d.setValue(
        "Mat4x3Value",
        glm::dmat4x3(
            27.1, 27.2, 27.3, 27.4, 27.5, 27.6, 27.7, 27.8, 27.9, 27.10, 27.11, 27.12
        )
    );
    d.setValue(
        "Mat4x4Value",
        glm::dmat4x4(
            28.1, 28.2, 28.3, 28.4, 28.5, 28.6, 28.7, 28.8,
            28.9, 28.10, 28.11, 28.12, 28.13, 28.14, 28.15, 28.16
        )
    );
    d.setValue(
        "Mat4Value",
        glm::dmat4(
            128.1, 128.2, 128.3, 128.4, 128.5, 128.6, 128.7, 128.8,
            128.9, 128.10, 128.11, 128.12, 128.13, 128.14, 128.15, 128.16
        )
    );
    d.setValue("Dmat2x2Value", glm::dmat2x2(29.1, 29.2, 29.3, 29.4));
    d.setValue("Dmat2Value", glm::dmat2(129.1, 129.2, 129.3, 129.4));
    d.setValue("Dmat2x3Value", glm::dmat2x3(30.1, 30.2, 30.3, 30.4, 30.5, 30.6));
    d.setValue(
        "Dmat2x4Value",
        glm::dmat2x4(31.1, 31.2, 31.3, 31.4, 31.5, 31.6, 31.7, 31.8)
    );
    d.setValue("Dmat3x2Value", glm::dmat3x2(32.1, 32.2, 32.3, 32.4, 32.5, 32.6));
    d.setValue(
        "Dmat3x3Value",
        glm::dmat3x3(33.1, 33.2, 33.3, 33.4, 33.5, 33.6, 33.7, 33.8, 33.9)
    );
    d.setValue(
        "Dmat3Value",
        glm::dmat3(133.1, 133.2, 133.3, 133.4, 133.5, 133.6, 133.7, 133.8, 133.9)
    );
    d.setValue(
        "Dmat3x4Value",
        glm::dmat3x4(
            34.1, 34.2, 34.3, 34.4, 34.5, 34.6, 34.7, 34.8, 34.9, 34.10, 34.11, 34.12
        )
    );
    d.setValue(
        "Dmat4x2Value",
        glm::dmat4x2(35.1, 35.2, 35.3, 35.4, 35.5, 35.6, 35.7, 35.8)
    );
    d.setValue(
        "Dmat4x3Value",
        glm::dmat4x3(
            36.1, 36.2, 36.3, 36.4, 36.5, 36.6, 36.7, 36.8, 36.9, 36.10, 36.11, 36.12
        )
    );
    d.setValue(
        "Dmat4x4Value",
        glm::dmat4x4(
            37.1, 37.2, 37.3, 37.4, 37.5, 37.6, 37.7, 37.8,
            37.9, 37.10, 37.11, 37.12, 37.13, 37.14, 37.15, 37.16
        )
    );
    d.setValue(
        "Dmat4Value",
        glm::dmat4(
            137.1, 137.2, 137.3, 137.4, 137.5, 137.6, 137.7, 137.8,
            137.9, 137.10, 137.11, 137.12, 137.13, 137.14, 137.15, 137.16
        )
    );
    {
        ghoul::Dictionary e;
        e.setValue("a", 1);
        e.setValue("b", 2.0);
        d.setValue("DictValue", e);
    }

    const Parameters p = codegen::bake<Parameters>(d);
    CHECK(p.boolValue == true);
    CHECK(p.intValue == 1);
    CHECK(p.intValue2 == 2);
    CHECK(p.doubleValue == 3.0);
    CHECK(p.floatValue == 4.f);
    CHECK(p.stringValue == "abc");
    CHECK(p.stringNotEmptyValue == "def");
    CHECK(p.pathValue == tmpFile);
    CHECK(p.directoryValue == tmpFolder);
    CHECK(p.ivec2Value == glm::ivec2(5, 6));
    CHECK(p.ivec3Value == glm::ivec3(7, 8, 9));
    CHECK(p.ivec4Value == glm::ivec4(10, 11, 12, 13));
    CHECK(p.dvec2Value == glm::dvec2(14.1, 14.2));
    CHECK(p.dvec3Value == glm::dvec3(15.1, 15.2, 15.3));
    CHECK(p.dvec4Value == glm::dvec4(16.1, 16.2, 16.3, 16.4));
    CHECK(p.vec2Value == glm::vec2(17.1f, 17.2f));
    CHECK(p.vec3Value == glm::vec3(18.1f, 18.2f, 18.3f));
    CHECK(p.vec4Value == glm::vec4(19.1f, 19.2f, 19.3, 19.4f));
    CHECK(p.mat2x2Value == glm::mat2x2(20.1f, 20.2f, 20.3f, 20.4f));
    CHECK(p.mat2Value == glm::mat2(120.1f, 120.2f, 120.3f, 120.4f));
    CHECK(p.mat2x3Value == glm::mat2x3(21.1f, 21.2f, 21.3f, 21.4f, 21.5f, 21.6f));
    CHECK(
        p.mat2x4Value ==
        glm::mat2x4(22.1f, 22.2f, 22.3f, 22.4f, 22.5f, 22.6f, 22.7f, 22.8f)
    );
    CHECK(p.mat3x2Value == glm::mat3x2(23.1f, 23.2f, 23.3f, 23.4f, 23.5f, 23.6f));
    CHECK(
        p.mat3x3Value ==
        glm::mat3x3(24.1f, 24.2f, 24.3f, 24.4f, 24.5f, 24.6f, 24.7f, 24.8f, 24.9f)
    );
    CHECK(
        p.mat3Value ==
        glm::mat3(124.1f, 124.2f, 124.3f, 124.4f, 124.5f, 124.6f, 124.7f, 124.8f, 124.9f)
    );
    CHECK(
        p.mat3x4Value ==
        glm::mat3x4(
            25.1f, 25.2f, 25.3f, 25.4f, 25.5f, 25.6f,
            25.7f, 25.8f, 25.9f, 25.10f, 25.11f, 25.12f
        )
    );
    CHECK(
        p.mat4x2Value ==
        glm::mat4x2(26.1f, 26.2f, 26.3f, 26.4f, 26.5f, 26.6f, 26.7f, 26.8f)
    );
    CHECK(
        p.mat4x3Value ==
        glm::mat4x3(
            27.1f, 27.2f, 27.3f, 27.4f, 27.5f, 27.6f,
            27.7f, 27.8f, 27.9f, 27.10f, 27.11f, 27.12f
        )
    );
    CHECK(
        p.mat4x4Value ==
        glm::mat4x4(
            28.1f, 28.2f, 28.3f, 28.4f, 28.5f, 28.6f, 28.7f, 28.8f,
            28.9f, 28.10f, 28.11f, 28.12f, 28.13f, 28.14f, 28.15f, 28.16f
        )
    );
    CHECK(
        p.mat4Value ==
        glm::mat4(
            128.1f, 128.2f, 128.3f, 128.4f, 128.5f, 128.6f, 128.7f, 128.8f,
            128.9f, 128.10f, 128.11f, 128.12f, 128.13f, 128.14f, 128.15f, 128.16f
        )
    );
    CHECK(p.dmat2x2Value == glm::dmat2x2(29.1, 29.2, 29.3, 29.4));
    CHECK(p.dmat2Value == glm::dmat2(129.1, 129.2, 129.3, 129.4));
    CHECK(p.dmat2x3Value == glm::dmat2x3(30.1, 30.2, 30.3, 30.4, 30.5, 30.6));
    CHECK(
        p.dmat2x4Value ==
        glm::dmat2x4(31.1, 31.2, 31.3, 31.4, 31.5, 31.6, 31.7, 31.8)
    );
    CHECK(p.dmat3x2Value == glm::dmat3x2(32.1, 32.2, 32.3, 32.4, 32.5, 32.6));
    CHECK(
        p.dmat3x3Value ==
        glm::dmat3x3(33.1, 33.2, 33.3, 33.4, 33.5, 33.6, 33.7, 33.8, 33.9)
    );
    CHECK(
        p.dmat3Value ==
        glm::dmat3(133.1, 133.2, 133.3, 133.4, 133.5, 133.6, 133.7, 133.8, 133.9)
    );
    CHECK(
        p.dmat3x4Value ==
        glm::dmat3x4(
            34.1, 34.2, 34.3, 34.4, 34.5, 34.6, 34.7, 34.8, 34.9, 34.10, 34.11, 34.12
        )
    );
    CHECK(
        p.dmat4x2Value ==
        glm::dmat4x2(35.1, 35.2, 35.3, 35.4, 35.5, 35.6, 35.7, 35.8)
    );
    CHECK(
        p.dmat4x3Value ==
        glm::dmat4x3(
            36.1, 36.2, 36.3, 36.4, 36.5, 36.6, 36.7, 36.8, 36.9, 36.10, 36.11, 36.12
        )
    );
    CHECK(
        p.dmat4x4Value ==
        glm::dmat4x4(
            37.1, 37.2, 37.3, 37.4, 37.5, 37.6, 37.7, 37.8,
            37.9, 37.10, 37.11, 37.12, 37.13, 37.14, 37.15, 37.16
        )
    );
    CHECK(
        p.dmat4Value ==
        glm::dmat4(
            137.1, 137.2, 137.3, 137.4, 137.5, 137.6, 137.7, 137.8,
            137.9, 137.10, 137.11, 137.12, 137.13, 137.14, 137.15, 137.16
        )
    );
    REQUIRE(p.dictValue.hasKey("a"));
    REQUIRE(p.dictValue.hasValue<int>("a"));
    CHECK(p.dictValue.value<int>("a") == 1);
    REQUIRE(p.dictValue.hasKey("b"));
    REQUIRE(p.dictValue.hasValue<double>("b"));
    CHECK(p.dictValue.value<double>("b") == 2.0);
}

TEST_CASE("Execution/Structs/Basic/Types:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 43);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "BoolValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "bool value documentation");
        CHECK(e.verifier->type() == "Boolean");
        CHECK(dynamic_cast<BoolVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "IntValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "int value documentation");
        CHECK(e.verifier->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[3];
        CHECK(e.key == "DoubleValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "double value documentation");
        CHECK(e.verifier->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[4];
        CHECK(e.key == "FloatValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "float value documentation");
        CHECK(e.verifier->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[5];
        CHECK(e.key == "StringValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "string value documentation");
        CHECK(e.verifier->type() == "String");
        StringVerifier* v = dynamic_cast<StringVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[6];
        CHECK(e.key == "StringNotEmptyValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "string not empty value documentation");
        CHECK(e.verifier->type() == "String");
        StringVerifier* v = dynamic_cast<StringVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->mustBeNotEmpty() == true);
    }
    {
        const DocumentationEntry& e = doc.entries[7];
        CHECK(e.key == "PathValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "path value documentation");
        CHECK(e.verifier->type() == "File");
        CHECK(dynamic_cast<FileVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[8];
        CHECK(e.key == "DirectoryValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "directory value documentation");
        CHECK(e.verifier->type() == "Directory");
        CHECK(dynamic_cast<DirectoryVerifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[9];
        CHECK(e.key == "Ivec2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "ivec2 value documentation");
        CHECK(e.verifier->type() == "Vector2<int>");
        CHECK(dynamic_cast<IntVector2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[10];
        CHECK(e.key == "Ivec3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "ivec3 value documentation");
        CHECK(e.verifier->type() == "Vector3<int>");
        CHECK(dynamic_cast<IntVector3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[11];
        CHECK(e.key == "Ivec4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "ivec4 value documentation");
        CHECK(e.verifier->type() == "Vector4<int>");
        CHECK(dynamic_cast<IntVector4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[12];
        CHECK(e.key == "Dvec2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dvec2 value documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        CHECK(dynamic_cast<DoubleVector2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[13];
        CHECK(e.key == "Dvec3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dvec3 value documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        CHECK(dynamic_cast<DoubleVector3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[14];
        CHECK(e.key == "Dvec4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dvec4 value documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        CHECK(dynamic_cast<DoubleVector4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[15];
        CHECK(e.key == "Vec2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vec2 value documentation");
        CHECK(e.verifier->type() == "Vector2<double>");
        CHECK(dynamic_cast<DoubleVector2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[16];
        CHECK(e.key == "Vec3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vec3 value documentation");
        CHECK(e.verifier->type() == "Vector3<double>");
        CHECK(dynamic_cast<DoubleVector3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[17];
        CHECK(e.key == "Vec4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vec4 value documentation");
        CHECK(e.verifier->type() == "Vector4<double>");
        CHECK(dynamic_cast<DoubleVector4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[18];
        CHECK(e.key == "Mat2x2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat2x2 value documentation");
        CHECK(e.verifier->type() == "Matrix2x2<double>");
        CHECK(dynamic_cast<DoubleMatrix2x2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[19];
        CHECK(e.key == "Mat2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat2 value documentation");
        CHECK(e.verifier->type() == "Matrix2x2<double>");
        CHECK(dynamic_cast<DoubleMatrix2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[20];
        CHECK(e.key == "Mat2x3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat2x3 value documentation");
        CHECK(e.verifier->type() == "Matrix2x3<double>");
        CHECK(dynamic_cast<DoubleMatrix2x3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[21];
        CHECK(e.key == "Mat2x4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat2x4 value documentation");
        CHECK(e.verifier->type() == "Matrix2x4<double>");
        CHECK(dynamic_cast<DoubleMatrix2x4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[22];
        CHECK(e.key == "Mat3x2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat3x2 value documentation");
        CHECK(e.verifier->type() == "Matrix3x2<double>");
        CHECK(dynamic_cast<DoubleMatrix3x2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[23];
        CHECK(e.key == "Mat3x3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat3x3 value documentation");
        CHECK(e.verifier->type() == "Matrix3x3<double>");
        CHECK(dynamic_cast<DoubleMatrix3x3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[24];
        CHECK(e.key == "Mat3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat3 value documentation");
        CHECK(e.verifier->type() == "Matrix3x3<double>");
        CHECK(dynamic_cast<DoubleMatrix3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[25];
        CHECK(e.key == "Mat3x4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat3x4 value documentation");
        CHECK(e.verifier->type() == "Matrix3x4<double>");
        CHECK(dynamic_cast<DoubleMatrix3x4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[26];
        CHECK(e.key == "Mat4x2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat4x2 value documentation");
        CHECK(e.verifier->type() == "Matrix4x2<double>");
        CHECK(dynamic_cast<DoubleMatrix4x2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[27];
        CHECK(e.key == "Mat4x3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat4x3 value documentation");
        CHECK(e.verifier->type() == "Matrix4x3<double>");
        CHECK(dynamic_cast<DoubleMatrix4x3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[28];
        CHECK(e.key == "Mat4x4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat4x4 value documentation");
        CHECK(e.verifier->type() == "Matrix4x4<double>");
        CHECK(dynamic_cast<DoubleMatrix4x4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[29];
        CHECK(e.key == "Mat4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat4 value documentation");
        CHECK(e.verifier->type() == "Matrix4x4<double>");
        CHECK(dynamic_cast<DoubleMatrix4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[30];
        CHECK(e.key == "Dmat2x2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat2x2 value documentation");
        CHECK(e.verifier->type() == "Matrix2x2<double>");
        CHECK(dynamic_cast<DoubleMatrix2x2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[31];
        CHECK(e.key == "Dmat2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat2 value documentation");
        CHECK(e.verifier->type() == "Matrix2x2<double>");
        CHECK(dynamic_cast<DoubleMatrix2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[32];
        CHECK(e.key == "Dmat2x3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat2x3 value documentation");
        CHECK(e.verifier->type() == "Matrix2x3<double>");
        CHECK(dynamic_cast<DoubleMatrix2x3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[33];
        CHECK(e.key == "Dmat2x4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat2x4 value documentation");
        CHECK(e.verifier->type() == "Matrix2x4<double>");
        CHECK(dynamic_cast<DoubleMatrix2x4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[34];
        CHECK(e.key == "Dmat3x2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat3x2 value documentation");
        CHECK(e.verifier->type() == "Matrix3x2<double>");
        CHECK(dynamic_cast<DoubleMatrix3x2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[35];
        CHECK(e.key == "Dmat3x3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat3x3 value documentation");
        CHECK(e.verifier->type() == "Matrix3x3<double>");
        CHECK(dynamic_cast<DoubleMatrix3x3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[36];
        CHECK(e.key == "Dmat3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat3 value documentation");
        CHECK(e.verifier->type() == "Matrix3x3<double>");
        CHECK(dynamic_cast<DoubleMatrix3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[37];
        CHECK(e.key == "Dmat3x4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat3x4 value documentation");
        CHECK(e.verifier->type() == "Matrix3x4<double>");
        CHECK(dynamic_cast<DoubleMatrix3x4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[38];
        CHECK(e.key == "Dmat4x2Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat4x2 value documentation");
        CHECK(e.verifier->type() == "Matrix4x2<double>");
        CHECK(dynamic_cast<DoubleMatrix4x2Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[39];
        CHECK(e.key == "Dmat4x3Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat4x3 value documentation");
        CHECK(e.verifier->type() == "Matrix4x3<double>");
        CHECK(dynamic_cast<DoubleMatrix4x3Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[40];
        CHECK(e.key == "Dmat4x4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat4x4 value documentation");
        CHECK(e.verifier->type() == "Matrix4x4<double>");
        CHECK(dynamic_cast<DoubleMatrix4x4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[41];
        CHECK(e.key == "Dmat4Value");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat4 value documentation");
        CHECK(e.verifier->type() == "Matrix4x4<double>");
        CHECK(dynamic_cast<DoubleMatrix4Verifier*>(e.verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[42];
        CHECK(e.key == "DictValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dict value documentation");
        CHECK(e.verifier->type() == "Table");
        CHECK(dynamic_cast<TableVerifier*>(e.verifier.get()));
    }
}
