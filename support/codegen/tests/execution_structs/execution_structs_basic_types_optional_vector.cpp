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
#include <vector>

namespace {
    struct [[codegen::Dictionary(BasicTypesOptionalVector)]] Parameters {
        // bool value documentation
        std::optional<std::vector<bool>> boolValue;

        // int value documentation
        std::optional<std::vector<int>> intValue;

        // double value documentation
        std::optional<std::vector<double>> doubleValue;

        // float value documentation
        std::optional<std::vector<float>> floatValue;

        // string value documentation
        std::optional<std::vector<std::string>> stringValue;

        // string not empty value documentation
        std::optional<std::vector<std::string>> stringNotEmptyValue
            [[codegen::notempty()]];

        // path value documentation
        std::optional<std::vector<std::filesystem::path>> pathValue;

        // directory value documentation
        std::optional<std::vector<std::filesystem::path>>
            directoryValue [[codegen::directory()]];

        // ivec2 value documentation
        std::optional<std::vector<glm::ivec2>> ivec2Value;

        // ivec3 value documentation
        std::optional<std::vector<glm::ivec3>> ivec3Value;

        // ivec4 value documentation
        std::optional<std::vector<glm::ivec4>> ivec4Value;

        // dvec2 value documentation
        std::optional<std::vector<glm::dvec2>> dvec2Value;

        // dvec3 value documentation
        std::optional<std::vector<glm::dvec3>> dvec3Value;

        // dvec4 value documentation
        std::optional<std::vector<glm::dvec4>> dvec4Value;

        // vec2 value documentation
        std::optional<std::vector<glm::vec2>> vec2Value;

        // vec3 value documentation
        std::optional<std::vector<glm::vec3>> vec3Value;

        // vec4 value documentation
        std::optional<std::vector<glm::vec4>> vec4Value;

        // mat2x2 value documentation
        std::optional<std::vector<glm::mat2x2>> mat2x2Value;

        // mat2 value documentation
        std::optional<std::vector<glm::mat2>> mat2Value;

        // mat2x3 value documentation
        std::optional<std::vector<glm::mat2x3>> mat2x3Value;

        // mat2x4 value documentation
        std::optional<std::vector<glm::mat2x4>> mat2x4Value;

        // mat3x2 value documentation
        std::optional<std::vector<glm::mat3x2>> mat3x2Value;

        // mat3x3 value documentation
        std::optional<std::vector<glm::mat3x3>> mat3x3Value;

        // mat3 value documentation
        std::optional<std::vector<glm::mat3>> mat3Value;

        // mat3x4 value documentation
        std::optional<std::vector<glm::mat3x4>> mat3x4Value;

        // mat4x2 value documentation
        std::optional<std::vector<glm::mat4x2>> mat4x2Value;

        // mat4x3 value documentation
        std::optional<std::vector<glm::mat4x3>> mat4x3Value;

        // mat4x4 value documentation
        std::optional<std::vector<glm::mat4x4>> mat4x4Value;

        // mat4 value documentation
        std::optional<std::vector<glm::mat4>> mat4Value;

        // dmat2x2 value documentation
        std::optional<std::vector<glm::dmat2x2>> dmat2x2Value;

        // dmat2 value documentation
        std::optional<std::vector<glm::dmat2>> dmat2Value;

        // dmat2x3 value documentation
        std::optional<std::vector<glm::dmat2x3>> dmat2x3Value;

        // dmat2x4 value documentation
        std::optional<std::vector<glm::dmat2x4>> dmat2x4Value;

        // dmat3x2 value documentation
        std::optional<std::vector<glm::dmat3x2>> dmat3x2Value;

        // dmat3x3 value documentation
        std::optional<std::vector<glm::dmat3x3>> dmat3x3Value;

        // dmat3 value documentation
        std::optional<std::vector<glm::dmat3>> dmat3Value;

        // dmat3x4 value documentation
        std::optional<std::vector<glm::dmat3x4>> dmat3x4Value;

        // dmat4x2 value documentation
        std::optional<std::vector<glm::dmat4x2>> dmat4x2Value;

        // dmat4x3 value documentation
        std::optional<std::vector<glm::dmat4x3>> dmat4x3Value;

        // dmat4x4 value documentation
        std::optional<std::vector<glm::dmat4x4>> dmat4x4Value;

        // dmat4 value documentation
        std::optional<std::vector<glm::dmat4>> dmat4Value;

        // dict value documentation
        std::optional<std::vector<ghoul::Dictionary>> dictValue;
    };
} // namespace
#include "execution_structs_basic_types_optional_vector_codegen.cpp"


TEST_CASE("Execution/Structs/Basic/Types/Optional/Vector:  Bake", "[Execution][Structs]")
{
    using namespace std::string_literals;

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path tmpFile1 =
        (path / "codegen_execution_basic_types_optional_vector_1.txt");
    {
        std::ofstream f(tmpFile1);
        f << "unit test";
    }
    const std::filesystem::path tmpFile2 =
        (path / "codegen_execution_basic_types_optional_vector_2.txt");
    {
        std::ofstream f(tmpFile2);
        f << "unit test";
    }
    const std::filesystem::path tmpFile3 =
        (path / "codegen_execution_basic_types_optional_vector_3.txt");
    {
        std::ofstream f(tmpFile3);
        f << "unit test";
    }

    const std::filesystem::path tmpFolder1 =
        (path / "codegen_execution_basic_types_optional_vector_1");
    std::filesystem::create_directories(tmpFolder1);

    const std::filesystem::path tmpFolder2 =
        (path / "codegen_execution_basic_types_optional_vector_2");
    std::filesystem::create_directories(tmpFolder2);

    const std::filesystem::path tmpFolder3 =
        (path / "codegen_execution_basic_types_optional_vector_3");
    std::filesystem::create_directories(tmpFolder3);

    ghoul::Dictionary d;
    {
        ghoul::Dictionary v;
        v.setValue("1", true);
        v.setValue("2", false);
        v.setValue("3", true);
        d.setValue("BoolValue", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", 2.0);
        v.setValue("2", 3.0);
        v.setValue("3", 4.0);
        d.setValue("IntValue", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", 5.1);
        v.setValue("2", 5.2);
        v.setValue("3", 5.3);
        d.setValue("DoubleValue", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", 6.1);
        v.setValue("2", 6.2);
        v.setValue("3", 6.3);
        d.setValue("FloatValue", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", "abc"s);
        v.setValue("2", "def"s);
        v.setValue("3", "ghi"s);
        d.setValue("StringValue", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", "jkl"s);
        v.setValue("2", "mno"s);
        v.setValue("3", "pqr"s);
        d.setValue("StringNotEmptyValue", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", tmpFile1);
        v.setValue("2", tmpFile2);
        v.setValue("3", tmpFile3);
        d.setValue("PathValue", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", tmpFolder1);
        v.setValue("2", tmpFolder2);
        v.setValue("3", tmpFolder3);
        d.setValue("DirectoryValue", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dvec2(7.0, 8.0));
        v.setValue("2", glm::dvec2(9.0, 10.0));
        v.setValue("3", glm::dvec2(11.0, 12.0));
        d.setValue("Ivec2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dvec3(13.0, 14.0, 15.0));
        v.setValue("2", glm::dvec3(16.0, 17.0, 18.0));
        v.setValue("3", glm::dvec3(19.0, 20.0, 21.0));
        d.setValue("Ivec3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dvec4(22.0, 23.0, 24.0, 25.0));
        v.setValue("2", glm::dvec4(26.0, 27.0, 28.0, 29.0));
        v.setValue("3", glm::dvec4(30.0, 31.0, 32.0, 33.0));
        d.setValue("Ivec4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dvec2(34.1, 34.2));
        v.setValue("2", glm::dvec2(35.1, 35.2));
        v.setValue("3", glm::dvec2(36.1, 36.2));
        d.setValue("Dvec2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dvec3(37.1, 37.2, 37.3));
        v.setValue("2", glm::dvec3(38.1, 38.2, 38.3));
        v.setValue("3", glm::dvec3(39.1, 39.2, 39.3));
        d.setValue("Dvec3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dvec4(40.1, 40.2, 40.3, 40.4));
        v.setValue("2", glm::dvec4(41.1, 41.2, 41.3, 41.4));
        v.setValue("3", glm::dvec4(42.1, 42.2, 42.3, 42.4));
        d.setValue("Dvec4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dvec2(43.1, 43.2));
        v.setValue("2", glm::dvec2(44.1, 44.2));
        v.setValue("3", glm::dvec2(45.1, 45.2));
        d.setValue("Vec2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dvec3(46.1, 46.2, 46.3));
        v.setValue("2", glm::dvec3(47.1, 47.2, 47.3));
        v.setValue("3", glm::dvec3(48.1, 48.2, 48.3));
        d.setValue("Vec3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dvec4(49.1, 49.2, 49.3, 49.4));
        v.setValue("2", glm::dvec4(50.1, 50.2, 50.3, 50.4));
        v.setValue("3", glm::dvec4(51.1, 51.2, 51.3, 51.4));
        d.setValue("Vec4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat2x2(52.1, 52.2, 52.3, 52.4));
        v.setValue("2", glm::dmat2x2(53.1, 53.2, 53.3, 53.4));
        v.setValue("3", glm::dmat2x2(54.1, 54.2, 54.3, 54.4));
        d.setValue("Mat2x2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat2x2(152.1, 152.2, 152.3, 152.4));
        v.setValue("2", glm::dmat2x2(153.1, 153.2, 153.3, 153.4));
        v.setValue("3", glm::dmat2x2(154.1, 154.2, 154.3, 154.4));
        d.setValue("Mat2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat2x3(55.1, 55.2, 55.3, 55.4, 55.5, 55.6));
        v.setValue("2", glm::dmat2x3(56.1, 56.2, 56.3, 56.4, 56.5, 56.6));
        v.setValue("3", glm::dmat2x3(57.1, 57.2, 57.3, 57.4, 57.5, 57.6));
        d.setValue("Mat2x3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat2x4(58.1, 58.2, 58.3, 58.4, 58.5, 58.6, 58.7, 58.8));
        v.setValue("2", glm::dmat2x4(59.1, 59.2, 59.3, 59.4, 59.5, 59.6, 59.7, 59.8));
        v.setValue("3", glm::dmat2x4(60.1, 60.2, 60.3, 60.4, 60.5, 60.6, 60.7, 60.8));
        d.setValue("Mat2x4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat3x2(61.1, 61.2, 61.3, 61.4, 61.5, 61.6));
        v.setValue("2", glm::dmat3x2(62.1, 62.2, 62.3, 62.4, 62.5, 62.6));
        v.setValue("3", glm::dmat3x2(63.1, 63.2, 63.3, 63.4, 63.5, 63.6));
        d.setValue("Mat3x2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat3x3(64.1, 64.2, 64.3, 64.4, 64.5, 64.6, 64.7, 64.8, 64.9)
        );
        v.setValue(
            "2",
            glm::dmat3x3(65.1, 65.2, 65.3, 65.4, 65.5, 65.6, 65.7, 65.8, 65.9)
        );
        v.setValue(
            "3",
            glm::dmat3x3(66.1, 66.2, 66.3, 66.4, 66.5, 66.6, 66.7, 66.8, 66.9)
        );
        d.setValue("Mat3x3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat3x3(164.1, 164.2, 164.3, 164.4, 164.5, 164.6, 164.7, 164.8, 164.9)
        );
        v.setValue(
            "2",
            glm::dmat3x3(165.1, 165.2, 165.3, 165.4, 165.5, 165.6, 165.7, 165.8, 165.9)
        );
        v.setValue(
            "3",
            glm::dmat3x3(166.1, 166.2, 166.3, 166.4, 166.5, 166.6, 166.7, 166.8, 166.9)
        );
        d.setValue("Mat3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat3x4(
                67.1, 67.2, 67.3, 67.4, 67.5, 67.6, 67.7, 67.8, 67.9, 67.10, 67.11, 67.12
            )
        );
        v.setValue(
            "2",
            glm::dmat3x4(
                68.1, 68.2, 68.3, 68.4, 68.5, 68.6, 68.7, 68.8, 68.9, 68.10, 68.11, 68.12
            )
        );
        v.setValue(
            "3",
            glm::dmat3x4(
                69.1, 69.2, 69.3, 69.4, 69.5, 69.6, 69.7, 69.8, 69.9, 69.10, 69.11, 69.12
            )
        );
        d.setValue("Mat3x4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat4x2(70.1, 70.2, 70.3, 70.4, 70.5, 70.6, 70.7, 70.8));
        v.setValue("2", glm::dmat4x2(71.1, 71.2, 71.3, 71.4, 71.5, 71.6, 71.7, 71.8));
        v.setValue("3", glm::dmat4x2(72.1, 72.2, 72.3, 72.4, 72.5, 72.6, 72.7, 72.8));
        d.setValue("Mat4x2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat4x3(
                73.1, 73.2, 73.3, 73.4, 73.5, 73.6, 73.7, 73.8, 73.9, 73.10, 73.11, 73.12
            )
        );
        v.setValue(
            "2",
            glm::dmat4x3(
                74.1, 74.2, 74.3, 74.4, 74.5, 74.6, 74.7, 74.8, 74.9, 74.10, 74.11, 74.12
            )
        );
        v.setValue(
            "3",
            glm::dmat4x3(
                75.1, 75.2, 75.3, 75.4, 75.5, 75.6, 75.7, 75.8, 75.9, 75.10, 75.11, 75.12
            )
        );
        d.setValue("Mat4x3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat4x4(
                76.1, 76.2, 76.3, 76.4, 76.5, 76.6, 76.7, 76.8,
                76.9, 76.10, 76.11, 76.12, 76.13, 76.14, 76.15, 76.16
            )
        );
        v.setValue(
            "2",
            glm::dmat4x4(
                77.1, 77.2, 77.3, 77.4, 77.5, 77.6, 77.7, 77.8,
                77.9, 77.10, 77.11, 77.12, 77.13, 77.14, 77.15, 77.16
            )
        );
        v.setValue(
            "3",
            glm::dmat4x4(
                78.1, 78.2, 78.3, 78.4, 78.5, 78.6, 78.7, 78.8,
                78.9, 78.10, 78.11, 78.12, 78.13, 78.14, 78.15, 78.16
            )
        );
        d.setValue("Mat4x4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat4x4(
                176.1, 176.2, 176.3, 176.4, 176.5, 176.6, 176.7, 176.8,
                176.9, 176.10, 176.11, 176.12, 176.13, 176.14, 176.15, 176.16
            )
        );
        v.setValue(
            "2",
            glm::dmat4x4(
                177.1, 177.2, 177.3, 177.4, 177.5, 177.6, 177.7, 177.8,
                177.9, 177.10, 177.11, 177.12, 177.13, 177.14, 177.15, 177.16
            )
        );
        v.setValue(
            "3",
            glm::dmat4x4(
                178.1, 178.2, 178.3, 178.4, 178.5, 178.6, 178.7, 178.8,
                178.9, 178.10, 178.11, 178.12, 178.13, 178.14, 178.15, 178.16
            )
        );
        d.setValue("Mat4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat2x2(79.1, 79.2, 79.3, 79.4));
        v.setValue("2", glm::dmat2x2(80.1, 80.2, 80.3, 80.4));
        v.setValue("3", glm::dmat2x2(81.1, 81.2, 81.3, 81.4));
        d.setValue("Dmat2x2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat2x2(179.1, 179.2, 179.3, 179.4));
        v.setValue("2", glm::dmat2x2(180.1, 180.2, 180.3, 180.4));
        v.setValue("3", glm::dmat2x2(181.1, 181.2, 181.3, 181.4));
        d.setValue("Dmat2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat2x3(82.1, 82.2, 82.3, 82.4, 82.5, 82.6));
        v.setValue("2", glm::dmat2x3(83.1, 83.2, 83.3, 83.4, 83.5, 83.6));
        v.setValue("3", glm::dmat2x3(84.1, 84.2, 84.3, 84.4, 84.5, 84.6));
        d.setValue("Dmat2x3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat2x4(85.1, 85.2, 85.3, 85.4, 85.5, 85.6, 85.7, 85.8));
        v.setValue("2", glm::dmat2x4(86.1, 86.2, 86.3, 86.4, 86.5, 86.6, 86.7, 86.8));
        v.setValue("3", glm::dmat2x4(87.1, 87.2, 87.3, 87.4, 87.5, 87.6, 87.7, 87.8));
        d.setValue("Dmat2x4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat3x2(88.1, 88.2, 88.3, 88.4, 88.5, 88.6));
        v.setValue("2", glm::dmat3x2(89.1, 89.2, 89.3, 89.4, 89.5, 89.6));
        v.setValue("3", glm::dmat3x2(90.1, 90.2, 90.3, 90.4, 90.5, 90.6));
        d.setValue("Dmat3x2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat3x3(91.1, 91.2, 91.3, 91.4, 91.5, 91.6, 91.7, 91.8, 91.9)
        );
        v.setValue(
            "2",
            glm::dmat3x3(92.1, 92.2, 92.3, 92.4, 92.5, 92.6, 92.7, 92.8, 92.9)
        );
        v.setValue(
            "3",
            glm::dmat3x3(93.1, 93.2, 93.3, 93.4, 93.5, 93.6, 93.7, 93.8, 93.9)
        );
        d.setValue("Dmat3x3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat3x3(191.1, 191.2, 191.3, 191.4, 191.5, 191.6, 191.7, 191.8, 191.9)
        );
        v.setValue(
            "2",
            glm::dmat3x3(192.1, 192.2, 192.3, 192.4, 192.5, 192.6, 192.7, 192.8, 192.9)
        );
        v.setValue(
            "3",
            glm::dmat3x3(193.1, 193.2, 193.3, 193.4, 193.5, 193.6, 193.7, 193.8, 193.9)
        );
        d.setValue("Dmat3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat3x4(
                94.1, 94.2, 94.3, 94.4, 94.5, 94.6, 94.7, 94.8, 94.9, 94.10, 94.11, 94.12
            )
        );
        v.setValue(
            "2",
            glm::dmat3x4(
                95.1, 95.2, 95.3, 95.4, 95.5, 95.6, 95.7, 95.8, 95.9, 95.10, 95.11, 95.12
            )
        );
        v.setValue(
            "3",
            glm::dmat3x4(
                96.1, 96.2, 96.3, 96.4, 96.5, 96.6, 96.7, 96.8, 96.9, 96.10, 96.11, 96.12
            )
        );
        d.setValue("Dmat3x4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue("1", glm::dmat4x2(97.1, 97.2, 97.3, 97.4, 97.5, 97.6, 97.7, 97.8));
        v.setValue("2", glm::dmat4x2(98.1, 98.2, 98.3, 98.4, 98.5, 98.6, 98.7, 98.8));
        v.setValue("3", glm::dmat4x2(99.1, 99.2, 99.3, 99.4, 99.5, 99.6, 99.7, 99.8));
        d.setValue("Dmat4x2Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat4x3(
                100.1, 100.2, 100.3, 100.4, 100.5, 100.6,
                100.7, 100.8, 100.9, 100.10, 100.11, 100.12
            )
        );
        v.setValue(
            "2",
            glm::dmat4x3(
                101.1, 101.2, 101.3, 101.4, 101.5, 101.6,
                101.7, 101.8, 101.9, 101.10, 101.11, 101.12
            )
        );
        v.setValue(
            "3",
            glm::dmat4x3(
                102.1, 102.2, 102.3, 102.4, 102.5, 102.6,
                102.7, 102.8, 102.9, 102.10, 102.11, 102.12
            )
        );
        d.setValue("Dmat4x3Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat4x4(
                103.1, 103.2, 103.3, 103.4, 103.5, 103.6, 103.7, 103.8,
                103.9, 103.10, 103.11, 103.12, 103.13, 103.14, 103.15, 103.16
            )
        );
        v.setValue(
            "2",
            glm::dmat4x4(
                104.1, 104.2, 104.3, 104.4, 104.5, 104.6, 104.7, 104.8,
                104.9, 104.10, 104.11, 104.12, 104.13, 104.14, 104.15, 104.16
            )
        );
        v.setValue(
            "3",
            glm::dmat4x4(
                105.1, 105.2, 105.3, 105.4, 105.5, 105.6, 105.7, 105.8,
                105.9, 105.10, 105.11, 105.12, 105.13, 105.14, 105.15, 105.16
            )
        );
        d.setValue("Dmat4x4Value", v);
    }
    {
        ghoul::Dictionary v;
        v.setValue(
            "1",
            glm::dmat4x4(
                1103.1, 1103.2, 1103.3, 1103.4, 1103.5, 1103.6, 1103.7, 1103.8,
                1103.9, 1103.10, 1103.11, 1103.12, 1103.13, 1103.14, 1103.15, 1103.16
            )
        );
        v.setValue(
            "2",
            glm::dmat4x4(
                1104.1, 1104.2, 1104.3, 1104.4, 1104.5, 1104.6, 1104.7, 1104.8,
                1104.9, 1104.10, 1104.11, 1104.12, 1104.13, 1104.14, 1104.15, 1104.16
            )
        );
        v.setValue(
            "3",
            glm::dmat4x4(
                1105.1, 1105.2, 1105.3, 1105.4, 1105.5, 1105.6, 1105.7, 1105.8,
                1105.9, 1105.10, 1105.11, 1105.12, 1105.13, 1105.14, 1105.15, 1105.16
            )
        );
        d.setValue("Dmat4Value", v);
    }
    {
        ghoul::Dictionary v;
        {
            ghoul::Dictionary e;
            e.setValue("a", 1);
            e.setValue("b", 2.0);
            v.setValue("1", e);
        }
        {
            ghoul::Dictionary e;
            e.setValue("a", 3);
            e.setValue("b", 4.0);
            v.setValue("2", e);
        }
        {
            ghoul::Dictionary e;
            e.setValue("a", 5);
            e.setValue("b", 6.0);
            v.setValue("3", e);
        }
        d.setValue("DictValue", v);
    }

    const Parameters p = codegen::bake<Parameters>(d);
    REQUIRE(p.boolValue.has_value());
    CHECK(p.boolValue->size() == 3);
    CHECK(*p.boolValue == std::vector<bool>{ true, false, true });
    REQUIRE(p.intValue.has_value());
    CHECK(p.intValue->size() == 3);
    CHECK(*p.intValue == std::vector<int>{ 2, 3, 4 });
    REQUIRE(p.doubleValue.has_value());
    CHECK(p.doubleValue->size() == 3);
    CHECK(*p.doubleValue == std::vector<double>{ 5.1, 5.2, 5.3 });
    REQUIRE(p.floatValue.has_value());
    CHECK(p.floatValue->size() == 3);
    CHECK(*p.floatValue == std::vector<float>{ 6.1f, 6.2f, 6.3f });
    REQUIRE(p.stringValue.has_value());
    CHECK(p.stringValue->size() == 3);
    CHECK(*p.stringValue == std::vector<std::string>{ "abc", "def", "ghi" });
    CHECK(p.stringNotEmptyValue->size() == 3);
    CHECK(*p.stringNotEmptyValue == std::vector<std::string>{ "jkl", "mno", "pqr" });
    REQUIRE(p.pathValue.has_value());
    CHECK(p.pathValue->size() == 3);
    CHECK(
        *p.pathValue ==
        std::vector<std::filesystem::path>{ tmpFile1, tmpFile2, tmpFile3 }
    );
    REQUIRE(p.directoryValue.has_value());
    CHECK(p.directoryValue->size() == 3);
    CHECK(
        *p.directoryValue ==
        std::vector<std::filesystem::path>{ tmpFolder1, tmpFolder2, tmpFolder3 }
    );
    REQUIRE(p.ivec2Value.has_value());
    CHECK(p.ivec2Value->size() == 3);
    CHECK(*p.ivec2Value == std::vector<glm::ivec2>{ { 7, 8 }, { 9, 10 }, { 11, 12 } });
    REQUIRE(p.ivec3Value.has_value());
    CHECK(p.ivec3Value->size() == 3);
    CHECK(
        *p.ivec3Value ==
        std::vector<glm::ivec3>{ { 13, 14, 15 }, { 16, 17, 18 }, { 19, 20, 21 } }
    );
    REQUIRE(p.ivec4Value.has_value());
    CHECK(p.ivec4Value->size() == 3);
    CHECK(
        *p.ivec4Value ==
        std::vector<glm::ivec4>{
            { 22, 23, 24, 25 }, { 26, 27, 28, 29 }, { 30, 31, 32, 33 }
        }
    );
    REQUIRE(p.dvec2Value.has_value());
    CHECK(p.dvec2Value->size() == 3);
    CHECK(
        *p.dvec2Value ==
        std::vector<glm::dvec2>{ { 34.1, 34.2 }, { 35.1, 35.2 }, { 36.1, 36.2 }}
    );
    REQUIRE(p.dvec3Value.has_value());
    CHECK(p.dvec3Value->size() == 3);
    CHECK(
        *p.dvec3Value ==
        std::vector<glm::dvec3>{
            { 37.1, 37.2, 37.3 }, { 38.1, 38.2, 38.3 }, { 39.1, 39.2, 39.3 }
        }
    );
    REQUIRE(p.dvec4Value.has_value());
    CHECK(p.dvec4Value->size() == 3);
    CHECK(
        *p.dvec4Value ==
        std::vector<glm::dvec4>{
            { 40.1, 40.2, 40.3, 40.4 },
            { 41.1, 41.2, 41.3, 41.4 },
            { 42.1, 42.2, 42.3, 42.4 }
        }
    );
    REQUIRE(p.vec2Value.has_value());
    CHECK(p.vec2Value->size() == 3);
    CHECK(
        *p.vec2Value ==
        std::vector<glm::vec2>{ { 43.1f, 43.2f }, { 44.1f, 44.2f }, { 45.1f, 45.2f }}
    );
    REQUIRE(p.vec3Value.has_value());
    CHECK(p.vec3Value->size() == 3);
    CHECK(
        *p.vec3Value ==
        std::vector<glm::vec3>{
            { 46.1f, 46.2f, 46.3f }, { 47.1f, 47.2f, 47.3f }, { 48.1f, 48.2f, 48.3f }
    }
    );
    REQUIRE(p.vec4Value.has_value());
    CHECK(p.vec4Value->size() == 3);
    CHECK(
        *p.vec4Value ==
        std::vector<glm::vec4>{
            { 49.1f, 49.2f, 49.3f, 49.4f },
            { 50.1f, 50.2f, 50.3f, 50.4f },
            { 51.1f, 51.2f, 51.3f, 51.4f }
        }
    );
    REQUIRE(p.mat2x2Value.has_value());
    CHECK(p.mat2x2Value->size() == 3);
    CHECK(
        *p.mat2x2Value ==
        std::vector<glm::mat2x2>{
            { 52.1f, 52.2f, 52.3f, 52.4f },
            { 53.1f, 53.2f, 53.3f, 53.4f },
            { 54.1f, 54.2f, 54.3f, 54.4f }
        }
    );
    REQUIRE(p.mat2Value.has_value());
    CHECK(p.mat2Value->size() == 3);
    CHECK(
        *p.mat2Value ==
        std::vector<glm::mat2>{
            { 152.1f, 152.2f, 152.3f, 152.4f },
            { 153.1f, 153.2f, 153.3f, 153.4f },
            { 154.1f, 154.2f, 154.3f, 154.4f }
    }
    );
    REQUIRE(p.mat2x3Value.has_value());
    CHECK(p.mat2x3Value->size() == 3);
    CHECK(
        *p.mat2x3Value ==
        std::vector<glm::mat2x3>{
            { 55.1f, 55.2f, 55.3f, 55.4f, 55.5f, 55.6f },
            { 56.1f, 56.2f, 56.3f, 56.4f, 56.5f, 56.6f },
            { 57.1f, 57.2f, 57.3f, 57.4f, 57.5f, 57.6f }
        }
    );
    REQUIRE(p.mat2x4Value.has_value());
    CHECK(p.mat2x4Value->size() == 3);
    CHECK(
        *p.mat2x4Value ==
        std::vector<glm::mat2x4>{
            { 58.1f, 58.2f, 58.3f, 58.4f, 58.5f, 58.6f, 58.7f, 58.8f },
            { 59.1f, 59.2f, 59.3f, 59.4f, 59.5f, 59.6f, 59.7f, 59.8f },
            { 60.1f, 60.2f, 60.3f, 60.4f, 60.5f, 60.6f, 60.7f, 60.8f }
        }
    );
    REQUIRE(p.mat3x2Value.has_value());
    CHECK(p.mat3x2Value->size() == 3);
    CHECK(
        *p.mat3x2Value ==
        std::vector<glm::mat3x2>{
            { 61.1f, 61.2f, 61.3f, 61.4f, 61.5f, 61.6f },
            { 62.1f, 62.2f, 62.3f, 62.4f, 62.5f, 62.6f },
            { 63.1f, 63.2f, 63.3f, 63.4f, 63.5f, 63.6f }
        }
    );
    REQUIRE(p.mat3x3Value.has_value());
    CHECK(p.mat3x3Value->size() == 3);
    CHECK(
        *p.mat3x3Value ==
        std::vector<glm::mat3x3>{
            { 64.1f, 64.2f, 64.3f, 64.4f, 64.5f, 64.6f, 64.7f, 64.8f, 64.9f },
            { 65.1f, 65.2f, 65.3f, 65.4f, 65.5f, 65.6f, 65.7f, 65.8f, 65.9f },
            { 66.1f, 66.2f, 66.3f, 66.4f, 66.5f, 66.6f, 66.7f, 66.8f, 66.9f }
        }
    );
    REQUIRE(p.mat3Value.has_value());
    CHECK(p.mat3Value->size() == 3);
    CHECK(
        *p.mat3Value ==
        std::vector<glm::mat3>{
            { 164.1f, 164.2f, 164.3f, 164.4f, 164.5f, 164.6f, 164.7f, 164.8f, 164.9f },
            { 165.1f, 165.2f, 165.3f, 165.4f, 165.5f, 165.6f, 165.7f, 165.8f, 165.9f },
            { 166.1f, 166.2f, 166.3f, 166.4f, 166.5f, 166.6f, 166.7f, 166.8f, 166.9f }
    }
    );
    REQUIRE(p.mat3x4Value.has_value());
    CHECK(p.mat3x4Value->size() == 3);
    CHECK(
        *p.mat3x4Value ==
        std::vector<glm::mat3x4>{
            { 67.1f, 67.2f, 67.3f, 67.4f, 67.5f, 67.6f,
              67.7f, 67.8f, 67.9f, 67.10f, 67.11f, 67.12f },
            { 68.1f, 68.2f, 68.3f, 68.4f, 68.5f, 68.6f,
              68.7f, 68.8f, 68.9f, 68.10f, 68.11f, 68.12f },
            { 69.1f, 69.2f, 69.3f, 69.4f, 69.5f, 69.6f,
              69.7f, 69.8f, 69.9f, 69.10f, 69.11f, 69.12f }
        }
    );
    REQUIRE(p.mat4x2Value.has_value());
    CHECK(p.mat4x2Value->size() == 3);
    CHECK(
        *p.mat4x2Value ==
        std::vector<glm::mat4x2>{
            { 70.1f, 70.2f, 70.3f, 70.4f, 70.5f, 70.6f, 70.7f, 70.8f },
            { 71.1f, 71.2f, 71.3f, 71.4f, 71.5f, 71.6f, 71.7f, 71.8f },
            { 72.1f, 72.2f, 72.3f, 72.4f, 72.5f, 72.6f, 72.7f, 72.8f }
    }
    );
    REQUIRE(p.mat4x3Value.has_value());
    CHECK(p.mat4x3Value->size() == 3);
    CHECK(
        *p.mat4x3Value ==
        std::vector<glm::mat4x3>{
            { 73.1f, 73.2f, 73.3f, 73.4f, 73.5f, 73.6f,
              73.7f, 73.8f, 73.9f, 73.10f, 73.11f, 73.12f },
            { 74.1f, 74.2f, 74.3f, 74.4f, 74.5f, 74.6f,
              74.7f, 74.8f, 74.9f, 74.10f, 74.11f, 74.12f },
            { 75.1f, 75.2f, 75.3f, 75.4f, 75.5f, 75.6f,
              75.7f, 75.8f, 75.9f, 75.10f, 75.11f, 75.12f }
    }
    );
    REQUIRE(p.mat4x4Value.has_value());
    CHECK(p.mat4x4Value->size() == 3);
    CHECK(
        *p.mat4x4Value ==
        std::vector<glm::mat4x4>{
            { 76.1f, 76.2f, 76.3f, 76.4f, 76.5f, 76.6f, 76.7f, 76.8f,
              76.9f, 76.10f, 76.11f, 76.12f, 76.13f, 76.14f, 76.15f, 76.16f },
            { 77.1f, 77.2f, 77.3f, 77.4f, 77.5f, 77.6f, 77.7f, 77.8f,
              77.9f, 77.10f, 77.11f, 77.12f, 77.13f, 77.14f, 77.15f, 77.16f },
            { 78.1f, 78.2f, 78.3f, 78.4f, 78.5f, 78.6f, 78.7f, 78.8f,
              78.9f, 78.10f, 78.11f, 78.12f, 78.13f, 78.14f, 78.15f, 78.16f }
        }
    );
    REQUIRE(p.mat4Value.has_value());
    CHECK(p.mat4Value->size() == 3);
    CHECK(
        *p.mat4Value ==
        std::vector<glm::mat4>{
            { 176.1f, 176.2f, 176.3f, 176.4f, 176.5f, 176.6f, 176.7f, 176.8f,
                176.9f, 176.10f, 176.11f, 176.12f, 176.13f, 176.14f, 176.15f, 176.16f },
            { 177.1f, 177.2f, 177.3f, 177.4f, 177.5f, 177.6f, 177.7f, 177.8f,
              177.9f, 177.10f, 177.11f, 177.12f, 177.13f, 177.14f, 177.15f, 177.16f },
            { 178.1f, 178.2f, 178.3f, 178.4f, 178.5f, 178.6f, 178.7f, 178.8f,
              178.9f, 178.10f, 178.11f, 178.12f, 178.13f, 178.14f, 178.15f, 178.16f }
    }
    );
    REQUIRE(p.dmat2x2Value.has_value());
    CHECK(p.dmat2x2Value->size() == 3);
    CHECK(
        *p.dmat2x2Value ==
        std::vector<glm::dmat2x2>{
            { 79.1, 79.2, 79.3, 79.4 },
            { 80.1, 80.2, 80.3, 80.4 },
            { 81.1, 81.2, 81.3, 81.4 }
        }
    );
    REQUIRE(p.dmat2Value.has_value());
    CHECK(p.dmat2Value->size() == 3);
    CHECK(
        *p.dmat2Value ==
        std::vector<glm::dmat2>{
            { 179.1, 179.2, 179.3, 179.4 },
            { 180.1, 180.2, 180.3, 180.4 },
            { 181.1, 181.2, 181.3, 181.4 }
    }
    );
    REQUIRE(p.dmat2x3Value.has_value());
    CHECK(p.dmat2x3Value->size() == 3);
    CHECK(
        *p.dmat2x3Value ==
        std::vector<glm::dmat2x3>{
            { 82.1, 82.2, 82.3, 82.4, 82.5, 82.6 },
            { 83.1, 83.2, 83.3, 83.4, 83.5, 83.6 },
            { 84.1, 84.2, 84.3, 84.4, 84.5, 84.6 }
        }
    );
    REQUIRE(p.dmat2x4Value.has_value());
    CHECK(p.dmat2x4Value->size() == 3);
    CHECK(
        *p.dmat2x4Value ==
        std::vector<glm::dmat2x4>{
            { 85.1, 85.2, 85.3, 85.4, 85.5, 85.6, 85.7, 85.8 },
            { 86.1, 86.2, 86.3, 86.4, 86.5, 86.6, 86.7, 86.8 },
            { 87.1, 87.2, 87.3, 87.4, 87.5, 87.6, 87.7, 87.8 }
        }
    );
    REQUIRE(p.dmat3x2Value.has_value());
    CHECK(p.dmat3x2Value->size() == 3);
    CHECK(
        *p.dmat3x2Value ==
        std::vector<glm::dmat3x2>{
            { 88.1, 88.2, 88.3, 88.4, 88.5, 88.6 },
            { 89.1, 89.2, 89.3, 89.4, 89.5, 89.6 },
            { 90.1, 90.2, 90.3, 90.4, 90.5, 90.6 }
        }
    );
    REQUIRE(p.dmat3x3Value.has_value());
    CHECK(p.dmat3x3Value->size() == 3);
    CHECK(
        *p.dmat3x3Value ==
        std::vector<glm::dmat3x3>{
            { 91.1, 91.2, 91.3, 91.4, 91.5, 91.6, 91.7, 91.8, 91.9 },
            { 92.1, 92.2, 92.3, 92.4, 92.5, 92.6, 92.7, 92.8, 92.9 },
            { 93.1, 93.2, 93.3, 93.4, 93.5, 93.6, 93.7, 93.8, 93.9 }
        }
    );
    REQUIRE(p.dmat3Value.has_value());
    CHECK(p.dmat3Value->size() == 3);
    CHECK(
        *p.dmat3Value ==
        std::vector<glm::dmat3>{
            { 191.1, 191.2, 191.3, 191.4, 191.5, 191.6, 191.7, 191.8, 191.9 },
            { 192.1, 192.2, 192.3, 192.4, 192.5, 192.6, 192.7, 192.8, 192.9 },
            { 193.1, 193.2, 193.3, 193.4, 193.5, 193.6, 193.7, 193.8, 193.9 }
    }
    );
    REQUIRE(p.dmat3x4Value.has_value());
    CHECK(p.dmat3x4Value->size() == 3);
    CHECK(
        *p.dmat3x4Value ==
        std::vector<glm::dmat3x4>{
            { 94.1, 94.2, 94.3, 94.4, 94.5, 94.6, 94.7, 94.8, 94.9, 94.10, 94.11, 94.12 },
            { 95.1, 95.2, 95.3, 95.4, 95.5, 95.6, 95.7, 95.8, 95.9, 95.10, 95.11, 95.12 },
            { 96.1, 96.2, 96.3, 96.4, 96.5, 96.6, 96.7, 96.8, 96.9, 96.10, 96.11, 96.12 }
        }
    );
    REQUIRE(p.dmat4x2Value.has_value());
    CHECK(p.dmat4x2Value->size() == 3);
    CHECK(
        *p.dmat4x2Value ==
        std::vector<glm::dmat4x2>{
            { 97.1, 97.2, 97.3, 97.4, 97.5, 97.6, 97.7, 97.8 },
            { 98.1, 98.2, 98.3, 98.4, 98.5, 98.6, 98.7, 98.8 },
            { 99.1, 99.2, 99.3, 99.4, 99.5, 99.6, 99.7, 99.8 }
        }
    );
    REQUIRE(p.dmat4x3Value.has_value());
    CHECK(p.dmat4x3Value->size() == 3);
    CHECK(
        *p.dmat4x3Value ==
        std::vector<glm::dmat4x3>{
            { 100.1, 100.2, 100.3, 100.4, 100.5, 100.6,
              100.7, 100.8, 100.9, 100.10, 100.11, 100.12 },
            { 101.1, 101.2, 101.3, 101.4, 101.5, 101.6,
              101.7, 101.8, 101.9, 101.10, 101.11, 101.12 },
            { 102.1, 102.2, 102.3, 102.4, 102.5, 102.6,
              102.7, 102.8, 102.9, 102.10, 102.11, 102.12 }
        }
    );
    REQUIRE(p.dmat4x4Value.has_value());
    CHECK(p.dmat4x4Value->size() == 3);
    CHECK(
        *p.dmat4x4Value ==
        std::vector<glm::dmat4x4>{
            { 103.1, 103.2, 103.3, 103.4, 103.5, 103.6, 103.7, 103.8,
              103.9, 103.10, 103.11, 103.12, 103.13, 103.14, 103.15, 103.16 },
            { 104.1, 104.2, 104.3, 104.4, 104.5, 104.6, 104.7, 104.8,
              104.9, 104.10, 104.11, 104.12, 104.13, 104.14, 104.15, 104.16 },
            { 105.1, 105.2, 105.3, 105.4, 105.5, 105.6, 105.7, 105.8,
              105.9, 105.10, 105.11, 105.12, 105.13, 105.14, 105.15, 105.16 }
        }
    );
    REQUIRE(p.dmat4Value.has_value());
    CHECK(p.dmat4Value->size() == 3);
    CHECK(
        *p.dmat4Value ==
        std::vector<glm::dmat4>{
            { 1103.1, 1103.2, 1103.3, 1103.4, 1103.5, 1103.6, 1103.7, 1103.8,
                1103.9, 1103.10, 1103.11, 1103.12, 1103.13, 1103.14, 1103.15, 1103.16 },
            { 1104.1, 1104.2, 1104.3, 1104.4, 1104.5, 1104.6, 1104.7, 1104.8,
              1104.9, 1104.10, 1104.11, 1104.12, 1104.13, 1104.14, 1104.15, 1104.16 },
            { 1105.1, 1105.2, 1105.3, 1105.4, 1105.5, 1105.6, 1105.7, 1105.8,
              1105.9, 1105.10, 1105.11, 1105.12, 1105.13, 1105.14, 1105.15, 1105.16 }
        }
    );
    REQUIRE(p.dictValue.has_value());
    REQUIRE(p.dictValue->size() == 3);
    REQUIRE(p.dictValue->at(0).hasKey("a"));
    REQUIRE(p.dictValue->at(0).hasValue<int>("a"));
    CHECK(p.dictValue->at(0).value<int>("a") == 1);
    REQUIRE(p.dictValue->at(0).hasKey("b"));
    REQUIRE(p.dictValue->at(0).hasValue<double>("b"));
    CHECK(p.dictValue->at(0).value<double>("b") == 2.0);
    REQUIRE(p.dictValue->at(1).hasKey("a"));
    REQUIRE(p.dictValue->at(1).hasValue<int>("a"));
    CHECK(p.dictValue->at(1).value<int>("a") == 3);
    REQUIRE(p.dictValue->at(1).hasKey("b"));
    REQUIRE(p.dictValue->at(1).hasValue<double>("b"));
    CHECK(p.dictValue->at(1).value<double>("b") == 4.0);
    REQUIRE(p.dictValue->at(2).hasKey("a"));
    REQUIRE(p.dictValue->at(2).hasValue<int>("a"));
    CHECK(p.dictValue->at(2).value<int>("a") == 5);
    REQUIRE(p.dictValue->at(2).hasKey("b"));
    REQUIRE(p.dictValue->at(2).hasValue<double>("b"));
    CHECK(p.dictValue->at(2).value<double>("b") == 6.0);


    const ghoul::Dictionary e;
    const Parameters p2 = codegen::bake<Parameters>(e);
    CHECK(!p2.boolValue.has_value());
    CHECK(!p2.intValue.has_value());
    CHECK(!p2.doubleValue.has_value());
    CHECK(!p2.floatValue.has_value());
    CHECK(!p2.stringValue.has_value());
    CHECK(!p2.stringNotEmptyValue.has_value());
    CHECK(!p2.pathValue.has_value());
    CHECK(!p2.directoryValue.has_value());
    CHECK(!p2.ivec2Value.has_value());
    CHECK(!p2.ivec3Value.has_value());
    CHECK(!p2.ivec4Value.has_value());
    CHECK(!p2.dvec2Value.has_value());
    CHECK(!p2.dvec3Value.has_value());
    CHECK(!p2.dvec4Value.has_value());
    CHECK(!p2.vec2Value.has_value());
    CHECK(!p2.vec3Value.has_value());
    CHECK(!p2.vec4Value.has_value());
    CHECK(!p2.mat2x2Value.has_value());
    CHECK(!p2.mat2Value.has_value());
    CHECK(!p2.mat2x3Value.has_value());
    CHECK(!p2.mat2x4Value.has_value());
    CHECK(!p2.mat3x2Value.has_value());
    CHECK(!p2.mat3x3Value.has_value());
    CHECK(!p2.mat3Value.has_value());
    CHECK(!p2.mat3x4Value.has_value());
    CHECK(!p2.mat4x2Value.has_value());
    CHECK(!p2.mat4x3Value.has_value());
    CHECK(!p2.mat4x4Value.has_value());
    CHECK(!p2.mat4Value.has_value());
    CHECK(!p2.dmat2x2Value.has_value());
    CHECK(!p2.dmat2Value.has_value());
    CHECK(!p2.dmat2x3Value.has_value());
    CHECK(!p2.dmat2x4Value.has_value());
    CHECK(!p2.dmat3x2Value.has_value());
    CHECK(!p2.dmat3x3Value.has_value());
    CHECK(!p2.dmat3Value.has_value());
    CHECK(!p2.dmat3x4Value.has_value());
    CHECK(!p2.dmat4x2Value.has_value());
    CHECK(!p2.dmat4x3Value.has_value());
    CHECK(!p2.dmat4x4Value.has_value());
    CHECK(!p2.dmat4Value.has_value());
    CHECK(!p2.dictValue.has_value());
}

TEST_CASE(
    "Execution/Structs/Basic/Types/Optional/Vector:  Documentation",
    "[Execution][Structs]"
)
{
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 42);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "BoolValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "bool value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Boolean");
        CHECK(dynamic_cast<BoolVerifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "IntValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "int value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[2];
        CHECK(e.key == "DoubleValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "double value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[3];
        CHECK(e.key == "FloatValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "float value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[4];
        CHECK(e.key == "StringValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "string value documentation");
        CHECK(e.verifier->type() == "List of strings");
        StringListVerifier* t = dynamic_cast<StringListVerifier*>(e.verifier.get());
        REQUIRE(t);
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(!t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "String");
        StringVerifier* v =
            dynamic_cast<StringVerifier*>(t->documentations[0].verifier.get());
        REQUIRE(v);
        CHECK(v->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[5];
        CHECK(e.key == "StringNotEmptyValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "string not empty value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "String");
        StringVerifier* v =
            dynamic_cast<StringVerifier*>(t->documentations[0].verifier.get());
        REQUIRE(v);
        CHECK(v->mustBeNotEmpty() == true);
    }
    {
        const DocumentationEntry& e = doc.entries[6];
        CHECK(e.key == "PathValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "path value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].verifier->type() == "File");
        CHECK(dynamic_cast<FileVerifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[7];
        CHECK(e.key == "DirectoryValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "directory value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].verifier->type() == "Directory");
        CHECK(dynamic_cast<DirectoryVerifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[8];
        CHECK(e.key == "Ivec2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "ivec2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Vector2<int>");
        CHECK(dynamic_cast<IntVector2Verifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[9];
        CHECK(e.key == "Ivec3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "ivec3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Vector3<int>");
        CHECK(dynamic_cast<IntVector3Verifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[10];
        CHECK(e.key == "Ivec4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "ivec4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Vector4<int>");
        CHECK(dynamic_cast<IntVector4Verifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[11];
        CHECK(e.key == "Dvec2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dvec2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Vector2<double>");
        CHECK(dynamic_cast<DoubleVector2Verifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[12];
        CHECK(e.key == "Dvec3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dvec3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Vector3<double>");
        CHECK(dynamic_cast<DoubleVector3Verifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[13];
        CHECK(e.key == "Dvec4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dvec4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Vector4<double>");
        CHECK(dynamic_cast<DoubleVector4Verifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[14];
        CHECK(e.key == "Vec2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vec2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Vector2<double>");
        CHECK(dynamic_cast<DoubleVector2Verifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[15];
        CHECK(e.key == "Vec3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vec3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Vector3<double>");
        CHECK(dynamic_cast<DoubleVector3Verifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[16];
        CHECK(e.key == "Vec4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vec4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Vector4<double>");
        CHECK(dynamic_cast<DoubleVector4Verifier*>(t->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[17];
        CHECK(e.key == "Mat2x2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat2x2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix2x2<double>");
        CHECK(
            dynamic_cast<DoubleMatrix2x2Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[18];
        CHECK(e.key == "Mat2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix2x2<double>");
        CHECK(
            dynamic_cast<DoubleMatrix2Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[19];
        CHECK(e.key == "Mat2x3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat2x3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix2x3<double>");
        CHECK(
            dynamic_cast<DoubleMatrix2x3Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[20];
        CHECK(e.key == "Mat2x4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat2x4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix2x4<double>");
        CHECK(
            dynamic_cast<DoubleMatrix2x4Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[21];
        CHECK(e.key == "Mat3x2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat3x2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix3x2<double>");
        CHECK(
            dynamic_cast<DoubleMatrix3x2Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[22];
        CHECK(e.key == "Mat3x3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat3x3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix3x3<double>");
        CHECK(
            dynamic_cast<DoubleMatrix3x3Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[23];
        CHECK(e.key == "Mat3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix3x3<double>");
        CHECK(
            dynamic_cast<DoubleMatrix3Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[24];
        CHECK(e.key == "Mat3x4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat3x4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix3x4<double>");
        CHECK(
            dynamic_cast<DoubleMatrix3x4Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[25];
        CHECK(e.key == "Mat4x2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat4x2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix4x2<double>");
        CHECK(
            dynamic_cast<DoubleMatrix4x2Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[26];
        CHECK(e.key == "Mat4x3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat4x3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix4x3<double>");
        CHECK(
            dynamic_cast<DoubleMatrix4x3Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[27];
        CHECK(e.key == "Mat4x4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat4x4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix4x4<double>");
        CHECK(
            dynamic_cast<DoubleMatrix4x4Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[28];
        CHECK(e.key == "Mat4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "mat4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix4x4<double>");
        CHECK(
            dynamic_cast<DoubleMatrix4Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[29];
        CHECK(e.key == "Dmat2x2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat2x2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix2x2<double>");
        CHECK(
            dynamic_cast<DoubleMatrix2x2Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[30];
        CHECK(e.key == "Dmat2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix2x2<double>");
        CHECK(
            dynamic_cast<DoubleMatrix2Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[31];
        CHECK(e.key == "Dmat2x3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat2x3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix2x3<double>");
        CHECK(
            dynamic_cast<DoubleMatrix2x3Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[32];
        CHECK(e.key == "Dmat2x4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat2x4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix2x4<double>");
        CHECK(
            dynamic_cast<DoubleMatrix2x4Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[33];
        CHECK(e.key == "Dmat3x2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat3x2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix3x2<double>");
        CHECK(
            dynamic_cast<DoubleMatrix3x2Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[34];
        CHECK(e.key == "Dmat3x3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat3x3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix3x3<double>");
        CHECK(
            dynamic_cast<DoubleMatrix3x3Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[35];
        CHECK(e.key == "Dmat3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix3x3<double>");
        CHECK(
            dynamic_cast<DoubleMatrix3Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[36];
        CHECK(e.key == "Dmat3x4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat3x4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix3x4<double>");
        CHECK(
            dynamic_cast<DoubleMatrix3x4Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[37];
        CHECK(e.key == "Dmat4x2Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat4x2 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix4x2<double>");
        CHECK(
            dynamic_cast<DoubleMatrix4x2Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[38];
        CHECK(e.key == "Dmat4x3Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat4x3 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix4x3<double>");
        CHECK(
            dynamic_cast<DoubleMatrix4x3Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[39];
        CHECK(e.key == "Dmat4x4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat4x4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix4x4<double>");
        CHECK(
            dynamic_cast<DoubleMatrix4x4Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[40];
        CHECK(e.key == "Dmat4Value");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dmat4 value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].optional);
        CHECK(t->documentations[0].verifier->type() == "Matrix4x4<double>");
        CHECK(
            dynamic_cast<DoubleMatrix4Verifier*>(t->documentations[0].verifier.get())
        );
    }
    {
        const DocumentationEntry& e = doc.entries[41];
        CHECK(e.key == "DictValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dict value documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* t = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(t->documentations.size() == 1);
        CHECK(t->documentations[0].key == "*");
        CHECK(t->documentations[0].verifier->type() == "Table");
        CHECK(dynamic_cast<TableVerifier*>(t->documentations[0].verifier.get()));
    }
}
