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
#include <openspace/lua/lua_helper.h>
#include <openspace/misc/dictionary.h>
#include <openspace/scripting/lualibrary.h>
#include <array>
#include <optional>

using namespace openspace;

namespace {
    [[codegen::luawrap]] void tupleVec3(std::tuple<glm::dvec3> arg) {
        CHECK(std::get<0>(arg).x == 1.1);
        CHECK(std::get<0>(arg).y == 2.2);
        CHECK(std::get<0>(arg).z == 3.3);
    }

    [[codegen::luawrap]] void tupleBoolInt(std::tuple<bool, int> arg) {
        CHECK(std::get<0>(arg) == true);
        CHECK(std::get<1>(arg) == 1);
    }

    [[codegen::luawrap]] void tupleDoubleFloatStringDouble(std::tuple<double, float, std::string, double> arg) {
        CHECK(std::get<0>(arg) == 1.1);
        CHECK(std::get<1>(arg) == 2.2f);
        CHECK(std::get<2>(arg) == "abc");
        CHECK(std::get<3>(arg) == 3.3);
    }

    [[codegen::luawrap]] void tupleVec3Defaulted(std::tuple<glm::dvec3> arg = glm::dvec3(1.1, 2.2, 3.3)) {
        CHECK(std::get<0>(arg).x == 1.1);
        CHECK(std::get<0>(arg).y == 2.2);
        CHECK(std::get<0>(arg).z == 3.3);
    }

    [[codegen::luawrap]] void tupleBoolIntDefaulted(std::tuple<bool, int> arg = std::tuple(true, 1)) {
        CHECK(std::get<0>(arg) == true);
        CHECK(std::get<1>(arg) == 1);
    }

    [[codegen::luawrap]] void tupleDoubleFloatStringDoubleDefaulted(std::tuple<double, float, std::string, double> arg = std::tuple(1.1, 2.2f, "abc", 3.3)) {
        CHECK(std::get<0>(arg) == 1.1);
        CHECK(std::get<1>(arg) == 2.2f);
        CHECK(std::get<2>(arg) == "abc");
        CHECK(std::get<3>(arg) == 3.3);
    }

    [[codegen::luawrap]] void tupleVec3DefaultedCheck(bool isDefaulted, std::tuple<glm::dvec3> arg = glm::dvec3(1.1, 2.2, 3.3)) {
        if (isDefaulted) {
            CHECK(std::get<0>(arg).x == 1.1);
            CHECK(std::get<0>(arg).y == 2.2);
            CHECK(std::get<0>(arg).z == 3.3);
        }
        else {
            CHECK(std::get<0>(arg).x == 4.4);
            CHECK(std::get<0>(arg).y == 5.5);
            CHECK(std::get<0>(arg).z == 6.6);
        }
    }

    [[codegen::luawrap]] void tupleBoolIntDefaultedCheck(bool isDefaulted, std::tuple<bool, int> arg = std::tuple(true, 1)) {
        if (isDefaulted) {
            CHECK(std::get<0>(arg) == true);
            CHECK(std::get<1>(arg) == 1);
        }
        else {
            CHECK(std::get<0>(arg) == false);
            CHECK(std::get<1>(arg) == 2);
        }
    }

    [[codegen::luawrap]] void tupleDoubleFloatStringDoubleDefaultedCheck(bool isDefaulted, std::tuple<double, float, std::string, double> arg = std::tuple(1.1, 2.2f, "abc", 3.3)) {
        if (isDefaulted) {
            CHECK(std::get<0>(arg) == 1.1);
            CHECK(std::get<1>(arg) == 2.2f);
            CHECK(std::get<2>(arg) == "abc");
            CHECK(std::get<3>(arg) == 3.3);
        }
        else {
            CHECK(std::get<0>(arg) == 4.4);
            CHECK(std::get<1>(arg) == 5.5f);
            CHECK(std::get<2>(arg) == "def");
            CHECK(std::get<3>(arg) == 6.6);
        }
    }

    [[codegen::luawrap]] void tupleVec3Map(std::map<std::string, std::tuple<glm::dvec3>> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(std::get<0>(arg.find("key1")->second).x == 1.1);
        CHECK(std::get<0>(arg.find("key1")->second).y == 2.2);
        CHECK(std::get<0>(arg.find("key1")->second).z == 3.3);
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(std::get<0>(arg.find("key2")->second).x == 4.4);
        CHECK(std::get<0>(arg.find("key2")->second).y == 5.5);
        CHECK(std::get<0>(arg.find("key2")->second).z == 6.6);
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(std::get<0>(arg.find("key3")->second).x == 7.7);
        CHECK(std::get<0>(arg.find("key3")->second).y == 8.8);
        CHECK(std::get<0>(arg.find("key3")->second).z == 9.9);
    }

    [[codegen::luawrap]] void tupleBoolIntMap(std::map<std::string, std::tuple<bool, int>> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(std::get<0>(arg.find("key1")->second) == true);
        CHECK(std::get<1>(arg.find("key1")->second) == 1);
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(std::get<0>(arg.find("key2")->second) == false);
        CHECK(std::get<1>(arg.find("key2")->second) == 2);
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(std::get<0>(arg.find("key3")->second) == true);
        CHECK(std::get<1>(arg.find("key3")->second) == 3);
    }

    [[codegen::luawrap]] void tupleDoubleFloatStringDoubleMap(std::map<std::string, std::tuple<double, float, std::string, double>> arg) {
        CHECK(arg.size() == 3);
        REQUIRE(arg.find("key1") != arg.end());
        CHECK(std::get<0>(arg.find("key1")->second) == 1.1);
        CHECK(std::get<1>(arg.find("key1")->second) == 2.2f);
        CHECK(std::get<2>(arg.find("key1")->second) == "abc");
        CHECK(std::get<3>(arg.find("key1")->second) == 3.3);
        REQUIRE(arg.find("key2") != arg.end());
        CHECK(std::get<0>(arg.find("key2")->second) == 4.4);
        CHECK(std::get<1>(arg.find("key2")->second) == 5.5f);
        CHECK(std::get<2>(arg.find("key2")->second) == "def");
        CHECK(std::get<3>(arg.find("key2")->second) == 6.6);
        REQUIRE(arg.find("key3") != arg.end());
        CHECK(std::get<0>(arg.find("key3")->second) == 7.7);
        CHECK(std::get<1>(arg.find("key3")->second) == 8.8f);
        CHECK(std::get<2>(arg.find("key3")->second) == "ghi");
        CHECK(std::get<3>(arg.find("key3")->second) == 9.9);
    }

    [[codegen::luawrap]] void tupleVec3Optional(std::optional<std::tuple<glm::dvec3>> arg) {
        REQUIRE(arg.has_value());
        CHECK(std::get<0>(*arg).x == 1.1);
        CHECK(std::get<0>(*arg).y == 2.2);
        CHECK(std::get<0>(*arg).z == 3.3);
    }

    [[codegen::luawrap]] void tupleBoolIntOptional(std::optional<std::tuple<bool, int>> arg) {
        REQUIRE(arg.has_value());
        CHECK(std::get<0>(*arg) == true);
        CHECK(std::get<1>(*arg) == 1);
    }

    [[codegen::luawrap]] void tupleDoubleFloatStringDoubleOptional(std::optional<std::tuple<double, float, std::string, double>> arg) {
        REQUIRE(arg.has_value());
        CHECK(std::get<0>(*arg) == 1.1);
        CHECK(std::get<1>(*arg) == 2.2f);
        CHECK(std::get<2>(*arg) == "abc");
        CHECK(std::get<3>(*arg) == 3.3);
    }

    [[codegen::luawrap]] void tupleVec3Vector(std::vector<std::tuple<glm::dvec3>> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(std::get<0>(arg[0]).x == 1.1);
        CHECK(std::get<0>(arg[0]).y == 2.2);
        CHECK(std::get<0>(arg[0]).z == 3.3);
        CHECK(std::get<0>(arg[1]).x == 4.4);
        CHECK(std::get<0>(arg[1]).y == 5.5);
        CHECK(std::get<0>(arg[1]).z == 6.6);
        CHECK(std::get<0>(arg[2]).x == 7.7);
        CHECK(std::get<0>(arg[2]).y == 8.8);
        CHECK(std::get<0>(arg[2]).z == 9.9);
    }

    [[codegen::luawrap]] void tupleBoolIntVector(std::vector<std::tuple<bool, int>> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(std::get<0>(arg[0]) == true);
        CHECK(std::get<1>(arg[0]) == 1);
        CHECK(std::get<0>(arg[1]) == false);
        CHECK(std::get<1>(arg[1]) == 2);
        CHECK(std::get<0>(arg[2]) == true);
        CHECK(std::get<1>(arg[2]) == 3);
    }

    [[codegen::luawrap]] void tupleDoubleFloatStringDoubleVector(std::vector<std::tuple<double, float, std::string, double>> arg) {
        REQUIRE(arg.size() == 3);
        CHECK(std::get<0>(arg[0]) == 1.1);
        CHECK(std::get<1>(arg[0]) == 2.2f);
        CHECK(std::get<2>(arg[0]) == "abc");
        CHECK(std::get<3>(arg[0]) == 3.3);
        CHECK(std::get<0>(arg[1]) == 4.4);
        CHECK(std::get<1>(arg[1]) == 5.5f);
        CHECK(std::get<2>(arg[1]) == "def");
        CHECK(std::get<3>(arg[1]) == 6.6);
        CHECK(std::get<0>(arg[2]) == 7.7);
        CHECK(std::get<1>(arg[2]) == 8.8f);
        CHECK(std::get<2>(arg[2]) == "ghi");
        CHECK(std::get<3>(arg[2]) == 9.9);
    }

    [[codegen::luawrap]] void tupleVec3Array1(std::array<std::tuple<glm::dvec3>, 1> arg) {
        CHECK(std::get<0>(arg[0]).x == 1.1);
        CHECK(std::get<0>(arg[0]).y == 2.2);
        CHECK(std::get<0>(arg[0]).z == 3.3);
    }

    [[codegen::luawrap]] void tupleBoolIntArray1(std::array<std::tuple<bool, int>, 1> arg) {
        CHECK(std::get<0>(arg[0]) == true);
        CHECK(std::get<1>(arg[0]) == 1);
    }

    [[codegen::luawrap]] void tupleDoubleFloatStringDoubleArray1(std::array<std::tuple<double, float, std::string, double>, 1> arg) {
        CHECK(std::get<0>(arg[0]) == 1.1);
        CHECK(std::get<1>(arg[0]) == 2.2f);
        CHECK(std::get<2>(arg[0]) == "abc");
        CHECK(std::get<3>(arg[0]) == 3.3);
    }

    [[codegen::luawrap]] void tupleVec3Array5(std::array<std::tuple<glm::dvec3>, 5> arg) {
        CHECK(std::get<0>(arg[0]).x == 1.1);
        CHECK(std::get<0>(arg[0]).y == 2.2);
        CHECK(std::get<0>(arg[0]).z == 3.3);
        CHECK(std::get<0>(arg[1]).x == 4.4);
        CHECK(std::get<0>(arg[1]).y == 5.5);
        CHECK(std::get<0>(arg[1]).z == 6.6);
        CHECK(std::get<0>(arg[2]).x == 7.7);
        CHECK(std::get<0>(arg[2]).y == 8.8);
        CHECK(std::get<0>(arg[2]).z == 9.9);
        CHECK(std::get<0>(arg[3]).x == 10.10);
        CHECK(std::get<0>(arg[3]).y == 11.11);
        CHECK(std::get<0>(arg[3]).z == 12.12);
        CHECK(std::get<0>(arg[4]).x == 13.13);
        CHECK(std::get<0>(arg[4]).y == 14.14);
        CHECK(std::get<0>(arg[4]).z == 15.15);
    }

    [[codegen::luawrap]] void tupleBoolIntArray5(std::array<std::tuple<bool, int>, 5> arg) {
        CHECK(std::get<0>(arg[0]) == true);
        CHECK(std::get<1>(arg[0]) == 1);
        CHECK(std::get<0>(arg[1]) == false);
        CHECK(std::get<1>(arg[1]) == 2);
        CHECK(std::get<0>(arg[2]) == true);
        CHECK(std::get<1>(arg[2]) == 3);
        CHECK(std::get<0>(arg[3]) == false);
        CHECK(std::get<1>(arg[3]) == 4);
        CHECK(std::get<0>(arg[4]) == true);
        CHECK(std::get<1>(arg[4]) == 5);
    }

    [[codegen::luawrap]] void tupleDoubleFloatStringDoubleArray5(std::array<std::tuple<double, float, std::string, double>, 5> arg) {
        CHECK(std::get<0>(arg[0]) == 1.1);
        CHECK(std::get<1>(arg[0]) == 2.2f);
        CHECK(std::get<2>(arg[0]) == "abc");
        CHECK(std::get<3>(arg[0]) == 3.3);
        CHECK(std::get<0>(arg[1]) == 4.4);
        CHECK(std::get<1>(arg[1]) == 5.5f);
        CHECK(std::get<2>(arg[1]) == "def");
        CHECK(std::get<3>(arg[1]) == 6.6);
        CHECK(std::get<0>(arg[2]) == 7.7);
        CHECK(std::get<1>(arg[2]) == 8.8f);
        CHECK(std::get<2>(arg[2]) == "ghi");
        CHECK(std::get<3>(arg[2]) == 9.9);
        CHECK(std::get<0>(arg[3]) == 10.10);
        CHECK(std::get<1>(arg[3]) == 11.11f);
        CHECK(std::get<2>(arg[3]) == "jkl");
        CHECK(std::get<3>(arg[3]) == 12.12);
        CHECK(std::get<0>(arg[4]) == 13.13);
        CHECK(std::get<1>(arg[4]) == 14.14f);
        CHECK(std::get<2>(arg[4]) == "mno");
        CHECK(std::get<3>(arg[4]) == 15.15);
    }

    [[codegen::luawrap]] void tupleVec3Array10(std::array<std::tuple<glm::dvec3>, 10> arg) {
        CHECK(std::get<0>(arg[0]).x == 1.1);
        CHECK(std::get<0>(arg[0]).y == 2.2);
        CHECK(std::get<0>(arg[0]).z == 3.3);
        CHECK(std::get<0>(arg[1]).x == 4.4);
        CHECK(std::get<0>(arg[1]).y == 5.5);
        CHECK(std::get<0>(arg[1]).z == 6.6);
        CHECK(std::get<0>(arg[2]).x == 7.7);
        CHECK(std::get<0>(arg[2]).y == 8.8);
        CHECK(std::get<0>(arg[2]).z == 9.9);
        CHECK(std::get<0>(arg[3]).x == 10.10);
        CHECK(std::get<0>(arg[3]).y == 11.11);
        CHECK(std::get<0>(arg[3]).z == 12.12);
        CHECK(std::get<0>(arg[4]).x == 13.13);
        CHECK(std::get<0>(arg[4]).y == 14.14);
        CHECK(std::get<0>(arg[4]).z == 15.15);
        CHECK(std::get<0>(arg[5]).x == 16.16);
        CHECK(std::get<0>(arg[5]).y == 17.17);
        CHECK(std::get<0>(arg[5]).z == 18.18);
        CHECK(std::get<0>(arg[6]).x == 19.19);
        CHECK(std::get<0>(arg[6]).y == 20.20);
        CHECK(std::get<0>(arg[6]).z == 21.21);
        CHECK(std::get<0>(arg[7]).x == 22.22);
        CHECK(std::get<0>(arg[7]).y == 23.23);
        CHECK(std::get<0>(arg[7]).z == 24.24);
        CHECK(std::get<0>(arg[8]).x == 25.25);
        CHECK(std::get<0>(arg[8]).y == 26.26);
        CHECK(std::get<0>(arg[8]).z == 27.27);
        CHECK(std::get<0>(arg[9]).x == 28.28);
        CHECK(std::get<0>(arg[9]).y == 29.29);
        CHECK(std::get<0>(arg[9]).z == 30.30);
    }

    [[codegen::luawrap]] void tupleBoolIntArray10(std::array<std::tuple<bool, int>, 10> arg) {
        CHECK(std::get<0>(arg[0]) == true);
        CHECK(std::get<1>(arg[0]) == 1);
        CHECK(std::get<0>(arg[1]) == false);
        CHECK(std::get<1>(arg[1]) == 2);
        CHECK(std::get<0>(arg[2]) == true);
        CHECK(std::get<1>(arg[2]) == 3);
        CHECK(std::get<0>(arg[3]) == false);
        CHECK(std::get<1>(arg[3]) == 4);
        CHECK(std::get<0>(arg[4]) == true);
        CHECK(std::get<1>(arg[4]) == 5);
        CHECK(std::get<0>(arg[5]) == false);
        CHECK(std::get<1>(arg[5]) == 6);
        CHECK(std::get<0>(arg[6]) == true);
        CHECK(std::get<1>(arg[6]) == 7);
        CHECK(std::get<0>(arg[7]) == false);
        CHECK(std::get<1>(arg[7]) == 8);
        CHECK(std::get<0>(arg[8]) == true);
        CHECK(std::get<1>(arg[8]) == 9);
        CHECK(std::get<0>(arg[9]) == false);
        CHECK(std::get<1>(arg[9]) == 10);
    }

    [[codegen::luawrap]] void tupleDoubleFloatStringDoubleArray10(std::array<std::tuple<double, float, std::string, double>, 10> arg) {
        CHECK(std::get<0>(arg[0]) == 1.1);
        CHECK(std::get<1>(arg[0]) == 2.2f);
        CHECK(std::get<2>(arg[0]) == "abc");
        CHECK(std::get<3>(arg[0]) == 3.3);
        CHECK(std::get<0>(arg[1]) == 4.4);
        CHECK(std::get<1>(arg[1]) == 5.5f);
        CHECK(std::get<2>(arg[1]) == "def");
        CHECK(std::get<3>(arg[1]) == 6.6);
        CHECK(std::get<0>(arg[2]) == 7.7);
        CHECK(std::get<1>(arg[2]) == 8.8f);
        CHECK(std::get<2>(arg[2]) == "ghi");
        CHECK(std::get<3>(arg[2]) == 9.9);
        CHECK(std::get<0>(arg[3]) == 10.10);
        CHECK(std::get<1>(arg[3]) == 11.11f);
        CHECK(std::get<2>(arg[3]) == "jkl");
        CHECK(std::get<3>(arg[3]) == 12.12);
        CHECK(std::get<0>(arg[4]) == 13.13);
        CHECK(std::get<1>(arg[4]) == 14.14f);
        CHECK(std::get<2>(arg[4]) == "mno");
        CHECK(std::get<3>(arg[4]) == 15.15);
        CHECK(std::get<0>(arg[5]) == 16.16);
        CHECK(std::get<1>(arg[5]) == 17.17f);
        CHECK(std::get<2>(arg[5]) == "pqr");
        CHECK(std::get<3>(arg[5]) == 18.18);
        CHECK(std::get<0>(arg[6]) == 19.19);
        CHECK(std::get<1>(arg[6]) == 20.20f);
        CHECK(std::get<2>(arg[6]) == "stu");
        CHECK(std::get<3>(arg[6]) == 21.21);
        CHECK(std::get<0>(arg[7]) == 22.22);
        CHECK(std::get<1>(arg[7]) == 23.23f);
        CHECK(std::get<2>(arg[7]) == "vwx");
        CHECK(std::get<3>(arg[7]) == 24.24);
        CHECK(std::get<0>(arg[8]) == 25.25);
        CHECK(std::get<1>(arg[8]) == 26.26f);
        CHECK(std::get<2>(arg[8]) == "yzz");
        CHECK(std::get<3>(arg[8]) == 27.27);
        CHECK(std::get<0>(arg[9]) == 28.28);
        CHECK(std::get<1>(arg[9]) == 29.29f);
        CHECK(std::get<2>(arg[9]) == "ABC");
        CHECK(std::get<3>(arg[9]) == 30.30);
    }

    [[codegen::luawrap]] std::tuple<glm::dvec3> returnTupleVec3() {
        return glm::dvec3(1.1, 2.2, 3.3);
    }

    [[codegen::luawrap]] std::tuple<bool, int> returnTupleBoolInt() {
        return { true, 1 };
    }

    [[codegen::luawrap]] std::tuple<double, float, std::string, double> returnTupleDoubleFloatStringDouble() {
        return { 1.1, 2.2f, "abc", 3.3 };
    }

    [[codegen::luawrap]] std::map<std::string, std::tuple<glm::dvec3>> returnTupleVec3Map() {
        return {
            { "key1", glm::dvec3(1.1, 2.2, 3.3) },
            { "key2", glm::dvec3(4.4, 5.5, 6.6) },
            { "key3", glm::dvec3(7.7, 8.8, 9.9) }
        };
    }

    [[codegen::luawrap]] std::map<std::string, std::tuple<bool, int>> returnTupleBoolIntMap() {
        return {
            { "key1", { true, 1 } },
            { "key2", { false, 2 } },
            { "key3", { true, 3 } }
        };
    }

    [[codegen::luawrap]] std::map<std::string, std::tuple<double, float, std::string, double>> returnTupleDoubleFloatStringDoubleMap() {
        return {
            { "key1", { 1.1, 2.2f, "abc", 3.3 } },
            { "key2", { 4.4, 5.5f, "def", 6.6 } },
            { "key3", { 7.7, 8.8f, "ghi", 9.9 } }
        };
    }

    [[codegen::luawrap]] std::vector<std::tuple<glm::dvec3>> returnTupleVec3Vector() {
        return {
            std::tuple(glm::dvec3(1.1, 2.2, 3.3)),
            std::tuple(glm::dvec3(4.4, 5.5, 6.6)),
            std::tuple(glm::dvec3(7.7, 8.8, 9.9))
        };
    }

    [[codegen::luawrap]] std::vector<std::tuple<bool, int>> returnTupleBoolIntVector() {
        return {
            std::tuple(true, 1),
            std::tuple(false, 2),
            std::tuple(true, 3)
        };
    }

    [[codegen::luawrap]] std::vector<std::tuple<double, float, std::string, double>> returnTupleDoubleFloatStringDoubleVector() {
        return {
            std::tuple(1.1, 2.2f, "abc", 3.3),
            std::tuple(4.4, 5.5f, "def", 6.6),
            std::tuple(7.7, 8.8f, "ghi", 9.9)
        };
    }

    [[codegen::luawrap]] std::optional<std::tuple<glm::dvec3>> returnTupleVec3Optional() {
        return { glm::dvec3(1.1, 2.2, 3.3) };
    }

    [[codegen::luawrap]] std::optional<std::tuple<bool, int>> returnTupleBoolIntOptional() {
        return std::tuple(true, 1);
    }

    [[codegen::luawrap]] std::optional<std::tuple<double, float, std::string, double>> returnTupleDoubleFloatStringDoubleOptional() {
        return std::tuple<double, float, std::string, double>(1.1, 2.2f, "abc", 3.3);
    }

    [[codegen::luawrap]] std::optional<std::tuple<glm::dvec3>> returnTupleVec3OptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::optional<std::tuple<bool, int>> returnTupleBoolIntOptionalNullopt() {
        return std::nullopt;
    }

    [[codegen::luawrap]] std::optional<std::tuple<double, float, std::string, double>> returnTupleDoubleFloatStringDoubleOptionalNullopt() {
        return std::nullopt;
    }
} // namespace
#include "execution_luawrapper_types_tuple_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Arguments: tuple(vec3)", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::TupleVec3;
    CHECK(func.name == "tupleVec3");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(vec3)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::tuple(glm::dvec3(1.1, 2.2, 3.3)));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments: tuple(bool, int)", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::TupleBoolInt;
    CHECK(func.name == "tupleBoolInt");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Boolean, Integer)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::tuple(true, 1));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(double, float, string, double)",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleDoubleFloatStringDouble;
    CHECK(func.name == "tupleDoubleFloatStringDouble");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Number, Number, String, Number)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::tuple(1.1, 2.2f, "abc", 3.3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(vec3) defaulted",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleVec3Defaulted;
    CHECK(func.name == "tupleVec3Defaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(vec3)?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::tuple(glm::dvec3(1.1, 2.2, 3.3)));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(bool, int) defaulted",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleBoolIntDefaulted;
    CHECK(func.name == "tupleBoolIntDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Boolean, Integer)?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::tuple(true, 1));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(double, float, string, double) defaulted",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleDoubleFloatStringDoubleDefaulted;
    CHECK(func.name == "tupleDoubleFloatStringDoubleDefaulted");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Number, Number, String, Number)?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::tuple(1.1, 2.2f, "abc", 3.3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(vec3) defaulted check",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleVec3DefaultedCheck;
    CHECK(func.name == "tupleVec3DefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "(vec3)?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, std::tuple(glm::dvec3(4.4, 5.5, 6.6)));
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(state, true);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(bool, int) defaulted check",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleBoolIntDefaultedCheck;
    CHECK(func.name == "tupleBoolIntDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "(Boolean, Integer)?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, std::tuple(false, 2));
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(state, true);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: "
    "tuple(double, float, string, double) defaulted check",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleDoubleFloatStringDoubleDefaultedCheck;
    CHECK(func.name == "tupleDoubleFloatStringDoubleDefaultedCheck");
    REQUIRE(func.arguments.size() == 2);
    CHECK(func.arguments[0].name == "isDefaulted");
    CHECK(func.arguments[0].type == "Boolean");
    CHECK(func.arguments[1].name == "arg");
    CHECK(func.arguments[1].type == "(Number, Number, String, Number)?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, false, std::tuple(4.4, 5.5f, "def", 6.6));
    func.function(state);
    CHECK(lua_gettop(state) == 0);

    lua::push(state, true);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Arguments: tuple(vec3) map", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::TupleVec3Map;
    CHECK(func.name == "tupleVec3Map");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> (vec3)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::map<std::string, std::tuple<glm::dvec3>> m = {
        { "key1", std::tuple(glm::dvec3(1.1, 2.2, 3.3)) },
        { "key2", std::tuple(glm::dvec3(4.4, 5.5, 6.6)) },
        { "key3", std::tuple(glm::dvec3(7.7, 8.8, 9.9)) }
    };
    lua::push(state, m);
    const std::string s = lua::stackInformation(state);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(bool, int) map",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleBoolIntMap;
    CHECK(func.name == "tupleBoolIntMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> (Boolean, Integer)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::map<std::string, std::tuple<bool, int>> m = {
        { "key1", std::tuple(true, 1) },
        { "key2", std::tuple(false, 2) },
        { "key3", std::tuple(true, 3) }
    };
    lua::push(state, m);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(double, float, string, double) map",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleDoubleFloatStringDoubleMap;
    CHECK(func.name == "tupleDoubleFloatStringDoubleMap");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "String -> (Number, Number, String, Number)");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::map<std::string, std::tuple<double, float, std::string, double>> m = {
        { "key1", std::tuple(1.1, 2.2f, "abc", 3.3) },
        { "key2", std::tuple(4.4, 5.5f, "def", 6.6) },
        { "key3", std::tuple(7.7, 8.8f, "ghi", 9.9) }
    };
    lua::push(state, m);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(vec3) optional",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleVec3Optional;
    CHECK(func.name == "tupleVec3Optional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(vec3)?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::tuple(glm::dvec3(1.1, 2.2, 3.3)));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(bool, int) optional",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleBoolIntOptional;
    CHECK(func.name == "tupleBoolIntOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Boolean, Integer)?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::tuple(true, 1));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(double, float, string, double) optional",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleDoubleFloatStringDoubleOptional;
    CHECK(func.name == "tupleDoubleFloatStringDoubleOptional");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Number, Number, String, Number)?");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    lua::push(state, std::tuple(1.1, 2.2f, "abc", 3.3));
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(vec3) vector",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleVec3Vector;
    CHECK(func.name == "tupleVec3Vector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(vec3)[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::vector<std::tuple<glm::dvec3>> vec = {
        std::tuple(glm::dvec3(1.1, 2.2, 3.3)),
        std::tuple(glm::dvec3(4.4, 5.5, 6.6)),
        std::tuple(glm::dvec3(7.7, 8.8, 9.9))
    };
    lua::push(state, vec);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(bool, int) vector",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleBoolIntVector;
    CHECK(func.name == "tupleBoolIntVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Boolean, Integer)[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::vector<std::tuple<bool, int>> vec = {
        std::tuple(true, 1),
        std::tuple(false, 2),
        std::tuple(true, 3)
    };
    lua::push(state, vec);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(double, float, string, double) vector",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleDoubleFloatStringDoubleVector;
    CHECK(func.name == "tupleDoubleFloatStringDoubleVector");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Number, Number, String, Number)[]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::vector<std::tuple<double, float, std::string, double>> vec = {
        std::tuple(1.1, 2.2f, "abc", 3.3),
        std::tuple(4.4, 5.5f, "def", 6.6),
        std::tuple(7.7, 8.8f, "ghi", 9.9)
    };
    lua::push(state, vec);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(vec3) array1",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleVec3Array1;
    CHECK(func.name == "tupleVec3Array1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(vec3)[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::array<std::tuple<glm::dvec3>, 1> arr = {
        std::tuple(glm::dvec3(1.1, 2.2, 3.3))
    };
    lua::push(state, arr);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(bool, int) array1",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleBoolIntArray1;
    CHECK(func.name == "tupleBoolIntArray1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Boolean, Integer)[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::array<std::tuple<bool, int>, 1> arr = {
        std::tuple(true, 1)
    };
    lua::push(state, arr);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(double, float, string, double) array1",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleDoubleFloatStringDoubleArray1;
    CHECK(func.name == "tupleDoubleFloatStringDoubleArray1");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Number, Number, String, Number)[1]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::array<std::tuple<double, float, std::string, double>, 1> arr = {
        std::tuple(1.1, 2.2f, "abc", 3.3)
    };
    lua::push(state, arr);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(vec3) array5",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleVec3Array5;
    CHECK(func.name == "tupleVec3Array5");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(vec3)[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::array<std::tuple<glm::dvec3>, 5> arr = {
        std::tuple(glm::dvec3(1.1, 2.2, 3.3)),
        std::tuple(glm::dvec3(4.4, 5.5, 6.6)),
        std::tuple(glm::dvec3(7.7, 8.8, 9.9)),
        std::tuple(glm::dvec3(10.10, 11.11, 12.12)),
        std::tuple(glm::dvec3(13.13, 14.14, 15.15))
    };
    lua::push(state, arr);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(bool, int) array5",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleBoolIntArray5;
    CHECK(func.name == "tupleBoolIntArray5");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Boolean, Integer)[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::array<std::tuple<bool, int>, 5> arr = {
        std::tuple(true, 1),
        std::tuple(false, 2),
        std::tuple(true, 3),
        std::tuple(false, 4),
        std::tuple(true, 5)
    };
    lua::push(state, arr);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(double, float, string, double) array5",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleDoubleFloatStringDoubleArray5;
    CHECK(func.name == "tupleDoubleFloatStringDoubleArray5");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Number, Number, String, Number)[5]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::array<std::tuple<double, float, std::string, double>, 5> arr = {
        std::tuple(1.1, 2.2f, "abc", 3.3),
        std::tuple(4.4, 5.5f, "def", 6.6),
        std::tuple(7.7, 8.8f, "ghi", 9.9),
        std::tuple(10.10, 11.11f, "jkl", 12.12),
        std::tuple(13.13, 14.14f, "mno", 15.15)
    };
    lua::push(state, arr);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(vec3) array10",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleVec3Array10;
    CHECK(func.name == "tupleVec3Array10");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(vec3)[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::array<std::tuple<glm::dvec3>, 10> arr = {
        std::tuple(glm::dvec3(1.1, 2.2, 3.3)),
        std::tuple(glm::dvec3(4.4, 5.5, 6.6)),
        std::tuple(glm::dvec3(7.7, 8.8, 9.9)),
        std::tuple(glm::dvec3(10.10, 11.11, 12.12)),
        std::tuple(glm::dvec3(13.13, 14.14, 15.15)),
        std::tuple(glm::dvec3(16.16, 17.17, 18.18)),
        std::tuple(glm::dvec3(19.19, 20.20, 21.21)),
        std::tuple(glm::dvec3(22.22, 23.23, 24.24)),
        std::tuple(glm::dvec3(25.25, 26.26, 27.27)),
        std::tuple(glm::dvec3(28.28, 29.29, 30.30))
    };
    lua::push(state, arr);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(bool, int) array10",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleBoolIntArray10;
    CHECK(func.name == "tupleBoolIntArray10");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Boolean, Integer)[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::array<std::tuple<bool, int>, 10> arr = {
        std::tuple(true, 1),
        std::tuple(false, 2),
        std::tuple(true, 3),
        std::tuple(false, 4),
        std::tuple(true, 5),
        std::tuple(false, 6),
        std::tuple(true, 7),
        std::tuple(false, 8),
        std::tuple(true, 9),
        std::tuple(false, 10),
    };
    lua::push(state, arr);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Arguments: tuple(double, float, string, double) array10",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::TupleDoubleFloatStringDoubleArray10;
    CHECK(func.name == "tupleDoubleFloatStringDoubleArray10");
    REQUIRE(func.arguments.size() == 1);
    CHECK(func.arguments[0].name == "arg");
    CHECK(func.arguments[0].type == "(Number, Number, String, Number)[10]");
    CHECK(func.returnType.empty());
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    const std::array<std::tuple<double, float, std::string, double>, 10> arr = {
        std::tuple(1.1, 2.2f, "abc", 3.3),
        std::tuple(4.4, 5.5f, "def", 6.6),
        std::tuple(7.7, 8.8f, "ghi", 9.9),
        std::tuple(10.10, 11.11f, "jkl", 12.12),
        std::tuple(13.13, 14.14f, "mno", 15.15),
        std::tuple(16.16, 17.17f, "pqr", 18.18),
        std::tuple(19.19, 20.20f, "stu", 21.21),
        std::tuple(22.22, 23.23f, "vwx", 24.24),
        std::tuple(25.25, 26.26f, "yzz", 27.27),
        std::tuple(28.28, 29.29f, "ABC", 30.30)
    };
    lua::push(state, arr);
    func.function(state);
    CHECK(lua_gettop(state) == 0);
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  tuple(vec3)", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnTupleVec3;
    CHECK(func.name == "returnTupleVec3");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(vec3)");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::tuple<glm::dvec3> val = lua::value<std::tuple<glm::dvec3>>(state);
    CHECK(std::get<0>(val) == glm::dvec3(1.1, 2.2, 3.3));
    lua_close(state);
}

TEST_CASE("Execution/LuaWrapper/Return:  tuple(bool,int)", "[Execution][LuaWrapper]") {
    LuaLibrary::Function func = codegen::lua::ReturnTupleBoolInt;
    CHECK(func.name == "returnTupleBoolInt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(Boolean, Integer)");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    std::string abc = lua::stackInformation(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::tuple<bool, int> val = lua::value<std::tuple<bool, int>>(state);
    CHECK(std::get<0>(val) == true);
    CHECK(std::get<1>(val) == 1);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(double,float,string,double)",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleDoubleFloatStringDouble;
    CHECK(func.name == "returnTupleDoubleFloatStringDouble");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(Number, Number, String, Number)");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::tuple<double, float, std::string, double> val =
        lua::value<std::tuple<double, float, std::string, double>>(state);
    CHECK(std::get<0>(val) == 1.1);
    CHECK(std::get<1>(val) == 2.2f);
    CHECK(std::get<2>(val) == "abc");
    CHECK(std::get<3>(val) == 3.3);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(vec3) map",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleVec3Map;
    CHECK(func.name == "returnTupleVec3Map");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> (vec3)");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::map<std::string, std::tuple<glm::dvec3>> val =
        lua::value<std::map<std::string, std::tuple<glm::dvec3>>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val.find("key1") != val.end());
    CHECK(std::get<0>(val.find("key1")->second).x == 1.1);
    CHECK(std::get<0>(val.find("key1")->second).y == 2.2);
    CHECK(std::get<0>(val.find("key1")->second).z == 3.3);
    CHECK(val.find("key2") != val.end());
    CHECK(std::get<0>(val.find("key2")->second).x == 4.4);
    CHECK(std::get<0>(val.find("key2")->second).y == 5.5);
    CHECK(std::get<0>(val.find("key2")->second).z == 6.6);
    CHECK(val.find("key3") != val.end());
    CHECK(std::get<0>(val.find("key3")->second).x == 7.7);
    CHECK(std::get<0>(val.find("key3")->second).y == 8.8);
    CHECK(std::get<0>(val.find("key3")->second).z == 9.9);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(bool, int) map",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleBoolIntMap;
    CHECK(func.name == "returnTupleBoolIntMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> (Boolean, Integer)");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::map<std::string, std::tuple<bool, int>> val =
        lua::value<std::map<std::string, std::tuple<bool, int>>>(state);
    REQUIRE(val.size() == 3);
    CHECK(val.find("key1") != val.end());
    CHECK(std::get<0>(val.find("key1")->second) == true);
    CHECK(std::get<1>(val.find("key1")->second) == 1);
    CHECK(val.find("key2") != val.end());
    CHECK(std::get<0>(val.find("key2")->second) == false);
    CHECK(std::get<1>(val.find("key2")->second) == 2);
    CHECK(val.find("key3") != val.end());
    CHECK(std::get<0>(val.find("key3")->second) == true);
    CHECK(std::get<1>(val.find("key3")->second) == 3);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(double, float, string, double) map",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleDoubleFloatStringDoubleMap;
    CHECK(func.name == "returnTupleDoubleFloatStringDoubleMap");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "String -> (Number, Number, String, Number)");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::map<std::string, std::tuple<double, float, std::string, double>> val =
        lua::value<
            std::map<std::string, std::tuple<double, float, std::string, double>>
        >(state);
    REQUIRE(val.size() == 3);
    CHECK(val.find("key1") != val.end());
    CHECK(std::get<0>(val.find("key1")->second) == 1.1);
    CHECK(std::get<1>(val.find("key1")->second) == 2.2f);
    CHECK(std::get<2>(val.find("key1")->second) == "abc");
    CHECK(std::get<3>(val.find("key1")->second) == 3.3);
    CHECK(val.find("key2") != val.end());
    CHECK(std::get<0>(val.find("key2")->second) == 4.4);
    CHECK(std::get<1>(val.find("key2")->second) == 5.5f);
    CHECK(std::get<2>(val.find("key2")->second) == "def");
    CHECK(std::get<3>(val.find("key2")->second) == 6.6);
    CHECK(val.find("key3") != val.end());
    CHECK(std::get<0>(val.find("key3")->second) == 7.7);
    CHECK(std::get<1>(val.find("key3")->second) == 8.8f);
    CHECK(std::get<2>(val.find("key3")->second) == "ghi");
    CHECK(std::get<3>(val.find("key3")->second) == 9.9);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(vec3) vector",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleVec3Vector;
    CHECK(func.name == "returnTupleVec3Vector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(vec3)[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::vector<std::tuple<glm::dvec3>> val =
        lua::value<std::vector<std::tuple<glm::dvec3>>>(state);
    REQUIRE(val.size() == 3);
    CHECK(std::get<0>(val[0]).x == 1.1);
    CHECK(std::get<0>(val[0]).y == 2.2);
    CHECK(std::get<0>(val[0]).z == 3.3);
    CHECK(std::get<0>(val[1]).x == 4.4);
    CHECK(std::get<0>(val[1]).y == 5.5);
    CHECK(std::get<0>(val[1]).z == 6.6);
    CHECK(std::get<0>(val[2]).x == 7.7);
    CHECK(std::get<0>(val[2]).y == 8.8);
    CHECK(std::get<0>(val[2]).z == 9.9);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(bool, int) vector",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleBoolIntVector;
    CHECK(func.name == "returnTupleBoolIntVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(Boolean, Integer)[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::vector<std::tuple<bool, int>> val =
        lua::value<std::vector<std::tuple<bool, int>>>(state);
    REQUIRE(val.size() == 3);
    CHECK(std::get<0>(val[0]) == true);
    CHECK(std::get<1>(val[0]) == 1);
    CHECK(std::get<0>(val[1]) == false);
    CHECK(std::get<1>(val[1]) == 2);
    CHECK(std::get<0>(val[2]) == true);
    CHECK(std::get<1>(val[2]) == 3);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(double, float, string, double) vector",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleDoubleFloatStringDoubleVector;
    CHECK(func.name == "returnTupleDoubleFloatStringDoubleVector");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(Number, Number, String, Number)[]");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::vector<std::tuple<double, float, std::string, double>> val =
        lua::value<std::vector<std::tuple<double, float, std::string, double>>>(
            state
        );
    REQUIRE(val.size() == 3);
    CHECK(std::get<0>(val[0]) == 1.1);
    CHECK(std::get<1>(val[0]) == 2.2f);
    CHECK(std::get<2>(val[0]) == "abc");
    CHECK(std::get<3>(val[0]) == 3.3);
    CHECK(std::get<0>(val[1]) == 4.4);
    CHECK(std::get<1>(val[1]) == 5.5f);
    CHECK(std::get<2>(val[1]) == "def");
    CHECK(std::get<3>(val[1]) == 6.6);
    CHECK(std::get<0>(val[2]) == 7.7);
    CHECK(std::get<1>(val[2]) == 8.8f);
    CHECK(std::get<2>(val[2]) == "ghi");
    CHECK(std::get<3>(val[2]) == 9.9);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(vec3) optional",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleVec3Optional;
    CHECK(func.name == "returnTupleVec3Optional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(vec3)?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::optional<std::tuple<glm::dvec3>> val =
        lua::value<std::optional<std::tuple<glm::dvec3>>>(state);
    REQUIRE(val.has_value());
    CHECK(std::get<0>(*val).x == 1.1);
    CHECK(std::get<0>(*val).y == 2.2);
    CHECK(std::get<0>(*val).z == 3.3);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(bool, int) optional",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleBoolIntOptional;
    CHECK(func.name == "returnTupleBoolIntOptional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(Boolean, Integer)?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::optional<std::tuple<bool, int>> val =
        lua::value<std::optional<std::tuple<bool, int>>>(state);
    REQUIRE(val.has_value());
    CHECK(std::get<0>(*val) == true);
    CHECK(std::get<1>(*val) == 1);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(double, float, string, double) optional",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleDoubleFloatStringDoubleOptional;
    CHECK(func.name == "returnTupleDoubleFloatStringDoubleOptional");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(Number, Number, String, Number)?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 1);
    const std::optional<std::tuple<double, float, std::string, double>> val =
        lua::value<std::optional<std::tuple<double, float, std::string, double>>>(
            state
        );
    REQUIRE(val.has_value());
    CHECK(std::get<0>(*val) == 1.1);
    CHECK(std::get<1>(*val) == 2.2f);
    CHECK(std::get<2>(*val) == "abc");
    CHECK(std::get<3>(*val) == 3.3);
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(vec3) optional nullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleVec3OptionalNullopt;
    CHECK(func.name == "returnTupleVec3OptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(vec3)?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 0);
    const std::optional<std::tuple<glm::dvec3>> val =
        lua::value<std::optional<std::tuple<glm::dvec3>>>(state);
    CHECK(!val.has_value());
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(bool, int) optional nullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func = codegen::lua::ReturnTupleBoolIntOptionalNullopt;
    CHECK(func.name == "returnTupleBoolIntOptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(Boolean, Integer)?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 0);
    const std::optional<std::tuple<bool, int>> val =
        lua::value<std::optional<std::tuple<bool, int>>>(state);
    CHECK(!val.has_value());
    lua_close(state);
}

TEST_CASE(
    "Execution/LuaWrapper/Return:  tuple(double, float, string, double) optional nullopt",
    "[Execution][LuaWrapper]"
)
{
    LuaLibrary::Function func =
        codegen::lua::ReturnTupleDoubleFloatStringDoubleOptionalNullopt;
    CHECK(func.name == "returnTupleDoubleFloatStringDoubleOptionalNullopt");
    CHECK(func.arguments.empty());
    CHECK(func.returnType == "(Number, Number, String, Number)?");
    CHECK(func.helpText.empty());
    REQUIRE(func.function);

    lua_State* state = luaL_newstate();
    REQUIRE(state);
    func.function(state);
    REQUIRE(lua_gettop(state) == 0);
    const std::optional<std::tuple<double, float, std::string, double>> val =
        lua::value<std::optional<std::tuple<double, float, std::string, double>>>(
            state
        );
    CHECK(!val.has_value());
    lua_close(state);
}
