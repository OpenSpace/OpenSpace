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
#include <array>
#include <optional>
#include <vector>

namespace {
    struct [[codegen::Dictionary(Array)]] Parameters {
        // simpleArray documentation
        std::array<std::string, 5> simpleArray;

        // optionalArray documentation
        std::optional<std::array<std::string, 5>> optionalArray;

        // vectorArray documentation
        std::vector<std::array<std::string, 5>> vectorArray;

        // optionalVectorArray documentation
        std::optional<std::vector<std::array<std::string, 5>>> optionalVectorArray;

        // dictArray documentation
        std::array<ghoul::Dictionary, 3> dictArray;

        // optionalDictArray documentation
        std::optional<std::array<ghoul::Dictionary, 3>> optionalDictArray;

        // vectorDictArray documentation
        std::vector<std::array<ghoul::Dictionary, 3>> vectorDictArray;

        // optionalVectorDictArray documentation
        std::optional<std::vector<std::array<ghoul::Dictionary, 3>>> optionalVectorDictArray;
    };
} // namespace
#include "execution_structs_array_codegen.cpp"


TEST_CASE("Execution/Structs/Array:  Bake", "[Execution][Structs]") {
    using namespace std::literals;

    ghoul::Dictionary d;
    {
        ghoul::Dictionary e;
        e.setValue("1", "abc"s);
        e.setValue("2", "def"s);
        e.setValue("3", "ghi"s);
        e.setValue("4", "jkl"s);
        e.setValue("5", "mno"s);
        d.setValue("SimpleArray", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("1", "abc"s);
        e.setValue("2", "def"s);
        e.setValue("3", "ghi"s);
        e.setValue("4", "jkl"s);
        e.setValue("5", "mno"s);
        d.setValue("OptionalArray", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("1", "abc"s);
            f.setValue("2", "def"s);
            f.setValue("3", "ghi"s);
            f.setValue("4", "jkl"s);
            f.setValue("5", "mno"s);
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("1", "pqr"s);
            f.setValue("2", "stu"s);
            f.setValue("3", "vwx"s);
            f.setValue("4", "yza"s);
            f.setValue("5", "bcd"s);
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("1", "efg"s);
            f.setValue("2", "hij"s);
            f.setValue("3", "klm"s);
            f.setValue("4", "nop"s);
            f.setValue("5", "qrs"s);
            e.setValue("3", f);
        }
        d.setValue("VectorArray", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("1", "abc"s);
            f.setValue("2", "def"s);
            f.setValue("3", "ghi"s);
            f.setValue("4", "jkl"s);
            f.setValue("5", "mno"s);
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("1", "pqr"s);
            f.setValue("2", "stu"s);
            f.setValue("3", "vwx"s);
            f.setValue("4", "yza"s);
            f.setValue("5", "bcd"s);
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("1", "efg"s);
            f.setValue("2", "hij"s);
            f.setValue("3", "klm"s);
            f.setValue("4", "nop"s);
            f.setValue("5", "qrs"s);
            e.setValue("3", f);
        }
        d.setValue("OptionalVectorArray", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("A", "1"s);
            f.setValue("B", "2"s);
            f.setValue("C", "3"s);
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("D", "4"s);
            f.setValue("E", "5"s);
            f.setValue("F", "6"s);
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("G", "7"s);
            f.setValue("H", "8"s);
            f.setValue("I", "9"s);
            e.setValue("3", f);
        }
        d.setValue("DictArray", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("A", "1"s);
            f.setValue("B", "2"s);
            f.setValue("C", "3"s);
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("D", "4"s);
            f.setValue("E", "5"s);
            f.setValue("F", "6"s);
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("G", "7"s);
            f.setValue("H", "8"s);
            f.setValue("I", "9"s);
            e.setValue("3", f);
        }
        d.setValue("OptionalDictArray", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            {
                ghoul::Dictionary g;
                g.setValue("A", "1"s);
                g.setValue("B", "2"s);
                g.setValue("C", "3"s);
                f.setValue("1", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("2", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("3", g);
            }
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            {
                ghoul::Dictionary g;
                g.setValue("A", "1"s);
                g.setValue("B", "2"s);
                g.setValue("C", "3"s);
                f.setValue("1", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("2", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("3", g);
            }
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            {
                ghoul::Dictionary g;
                g.setValue("A", "1"s);
                g.setValue("B", "2"s);
                g.setValue("C", "3"s);
                f.setValue("1", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("2", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("3", g);
            }
            e.setValue("3", f);
        }
        d.setValue("VectorDictArray", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            {
                ghoul::Dictionary g;
                g.setValue("A", "1"s);
                g.setValue("B", "2"s);
                g.setValue("C", "3"s);
                f.setValue("1", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("2", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("3", g);
            }
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            {
                ghoul::Dictionary g;
                g.setValue("A", "1"s);
                g.setValue("B", "2"s);
                g.setValue("C", "3"s);
                f.setValue("1", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("2", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("3", g);
            }
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            {
                ghoul::Dictionary g;
                g.setValue("A", "1"s);
                g.setValue("B", "2"s);
                g.setValue("C", "3"s);
                f.setValue("1", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("2", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("3", g);
            }
            e.setValue("3", f);
        }
        d.setValue("OptionalVectorDictArray", e);
    }

    const Parameters p = codegen::bake<Parameters>(d);
    CHECK(p.simpleArray[0] == "abc");
    CHECK(p.simpleArray[1] == "def");
    CHECK(p.simpleArray[2] == "ghi");
    CHECK(p.simpleArray[3] == "jkl");
    CHECK(p.simpleArray[4] == "mno");

    REQUIRE(p.optionalArray.has_value());
    CHECK(p.optionalArray->at(0) == "abc");
    CHECK(p.optionalArray->at(1) == "def");
    CHECK(p.optionalArray->at(2) == "ghi");
    CHECK(p.optionalArray->at(3) == "jkl");
    CHECK(p.optionalArray->at(4) == "mno");

    REQUIRE(p.vectorArray.size() == 3);
    CHECK(p.vectorArray[0][0] == "abc");
    CHECK(p.vectorArray[0][1] == "def");
    CHECK(p.vectorArray[0][2] == "ghi");
    CHECK(p.vectorArray[0][3] == "jkl");
    CHECK(p.vectorArray[0][4] == "mno");
    CHECK(p.vectorArray[1][0] == "pqr");
    CHECK(p.vectorArray[1][1] == "stu");
    CHECK(p.vectorArray[1][2] == "vwx");
    CHECK(p.vectorArray[1][3] == "yza");
    CHECK(p.vectorArray[1][4] == "bcd");
    CHECK(p.vectorArray[2][0] == "efg");
    CHECK(p.vectorArray[2][1] == "hij");
    CHECK(p.vectorArray[2][2] == "klm");
    CHECK(p.vectorArray[2][3] == "nop");
    CHECK(p.vectorArray[2][4] == "qrs");

    REQUIRE(p.optionalVectorArray.has_value());
    REQUIRE(p.optionalVectorArray->size() == 3);
    CHECK(p.optionalVectorArray->at(0)[0] == "abc");
    CHECK(p.optionalVectorArray->at(0)[1] == "def");
    CHECK(p.optionalVectorArray->at(0)[2] == "ghi");
    CHECK(p.optionalVectorArray->at(0)[3] == "jkl");
    CHECK(p.optionalVectorArray->at(0)[4] == "mno");
    CHECK(p.optionalVectorArray->at(1)[0] == "pqr");
    CHECK(p.optionalVectorArray->at(1)[1] == "stu");
    CHECK(p.optionalVectorArray->at(1)[2] == "vwx");
    CHECK(p.optionalVectorArray->at(1)[3] == "yza");
    CHECK(p.optionalVectorArray->at(1)[4] == "bcd");
    CHECK(p.optionalVectorArray->at(2)[0] == "efg");
    CHECK(p.optionalVectorArray->at(2)[1] == "hij");
    CHECK(p.optionalVectorArray->at(2)[2] == "klm");
    CHECK(p.optionalVectorArray->at(2)[3] == "nop");
    CHECK(p.optionalVectorArray->at(2)[4] == "qrs");

    REQUIRE(p.dictArray[0].size() == 3);
    REQUIRE(p.dictArray[0].hasKey("A"));
    REQUIRE(p.dictArray[0].hasValue<std::string>("A"));
    CHECK(p.dictArray[0].value<std::string>("A") == "1"s);
    REQUIRE(p.dictArray[0].hasKey("B"));
    REQUIRE(p.dictArray[0].hasValue<std::string>("B"));
    CHECK(p.dictArray[0].value<std::string>("B") == "2"s);
    REQUIRE(p.dictArray[0].hasKey("C"));
    REQUIRE(p.dictArray[0].hasValue<std::string>("C"));
    CHECK(p.dictArray[0].value<std::string>("C") == "3"s);
    REQUIRE(p.dictArray[1].size() == 3);
    REQUIRE(p.dictArray[1].hasKey("D"));
    REQUIRE(p.dictArray[1].hasValue<std::string>("D"));
    CHECK(p.dictArray[1].value<std::string>("D") == "4"s);
    REQUIRE(p.dictArray[1].hasKey("E"));
    REQUIRE(p.dictArray[1].hasValue<std::string>("E"));
    CHECK(p.dictArray[1].value<std::string>("E") == "5"s);
    REQUIRE(p.dictArray[1].hasKey("F"));
    REQUIRE(p.dictArray[1].hasValue<std::string>("F"));
    CHECK(p.dictArray[1].value<std::string>("F") == "6"s);
    REQUIRE(p.dictArray[2].size() == 3);
    REQUIRE(p.dictArray[2].hasKey("G"));
    REQUIRE(p.dictArray[2].hasValue<std::string>("G"));
    CHECK(p.dictArray[2].value<std::string>("G") == "7"s);
    REQUIRE(p.dictArray[2].hasKey("H"));
    REQUIRE(p.dictArray[2].hasValue<std::string>("H"));
    CHECK(p.dictArray[2].value<std::string>("H") == "8"s);
    REQUIRE(p.dictArray[2].hasKey("I"));
    REQUIRE(p.dictArray[2].hasValue<std::string>("I"));
    CHECK(p.dictArray[2].value<std::string>("I") == "9"s);

    REQUIRE(p.optionalDictArray.has_value());
    REQUIRE(p.optionalDictArray->at(0).size() == 3);
    REQUIRE(p.optionalDictArray->at(0).hasKey("A"));
    REQUIRE(p.optionalDictArray->at(0).hasValue<std::string>("A"));
    CHECK(p.optionalDictArray->at(0).value<std::string>("A") == "1"s);
    REQUIRE(p.optionalDictArray->at(0).hasKey("B"));
    REQUIRE(p.optionalDictArray->at(0).hasValue<std::string>("B"));
    CHECK(p.optionalDictArray->at(0).value<std::string>("B") == "2"s);
    REQUIRE(p.optionalDictArray->at(0).hasKey("C"));
    REQUIRE(p.optionalDictArray->at(0).hasValue<std::string>("C"));
    CHECK(p.optionalDictArray->at(0).value<std::string>("C") == "3"s);
    REQUIRE(p.optionalDictArray->at(1).size() == 3);
    REQUIRE(p.optionalDictArray->at(1).hasKey("D"));
    REQUIRE(p.optionalDictArray->at(1).hasValue<std::string>("D"));
    CHECK(p.optionalDictArray->at(1).value<std::string>("D") == "4"s);
    REQUIRE(p.optionalDictArray->at(1).hasKey("E"));
    REQUIRE(p.optionalDictArray->at(1).hasValue<std::string>("E"));
    CHECK(p.optionalDictArray->at(1).value<std::string>("E") == "5"s);
    REQUIRE(p.optionalDictArray->at(1).hasKey("F"));
    REQUIRE(p.optionalDictArray->at(1).hasValue<std::string>("F"));
    CHECK(p.optionalDictArray->at(1).value<std::string>("F") == "6"s);
    REQUIRE(p.optionalDictArray->at(2).size() == 3);
    REQUIRE(p.optionalDictArray->at(2).hasKey("G"));
    REQUIRE(p.optionalDictArray->at(2).hasValue<std::string>("G"));
    CHECK(p.optionalDictArray->at(2).value<std::string>("G") == "7"s);
    REQUIRE(p.optionalDictArray->at(2).hasKey("H"));
    REQUIRE(p.optionalDictArray->at(2).hasValue<std::string>("H"));
    CHECK(p.optionalDictArray->at(2).value<std::string>("H") == "8"s);
    REQUIRE(p.optionalDictArray->at(2).hasKey("I"));
    REQUIRE(p.optionalDictArray->at(2).hasValue<std::string>("I"));
    CHECK(p.optionalDictArray->at(2).value<std::string>("I") == "9"s);

    REQUIRE(p.vectorDictArray.size() == 3);
    REQUIRE(p.vectorDictArray[0][0].hasKey("A"));
    REQUIRE(p.vectorDictArray[0][0].hasValue<std::string>("A"));
    CHECK(p.vectorDictArray[0][0].value<std::string>("A") == "1"s);
    REQUIRE(p.vectorDictArray[0][0].hasKey("B"));
    REQUIRE(p.vectorDictArray[0][0].hasValue<std::string>("B"));
    CHECK(p.vectorDictArray[0][0].value<std::string>("B") == "2"s);
    REQUIRE(p.vectorDictArray[0][0].hasKey("C"));
    REQUIRE(p.vectorDictArray[0][0].hasValue<std::string>("C"));
    CHECK(p.vectorDictArray[0][0].value<std::string>("C") == "3"s);
    REQUIRE(p.vectorDictArray[0][1].size() == 3);
    REQUIRE(p.vectorDictArray[0][1].hasKey("D"));
    REQUIRE(p.vectorDictArray[0][1].hasValue<std::string>("D"));
    CHECK(p.vectorDictArray[0][1].value<std::string>("D") == "4"s);
    REQUIRE(p.vectorDictArray[0][1].hasKey("E"));
    REQUIRE(p.vectorDictArray[0][1].hasValue<std::string>("E"));
    CHECK(p.vectorDictArray[0][1].value<std::string>("E") == "5"s);
    REQUIRE(p.vectorDictArray[0][1].hasKey("F"));
    REQUIRE(p.vectorDictArray[0][1].hasValue<std::string>("F"));
    CHECK(p.vectorDictArray[0][1].value<std::string>("F") == "6"s);
    REQUIRE(p.vectorDictArray[0][2].size() == 3);
    REQUIRE(p.vectorDictArray[0][2].hasKey("G"));
    REQUIRE(p.vectorDictArray[0][2].hasValue<std::string>("G"));
    CHECK(p.vectorDictArray[0][2].value<std::string>("G") == "7"s);
    REQUIRE(p.vectorDictArray[0][2].hasKey("H"));
    REQUIRE(p.vectorDictArray[0][2].hasValue<std::string>("H"));
    CHECK(p.vectorDictArray[0][2].value<std::string>("H") == "8"s);
    REQUIRE(p.vectorDictArray[0][2].hasKey("I"));
    REQUIRE(p.vectorDictArray[0][2].hasValue<std::string>("I"));
    CHECK(p.vectorDictArray[0][2].value<std::string>("I") == "9"s);
    REQUIRE(p.vectorDictArray[1].size() == 3);
    REQUIRE(p.vectorDictArray[1][0].size() == 3);
    REQUIRE(p.vectorDictArray[1][0].hasKey("A"));
    REQUIRE(p.vectorDictArray[1][0].hasValue<std::string>("A"));
    CHECK(p.vectorDictArray[1][0].value<std::string>("A") == "1"s);
    REQUIRE(p.vectorDictArray[1][0].hasKey("B"));
    REQUIRE(p.vectorDictArray[1][0].hasValue<std::string>("B"));
    CHECK(p.vectorDictArray[1][0].value<std::string>("B") == "2"s);
    REQUIRE(p.vectorDictArray[1][0].hasKey("C"));
    REQUIRE(p.vectorDictArray[1][0].hasValue<std::string>("C"));
    CHECK(p.vectorDictArray[1][0].value<std::string>("C") == "3"s);
    REQUIRE(p.vectorDictArray[1][1].size() == 3);
    REQUIRE(p.vectorDictArray[1][1].hasKey("D"));
    REQUIRE(p.vectorDictArray[1][1].hasValue<std::string>("D"));
    CHECK(p.vectorDictArray[1][1].value<std::string>("D") == "4"s);
    REQUIRE(p.vectorDictArray[1][1].hasKey("E"));
    REQUIRE(p.vectorDictArray[1][1].hasValue<std::string>("E"));
    CHECK(p.vectorDictArray[1][1].value<std::string>("E") == "5"s);
    REQUIRE(p.vectorDictArray[1][1].hasKey("F"));
    REQUIRE(p.vectorDictArray[1][1].hasValue<std::string>("F"));
    CHECK(p.vectorDictArray[1][1].value<std::string>("F") == "6"s);
    REQUIRE(p.vectorDictArray[1][2].size() == 3);
    REQUIRE(p.vectorDictArray[1][2].hasKey("G"));
    REQUIRE(p.vectorDictArray[1][2].hasValue<std::string>("G"));
    CHECK(p.vectorDictArray[1][2].value<std::string>("G") == "7"s);
    REQUIRE(p.vectorDictArray[1][2].hasKey("H"));
    REQUIRE(p.vectorDictArray[1][2].hasValue<std::string>("H"));
    CHECK(p.vectorDictArray[1][2].value<std::string>("H") == "8"s);
    REQUIRE(p.vectorDictArray[1][2].hasKey("I"));
    REQUIRE(p.vectorDictArray[1][2].hasValue<std::string>("I"));
    CHECK(p.vectorDictArray[1][2].value<std::string>("I") == "9"s);
    REQUIRE(p.vectorDictArray[2].size() == 3);
    REQUIRE(p.vectorDictArray[2][0].size() == 3);
    REQUIRE(p.vectorDictArray[2][0].hasKey("A"));
    REQUIRE(p.vectorDictArray[2][0].hasValue<std::string>("A"));
    CHECK(p.vectorDictArray[2][0].value<std::string>("A") == "1"s);
    REQUIRE(p.vectorDictArray[2][0].hasKey("B"));
    REQUIRE(p.vectorDictArray[2][0].hasValue<std::string>("B"));
    CHECK(p.vectorDictArray[2][0].value<std::string>("B") == "2"s);
    REQUIRE(p.vectorDictArray[2][0].hasKey("C"));
    REQUIRE(p.vectorDictArray[2][0].hasValue<std::string>("C"));
    CHECK(p.vectorDictArray[2][0].value<std::string>("C") == "3"s);
    REQUIRE(p.vectorDictArray[2][1].size() == 3);
    REQUIRE(p.vectorDictArray[2][1].hasKey("D"));
    REQUIRE(p.vectorDictArray[2][1].hasValue<std::string>("D"));
    CHECK(p.vectorDictArray[2][1].value<std::string>("D") == "4"s);
    REQUIRE(p.vectorDictArray[2][1].hasKey("E"));
    REQUIRE(p.vectorDictArray[2][1].hasValue<std::string>("E"));
    CHECK(p.vectorDictArray[2][1].value<std::string>("E") == "5"s);
    REQUIRE(p.vectorDictArray[2][1].hasKey("F"));
    REQUIRE(p.vectorDictArray[2][1].hasValue<std::string>("F"));
    CHECK(p.vectorDictArray[2][1].value<std::string>("F") == "6"s);
    REQUIRE(p.vectorDictArray[2][2].size() == 3);
    REQUIRE(p.vectorDictArray[2][2].hasKey("G"));
    REQUIRE(p.vectorDictArray[2][2].hasValue<std::string>("G"));
    CHECK(p.vectorDictArray[2][2].value<std::string>("G") == "7"s);
    REQUIRE(p.vectorDictArray[2][2].hasKey("H"));
    REQUIRE(p.vectorDictArray[2][2].hasValue<std::string>("H"));
    CHECK(p.vectorDictArray[2][2].value<std::string>("H") == "8"s);
    REQUIRE(p.vectorDictArray[2][2].hasKey("I"));
    REQUIRE(p.vectorDictArray[2][2].hasValue<std::string>("I"));
    CHECK(p.vectorDictArray[2][2].value<std::string>("I") == "9"s);

    REQUIRE(p.optionalVectorDictArray.has_value());
    REQUIRE(p.optionalVectorDictArray->size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(0)[0].hasKey("A"));
    REQUIRE(p.optionalVectorDictArray->at(0)[0].hasValue<std::string>("A"));
    CHECK(p.optionalVectorDictArray->at(0)[0].value<std::string>("A") == "1"s);
    REQUIRE(p.optionalVectorDictArray->at(0)[0].hasKey("B"));
    REQUIRE(p.optionalVectorDictArray->at(0)[0].hasValue<std::string>("B"));
    CHECK(p.optionalVectorDictArray->at(0)[0].value<std::string>("B") == "2"s);
    REQUIRE(p.optionalVectorDictArray->at(0)[0].hasKey("C"));
    REQUIRE(p.optionalVectorDictArray->at(0)[0].hasValue<std::string>("C"));
    CHECK(p.optionalVectorDictArray->at(0)[0].value<std::string>("C") == "3"s);
    REQUIRE(p.optionalVectorDictArray->at(0)[1].size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(0)[1].hasKey("D"));
    REQUIRE(p.optionalVectorDictArray->at(0)[1].hasValue<std::string>("D"));
    CHECK(p.optionalVectorDictArray->at(0)[1].value<std::string>("D") == "4"s);
    REQUIRE(p.optionalVectorDictArray->at(0)[1].hasKey("E"));
    REQUIRE(p.optionalVectorDictArray->at(0)[1].hasValue<std::string>("E"));
    CHECK(p.optionalVectorDictArray->at(0)[1].value<std::string>("E") == "5"s);
    REQUIRE(p.optionalVectorDictArray->at(0)[1].hasKey("F"));
    REQUIRE(p.optionalVectorDictArray->at(0)[1].hasValue<std::string>("F"));
    CHECK(p.optionalVectorDictArray->at(0)[1].value<std::string>("F") == "6"s);
    REQUIRE(p.optionalVectorDictArray->at(0)[2].size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(0)[2].hasKey("G"));
    REQUIRE(p.optionalVectorDictArray->at(0)[2].hasValue<std::string>("G"));
    CHECK(p.optionalVectorDictArray->at(0)[2].value<std::string>("G") == "7"s);
    REQUIRE(p.optionalVectorDictArray->at(0)[2].hasKey("H"));
    REQUIRE(p.optionalVectorDictArray->at(0)[2].hasValue<std::string>("H"));
    CHECK(p.optionalVectorDictArray->at(0)[2].value<std::string>("H") == "8"s);
    REQUIRE(p.optionalVectorDictArray->at(0)[2].hasKey("I"));
    REQUIRE(p.optionalVectorDictArray->at(0)[2].hasValue<std::string>("I"));
    CHECK(p.optionalVectorDictArray->at(0)[2].value<std::string>("I") == "9"s);
    REQUIRE(p.optionalVectorDictArray->at(1).size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(1)[0].size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(1)[0].hasKey("A"));
    REQUIRE(p.optionalVectorDictArray->at(1)[0].hasValue<std::string>("A"));
    CHECK(p.optionalVectorDictArray->at(1)[0].value<std::string>("A") == "1"s);
    REQUIRE(p.optionalVectorDictArray->at(1)[0].hasKey("B"));
    REQUIRE(p.optionalVectorDictArray->at(1)[0].hasValue<std::string>("B"));
    CHECK(p.optionalVectorDictArray->at(1)[0].value<std::string>("B") == "2"s);
    REQUIRE(p.optionalVectorDictArray->at(1)[0].hasKey("C"));
    REQUIRE(p.optionalVectorDictArray->at(1)[0].hasValue<std::string>("C"));
    CHECK(p.optionalVectorDictArray->at(1)[0].value<std::string>("C") == "3"s);
    REQUIRE(p.optionalVectorDictArray->at(1)[1].size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(1)[1].hasKey("D"));
    REQUIRE(p.optionalVectorDictArray->at(1)[1].hasValue<std::string>("D"));
    CHECK(p.optionalVectorDictArray->at(1)[1].value<std::string>("D") == "4"s);
    REQUIRE(p.optionalVectorDictArray->at(1)[1].hasKey("E"));
    REQUIRE(p.optionalVectorDictArray->at(1)[1].hasValue<std::string>("E"));
    CHECK(p.optionalVectorDictArray->at(1)[1].value<std::string>("E") == "5"s);
    REQUIRE(p.optionalVectorDictArray->at(1)[1].hasKey("F"));
    REQUIRE(p.optionalVectorDictArray->at(1)[1].hasValue<std::string>("F"));
    CHECK(p.optionalVectorDictArray->at(1)[1].value<std::string>("F") == "6"s);
    REQUIRE(p.optionalVectorDictArray->at(1)[2].size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(1)[2].hasKey("G"));
    REQUIRE(p.optionalVectorDictArray->at(1)[2].hasValue<std::string>("G"));
    CHECK(p.optionalVectorDictArray->at(1)[2].value<std::string>("G") == "7"s);
    REQUIRE(p.optionalVectorDictArray->at(1)[2].hasKey("H"));
    REQUIRE(p.optionalVectorDictArray->at(1)[2].hasValue<std::string>("H"));
    CHECK(p.optionalVectorDictArray->at(1)[2].value<std::string>("H") == "8"s);
    REQUIRE(p.optionalVectorDictArray->at(1)[2].hasKey("I"));
    REQUIRE(p.optionalVectorDictArray->at(1)[2].hasValue<std::string>("I"));
    CHECK(p.optionalVectorDictArray->at(1)[2].value<std::string>("I") == "9"s);
    REQUIRE(p.optionalVectorDictArray->at(2).size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(2)[0].size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(2)[0].hasKey("A"));
    REQUIRE(p.optionalVectorDictArray->at(2)[0].hasValue<std::string>("A"));
    CHECK(p.optionalVectorDictArray->at(2)[0].value<std::string>("A") == "1"s);
    REQUIRE(p.optionalVectorDictArray->at(2)[0].hasKey("B"));
    REQUIRE(p.optionalVectorDictArray->at(2)[0].hasValue<std::string>("B"));
    CHECK(p.optionalVectorDictArray->at(2)[0].value<std::string>("B") == "2"s);
    REQUIRE(p.optionalVectorDictArray->at(2)[0].hasKey("C"));
    REQUIRE(p.optionalVectorDictArray->at(2)[0].hasValue<std::string>("C"));
    CHECK(p.optionalVectorDictArray->at(2)[0].value<std::string>("C") == "3"s);
    REQUIRE(p.optionalVectorDictArray->at(2)[1].size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(2)[1].hasKey("D"));
    REQUIRE(p.optionalVectorDictArray->at(2)[1].hasValue<std::string>("D"));
    CHECK(p.optionalVectorDictArray->at(2)[1].value<std::string>("D") == "4"s);
    REQUIRE(p.optionalVectorDictArray->at(2)[1].hasKey("E"));
    REQUIRE(p.optionalVectorDictArray->at(2)[1].hasValue<std::string>("E"));
    CHECK(p.optionalVectorDictArray->at(2)[1].value<std::string>("E") == "5"s);
    REQUIRE(p.optionalVectorDictArray->at(2)[1].hasKey("F"));
    REQUIRE(p.optionalVectorDictArray->at(2)[1].hasValue<std::string>("F"));
    CHECK(p.optionalVectorDictArray->at(2)[1].value<std::string>("F") == "6"s);
    REQUIRE(p.optionalVectorDictArray->at(2)[2].size() == 3);
    REQUIRE(p.optionalVectorDictArray->at(2)[2].hasKey("G"));
    REQUIRE(p.optionalVectorDictArray->at(2)[2].hasValue<std::string>("G"));
    CHECK(p.optionalVectorDictArray->at(2)[2].value<std::string>("G") == "7"s);
    REQUIRE(p.optionalVectorDictArray->at(2)[2].hasKey("H"));
    REQUIRE(p.optionalVectorDictArray->at(2)[2].hasValue<std::string>("H"));
    CHECK(p.optionalVectorDictArray->at(2)[2].value<std::string>("H") == "8"s);
    REQUIRE(p.optionalVectorDictArray->at(2)[2].hasKey("I"));
    REQUIRE(p.optionalVectorDictArray->at(2)[2].hasValue<std::string>("I"));
    CHECK(p.optionalVectorDictArray->at(2)[2].value<std::string>("I") == "9"s);
}

TEST_CASE("Execution/Structs/Array:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 8);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "SimpleArray");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "simpleArray documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->count == 5);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "String");
        StringVerifier* w =
            dynamic_cast<StringVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "OptionalArray");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalArray documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->count == 5);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "String");
        StringVerifier* w =
            dynamic_cast<StringVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[2];
        CHECK(e.key == "VectorArray");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorArray documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w =
            dynamic_cast<TableVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);

        CHECK(w->count == 5);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].documentation.empty());
        CHECK(w->documentations[0].verifier->type() == "String");
        StringVerifier* u =
            dynamic_cast<StringVerifier*>(w->documentations[0].verifier.get());
        REQUIRE(u);
        CHECK(u->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[3];
        CHECK(e.key == "OptionalVectorArray");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalVectorArray documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w =
            dynamic_cast<TableVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);

        CHECK(w->count == 5);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].documentation.empty());
        CHECK(w->documentations[0].verifier->type() == "String");
        StringVerifier* u =
            dynamic_cast<StringVerifier*>(w->documentations[0].verifier.get());
        REQUIRE(u);
        CHECK(u->mustBeNotEmpty() == false);
    }
    {
        const DocumentationEntry& e = doc.entries[4];
        CHECK(e.key == "DictArray");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dictArray documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->count == 3);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "Table");
        REQUIRE(dynamic_cast<TableVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[5];
        CHECK(e.key == "OptionalDictArray");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalDictArray documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        CHECK(v->count == 3);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "Table");
        REQUIRE(dynamic_cast<TableVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[6];
        CHECK(e.key == "VectorDictArray");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorDictArray documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(
            v->documentations[0].verifier.get()
        );
        REQUIRE(w);
        CHECK(w->count == 3);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].documentation.empty());
        CHECK(w->documentations[0].verifier->type() == "Table");
        REQUIRE(dynamic_cast<TableVerifier*>(w->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[7];
        CHECK(e.key == "OptionalVectorDictArray");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalVectorDictArray documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(
            v->documentations[0].verifier.get()
        );
        REQUIRE(w);
        CHECK(w->count == 3);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].documentation.empty());
        CHECK(w->documentations[0].verifier->type() == "Table");
        REQUIRE(dynamic_cast<TableVerifier*>(w->documentations[0].verifier.get()));
    }
}
