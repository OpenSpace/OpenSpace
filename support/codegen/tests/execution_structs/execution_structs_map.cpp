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
#include <map>
#include <vector>

namespace {
    struct [[codegen::Dictionary(Map)]] Parameters {
        // simpleMap documentation
        std::map<std::string, std::string> simpleMap;

        // optionalMap documentation
        std::optional<std::map<std::string, std::string>> optionalMap;

        // vectorMap documentation
        std::vector<std::map<std::string, std::string>> vectorMap;

        // optionalVectorMap documentation
        std::optional<std::vector<std::map<std::string, std::string>>> optionalVectorMap;

        // dictMap documentation
        std::map<std::string, ghoul::Dictionary> dictMap;

        // optionalDictMap documentation
        std::optional<std::map<std::string, ghoul::Dictionary>> optionalDictMap;

        // vectorDictMap documentation
        std::vector<std::map<std::string, ghoul::Dictionary>> vectorDictMap;

        // optionalVectorDictMap documentation
        std::optional<std::vector<std::map<std::string, ghoul::Dictionary>>> optionalVectorDictMap;
    };
} // namespace
#include "execution_structs_map_codegen.cpp"


TEST_CASE("Execution/Structs/Map String Key:  Bake", "[Execution][Structs]") {
    using namespace std::literals;

    ghoul::Dictionary d;
    {
        ghoul::Dictionary e;
        e.setValue("a", "1"s);
        e.setValue("b", "2"s);
        e.setValue("c", "3"s);
        d.setValue("SimpleMap", e);
    }
    {
        ghoul::Dictionary e;
        e.setValue("d", "4"s);
        e.setValue("e", "5"s);
        e.setValue("f", "6"s);
        d.setValue("OptionalMap", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("g", "7"s);
            f.setValue("h", "8"s);
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("i", "9"s);
            f.setValue("j", "10"s);
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("k", "11"s);
            f.setValue("l", "12"s);
            e.setValue("3", f);
        }
        d.setValue("VectorMap", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("m", "13"s);
            f.setValue("n", "14"s);
            e.setValue("1", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("o", "15"s);
            f.setValue("p", "16"s);
            e.setValue("2", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("q", "17"s);
            f.setValue("r", "18"s);
            e.setValue("3", f);
        }
        d.setValue("OptionalVectorMap", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("A", "1"s);
            f.setValue("B", "2"s);
            f.setValue("C", "3"s);
            e.setValue("a", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("D", "4"s);
            f.setValue("E", "5"s);
            f.setValue("F", "6"s);
            e.setValue("b", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("G", "7"s);
            f.setValue("H", "8"s);
            f.setValue("I", "9"s);
            e.setValue("c", f);
        }
        d.setValue("DictMap", e);
    }
    {
        ghoul::Dictionary e;
        {
            ghoul::Dictionary f;
            f.setValue("A", "1"s);
            f.setValue("B", "2"s);
            f.setValue("C", "3"s);
            e.setValue("a", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("D", "4"s);
            f.setValue("E", "5"s);
            f.setValue("F", "6"s);
            e.setValue("b", f);
        }
        {
            ghoul::Dictionary f;
            f.setValue("G", "7"s);
            f.setValue("H", "8"s);
            f.setValue("I", "9"s);
            e.setValue("c", f);
        }
        d.setValue("OptionalDictMap", e);
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
                f.setValue("a", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("b", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("c", g);
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
                f.setValue("a", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("b", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("c", g);
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
                f.setValue("a", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("b", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("c", g);
            }
            e.setValue("3", f);
        }
        d.setValue("VectorDictMap", e);
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
                f.setValue("a", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("b", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("c", g);
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
                f.setValue("a", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("b", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("c", g);
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
                f.setValue("a", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("b", g);
            }
            {
                ghoul::Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("c", g);
            }
            e.setValue("3", f);
        }
        d.setValue("OptionalVectorDictMap", e);
    }

    const Parameters p = codegen::bake<Parameters>(d);
    REQUIRE(p.simpleMap.size() == 3);
    REQUIRE(p.simpleMap.find("a") != p.simpleMap.end());
    CHECK(p.simpleMap.at("a") == "1");
    REQUIRE(p.simpleMap.find("b") != p.simpleMap.end());
    CHECK(p.simpleMap.at("b") == "2");
    REQUIRE(p.simpleMap.find("c") != p.simpleMap.end());
    CHECK(p.simpleMap.at("c") == "3");

    REQUIRE(p.optionalMap.has_value());
    REQUIRE(p.optionalMap->size() == 3);
    REQUIRE(p.optionalMap->find("d") != p.optionalMap->end());
    CHECK(p.optionalMap->at("d") == "4");
    REQUIRE(p.optionalMap->find("e") != p.optionalMap->end());
    CHECK(p.optionalMap->at("e") == "5");
    REQUIRE(p.optionalMap->find("f") != p.optionalMap->end());
    CHECK(p.optionalMap->at("f") == "6");

    REQUIRE(p.vectorMap.size() == 3);
    REQUIRE(p.vectorMap[0].find("g") != p.vectorMap[0].end());
    CHECK(p.vectorMap[0].at("g") == "7");
    REQUIRE(p.vectorMap[0].find("h") != p.vectorMap[0].end());
    CHECK(p.vectorMap[0].at("h") == "8");

    REQUIRE(p.vectorMap[1].find("i") != p.vectorMap[1].end());
    CHECK(p.vectorMap[1].at("i") == "9");
    REQUIRE(p.vectorMap[1].find("j") != p.vectorMap[1].end());
    CHECK(p.vectorMap[1].at("j") == "10");

    REQUIRE(p.vectorMap[2].find("k") != p.vectorMap[2].end());
    CHECK(p.vectorMap[2].at("k") == "11");
    REQUIRE(p.vectorMap[2].find("l") != p.vectorMap[2].end());
    CHECK(p.vectorMap[2].at("l") == "12");

    REQUIRE(p.optionalVectorMap.has_value());
    REQUIRE(p.optionalVectorMap->size() == 3);
    REQUIRE(p.optionalVectorMap->at(0).find("m") != p.optionalVectorMap->at(0).end());
    CHECK(p.optionalVectorMap->at(0).at("m") == "13");
    REQUIRE(p.optionalVectorMap->at(0).find("n") != p.optionalVectorMap->at(0).end());
    CHECK(p.optionalVectorMap->at(0).at("n") == "14");

    REQUIRE(p.optionalVectorMap->at(1).find("o") != p.optionalVectorMap->at(1).end());
    CHECK(p.optionalVectorMap->at(1).at("o") == "15");
    REQUIRE(p.optionalVectorMap->at(1).find("p") != p.optionalVectorMap->at(1).end());
    CHECK(p.optionalVectorMap->at(1).at("p") == "16");

    REQUIRE(p.optionalVectorMap->at(2).find("q") != p.optionalVectorMap->at(2).end());
    CHECK(p.optionalVectorMap->at(2).at("q") == "17");
    REQUIRE(p.optionalVectorMap->at(2).find("r") != p.optionalVectorMap->at(2).end());
    CHECK(p.optionalVectorMap->at(2).at("r") == "18");

    REQUIRE(p.dictMap.size() == 3);
    REQUIRE(p.dictMap.find("a") != p.dictMap.end());
    REQUIRE(p.dictMap.at("a").size() == 3);
    REQUIRE(p.dictMap.at("a").hasKey("A"));
    REQUIRE(p.dictMap.at("a").hasValue<std::string>("A"));
    REQUIRE(p.dictMap.at("a").value<std::string>("A") == "1"s);
    REQUIRE(p.dictMap.at("a").hasKey("B"));
    REQUIRE(p.dictMap.at("a").hasValue<std::string>("B"));
    REQUIRE(p.dictMap.at("a").value<std::string>("B") == "2"s);
    REQUIRE(p.dictMap.at("a").hasKey("C"));
    REQUIRE(p.dictMap.at("a").hasValue<std::string>("C"));
    REQUIRE(p.dictMap.at("a").value<std::string>("C") == "3"s);
    REQUIRE(p.dictMap.at("b").size() == 3);
    REQUIRE(p.dictMap.at("b").hasKey("D"));
    REQUIRE(p.dictMap.at("b").hasValue<std::string>("D"));
    REQUIRE(p.dictMap.at("b").value<std::string>("D") == "4"s);
    REQUIRE(p.dictMap.at("b").hasKey("E"));
    REQUIRE(p.dictMap.at("b").hasValue<std::string>("E"));
    REQUIRE(p.dictMap.at("b").value<std::string>("E") == "5"s);
    REQUIRE(p.dictMap.at("b").hasKey("F"));
    REQUIRE(p.dictMap.at("b").hasValue<std::string>("F"));
    REQUIRE(p.dictMap.at("b").value<std::string>("F") == "6"s);
    REQUIRE(p.dictMap.at("c").size() == 3);
    REQUIRE(p.dictMap.at("c").hasKey("G"));
    REQUIRE(p.dictMap.at("c").hasValue<std::string>("G"));
    REQUIRE(p.dictMap.at("c").value<std::string>("G") == "7"s);
    REQUIRE(p.dictMap.at("c").hasKey("H"));
    REQUIRE(p.dictMap.at("c").hasValue<std::string>("H"));
    REQUIRE(p.dictMap.at("c").value<std::string>("H") == "8"s);
    REQUIRE(p.dictMap.at("c").hasKey("I"));
    REQUIRE(p.dictMap.at("c").hasValue<std::string>("I"));
    REQUIRE(p.dictMap.at("c").value<std::string>("I") == "9"s);

    REQUIRE(p.optionalDictMap.has_value());
    REQUIRE(p.optionalDictMap->size() == 3);
    REQUIRE(p.optionalDictMap->find("a") != p.optionalDictMap->end());
    REQUIRE(p.optionalDictMap->at("a").size() == 3);
    REQUIRE(p.optionalDictMap->at("a").hasKey("A"));
    REQUIRE(p.optionalDictMap->at("a").hasValue<std::string>("A"));
    REQUIRE(p.optionalDictMap->at("a").value<std::string>("A") == "1"s);
    REQUIRE(p.optionalDictMap->at("a").hasKey("B"));
    REQUIRE(p.optionalDictMap->at("a").hasValue<std::string>("B"));
    REQUIRE(p.optionalDictMap->at("a").value<std::string>("B") == "2"s);
    REQUIRE(p.optionalDictMap->at("a").hasKey("C"));
    REQUIRE(p.optionalDictMap->at("a").hasValue<std::string>("C"));
    REQUIRE(p.optionalDictMap->at("a").value<std::string>("C") == "3"s);
    REQUIRE(p.optionalDictMap->at("b").size() == 3);
    REQUIRE(p.optionalDictMap->at("b").hasKey("D"));
    REQUIRE(p.optionalDictMap->at("b").hasValue<std::string>("D"));
    REQUIRE(p.optionalDictMap->at("b").value<std::string>("D") == "4"s);
    REQUIRE(p.optionalDictMap->at("b").hasKey("E"));
    REQUIRE(p.optionalDictMap->at("b").hasValue<std::string>("E"));
    REQUIRE(p.optionalDictMap->at("b").value<std::string>("E") == "5"s);
    REQUIRE(p.optionalDictMap->at("b").hasKey("F"));
    REQUIRE(p.optionalDictMap->at("b").hasValue<std::string>("F"));
    REQUIRE(p.optionalDictMap->at("b").value<std::string>("F") == "6"s);
    REQUIRE(p.optionalDictMap->at("c").size() == 3);
    REQUIRE(p.optionalDictMap->at("c").hasKey("G"));
    REQUIRE(p.optionalDictMap->at("c").hasValue<std::string>("G"));
    REQUIRE(p.optionalDictMap->at("c").value<std::string>("G") == "7"s);
    REQUIRE(p.optionalDictMap->at("c").hasKey("H"));
    REQUIRE(p.optionalDictMap->at("c").hasValue<std::string>("H"));
    REQUIRE(p.optionalDictMap->at("c").value<std::string>("H") == "8"s);
    REQUIRE(p.optionalDictMap->at("c").hasKey("I"));
    REQUIRE(p.optionalDictMap->at("c").hasValue<std::string>("I"));
    REQUIRE(p.optionalDictMap->at("c").value<std::string>("I") == "9"s);

    REQUIRE(p.vectorDictMap.size() == 3);
    REQUIRE(p.vectorDictMap[0].size() == 3);
    REQUIRE(p.vectorDictMap[0].find("a") != p.vectorDictMap[0].end());
    REQUIRE(p.vectorDictMap[0].at("a").size() == 3);
    REQUIRE(p.vectorDictMap[0].at("a").hasKey("A"));
    REQUIRE(p.vectorDictMap[0].at("a").hasValue<std::string>("A"));
    REQUIRE(p.vectorDictMap[0].at("a").value<std::string>("A") == "1"s);
    REQUIRE(p.vectorDictMap[0].at("a").hasKey("B"));
    REQUIRE(p.vectorDictMap[0].at("a").hasValue<std::string>("B"));
    REQUIRE(p.vectorDictMap[0].at("a").value<std::string>("B") == "2"s);
    REQUIRE(p.vectorDictMap[0].at("a").hasKey("C"));
    REQUIRE(p.vectorDictMap[0].at("a").hasValue<std::string>("C"));
    REQUIRE(p.vectorDictMap[0].at("a").value<std::string>("C") == "3"s);
    REQUIRE(p.vectorDictMap[0].at("b").size() == 3);
    REQUIRE(p.vectorDictMap[0].at("b").hasKey("D"));
    REQUIRE(p.vectorDictMap[0].at("b").hasValue<std::string>("D"));
    REQUIRE(p.vectorDictMap[0].at("b").value<std::string>("D") == "4"s);
    REQUIRE(p.vectorDictMap[0].at("b").hasKey("E"));
    REQUIRE(p.vectorDictMap[0].at("b").hasValue<std::string>("E"));
    REQUIRE(p.vectorDictMap[0].at("b").value<std::string>("E") == "5"s);
    REQUIRE(p.vectorDictMap[0].at("b").hasKey("F"));
    REQUIRE(p.vectorDictMap[0].at("b").hasValue<std::string>("F"));
    REQUIRE(p.vectorDictMap[0].at("b").value<std::string>("F") == "6"s);
    REQUIRE(p.vectorDictMap[0].at("c").size() == 3);
    REQUIRE(p.vectorDictMap[0].at("c").hasKey("G"));
    REQUIRE(p.vectorDictMap[0].at("c").hasValue<std::string>("G"));
    REQUIRE(p.vectorDictMap[0].at("c").value<std::string>("G") == "7"s);
    REQUIRE(p.vectorDictMap[0].at("c").hasKey("H"));
    REQUIRE(p.vectorDictMap[0].at("c").hasValue<std::string>("H"));
    REQUIRE(p.vectorDictMap[0].at("c").value<std::string>("H") == "8"s);
    REQUIRE(p.vectorDictMap[0].at("c").hasKey("I"));
    REQUIRE(p.vectorDictMap[0].at("c").hasValue<std::string>("I"));
    REQUIRE(p.vectorDictMap[0].at("c").value<std::string>("I") == "9"s);
    REQUIRE(p.vectorDictMap[1].size() == 3);
    REQUIRE(p.vectorDictMap[1].find("a") != p.vectorDictMap[1].end());
    REQUIRE(p.vectorDictMap[1].at("a").size() == 3);
    REQUIRE(p.vectorDictMap[1].at("a").hasKey("A"));
    REQUIRE(p.vectorDictMap[1].at("a").hasValue<std::string>("A"));
    REQUIRE(p.vectorDictMap[1].at("a").value<std::string>("A") == "1"s);
    REQUIRE(p.vectorDictMap[1].at("a").hasKey("B"));
    REQUIRE(p.vectorDictMap[1].at("a").hasValue<std::string>("B"));
    REQUIRE(p.vectorDictMap[1].at("a").value<std::string>("B") == "2"s);
    REQUIRE(p.vectorDictMap[1].at("a").hasKey("C"));
    REQUIRE(p.vectorDictMap[1].at("a").hasValue<std::string>("C"));
    REQUIRE(p.vectorDictMap[1].at("a").value<std::string>("C") == "3"s);
    REQUIRE(p.vectorDictMap[1].at("b").size() == 3);
    REQUIRE(p.vectorDictMap[1].at("b").hasKey("D"));
    REQUIRE(p.vectorDictMap[1].at("b").hasValue<std::string>("D"));
    REQUIRE(p.vectorDictMap[1].at("b").value<std::string>("D") == "4"s);
    REQUIRE(p.vectorDictMap[1].at("b").hasKey("E"));
    REQUIRE(p.vectorDictMap[1].at("b").hasValue<std::string>("E"));
    REQUIRE(p.vectorDictMap[1].at("b").value<std::string>("E") == "5"s);
    REQUIRE(p.vectorDictMap[1].at("b").hasKey("F"));
    REQUIRE(p.vectorDictMap[1].at("b").hasValue<std::string>("F"));
    REQUIRE(p.vectorDictMap[1].at("b").value<std::string>("F") == "6"s);
    REQUIRE(p.vectorDictMap[1].at("c").size() == 3);
    REQUIRE(p.vectorDictMap[1].at("c").hasKey("G"));
    REQUIRE(p.vectorDictMap[1].at("c").hasValue<std::string>("G"));
    REQUIRE(p.vectorDictMap[1].at("c").value<std::string>("G") == "7"s);
    REQUIRE(p.vectorDictMap[1].at("c").hasKey("H"));
    REQUIRE(p.vectorDictMap[1].at("c").hasValue<std::string>("H"));
    REQUIRE(p.vectorDictMap[1].at("c").value<std::string>("H") == "8"s);
    REQUIRE(p.vectorDictMap[1].at("c").hasKey("I"));
    REQUIRE(p.vectorDictMap[1].at("c").hasValue<std::string>("I"));
    REQUIRE(p.vectorDictMap[1].at("c").value<std::string>("I") == "9"s);
    REQUIRE(p.vectorDictMap[2].size() == 3);
    REQUIRE(p.vectorDictMap[2].find("a") != p.vectorDictMap[2].end());
    REQUIRE(p.vectorDictMap[2].at("a").size() == 3);
    REQUIRE(p.vectorDictMap[2].at("a").hasKey("A"));
    REQUIRE(p.vectorDictMap[2].at("a").hasValue<std::string>("A"));
    REQUIRE(p.vectorDictMap[2].at("a").value<std::string>("A") == "1"s);
    REQUIRE(p.vectorDictMap[2].at("a").hasKey("B"));
    REQUIRE(p.vectorDictMap[2].at("a").hasValue<std::string>("B"));
    REQUIRE(p.vectorDictMap[2].at("a").value<std::string>("B") == "2"s);
    REQUIRE(p.vectorDictMap[2].at("a").hasKey("C"));
    REQUIRE(p.vectorDictMap[2].at("a").hasValue<std::string>("C"));
    REQUIRE(p.vectorDictMap[2].at("a").value<std::string>("C") == "3"s);
    REQUIRE(p.vectorDictMap[2].at("b").size() == 3);
    REQUIRE(p.vectorDictMap[2].at("b").hasKey("D"));
    REQUIRE(p.vectorDictMap[2].at("b").hasValue<std::string>("D"));
    REQUIRE(p.vectorDictMap[2].at("b").value<std::string>("D") == "4"s);
    REQUIRE(p.vectorDictMap[2].at("b").hasKey("E"));
    REQUIRE(p.vectorDictMap[2].at("b").hasValue<std::string>("E"));
    REQUIRE(p.vectorDictMap[2].at("b").value<std::string>("E") == "5"s);
    REQUIRE(p.vectorDictMap[2].at("b").hasKey("F"));
    REQUIRE(p.vectorDictMap[2].at("b").hasValue<std::string>("F"));
    REQUIRE(p.vectorDictMap[2].at("b").value<std::string>("F") == "6"s);
    REQUIRE(p.vectorDictMap[2].at("c").size() == 3);
    REQUIRE(p.vectorDictMap[2].at("c").hasKey("G"));
    REQUIRE(p.vectorDictMap[2].at("c").hasValue<std::string>("G"));
    REQUIRE(p.vectorDictMap[2].at("c").value<std::string>("G") == "7"s);
    REQUIRE(p.vectorDictMap[2].at("c").hasKey("H"));
    REQUIRE(p.vectorDictMap[2].at("c").hasValue<std::string>("H"));
    REQUIRE(p.vectorDictMap[2].at("c").value<std::string>("H") == "8"s);
    REQUIRE(p.vectorDictMap[2].at("c").hasKey("I"));
    REQUIRE(p.vectorDictMap[2].at("c").hasValue<std::string>("I"));
    REQUIRE(p.vectorDictMap[2].at("c").value<std::string>("I") == "9"s);

    REQUIRE(p.optionalVectorDictMap.has_value());
    REQUIRE(p.optionalVectorDictMap->size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).find("a") != p.optionalVectorDictMap->at(0).end());
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").hasKey("A"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").hasValue<std::string>("A"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").value<std::string>("A") == "1"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").hasKey("B"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").hasValue<std::string>("B"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").value<std::string>("B") == "2"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").hasKey("C"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").hasValue<std::string>("C"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("a").value<std::string>("C") == "3"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").hasKey("D"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").hasValue<std::string>("D"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").value<std::string>("D") == "4"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").hasKey("E"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").hasValue<std::string>("E"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").value<std::string>("E") == "5"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").hasKey("F"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").hasValue<std::string>("F"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("b").value<std::string>("F") == "6"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").hasKey("G"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").hasValue<std::string>("G"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").value<std::string>("G") == "7"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").hasKey("H"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").hasValue<std::string>("H"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").value<std::string>("H") == "8"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").hasKey("I"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").hasValue<std::string>("I"));
    REQUIRE(p.optionalVectorDictMap->at(0).at("c").value<std::string>("I") == "9"s);
    REQUIRE(p.optionalVectorDictMap->at(1).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(1).find("a") != p.optionalVectorDictMap->at(1).end());
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").hasKey("A"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").hasValue<std::string>("A"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").value<std::string>("A") == "1"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").hasKey("B"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").hasValue<std::string>("B"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").value<std::string>("B") == "2"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").hasKey("C"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").hasValue<std::string>("C"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("a").value<std::string>("C") == "3"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").hasKey("D"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").hasValue<std::string>("D"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").value<std::string>("D") == "4"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").hasKey("E"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").hasValue<std::string>("E"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").value<std::string>("E") == "5"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").hasKey("F"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").hasValue<std::string>("F"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("b").value<std::string>("F") == "6"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").hasKey("G"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").hasValue<std::string>("G"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").value<std::string>("G") == "7"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").hasKey("H"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").hasValue<std::string>("H"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").value<std::string>("H") == "8"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").hasKey("I"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").hasValue<std::string>("I"));
    REQUIRE(p.optionalVectorDictMap->at(1).at("c").value<std::string>("I") == "9"s);
    REQUIRE(p.optionalVectorDictMap->at(2).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(2).find("a") != p.optionalVectorDictMap->at(2).end());
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").hasKey("A"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").hasValue<std::string>("A"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").value<std::string>("A") == "1"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").hasKey("B"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").hasValue<std::string>("B"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").value<std::string>("B") == "2"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").hasKey("C"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").hasValue<std::string>("C"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("a").value<std::string>("C") == "3"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").hasKey("D"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").hasValue<std::string>("D"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").value<std::string>("D") == "4"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").hasKey("E"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").hasValue<std::string>("E"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").value<std::string>("E") == "5"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").hasKey("F"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").hasValue<std::string>("F"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("b").value<std::string>("F") == "6"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").hasKey("G"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").hasValue<std::string>("G"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").value<std::string>("G") == "7"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").hasKey("H"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").hasValue<std::string>("H"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").value<std::string>("H") == "8"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").hasKey("I"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").hasValue<std::string>("I"));
    REQUIRE(p.optionalVectorDictMap->at(2).at("c").value<std::string>("I") == "9"s);
}

TEST_CASE("Execution/Structs/Map String Key:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 8);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "SimpleMap");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "simpleMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
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
        CHECK(e.key == "OptionalMap");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
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
        CHECK(e.key == "VectorMap");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorMap documentation");
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
        CHECK(e.key == "OptionalVectorMap");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalVectorMap documentation");
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
        CHECK(e.key == "DictMap");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "dictMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "Table");
        REQUIRE(dynamic_cast<TableVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[5];
        CHECK(e.key == "OptionalDictMap");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalDictMap documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "Table");
        REQUIRE(dynamic_cast<TableVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[6];
        CHECK(e.key == "VectorDictMap");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "vectorDictMap documentation");
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
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].documentation.empty());
        CHECK(w->documentations[0].verifier->type() == "Table");
        REQUIRE(dynamic_cast<TableVerifier*>(w->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[7];
        CHECK(e.key == "OptionalVectorDictMap");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "optionalVectorDictMap documentation");
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
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "*");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].documentation.empty());
        CHECK(w->documentations[0].verifier->type() == "Table");
        REQUIRE(dynamic_cast<TableVerifier*>(w->documentations[0].verifier.get()));
    }
}
