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

using namespace openspace;

namespace {
    struct [[codegen::Dictionary(MapEnumKey)]] Parameters {
        enum class KeyType {
            Key1,
            Key2,
            Key3
        };

        // simpleMap documentation
        std::map<KeyType, std::string> simpleMap;

        // optionalMap documentation
        std::optional<std::map<KeyType, std::string>> optionalMap;

        // vectorMap documentation
        std::vector<std::map<KeyType, std::string>> vectorMap;

        // optionalVectorMap documentation
        std::optional<std::vector<std::map<KeyType, std::string>>> optionalVectorMap;

        // dictMap documentation
        std::map<KeyType, Dictionary> dictMap;

        // optionalDictMap documentation
        std::optional<std::map<KeyType, Dictionary>> optionalDictMap;

        // vectorDictMap documentation
        std::vector<std::map<KeyType, Dictionary>> vectorDictMap;

        // optionalVectorDictMap documentation
        std::optional<std::vector<std::map<KeyType, Dictionary>>> optionalVectorDictMap;
    };
} // namespace
#include "execution_structs_map_enum_key_codegen.cpp"


TEST_CASE("Execution/Structs/Map Enum Key:  Bake", "[Execution][Structs]") {
    using namespace std::literals;

    Dictionary d;
    {
        Dictionary e;
        e.setValue("Key1", "1"s);
        e.setValue("Key2", "2"s);
        e.setValue("Key3", "3"s);
        d.setValue("SimpleMap", e);
    }
    {
        Dictionary e;
        e.setValue("Key1", "4"s);
        e.setValue("Key2", "5"s);
        e.setValue("Key3", "6"s);
        d.setValue("OptionalMap", e);
    }
    {
        Dictionary e;
        {
            Dictionary f;
            f.setValue("Key1", "7"s);
            f.setValue("Key2", "8"s);
            e.setValue("1", f);
        }
        {
            Dictionary f;
            f.setValue("Key1", "9"s);
            f.setValue("Key2", "10"s);
            e.setValue("2", f);
        }
        {
            Dictionary f;
            f.setValue("Key1", "11"s);
            f.setValue("Key2", "12"s);
            e.setValue("3", f);
        }
        d.setValue("VectorMap", e);
    }
    {
        Dictionary e;
        {
            Dictionary f;
            f.setValue("Key1", "13"s);
            f.setValue("Key2", "14"s);
            e.setValue("1", f);
        }
        {
            Dictionary f;
            f.setValue("Key1", "15"s);
            f.setValue("Key2", "16"s);
            e.setValue("2", f);
        }
        {
            Dictionary f;
            f.setValue("Key1", "17"s);
            f.setValue("Key2", "18"s);
            e.setValue("3", f);
        }
        d.setValue("OptionalVectorMap", e);
    }
    {
        Dictionary e;
        {
            Dictionary f;
            f.setValue("A", "1"s);
            f.setValue("B", "2"s);
            f.setValue("C", "3"s);
            e.setValue("Key1", f);
        }
        {
            Dictionary f;
            f.setValue("D", "4"s);
            f.setValue("E", "5"s);
            f.setValue("F", "6"s);
            e.setValue("Key2", f);
        }
        {
            Dictionary f;
            f.setValue("G", "7"s);
            f.setValue("H", "8"s);
            f.setValue("I", "9"s);
            e.setValue("Key3", f);
        }
        d.setValue("DictMap", e);
    }
    {
        Dictionary e;
        {
            Dictionary f;
            f.setValue("A", "1"s);
            f.setValue("B", "2"s);
            f.setValue("C", "3"s);
            e.setValue("Key1", f);
        }
        {
            Dictionary f;
            f.setValue("D", "4"s);
            f.setValue("E", "5"s);
            f.setValue("F", "6"s);
            e.setValue("Key2", f);
        }
        {
            Dictionary f;
            f.setValue("G", "7"s);
            f.setValue("H", "8"s);
            f.setValue("I", "9"s);
            e.setValue("Key3", f);
        }
        d.setValue("OptionalDictMap", e);
    }
    {
        Dictionary e;
        {
            Dictionary f;
            {
                Dictionary g;
                g.setValue("A", "1"s);
                g.setValue("B", "2"s);
                g.setValue("C", "3"s);
                f.setValue("Key1", g);
            }
            {
                Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("Key2", g);
            }
            {
                Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("Key3", g);
            }
            e.setValue("1", f);
        }
        {
            Dictionary f;
            {
                Dictionary g;
                g.setValue("A2", "1"s);
                g.setValue("B2", "2"s);
                g.setValue("C2", "3"s);
                f.setValue("Key1", g);
            }
            {
                Dictionary g;
                g.setValue("D2", "4"s);
                g.setValue("E2", "5"s);
                g.setValue("F2", "6"s);
                f.setValue("Key2", g);
            }
            {
                Dictionary g;
                g.setValue("G2", "7"s);
                g.setValue("H2", "8"s);
                g.setValue("I2", "9"s);
                f.setValue("Key3", g);
            }
            e.setValue("2", f);
        }
        {
            Dictionary f;
            {
                Dictionary g;
                g.setValue("A3", "1"s);
                g.setValue("B3", "2"s);
                g.setValue("C3", "3"s);
                f.setValue("Key1", g);
            }
            {
                Dictionary g;
                g.setValue("D3", "4"s);
                g.setValue("E3", "5"s);
                g.setValue("F3", "6"s);
                f.setValue("Key2", g);
            }
            {
                Dictionary g;
                g.setValue("G3", "7"s);
                g.setValue("H3", "8"s);
                g.setValue("I3", "9"s);
                f.setValue("Key3", g);
            }
            e.setValue("3", f);
        }
        d.setValue("VectorDictMap", e);
    }
    {
        Dictionary e;
        {
            Dictionary f;
            {
                Dictionary g;
                g.setValue("A", "1"s);
                g.setValue("B", "2"s);
                g.setValue("C", "3"s);
                f.setValue("Key1", g);
            }
            {
                Dictionary g;
                g.setValue("D", "4"s);
                g.setValue("E", "5"s);
                g.setValue("F", "6"s);
                f.setValue("Key2", g);
            }
            {
                Dictionary g;
                g.setValue("G", "7"s);
                g.setValue("H", "8"s);
                g.setValue("I", "9"s);
                f.setValue("Key3", g);
            }
            e.setValue("1", f);
        }
        {
            Dictionary f;
            {
                Dictionary g;
                g.setValue("A2", "1"s);
                g.setValue("B2", "2"s);
                g.setValue("C2", "3"s);
                f.setValue("Key1", g);
            }
            {
                Dictionary g;
                g.setValue("D2", "4"s);
                g.setValue("E2", "5"s);
                g.setValue("F2", "6"s);
                f.setValue("Key2", g);
            }
            {
                Dictionary g;
                g.setValue("G2", "7"s);
                g.setValue("H2", "8"s);
                g.setValue("I2", "9"s);
                f.setValue("Key3", g);
            }
            e.setValue("2", f);
        }
        {
            Dictionary f;
            {
                Dictionary g;
                g.setValue("A3", "1"s);
                g.setValue("B3", "2"s);
                g.setValue("C3", "3"s);
                f.setValue("Key1", g);
            }
            {
                Dictionary g;
                g.setValue("D3", "4"s);
                g.setValue("E3", "5"s);
                g.setValue("F3", "6"s);
                f.setValue("Key2", g);
            }
            {
                Dictionary g;
                g.setValue("G3", "7"s);
                g.setValue("H3", "8"s);
                g.setValue("I3", "9"s);
                f.setValue("Key3", g);
            }
            e.setValue("3", f);
        }
        d.setValue("OptionalVectorDictMap", e);
    }

    const Parameters p = codegen::bake<Parameters>(d);
    REQUIRE(p.simpleMap.size() == 3);
    REQUIRE(p.simpleMap.find(Parameters::KeyType::Key1) != p.simpleMap.end());
    CHECK(p.simpleMap.at(Parameters::KeyType::Key1) == "1");
    REQUIRE(p.simpleMap.find(Parameters::KeyType::Key2) != p.simpleMap.end());
    CHECK(p.simpleMap.at(Parameters::KeyType::Key2) == "2");
    REQUIRE(p.simpleMap.find(Parameters::KeyType::Key3) != p.simpleMap.end());
    CHECK(p.simpleMap.at(Parameters::KeyType::Key3) == "3");

    REQUIRE(p.optionalMap.has_value());
    REQUIRE(p.optionalMap->size() == 3);
    REQUIRE(p.optionalMap->find(Parameters::KeyType::Key1) != p.optionalMap->end());
    CHECK(p.optionalMap->at(Parameters::KeyType::Key1) == "4");
    REQUIRE(p.optionalMap->find(Parameters::KeyType::Key2) != p.optionalMap->end());
    CHECK(p.optionalMap->at(Parameters::KeyType::Key2) == "5");
    REQUIRE(p.optionalMap->find(Parameters::KeyType::Key3) != p.optionalMap->end());
    CHECK(p.optionalMap->at(Parameters::KeyType::Key3) == "6");

    REQUIRE(p.vectorMap.size() == 3);
    REQUIRE(p.vectorMap[0].find(Parameters::KeyType::Key1) != p.vectorMap[0].end());
    CHECK(p.vectorMap[0].at(Parameters::KeyType::Key1) == "7");
    REQUIRE(p.vectorMap[0].find(Parameters::KeyType::Key2) != p.vectorMap[0].end());
    CHECK(p.vectorMap[0].at(Parameters::KeyType::Key2) == "8");

    REQUIRE(p.vectorMap[1].find(Parameters::KeyType::Key1) != p.vectorMap[1].end());
    CHECK(p.vectorMap[1].at(Parameters::KeyType::Key1) == "9");
    REQUIRE(p.vectorMap[1].find(Parameters::KeyType::Key2) != p.vectorMap[1].end());
    CHECK(p.vectorMap[1].at(Parameters::KeyType::Key2) == "10");

    REQUIRE(p.vectorMap[2].find(Parameters::KeyType::Key1) != p.vectorMap[2].end());
    CHECK(p.vectorMap[2].at(Parameters::KeyType::Key1) == "11");
    REQUIRE(p.vectorMap[2].find(Parameters::KeyType::Key2) != p.vectorMap[2].end());
    CHECK(p.vectorMap[2].at(Parameters::KeyType::Key2) == "12");

    REQUIRE(p.optionalVectorMap.has_value());
    REQUIRE(p.optionalVectorMap->size() == 3);
    REQUIRE(p.optionalVectorMap->at(0).find(Parameters::KeyType::Key1) != p.optionalVectorMap->at(0).end());
    CHECK(p.optionalVectorMap->at(0).at(Parameters::KeyType::Key1) == "13");
    REQUIRE(p.optionalVectorMap->at(0).find(Parameters::KeyType::Key2) != p.optionalVectorMap->at(0).end());
    CHECK(p.optionalVectorMap->at(0).at(Parameters::KeyType::Key2) == "14");

    REQUIRE(p.optionalVectorMap->at(1).find(Parameters::KeyType::Key1) != p.optionalVectorMap->at(1).end());
    CHECK(p.optionalVectorMap->at(1).at(Parameters::KeyType::Key1) == "15");
    REQUIRE(p.optionalVectorMap->at(1).find(Parameters::KeyType::Key2) != p.optionalVectorMap->at(1).end());
    CHECK(p.optionalVectorMap->at(1).at(Parameters::KeyType::Key2) == "16");

    REQUIRE(p.optionalVectorMap->at(2).find(Parameters::KeyType::Key1) != p.optionalVectorMap->at(2).end());
    CHECK(p.optionalVectorMap->at(2).at(Parameters::KeyType::Key1) == "17");
    REQUIRE(p.optionalVectorMap->at(2).find(Parameters::KeyType::Key2) != p.optionalVectorMap->at(2).end());
    CHECK(p.optionalVectorMap->at(2).at(Parameters::KeyType::Key2) == "18");

    REQUIRE(p.dictMap.size() == 3);
    REQUIRE(p.dictMap.find(Parameters::KeyType::Key1) != p.dictMap.end());
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).size() == 3);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).hasKey("A"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).hasValue<std::string>("A"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).value<std::string>("A") == "1"s);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).hasKey("B"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).hasValue<std::string>("B"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).value<std::string>("B") == "2"s);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).hasKey("C"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).hasValue<std::string>("C"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key1).value<std::string>("C") == "3"s);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).size() == 3);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).hasKey("D"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).hasValue<std::string>("D"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).value<std::string>("D") == "4"s);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).hasKey("E"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).hasValue<std::string>("E"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).value<std::string>("E") == "5"s);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).hasKey("F"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).hasValue<std::string>("F"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key2).value<std::string>("F") == "6"s);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).size() == 3);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).hasKey("G"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).hasValue<std::string>("G"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).value<std::string>("G") == "7"s);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).hasKey("H"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).hasValue<std::string>("H"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).value<std::string>("H") == "8"s);
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).hasKey("I"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).hasValue<std::string>("I"));
    REQUIRE(p.dictMap.at(Parameters::KeyType::Key3).value<std::string>("I") == "9"s);

    REQUIRE(p.optionalDictMap.has_value());
    REQUIRE(p.optionalDictMap->size() == 3);
    REQUIRE(p.optionalDictMap->find(Parameters::KeyType::Key1) != p.optionalDictMap->end());
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).size() == 3);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).hasKey("A"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).hasValue<std::string>("A"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).value<std::string>("A") == "1"s);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).hasKey("B"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).hasValue<std::string>("B"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).value<std::string>("B") == "2"s);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).hasKey("C"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).hasValue<std::string>("C"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key1).value<std::string>("C") == "3"s);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).size() == 3);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).hasKey("D"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).hasValue<std::string>("D"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).value<std::string>("D") == "4"s);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).hasKey("E"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).hasValue<std::string>("E"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).value<std::string>("E") == "5"s);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).hasKey("F"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).hasValue<std::string>("F"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key2).value<std::string>("F") == "6"s);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).size() == 3);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).hasKey("G"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).hasValue<std::string>("G"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).value<std::string>("G") == "7"s);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).hasKey("H"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).hasValue<std::string>("H"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).value<std::string>("H") == "8"s);
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).hasKey("I"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).hasValue<std::string>("I"));
    REQUIRE(p.optionalDictMap->at(Parameters::KeyType::Key3).value<std::string>("I") == "9"s);

    REQUIRE(p.vectorDictMap.size() == 3);
    REQUIRE(p.vectorDictMap[0].size() == 3);
    REQUIRE(p.vectorDictMap[0].find(Parameters::KeyType::Key1) != p.vectorDictMap[0].end());
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).size() == 3);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).hasKey("A"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).hasValue<std::string>("A"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).value<std::string>("A") == "1"s);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).hasKey("B"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).hasValue<std::string>("B"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).value<std::string>("B") == "2"s);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).hasKey("C"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).hasValue<std::string>("C"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key1).value<std::string>("C") == "3"s);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).size() == 3);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).hasKey("D"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).hasValue<std::string>("D"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).value<std::string>("D") == "4"s);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).hasKey("E"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).hasValue<std::string>("E"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).value<std::string>("E") == "5"s);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).hasKey("F"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).hasValue<std::string>("F"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key2).value<std::string>("F") == "6"s);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).size() == 3);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).hasKey("G"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).hasValue<std::string>("G"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).value<std::string>("G") == "7"s);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).hasKey("H"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).hasValue<std::string>("H"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).value<std::string>("H") == "8"s);
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).hasKey("I"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).hasValue<std::string>("I"));
    REQUIRE(p.vectorDictMap[0].at(Parameters::KeyType::Key3).value<std::string>("I") == "9"s);
    REQUIRE(p.vectorDictMap[1].size() == 3);
    REQUIRE(p.vectorDictMap[1].find(Parameters::KeyType::Key1) != p.vectorDictMap[1].end());
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).size() == 3);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).hasKey("A2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).hasValue<std::string>("A2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).value<std::string>("A2") == "1"s);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).hasKey("B2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).hasValue<std::string>("B2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).value<std::string>("B2") == "2"s);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).hasKey("C2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).hasValue<std::string>("C2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key1).value<std::string>("C2") == "3"s);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).size() == 3);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).hasKey("D2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).hasValue<std::string>("D2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).value<std::string>("D2") == "4"s);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).hasKey("E2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).hasValue<std::string>("E2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).value<std::string>("E2") == "5"s);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).hasKey("F2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).hasValue<std::string>("F2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key2).value<std::string>("F2") == "6"s);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).size() == 3);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).hasKey("G2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).hasValue<std::string>("G2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).value<std::string>("G2") == "7"s);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).hasKey("H2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).hasValue<std::string>("H2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).value<std::string>("H2") == "8"s);
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).hasKey("I2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).hasValue<std::string>("I2"));
    REQUIRE(p.vectorDictMap[1].at(Parameters::KeyType::Key3).value<std::string>("I2") == "9"s);
    REQUIRE(p.vectorDictMap[2].size() == 3);
    REQUIRE(p.vectorDictMap[2].find(Parameters::KeyType::Key1) != p.vectorDictMap[2].end());
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).size() == 3);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).hasKey("A3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).hasValue<std::string>("A3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).value<std::string>("A3") == "1"s);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).hasKey("B3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).hasValue<std::string>("B3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).value<std::string>("B3") == "2"s);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).hasKey("C3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).hasValue<std::string>("C3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key1).value<std::string>("C3") == "3"s);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).size() == 3);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).hasKey("D3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).hasValue<std::string>("D3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).value<std::string>("D3") == "4"s);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).hasKey("E3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).hasValue<std::string>("E3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).value<std::string>("E3") == "5"s);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).hasKey("F3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).hasValue<std::string>("F3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key2).value<std::string>("F3") == "6"s);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).size() == 3);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).hasKey("G3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).hasValue<std::string>("G3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).value<std::string>("G3") == "7"s);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).hasKey("H3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).hasValue<std::string>("H3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).value<std::string>("H3") == "8"s);
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).hasKey("I3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).hasValue<std::string>("I3"));
    REQUIRE(p.vectorDictMap[2].at(Parameters::KeyType::Key3).value<std::string>("I3") == "9"s);

    REQUIRE(p.optionalVectorDictMap.has_value());
    REQUIRE(p.optionalVectorDictMap->size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).find(Parameters::KeyType::Key1) != p.optionalVectorDictMap->at(0).end());
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).hasKey("A"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).hasValue<std::string>("A"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).value<std::string>("A") == "1"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).hasKey("B"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).hasValue<std::string>("B"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).value<std::string>("B") == "2"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).hasKey("C"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).hasValue<std::string>("C"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key1).value<std::string>("C") == "3"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).hasKey("D"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).hasValue<std::string>("D"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).value<std::string>("D") == "4"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).hasKey("E"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).hasValue<std::string>("E"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).value<std::string>("E") == "5"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).hasKey("F"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).hasValue<std::string>("F"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key2).value<std::string>("F") == "6"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).hasKey("G"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).hasValue<std::string>("G"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).value<std::string>("G") == "7"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).hasKey("H"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).hasValue<std::string>("H"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).value<std::string>("H") == "8"s);
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).hasKey("I"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).hasValue<std::string>("I"));
    REQUIRE(p.optionalVectorDictMap->at(0).at(Parameters::KeyType::Key3).value<std::string>("I") == "9"s);
    REQUIRE(p.optionalVectorDictMap->at(1).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(1).find(Parameters::KeyType::Key1) != p.optionalVectorDictMap->at(1).end());
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).hasKey("A2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).hasValue<std::string>("A2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).value<std::string>("A2") == "1"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).hasKey("B2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).hasValue<std::string>("B2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).value<std::string>("B2") == "2"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).hasKey("C2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).hasValue<std::string>("C2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key1).value<std::string>("C2") == "3"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).hasKey("D2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).hasValue<std::string>("D2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).value<std::string>("D2") == "4"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).hasKey("E2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).hasValue<std::string>("E2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).value<std::string>("E2") == "5"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).hasKey("F2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).hasValue<std::string>("F2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key2).value<std::string>("F2") == "6"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).hasKey("G2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).hasValue<std::string>("G2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).value<std::string>("G2") == "7"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).hasKey("H2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).hasValue<std::string>("H2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).value<std::string>("H2") == "8"s);
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).hasKey("I2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).hasValue<std::string>("I2"));
    REQUIRE(p.optionalVectorDictMap->at(1).at(Parameters::KeyType::Key3).value<std::string>("I2") == "9"s);
    REQUIRE(p.optionalVectorDictMap->at(2).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(2).find(Parameters::KeyType::Key1) != p.optionalVectorDictMap->at(2).end());
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).hasKey("A3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).hasValue<std::string>("A3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).value<std::string>("A3") == "1"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).hasKey("B3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).hasValue<std::string>("B3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).value<std::string>("B3") == "2"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).hasKey("C3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).hasValue<std::string>("C3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key1).value<std::string>("C3") == "3"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).hasKey("D3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).hasValue<std::string>("D3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).value<std::string>("D3") == "4"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).hasKey("E3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).hasValue<std::string>("E3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).value<std::string>("E3") == "5"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).hasKey("F3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).hasValue<std::string>("F3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key2).value<std::string>("F3") == "6"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).size() == 3);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).hasKey("G3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).hasValue<std::string>("G3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).value<std::string>("G3") == "7"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).hasKey("H3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).hasValue<std::string>("H3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).value<std::string>("H3") == "8"s);
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).hasKey("I3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).hasValue<std::string>("I3"));
    REQUIRE(p.optionalVectorDictMap->at(2).at(Parameters::KeyType::Key3).value<std::string>("I3") == "9"s);
}

TEST_CASE("Execution/Structs/Map Enum Key:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 8);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "SimpleMap");
        CHECK(!e.optional);
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
