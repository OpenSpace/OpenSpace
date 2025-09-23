/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/properties/misc/selectionproperty.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <string>
#include <vector>

TEST_CASE("SelectionProperty: Class Name and Default Value", "[selectionproperty]") {
    const openspace::properties::SelectionProperty p({ "id", "gui", "desc" });

    CHECK(p.className() == "SelectionProperty");
    CHECK(p.value().empty());
    CHECK(p.options().empty());
}

TEST_CASE("SelectionProperty: Set Value", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    const std::set<std::string> list{ "a", "b" };
    p.setValue(list);

    CHECK(p.value() == list);

    // Empty value
    p.setValue({});
    CHECK(p.value().empty());
}

TEST_CASE("SelectionProperty: Set Value - Invalid Input", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    p.setValue({ "d", "a" }); // d is not valid option and should be ignored

    CHECK(p.value() == std::set<std::string>{ "a" });
}

TEST_CASE("SelectionProperty: Set Value - Duplicates", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    p.setValue({ "a", "a" });

    // Duplicate should be ignored in set creation
    CHECK(p.value() == std::set<std::string>{ "a" });
}

TEST_CASE("SelectionProperty: Options and Selection Helpers", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b" });
    p.setValue({ "a" });

    CHECK(p.hasSelected());
    CHECK(p.isSelected("a"));
    CHECK((p.hasOption("a") && p.hasOption("b")));

    p.clearSelection();
    CHECK(!p.hasSelected());
    CHECK(p.value().empty());

    p.setValue({ "a" });
    p.clearOptions();

    // Clearing options should clear selection as well
    CHECK(p.options().empty());
    CHECK(!p.hasSelected());
    CHECK(p.value().empty());
}

TEST_CASE("SelectionProperty: Options Sorting", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "c", "b" });

    CHECK(p.options() == std::vector<std::string>{ "a", "b", "c" });
}

TEST_CASE("SelectionProperty: Add Options", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });

    p.addOption("a");
    p.addOption("b");
    p.addOption("a"); // duplicate

    const int nOptions = static_cast<int>(p.options().size());
    CHECK(nOptions == 2);
    CHECK(p.options() == std::vector<std::string>{ "a", "b" });
}

TEST_CASE("SelectionProperty: Get String Value", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    const std::set<std::string> list{ "a", "b" };
    p.setValue(list);

    std::string res = p.stringValue();
    CHECK(res == "[\"a\",\"b\"]");

    p.setValue({});
    std::string res2 = p.stringValue();
    CHECK(res2 == "[]");
}

TEST_CASE("SelectionProperty: Set Lua Value", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector{ "a", "b" });

    p.setLuaValue(L);

    CHECK(p.value() == std::set<std::string>{ "a", "b" });
}

TEST_CASE("SelectionProperty: Set Lua Value - Duplicates", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector{ "a", "a", "b" });

    p.setLuaValue(L);

    CHECK(p.value() == std::set<std::string>{ "a", "b" });
}

TEST_CASE("SelectionProperty: Set Lua Value - Invalid Key", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector{ "a", "d" });

    p.setLuaValue(L);

    CHECK(p.value() == std::set<std::string>{ "a" });
}

TEST_CASE("SelectionProperty: Get Lua Value", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    const std::set<std::string> list{ "a", "b" };
    p.setValue(list);

    const ghoul::lua::LuaState L;
    p.getLuaValue(L);

    ghoul::Dictionary reference;
    int i = 1;
    for (const std::string& k : list) {
        reference.setValue(ghoul::to_string(i), k);
        i++;
    }

    ghoul::Dictionary res = ghoul::lua::value<ghoul::Dictionary>(L);

    // Also test JSON representation for meaningful output
    CHECK(ghoul::formatJson(res) == ghoul::formatJson(reference));
    CHECK(res == reference);
}

TEST_CASE("SelectionProperty: Get Empty Lua Value", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    const ghoul::lua::LuaState L;
    p.getLuaValue(L);

    ghoul::Dictionary res = ghoul::lua::value<ghoul::Dictionary>(L);
    ghoul::Dictionary emptyDict;

    // Also test JSON representation for meaningful output
    CHECK(ghoul::formatJson(res) == ghoul::formatJson(emptyDict));
    CHECK(res == emptyDict);
}

TEST_CASE("SelectionProperty: Value From Copying Variable", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    p = std::set<std::string>{ "a", "b" };

    CHECK(p.value() == std::set<std::string>{ "a", "b" });
}

TEST_CASE("SelectionProperty: Re-set Options After Selection", "[selectionproperty]") {
    openspace::properties::SelectionProperty p({ "id", "gui", "desc" });
    p.setOptions({ "a", "b", "c" });

    p = std::set<std::string>{ "a", "b" };

    p.setOptions(std::vector<std::string>{ "a", "c", "d" }); // b no longer included
                                                             // => should be removed

    CHECK(p.value() == std::set<std::string>{ "a" });
}
