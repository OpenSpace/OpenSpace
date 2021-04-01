/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include "catch2/catch.hpp"

#include <openspace/properties/list/stringlistproperty.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <string>
#include <vector>

TEST_CASE("StringListProperty: Class Name and Default Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    REQUIRE(p.className() == "StringListProperty");
    REQUIRE(p.value() == std::vector<std::string>());
}

TEST_CASE("StringListProperty: Set Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };

    p.setValue(list);
    REQUIRE(p.value() == list);

    // Empty value
    p.setValue({});
    REQUIRE(p.value() == std::vector<std::string>());
}

TEST_CASE("StringListProperty: Get String Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };
    p.setValue(list);

    std::string res;
    p.getStringValue(res);

    REQUIRE(res == "[\"a\",\"b\",\"c\"]");
}

TEST_CASE("StringListProperty: Set Lua Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };

    ghoul::lua::LuaState L;
    ghoul::lua::push(L, list);

    p.setLuaValue(L);

    REQUIRE(p.value() == list);
}

TEST_CASE("StringListProperty: Set Lua Value - Empty", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector<std::string>{});
    p.setLuaValue(L);

    REQUIRE(p.value() == std::vector<std::string>{});
}

TEST_CASE("StringListProperty: Invalid Set Lua Value - Not List", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    ghoul::lua::LuaState L;
    ghoul::lua::push(L, 2); // Not a list

    bool success = p.setLuaValue(L);

    REQUIRE(!success);
}

TEST_CASE("StringListProperty: Get Lua Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };
    p.setValue(list);

    // Create dictionary to compare result against
    ghoul::Dictionary referenceDict;
    for (size_t i = 0; i < list.size(); i++) {
        referenceDict.setValue(ghoul::to_string(i + 1), list[i]);
    }

    ghoul::lua::LuaState L;
    p.getLuaValue(L);

    ghoul::Dictionary res = ghoul::lua::value<ghoul::Dictionary>(L);

    REQUIRE(res == referenceDict);
    // Also test JSON representation, for useful output
    REQUIRE(ghoul::formatJson(res) == ghoul::formatJson(referenceDict));
}

TEST_CASE("StringListProperty: Get Empty Lua Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    ghoul::lua::LuaState L;
    p.getLuaValue(L);

    ghoul::Dictionary res = ghoul::lua::value<ghoul::Dictionary>(L);
    ghoul::Dictionary emptyDict;

    // Also test JSON representation for meaningful output
    REQUIRE(ghoul::formatJson(res) == ghoul::formatJson(emptyDict));
    REQUIRE(res == emptyDict);
}

TEST_CASE("StringListProperty: Value From Copying Variable", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };
    p = list;

    REQUIRE(p.value() == list);
}
