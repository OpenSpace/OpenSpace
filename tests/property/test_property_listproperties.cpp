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

#include <openspace/properties/list/doublelistproperty.h>
#include <openspace/properties/list/intlistproperty.h>
#include <openspace/properties/list/stringlistproperty.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <string>
#include <vector>

TEST_CASE("StringListProperty: Class Name and Default Value", "[stringlistproperty]") {
    const openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    CHECK(p.className() == "StringListProperty");
    CHECK(p.value().empty());
}

TEST_CASE("StringListProperty: Set Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };

    p.setValue(list);
    CHECK(p.value() == list);

    // Empty value
    p.setValue({});
    CHECK(p.value().empty());
}

TEST_CASE("StringListProperty: Get String Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };
    p.setValue(list);

    std::string res = p.stringValue();

    CHECK(res == "[\"a\",\"b\",\"c\"]");
}

TEST_CASE("StringListProperty: Set Lua Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, list);

    p.setLuaValue(L);

    CHECK(p.value() == list);
}

TEST_CASE("StringListProperty: Set Lua Value - Empty", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector<std::string>{});
    p.setLuaValue(L);

    CHECK(p.value().empty());
}

TEST_CASE("StringListProperty: Invalid Set Lua Value - Not List", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, 2); // Not a list

    CHECK_THROWS(p.setLuaValue(L));
}

TEST_CASE("StringListProperty: Get Lua Value", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };
    p.setValue(list);

    const ghoul::lua::LuaState L;
    p.getLuaValue(L);

    CHECK(ghoul::lua::luaValueToString(L, 1) ==
        "{ [1] = \"a\", [2] = \"b\", [3] = \"c\" }"
    );
}

TEST_CASE("StringListProperty: Get Empty Lua Value", "[stringlistproperty]") {
    const openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    p.getLuaValue(L);

    CHECK(ghoul::lua::luaValueToString(L, 1) == "{}");
}

TEST_CASE("StringListProperty: Value From Copying Variable", "[stringlistproperty]") {
    openspace::properties::StringListProperty p({ "id", "gui", "desc" });

    const std::vector<std::string> list{ "a", "b", "c" };
    p = list;

    CHECK(p.value() == list);
}

// IntListProperty

TEST_CASE("IntListProperty: Class Name and Default Value", "[intlistproperty]") {
    const openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    CHECK(p.className() == "IntListProperty");
    CHECK(p.value().empty());
}

TEST_CASE("IntListProperty: Set Value", "[intlistproperty]") {
    openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    const std::vector<int> list{ 1, 2, 3};

    p.setValue(list);
    CHECK(p.value() == list);

    // Empty value
    p.setValue({});
    CHECK(p.value().empty());
}

TEST_CASE("IntListProperty: Get String Value", "[intlistproperty]") {
    openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    const std::vector<int> list{ 1, 2, 3 };
    p.setValue(list);

    std::string res = p.stringValue();

    CHECK(res == "[1,2,3]");
}

TEST_CASE("IntListProperty: Set Lua Value", "[intlistproperty]") {
    openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    const std::vector<int> list{ 1, 2, 3 };

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, list);

    p.setLuaValue(L);

    CHECK(p.value() == list);
}

TEST_CASE("IntListProperty: Set Lua Value - Empty", "[intlistproperty]") {
    openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector<int>());
    p.setLuaValue(L);

    CHECK(p.value().empty());
}

TEST_CASE("IntListProperty: Set Lua Value - Non-number", "[intlistproperty]") {
    openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector{ "not a number", "oops" });
    CHECK_THROWS(p.setLuaValue(L));
    CHECK(p.value().empty());
}

TEST_CASE("IntListProperty: Invalid Set Lua Value - Not List", "[intlistproperty]") {
    openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, 2); // Not a list

    CHECK_THROWS(p.setLuaValue(L));
}

TEST_CASE("IntListProperty: Get Lua Value", "[intlistproperty]") {
    openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    const std::vector<int> list{ 1, 2, 3 };
    p.setValue(list);

    const ghoul::lua::LuaState L;
    p.getLuaValue(L);

    CHECK(ghoul::lua::luaValueToString(L, 1) ==
        "{ [1] = 1, [2] = 2, [3] = 3 }"
    );
}

TEST_CASE("IntListProperty: Get Empty Lua Value", "[intlistproperty]") {
    const openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    p.getLuaValue(L);

    CHECK(ghoul::lua::luaValueToString(L, 1) == "{}");
}

TEST_CASE("IntListProperty: Value From Copying Variable", "[intlistproperty]") {
    openspace::properties::IntListProperty p({ "id", "gui", "desc" });

    const std::vector<int> list{ 1, 2, 3 };
    p = list;

    CHECK(p.value() == list);
}

// DoubleListProperty

TEST_CASE("DoubleListProperty: Class Name and Default Value", "[doublelistproperty]") {
    const openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    CHECK(p.className() == "DoubleListProperty");
    CHECK(p.value().empty());
}

TEST_CASE("DoubleListProperty: Set Value", "[doublelistproperty]") {
    openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    const std::vector<double> list{ 1.0, 2.0, 3.0 };

    p.setValue(list);
    CHECK(p.value() == list);

    // Empty value
    p.setValue({});
    CHECK(p.value().empty());
}

TEST_CASE("DoubleListProperty: Get String Value", "[doublelistproperty]") {
    openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    const std::vector<double> list{ 1.0, 2.0, 3.0 };
    p.setValue(list);

    std::string res = p.stringValue();

    CHECK(res == "[1.0,2.0,3.0]");
}

TEST_CASE("DoubleListProperty: Set Lua Value", "[doublelistproperty]") {
    openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    const std::vector<double> list{ 1.0, 2.0, 3.0 };

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, list);

    p.setLuaValue(L);

    CHECK(p.value() == list);
}

TEST_CASE("DoubleListProperty: Set Lua Value - Empty", "[doublelistproperty]") {
    openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector<double>());
    p.setLuaValue(L);

    CHECK(p.value().empty());
}

TEST_CASE("DoubleListProperty: Set Lua Value - Non-number", "[doublelistproperty]") {
    openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector{"not a number", "oops"});
    CHECK_THROWS(p.setLuaValue(L));
    CHECK(p.value().empty());
}

TEST_CASE("DoubleListProperty: Invalid Set Lua Value - Not List", "[doublelistproperty]") {
    openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    ghoul::lua::push(L, 2); // Not a list

    CHECK_THROWS(p.setLuaValue(L));
}

TEST_CASE("DoubleListProperty: Get Lua Value", "[doublelistproperty]") {
    openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    const std::vector<double> list{ 1.0, 2.1, 3.2 };
    p.setValue(list);

    const ghoul::lua::LuaState L;
    p.getLuaValue(L);

    CHECK(ghoul::lua::luaValueToString(L, 1) ==
        "{ [1] = 1, [2] = 2.1, [3] = 3.2 }"
    );
}

TEST_CASE("DoubleListProperty: Get Empty Lua Value", "[doublelistproperty]") {
    const openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    const ghoul::lua::LuaState L;
    p.getLuaValue(L);

    CHECK(ghoul::lua::luaValueToString(L, 1) == "{}");
}

TEST_CASE("DoubleListProperty: Value From Copying Variable", "[doublelistproperty]") {
    openspace::properties::DoubleListProperty p({ "id", "gui", "desc" });

    const std::vector<double> list{ 1.0, 2.0, 3.0 };
    p = list;

    CHECK(p.value() == list);
}
