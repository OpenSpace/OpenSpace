/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/json.h>
#include <openspace/topic/topics/luascripttopic.h>
#include <string>

using namespace openspace;

TEST_CASE("LuaScriptTopic: Scalar values", "[luascripttopic]") {
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json(true)) == "true");
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json(false)) == "false");
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json(nullptr)) == "nil");
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json(1)) == "1");
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json(2.5)) == "2.5");
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json(-3)) == "-3");
}

TEST_CASE("LuaScriptTopic: Strings", "[luascripttopic]") {
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json("abc")) == R"("abc")");
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json("")) == R"("")");

    // Characters that would end the string or start an escape sequence are escaped
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json(R"(a"b)")) == R"("a\"b")");
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json("a'b")) == R"("a\'b")");
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json(R"(a\b)")) == R"("a\\b")");
}

TEST_CASE("LuaScriptTopic: Arrays", "[luascripttopic]") {
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json::array()) == "{}");

    const nlohmann::json array = nlohmann::json::parse(R"([1, "a", true, null])");
    CHECK(LuaScriptTopic::jsonToLua(array) == R"({1,"a",true,nil})");

    const nlohmann::json nested = nlohmann::json::parse("[[1, 2], [3]]");
    CHECK(LuaScriptTopic::jsonToLua(nested) == "{{1,2},{3}}");
}

TEST_CASE("LuaScriptTopic: Objects", "[luascripttopic]") {
    CHECK(LuaScriptTopic::jsonToLua(nlohmann::json::object()) == "{}");

    const nlohmann::json object = nlohmann::json::parse(R"({ "a": 1, "b": "c" })");
    CHECK(LuaScriptTopic::jsonToLua(object) == R"({["a"] = 1,["b"] = "c"})");

    const nlohmann::json nested = nlohmann::json::parse(R"({ "a": { "b": [1] } })");
    CHECK(LuaScriptTopic::jsonToLua(nested) == R"({["a"] = {["b"] = {1}}})");

    // Keys are escaped the same way as string values
    const nlohmann::json key = nlohmann::json::parse(R"({ "a\"b": 1 })");
    CHECK(LuaScriptTopic::jsonToLua(key) == R"({["a\"b"] = 1})");
}
