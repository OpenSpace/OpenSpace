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

#include <openspace/filesystem/filesystem.h>
#include <openspace/lua/lua_helper.h>
#include <openspace/lua/luastate.h>
#include <openspace/misc/dictionary.h>
#include <openspace/glm.h>
#include <fstream>
#include <sstream>
#include <iostream>

using namespace openspace;

TEST_CASE("LuaToDictionary: Nested Tables", "[luatodictionary]") {
    constexpr std::string_view TestString = R"(
        glob = {
            A = {
                B = {
                    C = {
                        D = {
                            E = { 
                                F = { "127.0.0.1", "localhost" },
                                G = {}
                            }
                        }
                    }
                }
            }
        }
)";

    const lua::LuaState state;
    lua::runScript(state, TestString);
    //lua::runScriptFile(state, "C:/Users/alebo68/Desktop/test.lua");

    lua_getglobal(state, "glob");

    Dictionary dict;
    lua::luaDictionaryFromState(state, dict);

    REQUIRE(dict.hasValue<Dictionary>("A"));
    const Dictionary a = dict.value<Dictionary>("A");

    REQUIRE(a.hasValue<Dictionary>("B"));
    const Dictionary b = a.value<Dictionary>("B");

    REQUIRE(b.hasValue<Dictionary>("C"));
    const Dictionary c = b.value<Dictionary>("C");

    REQUIRE(c.hasValue<Dictionary>("D"));
    const Dictionary d = c.value<Dictionary>("D");

    REQUIRE(d.hasValue<Dictionary>("E"));
    const Dictionary e = d.value<Dictionary>("E");

    REQUIRE(e.hasValue<Dictionary>("F"));
    const Dictionary f = e.value<Dictionary>("F");

    CHECK(f.hasValue<std::string>("1"));
    CHECK(f.hasValue<std::string>("2"));
}

TEST_CASE("LuaToDictionary: Nested Tables 2", "[luatodictionary]") {
    constexpr std::string_view TestString = R"(
        ModuleConfigurations = {
            Server = {
                Interfaces = {
                    {
                        RequirePasswordAddresses = {}
                    },
                    {
                        RequirePasswordAddresses = {}
                    }
                }
            }
        }
)";

    const lua::LuaState state;
    lua::runScript(state, TestString);

    lua_getglobal(state, "ModuleConfigurations");
    const Dictionary d = lua::value<Dictionary>(state);
    CHECK(d.hasValue<Dictionary>("Server"));
}
