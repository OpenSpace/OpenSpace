/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/glm.h>
#include <fstream>
#include <sstream>
#include <iostream>

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

    const ghoul::lua::LuaState state;
    ghoul::lua::runScript(state, TestString);
    //ghoul::lua::runScriptFile(state, "C:/Users/alebo68/Desktop/test.lua");

    lua_getglobal(state, "glob");

    ghoul::Dictionary dict;
    ghoul::lua::luaDictionaryFromState(state, dict);

    REQUIRE(dict.hasValue<ghoul::Dictionary>("A"));
    const ghoul::Dictionary a = dict.value<ghoul::Dictionary>("A");

    REQUIRE(a.hasValue<ghoul::Dictionary>("B"));
    const ghoul::Dictionary b = a.value<ghoul::Dictionary>("B");

    REQUIRE(b.hasValue<ghoul::Dictionary>("C"));
    const ghoul::Dictionary c = b.value<ghoul::Dictionary>("C");

    REQUIRE(c.hasValue<ghoul::Dictionary>("D"));
    const ghoul::Dictionary d = c.value<ghoul::Dictionary>("D");

    REQUIRE(d.hasValue<ghoul::Dictionary>("E"));
    const ghoul::Dictionary e = d.value<ghoul::Dictionary>("E");

    REQUIRE(e.hasValue<ghoul::Dictionary>("F"));
    const ghoul::Dictionary f = e.value<ghoul::Dictionary>("F");

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

    const ghoul::lua::LuaState state;
    ghoul::lua::runScript(state, TestString);

    lua_getglobal(state, "ModuleConfigurations");
    const ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(state);
    CHECK(d.hasValue<ghoul::Dictionary>("Server"));
}
