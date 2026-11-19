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

#include <openspace/engine/globals.h>
#include <openspace/lua/lua_helper.h>
#include <openspace/scripting/scriptengine.h>
#include <string>

using namespace openspace;

namespace {
    // Runs the script, which has to assign its result to the global `Result`, and
    // returns that value as a string
    std::string runAndGetResult(const std::string& script) {
        lua_State* state = *global::scriptEngine->luaState();
        lua::runScript(state, script);
        lua_getglobal(state, "Result");
        std::string res = lua::value<std::string>(state);
        lua_pushnil(state);
        lua_setglobal(state, "Result");
        return res;
    }
} // namespace

TEST_CASE("FactoryConstructors: Sets Type", "[factoryconstructors]") {
    CHECK(
        runAndGetResult("Result = Renderable.RenderableTrailOrbit().Type") ==
        "RenderableTrailOrbit"
    );
    CHECK(
        runAndGetResult("Result = Translation.StaticTranslation().Type") ==
        "StaticTranslation"
    );
}

TEST_CASE("FactoryConstructors: Returns New Tables", "[factoryconstructors]") {
    CHECK(
        runAndGetResult(
            "local a = Renderable.RenderableTrailOrbit()\n"
            "local b = Renderable.RenderableTrailOrbit()\n"
            "a.Period = 2.5\n"
            "Result = tostring(rawequal(a, b)) .. ' ' .. tostring(b.Period)"
        ) == "false nil"
    );
}

TEST_CASE("FactoryConstructors: SceneGraphNode", "[factoryconstructors]") {
    CHECK(
        runAndGetResult(
            "local sgn = SceneGraphNode()\n"
            "Result = type(sgn) .. ' ' .. tostring(next(sgn))"
        ) == "table nil"
    );
    lua_State* state = *global::scriptEngine->luaState();
    CHECK_THROWS(lua::runScript(state, "SceneGraphNode({})"));
}

TEST_CASE("FactoryConstructors: Errors", "[factoryconstructors]") {
    lua_State* state = *global::scriptEngine->luaState();
    CHECK_THROWS(lua::runScript(state, "Renderable.RenderableTrailOrbit({})"));
    CHECK_THROWS(lua::runScript(state, "Renderable.RenderableTrailOrbit(nil)"));
    CHECK_THROWS(lua::runScript(state, "Renderable.DoesNotExist()"));
}
