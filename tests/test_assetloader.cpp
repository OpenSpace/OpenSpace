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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/scene/assetloader.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/synchronizationwatcher.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <exception>
#include <memory>

class AssetLoaderTest;

namespace {
    int passTest(lua_State* state) {
        bool* test = reinterpret_cast<bool*>(lua_touserdata(state, lua_upvalueindex(1)));
        *test = true;
        return 0;
    }
} // namespace

TEST_CASE("AssetLoader: Assertion", "[assetloader]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine->luaState();
    openspace::SynchronizationWatcher syncWatcher;
    openspace::AssetLoader assetLoader(
        state,
        &syncWatcher,
        absPath("${TESTDIR}/AssetLoaderTest/").string()
    );

    REQUIRE_NOTHROW(assetLoader.add("passassertion"));
    REQUIRE_NOTHROW(assetLoader.add("failassertion"));
}

TEST_CASE("AssetLoader: Basic Export Import", "[assetloader]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine->luaState();
    openspace::SynchronizationWatcher syncWatcher;
    openspace::AssetLoader assetLoader(
        state,
        &syncWatcher,
        absPath("${TESTDIR}/AssetLoaderTest/").string()
    );

    REQUIRE_NOTHROW(assetLoader.add("require"));
}

TEST_CASE("AssetLoader: Asset Functions", "[assetloader]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine->luaState();
    openspace::SynchronizationWatcher syncWatcher;
    openspace::AssetLoader assetLoader(
        state,
        &syncWatcher,
        absPath("${TESTDIR}/AssetLoaderTest/").string()
    );

    REQUIRE_NOTHROW(assetLoader.add("assetfunctionsexist"));
}

TEST_CASE("AssetLoader: Asset Initialization", "[assetloader]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine->luaState();
    openspace::SynchronizationWatcher syncWatcher;
    openspace::AssetLoader assetLoader(
        state,
        &syncWatcher,
        absPath("${TESTDIR}/AssetLoaderTest/").string()
    );

    bool passed;
    lua_pushlightuserdata(*state, &passed);
    lua_pushcclosure(*state, &passTest, 1);
    lua_setglobal(*state, "passTest");

    std::shared_ptr<openspace::Asset> asset = assetLoader.add("initialization");
    REQUIRE_NOTHROW(asset->initialize());
    REQUIRE(passed);
}
