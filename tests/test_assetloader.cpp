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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <exception>
#include <memory>

TEST_CASE("AssetLoader: Assertion", "[assetloader]") {
    using namespace openspace;

    const Scene scene = Scene(std::make_unique<SceneInitializer>());
    ghoul::lua::LuaState* state = global::scriptEngine->luaState();
    AssetManager assetLoader(state, absPath("${TESTDIR}/AssetLoaderTest/"));

    CHECK_NOTHROW(assetLoader.add("passassertion"));
    CHECK_NOTHROW(assetLoader.add("failassertion"));
}

TEST_CASE("AssetLoader: Basic Export Import", "[assetloader]") {
    using namespace openspace;

    Scene scene = Scene(std::make_unique<SceneInitializer>());
    ghoul::lua::LuaState* state = global::scriptEngine->luaState();
    AssetManager assetLoader(state, absPath("${TESTDIR}/AssetLoaderTest/"));

    CHECK_NOTHROW(assetLoader.add("require"));
}

TEST_CASE("AssetLoader: Asset Functions", "[assetloader]") {
    using namespace openspace;

    const Scene scene = Scene(std::make_unique<SceneInitializer>(1u));
    ghoul::lua::LuaState* state = global::scriptEngine->luaState();
    AssetManager assetLoader(state, absPath("${TESTDIR}/AssetLoaderTest/"));

    CHECK_NOTHROW(assetLoader.add("assetfunctionsexist"));
}
