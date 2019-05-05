/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include "gtest/gtest.h"
#include <openspace/scene/assetloader.h>
#include <openspace/scene/asset.h>

#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/synchronizationwatcher.h>
#include <openspace/documentation/documentation.h>

#include <openspace/scene/scene.h>

#include <ghoul/lua/lua_helper.h>
#include <fstream>

#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/filesystem/filesystem.h>

#include <exception>
#include <memory>

#include <openspace/engine/globals.h>
//#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>

class AssetLoaderTest;

namespace {
int passTest(lua_State* state);
}

class AssetLoaderTest : public ::testing::Test {
public:
    void pass() {
        _passedTest = true;
    }

    bool passed() {
        return _passedTest;
    }

protected:
    virtual void SetUp() {
        _scene = std::make_unique<openspace::Scene>(
            std::make_unique<openspace::SingleThreadedSceneInitializer>()
        );
        ghoul::lua::LuaState* state = openspace::global::scriptEngine.luaState();
        openspace::global::scriptEngine.initialize();
        _syncWatcher = std::make_unique<openspace::SynchronizationWatcher>();
        _assetLoader = std::make_unique<openspace::AssetLoader>(
            *state,
            _syncWatcher.get(),
            FileSys.absolutePath("${TESTDIR}/AssetLoaderTest/")
        );

        _passedTest = false;
        lua_pushlightuserdata(*state, this);
        lua_pushcclosure(*state, &passTest, 1);
        lua_setglobal(*state, "passTest");
    }

    virtual void TearDown() {
        openspace::global::scriptEngine.deinitialize();
    }

    std::unique_ptr<openspace::Scene> _scene;
    std::unique_ptr<openspace::AssetLoader> _assetLoader;
    std::unique_ptr<openspace::SynchronizationWatcher> _syncWatcher;
    bool _passedTest;
};

namespace {
int passTest(lua_State* state) {
    AssetLoaderTest *test =
        reinterpret_cast<AssetLoaderTest*>(lua_touserdata(state, lua_upvalueindex(1)));
    test->pass();
    return 0;
}
}

TEST_F(AssetLoaderTest, Assertions) {
    EXPECT_NO_THROW(_assetLoader->add("passassertion"));
    EXPECT_NO_THROW(_assetLoader->add("failassertion"));
}

TEST_F(AssetLoaderTest, BasicExportImport) {
    try {
        _assetLoader->add("require");
    }
    catch (const std::exception& e) {
        EXPECT_TRUE(false) << e.what();
    }
}

TEST_F(AssetLoaderTest, AssetFunctions) {
    try {
        _assetLoader->add("assetfunctionsexist");
    } catch (const std::exception& e) {
        EXPECT_TRUE(false) << e.what();
    }
}

TEST_F(AssetLoaderTest, DependencyFunctions) {
    try {
        _assetLoader->add("dependencyfunctionsexist");
    }
    catch (const std::exception& e) {
        EXPECT_TRUE(false) << e.what();
    }
}

TEST_F(AssetLoaderTest, AssetInitialization) {
    try {
        std::shared_ptr<openspace::Asset> asset = _assetLoader->add("initialization");
        asset->initialize();
        EXPECT_TRUE(passed());
    }
    catch (const std::exception& e) {
        EXPECT_TRUE(false) << e.what();
    }
}


