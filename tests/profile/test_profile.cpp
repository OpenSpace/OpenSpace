/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include "test_common.h"
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>

#include <openspace/scene/assetloader.h>
#include <openspace/scene/asset.h>
#include "openspace/scene/profile.h"
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/exception.h>
#include <ghoul/lua/lua_helper.h>

#include <iostream>
#include <iomanip>
#include <memory>

using namespace openspace;

namespace {
    int passTest(lua_State* state) {
        bool* test = reinterpret_cast<bool*>(lua_touserdata(state, lua_upvalueindex(1)));
        *test = true;
        return 0;
    }
} // namespace

class Profile2 : public Profile {
public:
    Profile2(AssetLoader& refAssetLoader) : _assLoader(refAssetLoader) {}
    std::string currentTimeUTC() const override {
        return "2020-02-29T01:23:45.00";
    }
    interaction::NavigationHandler::NavigationState currentCameraState() const override {
        interaction::NavigationHandler::NavigationState n;
        n.anchor = "Earth";
        n.aim = "Sun";
        n.referenceFrame = "root";
        n.position = {-1.0, -2.0, -3.0};
        n.up = {0.0, 0.0, 1.0};
        n.pitch = 0.0;
        n.yaw = 0.0;
        return n;
    }
    void addPropertiesMarkedAsChanged(std::string formattedLine) {
        _scenegraphProps.push_back(formattedLine);
    }
    std::vector<std::string> changedPropertiesFormatted() override {
        std::vector<std::string> formattedLines;
        for (std::string s : _scenegraphProps) {
            formattedLines.push_back(s);
        }
        return formattedLines;
    }
    bool usingProfile() const override {
        return true;
    }
    std::string initialProfile() const override {
        return _initProfile;
    }
    void setInitialProfile(std::string file) {
        _initProfile = file;
    }
    std::vector<Profile::AssetEvent> assetEvents() const override {
        return _assLoader.assetEvents();
    }
    void setProfileBaseDirectory(std::string dir) {
        _profileBaseDirectory = dir;
    }
private:
    std::vector<std::string> _scenegraphProps;
    std::string _initProfile;
    AssetLoader& _assLoader;
};

static void addLineHeaderForFailureMessage(std::string& s, size_t lineNumber) {
    s = "@line " + std::to_string(lineNumber) + ": '" + s + "'";
}

TEST_CASE("profile: Convert profileFile to asset", "[profile]") {
    testProfileFormat test = buildTestProfile1();
    std::string testFull_string = stringFromTestProfileFormat(test);

    std::istringstream iss(testFull_string);
    ProfileFile pf("default.profile");
    pf.readIn([&iss](std::string& line) {
        if (getline(iss, line))
            return true;
        else
            return false;
        }
    );

    Profile p;
    REQUIRE_NOTHROW(
        p.convertToScene(pf)
    );
}

TEST_CASE("profile: Verify conversion to scene", "[profile]") {
    std::istringstream iss(newHorizonsProfileInput);
    ProfileFile pf("default.profile");
    pf.readIn([&iss](std::string& line) {
        if (getline(iss, line))
            return true;
        else
            return false;
        }
    );

    Profile p;
    std::string result;
    REQUIRE_NOTHROW(
        result = p.convertToScene(pf)
    );
    
    if (result != newHorizonsExpectedSceneOutput) {
        std::string testing, comparing;
        StringPerLineReader sr_result(result);
        StringPerLineReader sr_standard(newHorizonsExpectedSceneOutput);

        size_t lineN = 1;
        while (sr_result.getNextLine(testing)) {
            sr_standard.getNextLine(comparing);
            addLineHeaderForFailureMessage(testing, lineN);
            addLineHeaderForFailureMessage(comparing, lineN);
            REQUIRE(testing == comparing);
            lineN++;
        }
        //If this fails there are extra lines in the comparison string that weren't in result
        REQUIRE(sr_standard.getNextLine(comparing) == false);
    }
    //REQUIRE(result == newHorizonsExpectedSceneOutput);
}

TEST_CASE("profile: Detect new properties", "[profile]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine.luaState();
    openspace::SynchronizationWatcher syncWatcher;
    AssetLoader assetLoader(
        state,
        &syncWatcher,
        FileSys.absolutePath("${TESTDIR}/profile/")
    );

    bool passed;
    lua_pushlightuserdata(*state, &passed);
    lua_pushcclosure(*state, &passTest, 1);
    lua_setglobal(*state, "passTest");

    Profile2 p(assetLoader);
    p.setProfileBaseDirectory("${TESTDIR}/profile");
    p.setInitialProfile("test2");
    p.addPropertiesMarkedAsChanged("initialized 1st\t123");
    p.addPropertiesMarkedAsChanged("initialized 2nd\t3.14159");
    p.addPropertiesMarkedAsChanged("initialized 3rd\ttested.");
    p.addPropertiesMarkedAsChanged("initialized fourth\tfalse.");
    std::string output = p.saveCurrentSettingsToProfile_string();
    REQUIRE(output == detectChangedPropsResult_1);
}

TEST_CASE("profile: Detect new added assets", "[profile]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine.luaState();
    openspace::SynchronizationWatcher syncWatcher;
    AssetLoader assetLoader(
        state,
        &syncWatcher,
        FileSys.absolutePath("${TESTDIR}/profile/")
    );

    bool passed;
    lua_pushlightuserdata(*state, &passed);
    lua_pushcclosure(*state, &passTest, 1);
    lua_setglobal(*state, "passTest");

    std::shared_ptr<openspace::Asset> asset = assetLoader.add("initialization");
    asset->initialize();
    std::shared_ptr<openspace::Asset> asset2 = assetLoader.add("test2");
    asset2->initialize();
    std::shared_ptr<openspace::Asset> asset3 = assetLoader.add("test3");
    asset3->initialize();

    Profile2 p(assetLoader);
    p.setProfileBaseDirectory("${TESTDIR}/profile");
    p.setInitialProfile("test2");
    std::string output = p.saveCurrentSettingsToProfile_string();
    REQUIRE(output == detectChangedAssetsResult_1);
}

TEST_CASE("profile: Detect new added assets after reset", "[profile]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine.luaState();
    openspace::SynchronizationWatcher syncWatcher;
    AssetLoader assetLoader(
        state,
        &syncWatcher,
        FileSys.absolutePath("${TESTDIR}/profile/")
    );

    bool passed;
    lua_pushlightuserdata(*state, &passed);
    lua_pushcclosure(*state, &passTest, 1);
    lua_setglobal(*state, "passTest");

    std::shared_ptr<openspace::Asset> asset = assetLoader.add("initialization");
    asset->initialize();
    std::shared_ptr<openspace::Asset> asset2 = assetLoader.add("test2");
    asset2->initialize();
    assetLoader.resetAssetEvents();
    std::shared_ptr<openspace::Asset> asset3 = assetLoader.add("test3");
    asset3->initialize();
    std::shared_ptr<openspace::Asset> asset4 = assetLoader.add("test4");
    asset4->initialize();

    Profile2 p(assetLoader);
    p.setProfileBaseDirectory("${TESTDIR}/profile");
    p.setInitialProfile("test2");
    std::string output = p.saveCurrentSettingsToProfile_string();
    REQUIRE(output == detectChangedAssetsResult_2);
}

TEST_CASE("profile: Detect repeat added assets from new", "[profile]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine.luaState();
    openspace::SynchronizationWatcher syncWatcher;
    AssetLoader assetLoader(
        state,
        &syncWatcher,
        FileSys.absolutePath("${TESTDIR}/profile/")
    );

    bool passed;
    lua_pushlightuserdata(*state, &passed);
    lua_pushcclosure(*state, &passTest, 1);
    lua_setglobal(*state, "passTest");

    std::shared_ptr<openspace::Asset> asset = assetLoader.add("test2");
    asset->initialize();
    std::shared_ptr<openspace::Asset> asset2 = assetLoader.add("test4");
    asset2->initialize();
    std::shared_ptr<openspace::Asset> asset3 = assetLoader.add("test2");
    asset3->initialize();

    Profile2 p(assetLoader);
    p.setProfileBaseDirectory("${TESTDIR}/profile");
    p.setInitialProfile("test2");
    std::string output = p.saveCurrentSettingsToProfile_string();
    REQUIRE(output == detectChangedAssetsResult_3);
}

TEST_CASE("profile: Detect repeat added assets from base", "[profile]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine.luaState();
    openspace::SynchronizationWatcher syncWatcher;
    AssetLoader assetLoader(
        state,
        &syncWatcher,
        FileSys.absolutePath("${TESTDIR}/profile/")
    );

    bool passed;
    lua_pushlightuserdata(*state, &passed);
    lua_pushcclosure(*state, &passTest, 1);
    lua_setglobal(*state, "passTest");

    std::shared_ptr<openspace::Asset> asset = assetLoader.add("test2");
    asset->initialize();
    std::shared_ptr<openspace::Asset> asset2 = assetLoader.add("test4");
    asset2->initialize();
    std::shared_ptr<openspace::Asset> asset3 = assetLoader.add("scene/solarsystem/planets/earth/earth");
    asset3->initialize();

    Profile2 p(assetLoader);
    p.setProfileBaseDirectory("${TESTDIR}/profile");
    p.setInitialProfile("test2");
    std::string output = p.saveCurrentSettingsToProfile_string();
    REQUIRE(output == detectChangedAssetsResult_4);
}

TEST_CASE("profile: Detect removed assets not already loaded", "[profile]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine.luaState();
    openspace::SynchronizationWatcher syncWatcher;
    AssetLoader assetLoader(
        state,
        &syncWatcher,
        FileSys.absolutePath("${TESTDIR}/profile/")
    );

    bool passed;
    lua_pushlightuserdata(*state, &passed);
    lua_pushcclosure(*state, &passTest, 1);
    lua_setglobal(*state, "passTest");

    std::shared_ptr<openspace::Asset> asset = assetLoader.add("test2");
    asset->initialize();
    std::shared_ptr<openspace::Asset> asset2 = assetLoader.add("test4");
    asset2->initialize();
    assetLoader.remove("test5");

    Profile2 p(assetLoader);
    p.setProfileBaseDirectory("${TESTDIR}/profile");
    p.setInitialProfile("test2");
    std::string output = p.saveCurrentSettingsToProfile_string();
    REQUIRE(output == detectChangedAssetsResult_5);
}

TEST_CASE("profile: Detect removed assets from already loaded", "[profile]") {
    openspace::Scene scene(std::make_unique<openspace::SingleThreadedSceneInitializer>());
    ghoul::lua::LuaState* state = openspace::global::scriptEngine.luaState();
    openspace::SynchronizationWatcher syncWatcher;
    AssetLoader assetLoader(
        state,
        &syncWatcher,
        FileSys.absolutePath("${TESTDIR}/profile/")
    );

    bool passed;
    lua_pushlightuserdata(*state, &passed);
    lua_pushcclosure(*state, &passTest, 1);
    lua_setglobal(*state, "passTest");

    std::shared_ptr<openspace::Asset> asset = assetLoader.add("test2");
    asset->initialize();
    std::shared_ptr<openspace::Asset> asset2 = assetLoader.add("test4");
    asset2->initialize();
    assetLoader.remove("scene/solarsystem/planets/earth/earth");
    assetLoader.remove("scene/solarsystem/planets/earth/satellites/satellites");

    Profile2 p(assetLoader);
    p.setProfileBaseDirectory("${TESTDIR}/profile");
    p.setInitialProfile("test2");
    std::string output = p.saveCurrentSettingsToProfile_string();
    REQUIRE(output == detectChangedAssetsResult_6);
}
