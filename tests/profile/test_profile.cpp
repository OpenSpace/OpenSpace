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
    std::string getCurrentTimeUTC() {
        return "2020-02-29T01:23:45.00";
    }
    interaction::NavigationHandler::NavigationState getCurrentCameraState() {
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
    std::vector<std::string> getChangedPropertiesFormatted() {
        std::vector<std::string> formattedLines;
        for (std::string s : _scenegraphProps) {
            formattedLines.push_back(s);
        }
        return formattedLines;
    }
    bool usingProfile() {
        return true;
    }
    std::string initialProfile() {
        return _initProfile;
    }
    void setInitialProfile(std::string file) {
        _initProfile = file;
    }
private:
    std::vector<std::string> _scenegraphProps;
    std::string _initProfile;
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

TEST_CASE("profile: Detect new required asset", "[profile]") {
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
    assetLoader.add("test1");

    Profile2 p;
    p.setInitialProfile(FileSys.absolutePath("${TESTDIR}/profile/test2.profile"));
    p.addPropertiesMarkedAsChanged("initialized 1st\t123");
    p.addPropertiesMarkedAsChanged("initialized 2nd\t3.14159");
    p.addPropertiesMarkedAsChanged("initialized 3rd\ttested.");
    std::string output = p.saveCurrentSettingsToProfile_string();
    std::cout << output << std::endl;
}
