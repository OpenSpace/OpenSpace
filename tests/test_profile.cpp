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

#include <openspace/navigation/navigationstate.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/scene/profile.h>
#include <ghoul/filesystem/filesystem.h>
#include <filesystem>
#include <fstream>
#include <json/json.hpp>

using namespace openspace;

namespace {
    /// Loads the contents of the file and creates a profile from it
    Profile loadProfile(const std::string& filename) {
        if (!std::filesystem::exists(absPath(filename))) {
            throw std::runtime_error("Could not find file)");
        }

        std::ifstream f(absPath(filename));
        std::string content(
            (std::istreambuf_iterator<char>(f)),
            std::istreambuf_iterator<char>()
        );

        return Profile(content);
    }

    /// Loads the contents of the file and returns the raw contents
    std::string loadFile(const std::string& filename) {
        std::ifstream f(absPath(filename));
        std::string content(
            (std::istreambuf_iterator<char>(f)),
            std::istreambuf_iterator<char>()
        );
        return content;
    }
} // namespace

//
// Minimal
// The absolute minimal profile that can be loaded
//
TEST_CASE("Minimal", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/minimal.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

//
// Basic functionality
//
TEST_CASE("Basic Meta (full)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/meta_full.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Meta (empty)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/meta_empty.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Meta (no name)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/meta_no_name.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Meta (no version)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/meta_no_version.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Meta (no description)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/meta_no_description.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Meta (no author)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/meta_no_author.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Meta (no url)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/meta_no_url.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Meta (no license)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/meta_no_license.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Module", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/modules.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Assets", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/assets.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Properties", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/properties.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Keybindings", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/keybindings.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Time Relative", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/time_relative.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Time Absolute", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/time_absolute.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Delta Times", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/deltatimes.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera NavState (full)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/camera_navstate_full.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera NavState (no aim)", "[profile]") {
    constexpr const char* File =
        "${TESTDIR}/profile/basic/camera_navstate_no_aim.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera NavState (no pitch)", "[profile]") {
    constexpr const char* File =
        "${TESTDIR}/profile/basic/camera_navstate_no_pitch.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera NavState (no up)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/camera_navstate_no_up.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera NavState (no yaw)", "[profile]") {
    constexpr const char* File =
        "${TESTDIR}/profile/basic/camera_navstate_no_yaw.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera GoToGeo (full)", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/camera_gotogeo.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera GoToGeo (with altitude)", "[profile]") {
    constexpr const char* File =
        "${TESTDIR}/profile/basic/camera_gotogeo_altitude.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Mark Nodes", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/basic/mark_nodes.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Additional Scripts", "[profile]") {
    constexpr const char* File =
        "${TESTDIR}/profile/basic/additional_scripts.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

//
// Integration
//
TEST_CASE("Integration Full Test", "[profile]") {
    constexpr const char* File = "${TESTDIR}/profile/integration/full_test.profile";
    Profile p = loadProfile(File);

    std::string serialized = p.serialize();
    std::string contents = loadFile(File);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

//
// Adding assets
//
TEST_CASE("Add asset to empty Profile", "[profile]") {
    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 10;
    source["version"]["minor"] = 11;

    Profile p(source.dump());
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    nlohmann::json target = source;
    target["assets"] = nlohmann::json::array();
    target["assets"].push_back("new-asset");
    std::string targetSerialized(Profile(target.dump()).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add asset to empty Profile (ignored)", "[profile]") {
    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 10;
    source["version"]["minor"] = 11;

    Profile p(source.dump());
    p.ignoreUpdates = true;
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    std::string targetSerialized(Profile(source.dump()).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add asset to not-empty Profile", "[profile]") {
    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 10;
    source["version"]["minor"] = 11;
    source["assets"] = nlohmann::json::array();
    source["assets"].push_back("old-asset");

    Profile p(source.dump());
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    nlohmann::json target = source;
    target["assets"].push_back("new-asset");
    std::string targetSerialized(Profile(target.dump()).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add asset to not-empty Profile (ignored)", "[profile]") {
    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 10;
    source["version"]["minor"] = 11;
    source["assets"] = nlohmann::json::array();
    source["assets"].push_back("old-asset");

    Profile p(source.dump());
    p.ignoreUpdates = true;
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    std::string targetSerialized(Profile(source.dump()).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add duplicate asset", "[profile]") {
    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 10;
    source["version"]["minor"] = 11;
    source["assets"] = nlohmann::json::array();
    source["assets"].push_back("old-asset");

    Profile p(source.dump());
    p.addAsset("new-asset");
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    nlohmann::json targetSource = source;
    targetSource["assets"].push_back("new-asset");
    std::string targetSerialized(Profile(targetSource.dump()).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

//
// Removing assets
//
TEST_CASE("Remove asset", "[profile]") {
    nlohmann::json target;
    target["version"] = nlohmann::json::object();
    target["version"]["major"] = 99;
    target["version"]["minor"] = 88;
    target["assets"] = nlohmann::json::array();
    target["assets"].push_back("asset1");

    nlohmann::json source = target;
    source["assets"].push_back("asset2");

    Profile p(source.dump());
    p.removeAsset("asset2");
    std::string originalSerialized = p.serialize();

    std::string targetSerialized(Profile(target.dump()).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Remove asset (ignored)", "[profile]") {
    nlohmann::json target;
    target["version"] = nlohmann::json::object();
    target["version"]["major"] = 99;
    target["version"]["minor"] = 88;
    target["assets"] = nlohmann::json::array();
    target["assets"].push_back("asset1");

    nlohmann::json source = target;
    source["assets"].push_back("asset2");


    Profile p(source.dump());
    p.ignoreUpdates = true;
    p.removeAsset("asset2");
    std::string originalSerialized = p.serialize();

    std::string targetSerialized(Profile(source.dump()).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Removing non-exisiting asset", "[profile]") {
    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 66;
    source["version"]["minor"] = 67;
    source["assets"] = nlohmann::json::array();
    source["assets"].push_back("asset1");
    source["assets"].push_back("asset3");

    Profile p(source.dump());
    REQUIRE_THROWS_WITH(
        p.removeAsset("unknown-asset"),
        Catch::Matchers::Contains("Tried to remove non-existing asset 'unknown-asset'")
    );
}

TEST_CASE("Removing non-exisiting asset (ignored)", "[profile]") {
    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 66;
    source["version"]["minor"] = 67;
    source["assets"] = nlohmann::json::array();
    source["assets"].push_back("asset1");
    source["assets"].push_back("asset3");

    Profile p(source.dump());
    p.ignoreUpdates = true;
    REQUIRE_NOTHROW(p.removeAsset("unknown-asset"));
}

//
// Save settings to profile
//
TEST_CASE("Save settings to profile", "[profile]") {
    properties::PropertyOwner owner({ "base" });
    properties::FloatProperty p1(properties::Property::PropertyInfo("p1", "a", "b"), 1.f);
    owner.addProperty(p1);
    properties::StringProperty p2(properties::Property::PropertyInfo("p2", "c", "d"));
    owner.addProperty(p2);

    p1 = 2.f;
    p2 = "test-string";

    interaction::NavigationState state;
    state.anchor = "anchor";
    state.aim = "aim";
    state.referenceFrame = "refFrame";
    state.position = glm::dvec3(1.0, 2.0, 3.0);
    state.up = glm::dvec3(4.0, 5.0, 6.0);
    state.yaw = -1.0;
    state.pitch = -2.0;

    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 1;
    source["version"]["minor"] = 0;
    Profile p(source.dump());
    p.saveCurrentSettingsToProfile(owner, "current-time", state);
    std::string serialized = p.serialize();

    nlohmann::json properties = nlohmann::json::array();
    properties.push_back({
        { "type", "setPropertyValueSingle" },
        { "name", "base.p1" },
        { "value", "2.000000" }
    });
    properties.push_back({
        { "type", "setPropertyValueSingle" },
        { "name", "base.p2" },
        { "value", "\"test-string\"" }
    });

    const nlohmann::json camera = {
        { "type", "setNavigationState" },
        { "anchor", "anchor" },
        { "aim", "aim" },
        { "frame", "refFrame" },
        { "position", { { "x", 1.0 }, { "y", 2.0 }, { "z", 3.0 } } },
        { "up", { { "x", 4.0 }, { "y", 5.0 }, { "z", 6.0 } } },
        { "yaw", -1.0 },
        { "pitch", -2.0 }
    };
    const nlohmann::json time = {
        { "type", "absolute" } ,
        { "value", "current-time" }
    };
    nlohmann::json target = source;
    target["properties"] = properties;
    target["camera"] = camera;
    target["time"] = time;

    std::string targetSerialized = Profile(target.dump()).serialize();

    REQUIRE(serialized == targetSerialized);
}

//
// Error states
//
//
// Version
//
TEST_CASE("(Error) Version: Missing value 'major'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/version/missing_major.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'version.major' field is missing")
    );
}

TEST_CASE("(Error) Version: Missing value 'minor'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/version/missing_minor.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'version.minor' field is missing")
    );
}

TEST_CASE("(Error) Version: Wrong type 'major'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/version/wrongtype_major.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'version.major' must be a number")
    );
}

TEST_CASE("(Error) Version: Wrong type 'minor'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/version/wrongtype_minor.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'version.minor' must be a number")
    );
}

TEST_CASE("(Error) Version: Wrong type 'major' and 'minor'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/version/wrongtype_major_minor.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'version.major' must be a number")
    );
}


//
// Modules
//

TEST_CASE("(Error) Module: Missing value 'name'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/module/missing_name.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'module.name' field is missing")
    );
}

TEST_CASE("(Error) Module: Wrong type 'name'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/module/wrongtype_name.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'module.name' must be a string")
    );
}

TEST_CASE("(Error) Module: Wrong type 'loadedInstruction'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/module/wrongtype_loadedInstruction.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'module.loadedInstruction' must be a string")
    );
}

TEST_CASE("(Error) Module: Wrong type 'notLoadedInstruction'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/module/wrongtype_notLoadedInstruction.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'module.notLoadedInstruction' must be a string")
    );
}


//
// Property
//
TEST_CASE("(Error) Property: Missing value 'name'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/property/missing_name.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'property.name' field is missing")
    );
}

TEST_CASE("(Error) Property: Missing value 'value'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/property/missing_value.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'property.value' field is missing")
    );
}

TEST_CASE("(Error) Property: Missing value 'name' and 'value'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/property/missing_name_value.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'property.name' field is missing")
    );
}

TEST_CASE("(Error) Property: Wrong value 'type'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/property/wrongvalue_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Unknown property set type")
    );
}

TEST_CASE("(Error) Property: Wrong type 'name'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/property/wrongtype_name.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'property.name' must be a string")
    );
}

TEST_CASE("(Error) Property: Wrong type 'value'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/property/wrongtype_value.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'property.value' must be a string")
    );
}


//
// Keybinding
//
TEST_CASE("(Error) Keybinding: Missing value 'key'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_key.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.key' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Missing value 'documentation'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_documentation.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.documentation' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Missing value 'name'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_name.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.name' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Missing value 'gui_path'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_guipath.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.gui_path' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Missing value 'is_local'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_islocal.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.is_local' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Wrong value 'key'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongvalue_key.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Could not find key for 'F50'")
    );
}

TEST_CASE("(Error) Keybinding: Wrong value 'key, modifier'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongvalue_modifier.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Unknown modifier key 'KEYKEY'")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'documentation'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_documentation.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.documentation' must be a string")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'gui_path'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_guipath.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.gui_path' must be a string")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'is_local'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_islocal.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.is_local' must be a boolean")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'name'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_name.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.name' must be a string")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'script'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_script.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'keybinding.script' must be a string")
    );
}


//
// Time
//
TEST_CASE("(Error) Time: Wrong value 'type'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/time/wrongvalue_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Unknown time type")
    );
}

TEST_CASE("(Error) Time (absolute): Missing value 'type'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/time/missing_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'time.type' field is missing")
    );
}

TEST_CASE("(Error) Time (relative): Missing value 'value'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/time/relative_missing_value.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'time.value' field is missing")
    );
}

//
// Deltatime
//
TEST_CASE("(Error) Deltatimes: Wrong type", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/deltatimes/wrongtype_value.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("type must be number, but is string")
    );
}

//
// Camera
//
TEST_CASE("(Error) Camera: Wrong value 'type'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/wrongvalue_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Unknown camera type")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'anchor'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_anchor.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.anchor' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'frame'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_frame.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.frame' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'position'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_position.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.position' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'anchor'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_anchor.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.anchor' must be a string")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'aim'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_aim.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.aim' must be a string")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'frame'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_frame.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.frame' must be a string")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'position'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_position.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.position' must be an object")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'position.x'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_position_x.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.position.x' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'position.x'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_position_x.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.position.x' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'position.y'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_position_y.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.position.y' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'position.y'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_position_y.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.position.y' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'position.z'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_position_z.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.position.z' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'position.z'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_position_z.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.position.z' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'up'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_up.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.up' must be an object")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'up.x'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_up_x.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.up.x' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'up.x'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_up_x.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.up.x' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'up.y'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_up_y.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.up.y' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'up.y'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_up_y.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.up.y' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'up.z'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_up_z.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.up.z' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'up.z'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_up_z.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.up.z' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'yaw'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_yaw.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.yaw' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'pitch'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_pitch.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("camera.pitch' must be a number")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Missing value 'anchor'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_missing_anchor.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.anchor' field is missing")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Missing value 'latitude'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_missing_latitude.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.latitude' field is missing")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Missing value 'longitude'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_missing_longitude.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.longitude' field is missing")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Wrong type 'anchor'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_wrongtype_anchor.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.anchor' must be a string")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Wrong type 'latitude'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_wrongtype_latitude.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.latitude' must be a number")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Wrong type 'longitude'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_wrongtype_longitude.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.longitude' must be a number")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Wrong type 'altitude'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_wrongtype_altitude.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("'camera.altitude' must be a number")
    );
}
