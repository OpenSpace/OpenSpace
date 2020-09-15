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

#include <openspace/interaction/navigationhandler.h>
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
//
TEST_CASE("Minimal", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/minimal.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

//
// Basic functionality
//
TEST_CASE("Basic Version", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_version.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Meta", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_meta.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Module", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_modules.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Assets", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_assets.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Properties", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_properties.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Keybindings", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_keybindings.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Time Relative", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_time_relative.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Time Absolute", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_time_absolute.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Delta Times", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_deltatimes.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera NavState", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_camera_navstate.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera GoToGeo", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_camera_gotogeo.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Camera GoToGeo altitude", "[profile]") {
    constexpr const char* TestFile = 
        "${TESTDIR}/profile/basic_camera_gotogeo_altitude.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Mark Nodes", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_mark_nodes.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

TEST_CASE("Basic Additional Scripts", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/basic_additional_scripts.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(nlohmann::json::parse(serialized) == nlohmann::json::parse(contents));
}

//
// Integration
//
TEST_CASE("Integration Full Test", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/integration_full_test.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

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
    target["assets"].push_back({ { "path", "new-asset" } });
    std::string targetSerialized(Profile(target.dump()).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add asset to empty Profile (ignored)", "[profile]") {
    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 10;
    source["version"]["minor"] = 11;

    Profile p(source.dump());
    p.setIgnoreUpdates(true);
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
    source["assets"].push_back({ { "path", "old-asset" } });

    Profile p(source.dump());
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    nlohmann::json target = source;
    target["assets"].push_back({ { "path", "new-asset" } });
    std::string targetSerialized(Profile(target.dump()).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add asset to not-empty Profile (ignored)", "[profile]") {
    nlohmann::json source;
    source["version"] = nlohmann::json::object();
    source["version"]["major"] = 10;
    source["version"]["minor"] = 11;
    source["assets"] = nlohmann::json::array();
    source["assets"].push_back({ { "path", "old-asset" } });

    Profile p(source.dump());
    p.setIgnoreUpdates(true);
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
    source["assets"].push_back({ { "path", "old-asset" } });

    Profile p(source.dump());
    p.addAsset("new-asset");
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    nlohmann::json targetSource = source;
    targetSource["assets"].push_back({ { "path", "new-asset" } });
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
    target["assets"].push_back({ { "path", "asset1" } });

    nlohmann::json source = target;
    source["assets"].push_back({ { "path", "asset2" } });

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
    target["assets"].push_back({ { "path", "asset1" } });

    nlohmann::json source = target;
    source["assets"].push_back({ { "path", "asset2" } });


    Profile p(source.dump());
    p.setIgnoreUpdates(true);
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
    source["assets"].push_back({ { "path", "asset1" } });
    source["assets"].push_back({ { "path", "asset3" } });

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
    source["assets"].push_back({ { "path", "asset1" } });
    source["assets"].push_back({ { "path", "asset3" } });

    Profile p(source.dump());
    p.setIgnoreUpdates(true);
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

    interaction::NavigationHandler::NavigationState state;
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
TEST_CASE("Error version malformed component", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_version_malformed_component.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("type must be number, but is string")
    );
}

TEST_CASE("Error property too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_property_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("key 'name' not found")
    );
}

TEST_CASE("Error property wrong parameter type 'type'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_property_wrong_parameter_value_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Unknown property set type")
    );
}

TEST_CASE("Error keybinding too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_keybinding_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("key 'documentation' not found")
    );
}

TEST_CASE("Error keybinding wrong parameter value 'key'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_keybinding_wrong_parameter_value_key.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Could not find key for 'F50'")
    );
}

TEST_CASE("Error keybinding wrong parameter value 'key, modifier'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_keybinding_wrong_parameter_value_modifier.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Unknown modifier key 'KEYKEY'")
    );
}

TEST_CASE("Error keybinding wrong parameter type 'local'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_keybinding_wrong_parameter_type_local.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("type must be boolean, but is string")
    );
}

TEST_CASE("Error time too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_time_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("key 'value' not found")
    );
}

TEST_CASE("Error time wrong parameter type 'type'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_time_wrong_parameter_value_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Unknown time type")
    );
}

TEST_CASE("Error deltatimes wrong parameter type", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_deltatimes_wrong_parameter_value_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("type must be number, but is string")
    );
}

TEST_CASE("Error camera navigation state too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_camera_navstate_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("key 'anchor' not found")
    );
}

TEST_CASE("Error camera navigation state too few parameters in position", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_too_few_components_position.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("key 'z' not found")
    );
}

TEST_CASE("Error camera navigation state too many parameters in position", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_too_many_components_position.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 3 fields for the camera's position, got 4")
    );
}

TEST_CASE("Error camera navigation state wrong parameter type position", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_wrong_component_type_position.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("type must be number, but is string")
    );
}

TEST_CASE("Error camera navigation state too few parameters in up vector", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_too_few_components_up.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("key 'z' not found")
    );
}

TEST_CASE("Error camera navigation state wrong parameter type up vector", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_wrong_component_type_up.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("type must be number, but is string")
    );
}

TEST_CASE("Error camera navigation state wrong parameter type up yaw", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_type_yaw.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("type must be number, but is string")
    );
}

TEST_CASE("Error camera navigation state wrong parameter type up pitch", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_type_pitch.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("type must be number, but is string")
    );
}

TEST_CASE("Error camera goToGeo too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_camera_gotogeo_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("key 'anchor' not found")
    );
}

TEST_CASE("Error camera wrong parameter value 'type'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_camera_wrong_parameter_value_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains(
            "Expected 'setNavigationState' or 'goToGeo' for the type, got ER"
        )
    );
}





