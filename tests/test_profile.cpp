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

using namespace openspace;

namespace {
    Profile loadProfile(const std::string& filename) {
        if (!std::filesystem::exists(absPath(filename))) {
            throw std::runtime_error("Could not find file)");
        }

        std::ifstream f(absPath(filename));

        std::vector<std::string> lines;
        std::string line;
        while (std::getline(f, line)) {
            lines.push_back(std::move(line));
        }

        return Profile(lines);
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

    REQUIRE(serialized == contents);
}

//
// Basic functionality
//
TEST_CASE("Basic Version One Component", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/basic_version_one_component.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    constexpr const char* contents = "#Version\n100.0\n";
    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Version Two Components", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/basic_version_two_components.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    constexpr const char* contents = "#Version\n100.200\n";
    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Module", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_modules.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Assets", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_assets.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Properties", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_properties.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Keybindings", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_keybindings.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Time Relative", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_time_relative.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Time Absolute", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_time_absolute.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Camera NavState", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_camera_navstate.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Camera GoToGeo", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_camera_gotogeo.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Camera GoToGeo altitude", "[profile]") {
    constexpr const char* TestFile = 
        "${TESTDIR}/profile/basic_camera_gotogeo_altitude.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Mark Nodes", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/basic_mark_nodes.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Additional Scripts", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/basic_additional_scripts.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

//
// Integration
//
TEST_CASE("Integration Full Test", "[profile]") {
    constexpr const char* TestFile = "${TESTDIR}/profile/integration_full_test.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    std::string contents = loadFile(TestFile);

    REQUIRE(serialized == contents);
}

TEST_CASE("Integration Full Test Permutation 1", "[profile]") {
    constexpr const char* TestFileOriginal =
        "${TESTDIR}/profile/integration_full_test_permutation_base.profile";
    constexpr const char* TestFilePermutation =
        "${TESTDIR}/profile/integration_full_test_permutation_1.profile";
    Profile original = loadProfile(TestFileOriginal);
    Profile permutation = loadProfile(TestFilePermutation);

    std::string originalSerialized = original.serialize();
    std::string permutationSerialized = permutation.serialize();
    REQUIRE(originalSerialized == permutationSerialized);
}

//
// Adding assets
//
TEST_CASE("Add asset to empty Profile", "[profile]") {
    std::vector<std::string> originalSource = {
        "#Version",
        "10.11"
    };
    Profile p(originalSource);
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    std::vector<std::string> targetSource = originalSource;
    targetSource.push_back("");
    targetSource.push_back("#Asset");
    targetSource.push_back("new-asset\t");
    std::string targetSerialized(Profile(targetSource).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add asset to empty Profile (ignored)", "[profile]") {
    std::vector<std::string> source = {
        "#Version",
        "10.11"
    };
    Profile p(source);
    p.setIgnoreUpdates(true);
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    std::string targetSerialized(Profile(source).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add asset to not-empty Profile", "[profile]") {
    std::vector<std::string> originalSource = {
        "#Version",
        "10.11",
        "",
        "#Asset",
        "old-asset\t"
    };
    Profile p(originalSource);
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    std::vector<std::string> targetSource = originalSource;
    targetSource.push_back("new-asset\t");
    std::string targetSerialized(Profile(targetSource).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add asset to not-empty Profile (ignored)", "[profile]") {
    std::vector<std::string> source = {
        "#Version",
        "10.11",
        "",
        "#Asset",
        "old-asset\t"
    };
    Profile p(source);
    p.setIgnoreUpdates(true);
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    std::string targetSerialized(Profile(source).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Add duplicate asset", "[profile]") {
    std::vector<std::string> originalSource = {
        "#Version",
        "10.11",
        "",
        "#Asset",
        "old-asset\t"
    };
    Profile p(originalSource);
    p.addAsset("new-asset");
    p.addAsset("new-asset");
    std::string originalSerialized = p.serialize();

    std::vector<std::string> targetSource = originalSource;
    targetSource.push_back("new-asset\t");
    std::string targetSerialized(Profile(targetSource).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

//
// Removing assets
//
TEST_CASE("Remove asset", "[profile]") {
    std::vector<std::string> targetSource = {
        "#Version",
        "99.88",
        "",
        "#Asset",
        "asset1\t"
    };
    std::vector<std::string> originalSource = targetSource;
    originalSource.push_back("asset2\t");

    Profile p(originalSource);
    p.removeAsset("asset2");
    std::string originalSerialized = p.serialize();

    std::string targetSerialized(Profile(targetSource).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Remove asset (ignored)", "[profile]") {
    std::vector<std::string> source = {
        "#Version",
        "99.88",
        "",
        "#Asset",
        "asset1\t",
        "asset2\t"
    };

    Profile p(source);
    p.setIgnoreUpdates(true);
    p.removeAsset("asset2");
    std::string originalSerialized = p.serialize();

    std::string targetSerialized(Profile(source).serialize());

    REQUIRE(originalSerialized == targetSerialized);
}

TEST_CASE("Removing non-exisiting asset", "[profile]") {
    std::vector<std::string> source = {
        "#Version",
        "66.67",
        "",
        "#Asset",
        "asset1\t",
        "asset3\t"
    };

    Profile p(source);
    REQUIRE_THROWS_WITH(
        p.removeAsset("unknown-asset"),
        Catch::Matchers::Contains("Tried to remove non-existing asset 'unknown-asset'")
    );
}

TEST_CASE("Removing non-exisiting asset (ignored)", "[profile]") {
    std::vector<std::string> source = {
        "#Version",
        "66.67",
        "",
        "#Asset",
        "asset1\t",
        "asset3\t"
    };

    Profile p(source);
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

    std::vector<std::string> baseSource = {
        "#Version",
        "1.0"
    };
    Profile p(baseSource);
    p.saveCurrentSettingsToProfile(owner, "current-time", state);
    std::string serialized = p.serialize();

    std::vector<std::string> targetSource = baseSource;
    targetSource.push_back("");
    targetSource.push_back("#Property");
    targetSource.push_back("setPropertyValueSingle\tbase.p1\t2.000000");
    targetSource.push_back("setPropertyValueSingle\tbase.p2\t\"test-string\"");
    targetSource.push_back("");
    targetSource.push_back("#Camera");
    targetSource.push_back(
        "setNavigationState\tanchor\taim\trefFrame\t1.0, 2.0, 3.0\t4.0, 5.0, 6.0\t"
        "-1.0\t-2.0"
    );
    targetSource.push_back("");
    targetSource.push_back("#Time");
    targetSource.push_back("absolute\tcurrent-time");

    std::string targetSerialized = Profile(targetSource).serialize();

    REQUIRE(serialized == targetSerialized);
}

//
// Error states
//
TEST_CASE("Error unrecognized header", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_unrecognized_header.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Invalid section header") &&
        Catch::Matchers::Contains("#Azzet")
    );
}

TEST_CASE("Error missing separator line", "[profile}") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_missing_separator_line.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Sections in profile must be separated by empty lines")
    );
}

TEST_CASE("Error version not first header", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_version_not_first.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("First header in the file must be Version") &&
        Catch::Matchers::Contains("#Asset")
    );
}

TEST_CASE("Error two version sections", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_two_version_sections.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Version section can only appear once per profile")
    );
}

TEST_CASE("Error two camera sections", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_two_camera_sections.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Camera section can only appear once per profile")
    );
}

TEST_CASE("Error two time sections", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_two_time_sections.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Time section can only appear once per profile")
    );
}

TEST_CASE("Error version malformed component", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_version_malformed_component.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Error parsing Version. Version number is not a number")
    );
}

TEST_CASE("Error version too many components", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_version_too_many_components.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 1-2 version components, got 3")
    );
}

TEST_CASE("Error module too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_module_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 3 fields in a Module entry, got 1")
    );
}

TEST_CASE("Error module too many parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_module_too_many_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 3 fields in a Module entry, got 4")
    );
}

TEST_CASE("Error asset too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_asset_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 2 fields in an Asset entry, got 1")
    );
}

TEST_CASE("Error asset too many parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_asset_too_many_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 2 fields in an Asset entry, got 3")
    );
}

TEST_CASE("Error property too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_property_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 3 fields in Property entry, got 1")
    );
}

TEST_CASE("Error property too many parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_property_too_many_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 3 fields in Property entry, got 4")
    );
}

TEST_CASE("Error property wrong parameter type 'type'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_property_wrong_parameter_value_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains(
            "Expected property set type 'setPropertyValue' or "
            "'setPropertyValueSingle', got 'unknown-set-property-command'"
        )
    );
}

TEST_CASE("Error keybinding too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_keybinding_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 6 fields in Keybinding entry, got 1")
    );
}

TEST_CASE("Error keybinding too many parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_keybinding_too_many_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 6 fields in Keybinding entry, got 7")
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
        Catch::Matchers::Contains("Expected 'false' or 'true' for the local path, got ER")
    );
}

TEST_CASE("Error time too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_time_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 2 fields in Time entry, got 1")
    );
}

TEST_CASE("Error time too many parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_time_too_many_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 2 fields in Time entry, got 3")
    );
}

TEST_CASE("Error time wrong parameter type 'type'", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_time_wrong_parameter_value_type.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains(
            "Expected 'absolute' or 'relative' for the type, got ER"
        )
    );
}

TEST_CASE("Error camera navigation state too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_camera_navstate_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 8 fields in the Camera entry, got 1")
    );
}

TEST_CASE("Error camera navigation state too many parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_camera_navstate_too_many_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 8 fields in the Camera entry, got 9")
    );
}

TEST_CASE("Error camera navigation state too few parameters in position", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_too_few_components_position.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 3 fields for the camera's position, got 2")
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
        Catch::Matchers::Contains("Camera's position components must be numbers")
    );
}

TEST_CASE("Error camera navigation state too few parameters in up vector", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_too_few_components_up.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains(
            "Expected 0 or 3 fields for the camera's up vector, got 2"
        )
    );
}

TEST_CASE("Error camera navigation state wrong parameter type up vector", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_wrong_component_type_up.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Camera's up vector components must be numbers")
    );
}

TEST_CASE("Error camera navigation state wrong parameter type up yaw", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_type_yaw.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Camera's yaw value must be a number")
    );
}

TEST_CASE("Error camera navigation state wrong parameter type up pitch", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/"
        "error_camera_navstate_wrong_parameter_type_pitch.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Camera's pitch value must be a number")
    );
}

TEST_CASE("Error camera goToGeo too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_camera_gotogeo_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 5 fields in the Camera entry, got 1")
    );
}

TEST_CASE("Error camera goToGeo too many parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_camera_gotogeo_too_many_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 5 fields in the Camera entry, got 6")
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





