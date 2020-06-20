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
//#include "test_common.h"
//#include <openspace/engine/configuration.h>
//#include <openspace/engine/globals.h>
//#include <openspace/engine/openspaceengine.h>
//
//#include <openspace/scene/assetloader.h>
//#include <openspace/scene/asset.h>
//#include "openspace/scene/profile.h"
//#include <openspace/scene/scene.h>
//#include <openspace/scene/scenegraphnode.h>
//#include <openspace/scene/sceneinitializer.h>
//#include <openspace/scripting/scriptengine.h>
//#include <ghoul/misc/exception.h>
//#include <ghoul/lua/lua_helper.h>
//
//#include <iostream>
//#include <iomanip>
//#include <memory>


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
    constexpr const char* contents = "#Version\n100.0.0\n";
    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Version Two Components", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/basic_version_two_components.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    constexpr const char* contents = "#Version\n100.200.0\n";
    REQUIRE(serialized == contents);
}

TEST_CASE("Basic Version Three Components", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/basic_version_three_components.profile";
    Profile p = loadProfile(TestFile);

    std::string serialized = p.serialize();
    constexpr const char* contents = "#Version\n100.200.300\n";
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


//
// Error states
//
TEST_CASE("Error Unrecognized Header", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_unrecognized_header.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Invalid section header") &&
        Catch::Matchers::Contains("#Azzet")
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
        Catch::Matchers::Contains("Expected 1-3 version components, got 4")
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

TEST_CASE("Error profile too few parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_property_too_few_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 3 fields in Property entry, got 1")
    );
}

TEST_CASE("Error profile too many parameters", "[profile]") {
    constexpr const char* TestFile =
        "${TESTDIR}/profile/error_property_too_many_parameters.profile";
    REQUIRE_THROWS_WITH(
        loadProfile(TestFile),
        Catch::Matchers::Contains("Expected 3 fields in Property entry, got 4")
    );
}

TEST_CASE("Error profile wrong parameter type 'type'", "[profile]") {
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





