/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <catch2/matchers/catch_matchers_exception.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

#include <openspace/navigation/navigationstate.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/scene/profile.h>
#include <ghoul/filesystem/filesystem.h>
#include <filesystem>
#include <fstream>
#include <json/json.hpp>

// clang-tidy is convinced that it is possible to use emplace_back instead of push_back
// for the profiole types, but I haven't been able to convince the Visual Studio
// compiler to agree
// NOLINTBEGIN(modernize-use-emplace)

namespace openspace {
    bool operator==(const openspace::Profile::Version& lhs,
                    const openspace::Profile::Version& rhs) noexcept
    {
        return lhs.major == rhs.major && lhs.minor == rhs.minor;
    }

    bool operator==(const openspace::Profile::Module& lhs,
                    const openspace::Profile::Module& rhs) noexcept
    {
        return lhs.name == rhs.name &&
               lhs.loadedInstruction == rhs.loadedInstruction &&
               lhs.notLoadedInstruction == rhs.notLoadedInstruction;
    }

    bool operator==(const openspace::Profile::Meta& lhs,
                    const openspace::Profile::Meta& rhs) noexcept
    {
        return lhs.name == rhs.name &&
               lhs.version == rhs.version &&
               lhs.description == rhs.description &&
               lhs.author == rhs.author &&
               lhs.url == rhs.url &&
               lhs.license == rhs.license;
    }

    bool operator==(const openspace::Profile::Property& lhs,
                    const openspace::Profile::Property& rhs) noexcept
    {
        return lhs.setType == rhs.setType &&
               lhs.name == rhs.name &&
               lhs.value == rhs.value;
    }

    bool operator==(const openspace::Profile::Action& lhs,
                    const openspace::Profile::Action& rhs) noexcept
    {
        return lhs.identifier == rhs.identifier &&
               lhs.documentation == rhs.documentation &&
               lhs.name == rhs.name &&
               lhs.guiPath == rhs.guiPath &&
               lhs.isLocal == rhs.isLocal &&
               lhs.script == rhs.script;
    }

    bool operator==(const openspace::Profile::Keybinding& lhs,
                    const openspace::Profile::Keybinding& rhs) noexcept
    {
        return lhs.key == rhs.key && lhs.action == rhs.action;
    }

    bool operator==(const openspace::Profile::Time& lhs,
                    const openspace::Profile::Time& rhs) noexcept
    {
        return lhs.type == rhs.type && lhs.value == rhs.value;
    }

    bool operator==(const openspace::Profile::CameraGoToNode& lhs,
        const openspace::Profile::CameraGoToNode& rhs) noexcept
    {
        return lhs.anchor == rhs.anchor && lhs.height == rhs.height;
    }

    bool operator==(const openspace::Profile::CameraNavState& lhs,
                    const openspace::Profile::CameraNavState& rhs) noexcept
    {
        return lhs.anchor == rhs.anchor &&
               lhs.aim == rhs.aim &&
               lhs.referenceFrame == rhs.referenceFrame &&
               lhs.position == rhs.position &&
               lhs.up == rhs.up &&
               lhs.yaw == rhs.yaw &&
               lhs.pitch == rhs.pitch;
    }

    bool operator==(const openspace::Profile::CameraGoToGeo& lhs,
                    const openspace::Profile::CameraGoToGeo& rhs) noexcept
    {
        return lhs.anchor == rhs.anchor &&
               lhs.latitude == rhs.latitude &&
               lhs.longitude == rhs.longitude &&
               lhs.altitude == rhs.altitude;
    }

    bool operator==(const openspace::Profile& lhs,
                    const openspace::Profile& rhs) noexcept
    {
        return lhs.version == rhs.version &&
               lhs.modules == rhs.modules &&
               lhs.meta == rhs.meta &&
               lhs.assets == rhs.assets &&
               lhs.properties == rhs.properties &&
               lhs.actions == rhs.actions &&
               lhs.keybindings == rhs.keybindings &&
               lhs.time == rhs.time &&
               lhs.deltaTimes == rhs.deltaTimes &&
               lhs.camera == rhs.camera &&
               lhs.markNodes == rhs.markNodes &&
               lhs.additionalScripts == rhs.additionalScripts &&
               lhs.ignoreUpdates == rhs.ignoreUpdates;
    }

    std::ostream& operator<<(std::ostream& os, const openspace::Profile& profile) {
        os << profile.serialize();
        return os;
    }
} // namespace openspace

using namespace openspace;

//
// Minimal
// The absolute minimal profile that can be loaded
//
TEST_CASE("Minimal", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/minimal.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;

    CHECK(profile == ref);
}

//
// Basic functionality
//
TEST_CASE("Basic Meta (full)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/meta_full.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.meta = {
        .name = "name",
        .version = "version",
        .description = "description",
        .author = "author",
        .url = "url",
        .license = "license"
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Meta (empty)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/meta_empty.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.meta = Profile::Meta();

    CHECK(profile == ref);
}

TEST_CASE("Basic Meta (no name)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/meta_no_name.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.meta = {
        .version = "version",
        .description = "description",
        .author = "author",
        .url = "url",
        .license = "license"
    };


    CHECK(profile == ref);
}

TEST_CASE("Basic Meta (no version)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/meta_no_version.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.meta = {
        .name = "name",
        .description = "description",
        .author = "author",
        .url = "url",
        .license = "license"
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Meta (no description)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/meta_no_description.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.meta = {
        .name = "name",
        .version = "version",
        .author = "author",
        .url = "url",
        .license = "license"
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Meta (no author)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/meta_no_author.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.meta = {
        .name = "name",
        .version = "version",
        .description = "description",
        .url = "url",
        .license = "license"
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Meta (no url)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/meta_no_url.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.meta = {
        .name = "name",
        .version = "version",
        .description = "description",
        .author = "author",
        .license = "license"
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Meta (no license)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/meta_no_license.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.meta = {
        .name = "name",
        .version = "version",
        .description = "description",
        .author = "author",
        .url = "url"
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Module", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/modules.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.modules.push_back({
        .name = "abs-module"
    });
    ref.modules.push_back({
        .name = "def-module",
        .loadedInstruction = "instr"
    });
    ref.modules.push_back({
        .name = "ghi-module",
        .notLoadedInstruction = "not_instr"
    });
    ref.modules.push_back({
        .name = "jkl-module",
        .loadedInstruction = "instr",
        .notLoadedInstruction = "not_instr"
    });

    CHECK(profile == ref);
}

TEST_CASE("Basic Assets", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/assets.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.assets.emplace_back("folder1/folder2/asset");
    ref.assets.emplace_back("folder3/folder4/asset2");
    ref.assets.emplace_back("folder5/folder6/asset3");

    CHECK(profile == ref);
}

TEST_CASE("Basic Properties", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/properties.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValue,
        .name = "property_name_1",
        .value = "property_value_1"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValue,
        .name = "property_name_2",
        .value = "property_value_2"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValue,
        .name = "property_name_3",
        .value = "property_value_3"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValueSingle,
        .name = "property_name_4",
        .value = "property_value_4"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValueSingle,
        .name = "property_name_5",
        .value = "property_value_5"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValueSingle,
        .name = "property_name_6",
        .value = "property_value_6"
    });

    CHECK(profile == ref);
}

TEST_CASE("Basic Keybindings", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/keybindings.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.actions.push_back({
        .identifier = "profile.keybind.0",
        .documentation = "T documentation",
        .name = "T name",
        .guiPath = "T Gui-Path",
        .isLocal = true,
        .script = "T script"
    });
    ref.keybindings.push_back({
        .key = { Key::T, KeyModifier::None },
        .action = "profile.keybind.0"
    });

    ref.actions.push_back({
        .identifier = "profile.keybind.1",
        .documentation = "U documentation",
        .name = "U name",
        .guiPath = "U Gui-Path",
        .isLocal = false,
        .script = "U script"
    });
    ref.keybindings.push_back({
        .key = { Key::U, KeyModifier::None },
        .action = "profile.keybind.1"
    });

    ref.actions.push_back({
        .identifier = "profile.keybind.2",
        .documentation = "CTRL+V documentation",
        .name = "CTRL+V name",
        .guiPath = "CTRL+V Gui-Path",
        .isLocal = false,
        .script = "CTRL+V script"
    });
    ref.keybindings.push_back({
        .key = { Key::V, KeyModifier::Control },
        .action = "profile.keybind.2"
    });

    CHECK(profile == ref);
}

TEST_CASE("Basic Time Relative", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/time_relative.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.time = {
        .type = Profile::Time::Type::Relative,
        .value = "-1d"
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Time Absolute", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/time_absolute.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.time = {
        .type = Profile::Time::Type::Absolute,
        .value = "2020-06-01T12:00:00"
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Delta Times", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/deltatimes.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.deltaTimes.push_back(1.0);
    ref.deltaTimes.push_back(30.0);
    ref.deltaTimes.push_back(60.0);
    ref.deltaTimes.push_back(1000.0);
    ref.deltaTimes.push_back(36000.0);

    CHECK(profile == ref);
}

TEST_CASE("Basic Camera NavState (full)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/camera_navstate_full.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.camera = Profile::CameraNavState {
        .anchor = "none",
        .aim = "aim",
        .referenceFrame = "root",
        .position = glm::dvec3(1.0, 2.0, 3.0),
        .up = glm::dvec3(4.0, 5.0, 6.0),
        .yaw = 10.0,
        .pitch = -10.0
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Camera NavState (no aim)", "[profile]") {
    constexpr std::string_view File =
        "${TESTDIR}/profile/basic/camera_navstate_no_aim.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.camera = Profile::CameraNavState {
        .anchor = "none",
        .referenceFrame = "root",
        .position = glm::dvec3(1.0, 2.0, 3.0),
        .up = glm::dvec3(4.0, 5.0, 6.0),
        .yaw = 10.0,
        .pitch = -10.0
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Camera NavState (no pitch)", "[profile]") {
    constexpr std::string_view File =
        "${TESTDIR}/profile/basic/camera_navstate_no_pitch.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.camera = Profile::CameraNavState {
        .anchor = "none",
        .aim = "aim",
        .referenceFrame = "root",
        .position = glm::dvec3(1.0, 2.0, 3.0),
        .up = glm::dvec3(4.0, 5.0, 6.0),
        .yaw = 10.0
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Camera NavState (no up)", "[profile]") {
    constexpr std::string_view File =
        "${TESTDIR}/profile/basic/camera_navstate_no_up.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.camera = Profile::CameraNavState {
        .anchor = "none",
        .aim = "aim",
        .referenceFrame = "root",
        .position = glm::dvec3(1.0, 2.0, 3.0),
        .yaw = 10.0,
        .pitch = -10.0
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Camera NavState (no yaw)", "[profile]") {
    constexpr std::string_view File =
        "${TESTDIR}/profile/basic/camera_navstate_no_yaw.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.camera = Profile::CameraNavState {
        .anchor = "none",
        .aim = "aim",
        .referenceFrame = "root",
        .position = glm::dvec3(1.0, 2.0, 3.0),
        .up = glm::dvec3(4.0, 5.0, 6.0),
        .pitch = -10.0,
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Camera GoToGeo (full)", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/camera_gotogeo.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.camera = Profile::CameraGoToGeo {
        .anchor = "anchor",
        .latitude = 1.0,
        .longitude = 2.0
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Camera GoToGeo (with altitude)", "[profile]") {
    constexpr std::string_view File =
        "${TESTDIR}/profile/basic/camera_gotogeo_altitude.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.camera = Profile::CameraGoToGeo {
        .anchor = "anchor",
        .latitude = 1.0,
        .longitude = 2.0,
        .altitude = 4.0
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Camera GoToNode", "[profile]") {
    constexpr std::string_view File =
        "${TESTDIR}/profile/basic/camera_gotonode.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.camera = Profile::CameraGoToNode {
        .anchor = "anchor"
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Camera GoToNode (with height)", "[profile]") {
    constexpr std::string_view File =
        "${TESTDIR}/profile/basic/camera_gotonode_height.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.camera = Profile::CameraGoToNode {
        .anchor = "anchor",
        .height = 100.0
    };

    CHECK(profile == ref);
}

TEST_CASE("Basic Mark Nodes", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/basic/mark_nodes.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.markNodes.emplace_back("node-1");
    ref.markNodes.emplace_back("node-2");
    ref.markNodes.emplace_back("node-3");

    CHECK(profile == ref);
}

TEST_CASE("Basic Additional Scripts", "[profile]") {
    constexpr std::string_view File =
        "${TESTDIR}/profile/basic/additional_scripts.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.additionalScripts.emplace_back("script-1");
    ref.additionalScripts.emplace_back("script-2");
    ref.additionalScripts.emplace_back("script-3");

    CHECK(profile == ref);
}

//
// Integration
//
TEST_CASE("Integration Full Test", "[profile]") {
    constexpr std::string_view File = "${TESTDIR}/profile/integration/full_test.profile";
    Profile profile = Profile(absPath(File));

    Profile ref;
    ref.version = Profile::CurrentVersion;
    ref.meta = {
        .name = "name",
        .version = "version",
        .description = "description",
        .author = "author",
        .url = "url",
        .license = "license"
    };
    ref.modules.push_back({
        .name = "abs-module"
    });
    ref.modules.push_back({
        .name = "def-module",
        .loadedInstruction = "instr"
    });
    ref.modules.push_back({
        .name = "ghi-module",
        .notLoadedInstruction = "not_instr"
    });
    ref.modules.push_back({
        .name = "jkl-module",
        .loadedInstruction = "instr",
        .notLoadedInstruction = "not_instr"
    });
    ref.assets.emplace_back("scene/solarsystem/planets/earth/earth");
    ref.assets.emplace_back("scene/solarsystem/planets/earth/satellites/satellites");
    ref.assets.emplace_back("folder1/folder2/asset");
    ref.assets.emplace_back("folder3/folder4/asset2");
    ref.assets.emplace_back("folder5/folder6/asset3");
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValue,
        .name = "{earth_satellites}.Renderable.Enabled",
        .value = "false"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValue,
        .name = "property_name_1",
        .value = "property_value_1"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValue,
        .name = "property_name_2",
        .value = "property_value_2"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValue,
        .name = "property_name_3",
        .value = "property_value_3"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValueSingle,
        .name = "property_name_4",
        .value = "property_value_4"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValueSingle,
        .name = "property_name_5",
        .value = "property_value_5"
    });
    ref.properties.push_back({
        .setType = Profile::Property::SetType::SetPropertyValueSingle,
        .name = "property_name_6",
        .value = "property_value_6"
    });

    ref.actions.push_back({
        .identifier = "profile.keybind.0",
        .documentation = "T documentation",
        .name = "T name",
        .guiPath = "T Gui-Path",
        .isLocal = true,
        .script = "T script"
    });
    ref.keybindings.push_back({
        .key = { Key::T, KeyModifier::None },
        .action = "profile.keybind.0"
    });

    ref.actions.push_back({
        .identifier = "profile.keybind.1",
        .documentation = "U documentation",
        .name = "U name",
        .guiPath = "U Gui-Path",
        .isLocal = false,
        .script = "U script"
    });
    ref.keybindings.push_back({
        .key = { Key::U, KeyModifier::None },
        .action = "profile.keybind.1"
    });

    ref.actions.push_back({
        .identifier = "profile.keybind.2",
        .documentation = "CTRL+V documentation",
        .name = "CTRL+V name",
        .guiPath = "CTRL+V Gui-Path",
        .isLocal = false,
        .script = "CTRL+V script"
    });
    ref.keybindings.push_back({
        .key = { Key::V, KeyModifier::Control },
        .action = "profile.keybind.2"
    });

    ref.time = {
        .type = Profile::Time::Type::Relative,
        .value = "-1d"
    };

    ref.camera = Profile::CameraGoToGeo {
        .anchor = "Earth",
        .latitude = 58.5877,
        .longitude = 16.1924,
        .altitude = 2.0e+07
    };

    ref.markNodes.emplace_back("Earth");
    ref.markNodes.emplace_back("Mars");
    ref.markNodes.emplace_back("Moon");
    ref.markNodes.emplace_back("Sun");

    ref.additionalScripts.emplace_back("script-1");
    ref.additionalScripts.emplace_back("script-2");
    ref.additionalScripts.emplace_back("script-3");

    CHECK(profile == ref);
}

//
// Adding assets
//
TEST_CASE("Add asset to empty Profile", "[profile]") {
    Profile profile;
    profile.version = Profile::CurrentVersion;

    profile.addAsset("new-asset");

    REQUIRE(profile.assets.size() == 1);
    CHECK(profile.assets[0] == "new-asset");
}

TEST_CASE("Add asset to empty Profile (ignored)", "[profile]") {
    Profile profile;
    profile.version = Profile::CurrentVersion;

    profile.ignoreUpdates = true;
    profile.addAsset("new-asset");

    CHECK(profile.assets.empty());
}

TEST_CASE("Add asset to not-empty Profile", "[profile]") {
    Profile profile;
    profile.version = Profile::CurrentVersion;
    profile.assets.emplace_back("old-asset");

    profile.addAsset("new-asset");

    REQUIRE(profile.assets.size() == 2);
    CHECK(profile.assets[0] == "old-asset");
    CHECK(profile.assets[1] == "new-asset");
}

TEST_CASE("Add asset to not-empty Profile (ignored)", "[profile]") {
    Profile profile;
    profile.version = Profile::CurrentVersion;
    profile.assets.emplace_back("old-asset");

    profile.ignoreUpdates = true;
    profile.addAsset("new-asset");

    REQUIRE(profile.assets.size() == 1);
    CHECK(profile.assets[0] == "old-asset");
}

TEST_CASE("Add duplicate asset", "[profile]") {
    Profile profile;
    profile.version = Profile::CurrentVersion;

    profile.addAsset("new-asset");
    profile.addAsset("new-asset");

    REQUIRE(profile.assets.size() == 1);
    CHECK(profile.assets[0] == "new-asset");
}

//
// Removing assets
//
TEST_CASE("Remove asset", "[profile]") {
    Profile profile;
    profile.version = Profile::CurrentVersion;

    profile.addAsset("asset1");
    REQUIRE(profile.assets.size() == 1);
    CHECK(profile.assets[0] == "asset1");

    profile.addAsset("asset2");
    REQUIRE(profile.assets.size() == 2);
    CHECK(profile.assets[0] == "asset1");
    CHECK(profile.assets[1] == "asset2");

    profile.removeAsset("asset2");
    REQUIRE(profile.assets.size() == 1);
    CHECK(profile.assets[0] == "asset1");
}

TEST_CASE("Remove asset (ignored)", "[profile]") {
    Profile profile;
    profile.version = Profile::CurrentVersion;

    profile.addAsset("asset1");
    REQUIRE(profile.assets.size() == 1);
    CHECK(profile.assets[0] == "asset1");

    profile.addAsset("asset2");
    REQUIRE(profile.assets.size() == 2);
    CHECK(profile.assets[0] == "asset1");
    CHECK(profile.assets[1] == "asset2");

    profile.ignoreUpdates = true;
    profile.removeAsset("asset2");
    REQUIRE(profile.assets.size() == 2);
    CHECK(profile.assets[0] == "asset1");
    CHECK(profile.assets[1] == "asset2");
}

TEST_CASE("Removing non-exisiting asset", "[profile]") {
    Profile profile;
    profile.version = Profile::CurrentVersion;

    profile.assets.emplace_back("asset1");
    profile.assets.emplace_back("asset3");

    CHECK_NOTHROW(profile.removeAsset("unknown-asset"));
}

TEST_CASE("Removing non-exisiting asset (ignored)", "[profile]") {
    Profile profile;
    profile.version = Profile::CurrentVersion;

    profile.assets.emplace_back("asset1");
    profile.assets.emplace_back("asset3");

    profile.ignoreUpdates = true;
    CHECK_NOTHROW(profile.removeAsset("unknown-asset"));
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

    Profile profile;
    profile.version = Profile::CurrentVersion;
    profile.saveCurrentSettingsToProfile(owner, "current-time", state);

    REQUIRE(profile.properties.size() == 2);
    CHECK(
        profile.properties[0].setType ==
        Profile::Property::SetType::SetPropertyValueSingle
    );
    CHECK(profile.properties[0].name == "base.p1");
    CHECK(profile.properties[0].value == "2.000000");
    CHECK(
        profile.properties[1].setType ==
        Profile::Property::SetType::SetPropertyValueSingle
    );
    CHECK(profile.properties[1].name == "base.p2");
    CHECK(profile.properties[1].value == "\"test-string\"");

    REQUIRE(profile.camera.has_value());
    REQUIRE(std::holds_alternative<Profile::CameraNavState>(*profile.camera));
    Profile::CameraNavState camera = std::get<Profile::CameraNavState>(*profile.camera);
    CHECK(camera.anchor == "anchor");
    CHECK(camera.aim == "aim");
    CHECK(camera.referenceFrame == "refFrame");
    CHECK(camera.position == glm::dvec3(1.0, 2.0, 3.0));
    CHECK(camera.up == glm::dvec3(4.0, 5.0, 6.0));
    CHECK(camera.yaw == -1.0);
    CHECK(camera.pitch == -2.0);

    REQUIRE(profile.time.has_value());
    CHECK(profile.time->type == Profile::Time::Type::Absolute);
    CHECK(profile.time->value == "current-time");
}

//
// Error states
//
//
// Version
//
TEST_CASE("(Error) Version: Missing value 'major'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/version/missing_major.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'version.major' field is missing")
    );
}

TEST_CASE("(Error) Version: Missing value 'minor'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/version/missing_minor.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'version.minor' field is missing")
    );
}

TEST_CASE("(Error) Version: Wrong type 'major'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/version/wrongtype_major.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'version.major' must be a number")
    );
}

TEST_CASE("(Error) Version: Wrong type 'minor'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/version/wrongtype_minor.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'version.minor' must be a number")
    );
}

TEST_CASE("(Error) Version: Wrong type 'major' and 'minor'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/version/wrongtype_major_minor.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'version.major' must be a number")
    );
}


//
// Modules
//

TEST_CASE("(Error) Module: Missing value 'name'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/module/missing_name.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'module.name' field is missing")
    );
}

TEST_CASE("(Error) Module: Wrong type 'name'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/module/wrongtype_name.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'module.name' must be a string")
    );
}

TEST_CASE("(Error) Module: Wrong type 'loadedInstruction'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/module/wrongtype_loadedInstruction.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'module.loadedInstruction' must be a string")
    );
}

TEST_CASE("(Error) Module: Wrong type 'notLoadedInstruction'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/module/wrongtype_notLoadedInstruction.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'module.notLoadedInstruction' must be a string")
    );
}


//
// Property
//
TEST_CASE("(Error) Property: Missing value 'name'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/property/missing_name.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'property.name' field is missing")
    );
}

TEST_CASE("(Error) Property: Missing value 'value'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/property/missing_value.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'property.value' field is missing")
    );
}

TEST_CASE("(Error) Property: Missing value 'name' and 'value'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/property/missing_name_value.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'property.name' field is missing")
    );
}

TEST_CASE("(Error) Property: Wrong value 'type'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/property/wrongvalue_type.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) Unknown property set type")
    );
}

TEST_CASE("(Error) Property: Wrong type 'name'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/property/wrongtype_name.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'property.name' must be a string")
    );
}

TEST_CASE("(Error) Property: Wrong type 'value'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/property/wrongtype_value.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'property.value' must be a string")
    );
}


//
// Keybinding
//
TEST_CASE("(Error) Keybinding: Missing value 'key'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_key.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.key' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Missing value 'documentation'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_documentation.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.documentation' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Missing value 'name'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_name.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.name' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Missing value 'gui_path'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_guipath.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.gui_path' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Missing value 'is_local'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/missing_islocal.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.is_local' field is missing")
    );
}

TEST_CASE("(Error) Keybinding: Wrong value 'key'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongvalue_key.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("Could not find key for 'F50'")
    );
}

TEST_CASE("(Error) Keybinding: Wrong value 'key, modifier'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongvalue_modifier.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("Unknown modifier key 'KEYKEY'")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'documentation'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_documentation.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.documentation' must be a string")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'gui_path'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_guipath.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.gui_path' must be a string")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'is_local'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_islocal.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.is_local' must be a boolean")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'name'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_name.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.name' must be a string")
    );
}

TEST_CASE("(Error) Keybinding: Wrong type 'script'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/keybinding/wrongtype_script.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'keybinding.script' must be a string")
    );
}


//
// Time
//
TEST_CASE("(Error) Time: Wrong value 'type'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/time/wrongvalue_type.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) Unknown time type")
    );
}

TEST_CASE("(Error) Time (absolute): Missing value 'type'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/time/missing_type.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'time.type' field is missing")
    );
}

TEST_CASE("(Error) Time (relative): Missing value 'value'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/time/relative_missing_value.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'time.value' field is missing")
    );
}

//
// Deltatime
//
TEST_CASE("(Error) Deltatimes: Wrong type", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/deltatimes/wrongtype_value.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::ContainsSubstring("type must be number, but is string")
    );
}

//
// Camera
//
TEST_CASE("(Error) Camera: Wrong value 'type'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/wrongvalue_type.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) Unknown camera type")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'anchor'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_anchor.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.anchor' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'frame'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_frame.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.frame' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'position'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_position.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.position' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'anchor'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_anchor.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.anchor' must be a string")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'aim'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_aim.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.aim' must be a string")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'frame'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_frame.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.frame' must be a string")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'position'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_position.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.position' must be an object")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'position.x'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_position_x.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.position.x' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'position.x'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_position_x.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.position.x' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'position.y'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_position_y.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.position.y' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'position.y'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_position_y.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.position.y' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'position.z'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_position_z.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.position.z' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'position.z'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_position_z.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.position.z' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'up'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_up.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.up' must be an object")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'up.x'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_up_x.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.up.x' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'up.x'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_up_x.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.up.x' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'up.y'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_up_y.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.up.y' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'up.y'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_up_y.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.up.y' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Missing value 'up.z'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_missing_up_z.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.up.z' field is missing")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'up.z'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_up_z.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.up.z' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'yaw'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_yaw.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.yaw' must be a number")
    );
}

TEST_CASE("(Error) Camera (NavState): Wrong type 'pitch'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/navstate_wrongtype_pitch.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.pitch' must be a number")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Missing value 'anchor'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_missing_anchor.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.anchor' field is missing")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Missing value 'latitude'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_missing_latitude.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.latitude' field is missing")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Missing value 'longitude'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_missing_longitude.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.longitude' field is missing")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Wrong type 'anchor'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_wrongtype_anchor.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.anchor' must be a string")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Wrong type 'latitude'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_wrongtype_latitude.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.latitude' must be a number")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Wrong type 'longitude'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_wrongtype_longitude.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.longitude' must be a number")
    );
}

TEST_CASE("(Error) Camera (GoToGeo): Wrong type 'altitude'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotogeo_wrongtype_altitude.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.altitude' must be a number")
    );
}

TEST_CASE("(Error) Camera (GoToNode): Wrong type 'anchor'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotonode_wrongtype_anchor.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.anchor' must be a string")
    );
}

TEST_CASE("(Error) Camera (GoToNode): Wrong type 'height'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotonode_wrongtype_height.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.height' must be a number")
    );
}

TEST_CASE("(Error) Camera (GoToNode): Missing value 'anchor'", "[profile]") {
    constexpr std::string_view TestFile =
        "${TESTDIR}/profile/error/camera/gotonode_missing_anchor.profile";
    CHECK_THROWS_WITH(
        Profile(absPath(TestFile)),
        Catch::Matchers::Equals("(profile) 'camera.anchor' field is missing")
    );
}

// @TODO (2023-05-17, emmbr) Seems like the profile loading actually do not validate
// profile values to a larger degree than type and whether the value is missing, currently.
// So this is left as future work
//
//TEST_CASE("(Error) Camera (GoToNode): Invalid value for 'height' - negative", "[profile]") {
//    constexpr std::string_view TestFile =
//        "${TESTDIR}/profile/error/camera/gotonode_invalidvalue_negative_height.profile";
//    CHECK_THROWS_WITH(
//        Profile(absPath(TestFile)),
//        Catch::Matchers::Equals("(profile) 'camera.height' must be a larger than zero")
//    );
//}
//
//TEST_CASE("(Error) Camera (GoToNode): Invalid value for 'height' - zero", "[profile]") {
//    constexpr std::string_view TestFile =
//        "${TESTDIR}/profile/error/camera/gotonode_invalidvalue_zero_height.profile";
//    CHECK_THROWS_WITH(
//        Profile(absPath(TestFile)),
//        Catch::Matchers::Equals("(profile) 'camera.height' must be a larger than zero")
//    );
//}

// NOLINTEND(modernize-use-emplace)
