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
#include <catch2/matchers/catch_matchers_exception.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

#include <openspace/engine/settings.h>
#include <openspace/json.h>
#include <filesystem>
#include <fstream>

using namespace openspace;

TEST_CASE("Settings Load: Empty", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_empty.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Load: Really Empty", "[settings]") {
    constexpr std::string_view Source = R"(
{
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_really-empty.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Empty", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_empty.json";

    const Settings srcSettings;
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Started Before", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "started-before": false
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_started-before.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    REQUIRE(settings.hasStartedBefore.has_value());
    CHECK(*settings.hasStartedBefore == false);
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Started Before", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_started-before.json";

    const Settings srcSettings = {
        .hasStartedBefore = false
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Configuration", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "config": "abc"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_config.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    REQUIRE(settings.configuration.has_value());
    CHECK(*settings.configuration == "abc");
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Configuration", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_config.json";

    const Settings srcSettings = {
        .configuration = "abc"
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Configuration Remember", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "config-remember": true
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_config_remember.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    REQUIRE(settings.rememberLastConfiguration.has_value());
    CHECK(*settings.rememberLastConfiguration == true);
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Configuration Remember", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_config_remember.json";

    const Settings srcSettings = {
        .rememberLastConfiguration = true
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Profile", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "profile": "def"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_profile.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    REQUIRE(settings.profile.has_value());
    CHECK(*settings.profile == "def");
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Profile", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_profile.json";

    const Settings srcSettings = {
        .profile = "def"
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Profile Remember", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "profile-remember": false
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_profile_remember.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    REQUIRE(settings.rememberLastProfile.has_value());
    CHECK(*settings.rememberLastProfile == false);
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Profile Remember", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_profile.json";

    const Settings srcSettings = {
        .rememberLastProfile = false
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Visibility/NoviceUser", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "visibility": "NoviceUser"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_visibility_novice.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    REQUIRE(settings.visibility.has_value());
    CHECK(*settings.visibility == properties::Property::Visibility::NoviceUser);
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Visibility/NoviceUser", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_noviceuser.json";

    const Settings srcSettings = {
        .visibility = openspace::properties::Property::Visibility::NoviceUser
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Visibility/User", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "visibility": "User"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_visibility_user.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    REQUIRE(settings.visibility.has_value());
    CHECK(*settings.visibility == properties::Property::Visibility::User);
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Visibility/User", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_user.json";

    const Settings srcSettings = {
        .visibility = openspace::properties::Property::Visibility::User
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Visibility/AdvancedUser", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "visibility": "AdvancedUser"
}
)";

    const  std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_vis_advanced.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    REQUIRE(settings.visibility.has_value());
    CHECK(*settings.visibility == properties::Property::Visibility::AdvancedUser);
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Visibility/AdvancedUser", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_advanceduser.json";

    const Settings srcSettings = {
        .visibility = openspace::properties::Property::Visibility::AdvancedUser
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Visibility/Developer", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "visibility": "Developer"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_vis_developer.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    REQUIRE(settings.visibility.has_value());
    CHECK(*settings.visibility == properties::Property::Visibility::Developer);
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Visibility/Developer", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_developer.json";

    const Settings srcSettings = {
        .visibility = openspace::properties::Property::Visibility::Developer
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Bypass Launcher", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "bypass": false
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_bypass.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    REQUIRE(settings.bypassLauncher.has_value());
    CHECK(*settings.bypassLauncher == false);
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: Bypass Launcher", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_bypass.json";

    const Settings srcSettings = {
        .bypassLauncher = false
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: LayerServer/All", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "layerserver": "All"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_layerserver_all.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    REQUIRE(settings.layerServer.has_value());
    CHECK(*settings.layerServer == Configuration::LayerServer::All);
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: LayerServer/All", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_layerserver_all.json";

    const Settings srcSettings = {
        .layerServer = openspace::Configuration::LayerServer::All
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: LayerServer/NewYork", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "layerserver": "NewYork"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_layerserver_nyc.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    REQUIRE(settings.layerServer.has_value());
    CHECK(*settings.layerServer == Configuration::LayerServer::NewYork);
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: LayerServer/NewYork", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_layerserver_nyc.json";

    const Settings srcSettings = {
        .layerServer = openspace::Configuration::LayerServer::NewYork
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: LayerServer/Sweden", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "layerserver": "Sweden"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file =
        path / "test_settings_load_layerserver_sweden.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    REQUIRE(settings.layerServer.has_value());
    CHECK(*settings.layerServer == Configuration::LayerServer::Sweden);
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: LayerServer/Sweden", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_layers_sweden.json";

    const Settings srcSettings = {
        .layerServer = openspace::Configuration::LayerServer::Sweden
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: LayerServer/Utah", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "layerserver": "Utah"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_layerserver_utah.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    REQUIRE(settings.layerServer.has_value());
    CHECK(*settings.layerServer == Configuration::LayerServer::Utah);
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: LayerServer/Utah", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_layerserver_utah.json";

    const Settings srcSettings = {
        .layerServer = openspace::Configuration::LayerServer::Utah
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: LayerServer/None", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "layerserver": "None"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_layerserver_none.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    REQUIRE(settings.layerServer.has_value());
    CHECK(*settings.layerServer == Configuration::LayerServer::None);
    CHECK(!settings.mrf.isEnabled.has_value());
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: LayerServer/None", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_layerserver_none.json";

    const Settings srcSettings = {
        .layerServer = openspace::Configuration::LayerServer::None
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: MRF IsEnabled", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "mrf": {
        "enabled": true
    }
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_mrf_isenabled.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    REQUIRE(settings.mrf.isEnabled.has_value());
    CHECK(*settings.mrf.isEnabled == true);
    CHECK(!settings.mrf.location.has_value());
}

TEST_CASE("Settings Save: MRF IsEnabled", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_mrf_isenabled.json";

    const Settings srcSettings = {
        .mrf = Settings::MRF {
            .isEnabled = true
        }
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: MRF Location", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "mrf": {
        "location": "ghi"
    }
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_mrf_location.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    CHECK(!settings.hasStartedBefore.has_value());
    CHECK(!settings.configuration.has_value());
    CHECK(!settings.rememberLastConfiguration.has_value());
    CHECK(!settings.profile.has_value());
    CHECK(!settings.rememberLastProfile.has_value());
    CHECK(!settings.visibility.has_value());
    CHECK(!settings.bypassLauncher.has_value());
    CHECK(!settings.layerServer.has_value());
    CHECK(!settings.mrf.isEnabled.has_value());
    REQUIRE(settings.mrf.location.has_value());
    CHECK(*settings.mrf.location == "ghi");
}

TEST_CASE("Settings Save: MRF Location", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_mrf_location.json";

    const Settings srcSettings = {
        .mrf = Settings::MRF {
            .location = "ghi"
        }
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load: Full", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "started-before": false,
    "config": "abc",
    "config-remember": true,
    "profile": "def",
    "profile-remember": false,
    "visibility": "NoviceUser",
    "bypass": false,
    "layerserver": "All",
    "mrf": {
        "enabled": true,
        "location": "ghi"
    }
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_full.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    const Settings settings = loadSettings(file);

    REQUIRE(settings.hasStartedBefore.has_value());
    CHECK(*settings.hasStartedBefore == false);
    REQUIRE(settings.configuration.has_value());
    CHECK(*settings.configuration == "abc");
    REQUIRE(settings.rememberLastConfiguration.has_value());
    CHECK(*settings.rememberLastConfiguration == true);
    REQUIRE(settings.profile.has_value());
    CHECK(*settings.profile == "def");
    REQUIRE(settings.rememberLastProfile.has_value());
    CHECK(*settings.rememberLastProfile == false);
    REQUIRE(settings.visibility.has_value());
    CHECK(*settings.visibility == properties::Property::Visibility::NoviceUser);
    REQUIRE(settings.bypassLauncher.has_value());
    CHECK(*settings.bypassLauncher == false);
    REQUIRE(settings.layerServer.has_value());
    CHECK(*settings.layerServer == Configuration::LayerServer::All);
    REQUIRE(settings.mrf.isEnabled.has_value());
    CHECK(*settings.mrf.isEnabled == true);
    REQUIRE(settings.mrf.location.has_value());
    CHECK(*settings.mrf.location == "ghi");
}

TEST_CASE("Settings Save: Full", "[settings]") {
    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_save_full.json";

    const Settings srcSettings = {
        .hasStartedBefore = false,
        .configuration = "abc",
        .rememberLastConfiguration = true,
        .profile = "def",
        .rememberLastProfile = false,
        .visibility = openspace::properties::Property::Visibility::NoviceUser,
        .bypassLauncher = false,
        .layerServer = openspace::Configuration::LayerServer::All,
        .mrf = Settings::MRF {
            .isEnabled = true,
            .location = "ghi"
        }
    };
    saveSettings(srcSettings, file);

    const Settings cmpSettings = loadSettings(file);
    CHECK(srcSettings == cmpSettings);
}

TEST_CASE("Settings Load Fail: Illegal version", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": -1
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_illegal_ver.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: Started before", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "started-before": "abc"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_start-before.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: Config", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "config": 1
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_config.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: Profile", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "profile": 1
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_profile.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: Visibility type", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "visibility": 1
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_vis_type.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: Visibility value", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "visibility": "abc"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_vis_value.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: Bypass Launcher", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "bypass": 1
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_bypass.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: LayerServer type", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "layerserver": 1
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file =
        path / "test_settings_load_fail_layerserver_type.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: LayerServer value", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "layerserver": "abc"
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_layer_value.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: MRF", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "mrf": 1
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_mrf.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: MRF/enabled", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "mrf": {
        "enabled": 1
    }
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_mrf_enabled.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}

TEST_CASE("Settings Load Fail: MRF/location", "[settings]") {
    constexpr std::string_view Source = R"(
{
    "version": 1,
    "mrf": {
        "location": 1
    }
}
)";

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "test_settings_load_fail_mrf_location.json";
    {
        std::ofstream f(file);
        f << Source;
    }

    CHECK_THROWS(loadSettings(file));
}
