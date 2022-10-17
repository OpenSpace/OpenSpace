/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/engine/configuration.h>
#include <ghoul/fmt.h>
#include <filesystem>
#include <fstream>

using namespace openspace::configuration;

namespace {
    // The absolute minimal configuration that is still expected to be loaded
    std::string MinimalConfig = R"(
Paths = {}
FontSize = {
  FrameInfo = 1.0,
  Shutdown = 2.0,
  Log = 3.0,
  CameraInfo = 4.0,
  VersionInfo = 5.0
}
)";

    void writeConfig(const std::string& filename, std::string_view content) {
        std::ofstream f(filename);
        f << MinimalConfig << '\n' << MinimalConfig << content;
    }

    Configuration loadConfiguration(const std::string& tag, std::string_view content) {
        std::string filename = fmt::format("test_configuration_{}.cfg", tag);
        std::filesystem::path path = std::filesystem::temp_directory_path();
        std::string file = (path / filename).string();
        writeConfig(file, content);
        try {
            Configuration conf = loadConfigurationFromFile(file, glm::ivec2(0), content);
            std::filesystem::remove(file);
            return conf;
        }
        catch (...) {
            std::filesystem::remove(file);
            throw;
        }
    }
} // namespace

TEST_CASE("Configuration: minimal", "[configuration]") {
    loadConfiguration("minimal", "");
}

TEST_CASE("Configuration: windowConfiguration", "[configuration]") {
    constexpr std::string_view Extra = R"(SGCTConfig = "foobar")";
    const Configuration c = loadConfiguration("windowConfiguration", Extra);
    CHECK(c.windowConfiguration == "foobar");
}

TEST_CASE("Configuration: asset", "[configuration]") {
    constexpr std::string_view Extra = R"(Asset = "foobar")";
    const Configuration c = loadConfiguration("asset", Extra);
    CHECK(c.asset == "foobar");
}

TEST_CASE("Configuration: profile", "[configuration]") {
    constexpr std::string_view Extra = R"(Profile = "foobar")";
    const Configuration c = loadConfiguration("profile", Extra);
    CHECK(c.profile == "foobar");
}

TEST_CASE("Configuration: globalCustomizationScripts", "[configuration]") {
    constexpr std::string_view Extra = R"(GlobalCustomizationScripts = { "foo", "bar" })";
    const Configuration c = loadConfiguration("globalCustomization", Extra);
    CHECK(c.globalCustomizationScripts.size() == 2);
    CHECK(c.globalCustomizationScripts == std::vector<std::string>{ "foo", "bar" });
}

TEST_CASE("Configuration: paths", "[configuration]") {
    constexpr std::string_view Extra = R"(Paths = { foo = "1", bar = "2" })";
    const Configuration c = loadConfiguration("paths", Extra);
    CHECK(c.pathTokens.size() == 2);
    CHECK(
        c.pathTokens ==
        std::map<std::string, std::string>{ { "foo", "1" }, { "bar", "2" } }
    );
}

TEST_CASE("Configuration: fonts", "[configuration]") {
    constexpr std::string_view Extra = R"(Fonts = { foo = "1", bar = "2" })";
    const Configuration c = loadConfiguration("fonts", Extra);
    CHECK(c.fonts.size() == 2);
    CHECK(
        c.fonts ==
        std::map<std::string, std::string>{ { "foo", "1" }, { "bar", "2" } }
    );
}

TEST_CASE("Configuration: logging", "[configuration]") {
    Configuration defaultConf;
    {
        // Empty
        constexpr std::string_view Extra = R"(Logging = {})";
        const Configuration c = loadConfiguration("logging1", Extra);

        CHECK(c.logging.level == defaultConf.logging.level);
        CHECK(c.logging.forceImmediateFlush == defaultConf.logging.forceImmediateFlush);
        CHECK(
            c.logging.capabilitiesVerbosity ==
            defaultConf.logging.capabilitiesVerbosity
        );
        CHECK(c.logging.logs == defaultConf.logging.logs);
    }
    {
        // level
        constexpr std::string_view Extra = R"(Logging = { LogLevel = "Fatal" })";
        const Configuration c = loadConfiguration("logging2", Extra);

        CHECK(c.logging.level == "Fatal");
        CHECK(c.logging.forceImmediateFlush == defaultConf.logging.forceImmediateFlush);
        CHECK(
            c.logging.capabilitiesVerbosity ==
            defaultConf.logging.capabilitiesVerbosity
        );
        CHECK(c.logging.logs == defaultConf.logging.logs);
    }
    {
        // forceimmediate
        constexpr std::string_view Extra = R"(Logging = { ImmediateFlush = false })";
        const Configuration c = loadConfiguration("logging3", Extra);

        CHECK(c.logging.level == defaultConf.logging.level);
        CHECK(c.logging.forceImmediateFlush == false);
        CHECK(
            c.logging.capabilitiesVerbosity ==
            defaultConf.logging.capabilitiesVerbosity
        );
        CHECK(c.logging.logs == defaultConf.logging.logs);
    }
    {
        // logs
        constexpr std::string_view Extra = R"(
Logging = {
    Logs = {
        { Type = "html", File = "foobar", Append = false }
    }
}
)";
        const Configuration c = loadConfiguration("logging4", Extra);

        CHECK(c.logging.level == defaultConf.logging.level);
        CHECK(c.logging.forceImmediateFlush == defaultConf.logging.forceImmediateFlush);
        CHECK(
            c.logging.capabilitiesVerbosity ==
            defaultConf.logging.capabilitiesVerbosity
        );
        REQUIRE(c.logging.logs.size() == 1);
        const ghoul::Dictionary& d = c.logging.logs[0];
        REQUIRE(d.hasValue<std::string>("Type"));
        CHECK(d.value<std::string>("Type") == "html");
        REQUIRE(d.hasValue<std::string>("File"));
        CHECK(d.value<std::string>("File") == "foobar");
        REQUIRE(d.hasValue<bool>("Append"));
        CHECK(d.value<bool>("Append") == false);
    }
    {
        // capabilities verbosity
        constexpr std::string_view Extra = R"(Logging = { CapabilitiesVerbosity = "Full" })";
        const Configuration c = loadConfiguration("logging5", Extra);

        CHECK(c.logging.level == defaultConf.logging.level);
        CHECK(c.logging.forceImmediateFlush == defaultConf.logging.forceImmediateFlush);
        CHECK(c.logging.capabilitiesVerbosity == "Full");
        CHECK(c.logging.logs == defaultConf.logging.logs);
    }
}

TEST_CASE("Configuration: scriptlog", "[configuration]") {
    constexpr std::string_view Extra = R"(ScriptLog = "foobar")";
    const Configuration c = loadConfiguration("scriptlog", Extra);
    CHECK(c.scriptLog == "foobar");
}

TEST_CASE("Configuration: documentationpath", "[configuration]") {
    constexpr std::string_view Extra = R"(Documentation = { Path = "foobar" })";
    const Configuration c = loadConfiguration("documentationpath", Extra);
    CHECK(c.documentation.path == "foobar");
}

TEST_CASE("Configuration: versioncheckurl", "[configuration]") {
    constexpr std::string_view Extra = R"(VersionCheckUrl = "foobar")";
    const Configuration c = loadConfiguration("versioncheckurl", Extra);
    CHECK(c.versionCheckUrl == "foobar");
}

TEST_CASE("Configuration: useMultithreadedInit", "[configuration]") {
    constexpr std::string_view Extra = R"(UseMultithreadedInitialization = true)";
    const Configuration c = loadConfiguration("useMultithreadedInit", Extra);
    CHECK(c.useMultithreadedInitialization == true);
}

TEST_CASE("Configuration: loadingscreen", "[configuration]") {
    Configuration defaultConf;

    {
        // empty
        constexpr std::string_view Extra = R"(LoadingScreen = {})";
        const Configuration c = loadConfiguration("loadingscreen1", Extra);
        CHECK(
            c.loadingScreen.isShowingMessages ==
            defaultConf.loadingScreen.isShowingMessages
        );
        CHECK(
            c.loadingScreen.isShowingProgressbar ==
            defaultConf.loadingScreen.isShowingProgressbar
        );
        CHECK(
            c.loadingScreen.isShowingNodeNames ==
            defaultConf.loadingScreen.isShowingNodeNames
        );
    }
    {
        // isShowingMessages
        constexpr std::string_view Extra = R"(LoadingScreen = { ShowMessage = true })";
        const Configuration c = loadConfiguration("loadingscreen2", Extra);
        CHECK(c.loadingScreen.isShowingMessages == true);
        CHECK(
            c.loadingScreen.isShowingProgressbar ==
            defaultConf.loadingScreen.isShowingProgressbar
        );
        CHECK(
            c.loadingScreen.isShowingNodeNames ==
            defaultConf.loadingScreen.isShowingNodeNames
        );
    }
    {
        // isShowingProgressbar
        constexpr std::string_view Extra = R"(LoadingScreen = { ShowProgressbar = true })";
        const Configuration c = loadConfiguration("loadingscreen3", Extra);
        CHECK(
            c.loadingScreen.isShowingMessages ==
            defaultConf.loadingScreen.isShowingMessages
        );
        CHECK(c.loadingScreen.isShowingProgressbar == true);
        CHECK(
            c.loadingScreen.isShowingNodeNames ==
            defaultConf.loadingScreen.isShowingNodeNames
        );
    }
    {
        // isShowingNodeNames
        constexpr std::string_view Extra = R"(LoadingScreen = { ShowNodeNames = true })";
        const Configuration c = loadConfiguration("loadingscreen4", Extra);
        CHECK(
            c.loadingScreen.isShowingMessages ==
            defaultConf.loadingScreen.isShowingMessages
        );
        CHECK(
            c.loadingScreen.isShowingProgressbar ==
            defaultConf.loadingScreen.isShowingProgressbar
        );
        CHECK(c.loadingScreen.isShowingNodeNames == true);
    }
}

TEST_CASE("Configuration: isCheckingOpenGLState", "[configuration]") {
    constexpr std::string_view Extra = R"(CheckOpenGLState = true)";
    const Configuration c = loadConfiguration("isCheckingOpenGLState", Extra);
    CHECK(c.isCheckingOpenGLState == true);
}

TEST_CASE("Configuration: isLoggingOpenGLCalls", "[configuration]") {
    constexpr std::string_view Extra = R"(LogEachOpenGLCall = true)";
    const Configuration c = loadConfiguration("isLoggingOpenGLCalls", Extra);
    CHECK(c.isLoggingOpenGLCalls == true);
}

TEST_CASE("Configuration: shutdownCountdown", "[configuration]") {
    constexpr std::string_view Extra = R"(ShutdownCountdown = 0.5)";
    const Configuration c = loadConfiguration("shutdownCountdown", Extra);
    CHECK(c.shutdownCountdown == 0.5f);
}

TEST_CASE("Configuration: shouldUseScreenshotDate", "[configuration]") {
    constexpr std::string_view Extra = R"(ScreenshotUseDate = true)";
    const Configuration c = loadConfiguration("shouldUseScreenshotDate", Extra);
    CHECK(c.shouldUseScreenshotDate == true);
}

TEST_CASE("Configuration: onScreenTextScaling", "[configuration]") {
    constexpr std::string_view Extra = R"(OnScreenTextScaling = "framebuffer")";
    const Configuration c = loadConfiguration("onScreenTextScaling", Extra);
    CHECK(c.onScreenTextScaling == "framebuffer");
}

TEST_CASE("Configuration: usePerProfileCache", "[configuration]") {
    constexpr std::string_view Extra = R"(PerProfileCache = true)";
    const Configuration c = loadConfiguration("usePerProfileCache", Extra);
    CHECK(c.usePerProfileCache == true);
}

TEST_CASE("Configuration: isRenderingOnMasterDisabled", "[configuration]") {
    constexpr std::string_view Extra = R"(DisableRenderingOnMaster = true)";
    const Configuration c = loadConfiguration("isRenderingOnMasterDisabled", Extra);
    CHECK(c.isRenderingOnMasterDisabled == true);
}

TEST_CASE("Configuration: globalRotation", "[configuration]") {
    constexpr std::string_view Extra = R"(GlobalRotation = { 1.0, 2.0, 3.0 })";
    const Configuration c = loadConfiguration("globalRotation", Extra);
    CHECK(c.globalRotation == glm::vec3(1.0, 2.0, 3.0));
}

TEST_CASE("Configuration: screenSpaceRotation", "[configuration]") {
    constexpr std::string_view Extra = R"(ScreenSpaceRotation = { 1.0, 2.0, 3.0 })";
    const Configuration c = loadConfiguration("screenSpaceRotation", Extra);
    CHECK(c.screenSpaceRotation == glm::vec3(1.0, 2.0, 3.0));
}

TEST_CASE("Configuration: masterRotation", "[configuration]") {
    constexpr std::string_view Extra = R"(MasterRotation = { 1.0, 2.0, 3.0 })";
    const Configuration c = loadConfiguration("masterRotation", Extra);
    CHECK(c.masterRotation == glm::vec3(1.0, 2.0, 3.0));
}

TEST_CASE("Configuration: isConsoleDisabled", "[configuration]") {
    constexpr std::string_view Extra = R"(DisableInGameConsole = true)";
    const Configuration c = loadConfiguration("isConsoleDisabled", Extra);
    CHECK(c.isConsoleDisabled == true);
}

TEST_CASE("Configuration: bypassLauncher", "[configuration]") {
    constexpr std::string_view Extra = R"(BypassLauncher = true)";
    const Configuration c = loadConfiguration("bypassLauncher", Extra);
    CHECK(c.bypassLauncher == true);
}

TEST_CASE("Configuration: moduleConfigurations", "[configuration]") {
    {
        // empty
        constexpr std::string_view Extra = R"(ModuleConfigurations = {})";
        const Configuration c = loadConfiguration("moduleConfigurations", Extra);
        CHECK(c.moduleConfigurations.empty());
    }
    {
        // values
        constexpr std::string_view Extra = R"(
ModuleConfigurations = {
    Foo = {
        Foo2 = 1.0,
        Foo3 = "abc"
    },
    Bar = {
        Bar2 = true,
        Bar3 = { 1.0, 2.0, 3.0 }
    }
}
)";
        const Configuration c = loadConfiguration("moduleConfigurations", Extra);
        REQUIRE(c.moduleConfigurations.size() == 2);
        ghoul::Dictionary foo = c.moduleConfigurations.at("Foo");
        REQUIRE(foo.size() == 2);
        REQUIRE(foo.hasValue<double>("Foo2"));
        CHECK(foo.value<double>("Foo2") == 1.0);
        REQUIRE(foo.hasValue<std::string>("Foo3"));
        CHECK(foo.value<std::string>("Foo3") == std::string("abc"));

        ghoul::Dictionary bar = c.moduleConfigurations.at("Bar");
        REQUIRE(bar.size() == 2);
        REQUIRE(bar.hasValue<bool>("Bar2"));
        CHECK(bar.value<bool>("Bar2") == true);
        REQUIRE(bar.hasValue<glm::dvec3>("Bar3"));
        CHECK(bar.value<glm::dvec3>("Bar3") == glm::dvec3(1.0, 2.0, 3.0));
    }
}

TEST_CASE("Configuration: openGLDebugContext", "[configuration]") {
    Configuration defaultConf;
    {
        // empty-ish / activate
        constexpr std::string_view Extra = R"(OpenGLDebugContext = { Activate = true })";
        const Configuration c = loadConfiguration("openGLDebugContext1", Extra);
        CHECK(c.openGLDebugContext.isActive == true);
        CHECK(c.openGLDebugContext.printStacktrace == false);
        CHECK(
            c.openGLDebugContext.isSynchronous ==
            defaultConf.openGLDebugContext.isSynchronous
        );
        REQUIRE(
            c.openGLDebugContext.identifierFilters.size() ==
            defaultConf.openGLDebugContext.identifierFilters.size()
        );
        for (size_t i = 0; i < c.openGLDebugContext.identifierFilters.size(); i += 1) {
            CHECK(
                c.openGLDebugContext.identifierFilters[i].identifier ==
                defaultConf.openGLDebugContext.identifierFilters[i].identifier
            );
            CHECK(
                c.openGLDebugContext.identifierFilters[i].source ==
                defaultConf.openGLDebugContext.identifierFilters[i].source
            );
            CHECK(
                c.openGLDebugContext.identifierFilters[i].type ==
                defaultConf.openGLDebugContext.identifierFilters[i].type
            );
        }
        CHECK(
            c.openGLDebugContext.severityFilters ==
            defaultConf.openGLDebugContext.severityFilters
        );
    }
    {
        // isSynchronous
        constexpr std::string_view Extra = R"(
OpenGLDebugContext = { Activate = true, Synchronous = true }
)";
        const Configuration c = loadConfiguration("openGLDebugContext2", Extra);
        CHECK(c.openGLDebugContext.isActive == true);
        CHECK(c.openGLDebugContext.isSynchronous == true);
        REQUIRE(
            c.openGLDebugContext.identifierFilters.size() ==
            defaultConf.openGLDebugContext.identifierFilters.size()
        );
        for (size_t i = 0; i < c.openGLDebugContext.identifierFilters.size(); i += 1) {
            CHECK(
                c.openGLDebugContext.identifierFilters[i].identifier ==
                defaultConf.openGLDebugContext.identifierFilters[i].identifier
            );
            CHECK(
                c.openGLDebugContext.identifierFilters[i].source ==
                defaultConf.openGLDebugContext.identifierFilters[i].source
            );
            CHECK(
                c.openGLDebugContext.identifierFilters[i].type ==
                defaultConf.openGLDebugContext.identifierFilters[i].type
            );
        }
        CHECK(
            c.openGLDebugContext.severityFilters ==
            defaultConf.openGLDebugContext.severityFilters
        );
    }
    {
        // identifierFilters
        constexpr std::string_view Extra = R"(
OpenGLDebugContext = {
    Activate = true,
    PrintStacktrace = true,
    FilterIdentifier = {
        { Identifier = 1, Source = "API", Type = "Error" },
        { Identifier = 2, Source = "Window System", Type = "Deprecated" },
        { Identifier = 3, Source = "Shader Compiler", Type = "Undefined" },
        { Identifier = 4, Source = "Third Party", Type = "Portability" },
        { Identifier = 5, Source = "Application", Type = "Performance" },
        { Identifier = 6, Source = "Other", Type = "Marker" },
        { Identifier = 7, Source = "Don't care", Type = "Push group" },
        { Identifier = 8, Source = "API", Type = "Pop group" },
        { Identifier = 9, Source = "Window System", Type = "Other" },
        { Identifier = 10, Source = "Shader Compiler", Type = "Don't care" }
    }
}
)";
        const Configuration c = loadConfiguration("openGLDebugContext3", Extra);
        CHECK(c.openGLDebugContext.isActive == true);
        CHECK(c.openGLDebugContext.printStacktrace == true);
        CHECK(
            c.openGLDebugContext.isSynchronous ==
            defaultConf.openGLDebugContext.isSynchronous
        );
        REQUIRE(c.openGLDebugContext.identifierFilters.size() == 10);
        CHECK(c.openGLDebugContext.identifierFilters[0].identifier == 1);
        CHECK(c.openGLDebugContext.identifierFilters[0].source == "API");
        CHECK(c.openGLDebugContext.identifierFilters[0].type == "Error");
        CHECK(c.openGLDebugContext.identifierFilters[1].identifier == 2);
        CHECK(c.openGLDebugContext.identifierFilters[1].source == "Window System");
        CHECK(c.openGLDebugContext.identifierFilters[1].type == "Deprecated");
        CHECK(c.openGLDebugContext.identifierFilters[2].identifier == 3);
        CHECK(c.openGLDebugContext.identifierFilters[2].source == "Shader Compiler");
        CHECK(c.openGLDebugContext.identifierFilters[2].type == "Undefined");
        CHECK(c.openGLDebugContext.identifierFilters[3].identifier == 4);
        CHECK(c.openGLDebugContext.identifierFilters[3].source == "Third Party");
        CHECK(c.openGLDebugContext.identifierFilters[3].type == "Portability");
        CHECK(c.openGLDebugContext.identifierFilters[4].identifier == 5);
        CHECK(c.openGLDebugContext.identifierFilters[4].source == "Application");
        CHECK(c.openGLDebugContext.identifierFilters[4].type == "Performance");
        CHECK(c.openGLDebugContext.identifierFilters[5].identifier == 6);
        CHECK(c.openGLDebugContext.identifierFilters[5].source == "Other");
        CHECK(c.openGLDebugContext.identifierFilters[5].type == "Marker");
        CHECK(c.openGLDebugContext.identifierFilters[6].identifier == 7);
        CHECK(c.openGLDebugContext.identifierFilters[6].source == "Don't care");
        CHECK(c.openGLDebugContext.identifierFilters[6].type == "Push group");
        CHECK(c.openGLDebugContext.identifierFilters[7].identifier == 8);
        CHECK(c.openGLDebugContext.identifierFilters[7].source == "API");
        CHECK(c.openGLDebugContext.identifierFilters[7].type == "Pop group");
        CHECK(c.openGLDebugContext.identifierFilters[8].identifier == 9);
        CHECK(c.openGLDebugContext.identifierFilters[8].source == "Window System");
        CHECK(c.openGLDebugContext.identifierFilters[8].type == "Other");
        CHECK(c.openGLDebugContext.identifierFilters[9].identifier == 10);
        CHECK(c.openGLDebugContext.identifierFilters[9].source == "Shader Compiler");
        CHECK(c.openGLDebugContext.identifierFilters[9].type == "Don't care");

        CHECK(
            c.openGLDebugContext.severityFilters ==
            defaultConf.openGLDebugContext.severityFilters
        );
    }
    {
        // filterSeverity
        constexpr std::string_view Extra = R"(
OpenGLDebugContext = { Activate = true, FilterSeverity = { "High", "Medium" } }
)";
        const Configuration c = loadConfiguration("openGLDebugContext4", Extra);
        CHECK(c.openGLDebugContext.isActive == true);
        CHECK(c.openGLDebugContext.printStacktrace == false);
        CHECK(
            c.openGLDebugContext.isSynchronous ==
            defaultConf.openGLDebugContext.isSynchronous
        );
        REQUIRE(
            c.openGLDebugContext.identifierFilters.size() ==
            defaultConf.openGLDebugContext.identifierFilters.size()
        );
        for (size_t i = 0; i < c.openGLDebugContext.identifierFilters.size(); i += 1) {
            CHECK(
                c.openGLDebugContext.identifierFilters[i].identifier ==
                defaultConf.openGLDebugContext.identifierFilters[i].identifier
            );
            CHECK(
                c.openGLDebugContext.identifierFilters[i].source ==
                defaultConf.openGLDebugContext.identifierFilters[i].source
            );
            CHECK(
                c.openGLDebugContext.identifierFilters[i].type ==
                defaultConf.openGLDebugContext.identifierFilters[i].type
            );
        }
        REQUIRE(c.openGLDebugContext.severityFilters.size() == 2);
        CHECK(
            c.openGLDebugContext.severityFilters ==
            std::vector<std::string>{ "High", "Medium" }
        );
    }
}

TEST_CASE("Configuration: httpProxy", "[configuration]") {
    Configuration defaultConf;
    {
        // empty-ish / address + port
        constexpr std::string_view Extra = R"(
HttpProxy = {
    Address = "foobar",
    Port = 1234
}
)";
        const Configuration c = loadConfiguration("httpProxy1", Extra);
        CHECK(c.httpProxy.usingHttpProxy == defaultConf.httpProxy.usingHttpProxy);
        CHECK(c.httpProxy.address == "foobar");
        CHECK(c.httpProxy.port == 1234);
        CHECK(c.httpProxy.authentication == defaultConf.httpProxy.authentication);
        CHECK(c.httpProxy.user == defaultConf.httpProxy.user);
        CHECK(c.httpProxy.password == defaultConf.httpProxy.password);
    }
    {
        // activate
        constexpr std::string_view Extra = R"(
HttpProxy = {
    Activate = true,
    Address = "foobar",
    Port = 1234
}
)";
        const Configuration c = loadConfiguration("httpProxy2", Extra);
        CHECK(c.httpProxy.usingHttpProxy == true);
        CHECK(c.httpProxy.address == "foobar");
        CHECK(c.httpProxy.port == 1234);
        CHECK(c.httpProxy.authentication == defaultConf.httpProxy.authentication);
        CHECK(c.httpProxy.user == defaultConf.httpProxy.user);
        CHECK(c.httpProxy.password == defaultConf.httpProxy.password);
    }
    {
        // authentication
        constexpr std::string_view Extra = R"(
HttpProxy = {
    Address = "foobar",
    Port = 1234,
    Authentication = "ntlm"
}
)";
        const Configuration c = loadConfiguration("httpProxy3", Extra);
        CHECK(c.httpProxy.usingHttpProxy == defaultConf.httpProxy.usingHttpProxy);
        CHECK(c.httpProxy.address == "foobar");
        CHECK(c.httpProxy.port == 1234);
        CHECK(c.httpProxy.authentication == "ntlm");
        CHECK(c.httpProxy.user == defaultConf.httpProxy.user);
        CHECK(c.httpProxy.password == defaultConf.httpProxy.password);
    }
    {
        // user
        constexpr std::string_view Extra = R"(
HttpProxy = {
    Address = "foobar",
    Port = 1234,
    User = "user-bar"
}
)";
        const Configuration c = loadConfiguration("httpProxy4", Extra);
        CHECK(c.httpProxy.usingHttpProxy == defaultConf.httpProxy.usingHttpProxy);
        CHECK(c.httpProxy.address == "foobar");
        CHECK(c.httpProxy.port == 1234);
        CHECK(c.httpProxy.authentication == defaultConf.httpProxy.authentication);
        CHECK(c.httpProxy.user == "user-bar");
        CHECK(c.httpProxy.password == defaultConf.httpProxy.password);
    }
    {
        // password
        constexpr std::string_view Extra = R"(
HttpProxy = {
    Address = "foobar",
    Port = 1234,
    Password = "password-bar"
}
)";
        const Configuration c = loadConfiguration("httpProxy5", Extra);
        CHECK(c.httpProxy.usingHttpProxy == defaultConf.httpProxy.usingHttpProxy);
        CHECK(c.httpProxy.address == "foobar");
        CHECK(c.httpProxy.port == 1234);
        CHECK(c.httpProxy.authentication == defaultConf.httpProxy.authentication);
        CHECK(c.httpProxy.user == defaultConf.httpProxy.user);
        CHECK(c.httpProxy.password == "password-bar");
    }
}
