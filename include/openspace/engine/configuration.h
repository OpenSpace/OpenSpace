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

#ifndef __OPENSPACE_CORE___CONFIGURATION___H__
#define __OPENSPACE_CORE___CONFIGURATION___H__

#include <ghoul/lua/luastate.h>
#include <ghoul/misc/dictionary.h>
#include <filesystem>
#include <map>
#include <string>
#include <vector>

namespace openspace::documentation { struct Documentation; }

namespace openspace::configuration {

struct Configuration {
    Configuration() = default;
    Configuration(Configuration&&) = default;
    Configuration(const Configuration&) = delete;
    Configuration& operator=(const Configuration&) = delete;
    Configuration& operator=(Configuration&&) = default;

    std::string windowConfiguration = "${CONFIG}/single.xml";
    std::string asset;
    std::string profile;
    std::vector<std::string> readOnlyProfiles;
    std::vector<std::string> globalCustomizationScripts;
    std::map<std::string, std::string> pathTokens = {
        { "CACHE" , "CACHE = \"${BASE}/cache\"" }
    };
    std::map<std::string, std::string> fonts;

    struct FontSizes {
        float frameInfo;
        float shutdown;
        float log;
        float cameraInfo;
        float versionInfo;
    };
    FontSizes fontSize;

    struct Logging {
        std::string level = "Info";
        bool forceImmediateFlush = false;
        std::string capabilitiesVerbosity = "Default";
        std::vector<ghoul::Dictionary> logs;
    };
    Logging logging;

    std::string scriptLog;

    struct DocumentationInfo {
        std::string path;
    };
    DocumentationInfo documentation;

    std::string versionCheckUrl;
    bool useMultithreadedInitialization = false;

    struct LoadingScreen {
        bool isShowingMessages = true;
        bool isShowingNodeNames = true;
        bool isShowingProgressbar = true;
    };
    LoadingScreen loadingScreen;

    bool isCheckingOpenGLState = false;
    bool isLoggingOpenGLCalls = false;
    bool isPrintingEvents = false;

    float shutdownCountdown = 0.f;

    bool shouldUseScreenshotDate = false;

    std::string onScreenTextScaling = "window";
    bool usePerProfileCache = false;

    bool isRenderingOnMasterDisabled = false;
    glm::vec3 globalRotation = glm::vec3(0.0);
    glm::vec3 screenSpaceRotation = glm::vec3(0.0);
    glm::vec3 masterRotation = glm::vec3(0.0);
    bool isConsoleDisabled = false;
    bool bypassLauncher = false;

    std::map<std::string, ghoul::Dictionary> moduleConfigurations;

    struct OpenGLDebugContext {
        bool isActive = false;
        bool printStacktrace = false;
        bool isSynchronous = true;
        struct IdentifierFilter {
            std::string type;
            std::string source;
            unsigned int identifier = 0;
        };
        std::vector<IdentifierFilter> identifierFilters;
        std::vector<std::string> severityFilters;
    };
    OpenGLDebugContext openGLDebugContext;

    struct HTTPProxy {
        bool usingHttpProxy = false;
        std::string address;
        unsigned int port = 0;
        std::string authentication = "BASIC";
        std::string user;
        std::string password;
    };
    HTTPProxy httpProxy;

    // Values not read from the openspace.cfg file
    std::string sgctConfigNameInitialized;

    static documentation::Documentation Documentation;
    ghoul::lua::LuaState state;
};

std::filesystem::path findConfiguration(const std::string& filename = "openspace.cfg");

Configuration loadConfigurationFromFile(const std::filesystem::path& filename,
    const glm::ivec2& primaryMonitorResolution, std::string_view overrideScript);

} // namespace openspace::configuration

#endif // __OPENSPACE_CORE___CONFIGURATION___H__
