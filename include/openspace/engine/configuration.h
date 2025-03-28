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

#ifndef __OPENSPACE_CORE___CONFIGURATION___H__
#define __OPENSPACE_CORE___CONFIGURATION___H__

#include <openspace/properties/property.h>
#include <openspace/util/keys.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/misc/dictionary.h>
#include <filesystem>
#include <map>
#include <string>
#include <vector>

namespace openspace {

namespace documentation { struct Documentation; }

struct Configuration {
    Configuration() = default;
    Configuration(Configuration&&) = default;
    Configuration(const Configuration&) = delete;
    Configuration& operator=(const Configuration&) = delete;
    Configuration& operator=(Configuration&&) = default;

    ghoul::Dictionary createDictionary();

    std::string windowConfiguration = "${CONFIG}/single.json";
    std::string asset;
    std::string profile;

    properties::Property::Visibility propertyVisibility =
        properties::Property::Visibility::User;

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
    bool verboseScriptLog = false;
    int scriptLogRotation = 3;

    struct DocumentationInfo {
        std::string path;
    };
    DocumentationInfo documentation;

    std::string versionCheckUrl;
    bool useMultithreadedInitialization = false;

    struct LoadingScreen {
        bool isShowingMessages = true;
        bool isShowingNodeNames = true;
        bool isShowingLogMessages = true;
    };
    LoadingScreen loadingScreen;

    bool isCheckingOpenGLState = false;
    bool isLoggingOpenGLCalls = false;
    bool isPrintingEvents = false;

    Key consoleKey = Key::GraveAccent;

    float shutdownCountdown = 0.f;

    bool shouldUseScreenshotDate = false;

    bool sandboxedLua = true;

    std::string onScreenTextScaling = "window";
    bool usePerProfileCache = false;

    bool isRenderingOnMasterDisabled = false;
    glm::vec3 globalRotation = glm::vec3(0.0);
    glm::vec3 screenSpaceRotation = glm::vec3(0.0);
    glm::vec3 masterRotation = glm::vec3(0.0);
    bool isConsoleDisabled = false;
    bool bypassLauncher = false;

    enum LayerServer {
        All = 0,
        NewYork,
        Sweden,
        Utah,
        None
    };
    LayerServer layerServer = LayerServer::All;

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

    static documentation::Documentation Documentation();
    ghoul::lua::LuaState state;
};

std::filesystem::path findConfiguration(const std::string& filename = "openspace.cfg");

Configuration loadConfigurationFromFile(const std::filesystem::path& configurationFile,
    const std::filesystem::path& settingsFile,
    const glm::ivec2& primaryMonitorResolution);

Configuration::LayerServer stringToLayerServer(std::string_view server);
std::string layerServerToString(Configuration::LayerServer server);

} // namespace openspace

#endif // __OPENSPACE_CORE___CONFIGURATION___H__
