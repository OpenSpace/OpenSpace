/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <map>
#include <string>
#include <vector>

namespace openspace::documentation { struct Documentation; }

namespace openspace::configuration {

struct Configuration {
    std::string windowConfiguration = "${CONFIG}/single.xml";
    std::string asset;
    std::vector<std::string> globalCustomizationScripts;
    std::map<std::string, std::string> pathTokens = {
        { "CACHE" , "CACHE = \"${BASE}/cache\"" }
    };
    std::map<std::string, std::string> fonts;

    struct Logging {
        std::string level = "Info";
        bool forceImmediateFlush = false;
        std::string capabilitiesVerbosity = "Default";
        std::vector<ghoul::Dictionary> logs;
    };
    Logging logging;

    std::string scriptLog = "";

    struct DocumentationInfo {
        std::string lua = "";
        std::string property = "";
        std::string sceneProperty = "";
        std::string keyboard = "";
        std::string documentation = "";
        std::string factory = "";
        std::string license = "";
    };
    DocumentationInfo documentation;

    bool useMultithreadedInitialization = false;

    struct LoadingScreen {
        bool isShowingMessages = true;
        bool isShowingNodeNames = true;
        bool isShowingProgressbar = true;
    };
    LoadingScreen loadingScreen;

    bool isCheckingOpenGLState = false;
    bool isLoggingOpenGLCalls = false;

    float shutdownCountdown = 0.f;

    bool shouldUseScreenshotDate = false;

    std::string onScreenTextScaling = "window";
    bool usePerSceneCache = false;

    bool isRenderingOnMasterDisabled = false;
    glm::dvec3 globalRotation;
    glm::dvec3 masterRotation;
    bool isConsoleDisabled = false;

    std::map<std::string, ghoul::Dictionary> moduleConfigurations;

    std::string renderingMethod = "Framebuffer";

    struct OpenGLDebugContext {
        bool isActive = false;
        bool isSynchronous = true;
        struct IdentifierFilter {
            std::string type = "";
            std::string source = "";
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


    static documentation::Documentation Documentation;
    ghoul::lua::LuaState state;
};

std::string findConfiguration(const std::string& filename = "openspace.cfg");

Configuration loadConfigurationFromFile(const std::string& filename);

void parseLuaState(Configuration& configuration);

} // namespace openspace::configuration

#endif // __OPENSPACE_CORE___CONFIGURATION___H__
