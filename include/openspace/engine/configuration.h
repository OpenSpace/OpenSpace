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

namespace openspace {

struct Configuration {
    std::string windowConfiguration;
    std::string asset;
    std::vector<std::string> globalCustomizationScripts;
    std::map<std::string, std::string> pathTokens;
    std::map<std::string, std::string> fonts;

    struct Logging {
        std::string level;
        std::string directory;
        std::string performancePrefix;
        bool forceImmediateFlush;
        std::string capabilitiesVerbosity;
        std::vector<ghoul::Dictionary> logs;
    };
    Logging logging;

    std::string scriptLog;

    struct Documentation {
        std::string lua;
        std::string property;
        std::string sceneProperty;
        std::string keyboard;
        std::string documentation;
        std::string factory;
        std::string license;
    };
    Documentation documentation;

    bool useMultithreadedInitialization;

    struct LoadingScreen {
        bool isShowingMessages;
        bool isShowingNodeNames;
        bool isShowingProgressbar;
    };
    LoadingScreen loadingScreen;

    bool isCheckingOpenGLState;
    bool isLoggingOpenGLCalls;

    int shutdownCountdown;

    bool shouldUseScreenshotDate;

    std::string onScreenTextScaling;
    bool usePerSceneCache;
    
    bool isRenderingOnMasterDisabled;
    bool isSceneOnMasterDisabled;

    std::map<std::string, ghoul::Dictionary> moduleConfigurations;

    std::string renderingMethod;
    
    struct OpenGLDebugContext {
        bool isActive;
        bool isSynchronous;
        struct IdentifierFilter {
            std::string type;
            std::string source;
            int identifier;
        };
        std::vector<IdentifierFilter> identifierFilters;
        std::vector<std::string> severityFilters;
    };
    OpenGLDebugContext openGLDebugContext;

    std::string serverPasskey;
    bool doesRequireSocketAuthentication;
    std::string clientAddressWhitelist;
    std::string webHelperLocation;
    std::string cefWebGuiUrl;

    struct HTTPProxy {
        std::string address;
        std::string port;
        std::string user;
        std::string password;
    };
    HTTPProxy httpProxy;




    ghoul::lua::LuaState state;
};

std::string findConfiguration(const std::string& filename = "openspace.cfg");

Configuration loadConfigurationFromFile(const std::string& filename);

} // namespace openspace

#endif // __OPENSPACE_CORE___CONFIGURATION___H__
