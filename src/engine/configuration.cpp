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

#include <openspace/engine/configuration.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/assert.h>

namespace {
    constexpr const char* BasePathToken = "${BASE}";
    // We can't use ${SCRIPTS} here as that hasn't been defined by this point
    constexpr const char* InitialConfigHelper =
                                               "${BASE}/scripts/configuration_helper.lua";

    // Variable names for the openspace.cfg file
    // These are also used in the _doc include file
    constexpr const char* KeySGCTConfig = "SGCTConfig";
    constexpr const char* KeyAsset = "Asset";
    constexpr const char* KeyProfile = "Profile";
    constexpr const char* KeyGlobalCustomizationScripts = "GlobalCustomizationScripts";
    constexpr const char* KeyPaths = "Paths";
    constexpr const char* KeyFonts = "Fonts";
    constexpr const char* KeyLogging = "Logging";
    constexpr const char* KeyLogDir = "LogDir";
    constexpr const char* KeyPerformancePrefix = "PerformancePrefix";
    constexpr const char* KeyLogLevel = "LogLevel";
    constexpr const char* KeyImmediateFlush = "ImmediateFlush";
    constexpr const char* KeyLogs = "Logs";
    constexpr const char* KeyCapabilitiesVerbosity = "CapabilitiesVerbosity";
    constexpr const char* KeyDocumentationPath = "Path";
    constexpr const char* KeyDocumentation = "Documentation";
    constexpr const char* KeyScriptLog = "ScriptLog";
    constexpr const char* KeyShutdownCountdown = "ShutdownCountdown";
    constexpr const char* KeyPerSceneCache = "PerSceneCache";
    constexpr const char* KeyOnScreenTextScaling = "OnScreenTextScaling";
    constexpr const char* KeyRenderingMethod = "RenderingMethod";
    constexpr const char* KeyDisableRenderingOnMaster = "DisableRenderingOnMaster";
    constexpr const char* KeyGlobalRotation = "GlobalRotation";
    constexpr const char* KeyScreenSpaceRotation = "ScreenSpaceRotation";
    constexpr const char* KeyMasterRotation = "MasterRotation";
    constexpr const char* KeyDisableInGameConsole = "DisableInGameConsole";
    constexpr const char* KeyScreenshotUseDate = "ScreenshotUseDate";
    constexpr const char* KeyHttpProxy = "HttpProxy";
    constexpr const char* KeyAddress = "Address";
    constexpr const char* KeyPort = "Port";
    constexpr const char* KeyAuthentication = "Authentication";
    constexpr const char* KeyUser = "User";
    constexpr const char* KeyPassword = "Password";
    constexpr const char* KeyOpenGLDebugContext = "OpenGLDebugContext";
    constexpr const char* KeyActivate = "Activate";
    constexpr const char* KeySynchronous = "Synchronous";
    constexpr const char* KeyFilterIdentifier = "FilterIdentifier";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeySource = "Source";
    constexpr const char* KeyType = "Type";
    constexpr const char* KeyFilterSeverity = "FilterSeverity";
    constexpr const char* KeyCheckOpenGLState = "CheckOpenGLState";
    constexpr const char* KeyLogEachOpenGLCall = "LogEachOpenGLCall";
    constexpr const char* KeyVersionCheckUrl = "VersionCheckUrl";
    constexpr const char* KeyUseMultithreadedInitialization =
                                                         "UseMultithreadedInitialization";
    constexpr const char* KeyLoadingScreen = "LoadingScreen";
    constexpr const char* KeyShowMessage = "ShowMessage";
    constexpr const char* KeyShowNodeNames = "ShowNodeNames";
    constexpr const char* KeyShowProgressbar = "ShowProgressbar";
    constexpr const char* KeyModuleConfigurations = "ModuleConfigurations";

    constexpr const char* KeySgctConfigNameInitialized = "sgctconfiginitializeString";
    constexpr const char* KeyReadOnlyProfiles = "ReadOnlyProfiles";
    constexpr const char* KeyBypassLauncher = "BypassLauncher";

    template <typename T>
    void getValue(ghoul::lua::LuaState& L, const char* name, T& value) {
        using namespace openspace::configuration;

        auto it = std::find_if(
            Configuration::Documentation.entries.begin(),
            Configuration::Documentation.entries.end(),
            [name](const openspace::documentation::DocumentationEntry& e) {
            return e.key == name;
        }
        );

        bool isOptional =
            it != Configuration::Documentation.entries.end()
            ? it->optional :
            true;

        lua_getglobal(L, name);
        if (isOptional && lua_isnil(L, -1)) {
            return;
        }

        if (!isOptional && lua_isnil(L, -1)) {
            openspace::documentation::TestResult testResult = {
                false,
                { {
                    name,
                    openspace::documentation::TestResult::Offense::Reason::MissingKey
                }},
                {}
            };
            throw openspace::documentation::SpecificationError(
                std::move(testResult),
                "Configuration"
            );
        }

        if constexpr (std::is_same_v<T, glm::dvec3>) {
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);
            glm::dvec3 res;
            res.x = d.value<double>("1");
            res.y = d.value<double>("2");
            res.z = d.value<double>("3");
            value = res;
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, std::vector<std::string>>) {
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            std::vector<std::string> res;
            for (size_t i = 1; i <= d.size(); ++i) {
                res.push_back(d.value<std::string>(std::to_string(i)));
            }
            value = res;
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, std::map<std::string, std::string>>) {
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            std::map<std::string, std::string> res;
            for (size_t i = 0; i < d.size(); ++i) {
                std::string key = d.keys()[i];
                std::string v = d.value<std::string>(key);
                res[std::move(key)] = std::move(v);
            }
            value = res;
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, std::map<std::string, ghoul::Dictionary>>) {
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            std::map<std::string, ghoul::Dictionary> res;
            for (size_t i = 0; i < d.size(); ++i) {
                std::string key = d.keys()[i];
                ghoul::Dictionary v = d.value<ghoul::Dictionary>(key);
                res[std::move(key)] = std::move(v);
            }
            value = res;
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::Logging>) {
            Configuration::Logging& v = static_cast<Configuration::Logging&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            d.getValue(KeyLogLevel, v.level);
            d.getValue(KeyImmediateFlush, v.forceImmediateFlush);
            d.getValue(KeyCapabilitiesVerbosity, v.capabilitiesVerbosity);

            if (d.hasKey(KeyLogs) && d.hasValue<ghoul::Dictionary>(KeyLogs)) {
                ghoul::Dictionary l = d.value<ghoul::Dictionary>(KeyLogs);
                std::vector<ghoul::Dictionary> res;
                for (size_t i = 1; i <= l.size(); ++i) {
                    res.push_back(l.value<ghoul::Dictionary>(std::to_string(i)));
                }
                v.logs = res;
            }
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::DocumentationInfo>) {
            Configuration::DocumentationInfo& v =
                static_cast<Configuration::DocumentationInfo&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            d.getValue(KeyDocumentationPath, v.path);
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::LoadingScreen>) {
            Configuration::LoadingScreen& v =
                static_cast<Configuration::LoadingScreen&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            d.getValue(KeyShowMessage, v.isShowingMessages);
            d.getValue(KeyShowNodeNames, v.isShowingNodeNames);
            d.getValue(KeyShowProgressbar, v.isShowingProgressbar);
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::OpenGLDebugContext>) {
            Configuration::OpenGLDebugContext& v =
                static_cast<Configuration::OpenGLDebugContext&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            d.getValue(KeyActivate, v.isActive);
            d.getValue(KeySynchronous, v.isSynchronous);

            if (d.hasKey(KeyFilterIdentifier) &&
                d.hasValue<ghoul::Dictionary>(KeyFilterIdentifier))
            {
                ghoul::Dictionary f = d.value<ghoul::Dictionary>(KeyFilterIdentifier);

                std::vector<Configuration::OpenGLDebugContext::IdentifierFilter> res;
                for (size_t i = 1; i <= f.size(); ++i) {
                    Configuration::OpenGLDebugContext::IdentifierFilter filter;
                    ghoul::Dictionary fi = f.value<ghoul::Dictionary>(std::to_string(i));

                    double id = static_cast<double>(filter.identifier);
                    fi.getValue(KeyIdentifier, id);
                    filter.identifier = static_cast<unsigned int>(id);
                    fi.getValue(KeySource, filter.source);
                    fi.getValue(KeyType, filter.type);

                    res.push_back(filter);
                }

                v.identifierFilters = res;
            }

            if (d.hasKey(KeyFilterSeverity) &&
                d.hasValue<ghoul::Dictionary>(KeyFilterSeverity))
            {
                ghoul::Dictionary f = d.value<ghoul::Dictionary>(KeyFilterSeverity);

                std::vector<std::string> res;
                for (size_t i = 1; i <= f.size(); ++i) {
                    res.push_back(f.value<std::string>(std::to_string(i)));
                }
                v.severityFilters = res;
            }
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::HTTPProxy>) {
            Configuration::HTTPProxy& v = static_cast<Configuration::HTTPProxy&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            d.getValue(KeyActivate, v.usingHttpProxy);
            d.getValue(KeyAddress, v.address);
            double p = static_cast<double>(v.port);
            d.getValue(KeyPort, p);
            v.port = static_cast<unsigned int>(p);
            d.getValue(KeyAuthentication, v.authentication);
            d.getValue(KeyUser, v.user);
            d.getValue(KeyPassword, v.password);
        }
        else {
            value = ghoul::lua::value<T>(L);
        }
    }

} // namespace

#include "configuration_doc.inl"

namespace openspace::configuration {

void parseLuaState(Configuration& configuration) {
    using namespace ghoul::lua;

    // Shorten the rest of this function
    Configuration& c = configuration;
    LuaState& s = c.state;

    getValue(s, KeySGCTConfig, c.windowConfiguration);
    getValue(s, KeyAsset, c.asset);
    getValue(s, KeyProfile, c.profile);
    getValue(s, KeyGlobalCustomizationScripts, c.globalCustomizationScripts);
    getValue(s, KeyPaths, c.pathTokens);
    getValue(s, KeyFonts, c.fonts);
    getValue(s, KeyScriptLog, c.scriptLog);
    getValue(s, KeyVersionCheckUrl, c.versionCheckUrl);
    getValue(s, KeyUseMultithreadedInitialization, c.useMultithreadedInitialization);
    getValue(s, KeyCheckOpenGLState, c.isCheckingOpenGLState);
    getValue(s, KeyLogEachOpenGLCall, c.isLoggingOpenGLCalls);
    getValue(s, KeyShutdownCountdown, c.shutdownCountdown);
    getValue(s, KeyScreenshotUseDate, c.shouldUseScreenshotDate);
    getValue(s, KeyOnScreenTextScaling, c.onScreenTextScaling);
    getValue(s, KeyPerSceneCache, c.usePerSceneCache);
    getValue(s, KeyDisableRenderingOnMaster, c.isRenderingOnMasterDisabled);

    getValue(s, KeyGlobalRotation, c.globalRotation);
    getValue(s, KeyScreenSpaceRotation, c.screenSpaceRotation);
    getValue(s, KeyMasterRotation, c.masterRotation);
    getValue(s, KeyDisableInGameConsole, c.isConsoleDisabled);
    getValue(s, KeyRenderingMethod, c.renderingMethod);
    getValue(s, KeyLogging, c.logging);
    getValue(s, KeyDocumentation, c.documentation);
    getValue(s, KeyLoadingScreen, c.loadingScreen);
    getValue(s, KeyModuleConfigurations, c.moduleConfigurations);
    getValue(s, KeyOpenGLDebugContext, c.openGLDebugContext);
    getValue(s, KeyHttpProxy, c.httpProxy);

    getValue(s, KeySgctConfigNameInitialized, c.sgctConfigNameInitialized);
    getValue(s, KeyReadOnlyProfiles, c.readOnlyProfiles);
    getValue(s, KeyBypassLauncher, c.bypassLauncher);
}

std::string findConfiguration(const std::string& filename) {
    using ghoul::filesystem::Directory;

    Directory directory = FileSys.absolutePath("${BIN}");

    while (true) {
        std::string fullPath = FileSys.pathByAppendingComponent(
            directory,
            filename
        );

        if (FileSys.fileExists(fullPath)) {
            // We have found the configuration file and can bail out
            return fullPath;
        }

        // Otherwise, we traverse the directory tree up
        Directory nextDirectory = directory.parentDirectory(
            ghoul::filesystem::Directory::AbsolutePath::Yes
        );

        if (directory.path() == nextDirectory.path()) {
            // We have reached the root of the file system and did not find the file
            throw ghoul::RuntimeError(
                "Could not find configuration file '" + filename + "'",
                "ConfigurationManager"
            );
        }
        directory = nextDirectory;
    }
}

Configuration loadConfigurationFromFile(const std::string& filename) {
    ghoul_assert(!filename.empty(), "Filename must not be empty");
    ghoul_assert(FileSys.fileExists(filename), "File must exist");

    Configuration result;

    // Register the base path as the directory where 'filename' lives
    std::string basePath = ghoul::filesystem::File(filename).directoryName();
    FileSys.registerPathToken(BasePathToken, basePath);

    // If there is an initial config helper file, load it into the state
    if (FileSys.fileExists(absPath(InitialConfigHelper))) {
        ghoul::lua::runScriptFile(result.state, absPath(InitialConfigHelper));
    }

    // Load the configuration file into the state
    ghoul::lua::runScriptFile(result.state, filename);

    parseLuaState(result);

    return result;
}

} // namespace openspace::configuration
