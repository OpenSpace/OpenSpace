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

#include <openspace/engine/configuration.h>

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/assert.h>


namespace {
    constexpr const char* BasePathToken = "${BASE}";
    // We can't use ${SCRIPTS} here as that hasn't been defined by this point
    constexpr const char* InitialConfigHelper =
                                               "${BASE}/scripts/configuration_helper.lua";

    constexpr const char* KeySGCTConfig = "SGCTConfig";
    constexpr const char* KeyAsset = "Asset";
    constexpr const char* KeyGlobalCustomizationScripts = "GlobalCustomizationScripts";
    constexpr const char* KeyPaths = "Paths";
    constexpr const char* KeyPathsCACHE = "Paths.CACHE";
    constexpr const char* KeyFonts = "Fonts";
    constexpr const char* KeyLogging = "Logging";
    constexpr const char* KeyLogDir = "LogDir";
    constexpr const char* KeyPerformancePrefix = "PerformancePrefix";
    constexpr const char* KeyLogLevel = "LogLevel";
    constexpr const char* KeyImmediateFlush = "ImmediateFlush";
    constexpr const char* KeyLogs = "Logs";
    constexpr const char* KeyCapabilitiesVerbosity = "CapabilitiesVerbosity";
    constexpr const char* KeyLuaDocumentation = "LuaDocumentation";
    constexpr const char* KeyPropertyDocumentation = "PropertyDocumentation";
    constexpr const char* KeyScriptLog = "ScriptLog";
    constexpr const char* KeyKeyboardShortcuts = "KeyboardShortcuts";
    constexpr const char* KeyDocumentation = "Documentation";
    constexpr const char* KeyFactoryDocumentation = "FactoryDocumentation";
    constexpr const char* KeyRequireSocketAuthentication = "RequireSocketAuthentication";
    constexpr const char* KeyServerPasskey = "ServerPasskey";
    constexpr const char* KeyClientAddressWhitelist = "ClientAddressWhitelist";
    constexpr const char* KeyLicenseDocumentation = "LicenseDocumentation";
    constexpr const char* KeyShutdownCountdown = "ShutdownCountdown";
    constexpr const char* KeyPerSceneCache = "PerSceneCache";
    constexpr const char* KeyOnScreenTextScaling = "OnScreenTextScaling";
    constexpr const char* KeyRenderingMethod = "RenderingMethod";
    constexpr const char* KeyDisableRenderingOnMaster = "DisableRenderingOnMaster";
    constexpr const char* KeyDisableSceneOnMaster = "DisableSceneOnMaster";
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
    constexpr const char* KeyUseMultithreadedInitialization =
                                                         "UseMultithreadedInitialization";
    constexpr const char* KeyLoadingScreen = "LoadingScreen";
    constexpr const char* KeyShowMessage = "ShowMessage";
    constexpr const char* KeyShowNodeNames = "ShowNodeNames";
    constexpr const char* KeyShowProgressbar = "ShowProgressbar";
    constexpr const char* KeyModuleConfigurations = "ModuleConfigurations";

} // namespace

#include "configuration_doc.inl"

namespace openspace {

void parseLuaState(Configuration& configuration) {
    ghoul::lua::LuaState& s = configuration.state;

    // Load global table and apply documentation feature 
    configuration.windowConfiguration = ghoul::lua::value<std::string>(s, KeySGCTConfig);
}


std::string findConfiguration(const std::string& filename) {
    using ghoul::filesystem::Directory;

    Directory directory = FileSys.currentDirectory();

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

} // namespace openspace
