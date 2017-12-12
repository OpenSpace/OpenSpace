/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/engine/configurationmanager.h>

#include <openspace/documentation/documentation.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/misc/exception.h>

using std::string;

#include "configurationmanager_doc.inl"

namespace {
    const char* _configurationFile = "openspace.cfg";
    const char* _keyBasePath = "BASE_PATH";
    const char* _initialConfigHelper = "${BASE_PATH}/scripts/configuration_helper.lua";
} // namespace

namespace openspace {

const string ConfigurationManager::KeyPaths = "Paths";
const string ConfigurationManager::KeyCache = "CACHE";
const string ConfigurationManager::KeyFonts = "Fonts";
const string ConfigurationManager::KeyConfigSgct = "SGCTConfig";
const string ConfigurationManager::KeyGlobalCustomizationScripts =
    "GlobalCustomizationScripts";

const string ConfigurationManager::PartType = "Type";
const string ConfigurationManager::PartFile = "File";

const string ConfigurationManager::KeyLuaDocumentation = "LuaDocumentation";
const string ConfigurationManager::KeyScriptLog = "ScriptLog";
const string ConfigurationManager::KeyPropertyDocumentation = "PropertyDocumentation";
const string ConfigurationManager::KeyKeyboardShortcuts = "KeyboardShortcuts";
const string ConfigurationManager::KeyDocumentation = "Documentation";
const string ConfigurationManager::KeyFactoryDocumentation = "FactoryDocumentation";
const string ConfigurationManager::KeySceneLicenseDocumentation = "LicenseDocumentation";
const string ConfigurationManager::KeyConfigScene = "Scene";
const string ConfigurationManager::KeyConfigTasksRoot = "TasksRoot";

const string ConfigurationManager::KeyLogging = "Logging";
const string ConfigurationManager::PartLogDir = "LogDir";
const string ConfigurationManager::PartLogLevel = "LogLevel";
const string ConfigurationManager::PartImmediateFlush = "ImmediateFlush";
const string ConfigurationManager::PartLogPerformancePrefix = "PerformancePrefix";

const string ConfigurationManager::PartLogs = "Logs";
const string ConfigurationManager::PartAppend = "Append";
const string ConfigurationManager::PartCapabilitiesVerbosity = "CapabilitiesVerbosity";

const string ConfigurationManager::KeyLauncher = "Launcher";

const string ConfigurationManager::KeyCapabilitiesVerbosity =
    KeyLogging + "." + PartCapabilitiesVerbosity;

const string ConfigurationManager::KeyShutdownCountdown = "ShutdownCountdown";
const string ConfigurationManager::KeyDisableMasterRendering = "DisableRenderingOnMaster";
const string ConfigurationManager::KeyDisableSceneOnMaster = "DisableSceneOnMaster";
const string ConfigurationManager::KeyDownloadRequestURL = "DownloadRequestURL";
const string ConfigurationManager::KeyPerSceneCache = "PerSceneCache";
const string ConfigurationManager::KeyRenderingMethod = "RenderingMethod";

const string ConfigurationManager::KeyOnScreenTextScaling = "OnScreenTextScaling";

const string ConfigurationManager::KeyHttpProxy = "HttpProxy";
const string ConfigurationManager::PartHttpProxyAddress = "Address";
const string ConfigurationManager::PartHttpProxyPort = "Port";
const string ConfigurationManager::PartHttpProxyAuthentication = "Authentication";
const string ConfigurationManager::PartHttpProxyUser = "User";
const string ConfigurationManager::PartHttpProxyPassword = "Password";

const string ConfigurationManager::KeyOpenGLDebugContext = "OpenGLDebugContext";
const string ConfigurationManager::PartActivate = "Activate";
const string ConfigurationManager::PartSynchronous = "Synchronous";
const string ConfigurationManager::PartFilterIdentifier = "FilterIdentifier";
const string ConfigurationManager::PartFilterIdentifierSource = "Source";
const string ConfigurationManager::PartFilterIdentifierType = "Type";
const string ConfigurationManager::PartFilterIdentifierIdentifier = "Identifier";
const string ConfigurationManager::PartFilterSeverity = "PartFilterSeverity";
const string ConfigurationManager::KeyCheckOpenGLState = "CheckOpenGLState";
const string ConfigurationManager::KeyLogEachOpenGLCall = "LogEachOpenGLCall";

const string ConfigurationManager::KeyUseMultithreadedInitialization =
    "UseMultithreadedInitialization";

const string ConfigurationManager::KeyLoadingScreen = "LoadingScreen";
const string ConfigurationManager::PartShowMessage = "ShowMessage";
const string ConfigurationManager::PartShowNodeNames = "ShowNodeNames";
const string ConfigurationManager::PartShowProgressbar = "ShowProgressbar";

string ConfigurationManager::findConfiguration(const string& filename) {
    using ghoul::filesystem::Directory;

    Directory directory = FileSys.currentDirectory();

    while (true) {
        std::string fullPath = FileSys.pathByAppendingComponent(
            directory,
            _configurationFile
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

void ConfigurationManager::loadFromFile(const string& filename) {
    using ghoul::filesystem::FileSystem;

    ghoul_assert(!filename.empty(), "Filename must not be empty");
    ghoul_assert(FileSys.fileExists(filename), "File must exist");

    // ${BASE_PATH}
    string basePathToken = FileSystem::TokenOpeningBraces + string(_keyBasePath) +
        FileSystem::TokenClosingBraces;

    // Retrieving the directory in which the configuration file lies
    string absolutePath = FileSys.absolutePath(filename);
    string basePath = ghoul::filesystem::File(absolutePath).directoryName();
    FileSys.registerPathToken(basePathToken, basePath);

    ghoul::lua::LuaState state;

    if (FileSys.fileExists(absPath(_initialConfigHelper))) {
        ghoul::lua::runScriptFile(state, absPath(_initialConfigHelper));
    }

    // Loading the configuration file into ourselves
    ghoul::lua::loadDictionaryFromFile(filename, *this, state);

    // Perform testing against the documentation/specification
    openspace::documentation::testSpecificationAndThrow(
        ConfigurationManager::Documentation(),
        *this,
        "ConfigurationManager"
    );

    // Register all the paths
    ghoul::Dictionary dictionary = value<ghoul::Dictionary>(KeyPaths);

    for (std::string key : dictionary.keys()) {
        std::string p = dictionary.value<std::string>(key);
        std::string fullKey =
            FileSystem::TokenOpeningBraces + key + FileSystem::TokenClosingBraces;
        LDEBUGC("ConfigurationManager", "Registering path " << fullKey << ": " << p);

        bool override = (basePathToken == fullKey);
        if (override) {
            LINFOC("ConfigurationManager", "Overriding base path with '" << p << "'");
        }

        using Override = ghoul::filesystem::FileSystem::Override;
        FileSys.registerPathToken(
            std::move(fullKey),
            std::move(p),
            override ? Override::Yes : Override::No
        );
    }

    // Remove the Paths dictionary from the configuration manager as those paths might
    // change later and we don't want to be forced to keep our local copy up to date
    removeKey(KeyPaths);
}

}  // namespace openspace
