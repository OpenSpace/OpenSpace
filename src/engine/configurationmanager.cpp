/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/exception.h>
#include <ghoul/filesystem/filesystem.h>

#include <list>

using std::string;

#include "configurationmanager_doc.inl"

namespace {
    const string _configurationFile = "openspace.cfg";
    const string _keyBasePath = "BASE_PATH";
}

namespace openspace {

const string ConfigurationManager::KeyPaths = "Paths";
const string ConfigurationManager::KeyCache = "CACHE";
const string ConfigurationManager::KeyFonts = "Fonts";
const string ConfigurationManager::KeyConfigSgct = "SGCTConfig";

const string ConfigurationManager::PartType = "Type";
const string ConfigurationManager::PartFile = "File";

const string ConfigurationManager::KeyLuaDocumentation = "LuaDocumentation";
const string ConfigurationManager::KeyScriptLog = "ScriptLog";
const string ConfigurationManager::KeyPropertyDocumentation = "PropertyDocumentation";
const string ConfigurationManager::KeyKeyboardShortcuts = "KeyboardShortcuts";
const string ConfigurationManager::KeyDocumentation = "Documentation";
const string ConfigurationManager::KeyFactoryDocumentation = "FactoryDocumentation";
const string ConfigurationManager::KeyConfigScene = "Scene";

const string ConfigurationManager::KeyLogging = "Logging";
const string ConfigurationManager::PartLogLevel = "LogLevel";
const string ConfigurationManager::PartImmediateFlush = "ImmediateFlush";
const string ConfigurationManager::PartLogs = "Logs";
const string ConfigurationManager::PartAppend = "Append";
const string ConfigurationManager::PartCapabilitiesVerbosity = "CapabilitiesVerbosity";

const string ConfigurationManager::KeyCapabilitiesVerbosity =
    KeyLogging + "." + PartCapabilitiesVerbosity;

const string ConfigurationManager::KeyShutdownCountdown = "ShutdownCountdown";
const string ConfigurationManager::KeyDisableMasterRendering = "DisableRenderingOnMaster";
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

string ConfigurationManager::findConfiguration(const string& filename) {
    using ghoul::filesystem::Directory;
    
    Directory directory = FileSys.currentDirectory();
    std::string configurationName = _configurationFile;
    
    while (true) {
        std::string&& fullPath = FileSys.pathByAppendingComponent(directory,
                                                                  configurationName);
        bool exists = FileSys.fileExists(fullPath);
        if (exists)
            return fullPath;
        
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
    
    if (!FileSys.fileExists(filename)) {
        throw ghoul::FileNotFoundError(filename, "ConfigurationManager");
    }

    // ${BASE_PATH}
    string basePathToken = FileSystem::TokenOpeningBraces + _keyBasePath
        + FileSystem::TokenClosingBraces;

    // Retrieving the directory in which the configuration file lies
    string absolutePath = FileSys.absolutePath(filename);
    string basePath = ghoul::filesystem::File(absolutePath).directoryName();
    FileSys.registerPathToken(basePathToken, basePath);

    // Loading the configuration file into ourselves
    ghoul::lua::loadDictionaryFromFile(filename, *this);

    // Perform testing against the documentation/specification
    openspace::documentation::testSpecificationAndThrow(
        ConfigurationManager::Documentation(),
        *this,
        "ConfigurationManager"
    );

    // Register all the paths
    ghoul::Dictionary dictionary = value<ghoul::Dictionary>(KeyPaths);

    std::vector<std::string> pathKeys = dictionary.keys();
    for (std::string key : pathKeys) {
        std::string p;
        if (dictionary.getValue(key, p)) {
            std::string fullKey =
                FileSystem::TokenOpeningBraces + key + FileSystem::TokenClosingBraces;
            LDEBUGC("ConfigurationManager", "Registering path " << fullKey << ": " << p);
            
            bool override = (basePathToken == fullKey);
            if (override)
                LINFOC("ConfigurationManager", "Overriding base path with '" << p << "'");

            using Override = ghoul::filesystem::FileSystem::Override;
            FileSys.registerPathToken(
                std::move(fullKey),
                std::move(p),
                override ? Override::Yes : Override::No
            );
        }
    }

    bool complete = checkCompleteness();
    if (!complete) {
        throw ghoul::RuntimeError(
            "Configuration file '" + filename + "' was not complete",
            "ConfigurationManager"
        );
    }

    // Remove the Paths dictionary from the configuration manager as those paths might
    // change later and we don't want to be forced to keep our local copy up to date
    removeKey(KeyPaths);
}

bool ConfigurationManager::checkCompleteness() const {
    std::vector<std::string> requiredTokens = {
        KeyPaths,
        KeyPaths + "." + KeyCache,
        KeyFonts,
        KeyConfigSgct
    };

    bool totalSuccess = true;
    for (const std::string& token : requiredTokens) {
        bool success = hasKey(token);

        if (!success) {
            LFATALC(
                "ConfigurationManager",
                "Configuration file did not contain required key '" << token << "'"
            );
        }

        totalSuccess &= success;
    }

    return totalSuccess;
}

}  // namespace openspace
