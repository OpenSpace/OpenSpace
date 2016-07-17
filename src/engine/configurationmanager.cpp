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

namespace {
    const string _configurationFile = "openspace.cfg";
    const string _keyBasePath = "BASE_PATH";
}

namespace openspace {

const string ConfigurationManager::KeyPaths = "Paths";
const string ConfigurationManager::KeyCache = "CACHE";
const string ConfigurationManager::KeyFonts = "Fonts";
const string ConfigurationManager::KeyConfigSgct = "SGCTConfig";
const string ConfigurationManager::KeyLuaDocumentationType = "LuaDocumentationFile.Type";
const string ConfigurationManager::KeyLuaDocumentationFile = "LuaDocumentationFile.File";
const string ConfigurationManager::KeyScriptLogType = "ScriptLogFile.Type";
const string ConfigurationManager::KeyScriptLogFile = "ScriptLogFile.File";
const string ConfigurationManager::KeyPropertyDocumentationType =
    "PropertyDocumentationFile.Type";
const string ConfigurationManager::KeyPropertyDocumentationFile =
    "PropertyDocumentationFile.File";
const string ConfigurationManager::KeyKeyboardShortcutsType = "KeyboardShortcuts.Type";
const string ConfigurationManager::KeyKeyboardShortcutsFile = "KeyboardShortcuts.File";
const string ConfigurationManager::KeyConfigScene = "Scene";
const string ConfigurationManager::KeySpiceTimeKernel = "SpiceKernel.Time";
const string ConfigurationManager::KeySpiceLeapsecondKernel = "SpiceKernel.LeapSecond";
const string ConfigurationManager::KeyLogLevel = "Logging.LogLevel";
const string ConfigurationManager::KeyLogImmediateFlush = "Logging.ImmediateFlush";
const string ConfigurationManager::KeyLogs = "Logging.Logs";
const string ConfigurationManager::KeyCapabilitiesVerbosity =
    "Logging.CapabilitiesVerbosity";
const string ConfigurationManager::KeyShutdownCountdown = "ShutdownCountdown";
const string ConfigurationManager::KeyDisableMasterRendering = "DisableRenderingOnMaster";
const string ConfigurationManager::KeyDownloadRequestURL = "DownloadRequestURL";

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
    
    if (!FileSys.fileExists(filename))
        throw ghoul::FileNotFoundError(filename, "ConfigurationManager");

    // ${BASE_PATH}
    string basePathToken = FileSystem::TokenOpeningBraces + _keyBasePath
        + FileSystem::TokenClosingBraces;

    // Retrieving the directory in which the configuration file lies
    string absolutePath = FileSys.absolutePath(filename);
    string basePath = ghoul::filesystem::File(absolutePath).directoryName();
    FileSys.registerPathToken(basePathToken, basePath);

    // Loading the configuration file into ourselves
    ghoul::lua::loadDictionaryFromFile(filename, *this);

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
