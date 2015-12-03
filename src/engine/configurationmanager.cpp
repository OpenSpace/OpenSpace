/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/util/constants.h>

#include <ghoul/lua/lua_helper.h>
#include <ghoul/filesystem/filesystem.h>

#include <list>

namespace {
	const std::string _loggerCat = "ConfigurationManager";

	const std::string _keyBasePath = "BASE_PATH";
}

namespace openspace {

const std::string ConfigurationManager::KeyPaths = "Paths";
const std::string ConfigurationManager::KeyCache = "CACHE";
const std::string ConfigurationManager::KeyCachePath = KeyPaths + "." + KeyCache;
const std::string ConfigurationManager::KeyFonts = "Fonts";
const std::string ConfigurationManager::KeyConfigSgct = "SGCTConfig";
const std::string ConfigurationManager::KeyLuaDocumentationType = "LuaDocumentationFile.Type";
const std::string ConfigurationManager::KeyLuaDocumentationFile = "LuaDocumentationFile.File";
const std::string ConfigurationManager::KeyPropertyDocumentationType = "PropertyDocumentationFile.Type";
const std::string ConfigurationManager::KeyPropertyDocumentationFile = "PropertyDocumentationFile.File";
const std::string ConfigurationManager::KeyConfigScene = "Scene";
const std::string ConfigurationManager::KeyEnableGui = "EnableGUI";
const std::string ConfigurationManager::KeyStartupScript = "StartupScripts";
const std::string ConfigurationManager::KeySettingsScript = "SettingsScripts";
const std::string ConfigurationManager::KeySpiceTimeKernel = "SpiceKernel.Time";
const std::string ConfigurationManager::KeySpiceLeapsecondKernel = "SpiceKernel.LeapSecond";
const std::string ConfigurationManager::KeyLogLevel = "Logging.LogLevel";
const std::string ConfigurationManager::KeyLogImmediateFlush = "Logging.ImmediateFlush";
const std::string ConfigurationManager::KeyLogs = "Logging.Logs";
const std::string ConfigurationManager::KeyCapabilitiesVerbosity = "Logging.CapabilitiesVerbosity";
const std::string ConfigurationManager::KeyDisableMasterRendering = "DisableRenderingOnMaster";
const std::string ConfigurationManager::KeyDownloadRequestURL = "DownloadRequestURL";

bool ConfigurationManager::loadFromFile(const std::string& filename) {
	using ghoul::filesystem::FileSystem;
    if (!FileSys.fileExists(filename)) {
		LERROR("Could not find file '" << filename << "'");
        return false;
	}

	// ${BASE_PATH}
	std::string&& basePathToken = FileSystem::TokenOpeningBraces + _keyBasePath
        + FileSystem::TokenClosingBraces;

	// Retrieving the directory in which the configuration file lies
    std::string absolutePath = FileSys.absolutePath(filename);
	std::string basePath = ghoul::filesystem::File(absolutePath).directoryName();
	FileSys.registerPathToken(basePathToken, basePath);

	// Loading the configuration file into ourselves
    try {
        ghoul::lua::loadDictionaryFromFile(filename, *this);
    }
    catch (...) {
		LERROR("Loading dictionary from file failed");
		return false;
	}

	// Register all the paths
	ghoul::Dictionary dictionary;
	const bool success = getValue(KeyPaths, dictionary);
	if (!success) {
		LERROR("Configuration does not contain the key '" << KeyPaths << "'");
		return false;
	}

    std::vector<std::string> pathKeys = dictionary.keys();
    for (std::string key : pathKeys) {
        std::string p;
        if (dictionary.getValue(key, p)) {
            std::string fullKey
                  = FileSystem::TokenOpeningBraces + key
                    + FileSystem::TokenClosingBraces;
            LDEBUG("Registering path " << fullKey << ": " << p);
			
			bool override = (basePathToken == fullKey);
			
            if (override)
                LINFO("Overriding base path with '" << p << "'");
            FileSys.registerPathToken(std::move(fullKey), std::move(p), override);
        }
    }

	bool complete = checkCompleteness();
	if (!complete)
		return false;

	// Remove the Paths dictionary from the configuration manager as those paths might
	// change later and we don't want to be forced to keep our local copy up to date
	removeKey(KeyPaths);

	return true;
}

bool ConfigurationManager::checkCompleteness() const {
	std::vector<std::string> requiredTokens = {
		KeyPaths,
		KeyCachePath,
		KeyFonts,
		KeyConfigSgct
	};

	bool totalSuccess = true;
	for (const std::string& token : requiredTokens) {
		bool success = hasKey(token);

		if (!success)
			LFATAL("Configuration file did not contain required key '" << token << "'");

		totalSuccess &= success;
	}

	return totalSuccess;
}

}  // namespace openspace
