/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

namespace {
	const std::string _loggerCat = "ConfigurationManager";

	const std::string _keyBasePath = "BASE_PATH";
}

namespace openspace {

bool ConfigurationManager::loadFromFile(const std::string& filename) {
	using ghoul::filesystem::FileSystem;
	using constants::configurationmanager::keyPaths;
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
	const bool loadingSuccess = ghoul::lua::loadDictionaryFromFile(filename, *this);
	if (!loadingSuccess) {
		LERROR("Loading dictionary from file failed");
		return false;
	}

	// Register all the paths
	ghoul::Dictionary dictionary;
	const bool success = getValue(keyPaths, dictionary);
	if (!success) {
		LERROR("Configuration does not contain the key '" << keyPaths << "'");
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

	

	return true;
}


}  // namespace openspace
