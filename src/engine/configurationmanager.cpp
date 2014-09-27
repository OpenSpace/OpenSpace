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
}

namespace openspace {

bool ConfigurationManager::loadFromFile(const std::string& filename) {
	const bool basePathSuccess = registerBasePathFromConfigurationFile(filename);
	if (!basePathSuccess) {
		LERROR("Registering Base Path failed");
		return false;
	}
	
	const bool loadingSuccess = ghoul::lua::loadDictionaryFromFile(filename, *this);
	if (!loadingSuccess) {
		LERROR("Loading dictionary from file failed");
		return false;
	}

	const bool registerSuccess = registerPaths();
	if (!registerSuccess) {
		LERROR("Registering paths failed");
		return false;
	}

	return true;
}

bool ConfigurationManager::registerPaths() {
	ghoul::Dictionary dictionary;
	const bool success = getValueSafe(constants::configurationmanager::keyPaths, dictionary);
	if (!success) {
		LERROR("Configuration does not contain the key '" <<
			constants::configurationmanager::keyPaths << "'");
		return false;
	}

    std::vector<std::string>&& pathKeys = dictionary.keys();
    for (const std::string& key : pathKeys) {
        std::string p;
        if (dictionary.getValue(key, p)) {
            const std::string fullKey
                  = ghoul::filesystem::FileSystem::TokenOpeningBraces + key
                    + ghoul::filesystem::FileSystem::TokenClosingBraces;
            LDEBUG("Registering path " << fullKey << ": " << p);
			
			std::string&& basePath = ghoul::filesystem::FileSystem::TokenOpeningBraces +
					constants::configurationmanager::paths::keyBasePath
                    + ghoul::filesystem::FileSystem::TokenClosingBraces;

			bool override = (basePath == fullKey);
			
            if (override)
                LINFO("Overriding base path with '" << p << "'");
            FileSys.registerPathToken(fullKey, p, override);
        }
    }
	return true;
}

bool ConfigurationManager::registerBasePathFromConfigurationFile(const std::string& filename)
{
    if (!FileSys.fileExists(filename))
        return false;

    const std::string absolutePath = FileSys.absolutePath(filename);

    std::string::size_type last
          = absolutePath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
    if (last == std::string::npos)
        return false;

    std::string basePath = absolutePath.substr(0, last);

	std::string&& basePathToken = ghoul::filesystem::FileSystem::TokenOpeningBraces +
			constants::configurationmanager::paths::keyBasePath
            + ghoul::filesystem::FileSystem::TokenClosingBraces;

	FileSys.registerPathToken(basePathToken, basePath);

    return true;
}

}  // namespace openspace
