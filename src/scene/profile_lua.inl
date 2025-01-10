/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

namespace {

/**
 * Collects all changes that have been made since startup, including all property changes
 * and assets required, requested, or removed. All changes will be added to the profile
 * that OpenSpace was started with, and the new saved file will contain all of this
 * information. If the argument is provided, the settings will be saved into new profile
 * with that name. If the argument is blank, the current profile will be saved to a backup
 * file and the original profile will be overwritten. The second argument determines if a
 * file that already exists should be overwritten, which is 'false' by default.
 */
[[codegen::luawrap]] void saveSettingsToProfile(std::optional<std::string> saveFilePath,
                                                bool overwrite = true)
{
    using namespace openspace;

    if (!saveFilePath.has_value()) {
        std::time_t t = std::time(nullptr);
        std::tm* utcTime = std::gmtime(&t);
        ghoul_assert(utcTime, "Conversion to UTC failed");

        std::string time = std::format(
            "{:04d}-{:02d}-{:02d}T{:02d}_{:02d}_{:02d}",
            utcTime->tm_year + 1900,
            utcTime->tm_mon + 1,
            utcTime->tm_mday,
            utcTime->tm_hour,
            utcTime->tm_min,
            utcTime->tm_sec
        );
        std::filesystem::path path = global::configuration->profile;
        path.replace_extension();
        std::string newFile = std::format("{}_{}", path, time);
        std::string sourcePath = std::format(
            "{}/{}.profile", absPath("${USER_PROFILES}"), global::configuration->profile
        );
        std::string destPath = std::format(
            "{}/{}.profile", absPath("${PROFILES}"), global::configuration->profile
        );
        if (!std::filesystem::is_regular_file(sourcePath)) {
            sourcePath = std::format(
                "{}/{}.profile",
                absPath("${USER_PROFILES}"), global::configuration->profile
            );
        }
        LINFOC(
            "Profile",
            std::format("Saving a copy of the old profile as '{}'", newFile)
        );
        std::filesystem::copy(sourcePath, destPath);
        saveFilePath = global::configuration->profile;
    }
    if (saveFilePath->empty()) {
        throw ghoul::lua::LuaError("Save filepath string is empty");
    }

    const properties::PropertyOwner& root = *global::rootPropertyOwner;
    std::string currentTime = std::string(global::timeManager->time().ISO8601());
    interaction::NavigationState navState = global::navigationHandler->navigationState();
    global::profile->saveCurrentSettingsToProfile(root, currentTime, navState);
    global::configuration->profile = *saveFilePath;

    if (saveFilePath->find('/') != std::string::npos) {
        throw ghoul::lua::LuaError("Profile filename must not contain (/) elements");
    }
    else if (saveFilePath->find(':') != std::string::npos) {
        throw ghoul::lua::LuaError("Profile filename must not contain (:) elements");
    }
    else if (saveFilePath->find('.') != std::string::npos) {
        throw ghoul::lua::LuaError(
            "Only provide the filename to save without file extension"
        );
    }

    std::string absFilename = std::format(
        "{}/{}.profile", absPath("${PROFILES}"), *saveFilePath
    );
    if (!std::filesystem::is_regular_file(absFilename)) {
        absFilename = absPath("${USER_PROFILES}/" + *saveFilePath + ".profile").string();
    }

    if (std::filesystem::is_regular_file(absFilename) && !overwrite) {
        throw ghoul::lua::LuaError(
            std::format(
                "Unable to save profile '{}'. File of same name already exists",
                absFilename
            )
        );
    }

    std::ofstream outFile;
    // @TODO (abock, 2020-06-15) Replace with non-throwing stream
    try {
        outFile.open(absFilename, std::ofstream::out);
    }
    catch (const std::ofstream::failure& e) {
        throw ghoul::lua::LuaError(
            std::format(
                "Exception opening profile file for write '{}': {}", absFilename, e.what()
            )
        );
    }

    try {
        outFile << global::profile->serialize();
    }
    catch (const std::ofstream::failure& e) {
        throw ghoul::lua::LuaError(
            std::format("Data write error to file '{}': {}", absFilename, e.what())
        );
    }
}

#include "profile_lua_codegen.cpp"

} // namespace
