/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>

using namespace openspace;

namespace {

/**
 * Returns the name of the profile with which OpenSpace was started.
 */
[[codegen::luawrap]] std::string profileName() {
    std::string p = global::configuration->profile.profile;
    const std::string builtInPath = absPath("${PROFILES}").string();
    const std::string userPath = absPath("${USER_PROFILES}").string();

    if (p.starts_with(builtInPath)) {
        return p.substr(builtInPath.size() + 1);
    }
    else if (p.starts_with(userPath)) {
        return p.substr(userPath.size() + 1);
    }
    else {
        return p;
    }
}

/**
 * Returns the full path of the profile with which OpenSpace was started.
 */
[[codegen::luawrap]] std::filesystem::path profilePath() {
    return global::configuration->profile.profile;
}

/**
 * Returns the enabled add-ons of the profile that was started.
 */
[[codegen::luawrap]] std::vector<std::string> profileAddons() {
    return global::configuration->profile.addons;
}

/**
 * Collects all changes that have been made since startup, including all property changes
 * and assets required, requested, or removed. All changes will be added to the profile
 * that OpenSpace was started with, and the new saved file will contain all of this
 * information. If the argument is provided, the settings will be saved into new profile
 * with that name. If the argument is blank, the current profile will be saved to a backup
 * file and the original profile will be overwritten. The second argument determines if a
 * file that already exists should be overwritten, which is 'false' by default.
 */
[[codegen::luawrap]] void saveSettingsToProfile(std::optional<std::string> saveFileName,
                                                bool shouldOverwrite = false)
{
    const bool receivedSaveFileName = saveFileName.has_value();
    if (!receivedSaveFileName) {
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
        std::filesystem::path path = global::configuration->profile.profile;
        const std::string extension = path.extension().string();
        path.replace_extension();
        const std::string backupPath = std::format("{}_{}{}", path, time, extension);
        const std::string sourcePath = global::configuration->profile.profile;
        LINFOC(
            "Profile",
            std::format("Saving a copy of the old profile as '{}'", backupPath)
        );
        std::filesystem::copy(sourcePath, backupPath);
        saveFileName = path.stem().string();
    }
    if (saveFileName->empty()) {
        throw ghoul::lua::LuaError("Save filepath string is empty");
    }

    const PropertyOwner& root = *global::rootPropertyOwner;
    std::string currentTime = std::string(global::timeManager->time().ISO8601());
    NavigationState navState = global::navigationHandler->navigationState();
    global::profile->saveCurrentSettingsToProfile(root, currentTime, navState);

    if (saveFileName->contains('/')) {
        throw ghoul::lua::LuaError("Profile filename must not contain (/) elements");
    }
    else if (saveFileName->contains(':')) {
        throw ghoul::lua::LuaError("Profile filename must not contain (:) elements");
    }
    else if (saveFileName->contains('.')) {
        throw ghoul::lua::LuaError(
            "Only provide the filename to save without file extension"
        );
    }

    std::string absFilename = std::format(
        "{}/{}.profile", absPath("${PROFILES}"), *saveFileName
    );
    if (!std::filesystem::is_regular_file(absFilename)) {
        absFilename = std::format(
            "{}/{}.profile", absPath("${USER_PROFILES}"), *saveFileName
        );
    }

    if (std::filesystem::is_regular_file(absFilename) &&
        !shouldOverwrite &&
        receivedSaveFileName)
    {
        throw ghoul::lua::LuaError(
            std::format(
                "Unable to save profile '{}'. File of same name already exists",
                absFilename
            )
        );
    }

    global::configuration->profile.profile = absFilename;

    std::ofstream outFile = std::ofstream(absFilename, std::ofstream::out);
    if (!outFile.good()) {
        throw ghoul::lua::LuaError(std::format(
            "Exception opening profile file for write '{}'", absFilename
        ));
    }

    try {
        outFile << global::profile->serialize();
    }
    catch (const std::ofstream::failure&) {
        throw ghoul::lua::LuaError(std::format(
            "Data write error to file '{}'", absFilename
        ));
    }
}

} // namespace

#include "profile_lua_codegen.cpp"
