/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ctime>
#include <filesystem>

namespace openspace::luascriptfunctions {

int saveSettingsToProfile(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 0, 2 }, "lua::saveSettingsToProfile");
    auto [saveFilePath, overwrite] =
        ghoul::lua::values<std::optional<std::string>, std::optional<bool>>(L);
    overwrite = overwrite.value_or(true);

    if (!saveFilePath.has_value()) {
        std::time_t t = std::time(nullptr);
        std::tm* utcTime = std::gmtime(&t);
        ghoul_assert(utcTime, "Conversion to UTC failed");

        std::string time = fmt::format(
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
        std::string newFile = fmt::format("{}_{}", path.string(), time);
        std::string sourcePath = fmt::format(
            "{}/{}.profile",
            absPath("${USER_PROFILES}").string(), global::configuration->profile
        );
        std::string destPath = fmt::format(
            "{}/{}.profile",
            absPath("${PROFILES}").string(), global::configuration->profile
        );
        if (!std::filesystem::is_regular_file(sourcePath)) {
            sourcePath = fmt::format(
                "{}/{}.profile",
                absPath("${USER_PROFILES}").string(), global::configuration->profile
            );
        }
        LINFOC("Profile", fmt::format("Saving a copy of the old profile as {}", newFile));
        std::filesystem::copy(sourcePath, destPath);
        saveFilePath = global::configuration->profile;
    }
    else {
        if (saveFilePath->empty()) {
            return ghoul::lua::luaError(L, "Save filepath string is empty");
        }
    }

    const properties::PropertyOwner& root = *global::rootPropertyOwner;
    std::string currentTime = std::string(global::timeManager->time().ISO8601());
    interaction::NavigationState navState = global::navigationHandler->navigationState();
    global::profile->saveCurrentSettingsToProfile(root, currentTime, navState);
    global::configuration->profile = *saveFilePath;

    if (saveFilePath->find('/') != std::string::npos) {
        return ghoul::lua::luaError(L, "Profile filename must not contain (/) elements");
    }
    else if (saveFilePath->find(':') != std::string::npos) {
        return ghoul::lua::luaError(L, "Profile filename must not contain (:) elements");
    }
    else if (saveFilePath->find('.') != std::string::npos) {
        return ghoul::lua::luaError(
            L,
            "Only provide the filename to save without file extension"
        );
    }

    std::string absFilename = fmt::format(
        "{}/{}.profile", absPath("${PROFILES}").string(), *saveFilePath
    );
    if (!std::filesystem::is_regular_file(absFilename)) {
        absFilename = absPath("${USER_PROFILES}/" + *saveFilePath + ".profile").string();
    }

    if (std::filesystem::is_regular_file(absFilename) && !overwrite) {
        return ghoul::lua::luaError(
            L,
            fmt::format(
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
        return ghoul::lua::luaError(
            L,
            fmt::format(
                "Exception opening profile file for write: {} ({})", absFilename, e.what()
            )
        );
    }

    try {
        outFile << global::profile->serialize();
    }
    catch (const std::ofstream::failure& e) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Data write error to file: {} ({})", absFilename, e.what())
        );
    }
    return 0;
}

} // namespace openspace::luascriptfunctions
