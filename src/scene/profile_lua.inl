/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <ghoul/filesystem/file.h>
#include <ctime>
#include <filesystem>

namespace openspace::luascriptfunctions {

int saveSettingsToProfile(lua_State* L) {
    if (!global::configuration.usingProfile) {
        return luaL_error(
            L,
            "Program was not started with a profile, so cannot use this "
            "save-current-settings feature"
        );
    }

    const int n = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 0, 2 },
        "lua::saveSettingsToProfile"
    );

    LINFOC("1", ghoul::lua::stackInformation(L));

    std::string saveFilePath;
    if (n == 0) {
        const ghoul::filesystem::File f = global::configuration.profile;

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
        std::string newFile = fmt::format(
            "{}_{}.{}",
            f.fullBaseName(), time, f.fileExtension()
        );
        std::filesystem::copy(global::configuration.profile, newFile);
        saveFilePath = global::configuration.profile;
    }
    else {
        saveFilePath = ghoul::lua::value<std::string>(L, 1);
        LINFOC("2", ghoul::lua::stackInformation(L));
        if (saveFilePath.empty()) {
            return luaL_error(L, "save filepath string is empty");
        }
    }

    global::profile.saveCurrentSettingsToProfile();
    global::configuration.profile = saveFilePath;

    if (saveFilePath.find('/') != std::string::npos) {
        return luaL_error(L, "Profile filename must not contain path (/) elements");
    }
    else if (saveFilePath.find(':') != std::string::npos) {
        return luaL_error(L, "Profile filename must not contain path (:) elements");
    }
    else if (saveFilePath.find('.') != std::string::npos) {
        return luaL_error(L, "Only provide the filename to save without file extension");
    }
    const std::string absFilename = absPath("${ASSETS}/" + saveFilePath + ".profile");

    const bool overwrite = (n == 2) ? ghoul::lua::value<bool>(L, 2) : false;
    LINFOC("3", ghoul::lua::stackInformation(L));

    if (FileSys.fileExists(absFilename) && !overwrite) {
        return luaL_error(
            L, 
            fmt::format(
                "Unable to save profile '{}'. File of same name already exists.",
                absFilename.c_str()
            ).c_str()
        );
    }

    std::ofstream outFile;
    // @TODO (abock, 2020-06-15) Replace with non-throwing stream
    try {
        outFile.open(absFilename, std::ofstream::out);
    }
    catch (const std::ofstream::failure& e) {
        return luaL_error(
            L,
            fmt::format(
                "Exception opening profile file for write: {} ({})", absFilename, e.what()
            ).c_str()
        );
    }

    try {
        outFile << serialize(global::profile.profile);
    }
    catch (const std::ofstream::failure& e) {
        return luaL_error(
            L,
            fmt::format(
                "Data write error to file: {} ({})",
                absFilename, e.what()
            ).c_str()
        );
    }

    outFile.close();

    lua_settop(L, 0);
    return 0;
}

} // namespace openspace::luascriptfunctions
