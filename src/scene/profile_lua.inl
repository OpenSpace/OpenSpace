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

namespace openspace::luascriptfunctions {

int saveCurrentSettingsToProfile(lua_State* L) {
    if (!global::configuration.usingProfile) {
        return luaL_error(
            L,
            "Program was not started with a profile, so cannot use this "
            "save-current-settings feature"
        );
    }

    const int n = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 0, 1 },
        "lua::saveCurrentSettingsToProfile"
    );

    using ghoul::lua::luaTypeToString;

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
        saveFilePath = fmt::format("{}_{}.{}", f.fullBaseName(), time, f.fileExtension());
    }
    else {
        saveFilePath = ghoul::lua::value<std::string>(
            L,
            1,
            ghoul::lua::PopValue::Yes
        );
        if (saveFilePath.empty()) {
            return luaL_error(L, "save filepath string is empty");
        }
    }

    global::profile.saveCurrentSettingsToProfile(saveFilePath);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions
