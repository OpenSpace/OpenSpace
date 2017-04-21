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

namespace openspace {
namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * toggleShutdown():
 * Toggles the shutdown mode that will close the application after the countdown timer is
 * reached
 */
int toggleShutdown(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 0)
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

    OsEng.toggleShutdownMode();

    return 1;
}

/**
* \ingroup LuaScripts
* writeDocumentation():
* Writes out documentation files
*/
int writeDocumentation(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 0)
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

    OsEng.writeDocumentation();

    return 1;
}

/**
* \ingroup LuaScripts
* downloadFile():
* Downloads a file from Lua interpreter
*/
int downloadFile(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 2)
        return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);
    std::string uri = luaL_checkstring(L, -2);
    std::string savePath = luaL_checkstring(L, -1);

    const std::string _loggerCat = "OpenSpaceEngine";
    LINFO("Downloading file from " << uri);
    DownloadManager dm = openspace::DownloadManager("", 1, false);
    std::shared_ptr<openspace::DownloadManager::FileFuture> future =
        dm.downloadFile(uri, absPath("${SCENE}/" + savePath), true, true, 5);
    if (!future || (future && !future->isFinished)) {
        std::string errorMsg = "Download failed";
        if (future)
            errorMsg += ": " + future->errorMessage;
        return luaL_error(L, errorMsg.c_str());
    }
    return 1;
}

} // namespace luascriptfunctions
} // namespace openspace
