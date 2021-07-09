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

#include <openspace/engine/globals.h>

#include <ghoul/filesystem/filesystem.h>


#ifdef WIN32
#include <windows.h>
#include <shellapi.h>
#endif // WIN32

namespace {
    constexpr const char* documentationFilename = "${BASE}/documentation/index.html";
    constexpr const char* userGuidesUrl = "https://www.openspaceproject.com/guides";
    constexpr const char* tutorialsUrl = "https://www.openspaceproject.com/tutorial-videos";
    constexpr const char* serverStatusUrl = "http://status.openspaceproject.com/";
    constexpr const char* supportUrl = "https://openspacesupport.slack.com/";
} // namespace

namespace openspace::luascriptfunctions {

    int openDocumentation(lua_State* L) {
#ifdef WIN32
        std::string path = absPath(documentationFilename).string();
        HINSTANCE result = ShellExecute(nullptr, "open", path.c_str(), "", "", SW_SHOW);
#endif // WIN32
        return 0;
    }

    int openUserGuides(lua_State* L) {
#ifdef WIN32
        ShellExecute(nullptr, "open", userGuidesUrl, "", "", SW_SHOW);
#endif // WIN32
        return 0;
    }

    int openTutorials(lua_State* L) {
#ifdef WIN32
        ShellExecute(nullptr, "open", tutorialsUrl, "", "", SW_SHOW);
#endif // WIN32
        return 0;
    }

    int openSupport(lua_State* L) {
#ifdef WIN32
        ShellExecute(nullptr, "open", supportUrl, "", "", SW_SHOW);
#endif // WIN32
        return 0;
    }

    int openServerStatus(lua_State* L) {
#ifdef WIN32
        ShellExecute(nullptr, "open", serverStatusUrl, "", "", SW_SHOW);
#endif // WIN32
        return 0;
    }

} // namespace openspace::luascriptfunctions
