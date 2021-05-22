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

#include <openspace/interaction/helpmanager.h>

#include <openspace/engine/globals.h>
#include <openspace/scripting/lualibrary.h>

#include "helpmanager_lua.inl"



namespace openspace::interaction {


    scripting::LuaLibrary HelpManager::luaLibrary() {
#ifdef WIN32
        return {
                 "",
                 {
                     {
                         "openDocumentation",
                         &luascriptfunctions::openDocumentation,
                         {},
                         "string",
                         "Open documentation for the current profile in a browser"
                     },   
                     {
                         "openUserGuides",
                         &luascriptfunctions::openUserGuides,
                         {},
                         "string",
                         "Open OpenSpace User Guides in a browser"
                     },
                     {
                         "openTutorials",
                         &luascriptfunctions::openTutorials,
                         {},
                         "string",
                         "Open OpenSpace tutorials in a browser"
                     },
                     {
                         "openSupport",
                         &luascriptfunctions::openSupport,
                         {},
                         "string",
                         "Access Support options for OpenSpace in a browser"
                     },
                     {
                         "openServerStatus",
                         &luascriptfunctions::openServerStatus,
                         {},
                         "string",
                         "Open the OpenSpace Server Status page in a browser"
                     }
                 },
        };
#else // WIN32
        return {
        };
#endif // WIN32
    }

} // namespace openspace::interaction
