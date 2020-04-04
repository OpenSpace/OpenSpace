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

#include <openspace/scene/profile.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scenelicensewriter.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <string>
#include <stack>

#include "profile_lua.inl"

namespace {
    constexpr const char* _loggerCat = "Profile";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyParent = "Parent";

} // namespace

namespace openspace {

void Profile::saveCurrentSettingsToProfile(std::string filename) {
    if (! global::configuration.usingProfile) {
        std::string errorMessage = "Program was not started using a profile, "
            "so cannot use this save-current-settings feature.";
        LERROR(errorMessage);
    }
    std::string initProfile = global::configuration.profile;
}

scripting::LuaLibrary Profile::luaLibrary() {
    return {
        "",
        {
            {
                "saveCurrentSettingsToProfile",
                &luascriptfunctions::saveCurrentSettingsToProfile,
                {},
                "string",
                "Collects all changes that have been made since startup, including all "
                "property changes and assets required, requested, or removed. All "
                "changes will be added to the profile that OpenSpace was started with, "
                "and the new saved file will contain all of this information. The "
                "file will be named according to the argument provided. If this argument "
                "is blank, then the new file will have the base name of the profile that "
                " was started, with a _YYYYMMDD suffix."
            }
        }
    };
}

}  // namespace openspace
