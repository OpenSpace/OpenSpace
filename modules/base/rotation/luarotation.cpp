/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/base/rotation/luarotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <chrono>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ScriptInfo = {
        "Script",
        "Script",
        "This value is the path to the Lua script that will be executed to compute the "
        "rotation for this transformation. The script needs to define a function "
        "'rotation' that takes the current simulation time in seconds past the J2000 "
        "epoch as the first argument, the current wall time as milliseconds past the "
        "J2000 epoch as the second argument and computes the rotation returned as 9 "
        "values."
    };
} // namespace

namespace openspace {

documentation::Documentation LuaRotation::Documentation() {
    using namespace openspace::documentation;
    return {
        "Lua Rotation",
        "base_transform_rotation_lua",
        {
            {
                "Type",
                new StringEqualVerifier("LuaRotation"),
                Optional::No
            },
            {
                ScriptInfo.identifier,
                new StringVerifier,
                Optional::No,
                ScriptInfo.description
            }
        }
    };
}

LuaRotation::LuaRotation()
    : _luaScriptFile(ScriptInfo)
    , _state(ghoul::lua::LuaState::IncludeStandardLibrary::No)
{
    addProperty(_luaScriptFile);

    _luaScriptFile.onChange([&]() {
        requireUpdate();
        _fileHandle = std::make_unique<ghoul::filesystem::File>(_luaScriptFile);
        _fileHandle->setCallback([&](const ghoul::filesystem::File&) {
            requireUpdate();
        });
    });
}

LuaRotation::LuaRotation(const ghoul::Dictionary& dictionary) : LuaRotation() {
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "LuaRotation"
    );

    _luaScriptFile = absPath(dictionary.value<std::string>(ScriptInfo.identifier));
}

glm::dmat3 LuaRotation::matrix(const UpdateData& data) const {
    ghoul::lua::runScriptFile(_state, _luaScriptFile);

    // Get the scaling function
    lua_getglobal(_state, "rotation");
    const bool isFunction = lua_isfunction(_state, -1);
    if (!isFunction) {
        LERRORC(
            "LuaRotation",
            fmt::format("Script '{}' does nto have a function 'rotation'", _luaScriptFile)
        );
        return glm::dmat3(1.0);
    }

    // First argument is the number of seconds past the J2000 epoch in ingame time
    ghoul::lua::push(_state, data.time.j2000Seconds());

    // Second argument is the number of seconds past the J2000 epoch of the last frame
    ghoul::lua::push(_state, data.previousFrameTime.j2000Seconds());

    // Third argument is the number of milliseconds past the J2000 epoch in wallclock
    using namespace std::chrono;
    auto now = high_resolution_clock::now();
    ghoul::lua::push(_state, duration_cast<milliseconds>(now.time_since_epoch()).count());

    // Execute the scaling function
    int success = lua_pcall(_state, 2, 9, 0);
    if (success != 0) {
        LERRORC(
            "LuaScale",
            fmt::format("Error executing 'rotation': {}", lua_tostring(_state, -1))
        );
    }

    double values[9];
    for (int i = 0; i < 9; ++i) {
        values[i] = luaL_checknumber(_state, -1 - i);
    }

    return glm::make_mat3(values);
}

} // namespace openspace
