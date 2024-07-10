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
        "epoch as the first argument, the simulation time in seconds past the J2000 "
        "epoch of the last frame as the second argument, and the current wall time as "
        "milliseconds past the J2000 epoch as the third argument. It computes the "
        "rotation value factors returned as a table containing the 9 values that make up "
        "the resulting rotation matrix.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(LuaRotation)]] Parameters {
        // [[codegen::verbatim(ScriptInfo.description)]]
        std::filesystem::path script;
    };
#include "luarotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation LuaRotation::Documentation() {
    return codegen::doc<Parameters>("base_transform_rotation_lua");
}

LuaRotation::LuaRotation(const ghoul::Dictionary& dictionary)
    : _luaScriptFile(ScriptInfo)
    , _state(
        ghoul::lua::LuaState::IncludeStandardLibrary::Yes,
        ghoul::lua::LuaState::StrictState::No
    )
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _luaScriptFile.onChange([this]() {
        requireUpdate();
        _fileHandle = std::make_unique<ghoul::filesystem::File>(_luaScriptFile.value());
        _fileHandle->setCallback([this]() { requireUpdate(); });
    });

    addProperty(_luaScriptFile);

    _luaScriptFile = p.script.string();
}

glm::dmat3 LuaRotation::matrix(const UpdateData& data) const {
    ghoul::lua::runScriptFile(_state, _luaScriptFile.value());

    // Get the scaling function
    lua_getglobal(_state, "rotation");
    const bool isFunction = lua_isfunction(_state, -1);
    if (!isFunction) {
        LERRORC(
            "LuaRotation",
            std::format(
                "Script '{}' does not have a function 'rotation'", _luaScriptFile.value()
            )
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
    const int success = lua_pcall(_state, 3, 1, 0);
    if (success != 0) {
        LERRORC(
            "LuaRotation",
            std::format("Error executing 'rotation': {}", lua_tostring(_state, -1))
        );
    }
    const glm::dmat3 rotation = ghoul::lua::value<glm::dmat3>(_state);
    return rotation;
}

} // namespace openspace
