/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <ghoul/lua/lua_helper.h>

// Load mission phases from file.
[[codegen::luawrap]] void loadMission(ghoul::Dictionary mission) {
    // TODO: Check if mission table is valid
    openspace::global::missionManager->loadMission(openspace::Mission(mission));
}

// Unloads a previously loaded mission.
[[codegen::luawrap]] void unloadMission(
                         std::variant<std::string, ghoul::Dictionary> identifierOrMission)
{
    using namespace openspace;

    std::string identifier;
    if (std::holds_alternative<std::string>(identifierOrMission)) {
        identifier = std::move(std::get<std::string>(identifierOrMission));
    }
    else {
        ghoul::Dictionary dict = std::get<ghoul::Dictionary>(identifierOrMission);
        if (!dict.hasValue<std::string>("Identifier")) {
            throw ghoul::lua::LuaError("Mission table needs 'Identifier'");
        }

        identifier = dict.value<std::string>("Identifier");
    }


    if (identifier.empty()) {
        throw ghoul::lua::LuaError("Mission identifier is empty");
    }

    if (!global::missionManager->hasMission(identifier)) {
        throw ghoul::lua::LuaError("Mission was not previously loaded");
    }

    global::missionManager->unloadMission(identifier);
}

// Returns whether a mission with the provided name has been loaded.
[[codegen::luawrap]] bool hasMission(std::string identifier) {
    if (identifier.empty()) {
        throw ghoul::lua::LuaError("Mission identifier is empty");
    }

    bool hasMission = openspace::global::missionManager->hasMission(identifier);
    return hasMission;
}

// Set the currnet mission.
[[codegen::luawrap]] void setCurrentMission(std::string identifier) {
    if (identifier.empty()) {
        throw ghoul::lua::LuaError("Mission identifier is empty");
    }
    openspace::global::missionManager->setCurrentMission(identifier);
}
#include "missionmanager_lua_codegen.cpp"

