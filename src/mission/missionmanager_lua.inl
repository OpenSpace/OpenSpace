/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

// Load mission phases from file.
[[codegen::luawrap]] std::string loadMission(std::string missionFileName) {
    if (missionFileName.empty()) {
        throw ghoul::lua::LuaError("Filepath is empty");
    }

    std::string name = openspace::global::missionManager->loadMission(missionFileName);
    return name;
}

// Unloads a previously loaded mission.
[[codegen::luawrap]] void unloadMission(std::string missionName) {
    using namespace openspace;

    if (missionName.empty()) {
        throw ghoul::lua::LuaError("Mission name is empty");
    }

    if (!global::missionManager->hasMission(missionName)) {
        throw ghoul::lua::LuaError("Mission was not previously loaded");
    }

    global::missionManager->unloadMission(missionName);
}

// Returns whether a mission with the provided name has been loaded.
[[codegen::luawrap]] bool hasMission(std::string missionName) {
    if (missionName.empty()) {
        throw ghoul::lua::LuaError("Missing name is empty");
    }

    bool hasMission = openspace::global::missionManager->hasMission(missionName);
    return hasMission;
}

// Set the currnet mission.
[[codegen::luawrap]] void setCurrentMission(std::string missionName) {
    if (missionName.empty()) {
        throw ghoul::lua::LuaError("Mission name is empty");
    }
    openspace::global::missionManager->setCurrentMission(missionName);
}

#include "missionmanager_lua_codegen.cpp"
