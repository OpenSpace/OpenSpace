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

#include <openspace/mission/missionmanager.h>

#include <openspace/events/eventengine.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <filesystem>

#include "missionmanager_lua.inl"

namespace openspace {

MissionManager::MissionManagerException::MissionManagerException(std::string error)
    : ghoul::RuntimeError(std::move(error), "MissionManager")
{}

MissionManager::MissionManager() : _currentMission(_missionMap.end()) {}

void MissionManager::setCurrentMission(const std::string& identifier) {
    ghoul_assert(!identifier.empty(), "missionName must not be empty");

    auto it = _missionMap.find(identifier);
    if (it == _missionMap.end()) {
        throw MissionManagerException("Mission has not been loaded");
    }
    else {
        _currentMission = it;
    }
}

bool MissionManager::hasCurrentMission() const {
    return _currentMission != _missionMap.end();
}


std::string MissionManager::loadMission(Mission mission) {
    // Changing the values might invalidate the _currentMission iterator
    const std::string currentMission = hasCurrentMission() ? _currentMission->first : "";
    const std::string identifier = mission.identifier();
    _missionMap.insert({ identifier, std::move(mission) });
    if (_missionMap.size() == 1) {
        setCurrentMission(identifier);
    }

    if (!currentMission.empty()) {
        setCurrentMission(currentMission);
    }

    global::eventEngine->publishEvent<events::EventMissionAdded>(identifier);
    return identifier;
}

void MissionManager::unloadMission(const std::string& identifier) {
    ghoul_assert(!identifier.empty(), "missionName must not be empty");
    auto it = _missionMap.find(identifier);
    ghoul_assert(it != _missionMap.end(), "missionName must be a loaded mission");

    if (it == _currentMission) {
        _currentMission = _missionMap.end();
    }

    global::eventEngine->publishEvent<events::EventMissionRemoved>(identifier);
    _missionMap.erase(it);
}

bool MissionManager::hasMission(const std::string& identifier) {
    return _missionMap.find(identifier) != _missionMap.end();
}

const Mission& MissionManager::currentMission() {
    if (_currentMission == _missionMap.end()) {
        throw MissionManagerException("No current mission has been specified");
    }
    return _currentMission->second;
}

const std::map<std::string, Mission>& MissionManager::missionMap() {
    return _missionMap;
}

scripting::LuaLibrary MissionManager::luaLibrary() {
    return {
        "",
        {
            codegen::lua::LoadMission,
            codegen::lua::UnloadMission,
            codegen::lua::HasMission,
            codegen::lua::SetCurrentMission
        }
    };
}

// Singleton
}  // namespace openspace
