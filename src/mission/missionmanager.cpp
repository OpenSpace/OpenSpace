/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>

#include "missionmanager_lua.inl"

namespace openspace {

MissionManager::MissionManagerException::MissionManagerException(std::string error)
    : ghoul::RuntimeError(std::move(error), "MissionManager")
{}

MissionManager::MissionManager() : _currentMission(_missionMap.end()) {}

void MissionManager::setCurrentMission(const std::string& missionName) {
    ghoul_assert(!missionName.empty(), "missionName must not be empty");

    auto it = _missionMap.find(missionName);
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

std::string MissionManager::loadMission(const std::string& filename) {
    ghoul_assert(!filename.empty(), "filename must not be empty");
    ghoul_assert(!FileSys.containsToken(filename), "filename must not contain tokens");
    ghoul_assert(FileSys.fileExists(filename), "filename " + filename + " must exist");

    // Changing the values might invalidate the _currentMission iterator
    std::string currentMission =  hasCurrentMission() ? _currentMission->first : "";

    Mission mission = missionFromFile(filename);
    std::string missionName = mission.name();
    _missionMap.insert({ missionName, std::move(mission) });
    if (_missionMap.size() == 1) {
        setCurrentMission(missionName);
    }

    if (!currentMission.empty()) {
        setCurrentMission(currentMission);
    }

    return missionName;
}

void MissionManager::unloadMission(const std::string& missionName) {
    ghoul_assert(!missionName.empty(), "missionName must not be empty");
    auto it = _missionMap.find(missionName);
    ghoul_assert(it != _missionMap.end(), "missionName must be a loaded mission");

    if (it == _currentMission) {
        _currentMission = _missionMap.end();
    }

    _missionMap.erase(it);
}

bool MissionManager::hasMission(const std::string& missionName) {
    return _missionMap.find(missionName) != _missionMap.end();
}

const Mission& MissionManager::currentMission() {
    if (_currentMission == _missionMap.end()) {
        throw MissionManagerException("No current mission has been specified");
    }
    return _currentMission->second;
}

scripting::LuaLibrary MissionManager::luaLibrary() {
    return {
        "",
        {
            {
                "loadMission",
                &luascriptfunctions::loadMission,
                {},
                "string",
                "Load mission phases from file"
            },
            {
                "unloadMission",
                &luascriptfunctions::unloadMission,
                {},
                "string",
                "Unloads a previously loaded mission"
            },
            {
                "hasMission",
                &luascriptfunctions::hasMission,
                {},
                "string",
                "Returns whether a mission with the provided name has been loaded"
            },
            {
                "setCurrentMission",
                &luascriptfunctions::setCurrentMission,
                {},
                "string",
                "Set the currnet mission"
            },
        }
    };
}

// Singleton


}  // namespace openspace
