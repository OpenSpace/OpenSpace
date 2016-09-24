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

#include <openspace/mission/missionmanager.h>

#include <ghoul/misc/assert.h>

#include <assert.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/spicemanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>


namespace {
    const std::string _loggerCat = "MissionPhaseSequencer";

    const std::string KEY_PHASE_NAME = "Name";
    const std::string KEY_PHASE_DESCRIPTION = "Description";
    const std::string KEY_PHASE_SUBPHASES = "Phases";
    const std::string KEY_TIME_RANGE = "TimeRange";
}

namespace openspace {

MissionManager* MissionManager::_instance = nullptr;

MissionManager& MissionManager::ref() {
    ghoul_assert(_instance, "Instance has not been initiated");
    return *_instance;
}

void MissionManager::initialize() {
    ghoul_assert(!_instance, "Instance has been initialized before");
    _instance = new MissionManager;
    OsEng.scriptEngine().addLibrary(MissionManager::luaLibrary());
}

void MissionManager::deinitialize() {
    delete _instance;
    _instance = nullptr;
}

void MissionManager::setCurrentMission(const std::string& missionName) {
    auto it = _missionMap.find(missionName);
    if (it == _missionMap.end()) {
        LWARNING("Mission with name \"" << missionName << "\" has not been loaded!");
    }
    else {
        _currentMission = it;
    }
}

/**
* Returns true if a current mission exists
*/

bool MissionManager::hasCurrentMission() const {
    return _currentMission != _missionMap.end();
}

void MissionManager::loadMission(const std::string& filepath) {
    // Changing the values might invalidate the _currentMission iterator
    const std::string& currentMission = _currentMission->first;

    Mission mission(filepath);
    _missionMap[mission.name()] = std::move(mission);
    if (_missionMap.size() == 1) {
        setCurrentMission(mission.name());
    }

    setCurrentMission(currentMission);
}

const Mission& MissionManager::currentMission() {
    if (_currentMission == _missionMap.end()) {
        LWARNING("No current mission has been specified. returning dummy mission");
        return Mission();
    }
    return _currentMission->second;
}

namespace luascriptfunctions { 
    int loadMission(lua_State* L) {
        using ghoul::lua::luaTypeToString;
        int nArguments = lua_gettop(L);
        if (nArguments != 1)
            return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

        std::string missionFileName = luaL_checkstring(L, -1);
        if (missionFileName.empty()) {
            return luaL_error(L, "filepath string is empty");
        }
        MissionManager::ref().loadMission(missionFileName);
    }

    int setCurrentMission(lua_State* L) {
        using ghoul::lua::luaTypeToString;
        int nArguments = lua_gettop(L);
        if (nArguments != 1)
            return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

        std::string missionName = luaL_checkstring(L, -1);
        if (missionName.empty()) {
            return luaL_error(L, "mission name string is empty");
        }
        MissionManager::ref().setCurrentMission(missionName);
    }
} // namespace luascriptfunction

scripting::LuaLibrary MissionManager::luaLibrary() {
    return {
        "",
        {
            {
                "loadMission",
                &luascriptfunctions::loadMission,
                "string",
                "Load mission phases from file"
            },
            {
                "setCurrentMission",
                &luascriptfunctions::setCurrentMission,
                "string",
                "Set the currnet mission"
            },
        }
    };
}

// Singleton

MissionManager::MissionManager()
     : _currentMission(_missionMap.end())
{}

}  // namespace openspace
