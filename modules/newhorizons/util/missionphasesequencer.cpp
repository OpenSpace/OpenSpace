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

#include <assert.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/spicemanager.h>
#include <modules/newhorizons/util/missionmanager.h>
#include <openspace/engine/openspaceengine.h>



namespace {
    const std::string _loggerCat = "MissionPhaseSequencer";
}





namespace openspace {

MissionPhase::MissionPhase(const ghoul::Dictionary& dict) {
    const auto byPhaseStartTime = [](const MissionPhase& a, const MissionPhase& b)->bool{
        return a.timeRange().start < b.timeRange().start;
    };

    _name = dict.value<std::string>("Name");
    ghoul::Dictionary childDicts;
    if (dict.getValue("Phases", childDicts)) {
        // This is a nested mission phase
        size_t numSubPhases = childDicts.size();
        _subphases.resize(numSubPhases);
        for (size_t i = 0; i < numSubPhases; ++i) {
            std::string key = std::to_string(i + 1);
            _subphases[i] = MissionPhase(childDicts.value<ghoul::Dictionary>(key));
        }
            
        std::stable_sort(_subphases.begin(), _subphases.end(), byPhaseStartTime);

        // The subphases will have a total time phases
        TimeRange timeRangeSubPhases;
        timeRangeSubPhases.start = _subphases[0].timeRange().start;
        timeRangeSubPhases.end = _subphases.back().timeRange().end;

        // user may specify an overall time range. In that case expand this timerange.
        TimeRange overallTimeRange;
        try {
            overallTimeRange = parseTimeRange(dict);
            ghoul_assert(overallTimeRange.includes(timeRangeSubPhases),
                "User specified time range must at least include its subphases'");
            _timeRange.include(overallTimeRange);
        }
        catch (...) {
            // Its OK to not specify an overall time range, the time range for the 
            // subphases will simply be used. 
            _timeRange.include(timeRangeSubPhases);
        }
    }
    else {
        _timeRange = parseTimeRange(dict);
    }
};

TimeRange MissionPhase::parseTimeRange(const ghoul::Dictionary& dict) {
    std::string startTimeStr;
    std::string endTimeStr;
    bool success = true;
    success &= dict.getValue("StartTime", startTimeStr);
    success &= dict.getValue("EndTime", endTimeStr);
    
    if (!success) {
        // Had to do this because ghoul::Dictionary::value<>(std::string key) throws 
        // uncatchable xtree error on my AMNH windwos machine/ eb)
        throw "meh";
    }
    // Parse to date
    TimeRange timeRange;
    timeRange.start = SpiceManager::ref().ephemerisTimeFromDate(startTimeStr);
    timeRange.end = SpiceManager::ref().ephemerisTimeFromDate(endTimeStr);
    return timeRange;
}





Mission::Mission(std::string filepath) 
    : MissionPhase(readDictFromFile(filepath))
    , _filepath(filepath) 
{
    
}

ghoul::Dictionary Mission::readDictFromFile(std::string filepath) {
    filepath = absPath(filepath);
    LINFO("Reading mission phases fomr file: " << filepath);
    if (!FileSys.fileExists(filepath))
        throw ghoul::FileNotFoundError(filepath, "Mission file path");

    ghoul::Dictionary missionDict;
    try {
        ghoul::lua::loadDictionaryFromFile(filepath, missionDict);
        return missionDict;
    }
    catch (ghoul::RuntimeError& e) {
        LWARNING("Unable to load mission phases");
        LWARNING(e.message);
    }
    return {};
}




MissionManager* MissionManager::_instance = nullptr;

MissionManager& MissionManager::ref() {
    assert(_instance != nullptr);
    return *_instance;
}

void MissionManager::initialize() {
    assert(_instance == nullptr);
    _instance = new MissionManager;
    OsEng.scriptEngine().addLibrary(MissionManager::luaLibrary());
}

void MissionManager::deinitialize() {
    delete _instance;
    _instance = nullptr;
}

void MissionManager::setCurrentMission(const std::string missionName) {
    auto it = _missionMap.find(missionName);
    if (it == _missionMap.end()) {
        LWARNING("Mission with name \"" << missionName << "\" has not been loaded!");
    }
    else {
        _currentMissionIter = it;
    }
}

void MissionManager::loadMission(const std::string& filepath) {
    Mission mission(filepath);
    _missionMap[mission.name()] = mission;
    if (_missionMap.size() == 1) {
        setCurrentMission(mission.name());
    }
}

const Mission& MissionManager::currentMission() {
    if (_currentMissionIter == _missionMap.end()) {
        LWARNING("No current mission has been specified. returning dummy mission");
    }
    return _currentMissionIter->second;
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
    return{
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

}  // namespace openspace
