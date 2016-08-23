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

    const std::string KEY_PHASE_NAME = "Name";
    const std::string KEY_PHASE_DESCRIPTION = "Description";
    const std::string KEY_PHASE_SUBPHASES = "Phases";
    const std::string KEY_TIME_RANGE = "TimeRange";
}





namespace openspace {

MissionPhase::MissionPhase(const ghoul::Dictionary& dict) {
    const auto byPhaseStartTime = [](const MissionPhase& a, const MissionPhase& b)->bool{
        return a.timeRange().start < b.timeRange().start;
    };

    _name = dict.value<std::string>(KEY_PHASE_NAME);
    if (!dict.getValue(KEY_PHASE_DESCRIPTION, _description)) {
        // If no description specified, just init to empty string
        _description = "";
    }
    
    ghoul::Dictionary childDicts;
    if (dict.getValue(KEY_PHASE_SUBPHASES, childDicts)) {
        // This is a nested mission phase
        _subphases.resize(childDicts.size());
        for (size_t i = 0; i < childDicts.size(); ++i) {
            std::string key = std::to_string(i + 1);
            _subphases[i] = MissionPhase(childDicts.value<ghoul::Dictionary>(key));
        }

        // Ensure subphases are sorted
        std::stable_sort(_subphases.begin(), _subphases.end(), byPhaseStartTime);

        // Calculate the total time range of all subphases
        TimeRange timeRangeSubPhases;
        timeRangeSubPhases.start = _subphases[0].timeRange().start;
        timeRangeSubPhases.end = _subphases.back().timeRange().end;

        // user may specify an overall time range. In that case expand this timerange.
        ghoul::Dictionary timeRangeDict;
        if (dict.getValue(KEY_TIME_RANGE, timeRangeDict)) {
            TimeRange overallTimeRange(timeRangeDict);
            ghoul_assert(overallTimeRange.includes(timeRangeSubPhases),
                "User specified time range must at least include its subphases'");
            _timeRange.include(overallTimeRange);
        }
        else {
            // Its OK to not specify an overall time range, the time range for the 
            // subphases will simply be used. 
            _timeRange.include(timeRangeSubPhases);
        }
    }
    else {
        ghoul::Dictionary timeRangeDict;
        if (dict.getValue(KEY_TIME_RANGE, timeRangeDict)) {
            _timeRange = TimeRange(timeRangeDict); // throws exception if unable to parse
        }
        else {
            throw std::runtime_error("Must specify key: " + KEY_TIME_RANGE);
        }
    }
};

std::list<const MissionPhase*> MissionPhase::phaseTrace(double time, int maxDepth) const {
    std::list<const MissionPhase*> trace;
    if (_timeRange.includes(time)) {
        trace.push_back(this);
        phaseTrace(time, trace, maxDepth);
    }
    return std::move(trace);
}

bool MissionPhase::phaseTrace(double time, std::list<const MissionPhase*>& trace, int maxDepth) const {
    if (maxDepth == 0) {
        return false;
    }

    for (int i = 0; i < _subphases.size(); ++i) {
        if (_subphases[i]._timeRange.includes(time)) {
            trace.push_back(&_subphases[i]);
            _subphases[i].phaseTrace(time, trace, maxDepth - 1);
            return true; // only add the first one
        }
        // Since time ranges are sorted we can do early termination
        else if (_subphases[i]._timeRange.start > time) {
            return false;
        }
    }
    return true;
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
